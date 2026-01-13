# Macula Console Integration

This guide explains how to integrate bc_gitops with macula-console for mesh-wide application orchestration visibility.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Macula Console                               │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │                    GitOps Dashboard                          │    │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐       │    │
│  │  │   Node A     │  │   Node B     │  │   Node C     │       │    │
│  │  │  3 apps      │  │  2 apps      │  │  4 apps      │       │    │
│  │  │  synced ✓    │  │  pending ⏳   │  │  synced ✓    │       │    │
│  │  └──────────────┘  └──────────────┘  └──────────────┘       │    │
│  └─────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────┘
          │                    │                    │
          │ DHT RPC            │ DHT RPC            │ DHT RPC
          │ PubSub             │ PubSub             │ PubSub
          ▼                    ▼                    ▼
    ┌───────────┐        ┌───────────┐        ┌───────────┐
    │  Node A   │        │  Node B   │        │  Node C   │
    │ bc_gitops │◄──────►│ bc_gitops │◄──────►│ bc_gitops │
    │  local    │  mesh  │  local    │  mesh  │  local    │
    └───────────┘        └───────────┘        └───────────┘
          │                    │                    │
          ▼                    ▼                    ▼
    ┌───────────┐        ┌───────────┐        ┌───────────┐
    │ Git Repo  │        │ Git Repo  │        │ Git Repo  │
    │ (local)   │        │ (local)   │        │ (local)   │
    └───────────┘        └───────────┘        └───────────┘
```

## Integration Patterns

### 1. Local bc_gitops per Node

Each edge node runs its own bc_gitops instance watching a local git repository. This provides:

- **Autonomy**: Nodes can operate independently when disconnected
- **Local state**: Each node manages its own application lifecycle
- **Resilience**: No single point of failure for orchestration

### 2. Mesh Exposure via DHT

bc_gitops can expose its state to the mesh via macula's DHT:

```erlang
%% Register bc_gitops RPC handlers
macula:register_rpc(<<"bc_gitops.status">>, fun bc_gitops:status/0).
macula:register_rpc(<<"bc_gitops.state">>, fun bc_gitops:get_current_state/0).
macula:register_rpc(<<"bc_gitops.reconcile">>, fun bc_gitops:reconcile/0).
```

### 3. Telemetry to PubSub Bridge

Forward bc_gitops telemetry events to mesh PubSub:

```erlang
%% In your application startup
bc_gitops_mesh_bridge:start_link().

%% Bridge module
-module(bc_gitops_mesh_bridge).

attach() ->
    Events = [
        [bc_gitops, reconcile, start],
        [bc_gitops, reconcile, stop],
        [bc_gitops, deploy, stop],
        [bc_gitops, upgrade, stop],
        [bc_gitops, remove, stop]
    ],
    telemetry:attach_many(
        <<"bc_gitops_mesh">>,
        Events,
        fun handle_event/4,
        #{}
    ).

handle_event(Event, Measurements, Metadata, _Config) ->
    %% Publish to mesh topic
    Topic = <<"gitops.events">>,
    Payload = #{
        node => node(),
        event => Event,
        measurements => Measurements,
        metadata => Metadata,
        timestamp => erlang:system_time(millisecond)
    },
    macula:publish(Topic, Payload).
```

## Macula Console Implementation

### GitOps Dashboard Component

```elixir
defmodule MaculaConsoleWeb.GitOpsLive do
  use MaculaConsoleWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Subscribe to mesh-wide gitops events
      Macula.subscribe("gitops.events")

      # Start periodic refresh of all nodes
      :timer.send_interval(10_000, self(), :refresh_nodes)
    end

    socket =
      socket
      |> assign(:nodes, %{})
      |> assign(:events, [])
      |> fetch_all_nodes()

    {:ok, socket}
  end

  defp fetch_all_nodes(socket) do
    # Get all mesh peers
    peers = Macula.get_peers()

    # Query each peer's bc_gitops state via RPC
    nodes =
      peers
      |> Task.async_stream(fn peer ->
        case Macula.rpc(peer, "bc_gitops.status", %{}) do
          {:ok, status} ->
            state = Macula.rpc(peer, "bc_gitops.state", %{})
            {peer, %{status: status, state: state}}
          {:error, _} ->
            {peer, %{status: :unreachable, state: %{}}}
        end
      end, timeout: 5_000)
      |> Enum.into(%{})

    assign(socket, :nodes, nodes)
  end

  @impl true
  def handle_info({:macula_pubsub, "gitops.events", payload}, socket) do
    # Handle mesh-wide gitops events
    event = %{
      id: System.unique_integer([:positive]),
      node: payload["node"],
      event: payload["event"],
      metadata: payload["metadata"],
      timestamp: payload["timestamp"]
    }

    events = [event | socket.assigns.events] |> Enum.take(100)

    {:noreply, assign(socket, :events, events)}
  end

  @impl true
  def handle_info(:refresh_nodes, socket) do
    {:noreply, fetch_all_nodes(socket)}
  end
end
```

### Key Features

1. **Mesh-wide Visibility**: See all nodes' orchestration state in one dashboard
2. **Real-time Events**: Subscribe to gitops events from all nodes
3. **Remote Actions**: Trigger reconciliation on any node via RPC
4. **Aggregate View**: Count total apps, healthy apps, pending updates across mesh

## Configuration

### bc_gitops Node Setup

```erlang
%% sys.config
[
  {bc_gitops, [
    {repo_url, "https://github.com/org/node-specs.git"},
    {branch, "main"},
    {reconcile_interval, 60000},
    %% Enable mesh integration
    {mesh_enabled, true},
    {mesh_topic, <<"gitops.events">>}
  ]},
  {macula, [
    {realm, <<"production">>},
    {bootstrap_nodes, ["boot.macula.io"]}
  ]}
].
```

### Security Considerations

- **UCAN Authorization**: Require proper capabilities for remote reconcile commands
- **Event Filtering**: Only expose necessary telemetry data to the mesh
- **Node Identity**: Verify node identity before accepting RPC commands

## Use Cases

### 1. Fleet Management

Monitor application deployments across all edge nodes:

```
┌─────────────────────────────────────────────┐
│ Fleet Status                                │
├─────────────────────────────────────────────┤
│ Nodes: 42          Apps: 156                │
│ Synced: 38         Running: 142             │
│ Pending: 4         Failed: 14               │
└─────────────────────────────────────────────┘
```

### 2. Rolling Updates

Coordinate updates across the mesh:

1. Update spec in git repo
2. Each node detects change on next reconcile
3. Console shows update progress across fleet
4. Alert if any node fails to update

### 3. Troubleshooting

When an app fails to deploy:

1. Console shows which nodes have failures
2. Click node to see detailed error
3. View reconciliation event log
4. Trigger manual reconcile if needed

## Related

- [bc_gitops API Reference](api.md)
- [Telemetry Events](telemetry.md)
- [Macula DHT Documentation](https://hexdocs.pm/macula/dht.html)
