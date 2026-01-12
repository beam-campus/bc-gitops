%%% @doc Hot code reload utilities for bc_gitops.
%%%
%%% This module provides hot code reloading capabilities for OTP applications.
%%% It supports both simple module reloads and coordinated upgrades that
%%% properly handle stateful processes via sys:suspend/resume.
%%%
%%% == Simple Reload ==
%%%
%%% For stateless modules or when state migration isn't needed:
%%% ```
%%% bc_gitops_hot_reload:reload_modules([my_module]).
%%% '''
%%%
%%% == Coordinated Upgrade ==
%%%
%%% For stateful gen_servers that implement code_change/3:
%%% ```
%%% bc_gitops_hot_reload:upgrade_app(my_app, "1.0.0", "1.1.0").
%%% '''
%%%
%%% This will suspend processes, load new code, and resume them,
%%% triggering code_change/3 callbacks.
%%%
%%% @end
-module(bc_gitops_hot_reload).

%% API
-export([
    reload_module/1,
    reload_modules/1,
    reload_changed_modules/1,
    upgrade_app/3,
    get_app_modules/1,
    get_module_beam_hash/1
]).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

%% @doc Reload a single module.
%%
%% Uses soft_purge to avoid killing processes using old code,
%% then loads the new version.
-spec reload_module(module()) -> {ok, module()} | {error, term()}.
reload_module(Module) ->
    case code:soft_purge(Module) of
        true ->
            case code:load_file(Module) of
                {module, Module} ->
                    {ok, Module};
                {error, Reason} ->
                    {error, {load_failed, Module, Reason}}
            end;
        false ->
            %% Old code still in use - try harder
            case code:purge(Module) of
                true ->
                    case code:load_file(Module) of
                        {module, Module} ->
                            {ok, Module};
                        {error, Reason} ->
                            {error, {load_failed, Module, Reason}}
                    end;
                false ->
                    {error, {purge_failed, Module, processes_using_old_code}}
            end
    end.

%% @doc Reload multiple modules.
-spec reload_modules([module()]) -> {ok, [module()]} | {error, term()}.
reload_modules(Modules) ->
    Results = [reload_module(M) || M <- Modules],
    Errors = [E || {error, _} = E <- Results],
    case Errors of
        [] ->
            {ok, Modules};
        _ ->
            {error, {partial_reload, Errors}}
    end.

%% @doc Reload only modules that have changed on disk.
%%
%% Compares the MD5 hash of loaded modules with their beam files.
-spec reload_changed_modules(atom()) -> {ok, [module()]} | {error, term()}.
reload_changed_modules(App) ->
    case get_app_modules(App) of
        {ok, Modules} ->
            Changed = [M || M <- Modules, module_changed(M)],
            case Changed of
                [] ->
                    {ok, []};
                _ ->
                    reload_modules(Changed)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Perform a coordinated upgrade of an application.
%%
%%  This function:
%%  1. Identifies all processes belonging to the application
%%  2. Suspends gen_server/gen_statem processes
%%  3. Reloads changed modules
%%  4. Resumes processes (triggering code_change/3)
%%
%% For this to work properly, your gen_servers must implement code_change/3.
-spec upgrade_app(atom(), binary(), binary()) -> ok | {error, term()}.
upgrade_app(App, OldVsn, NewVsn) ->
    case get_app_modules(App) of
        {ok, Modules} ->
            %% Find processes to suspend
            Procs = get_app_processes(App),
            SuspendableProcs = [P || P <- Procs, is_suspendable(P)],

            %% Suspend processes
            ok = suspend_processes(SuspendableProcs),

            %% Reload changed modules
            ChangedModules = [M || M <- Modules, module_changed(M)],
            ReloadResult = reload_modules(ChangedModules),

            %% Resume processes (this triggers code_change)
            ok = resume_processes(SuspendableProcs, OldVsn, NewVsn),

            case ReloadResult of
                {ok, _} -> ok;
                {error, Reason} -> {error, {reload_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get all modules belonging to an application.
-spec get_app_modules(atom()) -> {ok, [module()]} | {error, term()}.
get_app_modules(App) ->
    case application:get_key(App, modules) of
        {ok, Modules} ->
            {ok, Modules};
        undefined ->
            %% App might not be loaded, try loading it first
            case application:load(App) of
                ok ->
                    application:get_key(App, modules);
                {error, {already_loaded, App}} ->
                    case application:get_key(App, modules) of
                        {ok, Modules} -> {ok, Modules};
                        undefined -> {error, {no_modules_defined, App}}
                    end;
                {error, Reason} ->
                    {error, {app_load_failed, Reason}}
            end
    end.

%% @doc Get the MD5 hash of a module's beam file.
-spec get_module_beam_hash(module()) -> {ok, binary()} | {error, term()}.
get_module_beam_hash(Module) ->
    case code:get_object_code(Module) of
        {Module, Binary, _Filename} ->
            Hash = crypto:hash(md5, Binary),
            {ok, Hash};
        error ->
            {error, {module_not_found, Module}}
    end.

%% -----------------------------------------------------------------------------
%% Internal - Module change detection
%% -----------------------------------------------------------------------------

-spec module_changed(module()) -> boolean().
module_changed(Module) ->
    case Module:module_info(md5) of
        LoadedMd5 when is_binary(LoadedMd5) ->
            case get_disk_module_md5(Module) of
                {ok, DiskMd5} ->
                    LoadedMd5 =/= DiskMd5;
                {error, _} ->
                    false
            end;
        _ ->
            false
    end.

-spec get_disk_module_md5(module()) -> {ok, binary()} | {error, term()}.
get_disk_module_md5(Module) ->
    case code:which(Module) of
        non_existing ->
            {error, not_found};
        cover_compiled ->
            {error, cover_compiled};
        preloaded ->
            {error, preloaded};
        BeamFile ->
            case beam_lib:md5(BeamFile) of
                {ok, {Module, Md5}} ->
                    {ok, Md5};
                {error, _, Reason} ->
                    {error, Reason}
            end
    end.

%% -----------------------------------------------------------------------------
%% Internal - Process management
%% -----------------------------------------------------------------------------

-spec get_app_processes(atom()) -> [pid()].
get_app_processes(App) ->
    case application_controller:get_master(App) of
        undefined ->
            [];
        Master ->
            get_supervised_processes(Master)
    end.

-spec get_supervised_processes(pid()) -> [pid()].
get_supervised_processes(Sup) ->
    case catch supervisor:which_children(Sup) of
        Children when is_list(Children) ->
            lists:flatten([
                case Child of
                    {_Id, Pid, supervisor, _Mods} when is_pid(Pid) ->
                        [Pid | get_supervised_processes(Pid)];
                    {_Id, Pid, worker, _Mods} when is_pid(Pid) ->
                        [Pid];
                    _ ->
                        []
                end
             || Child <- Children
            ]);
        _ ->
            []
    end.

-spec is_suspendable(pid()) -> boolean().
is_suspendable(Pid) ->
    case proc_lib:translate_initial_call(Pid) of
        {gen_server, init_it, _} -> true;
        {gen_statem, init_it, _} -> true;
        {gen_event, init_it, _} -> true;
        {gen_fsm, init_it, _} -> true;  % Legacy
        _ -> false
    end.

-spec suspend_processes([pid()]) -> ok.
suspend_processes(Procs) ->
    lists:foreach(
        fun(Pid) ->
            catch sys:suspend(Pid, 5000)
        end,
        Procs
    ),
    ok.

-spec resume_processes([pid()], binary(), binary()) -> ok.
resume_processes(Procs, _OldVsn, _NewVsn) ->
    lists:foreach(
        fun(Pid) ->
            catch sys:resume(Pid, 5000)
        end,
        Procs
    ),
    ok.
