-module(proc_memory_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0, get/0, reset/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

get() ->
    gen_server:call(?MODULE, get, infinity).

reset() ->
    gen_server:call(?MODULE, reset, infinity).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    timer:send_after(100, tick),
    {ok, {[], []}}.

handle_call(get, _From, State) ->
    {reply, State, State};
handle_call(reset, _From, _State) ->
    {reply, ok, {[], []}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State = {StoredMem, _}) ->
    Memory = erlang:memory(),
    StoredTotal = proplists:get_value(total, StoredMem, 0),
    Total = proplists:get_value(total, Memory, 0),
    NewState = case StoredTotal < Total of
                   true ->
                       {Memory, perf_misc:offenders()};
                   false ->
                       State
               end,
    timer:send_after(100, tick),
    {noreply, NewState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
