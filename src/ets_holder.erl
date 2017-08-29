-module(ets_holder).

-behaviour(gen_server).

-include("ns_common.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    random:seed(),
    create_ets(blah_table),
    lists:foreach(
      fun (A) ->
              Name = list_to_atom(lists:flatten(io_lib:format("blah_table~p", [A]))),
              create_ets(Name)
      end, lists:seq(1, 20)),
    {ok, ok}.

create_ets(Name) ->
    ?log_debug("Creating table ~p", [Name]),
    BinKey = <<"replications/0d912e43858aadefb8836bb75c83e176/user_profile_versions/user_profile_versions/changes_left">>,

    _ = ets:new(Name, [protected, named_table]),
    lists:foreach(
      fun (_) ->
              Key = now(),
              List = [{BinKey, random:uniform(2000) + A} || A <- lists:seq(1, 100)],
              Entry = {Key, List},
              ets:insert_new(Name, Entry)
      end, lists:seq(1000, 2000)).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
