-module(perf_misc).

-export([offenders/0, get_binaries_ref_info/1, proc_binaries_summary/1]).

offenders() ->
    Processes = lists:reverse(lists:sort([{erlang:process_info(P, memory), P} ||
                                             P <- erlang:processes()])),
    {Offenders, _} = lists:split(5, Processes),
    [process_info(P, [registered_name, memory, current_stacktrace,
                      current_function, initial_call, heap_size]) ||
        {_, P} <- Offenders].

find_binary_processes(BinPtr) ->
    lists:foldl(
      fun (Pid, Acc) ->
              {binary, Bin} = erlang:process_info(Pid, binary),
              case proplists:is_defined(BinPtr, Bin) of
                  true ->
                      [Pid | Acc];
                  false ->
                      Acc
              end
      end, [], erlang:processes()).

process_id_info(Pid) ->
    [{pid, Pid} | process_info(
                    Pid,
                    [registered_name, current_stacktrace, initial_call, current_function])].

get_binaries_ref_info(Pid) ->
    {binary, Bin} = erlang:process_info(Pid, binary),
    lists:foldl(
      fun ({_Ptr, _Size, 1}, Acc) ->
              Acc;
          ({Ptr, Size, RefCnt}, Acc) ->
              [[{ptr, Ptr}, {size, Size}, {ref_cnt, RefCnt},
                {processes, [process_id_info(P) || P <- find_binary_processes(Ptr)]}] | Acc]
      end, [], Bin).

proc_binaries_summary(Pid) ->
    {binary, Bin} = erlang:process_info(Pid, binary),
    Size = lists:foldl(
             fun ({_, S, _}, A) ->
                     A + S
             end, 0, Bin),
    {length(Bin), Size}.
