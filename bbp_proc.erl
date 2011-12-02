-module(bbp_proc).
-compile([export_all]).

% Concurent Proccess Excercises
% http://www.erlang.org/course/exercises.html

% Two-way messenger
init_two_way_messages() ->
  Pid1 = spawn(fun two_way_messenger/0),
  Pid2 = spawn(fun two_way_messenger/0),
  Pid1 ! {"the_message", 5, Pid2}.

two_way_messenger() ->
  receive
    {Msg, 0, Pid} ->
      Pid ! {Msg, 0, self()},
      exit(done);
    {Msg, Num, Pid} ->
      io:format("message from ~w ~w~n", [Pid, Num]),
      Pid ! {Msg, Num - 1, self()},
      two_way_messenger();
    _ ->
      two_way_messenger()
  end.

% Ring
init_ring(NumMembers, NumMessages)  ->
  Members = spawn_members(NumMembers),
  io:fwrite("Starting ring with members "),
  lists:foreach(fun (I) -> io:fwrite("~w -> ", [I]) end, Members),
  io:fwrite("~n"),
  [Pid1|_] = Members,
  Pid1 ! {msg, NumMessages}.

spawn_members(NumMembers) when NumMembers > 1 ->
  First = spawn_member(),
  spawn_members(NumMembers-1, [First]).

spawn_members(0, [First|Tail]) ->
  First ! {set_next, lists:last(Tail)},
  [First|Tail];
spawn_members(Num, Members) ->
  Member = spawn_member(lists:last(Members)),
  spawn_members(Num-1, Members ++ [Member]).

spawn_member() -> spawn(fun nextless_ring_member/0).
spawn_member(Pid) ->
  spawn(fun () -> ring_member(Pid) end).

nextless_ring_member() ->
  receive
    {set_next, Next} -> ring_member(Next);
    _ -> nextless_ring_member()
  end.

ring_member(Next) ->
  receive
    {msg, 0} -> io:fwrite("~nDone!~n"), exit(done);
    {msg, Count} ->
      io:fwrite("~w    ~w~n", [Count, self()]),
      Next ! {msg, Count-1},
      ring_member(Next);
    _ -> ring_member(Next)
  end.

% Star
init_star(NumProc, NumMsg) ->
  Pid = spawn(fun () -> star_supervisor(NumProc * NumMsg, 0) end),
  io:fwrite("Star's Pid:  ~w~n", [Pid]),
  Pid ! {start, NumProc, NumMsg}.

star_supervisor(Limit, Current) when Limit == Current ->
  io:fwrite("Sup quitting, all done~n"),
  exit(done);
star_supervisor(Limit, Current) ->
  receive
    {start, NumProc, NumMsg} ->
      Children = star_children(self(), NumProc),
      io:fwrite("Made ~w children~n", [length(Children)]),
      star_send_messages(Children, NumMsg),
      star_supervisor(Limit, Current);
    {msg, Pid} ->
      io:fwrite("Received message ~w from pid ~w~n", [msg,Pid]),
      star_supervisor(Limit, Current+1);
    _-> star_supervisor(Limit, Current)
  end.

star_children(Sup, NumProc) ->
  First = spawn(fun () -> star_child(Sup) end),
  star_children(Sup, NumProc-1, [First]).

star_children(Sup, 1, Children) ->
  Child = spawn(fun () -> star_child(Sup) end),
  Children ++ [Child];
star_children(Sup, NumProc, Children) ->
  Child = spawn(fun () -> star_child(Sup) end),
  star_children(Sup, NumProc-1, Children ++ [Child]).

star_child(Sup) ->
  receive
    {stop} -> exit(done);
    {msg} ->
      Sup ! {msg, self()},
      star_child(Sup);
    _ -> star_child(Sup)
  end.

star_send_messages(Children, 0) ->
  io:fwrite("Stopping children~n", []),
  lists:foreach(fun (Child) -> io:fwrite("."), Child ! {stop} end, Children);
star_send_messages(Children, NumMsg) ->
  io:fwrite("Sending message ~w~n", [NumMsg]),
  lists:foreach(fun (Child) -> io:fwrite("."), Child ! {msg} end, Children),
  star_send_messages(Children, NumMsg-1).
