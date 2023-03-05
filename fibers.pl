/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(fibers,
	  [ call_in_fiber/1,	% :Goal
	    set_immediate/1,	% :Goal
	    set_timeout/2	% :Goal,+Time
	  ]).

/** <module> Engine based cooperative multi tasking

*/

:- use_module(library(debug)).

:- meta_predicate
       run_in_fiber(0),
       set_immediate(0),
       set_timeout(0, +).

%!  call_in_fiber(:Goal)
%
%   Run Goal in a fiber.

call_in_fiber(Goal) :-
    engine_create(true, engine_run(Goal), Engine),
    debug(sched, 'Run ~p in ~p', [Goal, Engine]),
    event_loop([Engine]).

engine_run(Goal) :-
    set_prolog_flag(heartbeat, 1000),
    call(Goal).

%!  engine_run(+Engines) is det.
%
%   Run the event loop until all engines have completed

event_loop([]) :-
    !.
event_loop(Engines) :-
    phrase(advance_tasks(Engines), NewEngines),
    event_loop(NewEngines).

advance_tasks([]) -->
    [].
advance_tasks([H|T]) -->
    advance(H),
    advance_tasks(T).

advance(start(Time, Engine)) -->
    !,
    { get_time(Now) },
    (   {Time > Now}
    ->   { engine_next_reified(Engine, Result) },
	 consume(Result, Engine)
    ;   [start(Time, Engine)]
    ).
advance(Engine) -->
    { engine_next_reified(Engine, Result) },
    consume(Result, Engine).

consume(the(heartbeat), From) -->
    !,
    [From].
consume(the(schedule(New)), From) -->
    !,
    [From,New].
consume(the(schedule_at(Start, New)), From) -->
    !,
    [From,start(Start,New)].
consume(the(true), From) -->
    !,
    { debug(sched, 'Completed ~p', [From]) }.
consume(no, From) -->
    !,
    { debug(sched, 'Failed ~p', [From]) }.
consume(throw(Error), From) -->
    { debug(sched, 'Error in ~p', [From]) },
    { print_message(error, Error) }.

%!  set_immediate(:Goal)
%
%   Create a new fiber for Goal.

set_immediate(Goal) :-
    engine_create(true, engine_run(Goal), Engine),
    debug(sched, 'Run ~p in ~p', [Goal, Engine]),
    engine_yield(schedule(Engine)).

%!  set_timeout(:Goal, +Time)

set_timeout(Goal, Time) :-
    get_time(Now),
    Start is Now+Time,
    engine_create(true, engine_run(Goal), Engine),
    engine_yield(schedule_at(Start, Engine)).

prolog:heartbeat :-
    (   is_async
    ->  engine_yield(heartbeat)
    ;   true
    ).

is_async :-
    '$can_yield'.
