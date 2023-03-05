:- module(sched_engines,
	  [ run_in_fiber/1,	% :Goal
	    set_immediate/1	% :Goal
	  ]).

/** <module> Engine based cooperative multi tasking

*/

:- use_module(library(debug)).

:- meta_predicate
       run_in_fiber(0).

%!  run_in_fiber(:Goal)
%
%   Run Goal in a fiber.

run_in_fiber(Goal) :-
    engine_create(true, engine_run(Goal), Engine),
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

advance(Engine) -->
    { engine_next_reified(Engine, Result) },
    consume(Result, Engine).

consume(the(heartbeat), From) -->
    [From].
consume(the(schedule(New)), [From]) -->
    [From,New].
consume(the(true), _From) -->
    [].
consume(no, _From) -->
    [].
consume(exeption(Error), _From) -->
    { print_message(error, Error) },
    [].

%!  set_immediate(:Goal)
%
%   Create a new fiber for Goal.

set_immediate(Goal) :-
    engine_create(true, engine_run(Goal), Engine),
    engine_yield(schedule(Engine)).

prolog:heartbeat :-
    (   is_async
    ->  engine_yield(heartbeat)
    ;   true
    ).

is_async :-
    '$can_yield'.
