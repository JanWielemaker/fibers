:- use_module(library(apply)).
:- use_module(library(debug)).

:- meta_predicate
       run_in_fiber(0).

:- dynamic
       schedule/1.

run_in_fiber(Goal) :-
    term_variables(Goal, Vars),
    Templ =.. [v|Vars],
    engine_create(Templ, engine_run(Goal), Engine),
    asserta(schedule(Engine)),
    event_loop([Engine]).

engine_run(Goal) :-
    set_prolog_flag(heartbeat, 1000),
    call(Goal).

event_loop([]) :-
    !.
event_loop(Engines) :-
    convlist(advance, Engines, Engines1),
    event_loop(Engines1).

advance(Engine, Engine) :-
    engine_next_reified(Engine, Result),
    consume(Result, Engine).

consume(the(Answer), Engine) =>
    debug(event_loop(answer), 'Got ~p from ~p', [Answer, Engine]).
consume(no, Engine) =>
    debug(event_loop(complete), 'Engine ~p failed', [Engine]),
    fail.
consume(exeption(Error), _Engine) =>
    print_message(error, Error),
    fail.

prolog:heartbeat :-
    thread_self(Me),
    (   is_async
    ->  format(user_error, '~p: heartbeat~n', [Me]),
	engine_yield(heartbeat)
    ;   format(user_error, '~p: ignored heartbeat~n', [Me])
    ).

is_async :-
    '$can_yield'.
