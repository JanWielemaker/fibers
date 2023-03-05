:- use_module(sched_engines).

create(N, Goal) :-
    forall(between(1, N, _), set_immediate(Goal)).
