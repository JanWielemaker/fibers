:- use_module(fibers).

% For example:
%
% ?- time(run_in_fiber(create(10, rtest_chats(100)))).

create(N, Goal) :-
    forall(between(1, N, _), set_immediate(Goal)).
