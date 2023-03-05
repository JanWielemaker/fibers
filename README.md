# SWI-Prolog fibers

The   `fibers`  library   allows  running   tasks  using   event-based
cooperative multi-tasking.

The current implementation is a proof of concept.   It allows starting
a single goal in a fiber using

    ?- call_in_fiber(Goal).

This creates a  fiber and runs a scheduling loop  until no more fibers
exist.  New fibers can be created from the orignal one using one of

    ?- set_immediate(Goal).
	?- set_timeout(Goal, Time).
