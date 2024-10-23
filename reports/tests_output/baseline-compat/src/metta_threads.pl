:- use_module(library(predicate_options)).
:- use_module(library(record)).

% convenience for async/3 options
:- record opts( policy:oneof([ephemeral,lazy])=ephemeral
              ).
:- predicate_options(spawn/2,2,[pass_to(async/3,3)]).
:- predicate_options(async/3,3, [ policy(+oneof([ephemeral,lazy]))
                                ]).

:- meta_predicate
    spawn(0),
    async(0,-),
    async(0,-,+),
    async_policy(+,0,-,+).

:- thread_local
    spawn_token_needs_await/1.

%% spawn(:Goal) is det.
%
%  Like spawn/2 with default options.
spawn(Goal) :-
    spawn(Goal, []).


%% spawn(:Goal,+Options) is det.
%
%  Seek solutions to Goal in a background thread. Solutions are
%  communicated to the calling thread by unifying free variables in
%  Goal.  If Goal has no free variables, you must use async/3 instead.
%  Options are passed through to async/3.
%
%  For example, the following code runs in about 1 second because both
%  sleep/1 calls happen in parallel. When foo/0 unifies L, it blocks
%  until silly/1 has finished.
%
%      silly(L) :-
%          sleep(1),
%          L = [a,b].
%      foo :-
%          spawn(silly(L)),
%          sleep(1),
%          L=[A,B],  % blocks, if necessary
%          writeln(A-B).
%
%  If Goal produces multiple solutions, they're iterated when
%  backtracking over the unification (=|L=[A,B]|= above). If Goal fails
%  or throws an exception, the calling thread sees it at the unification
%  point.
spawn(Goal,Options) :-
    term_variables(Goal, Vars),
    async(Goal, Token, Options),
    Id is random(1<<63),
    assert(spawn_token_needs_await(Id)),
    make_opts(Options,Opts),
    maplist(spawn_freeze(Id,Token,Opts), Vars).

spawn_freeze(Id,Token,Opts,Var) :-
    freeze(Var,spawn_thaw(Id,Token,Opts)).

spawn_thaw(Id,Token,Opts) :-
    ( retract(spawn_token_needs_await(Id)) ->
        debug(spawn,"Await on ~d",[Id]),
        await(Token)
    ; opts_policy(Opts,lazy) ->
        debug(spawn,"Awaiting again on ~d",[Id]),
        await(Token)
    ; % already called await/1 ->
        debug(spawn,"Already did await on ~d",[Id]),
        true
    ).


%% lazy(Goal) is det.
%
%  Postpone execution of goal until needed. This is just spawn/1
%  using the =lazy= thread policy.
%
%  lazy/1 can be helpful when complicated or expensive goals are only
%  needed in some code paths but duplicating those goals is too verbose.
%  It can be an alternative to creating a new, named predicate. For
%  example,
%
%      foo(Xs) :-
%          lazy(i_am_slow(a,B,[c(C),d(d),e(etc)])), % complicated
%
%          ( day_of_week(tuesday) ->
%              append(B,C,Xs)
%          ; phase_of_moon(full) ->
%              append(C,B,Xs)
%          ; true ->
%              % i_am_slow/3 not executed in this code path
%              Xs = [hi]
%          ).
lazy(Goal) :-
    spawn(Goal,[policy(lazy)]).


%% async(:Goal,-Token) is det.
%
%  Like async/3 with default options.
async(Goal,Token) :-
    async(Goal,Token,[]).


%% async(:Goal,-Token,+Options) is det.
%
%  Seek solutions to Goal in a background thread. Use await/1 with Token
%  to block until the computation is done. Solutions are communicated to
%  the calling thread by unifying free variables in Goal.  Both Goal and
%  its corresponding solutions are copied between threads. Be aware if
%  any of those terms are very large.
%
%  Options are as follows:
%
%    * policy(Policy)
%    If =ephemeral= (default), create a new thread in which to call
%    goal. If =lazy=, only execute Goal when await/1 is called; no
%    background threads are used.
async(Goal,Token,Options) :-
    make_opts(Options,Opts),
    opts_policy(Opts, Policy),
    async_policy(Policy, Goal, Token, Opts).


async_policy(ephemeral, Goal, Token, _Opts) :-
    % what does the caller need to track this computation?
    term_variables(Goal, Vars),
    message_queue_create(SolutionsQ, [max_size(1)]),
    Token = ephemeral_token(Vars,SolutionsQ),

    % start the worker thread
    Work = work(Goal,Vars,SolutionsQ),
    thread_create(ephemeral_worker(Work), _, [detached(true)]).
async_policy(lazy,Goal,Token,_Opts) :-
    Token = lazy_thunk(Goal).


ephemeral_worker(work(Goal,Vars,SolutionsQ)) :-
    debug(spawn,"Seeking solutions to: ~q", [Goal]),
    ( catch(call_cleanup(Goal,Done=true),E,true) *->
        ( nonvar(E) ->
            debug(spawn,"Caught exception: ~q", [E]),
            thread_send_message(SolutionsQ,exception(E))
        ; var(Done) ->
            debug(spawn,"Sending solution: ~q", [Vars]),
            thread_send_message(SolutionsQ,solution(Vars)),
            fail  % look for another solution
        ; Done=true ->
            debug(spawn,"Final solution: ~q", [Vars]),
            thread_send_message(SolutionsQ,final(Vars))
        )
    ; % no solutions ->
        debug(spawn, "Found no solutions", []),
        thread_send_message(SolutionsQ,none)
    ).


%% await(+Token)
%
%  Wait for solutions from an async/3 call. Token is an opaque value
%  provided by async/3 which identifies a background computation.
%
%  await/1 strives to have the same determinism as the original Goal
%  passed to async/3. If that goal fails, await/1 fails. If that goal
%  throws an exception, so does await/1. If that goal produces many
%  solutions, so does await/1 on backtracking.
await(ephemeral_token(Vars,SolutionsQ)) :-
    repeat,
    thread_get_message(SolutionsQ,Solution),
    ( Solution = solution(Vars) ->
        true
    ; Solution = final(Vars) ->
        !,
        true
    ; Solution = none ->
        !,
        fail
    ; Solution = exception(E) ->
        throw(E)
    ; % what? ->
        throw(unexpected_await_solution(Solution))
    ).
await(lazy_thunk(Goal)) :-
    call(Goal).

