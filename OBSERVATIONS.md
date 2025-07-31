# Thread Analysis

1.  There is no `CreateThread` event for the main thread.

    This causes the thread state analysis machine to fail, since there is no
    transition from `(ThreadInitial, RunThread)`.

    This could be addressed by issuing an init event that identifies the main
    thread.

2.  The RTS will issue `WakeupThread` events for threads that were previously
    stopped with `StopThread _ ThreadFinished`.

    This causes the thread state analysis machine to fail, since there is no
    transition from `(ThreadFinished, WakeupThread)`.

    This may be worth having a chat about, to see if this is correct behaviour.
    If this is correct behaviour for the RTS, then I should fix `ghc-events`.

3.  This RTS will occasionally issue multiple `WakeupThread` events in sequence
    for the same thread.

    This causes the thread state analysis machine to fail, since there is no
    transition from `(ThreadRunning, WakeupThread)`.

    This may be worth having a chat about, to see if this is correct behaviour.
    This is arguably correct behaviour for `ghc-events`, since the second such
    event does not cause any change.

4.  The thread state analysis machine will, of course, fail when connecting to
    the eventlog of an event that has already been running. (Come to think of
    it, that may well be the cause of #1.)

    This can be fixed by simply accepting the output state for the first event
    that we see for a particular thread, which is deterministic. Alternatively,
    this can be fixed by creating an init event that issues the current thread
    state.
