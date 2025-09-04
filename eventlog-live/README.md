# Threading Analyses

## Capability Usage Analysis

The capability usage analysis combines the GC span and mutator span analyses.
It classifies any time not covered by either a GC span or a mutator span as idle time.

### GC Span Analysis

The GC span analysis produces GC spans and follows this finite-state automaton:

```mermaid
stateDiagram-v2
  direction LR

  [*]  --> Idle
  Idle --> Idle : EndGC
  Idle --> GC   : StartGC
  GC   --> Idle : EndGC
  GC   --> GC   : StartGC
```

The transition from `GC` to `Idle` yields a GC span.

### Mutator Span Analysis

The mutator span analysis produces mutator spans and follows this automaton.
This is not a finite-state automaton, since the `Mutator[X]` state and the `RunThread[X]` and `StopThread[X]` events are indexed by a thread ID. Furthermore, each `StopThread` event has a `status` field (which can be either `ThreadFinished` or not). Finally, the automaton maintains a set of finished thread IDs, to block `RunThread[X]` transitions after a thread has finished.

```mermaid
stateDiagram-v2
  direction RL

  state Idle
  state Mutator[X]
  state RunThread[Y] <<choice>>
  state StopThread[Y] <<choice>>
  state RunThreadError
  RunThreadError : Error
  state StopThreadError
  StopThreadError : Error

  classDef badBadEvent fill:#f00,color:white,font-weight:bold,stroke-width:2px,stroke:yellow
  class RunThreadError, StopThreadError badEvent

  [*]             --> Idle
  Idle            --> Idle            : StopThread[X]
  Idle            --> Mutator[X]      : RunThread[X]
  Mutator[X]      --> StopThread[Y]   : StopThread[Y]
  StopThread[Y]   --> Idle            : if X=Y<br />if status=ThreadFinished, ignore future RunThread[X]
  StopThread[Y]   --> StopThreadError : if X≠Y
  StopThreadError --> Mutator[X]

  Mutator[X]      --> RunThread[Y]    : RunThread[Y]
  RunThread[Y]    --> Mutator[X]      : if X=Y
  RunThread[Y]    --> RunThreadError  : if X≠Y
  RunThreadError  --> Mutator[X]
```

The transition from `Mutator[X]` to `Idle` yields a mutator span.
While in the `Mutator[X]` state, any `RunThread[Y]` or `StopThread[Y]` events with `X≠Y` result in an error. Furthermore, when a `StopThread[Y]` event with `X=Y` and `status=ThreadFinished` status is processed, the thread `X` is added to a set of finished threads, and any further `RunThread[X]` events for that `X` are ignored. This is done because the GHC RTS frequently emits a false `RunThread[X]` event immediately after the `StopThread[X]` event with `status=ThreadFinished`.

## Thread State Analysis

The thread state analysis produces thread state spans and follows this finite-state automaton.
Unlike the [Mutator Span Analysis](#mutator-span-analysis), the thread events are not indexed by thread ID, because each thread-state analysis automaton only handles thread events for one specific thread.
Each `StopThread` event has a `status` field (which can be either `ThreadFinished` or not).

```mermaid
stateDiagram-v2
  direction RL

  state Running
  state Blocked
  state StopThread <<choice>>

  [*]        --> Running    : RunThread
  Blocked    --> Running    : RunThread
  Running    --> Running    : RunThread
  Running    --> StopThread : StopThread
  Blocked    --> StopThread : StopThread
  StopThread --> Blocked    : status≠ThreadFinished
  StopThread --> [*]        : status=ThreadFinished
```

The transition from `Running` to `Blocked` yields a thread state span with the `Running` category and the capability on which the thread is running. The transition from `Running` to `Running` does not yield any span, but the start of the span is adjusted to be the ealier of the two events. There is no need to adjust the capability, since running threads cannot be migrated.

The transitions from `Blocked` to `Running` and `Blocked` to `Blocked` yield thread state spans with the `Blocked` category and the reason why the thread is blocked. Each successive `Blocked` to `Blocked` transition yields another span, because the reason that the thread is blocked may change over time, e.g., from "yielding to the scheduler" to "blocked on an MVar".

# Notes

The `ThreadRunnable` event should not be used to analyse thread state, since it was dropped in [0e51109d](https://gitlab.haskell.org/ghc/ghc/-/commit/0e51109d010c474f60f7b3209e399c115c7bcec7) after not being used since [f361281c](https://gitlab.haskell.org/coot/ghc/-/commit/f361281c89fbce42865d8b8b27b0957205366186).

The `WakeupThread` event does not denote a thread state transition, but merely signifies that one thread has attempted to wake up another thread.

The GHC RTS frequently emits a pair of `RunThread` and `WakeupThread` events immediately after a `StopThread` event with the `ThreadFinished` status. Hence, once the final state is reached, all events, including `RunThread` events, are ignored.
