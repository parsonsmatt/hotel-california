# hotel-california

`hotel-california` is inspired by [`Trace`](https://github.com/Pondidum/Trace/) and [`otel-cli`](https://github.com/equinix-labs/otel-cli), a pair of utilities for tracing shell scripts.
We needed something like this in order to instrument local builds, so we could understand how much time people are spending waiting on builds, tests, etc.

# Usage

The binary name is `hotel`.
Currently, the only supported command is `exec`

```sh
$ hotel exec -- help
Usage: hotel exec [-s|--span-name SPAN_NAME] SCRIPT [SCRIPT...]

  Execute the given command with tracing enabled

Available options:
  -h,--help                Show this help text
```



# Background/FAQ

## Lol what's up with the name

Well `otel-cli` is a great name for the tool.
But this is a Haskell implementation, so I need an `h` in there somewhere.
`hotel-cli` sounds good.
But wait... `cli` ... what else could I do with that?
Ahah!

Sorry

## Difference from `Trace`

The `Trace` tool has the following basic workflow:

```sh
$ TRACE_PARENT=$(trace start "build")
$ make build
$ trace finish
```

When you call `trace start "trace-name"`, it generates a `TraceId` and `SpanId` and records that to a file in the temporary directory.
The filename carries the trace ID and span ID.
The file contains the name of the trace, the start time, and any other metadata you provide.

When you call `trace finish`, the tool looks for the `TRACE_PARENT` environment variable.
It then looks in the `$TMP/traces/state` directory for a file that matches the `TRACE_PARENT`.
It loads the file, creates a `Span` with the timestamp given in the file, and then calls `span.End`.
The tool then makes a network request to report this data.

### The Problems

#### Performance

The tool allows you to start groups and run commands that will make individual spans, allowing you to understand the overall trace.
`trace group start` is similar to `trace start` - it creates a new Span ID, attaches it to the parent span, and writes that to the temporary directory.
`trace task` does something a bit more idiomatic - it creates a `Span`, runs the command you provide, and then does `Span.end`.
`trace group finish` is similar to `trace finish` - it loads the parent span information for the group, creates a `Span`, and calls `span.End` with the timestamps loaded from the group.

The toy example I did is here:

```sh
$ TRACE_PARENT=$(trace start "why"); \
  GROUP=$(trace group start "why-1"); \
  trace task "$GROUP" -- go build; \
  trace group finish "$GROUP"; \
  trace finish
```

According to Honeycomb, this spends 170ms doing `go build`, and then incurs another 650ms to complete the entire process - the `why-1` group takes roughly 370ms extra, and then the final `trace finish` call adds another 300ms.

This performance hit may not be substantial for the apparent intention of the tool - instrumenting CI builds - but it is going to be a problem for *my* intention with the tool - instrumenting local developer workflows.
300ms is significant, but not terrible if incurred once.
However, incurring that for each step we want to record?
That's a problem.

The out-of-the-box solution is to use an OpenTelemetry collector that is local to the machine, and can report the spans periodically, in the background.
This is an extra deployment step, so it'd be nice to avoid that, if possible.

#### Nesting

The API requires you to work in this manner:

```sh
$ TRACE_PARENT=$(trace start "my-trace")
$ OUTER_GROUP=$(trace group start "neat")
$ INNER_GROUP=$(trace group start "neat" "$OUTER_GROUP")
$ trace task "$INNER_GROUP" -- make build
$ trace group finish "$INNER_GROUP"
$ trace group finish "$OUTER_GROUP"
$ trace finish
```

So any time you do `trace start`, you create a *root span*.
But any time you do `trace group finish`, you create a *child span*.

This makes it difficult to create a span, and just "know" if you're in a root or not.
You would need this in order to provide a composable interface: shell scripts calling other shell scripts which can all record spans.

## Difference from `otel-cli`

Well, `otel-cli` solves most of the above problems.
The main entry point is `otel-cli exec`, which runs a command for you, and reports a span for it.
You can nest `otel-cli exec` calls arbitrarily, which works nicely.
However, it too had some issues, with the most challenging being a bug around signals.
I simply couldn't figure out the behavior around signals in Golang, and all available internet advice wasn't exactly helpful.
I decided then to spike out this tool.
