# Transducers

## 1.4.0 (2025-02-15)

- `t-unique-by` for more control over how uniqueness is determined.
- `t-for` as a better pattern for doing something effectful over the stream.
- `t-any?`, `t-all?`, and `t-reduced?` as modern aliases.

#### Deprecated

- `t-for-each`: use `t-for` instead.

## 1.3.1 (2025-01-13)

#### Fixed

- `t-once` handles a `nil` argument better.

## 1.3.0 (2024-11-03)

#### Added

- `t-reduced` as a better wrapper than `make-transducers-reduced`.
- The `t-median` reducer.
- The `t-reversed` source to iterate from the end of an array/vector/string.
- `t-const` and `t-snoc` which were unintentionally missing.

#### Changed

- `t-anyp` and `t-allp` now yield `t` instead of just non-nil when they succeed.
- `t-concatenate` and `t-flatten` now support vectors/arrays/strings.

## 1.1.0 (2023-12-24)

Merry Christmas. The code itself was written in airports.

#### Added

- Source: `t-from-json-buffer` for streaming values from a buffer that contains a
  JSON array. Due to the unique way it traverses the buffer to read legal JSON,
  this function is a "Source", unlike `t-from-csv`, which is a transducer.
- Reducer: `t-into-json-buffer` to write a stream of objects into the current
  buffer as legal JSON. It uses the built-in `json-serialize` to do this. The
  user is expected to manage which buffer is actually considered "current".
- `buffer` objects (not their names) can now be passed directly to `transduce`
  instead of needing to wrap it in `t-buffer-read`. `t-buffer-read` is however
  still necessary if you wish to transduce over a buffer via its name (a string).

#### Changed

- `t-for-each` now yields `t` instead of `nil`.

## 1.0.0

Initial release.

