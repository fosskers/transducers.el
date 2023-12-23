# Transducers

## Unreleased

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

