# Transducers

## Unreleased

#### Added

- Added `t-from-json-buffer` for streaming values from a buffer that contains a
  JSON array. Due to the unique way it traverses the buffer to read legal JSON,
  this function is a "Source", unlike `t-from-csv`, which is a transducer.
- `buffer` objects (not their names) can now be passed directly to `transduce`
  instead of needing to wrap it in `t-buffer-read`. `t-buffer-read` is however
  still necessary if you wish to transduce over a buffer via its name (a string).

## 1.0.0

Initial release.

