# Transducers

## Unreleased

#### Added

- `buffer` objects (not their names) can now be passed directly to `transduce`
  instead of needing to wrap it in `t-buffer-read`. `t-buffer-read` is however
  still necessary if you wish to transduce over a buffer via its name (a string).

## 1.0.0

Initial release.

