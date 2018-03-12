# CHANGELOG

### 1.1.1

- `offline` and `online` were given more relaxed inner-Monad parameters:
  `MonadResource` and `MonadIO` respectively. Usually non-constrained functions
  perform better, but we maintain performance despite this change via
  `SPECIALIZE` pragmas.

### 1.1.0

- `attoparsec` support for `streaming` is used to manually parse dump-files
  in `offline`. This is a vast performance improvement over using
  `Network.Pcap.offline`.
