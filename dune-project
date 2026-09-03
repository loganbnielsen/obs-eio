(lang dune 3.0)
(name obs-eio)
(version 0.1.1)
(generate_opam_files true)

(package
 (name obs-eio)
 (synopsis "Eio-native observability handle: tracing, structured logging, and metrics")
 (description "\
A small Eio-native observability event model — distributed tracing (W3C traceparent),
structured logging, and metrics (counter/gauge/histogram) behind a single
capability-passed handle, with pluggable backends. Not a replacement for every OCaml
observability package: it is a compact core that backend packages such as
obs-loki-eio and obs-prometheus-eio adapt to real exporters.")
 (maintainers "lbendtlynielsen@gmail.com")
 (authors "lbendtly")
 (license "MIT")
 (homepage "https://github.com/loganbnielsen/obs-eio")
 (bug_reports "https://github.com/loganbnielsen/obs-eio/issues")
 (source (github loganbnielsen/obs-eio))
 (depends
  (ocaml (>= 5.4.0))
  (eio (>= 1.3))
  (mtime (>= 2.1.0))
  (eio_main (and (>= 1.3) :with-test))
  (alcotest :with-test)
  (odoc :with-doc)))
