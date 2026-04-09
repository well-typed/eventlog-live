{ hlib, ... }:
final: prev: with hlib; {

  eventlog-live = final.callPackage ./eventlog-live.nix { };
  eventlog-live-otelcol = final.callPackage ./eventlog-live-otelcol.nix { };
  oddball = final.callPackage ./oddball.nix { };

  # Jailbreak proto-lens to allow it to build with newer dependencies
  grapesy = dontCheck prev.grapesy;
  http2 = prev.callHackage "http2" "5.3.9" { };
  http2-tls = prev.callHackage "http2-tls" "0.4.5" { };
  eventlog-socket = prev.callHackage "eventlog-socket" "0.1.2.0" { };
  eventlog-socket-control = prev.callHackage "eventlog-socket-control" "0.1.0.0" { };
  optparse-applicative = prev.callHackage "optparse-applicative" "0.19.0.0" { };
  proto-lens = doJailbreak prev.proto-lens;
  proto-lens-protobuf-types = doJailbreak prev.proto-lens-protobuf-types;
  proto-lens-protoc = prev.callHackage "proto-lens-protoc" "0.9.0.0" { };
  snappy-c = doJailbreak prev.snappy-c;
  tasty-quickcheck = doJailbreak prev.tasty-quickcheck;
  tls = prev.callHackage "tls" "2.1.4" { };
}
