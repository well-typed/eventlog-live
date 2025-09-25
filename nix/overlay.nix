pkgs: final: prev: {
  # Jailbreak proto-lens to allow it to build with newer dependencies
  proto-lens = pkgs.haskell.lib.doJailbreak prev.proto-lens;
  proto-lens-protoc = prev.callHackage "proto-lens-protoc" "0.9.0.0" {};
  proto-lens-protobuf-types = pkgs.haskell.lib.doJailbreak prev.proto-lens-protobuf-types;
  tls = prev.callHackage "tls" "2.1.4" {};
  http2-tls = prev.callHackage "http2-tls" "0.4.5" {};
  snappy-c = pkgs.haskell.lib.doJailbreak prev.snappy-c;
  grapesy = pkgs.haskell.lib.dontCheck prev.grapesy;
  http2 = prev.callHackage "http2" "5.3.9" {};
  optparse-applicative = prev.callHackage "optparse-applicative" "0.19.0.0" {};
  tasty-quickcheck = pkgs.haskell.lib.doJailbreak prev.tasty-quickcheck;
}
