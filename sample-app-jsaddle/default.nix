with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/bd8c216922f8a01d068a20e5f335761d6635807d.tar.gz";
  sha256 = "1g19jz32nah064b4948mlinv7jkj5840j3q4khx43i3iznf0rkwa";
}) {});
{
  dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. { miso = miso-jsaddle; };
  release = pkgs.haskell.packages.ghcjs86.callCabal2nix "app" ./. {};
  inherit pkgs;
}
