{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    nexo = ./.;
    web = ./gui/web;
  };

  shells = {
    ghc = ["nexo" "web"];
    ghcjs = ["nexo" "web"];
  };

  shellToolOverrides = self: super: {
    inherit (pkgs.haskell.packages.ghc865) haskell-language-server;
  };

  # overrides = self: super: {
  #   megaparsec = self.callHackage "megaparsec" "9.0.1" {};
  #   recursion-schemes = self.callHackage "recursion-schemes" "5.2.2.1" {};
  #   tasty = self.callHackage "tasty" "1.4.1" {};
  # };
})
