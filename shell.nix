let pkgs = import <nixpkgs> {};
    haskellPkgs = pkgs.haskellPackages;
in {
  scrapeChangesStackEnv = pkgs.haskell.lib.buildStackProject {
    name = "scrapeChangesStackEnv";
    buildInputs = with haskellPkgs; [
      stack pkgs.zlib hasktags hdevtools ghc-mod
    ];
    ghc = pkgs.haskell.packages.ghc801.ghc;
  };
}
