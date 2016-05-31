let pkgs = import <nixpkgs> {};
    haskellPkgs = pkgs.haskellPackages;
in {
  scrapeChangesStackEnv = pkgs.haskell.lib.buildStackProject {
    name = "scrapeChangesStackEnv";
    buildInputs = with haskellPkgs; [
      hasktags stack hdevtools ghc-mod pkgs.zlib
    ]; 
  };
}
