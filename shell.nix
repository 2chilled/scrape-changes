let localPkgs = import <nixpkgs> {};
    pkgs =
      let p =
            localPkgs.fetchFromGitHub {
              owner = "NixOS";
              repo = "nixpkgs";
              rev = "e67416f7e2685ab6136863f96545a1c22f8a23cd";
              sha256 = "0j26cp99l8s3ybn3m8cnvc1ch76jvc7l1v5299w6dcszp9bqcawc";
            };
      in import p {};
    haskellPkgs = pkgs.haskellPackages;
in {
  scrapeChangesStackEnv = pkgs.haskell.lib.buildStackProject {
    name = "scrapeChangesStackEnv";
    buildInputs = with haskellPkgs; [
      stack pkgs.zlib hdevtools ghc-mod
    ];
    ghc = pkgs.haskell.packages.ghc802.ghc;
  };
}
