{ system ? builtins.currentSystem or "x86_64-linux"
, ghc ? "ghc9122"
}:

let
  nix = import ./nix;
  pkgs = nix.pkgSetForSystem system {
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };
  inherit (pkgs) lib;
  hsPkgSetOverlay = pkgs.callPackage ./nix/haskell/overlay.nix {
    inherit (nix) sources;
  };

  importGit = drv:
    drv.overrideAttrs (oa: {
      buildInputs = (oa.buildInputs or []) ++ [pkgs.git];
    });

  sources = [
    "^(trace-embrace.yaml|app|src|test).*$"
    "^changelog[.]md$"
    "^.*\\.cabal$"
  ];

  base = hsPkgs.callCabal2nix "git-phoenix" (lib.sourceByRegex ./. sources) { };
  git-phoenix-overlay = _hf: _hp: { git-phoenix = importGit base; };
  baseHaskellPkgs = pkgs.haskell.packages.${ghc};
  hsOverlays = [ hsPkgSetOverlay git-phoenix-overlay ];
  hsPkgs = baseHaskellPkgs.override (old: {
    overrides =
      builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      hsOverlays;
  });

  # hls = pkgs.haskell.lib.overrideCabal hsPkgs.haskell-language-server
  #    (_: { enableSharedExecutables = true; });

  shell = hsPkgs.shellFor {
    packages = p: [ p.git-phoenix ];
    nativeBuildInputs = (with pkgs; [
      cabal-install
      ghcid
      hlint
      niv
      pandoc
      git
    ]);
    ## ++ [ hls hsPkgs.upload-doc-to-hackage ];
    shellHook = ''
      export PS1='$ '
      echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref
    '';
  };

  git-phoenix = hsPkgs.git-phoenix;
in {
  inherit hsPkgs;
  inherit ghc;
  inherit pkgs;
  inherit shell;
  inherit git-phoenix;
}
