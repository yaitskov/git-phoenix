{ system ? builtins.currentSystem or "x86_64-linux"
, ghcName ? "ghc9122"
, staticBuild ? false
}:

let
  nix = import ./nix { inherit ghcName staticBuild; };
  originPkgs = nix.pkgSetForSystem system {
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };
  pkgs = if staticBuild then originPkgs.pkgsMusl else originPkgs;
  inherit (pkgs) lib;
  inherit (lib) strings;
  inherit (strings) concatStringsSep;
  staticExtraLibs = [
    "--ghc-option=-optl=-static"
    "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
    "--extra-lib-dirs=${pkgs.zlib.static}/lib"
    "--extra-lib-dirs=${pkgs.libelf.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
    "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
  ];
  hsPkgSetOverlay = pkgs.callPackage ./nix/haskell/overlay.nix {
    inherit (nix) sources;
  };
  assertStatic = drv:
    if staticBuild then
      drv.overrideAttrs(oa: {
        postInstall = (oa.postInstall or "") + ''
          for b in $out/bin/*
          do
            if ldd "$b"
            then
              echo "ldd succeeded on $b, which may mean that it is not statically linked"
              exit 1
            fi
          done
        '';})
    else drv;
  makeStatic = drv:
    drv.overrideAttrs(oa: {
      configureFlags =
        (oa.configureFlags or []) ++
        (if staticBuild then staticExtraLibs else []);
    });

  importGit = drv:
    drv.overrideAttrs (oa: {
      buildInputs = (oa.buildInputs or []) ++ [pkgs.git];
    });

  sources = [
    "^(trace-embrace.yaml|app|src|test).*$"
    "^(changelog[.]md|test-git-objects.*)$"
    "\\.git.*"
    "^.*\\.cabal$"
  ];

  base = hsPkgs.callCabal2nix "git-phoenix" (lib.sourceByRegex ./. sources) { };
  git-phoenix-overlay = _hf: _hp: { git-phoenix = assertStatic (makeStatic (importGit base)); };
  baseHaskellPkgs = pkgs.haskell.packages.${ghcName};
  hsOverlays = [ hsPkgSetOverlay git-phoenix-overlay ];
  hsPkgs = baseHaskellPkgs.override (old: {
    overrides =
      builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      hsOverlays;
  });

  shell = hsPkgs.shellFor {
    packages = p: [ p.git-phoenix ];
    nativeBuildInputs = (with pkgs; [
      cabal-install
      pandoc
      niv
      git
    ]) ++ [ hsPkgs.upload-doc-to-hackage ];
    shellHook =
      strings.concatStrings
        [''export PS1='$ '
           echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref
         ''
         (if staticBuild
          then ''
                 function cabal() {
                   case $1 in
                     build|test) ${pkgs.cabal-install.out}/bin/cabal "$@" \
                               ${concatStringsSep " " staticExtraLibs} ;;
                     *) ${pkgs.cabal-install.out}/bin/cabal "$@" ;;
                   esac
                 }
               ''
          else ""
         )];
  };

  git-phoenix = hsPkgs.git-phoenix;
in {
  inherit hsPkgs;
  inherit ghcName;
  inherit pkgs;
  inherit shell;
  inherit git-phoenix;
}
