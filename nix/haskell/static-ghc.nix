{ ghcName }:
(
  final: prev:
  {
    haskell = prev.haskell // {
      compiler = prev.haskell.compiler // {
        ${ghcName} = prev.haskell.compiler.${ghcName}.override {
          enableRelocatedStaticLibs = true;
          enableShared = false;
          enableDwarf = false;
        };
      };
    };
  }
)
