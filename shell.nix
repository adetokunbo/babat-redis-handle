let
  project = import ./default.nix;
in
  project.shellFor {
    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = true;
    inherit (project) index-state;

    # Some common tools can be added with the `tools` argument
    tools = {
      cabal = "3.6.2.0";
      hlint = "latest"; # Selects the latest version in the hackage.nix snapshot
      haskell-language-server = "latest";
      ghcid = "latest";
      fourmolu = {
        version = "latest";
        modules = [
          ({ lib, ... }: {
            options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "Cabal"; };
          })
        ];
      };
    };
    # See overlays/tools.nix for more details

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }