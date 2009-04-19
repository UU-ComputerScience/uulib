{nixpkgs ? ../nixpkgs}:

let

  pkgs = import nixpkgs {};


  jobs = rec {

    tarball =
      { uulibSrc ? {outPath = ./.; rev = 1234;}
      , officialRelease ? false
      }:

      pkgs.releaseTools.makeSourceTarball {
        name = "uulib-tarball";
        version = builtins.readFile ./VERSION;
        src = uulibSrc;
        inherit officialRelease;
	distTarget = "source_tarball";
      };


    build =
      { tarball ? jobs.tarball {}
      , system ? "i686-linux"
      }:

      with import nixpkgs {inherit system;};

      haskellPackages.cabal.mkDerivation (self: {
        pname = "uulib";
        version = "head";
        src = "${tarball}/tarballs/*.tar.gz";
      });
  };

in jobs
