{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base
      , base64-bytestring, binary, bytestring, cereal, conduit-extra
      , containers, data-default, directory, exceptions
      , filepath, hslogger, hspec, lens, monad-control
      , monad-loops, mtl, optparse-applicative, random
      , stdenv, stm, text
      , text-format, time, time-units, transformers, transformers-base
      , tuple, unordered-containers, vector
      , git, zlib, openssh, autoreconfHook, pkgconfig
      }:
      mkDerivation {
        pname = "rscoin";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        executableHaskellDepends = [
          async base
          base64-bytestring binary bytestring cereal conduit-extra
          containers data-default directory exceptions
          filepath hslogger hspec lens monad-control
          monad-loops mtl optparse-applicative random
          stdenv stm text
          text-format time time-units transformers transformers-base
          tuple unordered-containers vector
          autoreconfHook
        ];
        libraryPkgconfigDepends = 
          [ zlib git openssh autoreconfHook pkgconfig];
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
