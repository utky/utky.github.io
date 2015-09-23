with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, hakyll
             }:
             mkDerivation {
               pname = "utly.github.io";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [
                 base hakyll
               ];
               description = "A command line tool which provides pomodoro techniques";
               license = stdenv.lib.licenses.asl20;
             }) {};
in
  pkg.env
