{ callPackage
, haskell
, haskellPackages
, stdenv
, stack
, git
, cacert
, pkgconfig
, zlib
, which
, zeromq4
, wget
, glibcLocales
}:
let r   = callPackage (import ./R.nix) { };
    ghc = haskell.packages.ghc7102.ghcWithPackages (pkgs: [pkgs.shelly]);
in stdenv.mkDerivation {
    name = "alphasheets-0.1.0.0";
    src = ../../backend/server;
    buildInputs = [ stack
                    git
                    cacert
                    pkgconfig
                    zlib
                    haskellPackages.c2hs
                    which
                    zeromq4
                    wget
                    glibcLocales
                    ghc
                    r
                  ];
    LD_LIBRARY_PATH="${zlib}/lib:${zeromq4}/lib:${r}/lib/R/lib";
    GHC_PACKAGE_PATH="${ghc}/lib/${ghc.name}/package.conf.d";
    SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt";
    LANG="en_US.UTF-8";
    builder = ./builder.sh;
    shellHook = ''
        export STACK_ROOT=$(realpath ../../backend/server/.stack-work)
    '';
}
