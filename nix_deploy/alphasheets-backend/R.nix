{ stdenv
, fetchurl
, bzip2
, gfortran
, libjpeg
, libpng
, libtiff
, pcre
, perl
, xz
, zlib
, less
, icu
, pkgconfig
, bison
, which
, openblas
, curl
, tzdata
}:
stdenv.mkDerivation rec {
    name = "R-3.3.1";
    src = fetchurl {
        url = "http://cran.r-project.org/src/base/R-3/${name}.tar.gz";
        sha256 = "b93b7d878138279234160f007cb9b7f81b8a72c012a15566e9ec5395cfd9b6c1";
    };
    buildInputs = [ bzip2
                    gfortran
                    libjpeg
                    libpng
                    libtiff
                    pcre
                    perl
                    xz
                    zlib
                    less
                    icu
                    pkgconfig
                    bison
                    which
                    openblas
                    curl
                  ];
    patches = [ ./no-usr-local-search-paths.patch ];
    preConfigure = ''
      configureFlagsArray=(
        --disable-lto
        --without-recommended-packages
        --with-blas="-L${openblas}/lib -lopenblas"
        --with-lapack="-L${openblas}/lib -lopenblas"
        --without-readline
        --with-cairo
        --with-libpng
        --with-jpeglib
        --with-libtiff
        --with-system-zlib
        --with-system-bzlib
        --with-system-pcre
        --with-system-xz
        --with-ICU
        --without-x
        --enable-R-shlib
        AR=$(type -p ar)
        AWK=$(type -p gawk)
        CC=$(type -p gcc)
        CXX=$(type -p g++)
        FC="${gfortran}/bin/gfortran" F77="${gfortran}/bin/gfortran"
        RANLIB=$(type -p ranlib)
        R_SHELL="${stdenv.shell}"
      )
    '';
    installTargets = [ "install" "install-info" "install-pdf" ];
    doCheck = true;
    enableParallelBuilding = true;
    postFixup = ./r-postfixup.sh;
}
