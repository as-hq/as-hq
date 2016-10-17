{ fetchurl
, python27Packages
}:
python27Packages.buildPythonPackage {
    name = "statistics-1.0.3.5";
    src = fetchurl {
        url = https://pypi.python.org/packages/bb/3a/ae99a15e65636559d936dd2159d75af1619491e8cb770859fbc8aa62cef6/statistics-1.0.3.5.tar.gz;
        md5 = "d6d97f3a1a7b3192cff99e0f2b5956c3";
    };
    propagatedBuildInputs = [ python27Packages.docutils
                            ];
    #tests seem to be busted.
    doCheck = false;
}
