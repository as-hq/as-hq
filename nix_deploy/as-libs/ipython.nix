{ fetchurl
, python27Packages
}:
python27Packages.buildPythonPackage {
    name = "ipython-4.0.0";
    src = fetchurl {
        url = https://pypi.python.org/packages/3a/e6/486285e4aa0d9b7c5cc074743ccad753370d42d99deda48c2267e3a3620d/ipython-4.0.0.tar.gz;
        md5 = "c2fecbcf1c0fbdc82625c77a50733dd6";
    };
    propagatedBuildInputs = [ python27Packages.simplegeneric
                              python27Packages.pickleshare
                              python27Packages.pexpect
                              python27Packages.decorator
                              python27Packages.traitlets

                            ];
}
