{ fetchurl
, python27Packages
}:
python27Packages.buildPythonPackage {
    name = "yahoo-finance-1.2.1";
    src = fetchurl {
        url = https://pypi.python.org/packages/c2/57/d6fc70ca79a0ebcd29ab2204a23c79aecaaa44fe21bf79ca28a6a3c03cbe/yahoo-finance-1.2.1.zip;
        md5 = "550cd91e3e2ddb95770e7346dcd76316";
    };
    propagatedBuildInputs = [ python27Packages.pytz
                              python27Packages.simplejson
                            ];
}
