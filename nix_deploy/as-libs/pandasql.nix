{ fetchurl
, python27Packages
}:
python27Packages.buildPythonPackage {
    name = "pandasql-0.7.3";
    src = fetchurl {
        url = https://pypi.python.org/packages/6b/c4/ee4096ffa2eeeca0c749b26f0371bd26aa5c8b611c43de99a4f86d3de0a7/pandasql-0.7.3.tar.gz;
        md5 = "6bfca10a075d587d0da0c3ada496d613";
    };
    propagatedBuildInputs = [ python27Packages.sqlalchemy
                              python27Packages.numpy
                              python27Packages.pandas
                            ];
}
