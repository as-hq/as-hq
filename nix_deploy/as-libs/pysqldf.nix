{ fetchurl
, python27Packages
}:
python27Packages.buildPythonPackage {
    name = "pysqldf-1.2.3";
    src = fetchurl {
        url = https://pypi.python.org/packages/f8/57/d53fc768d8a3ed534cd670d033e342ce07b901f636491790a0c9fbb63743/pysqldf-1.2.3.tar.gz;
        md5 = "3a35914a1e4fe21cb716fb792cf18129";
    };
    propagatedBuildInputs = [ python27Packages.pandas
                            ];
}
