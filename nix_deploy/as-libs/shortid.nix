{ python27Packages
, fetchurl
}:
python27Packages.buildPythonPackage {
    name = "shortid-0.1.0";
    src = fetchurl {
        url = https://pypi.python.org/packages/18/7a/0a958e9750fd63173445a5e9acbb7d0c1a05497e8eafae2cb00789684732/shortid-0.1.0.tar.gz;
        md5 = "f815b01c49d1c46c6d47f98d5b158e60";
    };
}
