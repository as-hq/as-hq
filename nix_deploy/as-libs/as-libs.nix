{ callPackage
, python27Packages
}:
let pysqldf       = callPackage (import ./pysqldf.nix) {};
    statistics    = callPackage (import ./statistics.nix) {};
    shortid       = callPackage (import ./shortid.nix) {};
    wget          = callPackage (import ./wget.nix) {};
    zmq           = callPackage (import ./zmq.nix) {};
    pandasql      = callPackage (import ./pandasql.nix) {};
    yahoo-finance = callPackage (import ./yahoo-finance.nix) {};
    ipython       = callPackage (import ./ipython.nix) {};
in python27Packages.buildPythonPackage {
    name = "as-libs-0.1.4";
    src = ../../backend/as-libs/py;
    propagatedBuildInputs = [ python27Packages.traitlets
                              python27Packages.numpy
                              python27Packages.matplotlib
                              python27Packages.pandas
                              python27Packages.scipy
                              python27Packages.pyzmq
                              python27Packages.colour
                              python27Packages.openpyxl
                              python27Packages.six
                              python27Packages.sqlalchemy
                              python27Packages.beautifulsoup4
                              python27Packages.ujson
                              pysqldf
                              statistics
                              shortid
                              wget
                              zmq
                              pandasql
                              yahoo-finance
                              ipython
                            ];
}
