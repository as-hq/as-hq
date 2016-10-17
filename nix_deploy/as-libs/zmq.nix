{ fetchurl
, python27Packages
}:
python27Packages.buildPythonPackage {
    name = "zmq-0.0.0";
    src = fetchurl {
        url = https://pypi.python.org/packages/6e/78/833b2808793c1619835edb1a4e17a023d5d625f4f97ff25ffff986d1f472/zmq-0.0.0.tar.gz;
        md5 = "a3c66682bf81501314fb35f47fac8f14";
    };
    propagatedBuildInputs = [ python27Packages.pyzmq
                            ];
}
