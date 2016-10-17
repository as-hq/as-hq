{ python27Packages
, fetchurl
}:
python27Packages.buildPythonPackage {
    name = "wget-3.2";
    src = fetchurl {
        url = https://pypi.python.org/packages/47/6a/62e288da7bcda82b935ff0c6cfe542970f04e29c756b0e147251b2fb251f/wget-3.2.zip;
        md5 = "fb7a04ee59a56bd8db974146df1d7a8b";
    };
}
