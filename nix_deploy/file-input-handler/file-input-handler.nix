{ stdenv
, python27
}:
stdenv.mkDerivation {
    name = "file-input-handler";
    src = ../../backend/server/static;
    buildInputs = [ python27
                  ];
    builder = ./builder.sh;
}
