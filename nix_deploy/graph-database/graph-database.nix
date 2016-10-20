{ stdenv
, boost
, zeromq4
}:
stdenv.mkDerivation {
    name = "graph-database-0.1.0.0";
    src = ../../backend/graph-database;
    buildInputs = [ boost.all
                    zeromq4
                  ];
    builder = ./builder.sh;
}
