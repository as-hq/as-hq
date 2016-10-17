{ stdenv
, gnumake
}:
stdenv.mkDerivation {
    name = "graph-database-0.1.0.0";
    src = ../../backend/graph-database;
    buildInputs = [ 
                  ];
    builder = ./builder.sh;
}
