{ stdenv
, nodejs
, nodePackages
, git
}:
stdenv.mkDerivation {
    name = "alphasheets-frontend-0.1.0.0";
    src = ../../frontend;
    buildInputs = [ nodejs
                    nodePackages.npm
                    nodePackages.gulp
                    nodePackages.bower
                    git
                  ];
    builder = ./builder.sh;
}
