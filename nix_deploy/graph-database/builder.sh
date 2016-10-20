source $stdenv/setup
cp -r $src ./
chmod -R 0777 ./*graph*
cd *graph*
make
mkdir $out
mkdir $out/bin
cp ./server $out/bin/graph-database
