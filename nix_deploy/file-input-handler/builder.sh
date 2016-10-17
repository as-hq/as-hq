source $stdenv/setup
cp -R $src ./
chmod -R 0777 ./*static*
cd ./*static*
mkdir $out
mkdir $out/bin
cp ./file-input-handler.py $out/bin
