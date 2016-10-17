source $stdenv/setup
echo "builder.sh:"
cp -r $src ./
chmod -R 0777 ./
cd ./*frontend*
rm -rf ./dist/js
rm -rf ./node_modules
mkdir temphome
export HOME=`realpath ./temphome`
npm install
bower install
gulp prod-build
cp -r ./dist $out/
