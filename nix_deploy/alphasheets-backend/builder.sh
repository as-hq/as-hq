source $stdenv/setup
env
cp -r $src ./
chmod -R 0777 ./*server*
cd *server*
runhaskell ./nix-builder.hs
