set root (realpath (dirname (status --current-filename)))/..
cd $root/examples
spago bundle-app --main $argv[1] --to $root/bundle/$argv[2]/bundle.js
and cd $root
and npx parcel $root/examples/$argv[2]/index.html
