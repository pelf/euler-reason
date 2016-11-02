echo "Cleaning up..."
rm ./_build/src/$1*

eval $(./node_modules/.bin/dependencyEnv)
nopam

echo "Building..."
rebuild -use-ocamlfind -I src ./src/$1.native 2>&1

echo "Running"...
./_build/src/$1.native 2>&1
