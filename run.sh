# Optionally define the environment variable RUNS to be the number of
# runs per test, defaults to 1.
#
# Note that DEBUG shells emit debugging code into the machine code and
# even wasm-bound programs will be slower than in non-DEBUG shells.
SHELL=../../../../obj-release/dist/bin/js 
OPTIONS=--wasm-compiler=ion
for testcase in *.js; do
    $SHELL $OPTIONS $testcase
done

