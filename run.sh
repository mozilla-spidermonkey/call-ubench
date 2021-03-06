# Optionally define the environment variable RUNS to be the number of
# runs per test, defaults to 1.
#
# Note that DEBUG shells emit debugging code into the machine code and
# even wasm-bound programs will be slower than in non-DEBUG shells.
#SHELL="numactl --cpunodebind 0 $HOME/m-u/obj-release/dist/bin/js"
SHELL="numactl --cpunodebind 0 $HOME/aux/m-u/obj-release/dist/bin/js"
#SHELL="numactl --cpunodebind 1 $HOME/moz/tmp3/js"
OPTIONS=--wasm-compiler=ion
for testcase in $1*.js; do
    $SHELL $OPTIONS $testcase
done

