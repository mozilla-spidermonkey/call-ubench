# call-ubench

Microbenchmarks for wasm function calls, collected from various
sources.  See run.sh for some rudimentary information.

The fib benchmarks are newer, they take a principled approach to
exploring all the cases.

The sum-add benchmarks are older, they are a little ad-hoc still, and
there are some TODOs. Use with care.
* Note, sum-add/private-table/same-module has been observed to have some very nasty microarchitectural artifacts on Intel systems (both a Xeon and an i7).  Beware.

To see summary information about the programs, `grep INFO *.js`
