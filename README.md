# call-ubench

Microbenchmarks for wasm function calls, collected from various
sources.  See run.sh for some rudimentary information.

The fib benchmarks are newer, they take a principled approach to
exploring all the cases.

The sub-add benchmarks are older, they are a little ad-hoc still, and
there are some TODOs.

General TODOs:

- in email, Luke proposes: "And just to try to control for the branch
  predictor's role in this, it'd be lovely to have the numbers for a
  fixed callee (which is presumably well-predicted) and a
  randomly-variable callee (say, by having N callees in a table and
  then skipping through the table randomly in a way to thwart any
  prediction based on callsite history)."

