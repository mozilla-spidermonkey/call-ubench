/* Author: Lars T Hansen, Mozilla */

// INFO: Doubly-recursive fib(40) with indirect calls via a public table to one foreign-module function, table initialized by elems

function makeFib(index, calltable) {
    return new WebAssembly.Instance(new WebAssembly.Module(wasmTextToBinary(`
(module
  (type $ty (func (param i32) (result i32)))
  (import "" "calltable" (table $t 2 funcref))

  (elem (table $t) (i32.const ${index ^ 1}) $fib)

  (func $fib (export "fib") (param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
        (local.get $n)
        (i32.add (call_indirect (type $ty) (i32.sub (local.get $n) (i32.const 1)) (i32.const ${index}))
                 (call_indirect (type $ty) (i32.sub (local.get $n) (i32.const 2)) (i32.const ${index}))))))
`)),
                                    {"":{calltable}});
}

var table = new WebAssembly.Table({initial:2, element:"funcref"})
var Fib0 = makeFib(1, table);
var Fib1 = makeFib(0, table);

assertEq(Fib0.exports.fib(10), 55);

var runs = (function () { let k = Number(os.getenv("RUNS")); return isNaN(k) || k < 0 ? 1 : k })();
while (runs-- > 0) {
    var then = Date.now();
    Fib0.exports.fib(40);
    var now = Date.now();
    print("fib-initelems/public-table/cross-module " + (now - then));
}



