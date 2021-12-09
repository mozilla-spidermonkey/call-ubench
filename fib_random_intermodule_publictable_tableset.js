/* Author: Lars T Hansen, Mozilla */

// INFO: Doubly-recursive fib(40) with indirect calls via a public table to a set of foreign-module functions, tables initialized by Table.set

function makeFib(calltable) {
    return new WebAssembly.Instance(new WebAssembly.Module(wasmTextToBinary(`
(module

  (type $ty (func (param i32) (result i32)))
  (import "" "calltable" (table $t 8 funcref))

  ;; Random indices into the function table, indexed by the fib argument
  (memory 1)
  (data (i32.const 0) (i8 4 1 5 6 7 4 2 2 1 2 6 7 0 5 3 5 5 4 6 2 0 6 0 3 2 5 3 7 0 4 6 5 2 2 5 0 2 2 5 7 0))
  (data (i32.const 100) (i8 0 4 7 1 1 7 0 2 7 6 5 3 7 2 4 6 6 0 5 4 5 3 4 4 3 6 7 1 4 5 5 4 4 0 5 7 1 0 7 7 1))

  (func $fib0 (export "fib0") (param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
        (local.get $n)
        (i32.add (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 1)) (i32.load8_u (local.get $n)))
                 (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 2)) (i32.load8_u offset=100 (local.get $n))))))

  (func $fib1 (export "fib1") (param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
        (local.get $n)
        (i32.add (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 1)) (i32.load8_u (local.get $n)))
                 (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 2)) (i32.load8_u offset=100 (local.get $n))))))

  (func $fib2  (export "fib2")(param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
        (local.get $n)
        (i32.add (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 1)) (i32.load8_u (local.get $n)))
                 (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 2)) (i32.load8_u offset=100 (local.get $n))))))

  (func $fib3 (export "fib3") (param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
        (local.get $n)
        (i32.add (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 1)) (i32.load8_u (local.get $n)))
                 (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 2)) (i32.load8_u offset=100 (local.get $n))))))

  (func $fib4 (export "fib4") (param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
        (local.get $n)
        (i32.add (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 1)) (i32.load8_u (local.get $n)))
                 (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 2)) (i32.load8_u offset=100 (local.get $n))))))

  (func $fib5 (export "fib5") (param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
        (local.get $n)
        (i32.add (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 1)) (i32.load8_u (local.get $n)))
                 (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 2)) (i32.load8_u offset=100 (local.get $n))))))

  (func $fib6 (export "fib6") (param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
        (local.get $n)
        (i32.add (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 1)) (i32.load8_u (local.get $n)))
                 (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 2)) (i32.load8_u offset=100 (local.get $n))))))

  (func $fib7 (export "fib7") (param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
        (local.get $n)
        (i32.add (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 1)) (i32.load8_u (local.get $n)))
                 (call_indirect (table $t) (type $ty) (i32.sub (local.get $n) (i32.const 2)) (i32.load8_u offset=100 (local.get $n))))))
)
`)),
                                    {"":{calltable}});
}

var table0 = new WebAssembly.Table({initial:8, element:"funcref"})
var table1 = new WebAssembly.Table({initial:8, element:"funcref"})
var Fib1 = makeFib(table0);
var Fib0 = makeFib(table1);
for ( let i=0 ; i < 8; i++ ) {
  table0.set(i, Fib0.exports["fib" + i]);
  table1.set(i, Fib1.exports["fib" + i]);
}
assertEq(Fib0.exports.fib0(10), 55);
assertEq(Fib1.exports.fib0(10), 55);

var runs = (function () { let k = Number(os.getenv("RUNS")); return isNaN(k) || k < 0 ? 1 : k })();
while (runs-- > 0) {
    var then = Date.now();
    Fib0.exports.fib0(40);
    var now = Date.now();
    print("fib_random_intermodule_publictable_tableset " + (now - then));
}
