/* Author: Lars T Hansen, Mozilla */

// INFO: Doubly-recursive fib(40) with indirect calls via a private table to one same-module function, table initialized by elem, then processed by get/set

// The get/set pair should be performance-neutral but the initial landing of the
// call_indirect optimization makes it not so.

var ins = new WebAssembly.Instance(new WebAssembly.Module(wasmTextToBinary(`
(module
  (type $ty (func (param i32) (result i32)))
  (table $t 2 funcref)
  (elem $t (i32.const 0) $fib)

  (func $fib (export "fib") (param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
        (local.get $n)
        (i32.add (call_indirect (type $ty) (i32.sub (local.get $n) (i32.const 1)) (i32.const 0))
                 (call_indirect (type $ty) (i32.sub (local.get $n) (i32.const 2)) (i32.const 0)))))

  (func (export "getAndSet")
    (table.set (i32.const 0) (table.get (i32.const 0))))
)
`)));

ins.exports.getAndSet();
assertEq(ins.exports.fib(10), 55);

var runs = (function () { let k = Number(os.getenv("RUNS")); return isNaN(k) || k < 0 ? 1 : k })();
while (runs-- > 0) {
    var then = Date.now();
    ins.exports.fib(40);
    var now = Date.now();
    print("fib_fixed_intramodule_privatetable_elem_getset " + (now - then));
}
