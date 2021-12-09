/* Author: Lars T Hansen, Mozilla */

// INFO: Doubly-recursive fib(40) with indirect calls via a private table to one unknown same-module function, table initialized by elem

var ins = new WebAssembly.Instance(new WebAssembly.Module(wasmTextToBinary(`
(module
  ;; The memory has all zeroes, and this is important.  We load the call index from
  ;; memory to have parity with the "random" case.
  (memory 1)

  (type $ty (func (param i32) (result i32)))
  (table $t 2 funcref)
  (elem $t (i32.const 0) $fib)

  (func $fib (export "fib") (param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
        (local.get $n)
        (i32.add (call_indirect (type $ty) (i32.sub (local.get $n) (i32.const 1)) (i32.load8_u (local.get $n)))
                 (call_indirect (type $ty) (i32.sub (local.get $n) (i32.const 2)) (i32.load8_u offset=100 (local.get $n)))))))
`)));

assertEq(ins.exports.fib(10), 55);

var runs = (function () { let k = Number(os.getenv("RUNS")); return isNaN(k) || k < 0 ? 1 : k })();
while (runs-- > 0) {
    var then = Date.now();
    ins.exports.fib(40);
    var now = Date.now();
    print("fib_unknown_intramodule_privatetable_elem " + (now - then));
}
