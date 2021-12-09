/* Author: Lars T Hansen, Mozilla */

// INFO: Doubly-recursive fib(40) with direct local calls

var ins = new WebAssembly.Instance(new WebAssembly.Module(wasmTextToBinary(`
(module
  (func $fib (export "fib") (param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
        (local.get $n)
        (i32.add (call $fib (i32.sub (local.get $n) (i32.const 1)))
                 (call $fib (i32.sub (local.get $n) (i32.const 2)))))))
`)));

assertEq(ins.exports.fib(10), 55);

var runs = (function () { let k = Number(os.getenv("RUNS")); return isNaN(k) || k < 0 ? 1 : k })();
while (runs-- > 0) {
    var then = Date.now();
    ins.exports.fib(40);
    var now = Date.now();
    print("fib_direct " + (now - then));
}



