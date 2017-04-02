# Programming language written in [Rust](https://github.com/rust-lang/rust)
## Requirement
  - Rust nightly
  - LLVM 3.9

## Building(OS X)
  1. install rustup from https://www.rust-lang.org/en-US/install.html.
  2. rustup toolchain install nightly 
  3. brew install llvm@3.9
  4. brew link --force llvm@3.9
  3. cargo build

## Usage
```
Usage: target/debug/rusk FILE [options]

Options:
    -h, --help          print this help menu
    -l, --llvm          generate LLVM IR
    -p, --parse         parse source code and show AST
```
Now, parse program and generate LLVM IR only.

## value
### integer 
ex.)
```
123
0b01010101
0o777
0xdeadbeef
```

### floating point number
ex.)
```
1.5
2.5e2
```

### array
ex.)
```
[1, 2, 3]
```

### tuple
ex.)
```
{1, 2.5, 3}
```

## arithmetic
```
1 + 1
1 - 1
1 * 1
1 / 1
```
## comparison
```
1 < 1
1 <= 1
1 == 1
1 != 1
1 > 1
1 >= 1
```

## assignment
ex.)
```
i = 1
```

## indexing
ex.)
```
a = {1, 2, 3}
a.0 
```

## if expression
ex.)
```
if a == 2 {
    a + 1
}
else {
    a + 2
}
```

## while expression
ex.)
```
while a < 10 {
    sum = sum + a
}
```
There are no control statement. continue, break, return　etc.

## type
```
Int
Float
```
There are no type description for Array and Tuple.

## function definition
```
func name(arg: Int) -> Int{
    arg + 1
}
```

## function call
```
name(1)
```

## examples
see [program](https://github.com/nmasuko/rusk/program)

## using crates
[rust-peg](https://github.com/kevinmehall/rust-peg)
[llvmsys.rs](https://bitbucket.org/tari/llvm-sys.rs/)