# EVM decompiler

A simple EVM decompiler that I wrote while studying EVM internals.

## Building from source

```bash
git clone https://github.com/jubnzv/evm-decompiler.git
cd evm-decompiler
opam install --deps-only .    # first time only
dune build
```

## Usage

Compile some test files:

```bash
cd tests
./compile.py
```

Run the decompiler to see opcodes generated from the binaries:

```bash
_build/default/src/evm_decompiler.exe tests/*.bin
```
