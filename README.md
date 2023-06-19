# Aeroon

Aeroon provides an OCaml bindings for
[Aeron](https://github.com/real-logic/aeron), using [Aeron's C
API](https://github.com/real-logic/aeron/blob/master/aeron-client/src/main/c/aeronc.h)

To build it, first install and build
[Aeron](https://github.com/real-logic/aeron) by following the
instructions found there. Then clone the
[aeroon](https://github.com/onechronos/aeroon) repository:
```sh
git clone https://github.com/onechronos/aeroon.git
cd aeroon
```
Finally, build and install
[aeroon](https://github.com/onechronos/aeroon):
```sh
export AERON_ROOT=/path/to/aeron
opam pin add .
```
or
```sh
export AERON_ROOT=/path/to/aeron
dune build
dune install
```
Here, `/path/to/aeron` must be replaced with the path of the directory
where the [Aeron](https://github.com/real-logic/aeron) repository
had been previously installed.

Refer to the
[test](https://github.com/onechronos/aeroon/tree/master/test)
directory for usage examples that are OCaml translations of some of
the [C
examples](https://github.com/real-logic/aeron/tree/master/aeron-samples/src/main/c)
found in the [Aeron](https://github.com/real-logic/aeron) repository.
Note that to run these examples (indeed, any OCaml program using
[aeroon](https://github.com/onechronos/aeroon)), one has to tell the
system where to find the [Aeron](https://github.com/real-logic/aeron)
shared object library `libaeron.so`. For example:
```sh
export LD_LIBRARY_PATH=/path/to/aeron/build/lib
```
