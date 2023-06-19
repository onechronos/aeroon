# Aeroon

Aeroon provides an OCaml bindings for
[Aeron](https://github.com/real-logic/aeron), using [Aeron's C
API](https://github.com/real-logic/aeron/blob/master/aeron-client/src/main/c/aeronc.h)

To build, first install [Aeron](https://github.com/real-logic/aeron),
then either:

```sh
export AERON_ROOT=/path/to/aeron
opam pin add .
```
or

```sh
export AERON_ROOT=/path/to/aeron
dune install
```

Refer to the [test](https://github.com/onechronos/aeroon/test)
directory for usage examples that are OCaml translations of some of
the
[C examples](https://github.com/real-logic/aeron/tree/master/aeron-samples/src/main/c)
found in the Aeron repository.

