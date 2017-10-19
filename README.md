# crypto

instalá [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

$ stack setup
$ stack build

para buildear con profiling

$ stack build --profile

para correr

$ stack exec crypto-exe

y con profiling activado

$ stack exec -- crypto-exe +RTS -p
