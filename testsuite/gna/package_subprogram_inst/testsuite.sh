#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
if $GHDL --version | grep -q mcode; then
  # The instantiated declarations and bodies must not leave unreferenced
  # nodes.
  analyze --check-ast tb.vhdl
  elab_simulate --check-ast tb
else
  analyze tb.vhdl
  elab_simulate tb
fi

clean

echo "Test successful"
