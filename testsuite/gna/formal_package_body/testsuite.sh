#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
if $GHDL --version | grep -q mcode; then
  # The instantiated bodies must not leave unreferenced nodes.
  analyze --check-ast tb.vhdl
  elab_simulate --check-ast tb
  analyze --check-ast nested.vhdl
  elab_simulate --check-ast nested
  analyze --check-ast instances.vhdl
  # The elaboration of a macro-expanded entity leaves unreferenced nodes.
  elab_simulate instances
  # GHDL's simulator does not elaborate an instance of a package whose
  # generic subprogram is defaulted, so only analyze.
  analyze --check-ast default.vhdl
else
  analyze tb.vhdl
  elab_simulate tb
  analyze nested.vhdl
  elab_simulate nested
  analyze instances.vhdl
  elab_simulate instances
  analyze default.vhdl
fi

clean

echo "Test successful"
