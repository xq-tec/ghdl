#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
if $GHDL --version | grep -q mcode; then
  # The signature of an instantiation must not leave unreferenced nodes.
  analyze --check-ast tb.vhdl
  elab_simulate --check-ast tb
else
  analyze tb.vhdl
  elab_simulate tb
fi

analyze_failure err_signature_mismatch.vhdl
analyze_failure err_overloaded.vhdl
analyze_failure err_overloaded_operator.vhdl
analyze_failure err_generic_type_selection.vhdl
analyze_failure err_not_a_name.vhdl

clean

echo "Test successful"
