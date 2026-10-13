#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
if $GHDL --version | grep -q mcode; then
  # The resolution indications move to the element subtypes: no node may be
  # owned twice or left unreferenced.
  analyze --check-ast tb.vhdl
  analyze --check-ast conformance.vhdl
  analyze --check-ast sim.vhdl
  elab_simulate --check-ast sim
  analyze --check-ast sim_generic.vhdl
  elab_simulate --check-ast sim_generic
else
  analyze tb.vhdl
  analyze conformance.vhdl
  analyze sim.vhdl
  elab_simulate sim
  analyze sim_generic.vhdl
  elab_simulate sim_generic
fi
# GHDL's simulator does not handle a new resolution of a composite element
# that is bounded in the record type: tb.vhdl is only analyzed.

analyze_failure err_duplicate.vhdl
analyze_failure err_nested_element.vhdl
analyze_failure err_element_function.vhdl
analyze_failure err_conformance.vhdl
analyze_failure err_conformance_element.vhdl
analyze_failure err_access_element.vhdl

clean

echo "Test successful"
