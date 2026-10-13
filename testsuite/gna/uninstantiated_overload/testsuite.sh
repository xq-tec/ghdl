#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
if $GHDL --version | grep -q mcode; then
  analyze --check-ast tb.vhdl
else
  analyze tb.vhdl
fi
elab_simulate tb

analyze_failure err_homograph.vhdl
analyze_failure err_homograph_body.vhdl
analyze_failure err_nonconforming.vhdl
analyze_failure err_generic_subprogram.vhdl
analyze_failure err_generic_default.vhdl
analyze_failure err_generic_package.vhdl

clean

echo "Test successful"
