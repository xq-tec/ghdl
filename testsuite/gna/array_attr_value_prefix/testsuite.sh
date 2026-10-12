#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab_simulate tb

analyze_failure err_scalar.vhdl
analyze_failure err_scalar_attr.vhdl
analyze_failure err_static_call.vhdl
analyze_failure err_static_name.vhdl

clean

echo "Test successful"
