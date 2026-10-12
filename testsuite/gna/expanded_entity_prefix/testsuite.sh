#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab_simulate tb

analyze_failure err_other_arch.vhdl
analyze_failure err_entity_stmt.vhdl
analyze_failure err_before_decl.vhdl

clean

export GHDL_STD_FLAGS=--std=93
analyze_failure err_93.vhdl

clean

echo "Test successful"
