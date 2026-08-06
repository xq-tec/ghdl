#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab_simulate tb

analyze_failure err_element.vhdl

clean

# VHDL-93 still rejects discrete ranges in targets (and must not crash).
export GHDL_STD_FLAGS=--std=93
analyze_failure err_93.vhdl

clean

echo "Test successful"
