#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab_simulate tb

analyze incremental.vhdl
elab_simulate incremental

analyze all_default.vhdl
elab_simulate all_default

analyze later_arch.vhdl
elab_simulate later_arch

analyze_failure err_not_latest_arch.vhdl
analyze_failure err_mixed_all.vhdl
analyze_failure err_mixed_list.vhdl
analyze_failure err_mixed_default.vhdl

clean

echo "Test successful"
