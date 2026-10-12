#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze pkg.vhdl
analyze tb.vhdl
elab_simulate tb

analyze_failure err_out_on_in.vhdl
analyze_failure err_member_explicit.vhdl
analyze_failure err_member_default.vhdl
analyze_failure err_member_release.vhdl
analyze_failure err_whole.vhdl
analyze_failure err_nested.vhdl
analyze_failure err_elem_resolved.vhdl
analyze_failure err_record.vhdl
analyze_failure err_alias.vhdl
analyze_failure err_alias_subtype.vhdl

clean

echo "Test successful"
