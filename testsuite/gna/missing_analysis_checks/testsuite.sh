#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab_simulate tb
analyze ok_qualified_port.vhdl

analyze_failure err_pure_file.vhdl
analyze_failure err_pure_file_parent.vhdl
analyze_failure err_float_bounds.vhdl
analyze_failure err_port_unconstrained_actual.vhdl
analyze_failure err_generic_type_expression.vhdl
analyze_failure err_allocator_range.vhdl
analyze_failure err_protected_target.vhdl
analyze_failure err_case_matching_type.vhdl
analyze_failure err_select_matching_type.vhdl
analyze_failure err_context_clause.vhdl

rm -f missing_analysis_checks.dat
clean

echo "Test successful"
