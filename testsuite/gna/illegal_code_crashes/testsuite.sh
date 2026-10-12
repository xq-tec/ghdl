#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab_simulate tb

analyze_failure err_resolution_overload.vhdl
analyze_failure err_array_res_on_record.vhdl
analyze_failure err_record_res_element.vhdl
analyze_failure err_file_constraint.vhdl
analyze_failure err_base_type_mark.vhdl
analyze_failure err_group_attribute.vhdl
analyze_failure err_group_function_prefix.vhdl
analyze_failure err_event_index.vhdl
analyze_failure err_record_aggregate_range.vhdl
analyze_failure err_generic_subprogram_actual.vhdl
analyze_failure err_pathname_ambiguous.vhdl

clean

echo "Test successful"
