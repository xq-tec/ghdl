#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
if $GHDL --version | grep -q mcode; then
  # Folding an index or slice of 'IMAGE must not leave the folded string
  # unreferenced, and folding 'IMAGE, 'VALUE or an index must not share the
  # value of an enumeration literal or of a constant.
  analyze --check-ast tb.vhdl
  elab_simulate --check-ast tb
  analyze --check-ast contexts.vhdl
  elab_simulate --check-ast contexts
  analyze --check-ast image_param.vhdl
  elab_simulate --check-ast image_param
  analyze --check-ast image_bounds.vhdl
  elab_simulate_failure --check-ast image_bounds
  analyze --check-ast image_slice_bounds.vhdl
  elab_simulate_failure --check-ast image_slice_bounds
  analyze --check-ast image_slice_direction.vhdl
  elab_simulate_failure --check-ast image_slice_direction
else
  analyze tb.vhdl
  elab_simulate tb
  analyze contexts.vhdl
  elab_simulate contexts
  analyze image_param.vhdl
  elab_simulate image_param
  analyze image_bounds.vhdl
  elab_simulate_failure image_bounds
  analyze image_slice_bounds.vhdl
  elab_simulate_failure image_slice_bounds
  analyze image_slice_direction.vhdl
  elab_simulate_failure image_slice_direction
fi

analyze_failure err_stable_slice.vhdl
analyze_failure err_last_event_index.vhdl
analyze_failure err_last_value_element.vhdl
analyze_failure err_delayed_not_record.vhdl
analyze_failure err_delayed_out_port.vhdl

clean

echo "Test successful"
