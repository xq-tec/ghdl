#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze pkgs.vhdl
if $GHDL --version | grep -q mcode; then
  # The implicit generic map of DEFAULT must not leave unreferenced nodes.
  # GHDL's simulator does not elaborate formal generic packages with a
  # generic map aspect (other than <>), so only analyze.
  analyze --check-ast tb.vhdl
else
  analyze tb.vhdl
fi

analyze_failure err_constant.vhdl
analyze_failure err_type.vhdl
analyze_failure err_array_type.vhdl
analyze_failure err_subprogram.vhdl
analyze_failure err_package.vhdl
analyze_failure err_default_type.vhdl
analyze_failure err_default_value.vhdl
analyze_failure err_default_unassociated.vhdl
analyze_failure err_formal_map.vhdl
analyze_failure err_not_instance.vhdl

clean

echo "Test successful"
