#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
if $GHDL --version | grep -q mcode; then
  # Merged component configurations must not leave shared or unreferenced
  # nodes.
  GHDL_FLAGS=--check-ast
fi

analyze tb.vhdl

elab_simulate all_noblk_tb
elab_simulate all_blk_tb
elab_simulate others_noblk_tb
elab_simulate others_blk_tb
elab_simulate list_noblk_tb
elab_simulate list_blk_tb
elab_simulate list_map_tb
elab_simulate two_blk_tb
elab_simulate two_list_map_tb
elab_simulate all_spec_blk_tb
elab_simulate all_spec_map_tb

analyze nested.vhdl
elab_simulate nested

analyze aspects.vhdl
elab_simulate aspects

analyze_failure err_mixed_map.vhdl
analyze_failure err_open_map.vhdl

clean

echo "Test successful"
