#! /bin/sh

. ../../testenv.sh

# risim-ghdl: any prefix of an array type is allowed for array attributes
# (LRM08 8.1), not only object names.
export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro
analyze crc_pkg.vhdl
analyze wbcrc_syn.vhdl

export GHDL_STD_FLAGS="--std=08 -frelaxed"
analyze repro.vhdl
elab_simulate repro
analyze crc_pkg.vhdl wbcrc_syn.vhdl

clean

echo "Test successful"
