#! /bin/sh

. ../../testenv.sh

# risim-ghdl: any prefix of an array type is allowed for array attributes
# (LRM08 8.1), not only object names.
export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl
analyze -frelaxed ent.vhdl

clean

echo "Test successful"
