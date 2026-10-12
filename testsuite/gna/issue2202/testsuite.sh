#! /bin/sh

. ../../testenv.sh

# The component configuration has no binding indication, so it contains an
# implicit default binding indication (LRM08 3.4.3): dut is fully bound.
analyze repro.vhdl
elab_simulate repro_cfg

clean

echo "Test successful"
