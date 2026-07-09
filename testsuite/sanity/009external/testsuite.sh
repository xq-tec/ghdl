#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
  export GHDL_STD_FLAGS=--std=08

  analyze ../../synth/external01/external01.vhdl
  elab_simulate external01 --stop-time=1us

  analyze ../../synth/external01/external02.vhdl
  elab_simulate external02 --stop-time=1us

  analyze ../../synth/external01/external05.vhdl
  elab_simulate external05 --stop-time=1us

  analyze ../../synth/external01/externalerr02.vhdl
  elab_simulate externalerr02 --stop-time=1us

  analyze package_path.vhdl
  elab_simulate package_path --stop-time=1us

  analyze nested_external.vhdl
  elab_simulate nested_external --stop-time=1us

  analyze ../../gna/issue520/alias.vhdl
  elab_simulate alias_tb --stop-time=1us

  analyze ../../gna/issue440/ent2.vhdl
  elab_simulate ent2 --stop-time=1us

  # issue520/lrm.vhdl is LRM commentary (syntax errors by design); not runnable.

  clean
fi

echo "Test successful"
