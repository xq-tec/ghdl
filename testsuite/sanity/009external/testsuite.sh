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

  # The design export prints the type of each alias value.  For a view that
  # reshapes the object, that type must not dangle.
  analyze reshaped_view.vhdl
  elab_simulate reshaped_view --stop-time=1us
  "$GHDL" --design-to-json $GHDL_STD_FLAGS reshaped_view > reshaped_view.jsonl
  alias_types=$(sed -n 's/^{"value":{"id":[0-9]*,"val_kind":"alias","obj":[0-9]*,"type":\([0-9]*\),.*/\1/p' reshaped_view.jsonl)
  if [ -z "$alias_types" ]; then
    echo "no alias value exported"
    exit 1
  fi
  for t in $alias_types; do
    if ! grep -q "^{\"type\":{\"id\":$t,\"type_kind\":\"vector\"" reshaped_view.jsonl; then
      echo "alias type $t is not a vector"
      exit 1
    fi
  done
  rm -f reshaped_view.jsonl

  clean
fi

echo "Test successful"
