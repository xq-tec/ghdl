#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
if $GHDL --version | grep -q mcode; then
  # The bodies appended to the protected type body must not leave
  # unreferenced nodes.
  analyze --check-ast tb.vhdl
  elab_simulate --check-ast tb
else
  analyze tb.vhdl
  elab_simulate tb
fi

analyze_failure err_missing_body.vhdl
analyze_failure err_body_after.vhdl
analyze_failure err_access_param.vhdl
analyze_failure err_access_return.vhdl
analyze_failure err_method_wait.vhdl

clean

echo "Test successful"
