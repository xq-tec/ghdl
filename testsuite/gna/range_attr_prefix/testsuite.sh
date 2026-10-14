#! /bin/sh

. ../../testenv.sh

ERRS="err_left err_right err_high err_low err_ascending err_object"

# A range prefix is an error in strict mode.
for std in 93 08; do
  export GHDL_STD_FLAGS=--std=$std
  for f in $ERRS; do
    analyze_failure $f.vhdl 2> log.err
    grep -q "error: prefix of .* attribute must not be a range" log.err
  done
  clean
done

# It is a warning with -frelaxed (implied by --std=93c).
for flags in "--std=93c" "--std=08 -frelaxed"; do
  export GHDL_STD_FLAGS="$flags"
  for f in $ERRS; do
    analyze $f.vhdl 2> log.err
    grep -q "warning: prefix of .* attribute must not be a range" log.err
  done
  analyze tb.vhdl
  elab_simulate tb
  clean
done

rm -f log.err

echo "Test successful"
