#! /bin/sh

. ../../testenv.sh

for std in 93c 08; do
  export GHDL_STD_FLAGS=--std=$std
  analyze tb.vhdl
  elab_simulate tb
  clean
done

export GHDL_STD_FLAGS=--std=08
analyze tb08.vhdl
elab_simulate tb08
clean

for f in err_element_constraint err_parent_element err_type_element; do
  analyze_failure $f.vhdl 2> log.err
  grep -q "not static choice exclude others choice" log.err
done
clean

rm -f log.err

echo "Test successful"
