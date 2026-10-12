#! /bin/sh

. ../../testenv.sh

msg="type of a shared variable must be a protected type"

# VHDL-2008: a shared variable of a non-protected type is an error.
analyze_failure --std=08 tb.vhdl > ana.out 2>&1
grep -q "error: $msg" ana.out

# With -frelaxed, it is accepted silently.
analyze --std=08 -frelaxed tb.vhdl > ana.out 2>&1
if grep -q -e "$msg" -e "-Wshared" ana.out; then
  echo "unexpected diagnostic with -frelaxed"
  exit 1
fi
elab_simulate --std=08 -frelaxed tb

# -Wshared brings the warning back.
analyze --std=08 -frelaxed -Wshared tb.vhdl > ana.out 2>&1
grep -q "warning: $msg \[-Wshared\]" ana.out

# -Werror=shared turns it into an error.
analyze_failure --std=08 -frelaxed -Werror=shared tb.vhdl > ana.out 2>&1
grep -q "error: $msg" ana.out

# VHDL-93 has no such rule.
analyze --std=93 -Wshared tb.vhdl > ana.out 2>&1
if grep -q "$msg" ana.out; then
  echo "unexpected diagnostic with --std=93"
  exit 1
fi
elab_simulate --std=93 tb

clean --std=08
clean --std=93
rm -f ana.out

echo "Test successful"
