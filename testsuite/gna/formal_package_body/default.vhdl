-- A formal generic package with a body and generic map (default) whose
-- uninstantiated package has a generic subprogram (LRM08 6.5.7.2).
package default_fns is
  function dbl (x : integer) return integer;
end package;

package body default_fns is
  function dbl (x : integer) return integer is
  begin
    return 2 * x;
  end function;
end package body;

package gen_default is
  generic (pure function f (x : integer) return integer is work.default_fns.dbl);
end package;

package user_default is
  generic (package p is new work.gen_default generic map (default));
  function apply (x : integer) return integer;
end package;

package body user_default is
  function apply (x : integer) return integer is
  begin
    return p.f (x) + 100;
  end function;
end package body;

entity tb_default is
end;

architecture behav of tb_default is
  package idef is new work.gen_default;
  package ud is new work.user_default generic map (p => idef);
begin
  assert ud.apply (4) = 108 severity failure;
end;
