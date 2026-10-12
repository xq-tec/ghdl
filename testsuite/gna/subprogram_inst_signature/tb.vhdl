-- Subprogram instantiations with a signature that selects the uninstantiated
-- subprogram among overloads (LRM08 4.4).
package pkg is
  function scale generic (constant n : integer) (x : integer) return integer;
  function scale (x : integer; y : integer) return integer;
  function scale2 is new scale [integer return integer] generic map (n => 2);
  type rec is record
    v : integer;
  end record;
  --  An operator symbol always denotes the predefined operators too.
  function "+" generic (constant n : integer) (x : rec) return rec;
  function plus is new "+" [rec return rec] generic map (n => 7);
end package;

package body pkg is
  function scale generic (constant n : integer) (x : integer) return integer
  is
  begin
    return x * n;
  end function;
  function scale (x : integer; y : integer) return integer is
  begin
    return x * y;
  end function;
  function "+" generic (constant n : integer) (x : rec) return rec is
  begin
    return (v => x.v + n);
  end function;
end package body;

use work.pkg.all;

entity tb is end entity;

architecture a of tb is
  procedure inc generic (constant n : integer) (variable x : inout integer) is
  begin
    x := x + n;
  end procedure;
  function add generic (constant n : integer) (x : integer) return integer is
  begin
    return x + n;
  end function;
  --  An ordinary overload: the signature selects the uninstantiated one.
  function add (x : integer; y : integer) return integer is
  begin
    return x + y;
  end function;
  procedure inc3 is new inc [integer] generic map (n => 3);
  function add5 is new add [integer return integer] generic map (n => 5);
  function scale3 is new scale [integer return integer] generic map (n => 3);
begin
  process
    variable v : integer := 1;
  begin
    inc3 (v);
    assert v = 4 severity failure;
    assert add5 (v) = 9 severity failure;
    assert add (v, 2) = 6 severity failure;
    assert scale2 (v) = 8 severity failure;
    assert scale3 (v) = 12 severity failure;
    assert scale (v, 5) = 20 severity failure;
    assert plus (rec'(v => v)).v = 11 severity failure;
    report "ok";
    wait;
  end process;
end architecture;
