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
  --  An outer declaration of t, hidden in the signatures below.
  subtype t is boolean;
  --  LRM08 12.3 r): the generic types of the uninstantiated subprogram are
  --  visible by their simple names in the signature.
  function gen_p generic (type t) (x : t) return t is
  begin
    return x;
  end function;
  function id_int is new gen_p [t return t] generic map (t => integer);
  procedure gen_q generic (type t) (x : t; variable r : out integer) is
  begin
    r := 1;
  end procedure;
  procedure gen_q generic (type t) (x : t; y : t; variable r : out integer) is
  begin
    r := 2;
  end procedure;
  procedure q1 is new gen_q [t, integer] generic map (t => bit);
  procedure q2 is new gen_q [t, t, integer] generic map (t => bit);
  --  Overloads distinguished only by their generic types.
  function sel generic (type t1) (x : t1) return integer is
  begin
    return 1;
  end function;
  function sel generic (type t2) (x : t2) return integer is
  begin
    return 2;
  end function;
  function sel1 is new sel [t1 return integer] generic map (t1 => bit);
  function sel2 is new sel [t2 return integer] generic map (t2 => bit);
  --  A generic type and a type of the same profile position.
  function d generic (type t) (x : t) return integer is
  begin
    return 3;
  end function;
  function d generic (type t) (x : integer) return integer is
  begin
    return 4;
  end function;
  function d3 is new d [t return integer] generic map (t => integer);
  function d4 is new d [integer return integer] generic map (t => integer);
  --  For a candidate without a generic type t, t is the outer one.
  function w generic (type t) (x : t; y : t) return integer is
  begin
    return 5;
  end function;
  function w generic (type u) (x : u; y : t) return integer is
  begin
    return 6;
  end function;
  function w5 is new w [t, t return integer] generic map (t => bit);
  function w6 is new w [u, t return integer] generic map (u => bit);
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
    assert id_int (v) = 4 severity failure;
    q1 ('0', v);
    assert v = 1 severity failure;
    q2 ('0', '1', v);
    assert v = 2 severity failure;
    assert sel1 ('0') = 1 severity failure;
    assert sel2 ('0') = 2 severity failure;
    assert d3 (0) = 3 severity failure;
    assert d4 (0) = 4 severity failure;
    assert w5 ('0', '1') = 5 severity failure;
    assert w6 ('0', true) = 6 severity failure;
    report "ok";
    wait;
  end process;
end architecture;
