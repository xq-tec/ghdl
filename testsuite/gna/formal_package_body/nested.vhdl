-- Instances of a generic package with a formal generic package, declared in
-- the body of a generic package (LRM08 4.9, 6.5.7.2).  Instantiating the
-- nest_outer package copies the inner instances, including the formal generic
-- package of the inner instance.
package nest_gen is
  generic (type t; constant c : t; function to_int (x : t) return integer);
end package;

-- With a body.
package nest_user_body is
  generic (package p is new work.nest_gen generic map (<>));
  constant k : p.t := p.c;
  function get return integer;
  function same (a : p.t) return boolean;
end package;

package body nest_user_body is
  function get return integer is
    variable v : p.t := k;
  begin
    return p.to_int (x => v);
  end function;

  function same (a : p.t) return boolean is
  begin
    return p."=" (a, p.c);
  end function;
end package body;

-- Without a body.
package nest_user_spec is
  generic (package p is new work.nest_gen generic map (<>));
  constant k : p.t := p.c;
end package;

package nest_outer is
  generic (type tt; constant cc : tt; function conv (x : tt) return integer);
  function get return integer;
end package;

package body nest_outer is
  package gt is new work.nest_gen generic map (t => tt, c => cc, to_int => conv);
  package ub is new work.nest_user_body generic map (p => gt);
  package us is new work.nest_user_spec generic map (p => gt);

  function get return integer is
  begin
    assert ub.same (cc);
    assert us.k = cc;
    return ub.get + conv (us.k) + 1000;
  end function;
end package body;

entity nested is
end;

architecture behav of nested is
  function b2i (x : bit) return integer is
  begin
    return bit'pos (x) + 10;
  end function;

  package obit is new work.nest_outer generic map (tt => bit, cc => '1', conv => b2i);
begin
  process
  begin
    assert obit.get = 1022 severity failure;
    wait;
  end process;
end;
