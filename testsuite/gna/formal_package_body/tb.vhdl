-- Instances of generic packages with a body and a formal generic package
-- whose uninstantiated package has a generic type or a generic subprogram
-- (LRM08 6.5.7.2, 4.9).  The uninstantiated packages are macro-expanded, so
-- the bodies are instantiated, and their references to the formal package
-- (its generic type, constant and subprograms, through a selected name and
-- through a use clause) designate the declarations of the actual package.
-- Calls of the formal's generic subprograms with named associations refer to
-- the parameters of the actual subprograms.
package fns is
  function dbl (x : integer) return integer;
  function bit_to_int (x : bit) return integer;
end package;

package body fns is
  function dbl (x : integer) return integer is
  begin
    return 2 * x;
  end function;

  function bit_to_int (x : bit) return integer is
  begin
    return bit'pos (x) + 10;
  end function;
end package body;

package gen_type is
  generic (type t; constant c : t; pure function to_int (x : t) return integer);
end package;

package gen_subprg is
  generic (pure function f (x : integer) return integer);
end package;

-- Selected names of the formal package, with a box.
package user_type is
  generic (package p is new work.gen_type generic map (<>));
  constant k : p.t := p.c;
  function get return integer;
  function same return boolean;
end package;

package body user_type is
  subtype st is p.t;
  constant kb : st := p.c;

  function get return integer is
    variable v : p.t;
  begin
    v := kb;
    return p.to_int (x => v);
  end function;

  function same return boolean is
    variable v : p.t := p.c;
  begin
    return p."=" (v, k);
  end function;
end package body;

-- A use clause of the formal package.
package user_use is
  generic (package p is new work.gen_type generic map (<>));
  use p.all;
  function get return integer;
end package;

package body user_use is
  function get return integer is
    variable v : t := c;
  begin
    assert v = c;
    return to_int (v) + 1;
  end function;
end package body;

-- A formal generic subprogram, with a box and with an explicit generic map.
package user_subprg is
  generic (package p is new work.gen_subprg generic map (<>));
  function apply (x : integer) return integer;
end package;

package body user_subprg is
  function apply (x : integer) return integer is
  begin
    return p.f (x => x) + 1;
  end function;
end package body;

package user_map is
  generic (package p is new work.gen_subprg generic map (f => work.fns.dbl));
  function apply (x : integer) return integer;
end package;

package body user_map is
  function apply (x : integer) return integer is
  begin
    return p.f (x) * 10;
  end function;
end package body;

use work.fns.all;

entity tb is
end;

architecture behav of tb is
  function id (x : integer) return integer is
  begin
    return x;
  end function;

  package ibit is new work.gen_type
    generic map (t => bit, c => '1', to_int => bit_to_int);
  package iint is new work.gen_type
    generic map (t => integer, c => 42, to_int => id);
  package idbl is new work.gen_subprg generic map (f => dbl);

  package ut_bit is new work.user_type generic map (p => ibit);
  package ut_int is new work.user_type generic map (p => iint);
  package uu is new work.user_use generic map (p => ibit);
  package us is new work.user_subprg generic map (p => idbl);
  package um is new work.user_map generic map (p => idbl);
begin
  process
  begin
    assert ut_bit.get = 11 severity failure;
    assert ut_bit.same severity failure;
    assert ut_bit.k = '1' severity failure;
    assert ut_int.get = 42 severity failure;
    assert ut_int.same severity failure;
    assert ut_int.k = 42 severity failure;
    assert uu.get = 12 severity failure;
    assert us.apply (5) = 11 severity failure;
    assert um.apply (3) = 60 severity failure;
    wait;
  end process;
end;
