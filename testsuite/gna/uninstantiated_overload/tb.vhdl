-- Overloaded uninstantiated subprograms with different profiles
-- (LRM08 4.5.1, 4.10): a subprogram body belongs to the uninstantiated
-- subprogram declaration it conforms to.

package gp is
  generic (n : integer; type et);
  subtype st is integer range 0 to n;
end package;

package pkg is
  -- Declarations and bodies separate.
  function f generic (type t) (x : integer) return integer;
  function f generic (type t) (x : integer; y : bit) return integer;
  procedure p generic (type t) (x : integer; variable r : out integer);
  procedure p generic (type t) (x : bit; variable r : out integer);
  -- Profiles naming the generic type.
  function g generic (type t) (x : t) return integer;
  function g generic (type t) (x : t; y : integer) return integer;
  -- Generic constants with defaults, and generic subprograms.
  function k generic (constant n : integer := 7) (x : boolean) return integer;
  function k generic (function fn (v : integer) return integer)
    (x : integer) return integer;
  function k generic (function fn (v : integer) return integer is <>)
    (x : bit) return integer;
  -- Generic packages, and a profile naming a declaration of one.
  procedure m generic (package q is new work.gp generic map (n => 1,
                                                             et => bit))
    (x : integer);
  procedure m generic (package q is new work.gp generic map (<>)) (x : bit);
  procedure m generic (package q is new work.gp generic map (<>))
    (x : q.st; y : bit);
  -- An ordinary subprogram of the same name.
  procedure p (x : integer; y : integer; variable r : out integer);
end package;

package body pkg is
  -- Bodies in the reverse order of the declarations.
  function f generic (type t) (x : integer; y : bit) return integer is
  begin
    return x + 2;
  end function;

  function f generic (type t) (x : integer) return integer is
  begin
    return x + 1;
  end function;

  procedure p generic (type t) (x : bit; variable r : out integer) is
  begin
    r := 20;
  end procedure;

  procedure p generic (type t) (x : integer; variable r : out integer) is
  begin
    r := 10 + x;
  end procedure;

  function g generic (type t) (x : t; y : integer) return integer is
  begin
    return y;
  end function;

  function g generic (type t) (x : t) return integer is
  begin
    return 30;
  end function;

  function k generic (function fn (v : integer) return integer is <>)
    (x : bit) return integer is
  begin
    return fn (1);
  end function;

  function k generic (function fn (v : integer) return integer)
    (x : integer) return integer is
  begin
    return fn (x);
  end function;

  function k generic (constant n : integer := 7) (x : boolean) return integer
  is
  begin
    return n;
  end function;

  procedure m generic (package q is new work.gp generic map (<>))
    (x : q.st; y : bit) is
  begin
  end procedure;

  procedure m generic (package q is new work.gp generic map (<>)) (x : bit) is
  begin
  end procedure;

  procedure m generic (package q is new work.gp generic map (n => 1,
                                                             et => bit))
    (x : integer) is
  begin
  end procedure;

  procedure p (x : integer; y : integer; variable r : out integer) is
  begin
    r := 70 + x + y;
  end procedure;
end package body;

use work.pkg.all;

entity tb is
end entity;

architecture behav of tb is
  function f1 is new f [integer return integer] generic map (t => bit);
  function f2 is new f [integer, bit return integer] generic map (t => bit);
  procedure p1 is new p [integer, integer] generic map (t => bit);
  procedure p2 is new p [bit, integer] generic map (t => bit);

  function fn (v : integer) return integer is
  begin
    return v + 100;
  end function;

  function three (v : integer) return integer is
  begin
    return v + 3;
  end function;

  function k1 is new k [boolean return integer];
  function k2 is new k [integer return integer] generic map (fn => three);
  function k3 is new k [bit return integer];

  -- Declarations and bodies combined.
  function h generic (type t) (x : integer) return integer is
  begin
    return x * 2;
  end function;

  function h generic (type t) (x : bit) return integer is
  begin
    return 40;
  end function;

  -- Declarations and bodies separate.
  procedure q generic (type t) (x : integer; variable r : out integer);
  procedure q generic (type t) (x : boolean; variable r : out integer);

  procedure q generic (type t) (x : boolean; variable r : out integer) is
  begin
    r := 50;
  end procedure;

  procedure q generic (type t) (x : integer; variable r : out integer) is
  begin
    r := 60 + x;
  end procedure;

  function h1 is new h [integer return integer] generic map (t => integer);
  function h2 is new h [bit return integer] generic map (t => integer);
  procedure q1 is new q [integer, integer] generic map (t => integer);
  procedure q2 is new q [boolean, integer] generic map (t => integer);

  function f0 generic (type t) (x : integer) return integer is
  begin
    return 1;
  end function;

  function f01 is new f0 generic map (t => bit);
begin
  b : block
    -- Not the body of the outer f0, a new subprogram of the inner region.
    function f0 generic (type t) (x : integer) return integer is
    begin
      return 2;
    end function;

    function f02 is new f0 generic map (t => bit);
  begin
    process
    begin
      assert f01 (0) = 1 severity failure;
      assert f02 (0) = 2 severity failure;
      wait;
    end process;
  end block;

  process
    variable r : integer;
  begin
    assert f1 (5) = 6 severity failure;
    assert f2 (5, '1') = 7 severity failure;
    p1 (3, r);
    assert r = 13 severity failure;
    p2 ('0', r);
    assert r = 20 severity failure;
    assert h1 (4) = 8 severity failure;
    assert h2 ('1') = 40 severity failure;
    q1 (2, r);
    assert r = 62 severity failure;
    q2 (true, r);
    assert r = 50 severity failure;
    assert k1 (true) = 7 severity failure;
    assert k2 (1) = 4 severity failure;
    assert k3 ('0') = 101 severity failure;
    p (1, 2, r);
    assert r = 73 severity failure;
    report "PASS";
    wait;
  end process;
end architecture;
