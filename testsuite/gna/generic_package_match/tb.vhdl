-- Formal generic packages with an explicit or a DEFAULT generic map aspect,
-- associated with instances whose generic map aspects match
-- (LRM08 6.5.5, 6.5.7.2).
entity tb is
end entity;

architecture behav of tb is
  function inc (x : integer) return integer is
  begin
    return x + 1;
  end function;

  function dbl (x : integer) return integer is
  begin
    return 2 * x;
  end function;

  subtype small is integer range 0 to 7;
  subtype small2 is integer range 0 to 7;
  type rec is record
    a : bit_vector;
    b : integer;
  end record;
  subtype rec4 is rec (a (0 to 3));
  subtype rec4b is rec (a (0 to 3));
  constant k : integer := 2;

  --  The actuals of the formals' generic map aspects are generics of this
  --  package, so they are not checked when it is analyzed.
  package p_nest is
    generic (
      type t;
      function f (x : integer) return integer;
      n : integer;
      package ft is new work.p_type generic map (t => t);
      package fs is new work.p_sub generic map (f => f);
      package fc is new work.p_const generic map (g => n)
    );
  end package;

  --  Same values, written differently.
  package c1 is new work.p_const
    generic map (g => k, r => 2.0, s => "xy", b => "10", e => 'b',
                 tm => 1000 ps);
  package c2 is new work.p_const generic map (g => 5);
  package t1 is new work.p_type generic map (t => small);
  package t2 is new work.p_type generic map (t => bit_vector (0 to 3));
  package t3 is new work.p_type generic map (t => rec4);
  package s1 is new work.p_sub generic map (f => dbl);
  package k1 is new work.p_pkg generic map (ip => c1);
  --  INC is resolved here.
  package d1 is new work.p_def;
  package d2 is new work.p_def generic map (g => 3, inc => inc);
  package ti is new work.p_type generic map (t => integer);
  package n1 is new p_nest
    generic map (t => integer, f => dbl, n => 5, ft => ti, fs => s1,
                 fc => c2);
begin
  b1 : block
    generic (
      package fc is new work.p_const
        generic map (g => 2, r => 2.0, s => "xy", b => ('1', '0'),
                     e => 'b', tm => 1 ns);
      --  Positional associations, the other generics have their default.
      package fc2 is new work.p_const generic map (5, 1.0);
      --  Another subtype, with the same base type and range as SMALL.
      package ft1 is new work.p_type generic map (t => small2);
      package ft2 is new work.p_type generic map (t => bit_vector (0 to 3));
      package ft3 is new work.p_type generic map (t => rec4b);
      package fs is new work.p_sub generic map (f => dbl);
      package fk is new work.p_pkg generic map (ip => c1);
      --  INC is resolved here, to the same function.
      package fd1 is new work.p_def generic map (default);
      package fd2 is new work.p_def generic map (default);
      --  Any instance.
      package fb is new work.p_const generic map (<>)
    );
    generic map (fc => c1, fc2 => c2, ft1 => t1, ft2 => t2, ft3 => t3,
                 fs => s1, fk => k1, fd1 => d1, fd2 => d2, fb => c2);
  begin
    b2 : block
      --  N is not locally static, so the match is not checked at analysis.
      generic (
        n : integer;
        package fn is new work.p_const generic map (g => n)
      );
      generic map (n => 5, fn => c2);
    begin
      process
      begin
        assert fc.c = 2 and fn.c = 5 and fd1.c = 4 and fd2.c = 4
          and fb.c = 5 and fs.f (3) = 6
          severity failure;
        wait;
      end process;
    end block;
  end block;
end architecture;
