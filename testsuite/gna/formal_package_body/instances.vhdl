-- A formal generic package, whose uninstantiated package has a generic type
-- and a generic subprogram, in a generic subprogram and in an entity
-- (LRM08 6.5.6.2, 6.5.7.2).  Both are macro-expanded, and the references to
-- the formal package designate the declarations of the actual package.
package unit_gen is
  generic (type t; constant c : t; function to_int (x : t) return integer);
end package;

package unit_subprg is
  function get generic (package p is new work.unit_gen generic map (<>))
    (a : integer) return integer;
end package;

package body unit_subprg is
  function get generic (package p is new work.unit_gen generic map (<>))
    (a : integer) return integer
  is
    variable v : p.t := p.c;
  begin
    assert p."=" (v, p.c);
    return p.to_int (x => v) + a;
  end function;
end package body;

entity unit_ent is
  generic (package p is new work.unit_gen generic map (<>));
  port (o : out integer);
end;

architecture behav of unit_ent is
  signal v : p.t := p.c;
begin
  o <= p.to_int (v) + 3000;
end;

entity instances is
end;

architecture behav of instances is
  function b2i (x : bit) return integer is
  begin
    return bit'pos (x) + 10;
  end function;

  package ibit is new work.unit_gen
    generic map (t => bit, c => '1', to_int => b2i);

  function get_bit is new work.unit_subprg.get generic map (p => ibit);

  function local generic (package p is new work.unit_gen generic map (<>))
    return integer
  is
    variable v : p.t := p.c;
  begin
    return p.to_int (v) + 100;
  end function;

  function local_bit is new local generic map (p => ibit);

  signal s : integer;
begin
  inst : entity work.unit_ent generic map (p => ibit) port map (o => s);

  process
  begin
    assert get_bit (5) = 16 severity failure;
    assert local_bit = 111 severity failure;
    wait for 1 ns;
    assert s = 3011 severity failure;
    wait;
  end process;
end;
