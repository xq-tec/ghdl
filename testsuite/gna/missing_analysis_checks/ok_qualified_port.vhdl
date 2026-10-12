-- A qualified expression whose type mark denotes a globally static subtype
-- is allowed as non globally static actual of an unconstrained port
-- (LRM08 6.5.6.3).  Only analyzed: elaboration of such an actual is not
-- supported by the GHDL elaborator.
entity child2 is
  port (a : in bit_vector);
end entity;

architecture rtl of child2 is
begin
end architecture;

entity ok_qualified_port is
end entity;

architecture behav of ok_qualified_port is
  subtype bv2 is bit_vector (0 to 1);
  signal s : bit := '0';
begin
  u1 : entity work.child2 port map (a => bv2'(s & not s));
  u2 : entity work.child2 port map (a => bv2 (bit_vector'(s & not s)));
  u3 : entity work.child2 port map (a => (bv2'(s & not s)));
end architecture;
