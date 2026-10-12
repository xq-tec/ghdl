-- A non globally static actual of an unconstrained port must be one of the
-- forms listed in LRM08 6.5.6.3; a function call whose return type mark
-- is unconstrained is not.
entity child_port is
  port (a : in bit_vector);
end entity;

architecture rtl of child_port is
begin
end architecture;

entity err_port_unconstrained_actual is
end entity;

architecture behav of err_port_unconstrained_actual is
  function glue (x : bit; y : bit) return bit_vector is
  begin
    return x & y;
  end function;

  signal s : bit := '0';
begin
  u : entity work.child_port port map (a => glue (s, s));
end architecture;
