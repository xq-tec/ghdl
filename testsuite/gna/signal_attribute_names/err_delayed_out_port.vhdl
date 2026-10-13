-- An implicit signal cannot be the actual of a port of mode out
-- (LRM08 14.7.4, 16.2.4).
entity err_delayed_out_port_sub is
  port (o : out bit);
end entity;

architecture behav of err_delayed_out_port_sub is
begin
  o <= '1';
end architecture;

entity err_delayed_out_port is
end entity;

architecture bad of err_delayed_out_port is
  type rec is record
    a : bit;
  end record;
  signal s : rec;
begin
  u : entity work.err_delayed_out_port_sub port map (o => s'delayed(1 ns).a);
end architecture;
