-- Force mode OUT on a port of mode IN.
entity err_out_on_in is
  port (p : in bit);
end entity;

architecture behav of err_out_on_in is
begin
  process
  begin
    p <= force out '1';
    wait;
  end process;
end architecture;
