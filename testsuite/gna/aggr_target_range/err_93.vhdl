-- Discrete range choices in targets are illegal before VHDL-2008.
entity err_93 is
end entity;

architecture behav of err_93 is
begin
  process
    variable tmp1 : bit_vector(0 to 7);
    variable tmp2 : bit_vector(0 to 7);
  begin
    tmp1 := "10101010";
    (0 to 7 => tmp2) := tmp1;
    wait;
  end process;
end architecture;
