-- Discrete range + element-type expression is illegal for a target.
entity err_element is
end entity;

architecture behav of err_element is
begin
  process
    variable v : bit_vector(0 to 3);
  begin
    (0 to 3 => '0') := v;
    wait;
  end process;
end architecture;
