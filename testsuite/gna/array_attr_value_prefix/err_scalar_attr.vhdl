-- 'STABLE is a signal attribute, but not of an array type.
entity err_scalar_attr is
end entity;

architecture behav of err_scalar_attr is
  signal ss : bit_vector (0 to 3);
begin
  process
  begin
    report integer'image (ss'stable'length);
    wait;
  end process;
end architecture;
