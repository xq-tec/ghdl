-- The prefix of an array attribute must be of an array type.
entity err_scalar is
end entity;

architecture behav of err_scalar is
  function f (n : natural) return integer is
  begin
    return n;
  end function;
begin
  process
  begin
    report integer'image (f (1)'length);
    wait;
  end process;
end architecture;
