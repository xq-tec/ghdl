-- An array attribute of a function call is not locally static
-- (LRM08 9.4.2), so it cannot be a case choice.
entity err_static_call is
end entity;

architecture behav of err_static_call is
  function g (n : natural) return bit_vector is
    variable res : bit_vector (1 to 3);
  begin
    return res;
  end function;
begin
  process
    variable v : natural := 3;
  begin
    case v is
      when g (1)'length => null;
      when others => null;
    end case;
    wait;
  end process;
end architecture;
