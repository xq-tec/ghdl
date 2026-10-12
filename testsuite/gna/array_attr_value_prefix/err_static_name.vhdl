-- An array attribute of a name attribute is not locally static
-- (LRM08 9.4.2), so it cannot be a case choice.
entity err_static_name is
end entity;

architecture behav of err_static_name is
  signal s : bit;
begin
  process
    variable v : natural := 1;
  begin
    case v is
      when s'simple_name'length => null;
      when others => null;
    end case;
    wait;
  end process;
end architecture;
