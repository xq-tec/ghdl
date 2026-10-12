-- The expression of a matching case statement shall be of type BIT,
-- STD_ULOGIC, or a one-dimensional array of them (LRM08 10.9).
entity err_case_matching_type is
end entity;

architecture behav of err_case_matching_type is
begin
  process
    variable e : integer := 0;
  begin
    case? e is
      when 0 => null;
      when others => null;
    end case?;
    wait;
  end process;
end architecture;
