-- The expression of a matching selected assignment shall be of type BIT,
-- STD_ULOGIC, or a one-dimensional array of them (LRM08 10.5.4, 10.9).
entity err_select_matching_type is
end entity;

architecture behav of err_select_matching_type is
  signal e : integer := 0;
  signal o : bit;
begin
  with e select?
    o <= '1' when 0,
         '0' when others;
end architecture;
