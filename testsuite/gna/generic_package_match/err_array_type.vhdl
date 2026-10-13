-- The actual instance has another index range for a generic array type
-- (LRM08 6.5.7.2).
entity err_array_type is
end entity;

architecture behav of err_array_type is
  package t1 is new work.p_type generic map (t => bit_vector (3 downto 0));
begin
  b : block
    generic (
      package ft is new work.p_type generic map (t => bit_vector (0 to 3)));
    generic map (ft => t1);
  begin
  end block;
end architecture;
