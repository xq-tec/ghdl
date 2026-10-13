-- The actual instance has another constraint for a generic type
-- (LRM08 6.5.7.2).
entity err_type is
end entity;

architecture behav of err_type is
  subtype s7 is integer range 0 to 7;
  subtype s8 is integer range 0 to 8;
  package t1 is new work.p_type generic map (t => s7);
begin
  b : block
    generic (package ft is new work.p_type generic map (t => s8));
    generic map (ft => t1);
  begin
  end block;
end architecture;
