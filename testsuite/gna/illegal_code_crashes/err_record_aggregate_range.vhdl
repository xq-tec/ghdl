-- A range choice in a record aggregate (LRM08 9.3.3.1).
entity err_record_aggregate_range is
end entity;

architecture bad of err_record_aggregate_range is
  type rec is record
    a, b : integer;
  end record;
begin
  process
    variable r : rec;
  begin
    r := (0 to 1 => 0);
    wait;
  end process;
end architecture;
