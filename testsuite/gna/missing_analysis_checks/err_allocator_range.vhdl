-- The only allowed constraints in the subtype indication of an allocator
-- are array and record constraints (LRM08 9.3.7).
entity err_allocator_range is
end entity;

architecture behav of err_allocator_range is
  type int_ptr is access integer;
begin
  process
    variable p : int_ptr;
  begin
    p := new integer range 0 to 7;
    wait;
  end process;
end architecture;
