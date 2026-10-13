-- An error in the generic map aspect of a formal generic package.
entity err_formal_map is
end entity;

architecture behav of err_formal_map is
  package c1 is new work.p_const generic map (g => 1);
begin
  b : block
    generic (package fc is new work.p_const generic map (g => undefined));
    generic map (fc => c1);
  begin
  end block;
end architecture;
