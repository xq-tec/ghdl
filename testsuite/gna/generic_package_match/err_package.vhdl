-- The actual instance has another instance for a generic package
-- (LRM08 6.5.7.2).
entity err_package is
end entity;

architecture behav of err_package is
  package c1 is new work.p_const;
  package c2 is new work.p_const;
  package k1 is new work.p_pkg generic map (ip => c1);
begin
  b : block
    generic (package fk is new work.p_pkg generic map (ip => c2));
    generic map (fk => k1);
  begin
  end block;
end architecture;
