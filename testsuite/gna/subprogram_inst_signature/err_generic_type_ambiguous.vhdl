-- A signature naming a generic type that matches the profiles of two
-- uninstantiated subprograms (LRM08 4.4, 12.3 r).
entity err_generic_type_ambiguous is end entity;
architecture a of err_generic_type_ambiguous is
  procedure amb generic (type t) (x : t) is begin null; end procedure;
  procedure amb generic (type t; constant n : integer := 0) (x : t) is
  begin null; end procedure;
  procedure bad is new amb [t] generic map (t => integer);
begin end architecture;
