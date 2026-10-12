-- An expanded name cannot select a generic type of an uninstantiated
-- subprogram outside the subprogram (LRM08 8.3).
entity err_generic_type_selection is end entity;
architecture a of err_generic_type_selection is
  procedure gen_p generic (type t) (x : t) is begin null; end procedure;
  procedure bad is new gen_p [gen_p.t] generic map (t => integer);
begin end architecture;
