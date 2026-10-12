-- The uninstantiated subprogram name must be a name denoting a subprogram
-- (LRM08 4.4).
entity err_not_a_name is end entity;
architecture a of err_not_a_name is
  procedure gen_p generic (type t) (x : t) is begin null; end procedure;
  procedure bad is new gen_p'path_name generic map (t => integer);
begin end architecture;
