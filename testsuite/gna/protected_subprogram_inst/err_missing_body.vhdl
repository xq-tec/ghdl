-- The uninstantiated subprogram of an instantiation in a protected type
-- declaration has no body in the protected type body (LRM08 4.4: the
-- generic-mapped body occurs at the end of the protected type body).
entity err_missing_body is
end entity;

architecture behav of err_missing_body is
  type pt is protected
    procedure gen generic (type t) (variable v : out t);
    procedure int_gen is new gen generic map (t => integer);
  end protected;

  type pt is protected body
  end protected body;
begin
end architecture;
