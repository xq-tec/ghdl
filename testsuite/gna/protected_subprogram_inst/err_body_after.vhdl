-- The body of the uninstantiated subprogram follows the protected type body,
-- so it is not available at the end of the protected type body, where the
-- generic-mapped body occurs (LRM08 4.4).
entity err_body_after is
end entity;

architecture behav of err_body_after is
  procedure gen generic (type t) (variable v : out t);

  type pt is protected
    procedure int_gen is new gen generic map (t => integer);
  end protected;

  type pt is protected body
  end protected body;

  procedure gen generic (type t) (variable v : out t) is
  begin
  end procedure;
begin
end architecture;
