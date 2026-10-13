-- Two uninstantiated subprogram bodies with the same parameter and result
-- type profile are homographs (LRM08 4.5.1).
entity err_homograph_body is
end entity;

architecture behav of err_homograph_body is
  function f generic (type t) (x : integer) return integer is
  begin
    return 1;
  end function;

  function f generic (type t) (x : integer) return integer is
  begin
    return 2;
  end function;
begin
end architecture;
