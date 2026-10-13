-- Two record element resolutions for the same element (LRM08 6.3).
entity err_duplicate is
end entity;

architecture bad of err_duplicate is
  type bv is array (natural range <>) of bit;

  function res_or (v : bv) return bit is
  begin
    return v (v'left);
  end function;

  type rec is record
    a : bit;
  end record;

  subtype bad_t is (a res_or, a res_or) rec;
begin
end architecture;
