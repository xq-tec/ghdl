-- A nested record element resolution that names no element of the element
-- subtype (LRM08 6.3).
entity err_nested_element is
end entity;

architecture bad of err_nested_element is
  type bv is array (natural range <>) of bit;

  function res_or (v : bv) return bit is
  begin
    return v (v'left);
  end function;

  type inner is record
    x : bit;
  end record;

  type rec is record
    c : inner;
  end record;

  subtype bad_t is (c (z res_or)) rec;
begin
end architecture;
