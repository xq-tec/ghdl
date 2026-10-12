-- A record element resolution that names no element of the record (LRM08 6.3).
entity err_record_res_element is
end entity;

architecture bad of err_record_res_element is
  function resolved (s : bit_vector) return bit is
  begin
    return s (s'left);
  end function;

  type rec is record
    a : bit;
  end record;

  subtype bad_t is (missing resolved) rec;
begin
end architecture;
