-- Resolved composite subtypes for the force/release tests.
package pkg is
  type bv_vec is array (natural range <>) of bit_vector(0 to 1);

  function res_bv (v : bv_vec) return bit_vector;

  -- A resolved unconstrained composite subtype.
  subtype rv is res_bv bit_vector;

  -- An array whose elements are resolved composites (element resolution).
  subtype erv is (res_bv) bv_vec;
end package;

package body pkg is
  function res_bv (v : bv_vec) return bit_vector
  is
    variable r : bit_vector(0 to 1) := "00";
  begin
    for i in v'range loop
      r := r or v (i);
    end loop;
    return r;
  end function;
end package body;
