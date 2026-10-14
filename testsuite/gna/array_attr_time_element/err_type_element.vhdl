-- A constrained composite element subtype of the array type definition is
-- an array element constraint (LRM08 5.3.2.1): a generic bound makes it
-- globally static, so the attribute isn't locally static (LRM08 9.4.2).
entity err_type_element is
  generic (n : natural := 3);
end err_type_element;

architecture behav of err_type_element is
  type bv_vector is array (natural range <>) of bit_vector (0 to n);
  subtype s is bv_vector (1 to 2);
  constant c : s := (s'left => (others => '1'), others => (others => '0'));
begin
end behav;
