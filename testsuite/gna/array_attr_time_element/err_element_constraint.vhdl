-- An array element constraint of the prefix subtype itself counts: a
-- generic bound makes it globally static, so the attribute isn't locally
-- static (LRM08 9.4.2).
entity err_element_constraint is
  generic (n : natural := 3);
end err_element_constraint;

architecture behav of err_element_constraint is
  type bv_vector is array (natural range <>) of bit_vector;
  subtype s is bv_vector (1 to 2) (0 to n);
  constant c : s := (s'left => (others => '1'), others => (others => '0'));
begin
end behav;
