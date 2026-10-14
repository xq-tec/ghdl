-- An array element constraint inherited from the type mark counts: a
-- generic bound makes it globally static, so the attribute isn't locally
-- static (LRM08 9.4.2).
entity err_parent_element is
  generic (n : natural := 3);
end err_parent_element;

architecture behav of err_parent_element is
  type bv_vector is array (natural range <>) of bit_vector;
  subtype p is bv_vector (open) (0 to n);
  subtype s is p (1 to 2);
  constant c : s := (s'left => (others => '1'), others => (others => '0'));
begin
end behav;
