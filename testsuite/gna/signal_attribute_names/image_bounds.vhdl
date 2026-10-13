-- An index of 'IMAGE outside the bounds of the string fails at run time
-- (LRM08 8.4), instead of reading past the folded string.
entity image_bounds is
end entity;

architecture behav of image_bounds is
begin
  process
  begin
    assert integer'image(42)(3) = '0';
    wait;
  end process;
end architecture;
