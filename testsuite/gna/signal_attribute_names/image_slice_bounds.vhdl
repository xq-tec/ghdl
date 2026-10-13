-- A slice of 'IMAGE outside the bounds of the string fails at run time
-- (LRM08 8.5), instead of reading past the folded string.
entity image_slice_bounds is
end entity;

architecture behav of image_slice_bounds is
begin
  process
  begin
    assert integer'image(42)(2 to 3) = "20";
    wait;
  end process;
end architecture;
