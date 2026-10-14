entity err_left is
end err_left;

architecture behav of err_left is
  type arr is array (2 to 5) of bit;
  constant c : integer := arr'range'left;
begin
end behav;
