entity err_right is
end err_right;

architecture behav of err_right is
  type arr is array (2 to 5) of bit;
  constant c : integer := arr'range'right;
begin
end behav;
