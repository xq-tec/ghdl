entity err_low is
end err_low;

architecture behav of err_low is
  type arr is array (2 to 5) of bit;
  constant c : integer := arr'range'low;
begin
end behav;
