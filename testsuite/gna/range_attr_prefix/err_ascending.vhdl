entity err_ascending is
end err_ascending;

architecture behav of err_ascending is
  type arr is array (2 to 5) of bit;
  constant c : boolean := arr'range'ascending;
begin
end behav;
