entity err_high is
end err_high;

architecture behav of err_high is
  type arr is array (2 to 5) of bit;
  constant c : integer := arr'range'high;
begin
end behav;
