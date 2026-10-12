-- An operator symbol also denotes the predefined operators, so a signature
-- is required (LRM08 4.4).
package err_overloaded_operator is
  type rec is record
    v : integer;
  end record;
  function "+" generic (constant n : integer) (x : rec) return rec;
  function bad is new "+" generic map (n => 1);
end package;
