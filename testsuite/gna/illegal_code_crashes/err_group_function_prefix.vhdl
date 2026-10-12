-- A group constituent whose prefix is a function call (LRM08 6.10).
entity err_group_function_prefix is
end entity;

architecture bad of err_group_function_prefix is
  type rec is record
    x : bit;
  end record;

  function f return rec is
  begin
    return (x => '0');
  end function;

  group g_t is (signal);
  group g : g_t (f.x);
begin
end architecture;
