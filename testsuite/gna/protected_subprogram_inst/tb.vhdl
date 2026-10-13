-- Subprogram instantiation declarations in a protected type declaration
-- (LRM08 5.6.2, 4.4): procedure and function instantiations are methods,
-- called through a shared variable and through a process variable, and by
-- other methods.  The generic-mapped bodies are at the end of the protected
-- type body, so they see its declarations.
package gen_pkg is
  -- An uninstantiated subprogram declared outside the protected type.
  function scale generic (constant k : integer) (x : integer) return integer;

  -- Uninstantiated functions without parameters.
  type pair is record
    first, second : integer;
  end record;
  type int_vec is array (natural range <>) of integer;
  function make_pair generic (constant k : integer) return pair;
  function make_vec generic (constant k : integer) return int_vec;
end package;

package body gen_pkg is
  function scale generic (constant k : integer) (x : integer) return integer
  is
  begin
    return k * x;
  end function;

  function make_pair generic (constant k : integer) return pair is
  begin
    return (first => k, second => k + 1);
  end function;

  function make_vec generic (constant k : integer) return int_vec is
  begin
    return (k, k, k);
  end function;
end package body;

-- A protected type declared in a package: the body of the uninstantiated
-- subprogram is in the package body, before the protected type body.
package prot_pkg is
  procedure fill generic (constant val : integer) (variable v : out integer);

  type box is protected
    procedure fill7 is new fill generic map (val => 7);
    impure function get return integer;
  end protected;
end package;

package body prot_pkg is
  procedure fill generic (constant val : integer) (variable v : out integer) is
  begin
    v := val;
  end procedure;

  type box is protected body
    variable content : integer := 0;

    impure function get return integer is
    begin
      return content;
    end function;
  end protected body;
end package body;

use work.gen_pkg.all;
use work.prot_pkg.all;

entity tb is
end entity;

architecture behav of tb is
  type counter is protected
    -- An uninstantiated procedure of the protected type, instantiated twice.
    procedure add generic (constant step : integer) (n : natural);
    procedure add1 is new add generic map (step => 1);
    procedure add10 is new add generic map (step => 10);

    -- Uninstantiated functions of the protected type.
    impure function get generic (constant offset : integer) (n : integer)
      return integer;
    function get_int is new get generic map (offset => 0);
    impure function count_if generic (type t; constant ref : t) (v : t)
      return integer;
    function count_if_true is new count_if
      generic map (t => boolean, ref => true);
    function count_if_z is new count_if generic map (t => character, ref => 'z');

    -- An instantiation of a subprogram declared outside.
    function twice is new scale generic map (k => 2);

    -- A function instantiation without parameters.
    impure function peek generic (constant offset : integer) return integer;
    function peek1 is new peek generic map (offset => 1);

    -- A method that calls instantiated methods.
    procedure bump;
  end protected;

  type counter is protected body
    variable count : integer := 0;

    procedure add generic (constant step : integer) (n : natural) is
    begin
      for i in 1 to n loop
        count := count + step;
      end loop;
    end procedure;

    impure function get generic (constant offset : integer) (n : integer)
      return integer is
    begin
      return count + offset + n;
    end function;

    impure function count_if generic (type t; constant ref : t) (v : t)
      return integer is
    begin
      if v = ref then
        return count;
      else
        return -1;
      end if;
    end function;

    impure function peek generic (constant offset : integer) return integer is
    begin
      return count + offset;
    end function;

    -- An instantiation in the protected type body (not a method).
    procedure add100 is new add generic map (step => 100);

    procedure bump is
    begin
      add1 (1);
      add100 (1);
      count := count + peek1;
    end procedure;
  end protected body;

  function pair7 is new make_pair generic map (k => 7);
  function vec3 is new make_vec generic map (k => 3);

  shared variable sv : counter;
begin
  shared_p : process
  begin
    assert sv.count_if_true (true) = 0 severity failure;
    assert sv.count_if_z ('z') = 0 severity failure;
    sv.add1 (2);
    sv.add10 (3);
    assert sv.get_int (0) = 32 severity failure;
    assert sv.count_if_true (true) = 32 severity failure;
    assert sv.count_if_true (false) = -1 severity failure;
    assert sv.count_if_z ('z') = 32 severity failure;
    assert sv.count_if_z ('a') = -1 severity failure;
    assert sv.twice (21) = 42 severity failure;
    assert sv.peek1 = 33 severity failure;
    sv.bump;
    assert sv.get_int (0) = 267 severity failure;
    wait;
  end process;

  local_p : process
    variable v : counter;
    variable b : box;
    variable n : integer;
  begin
    v.add10 (1);
    v.add1 (5);
    assert v.get_int (1) = 16 severity failure;
    assert v.twice (v.get_int (0)) = 30 severity failure;
    assert v.peek1 = 16 severity failure;
    -- Function instantiations without parameters outside a protected type.
    assert pair7.second = 8 and pair7 = (7, 8) severity failure;
    assert vec3'length = 3 and vec3 (2) = 3 severity failure;
    b.fill7 (n);
    assert n = 7 and b.get = 0 severity failure;
    report "PASS";
    wait;
  end process;
end architecture;
