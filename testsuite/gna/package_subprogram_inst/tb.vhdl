-- Instances of generic packages that contain subprogram instantiation
-- declarations (LRM08 4.4), in the package declaration and in a protected
-- type declaration of it (LRM08 5.6.2).  The instantiations are called
-- through the package instances, for a package that is macro-expanded (it
-- has a generic type and a generic subprogram, so its body is instantiated
-- with each instance) and for one that is not, declared in an architecture
-- and as a library unit.  The generic subprogram of gen_macro is called with
-- a named association.
package gen_macro is
  generic (type t; constant k : integer; function conv (x : t) return integer);

  function add generic (constant n : integer) (x : t) return integer;
  function add3 is new add generic map (n => 3);
  function addk is new add generic map (n => k);

  procedure incr generic (constant n : integer) (variable v : inout integer);
  procedure incr2 is new incr generic map (n => 2);

  type counter is protected
    procedure bump generic (constant n : integer);
    procedure bump5 is new bump generic map (n => 5);
    procedure bumpk is new bump generic map (n => k);
    impure function get return integer;
  end protected;
end package;

package body gen_macro is
  function add generic (constant n : integer) (x : t) return integer is
  begin
    return conv (x => x) + n;
  end function;

  procedure incr generic (constant n : integer) (variable v : inout integer) is
  begin
    v := v + n;
  end procedure;

  type counter is protected body
    variable val : integer := 0;

    procedure bump generic (constant n : integer) is
    begin
      val := val + n;
    end procedure;

    impure function get return integer is
    begin
      return val;
    end function;
  end protected body;
end package body;

package gen_shared is
  generic (constant k : integer);

  function add generic (constant n : integer) (x : integer) return integer;
  function add3 is new add generic map (n => 3);
  function addk is new add generic map (n => k);

  type counter is protected
    procedure bump generic (constant n : integer);
    procedure bumpk is new bump generic map (n => k);
    impure function get return integer;
  end protected;
end package;

package body gen_shared is
  function add generic (constant n : integer) (x : integer) return integer is
  begin
    return x + n;
  end function;

  type counter is protected body
    variable val : integer := 0;

    procedure bump generic (constant n : integer) is
    begin
      val := val + n;
    end procedure;

    impure function get return integer is
    begin
      return val;
    end function;
  end protected body;
end package body;

package shared7 is new work.gen_shared generic map (k => 7);

use work.shared7;

entity tb is
end;

architecture behav of tb is
  function b2i (x : bit) return integer is
  begin
    return bit'pos (x);
  end function;

  package macro10 is new work.gen_macro
    generic map (t => bit, k => 10, conv => b2i);
  package shared3 is new work.gen_shared generic map (k => 3);

  shared variable mc : macro10.counter;
  shared variable sc7 : shared7.counter;
  shared variable sc3 : shared3.counter;
begin
  process
    variable v : integer := 1;
  begin
    assert macro10.add3 ('1') = 4 severity failure;
    assert macro10.addk ('0') = 10 severity failure;
    macro10.incr2 (v);
    assert v = 3 severity failure;
    mc.bump5;
    mc.bumpk;
    assert mc.get = 15 severity failure;

    assert shared7.add3 (1) = 4 severity failure;
    assert shared7.addk (1) = 8 severity failure;
    assert shared3.addk (1) = 4 severity failure;
    sc7.bumpk;
    sc3.bumpk;
    sc3.bumpk;
    assert sc7.get = 7 severity failure;
    assert sc3.get = 6 severity failure;
    wait;
  end process;
end;
