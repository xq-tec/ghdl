-- Before VHDL-2008, an expanded name with an entity prefix only denotes
-- declarations of the entity.
entity err_93 is
end entity;

architecture behav of err_93 is
  constant C : natural := 1;
  constant D : natural := err_93.C;
begin
end architecture;
