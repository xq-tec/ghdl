-- A signature whose profile matches no subprogram denoted by the name (LRM08 4.4).
entity err_signature_mismatch is end entity;
architecture a of err_signature_mismatch is
  procedure gen_p generic (type t) (x : t) is begin null; end procedure;
  procedure bad is new gen_p [integer, bit];
begin end architecture;
