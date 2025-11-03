      * COPYMEMBER: LIBGB/MESSWK
      ******************************************************************
      * REV:
      ******************************************************************

       01  MESS-X REDEFINES MESS.
           03  MESS-X-SUB   PIC 99.
           03  FILLER       PIC X.
           03  MESS-X-MON-1 PIC Z(9).99-.
           03  MESS-X-ALPH-1 REDEFINES MESS-X-MON-1 PIC X(13).
           03  FILLER       PIC X.
           03  MESS-X-MON-2 PIC Z(9).99-.
           03  MESS-X-ALPH-2 REDEFINES MESS-X-MON-2 PIC X(13).
           03  FILLER       PIC X.
           03  MESS-X-MON-3 PIC Z(9).99-.
           03  MESS-X-ALPH-3 REDEFINES MESS-X-MON-3 PIC X(13).
           03  FILLER       PIC X.
           03  MESS-X-MON-4 PIC Z(9).99-.
           03  MESS-X-ALPH-4 REDEFINES MESS-X-MON-4 PIC X(13).

       01  MESS-X REDEFINES MESS.
           03  FILLER       PIC XXX.
           03  MESS-X-DEC-1 PIC Z(9).9(8)-.
           03  FILLER       PIC X.
           03  MESS-X-DEC-2 PIC Z(9).9(8)-.
           03  FILLER       PIC X.
           03  MESS-X-DEC-3 PIC Z(9).9(8)-.

       01  MESS-X REDEFINES MESS.
           03  FILLER       PIC XXX.
           03  MESS-X-NUM-1 PIC Z(8)9-.
           03  FILLER       PIC X(4).
           03  MESS-X-NUM-2 PIC Z(8)9-.
           03  FILLER       PIC X(4).
           03  MESS-X-NUM-3 PIC Z(8)9-.
           03  FILLER       PIC X(4).
           03  MESS-X-NUM-4 PIC Z(8)9-.

       01  MESS-X REDEFINES MESS.
           03  MESS-MESS    PIC X(30).
           03  MESS-X-MON-5 PIC Z(9).99-.

       01  MESS-X REDEFINES MESS.
           03  MESS-GL-SUB  PIC 99.
           03  FILLER       PIC XX.
           03  MESS-GL-SUB2 PIC 99.
           03  FILLER       PIC XX.
           03  MESS-GL-ACCT PIC 9(10).

