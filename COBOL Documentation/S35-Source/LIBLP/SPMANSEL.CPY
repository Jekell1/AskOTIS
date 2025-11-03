      * COPYMEMBER: LIBLP/SPMANSEL
      *********************************************
      *             (SPMANSEL)
      *    ACTION ENTRY FIELD FOR SPMAN'S
      *    REV:
      *        JFF 070396 ADDED PAGE 20.
      *********************************************
       01  ACTION                VALUE SPACES.
           03  CHKEY             PIC X(4).
           03  CHNO              PIC 9(2).
           03  CHNO-X           REDEFINES CHNO
                                 PIC X(2).
       01  FORM-NUM1            REDEFINES ACTION.
           03  FORM-KEY1         PIC X(4).
           03  FORM-P1           PIC X.
           03  FORM-SEL1         PIC 9.
       01  FORM-NUM2            REDEFINES ACTION.
           03  FORM-KEY2         PIC X(3).
           03  FORM-P2           PIC X.
           03  FORM-SEL2         PIC 99.
               88 FORM-2-DIGITS  VALUE 10 11 12 13 14 15
                                       16 17 18 19 20 21 22 23.
