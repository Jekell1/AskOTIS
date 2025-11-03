      * COPYMEMBER: LIBLP/LP01ER
      ***********************************
      *             (LP)
      *    ERROR PRINT FILE
      *           (132 BYTES)
      ***********************************
       01  ER-REC.
           03  ER-GENERAL-REC.
               05  ER-PROGNAME      PIC X(6).
               05  ER-KEY           PIC X(9).
               05  ER-KEY-NUM       REDEFINES ER-KEY
                                    PIC 9(9).
               05  ER-ERROR         PIC X(60).
               05  ER-ACTION        PIC X(57).

           03  ER-EOD-REC           REDEFINES ER-GENERAL-REC.
               05  ER-DATE          PIC 99/99/99.
               05  FILLER           PIC X.
               05  ER-TIME          PIC 99/99/99.
               05  FILLER           PIC X.
               05  ER-USER          PIC X(10).
               05  ER-DIR           PIC XX.
               05  ER-SLASH         PIC X.
               05  ER-PROGRAM       PIC X(6).
               05  FILLER           PIC X(2).
               05  ER-MESSAGE.
                   07  ER-TYPE      PIC X(4).
                   07  FILLER       PIC X.
                   07  ER-CODE      PIC XX.
                   07  ER-COMMA     PIC X.
                   07  ER-SUB-CODE  PIC XX.
                   07  FILLER       PIC X.
                   07  ER-E-PATH    PIC X(30).
                   07  FILLER       PIC X.
                   07  ER-E-KEY     PIC X(51).



