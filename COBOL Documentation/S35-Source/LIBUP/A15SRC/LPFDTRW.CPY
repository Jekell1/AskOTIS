      * COPYMEMBER: LIBLP/LPFDTRW
      ******************************************************************
      *               TR-CODE  =  "W"
      *              CASH BALANCING RECORD
      *
      * NOTE: THIS RECORD IS FOR TRACKING/REFERENCE ONLY.
      *       IT DOES NOT UPDATE GL OR STATS
      *
      * REV:
      ******************************************************************
       01  TW-REC.
           03  TW-DAY              PIC 99      COMP.
           03  TW-BRNO             PIC 9(4)    COMP.
           03  TW-CODE             PIC X.
           03  TW-TILLNO           PIC X.
           03  TW-FILLER           PIC X(09).
      *-------------------------------------------------
           03  TW-CLOSE-TABLE      OCCURS 20.
               05  TW-POSTNAME     PIC X(08).
               05  TW-TIME         PIC 9(04)     COMP-3.
               05  TW-AMOUNT       PIC S9(07)V99 COMP-3.
               05  TW-OVERSHORT    PIC S9(07)V99 COMP-3.
           03  FILLER              PIC X(116).
