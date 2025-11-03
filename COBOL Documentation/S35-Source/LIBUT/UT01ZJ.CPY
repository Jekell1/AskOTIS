      * COPYMEMBER: LIBUT/UT01ZJ
      ******************************************************************
      *
      *   DESCRIPTION: JAY'S TEST FILE LAYOUT FOR TESTING
      *
      ******************************************************************
      * REV:
      * JKC 2022-1213 ***NEW***
      ******************************************************************
       78  ZJ-SIZE                     VALUE 9999.
       01  ZJ-REC.
           03  ZJ1-KEY.
               05  ZJ-OWNBR            PIC 9(4).
               05  ZJ-ACCTNO           PIC 9(8).
           03  ZJ-CLASS                PIC 9(3).

           03  ZJ-BWTABLEX.
               05  ZJ-SSNO1            PIC 9(9).
               05  ZJ-MAKERCD1         PIC X.
               05  ZJ-SSNO2            PIC 9(9).
               05  ZJ-MAKERCD2         PIC X.
               05  ZJ-SSNO3            PIC 9(9).
               05  ZJ-MAKERCD3         PIC X.
               05  ZJ-SSNO4            PIC 9(9).
               05  ZJ-MAKERCD4         PIC X.
           03  ZJ-BWTABLE              REDEFINES ZJ-BWTABLEX.
               05  ZJ-BORRTBL          OCCURS 4 TIMES.
                   07  ZJ-SSNO         PIC 9(9).
                   07  ZJ-MAKERCD      PIC X.

           03  ZJ-ORGST                PIC XX.
           03  ZJ-ORGBR                PIC 9(4)   COMP-3.
           03  ZJ-OWNST                PIC XX.
           03  ZJ-SPRCLASS             PIC 9(3).

           03  LN-POCD                 PIC XX.
               88  LN-POCD-NONLOAN         VALUE "**" "*I" "*R" "*U".
               88  LN-POCD-OPEN            VALUE "  ".
               88  LN-POCD-POFF            VALUE " 1" THRU "ZZ".
               88  LN-POCD-INRESCISSION    VALUE "*I".
               88  LN-POCD-RESCIND         VALUE "*R".
               88  LN-POCD-PB-RESC         VALUE "*P".
               88  LN-POCD-UNWIND          VALUE "*U".
               88  LN-POCD-VOID            VALUE "**".
               88  LN-POCD-TRANSFERRED     VALUE "BT".

      $XFD DATE=YYYYMMDD
           03  ZJ-ENTDATE              PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  ZJ-LOANDATE             PIC 9(8)      COMP-3.

           03  ZJ-FEETBL               OCCURS 20 TIMES.
               05  ZJ-FEECODE          PIC XX.
               05  ZJ-FEEAMT           PIC 9(5)V99   COMP-3.
               05  ZJ-FEEOURS          PIC X.
               05  ZJ-FEEFINCHG        PIC X.

           03  ZJ-PERIOD-TBL           OCCURS 3.
               05  ZJ-ACTIVITY-TBL     OCCURS 2.
                   07  ZJ-TYPE-TBL     OCCURS 11.
                       09  ZJ-NO       PIC S9(7)    COMP-3.
                       09  ZJ-AMT      PIC S9(9)V99 COMP-3.
                       09  ZJ-O2       PIC S9(9)V99 COMP-3.
                       09  ZJ-INT      PIC S9(9)V99 COMP-3.
