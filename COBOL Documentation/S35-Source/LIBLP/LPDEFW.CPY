      * COPYMEMBER: LIBLP/LPDEFW
      **************************************************************************
      *              LPDEFW
      *    DEFERMENT CALCULATION WORKERS
      *
      * REV:
      *  JTG 061688 ADDED BEG-PERIOD, END-PERIOD AND PAYS-MADE
      *  JTG 051089 CHANGED FIELDS TO COMP
      *  JTG 121489 ADDED DEF-WKRATE (FOR INPUTTED RATE)
      *  BAH 052190 ADDED DEF-LCHG-REFUND FOR PACESETTER
      *             ADDED COMMON DEF CALL AREA
      *  JTG 071392 ADDED DEF-DUEDATE, DEF-DUETERM
      *             FOR DEFERMENT BASE CODE 7
      *  JTG 050994 ADDED DEF-PAYDATE
      *  JTG 062795 ADDED DEF-WORKER, TO ALLOW A SIGNED WORKER
      *  KEC 082895 ADDED DEF-WORKER1 & 2 TO ALLOW FOR COMPARISON
      *             BETWEEN 2 VALUES.
      *  MJD 071696 ADDED DEF-ELAPSED-MTHS, HOLD WORKER FOR ELAPSED-MONTHS
      *  JTG 000621 CORRECTED FORMULA'S 99, 98 AND 97 INTEREST BEARING
      *             DEFERMENT CHARGES, BY SETTING UP DEF-TOTAL-INTDUE
      *             AS A INPUT FIELD AND NOT USING INDU-INTEREST MERCURY #224
      *  MJD 121416 ADDED DEF-PERCENT-V10 (71225 PR#384)
      **************************************************************************
      * INPUT AREA:
       01  DEF-NODEF          PIC S9        COMP VALUE 1.
       01  DEF-PAYDATE        PIC 9(8)      COMP VALUE 0.
       01  DEF-TOTAL-INTDUE   PIC S9(7)V99  COMP  VALUE +0.

      * USED BY LONPFC.C TO PASS INPUTTED RATE TO LPDEF:
       01  DEF-WKRATE         PIC 999V999   COMP   VALUE 0.

      * OUTPUT AREA:
       01  DEF-AMOUNT         PIC S9(7)V99  COMP  VALUE +0.
       01  DEF-LCHG-REFUND    PIC S9(5)V99  COMP  VALUE +0.
       01  DEF-DUEDATE        PIC 9(8)      COMP  VALUE 0.
       01  DEF-DUETERM        PIC 9(3)      COMP  VALUE 0.

      * WORK AREA:
       01  DEF-PERCENT        PIC 9(4)V9(4) COMP  VALUE 0.
       01  DEF-PERCENT-V10    PIC 9(4)V9(10) COMP  VALUE 0.
       01  DEF-BASE           PIC 9(6)V99   COMP  VALUE 0.
       01  DEF-WORKER         PIC S9(7)V99  COMP  VALUE 0.
       01  DEF-WORKER1        PIC S9(7)V99  COMP  VALUE 0.
       01  DEF-WORKER2        PIC S9(7)V99  COMP  VALUE 0.
       01  DEF-WRP            PIC 9(4)      COMP  VALUE 0.
       01  DEF-BEG-PERIOD-DATE     PIC 9(8)      COMP  VALUE 0.
       01  DEF-END-PERIOD-DATE     PIC 9(8)      COMP  VALUE 0.
       01  DEF-SAVEPDTH-DATE       PIC 9(8)      COMP  VALUE 0.
       01  DEF-PRIORRB        PIC S9(7)V99  COMP  VALUE 0.
       01  DEF-ADVANCE        PIC S9        COMP.
       01  DEF-PAYS-MADE      PIC 999       COMP.
       01  DEF-ELAPSED-MTHS   PIC 9(3)      COMP  VALUE 0.

      *********************************************************
      *    COMMON AREA FOR CALLED DEFERMENT CALCULATIONS:
      *********************************************************
       01  DEF-FORM-PATH.
           03  FILLER                PIC X(6)     VALUE "CL/DEF".
           03  DEF-FORM-PROG-TYPE    PIC X.
           03  DEF-FORM-PROG-NO      PIC 99.
       01  DEF-CANCEL-FG             PIC X        VALUE "Y".
       01  DEF-COMMON.
           03  DEF-COMMON-RTNCD      PIC X.
           03  DEF-COMMON-DEFCHG     PIC S9(7)V99 COMP-3.
           03  DEF-COMMON-AREA       PIC X(40).
      * USED FOR INDIANA DEFERMENT CHARGE FORMULA 8:
           03  FILLER                REDEFINES DEF-COMMON-AREA.
               05  DEF-POST-DATE     PIC 9(8)     COMP-3.
               05  DEF-LC-REBATE     PIC S9(7)V99 COMP-3.
