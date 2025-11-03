      * COPYMEMBER: LIBWI/FEEENTW
      *******************************************************************
      *    FEE ENTRY WINDOW WORKERS
      * REV:
      *  JTG 090497 CHANGED FROM 'LIBWI/FEEET1W' --> 'LIBWI/FEEENTW'
      *             CHANGED ALL FIELDS 'FW1-' --> 'FW-'
      *             ADDED FW-FEEFINCHG WORLD A90
      *******************************************************************
       01  FW-DISPLAY-LIST.
           03  FILLER          PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER          PIC X(40) VALUE
           "011 021 031 041 051 061 071 081 091 101 ".
           03  FILLER          PIC X(40) VALUE
           "012 022 032 042 052 062 072 081 092 102 ".
           03  FILLER          PIC X(40) VALUE
           "013 023 033 043 053 063 073 083 093 103.".

       01  FW-WIN-BUF.
           03  FW-FEECODE-BUF        PIC XX.
           03  FW-FEEDESC-BUF        PIC X(15).
           03  FW-FEEAMT-BUF         PIC ZZZ9.99.
           03  FW-FEEOURS-BUF        PIC X.

       01  FW-WIN-LIST.
           03  FW-LIST1              PIC 9(2).
           03  FILLER                PIC XX       VALUE "0 ".
           03  FW-LIST2              PIC 9(2).
           03  FILLER                PIC XX       VALUE "1 ".
           03  FW-LIST3              PIC 9(2).
           03  FILLER                PIC XX       VALUE "2 ".
           03  FW-LIST4              PIC 9(2).
           03  FILLER                PIC XX       VALUE "3.".

       01  FW-FIRST-FIELD            PIC 99.
       01  FW-PROTECT-FEE1-FG        PIC X.
       01  FW-PROTECT-FEE2-FG        PIC X.

       01  FW-FEE-TABLE.
           03  FW-FEE-TBL           OCCURS 10.
               05  FW-FEECODE        PIC XX.
               05  FW-FEEDESC        PIC X(15).
               05  FW-FEEAMT         PIC 9(4)V99.
               05  FW-FEEOURS        PIC X.
               05  FW-FEEFINCHG      PIC X.

       01  FW-FEE-TOTAL-TABLE.
           03  FW-FEE-TOTAL         OCCURS 3
                                     PIC S9(7)V99.

       01  FW-ELE                    PIC 9(2).
       01  FW-ONE                    PIC S9.
       01  FW-MAX                    PIC 99    VALUE 10.

       01  FW-FEE-TOTAL-LIST         PIC X(33) VALUE
           "TOT1P TOT1 TOT2P TOT2 TOT3P TOT3.".
       01  FW-FEE-TOTAL-BUFFER.
           03  FW-FEE-TOTAL-BUF     OCCURS 3.
               05  FW-FEE-TOTAL-P    PIC X(12).
               05  FW-FEE-TOTAL-A    PIC Z(5)9.99.

      **************************************************************
      *    FEE-ELEMENT-SPECIFICATIONS
      **************************************************************
       01  FEESPEC-TABLE.
      * 1. FEECODE #1
           03  FILLER          PIC X(8) VALUE "E  A020 ".
      * 2. FEECODE #2
           03  FILLER          PIC X(8) VALUE "E  A020 ".
      * 3. FEECODE #3
           03  FILLER          PIC X(8) VALUE "E  A020 ".
      * 4. FEECODE #4
           03  FILLER          PIC X(8) VALUE "E  A020 ".
      * 5. FEECODE #5
           03  FILLER          PIC X(8) VALUE "E  A020 ".
      * 6. FEECODE #6
           03  FILLER          PIC X(8) VALUE "E  A020 ".
      * 7. FEECODE #7
           03  FILLER          PIC X(8) VALUE "E  A020 ".
      * 8. FEECODE #8
           03  FILLER          PIC X(8) VALUE "E  A020 ".
      * 9. FEECODE #9
           03  FILLER          PIC X(8) VALUE "E  A020 ".
      *10. FEECODE #10
           03  FILLER          PIC X(8) VALUE "E  A020 ".
       01  FEESPEC-TAB        REDEFINES FEESPEC-TABLE.
           03  FEESPEC-REC     OCCURS 10 TIMES.
               05  FEESPEC     PIC X(7).
               05  FEECHFG     PIC X.

                
      **************************************************
