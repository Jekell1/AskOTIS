      * COPYMEMBER: LIBSP/SPINQ1.CMD
      ***************************************************
      *  mjd 061005 Added legend*-no-f3 fields for use
      *             with bwlns3 scan window.      PR#2843
      ***************************************************
       01  LEGEND-01.
           03  FILLER            PIC X(40) VALUE
           "F7-EXIT, F2-MEMO, F3-LOANS, F4-PAYMNTS, ".
           03  FILLER            PIC X(36) VALUE
           "F5-PAYOFF, F6-SCAN, ENTER ACTV:".
       01  LEGEND-01-NO-F3.
           03  FILLER            PIC X(30) VALUE
           "F7-EXIT, F2-MEMO, F4-PAYMNTS, ".
           03  FILLER            PIC X(36) VALUE
           "F5-PAYOFF, F6-SCAN, ENTER ACTV:".
       01  LEGEND-01-SKIP.
           03  FILLER            PIC X(37) VALUE
           "F7-EXIT, F1-SKIP, F2-MEMO, F3-LOANS, ". 
           03  FILLER            PIC X(40) VALUE
           "F4-PMTS, F5-PAYOFF, F6-SCAN, ENTER ACTV:".
       01  LEGEND-01-SKIP-NO-F3.
           03  FILLER            PIC X(27) VALUE
           "F7-EXIT, F1-SKIP, F2-MEMO, ". 
           03  FILLER            PIC X(40) VALUE
           "F4-PMTS, F5-PAYOFF, F6-SCAN, ENTER ACTV:".
       01  LEGEND-02.
           03  FILLER            PIC X(40) VALUE
           "F7-UPD, F1-CLR, F2, F3, F4, F5, F6-SCAN,".
           03  FILLER            PIC X(24) VALUE
           "^, v, ENTER RESULT CODE:".
       01  LEGEND-03.
           03  FILLER            PIC X(40) VALUE
           "F7-UPD, F1-CLR, F2, F3, F4, F5, ^, v, EN".
           03  FILLER            PIC X(25) VALUE
           "TER PROMISED TO PAY DATE:".
       01  LEGEND-04.
           03  FILLER            PIC X(40) VALUE
           "F7-UPD, F1-CLR, F2, F3, F4, F5, ^, v, EN".
           03  FILLER            PIC X(27) VALUE
           "TER PROMISED TO PAY AMOUNT:".
       01  LEGEND-05.
           03  FILLER            PIC X(40) VALUE
           "F7-UPD, F1-CLR, F2, F3, F4, F5, ^, v, EN".
           03  FILLER            PIC X(25) VALUE
           "TER SECOND PROMISED DATE:".
       01  LEGEND-06.
           03  FILLER            PIC X(40) VALUE
           "F7-UPD, F1-CLR, F2, F3, F4, F5, F6-SCAN,".
           03  FILLER            PIC X(33) VALUE
           " ^, v, ENTER FORM NO. TO BE SENT:".
       01  LEGEND-07.
           03  FILLER            PIC X(40) VALUE
           "F7-UPD, F1-CLR, F2, F3, F4, F5, F6-SCAN,".
           03  FILLER            PIC X(24) VALUE
           "^, v, ENTER ACTION CODE:".
       01  LEGEND-08.
           03  FILLER            PIC X(40) VALUE
           "F7-UPD, F1-CLR, F2, F3, F4, F5, F6-SCAN,".
           03  FILLER            PIC X(25) VALUE
           "^, v, ENTER COLLECTOR ID:".
       01  LEGEND-09.
           03  FILLER            PIC X(40) VALUE
           "F7-UPD, F1-CLR, F2, F3, F4, F5, ^, v, EN".
           03  FILLER            PIC X(21) VALUE
           "TER FLASHING MESSAGE:".
       01  LEGEND-10.
           03  FILLER            PIC X(40) VALUE
           "F7-UPD, F1-CLR, F2, F3, F4, F5, ^, v, EN".
           03  FILLER            PIC X(32) VALUE
           "TER LOAN MEMO LINE  (F6-expand):".
       01  LEGEND-MEMOIN.
           03  FILLER            PIC X(51) VALUE
           "F7-END, F1-RESET, ^, v, ENTER MEMO LINES:".
       01  LEGEND-AM-PM.
           03  FILLER            PIC X(20) VALUE
           " ENTER 'Am' or 'Pm':".
       01  LEGEND-MAX.
           03  FILLER            PIC X(42) VALUE
           "F7-UPDATE, F1-CLEAR, F2, F3, F4, F5, ^, v:".
