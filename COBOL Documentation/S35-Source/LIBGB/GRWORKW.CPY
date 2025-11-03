      * COPYMEMBER: LIBGB/GRWORKW
      ******************************************************************
      *
      * LIBGB/GRWORKW - VARIABLES USED WITH GRWORK
      *                 ALSO CONTAINS VARIABLES FOR LIBGB/GRTOTALS (GTT)
      *
      * REV:
      * KEC 032996 ADDED GTT-WORKERS.
      * BLV 010918 EXPANDED SEARCH TABLES FROM 500 TO 700
      * BLV 040107 ADDED WGR-SID-LEVEL, TABLE TO STORE LEVEL OF GROUP;
      *            CORRESPONDS TO WGR-SID
      * BLV 040520 EXPANDED SEARCH TABLES FROM 700 TO 998
      * BLV 061109 EXPANDED SEARCH TABLES FROM 998 TO 2000
      ******************************************************************
       01  WGR-WORD.
           03  WGR-CHAR        PIC X     OCCURS 4.
       01  WGR-WORDX           REDEFINES WGR-WORD.
           03  WGR-DIGIT       PIC 9     OCCURS 4.
       01  WGR-WORDY           REDEFINES WGR-WORD.
           03  WGR-NUMB        PIC 9(4).
       01  WGR-ZERO-TEST       PIC X(4) REDEFINES WGR-WORD.
           88 WGR-WORD-ZERO    VALUE
           "    " "0000" "0   " "00  " "000 " "   0".
       01  WGR-CNT1            PIC 9(6)  VALUE 0.
       01  WGR-CNT2            PIC 9(6)  VALUE 0.
       01  WGR-TYPE            PIC X     VALUE " ".
       01  WGR-ERR             PIC 9     VALUE 0.
       01  WGR-MIN-LEVEL       PIC 99    VALUE 0.
       01  WGR-LEVEL           PIC 9     VALUE 0.

       01  WGR-SEARCH.
           03  WGR-SID-TABLE.
               05  WGR-SID     PIC X(4)  OCCURS 2000.
           03  WGR-SEARCH-TABLE.
               05  WGR-SSTART  PIC 9(4)  COMP OCCURS 2000.
               05  WGR-SEND    PIC 9(4)  COMP OCCURS 2000.
               05  WGR-SLOC    PIC 9(4)  COMP OCCURS 2000.
               05  WGR-SID-LEVEL PIC 9   COMP OCCURS 2000.
           03  WGR-SDEPTH      PIC 9(4)  VALUE 0.
           03  WGR-SCURR-TABLE.
               05  WGR-SCURR   PIC 9(4)  COMP  OCCURS 15.
           03  WGR-SRESULT     PIC 9(4)  VALUE 0.
           03  WGR-TEST-GROUP  PIC X(4)  VALUE SPACES.
           03  WGR-SSTABLE     PIC 9(4)  VALUE 0.
           03  WGR-TDEX1       PIC 9(4)  VALUE 0.
           03  WGR-TDEX2       PIC 9(4)  VALUE 0.
           03  WGR-TDEX3       PIC 9(4)  VALUE 0.
           03  WGR-TCURR       PIC 9(4)  VALUE 0.
           03  WGR-TPLUS       PIC 9(4)  VALUE 0.
           03  WGR-NO-BRANCHES PIC 9(4)  VALUE 0.
           03  WGR-LEVEL-TAB.
               05  WGR-LEVEL-TABLE  PIC X  OCCURS 10.

       01  WGR-MSGS.
           03  FILLER      PIC X(20) VALUE "LOCATION NOT ON FILE".
           03  FILLER      PIC X(20) VALUE "NEED BRANCH NUMBER  ".
           03  FILLER      PIC X(20) VALUE "GROUP NOT ON FILE   ".
           03  FILLER      PIC X(20) VALUE "GROUP LEVEL WRONG   ".
       01  WGR-MSGX        REDEFINES WGR-MSGS.
           03  WGR-MSG     PIC X(20) OCCURS 4.

      ******************************************************************
      *
      *  VARIABLES FOR LIBGB/GRTOTALS (GTT)
      *
      ******************************************************************
       01  GTT-WORKERS.
           03  GTT-FIRST-TIME            PIC X(1)          VALUE SPACES.
           03  GTT-HEADER-FG             PIC X(1)          VALUE SPACES.
           03  GTT-SV-ENT-GROUP          PIC X(4)          VALUE SPACES.

           03  GTT-SUBSCRIPTS.
               05  GTT-GROUP-SUB         PIC 9(4)          VALUE ZEROES.
               05  GTT-PREV-SUB          PIC 9(4)          VALUE ZEROES.
               05  GTT-TOTAL-SUB         PIC 9(4)          VALUE ZEROES.
               05  GTT-LEVEL-SUB         PIC 9(4)          VALUE ZEROES.

           03  GTT-PREV-SCURR-TABLE.
               05  GTT-PREV-SCURR        PIC 9(4)  COMP    OCCURS 15.
