      * COPYMEMBER: LIBLP/LPUPWKW
      ***************************************************************
      *    LPUPWKW
      *    ESTABLISHED LOAN RELATED UNIT PERIOD WORKERS
      * REV:
      ***************************************************************
       01  UPWK-WORK-AREA.
           03  UPWK-OUTPUT-AREA.
               05  UPWK-UNITPER-WK         PIC 9(3)V9(13)  COMP-3.
               05  UPWK-UNITPER-CONTR-WK   PIC 9(4)V9(12)  COMP-3.
               05  UPWK-LC-GRACE-FAC       PIC 9(3)V9(13)  COMP-3.
               05  UPWK-LC-MIN-FAC         PIC 9(3)V9(13)  COMP-3.
               05  UPWK-LC-MAX-FAC         PIC 9(3)V9(13)  COMP-3.
               05  UPWK-LC-PER-FAC         PIC 9(3)V9(13)  COMP-3.
               05  UPWK-ERRCD              PIC XX.
               05  UPWK-UNITPER-PER-YEAR   PIC 9(3)V9(9)   COMP-3.
               05  UPWK-DAYS-IN-UNITPER    PIC 9(3)        COMP-3.

           03  UPWK-INPUT-AREA.
               05  UPWK-UNITPER-CD         PIC X.
               05  UPWK-UNITPER-FREQ       PIC 9(3)        COMP-3.
