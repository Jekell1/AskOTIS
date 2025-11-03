      * COPYMEMBER: LIBLP/LPMNUIW
      ********************************************
      *    MENU FOR CONVERSION ROUTINES (WORKERS)
      * 20191030 BAH REMOVED 5. LEDGER CARD EXTRACTION "W0W"
      *              MENU-MAX NOW 4 NOT 5
      ********************************************
       01  MENU-FIELD.
           03  FILLER             PIC X(4).
           03  MENU-5             PIC 9(1).
           03  FILLER             PIC X(1).
       01  MENU-FIELDX           REDEFINES MENU-FIELD.
           03  FILLER             PIC X(4).
           03  MENU-NO            PIC 9(2).
       01  MENU-FG                PIC 99.
       01  MENU-MAX               PIC 99 VALUE 4.
       01  MENU-MSG-MAX           PIC 99 VALUE 3.
       01  MENU-PG-FM             PIC X(42)   VALUE
           "E0EJ0JN0NF0F".
       01  MENU-PG-FMX           REDEFINES MENU-PG-FM.
           03  MENU-PROG-FORM     PIC X(3) OCCURS 4.
       01  MENU-MSGX.
           03  FILLER             PIC X(40) VALUE
           "SELECT #, F1-SCAN, F7-EXIT             :".
           03  FILLER             PIC X(40) VALUE
           "1-BORROWERS, 2-SALES FIN, 3-DIRECT LOAN,".
           03  FILLER             PIC X(40) VALUE
           " 4-BAL FWD POST ".
       01  MENU-MSGS             REDEFINES MENU-MSGX.
           03  MENU-MSG           PIC X(40) OCCURS 3.
