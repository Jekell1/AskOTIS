      * COPYMEMBER: LIBLP/LPEXTW
      ******************************************************************
      *   E X T R A C T   D A T A
      *   DATA TYPES: A-ALPHA
      *               B-NUMERIC
      *               C-SIGNED NUMERIC
      *               D-COMP-3
      *               E-SIGNED COMP-3
      *               F-COMP(BINARY)
      *               G-SIGNED COMP(BINARY)
      *
      * REV:
      *   TYPES F,G ADDED 06/89  JPN
      *  MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      *  BAH 991020 EXPANDED DATA-AREA FROM 1800 TO 4500 TO HANDLE LA-FILE
      * KEC 2016.0919 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         EXPANDED DATA-AREA FROM 4500 TO 8000 TO HANDLE LA-FILE
      *
      ******************************************************************
       01  A-BUF           VALUE " ".
           03  A-CHAR      PIC X      OCCURS 40.
       01  B-BUF REDEFINES A-BUF.
           03  FILLER      PIC X(22).
           03  B-CHAR      PIC 9(18).
       01  C-BUF REDEFINES A-BUF.
           03  FILLER      PIC X(22).
           03  C-CHAR      PIC S9(18).
       01  D-BUF REDEFINES A-BUF.
           03  FILLER      PIC X(30).
           03  D-CHAR      PIC 9(18)  COMP-3.
       01  E-BUF REDEFINES A-BUF.
           03  FILLER      PIC X(31).
           03  E-CHAR      PIC S9(17) COMP-3.
       01  F-BUF REDEFINES A-BUF.
           03  FILLER      PIC X(32).
           03  F-CHAR      PIC 9(18)  COMP.
       01  G-BUF REDEFINES A-BUF.
           03  FILLER      PIC X(32).
           03  G-CHAR      PIC S9(17) COMP.
       01  DATA-SUB        PIC 9(6)   COMP-3.
       01  DATA-OFFSET     PIC 9(6)   COMP-3.
       01  DATA-FIELD      PIC 9(4)   COMP-3.
       01  DATA-KEY        PIC 9(4)   COMP-3.
       01  DATA-TYPE       PIC 9(2).
       01  DATA-DECIMAL    PIC 9(2)   COMP-3.
       01  DATA-BYTES      PIC 9(2)   COMP-3.
       01  DATA-DIGITS     PIC 9(2)   COMP-3.
       01  DATA-NUMOUT     PIC S9(13)V9(5).
       01  DATA-AREA.
           03  DATA-CHAR   PIC X      OCCURS 8000.
       01  COMP-BIN-DATA-01.
           03  COMP-BIN-DATA   PIC X(18)  VALUE
              "112233344555667788".
           03  COMP-BIN-TBL REDEFINES COMP-BIN-DATA
                           PIC 9 OCCURS 18.
       01  SCOMP-BIN-DATA-01.
           03  SCOMP-BIN-DATA  PIC X(18)  VALUE
              "112233444556667788".
           03  SCOMP-BIN-TBL REDEFINES SCOMP-BIN-DATA
                           PIC 9 OCCURS 18.
       01  COMP-LOW-BYTES  PIC S9(13)V9(5) COMP.
       01  COMP-HIGH-BYTES PIC S9(13)V9(5) COMP.
