      * COPYMEMBER: LIBGB/HASHW

      *********************************************************
      *    HASH C ROUTINE WORKERS
      * MJD 130218 ADDED WORKERS FOR VERYANT COBOL VERSION OF PFS 'C' ROUTINES
      *********************************************************
       77  HASH-STAT                   PIC XX  VALUE "11".

       77  HASH-INCR        PIC 9(3).
       77  HASH-WK          USAGE SIGNED-INT.
       77  HASH-WK1         USAGE SIGNED-INT.
       77  HASH-WK2         USAGE SIGNED-INT.
       77  H-TOTAL          USAGE SIGNED-INT.


       01 D2B-CHAR      PIC X.
       01 D2B-BIN       PIC X(8).
       01 D2B-LB        PIC 9(2).
       01 D2B-RB        PIC 9(2).

       01 HASH-NUM-WK       PIC X COMP-X.

       01 D2B-DIV-RESULT   PIC 9.
       01 D2B-IDX          PIC S9.
       01 D2B-BIN-IDX      PIC 9.
       01 D2B-DIV-VAL-P2   PIC 9(3).
       01 HLK-CHAR         PIC X.
       01 HLK-BIN          PIC X(8).
       01 HLK-LB           PIC 9(2).
       01 HLK-RB           PIC 9(2).

       01  HASH-ROUTINE-WORK-AREA.
           03  FILLER                  PIC XX    VALUE "14".
           03  HASH-ACTION-CODE        PIC X     VALUE "H".
           03  HASH-COMMON-AREA        PIC X(52) VALUE " ".

      * (H) - HASH CHAR BUFFER ROUTINE
           03  HASH-H-AREA             REDEFINES HASH-COMMON-AREA.
               05  HASH-BUF            PIC X(40).
               05  HASH-SIZE           PIC 99.
               05  HASH-TOTAL          PIC 9(10).
  

      *********************************************************
