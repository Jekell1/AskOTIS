      * COPYMEMBER: LIBSP/SP01CP

      **************************************************
      *          COLLECTOR POOL FILE 
      * REV:
      *  MJD 990125 ----- UPDATED IN FLEXGEN -----
      **************************************************
       01  CP-REC.
           03  CP1-KEY.
               05  CP-ID                PIC X(3).
           03  CP-DESC                  PIC X(20).

      *
           03  CP-MEMBERS               PIC X(240).
           03  CP-A REDEFINES CP-MEMBERS.
               05  CP-COL-ID            PIC X(3) OCCURS 80.
           03  FILLER                   PIC X(26).
           03  CP-ALPHA-FLAG            PIC X.
      *  COLLECTOR-COUNT & LAST-SUBSCRIPT ARE WORKERS IN CPRUN
      *  USED IN SPLITING ACCOUNTS EVENLY THROUGHOUT POOL
           03  CP-TOTAL-ACCOUNTS        PIC 9(5) COMP-3.
           03  CP-COLLECTOR-COUNT       PIC 99.
           03  CP-LAST-SUBSCRIPT        PIC 99.
