      * COPYMEMBER: LIBSP/SP01CP
      ******************************************************************
      *
      * DESCRIPTION   : COLLECTOR POOL FILE
      *
      *=================================================================
      *
      * THERE IS MULTIPLE FILES FOR CPFILE THAT USE CP-PATH:
      *
      * GLOBAL - /USR/XXXX/R#/SP/CPFILE              [ CP-GLOBAL-PATH ]
      * LOCAL  - /USR/XXXX/R#/XX####/SP/CPFILE       [ CP-ALL-PATH    ]
      *
      *=================================================================
      * REV:
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * BAH 160406 CHANGED FOR A30
      *
      * KEC 2016.1006 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         MOVED CP-LTOUCH-DATE TO BE AFTER CP1-KEY.
      *
      ******************************************************************
       78  CP-SIZE                      VALUE 380.
       01  CP-REC.
           03  CP-BRNO                  PIC 9(4).
           03  CP1-KEY.
               05  CP-ID                PIC X(3).

      *>>XDD DATE=YYYYMMDD
           03  CP-LTOUCH-DATE           PIC 9(8) COMP-3.
           03  CP-DESC                  PIC X(20).

           03  CP-MEMBERS               PIC X(240).
           03  CP-A REDEFINES CP-MEMBERS.
               05  CP-COL-ID            PIC X(3) OCCURS 80.

           03  CP-ALPHA-FLAG            PIC X.
      *  COLLECTOR-COUNT & LAST-SUBSCRIPT ARE WORKERS IN CPRUN
      *  USED IN SPLITING ACCOUNTS EVENLY THROUGHOUT POOL
           03  CP-TOTAL-ACCOUNTS        PIC 9(7) COMP-3.
           03  CP-COLLECTOR-COUNT       PIC 9(3).
           03  CP-LAST-SUBSCRIPT        PIC 9(3).
           03  FILLER                   PIC X(97).
