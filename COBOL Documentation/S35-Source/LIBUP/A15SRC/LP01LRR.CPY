      * COPYMEMBER: LIBLP/LP01LRR
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LP01LRR
      *
      * DESCRIPTION   :  LOAN T(R)IAD RISK FILE
      *
      * CUSTOMER NAMES:  REGIONAL ACCEPTANCE
      *
      *=================================================================
      * REV:
      * KEC 2006.1006 {PR#00150} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ***NEW***
      *
      ******************************************************************
       78  LRR---MAX                                   VALUE 10.
       78  LRR---SIZE                                  VALUE 783.
      *=================================================================
       01  LRR-REC.
      
           03  LRR1-KEY.
               05  LRR-BRNO                            PIC 9(04).
                   88  LRR-BRNO-GLOBAL                 VALUE 9999.

      *-----------------------------------------------------------------
      *    GRID:
      *    -----
      *    X = BEHAVIOR SCORE
      *    Y = ACCOUNT BALANCE
      *-----------------------------------------------------------------
           03  LRR-GRID-TABLE.
               05  LRR-GRID-XY-OCCURS                  OCCURS LRR---MAX.

                   07  LRR-GRID-X-BEG                  PIC 9(06) COMP-6.
                   07  LRR-GRID-X-END                  PIC 9(06) COMP-6.
                   07  LRR-GRID-Y-BEG                  PIC 9(06) COMP-6.
                   07  LRR-GRID-Y-END                  PIC 9(06) COMP-6.

               05  LRR-GRID-X-OCCURS                   OCCURS LRR---MAX.
                   07  LRR-GRID-Y-OCCURS               OCCURS LRR---MAX.
                       09  LRR-GRID-ELE                PIC X(06).

      *-----------------------------------------------------------------
           03  LRR-RISK-FACTORS.
               05  LRR-RISK-L1                         PIC 9(01).
               05  LRR-RISK-L2                         PIC 9(01).
               05  LRR-RISK-L3                         PIC 9(01).
               05  LRR-RISK-M1                         PIC 9(01).
               05  LRR-RISK-M2                         PIC 9(01).
               05  LRR-RISK-M3                         PIC 9(01).
               05  LRR-RISK-H1                         PIC 9(01).
               05  LRR-RISK-H2                         PIC 9(01).
               05  LRR-RISK-H3                         PIC 9(01).

      *-----------------------------------------------------------------
           03  FILLER                                  PIC X(50).

