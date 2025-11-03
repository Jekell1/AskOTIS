      * COPYMEMBER: LIBLP/LP01LRB
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LP01LRB
      *
      * DESCRIPTION   :  LOAN T(R)IAD BEHAVIOR FILE
      *
      * CUSTOMER NAMES:  REGIONAL ACCEPTANCE
      *
      *=================================================================
      * REV:
      * KEC 2003.1215 {PR#00000} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ***NEW***
      *
      ******************************************************************
       78  LRB---MAX                                   VALUE 10.
       78  LRB---SIZE                                  VALUE 783.
      *=================================================================
       01  LRB-REC.
      
           03  LRB1-KEY.
               05  LRB-BRNO                            PIC 9(04).
                   88  LRB-BRNO-GLOBAL                 VALUE 9999.

      *-----------------------------------------------------------------
      *    GRID:
      *    -----
      *    X = BEHAVIOR SCORE
      *    Y = ACCOUNT BALANCE
      *-----------------------------------------------------------------
           03  LRB-GRID-TABLE.
               05  LRB-GRID-XY-OCCURS                  OCCURS LRB---MAX.

                   07  LRB-GRID-X-BEG                  PIC 9(06) COMP-6.
                   07  LRB-GRID-X-BEGX       REDEFINES LRB-GRID-X-BEG
                                                       PIC X(03).
                   07  LRB-GRID-X-END                  PIC 9(06) COMP-6.
                   07  LRB-GRID-X-ENDX       REDEFINES LRB-GRID-X-END
                                                       PIC X(03).
                   07  LRB-GRID-Y-BEG                  PIC 9(06) COMP-6.
                   07  LRB-GRID-Y-END                  PIC 9(06) COMP-6.

               05  LRB-GRID-X-OCCURS                   OCCURS LRB---MAX.
                   07  LRB-GRID-Y-OCCURS               OCCURS LRB---MAX.
                       09  LRB-GRID-ELE                PIC X(06).

      *-----------------------------------------------------------------
           03  LRB-RISK-FACTORS.
               05  LRB-RISK-L1                         PIC 9(01).
               05  LRB-RISK-L2                         PIC 9(01).
               05  LRB-RISK-L3                         PIC 9(01).
               05  LRB-RISK-M1                         PIC 9(01).
               05  LRB-RISK-M2                         PIC 9(01).
               05  LRB-RISK-M3                         PIC 9(01).
               05  LRB-RISK-H1                         PIC 9(01).
               05  LRB-RISK-H2                         PIC 9(01).
               05  LRB-RISK-H3                         PIC 9(01).

      *-----------------------------------------------------------------
           03  FILLER                                  PIC X(50).

