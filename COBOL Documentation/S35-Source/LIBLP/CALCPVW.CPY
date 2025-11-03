      * COPYMEMBER: LIBLP/CALCPVW
      ***************************************************************
      *    CALCPV
      *    PRESENT VALUE CALCULATION WORKERS
      * REV:
      *  161026 BAH CHANGED INTDATE AND 1STPYDATE TO 9(8)
      ***************************************************************
       01  CALCPV-WORK-AREA.
           03  CALCPV-OUTPUT-AREA.
               05  CALCPV-FINANCED          PIC 9(8)V99    COMP.
           03  CALCPV-INPUT-AREA.
               05  CALCPV-UNITPER-CD        PIC X.
               05  CALCPV-UNITPER-FREQ      PIC 9(3)       COMP.
               05  CALCPV-REGPYAMT          PIC 9(6)V99    COMP.
               05  CALCPV-1STPYAMT          PIC 9(6)V99    COMP.
               05  CALCPV-LASTPYAMT         PIC S9(6)V99   COMP
                                                           VALUE 0.
               05  CALCPV-TERM              PIC 9(4)       COMP.
               05  CALCPV-INTDATE           PIC 9(8)       COMP.
               05  CALCPV-1STPYDATE         PIC 9(8)       COMP.
               05  CALCPV-RATE              PIC S9(3)V9(4) COMP.
               05  CALCPV-DISFRMLA          PIC X.
           03  CALCPV-INTERMEDIATE-WORK-AREA.
               05  CALCPV-F                 PIC S9V9(17)   COMP.
               05  CALCPV-W                 PIC S999       COMP.
               05  CALCPV-I                 PIC S999V9(15) COMP.
               05  CALCPV-ADJ               PIC S999V9(15) COMP.
               05  CALCPV-ANI               PIC S999V9(15) COMP.
               05  CALCPV-LASTPYAMT-F       PIC S9(8)V9(8) COMP.
