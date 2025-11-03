      * COPYMEMBER: LIBLP/LPANTIW
      ****************************************************
      *    ANTICIPATED ADJUSTMENT WORKERS
      *
      * REV:
      *  110307 BAH REDEFINED WK-ANTICERN TABLE SO THEY COULD BE
      *             INITIALIZED AT 0 WHEN ENTERING LONPB0, ANTICERN(6)
      *             WAS JUNK NOT GETTING CLEARED FROM A TFC CHANGE, AND
      *             CAUSED JUNK IN PROCEEDS AND ANTICERN(1), ALSO
      *             CHANGED LPANTI TO CLEAN UP AN UGLY 'IF' STATEMENT, PL# 729
      ****************************************************
       01  ADJUST-WORKERS.
      * INPUT / OUTPUT FIELDS:
           03  WK-ANTICERNX.
               05  WK-ANTICERN1       PIC S9(6)V99 VALUE 0.
               05  WK-ANTICERN2       PIC S9(6)V99 VALUE 0.
               05  WK-ANTICERN3       PIC S9(6)V99 VALUE 0.
               05  WK-ANTICERN4       PIC S9(6)V99 VALUE 0.
               05  WK-ANTICERN5       PIC S9(6)V99 VALUE 0.
               05  WK-ANTICERN6       PIC S9(6)V99 VALUE 0.
           03  WK-ANTICERN REDEFINES WK-ANTICERNX
                                      PIC S9(6)V99 OCCURS 6.
