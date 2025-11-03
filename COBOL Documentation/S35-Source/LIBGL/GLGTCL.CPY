      * COPYMEMBER: LIBGL/GLGTCL

      ******************************************************************
      *    GLGTCL  GENERAL LEDGER TRANSACTION FILE CLEAR ROUTINE
      *
      * REV:
      * BLM 110712 ADDED CLEAR OF GT-POST-DATE, GT-POST-TIME
      ******************************************************************
       CLEAR-GT-FILE SECTION.
           MOVE SPACES TO GT-REC.

           MOVE 0      TO GT-DATE1 GT-SOURCE GT-DATE2 GT-BRANCH
                          GT-ACCTNO GT-DATE3 GT-LINE
                          GT-AMT GT-PERIOD
                          GT-POST-DATE GT-POST-TIME.
