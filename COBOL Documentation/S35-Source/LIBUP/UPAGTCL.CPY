      * COPYMEMBER: LIBGL/GLAGTCL

      ******************************************************************
      *   ASCII  GENERAL LEDGER TRANSACTION FILE CLEAR ROUTINE
      *
      ******************************************************************
       CLEAR-AGT-FILE SECTION.
           MOVE ALL X"09" TO AGT-REC.

           MOVE 0      TO AGT-DATE1 AGT-SOURCE AGT-DATE2 AGT-BRANCH
                          AGT-ACCTNO AGT-DATE3 AGT-LINE
                          AGT-AMT AGT-PERIOD
                          AGT-POST-DATE AGT-POST-TIME.
 
           MOVE SPACES TO AGT-FLAG AGT-REF AGT-DRCRFG AGT-EXPL 
                          AGT-SMITH-SENT AGT-POST-USERID.
