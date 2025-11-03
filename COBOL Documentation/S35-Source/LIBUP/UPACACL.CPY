      * COPYMEMBER: LIBUP/UPACACL
      ******************************************************************
      *    UPACACL    ASCII DRAWER FILE CLEAR ROUTINE
      *
      *  CS 210423 REMOVE FUTURE FIELDS
      ******************************************************************
       CLEAR-ACA-FILE SECTION.
           MOVE ALL X"09" TO ACA-REC.

           MOVE 0      TO ACA-BRNO ACA-DATE ACA-TILLNO ACA-BEGBAL
                          ACA-OPENED-HHMM ACA-CLOSED-HHMM  ACA-ENDBAL 
                          ACA-RUNNING ACA-DEPOSIT ACA-NET-CASH-NOTES
                          ACA-CHECK-DEPOSIT.
           MOVE SPACES TO ACA-DESC ACA-BALBY ACA-OPEN-USERID
                          ACA-PASSWORD.
