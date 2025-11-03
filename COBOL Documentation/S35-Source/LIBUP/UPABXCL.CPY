      * COPYMEMBER: LIBUP/UPBXCL
      ********************************************************
      *      ASCII BORROWER TRAILER FILE CLEAR ROUTINE
      *
      ********************************************************
       CLEAR-ABX-FILE SECTION.
           MOVE ALL X"09" TO ABX-REC.

           MOVE 0 TO ABX-KYC-ISSDATE
                     ABX-KYC-EXPDATE
                     ABX-KYC-LAST-VERDATE.

           MOVE SPACES TO ABX-ALT-ADR1 ABX-ALT-ADR2 ABX-ALT-CITY
                          ABX-ALT-STATE ABX-ALT-ZIP ABX-PREV-ADR1  
                          ABX-PREV-ADR2 ABX-PREV-CITY ABX-PREV-STATE 
                          ABX-PREV-ZIP ABX-KYC-ADR1 ABX-KYC-ADR2     
                          ABX-KYC-CITY ABX-KYC-STATE ABX-KYC-ZIP      
                          ABX-KYC-IDTYPE ABX-KYC-ISSAUTH  
                          ABX-KYC-NUMBER.
