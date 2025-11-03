      * COPYMEMBER: LIBUP/UPACHCL
      ******************************************************************
       CLEAR-ACH-FILE SECTION.
      ******************************************************************
           MOVE ALL X"09" TO ACH-REC.

           MOVE 0 TO ACH-BRNO ACH-DATE ACH-TIME ACH-SEQ ACH-LTOUCH-DATE
                     ACH-ACCTNO ACH-SSNO ACH-TYPE.

           MOVE SPACES TO ACH-CODE ACH-USERID ACH-DESCRIPT ACH-ORG-CHAR
                          ACH-NEW-CHAR.
