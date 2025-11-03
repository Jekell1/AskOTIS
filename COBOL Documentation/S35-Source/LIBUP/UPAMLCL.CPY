      * COPYMEMBER: LIBLP/LPAMLCL

      ********************************************************
      *    LPAMLCL    MAIL LIST FILE CLEAR ROUTINE
      *
      ********************************************************
       CLEAR-AML-FILE SECTION.
           MOVE ALL X"09" TO AML-REC.


           MOVE 0 TO AML-SSNO AML-REFDATE AML-OWNBR AML-LTOUCH-DATE
                     AML-LAST-FORM-NO AML-LAST-FORM-DATE.

           MOVE SPACES TO AML-LNAME AML-FNAME AML-ADR1 AML-ADR2 AML-CITY
                          AML-STATE AML-ZIP AML-SRCD AML-SOLICIT
                          AML-CONFIDENTIAL.
