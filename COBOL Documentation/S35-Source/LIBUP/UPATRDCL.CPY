      * COPYMEMBER: LIBUP/UPATRDCL

      ***********************************************************************
      *   ASCII DEPOSIT TRANSACTION FILE CLEAR ROUTINE
      ***********************************************************************
       CLEAR-ATRD-FILE SECTION.
           MOVE ALL X"09" TO ATRD-REC.

           MOVE 0 TO ATD-DATE ATD-BRNO
                     ATD-DEPOSIT ATD-BANK ATD-CASH
                     ATD-GLOVERSHORT ATD-OVERSHORT
                     ATD-POSTTIME ATD-POSTDATE
                     ATD-CHECK-DEPOSIT ATD-LTOUCH-DATE.

           MOVE SPACES TO  ATD-GLFG ATD-USERID.
