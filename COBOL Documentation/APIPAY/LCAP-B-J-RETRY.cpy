       LCAP-B-J-RETRY.
           IF (LCAP-TRAMT + LCAP-LPAPLC + LCAP-LPAPINT)
                                                > LCAP-AMTNEEDED
              PERFORM LCAP-UPD-LCPD
              GO TO LCAP-B-J-PMT.

      * TEST TO SEE IF THE SHORTAGE IS NOT GREATER
      * THAN THE DELINQUINCY FACTOR, SETUP IN STATE PARAMETERS.
           COMPUTE LCAP-WRK2 =
                   LCAP-AMTNEEDED - LCAP-TRAMT
                                  - LCAP-LCPAID
                                  - LCAP-LPAPLC
                                  - LCAP-LPAPINT.

           IF LCAP-WRK2 < LCAP-DELFAC
              PERFORM LCAP-UPD-LCPD.

