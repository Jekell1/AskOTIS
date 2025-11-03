      **************************************************
       INTEREST-DUE-MINIMUM SECTION.
      **************************************************
      * PACESETTER FINANCE:
           IF SP-RBSPOPT1(7) = 8
              IF INDU-LPTRCD = "PB" OR "RN" OR "SC" OR "PO"
                                  OR "RB" OR "RO"
                 ADD INDU-INTEREST LN-TOTINT LN-INTBAL GIVING
                                                     INDU-AMOUNT
                 IF INDU-AMOUNT < SP-RBMINRET(7)
                    COMPUTE INDU-INTEREST = INDU-INTEREST +
                       (SP-RBMINRET(7) - INDU-AMOUNT).

      **************************************************
