      * IF SP-LCFRMLA = "A" WE MUST NOT ADVANCE LATE CHARGE PAID THRU
      * TO THE MONTH & YEAR OF THE "AL" DATE, IF THE ACCESS DATE IS
      * PRIOR TO THE NORMAL PAY DAY OF THAT MONTH.
       LCAP-A-AL-OK-TEST.
           MOVE "Y" TO LCAP-AL-OK.
           IF LCAP-LPTRCD = "AL"
              IF SP-LCFRMLA = "A"
                 MOVE LCAP-PAYDATE TO NUM-DATE
                 IF PDTH-WK-CCYY = NUM-CCYY
                    IF PDTH-WK-MM = NUM-MO
                       MOVE LCAP-1STPYDATE TO SYS-DATE
                       IF NUM-DA < S-DD
                          MOVE "N" TO LCAP-AL-OK.
