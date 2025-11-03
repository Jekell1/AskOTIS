       LCAP-P-ADVANCE.
      *UPDATE LATE CHARGE PAID THRU
           PERFORM LCAP-UPD-LCPD.
           IF LN-PAY-SCHLD-FG = "Y"
              ADD 1 TO LCAP-PMT-NO
              MOVE SCHD-AMT(LCAP-PMT-NO) TO LCAP-AMTNEEDED
                                            LCHG-CURPAY
           ELSE
              MOVE LN-REGPYAMT TO LCAP-AMTNEEDED
                                  LCHG-CURPAY.
           GO TO LCAP-P-BEGIN.

      *******************************************************
      * NO ASSESSMENT IS REQUIRED; ADVANCE LCAP-LCPDTH-DATE
      * TO WHERE PAYMENTS TODATE ARE PAID THRU, ONLY
      * IF IT WILL ADVANCE LCPDTH, SINCE LCPDTH COULD
      * HAVE ADVANCED BEYOND PAID THRU VIA TIME.
      *******************************************************
