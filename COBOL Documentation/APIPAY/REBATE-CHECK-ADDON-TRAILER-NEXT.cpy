       REBATE-CHECK-ADDON-TRAILER-NEXT.
           MOVE LN-ACCTNO TO LTI-ACCTNO.
           ADD 1 TO LTI-SEQNO.
           PERFORM READ-LTI1-FILE.
           IF IO-BAD
              GO TO REBATE-CHECK-ADDON-TRAILER-EXIT
           ELSE
              IF LTI-INS-TYPE = REB-TEST-LTI-TYPE
                 MOVE "Y" TO REB-LTI-ADDON-FG
              END-IF
              GO TO REBATE-CHECK-ADDON-TRAILER-NEXT
           END-IF.
