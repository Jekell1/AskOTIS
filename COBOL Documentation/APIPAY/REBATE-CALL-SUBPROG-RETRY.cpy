       REBATE-CALL-SUBPROG-RETRY.
           IF REB-RECAST = "Y" AND SP-RBRECAST(REB-SUB) = 1
              MOVE REB-PAYDATE TO REB-COMMON-PAYDATE
              CALL FORM-PROGX USING FORM-PATH REB-COMMON LN-REC SP-REC
                                                         LTI-REC
                  ON OVERFLOW
                  GO TO REBATE-CALL-SUBPROG-RETRY
           ELSE
              CALL FORM-PROGX USING FORM-PATH REB-COMMON LN-REC LTI-REC
                  ON OVERFLOW
                  GO TO REBATE-CALL-SUBPROG-RETRY.

           IF REB-CANCEL-FG = "Y"
              CANCEL FORM-PROGX.
           MOVE "Y" TO REB-CANCEL-FG.

      *********************************
      *   EARLY PROVISION ROUTINES
      *********************************
