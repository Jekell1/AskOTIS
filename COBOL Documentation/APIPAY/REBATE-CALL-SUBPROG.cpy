      *********************************************
      *   ROUTINE TO CALL SUBPROGRAM:
      *
      *   MOVE FIELDS TO PASS TO REB-COMMON
      *   MOVE PROGRAM NAME TO REB-FORM-PROG
      *   IF NO CANCEL OF SUBPROG IS REQUESTED
      *      MOVE " " TO REB-CANCEL-FG.
      *********************************************
       REBATE-CALL-SUBPROG SECTION.
           MOVE REB-FORM-PATH TO FORM-NAM.
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
