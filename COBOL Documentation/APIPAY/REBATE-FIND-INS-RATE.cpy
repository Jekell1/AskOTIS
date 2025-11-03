      ***********************************************************
      *    FIND INSURANCE RATE FROM SPR RATE TABLE
      *              USING INTERPOLATION / OR NEXT HIGH
      *                       (A&H)
      *
      *    IN  - REB-INSSUB       = SPR INSURANCE LEVEL
      *    OUT - REB-HUNDREDS
      *                AND        = RATE FOR REMAINING INS. TERM
      *          REB-MANYDECIMALS
      * REV:
      *  JTG 113094 CHANGED TO PASS RATE BACK WITH 4 DECIMALS
      *             SP-RBFRMLA = 'R' FOR TIME LOAN, CA AH REFUND
      *  JTG 070595 CHANGED TO USE INTERPOLATION OR NEXT HIGH
      *             RE: SPR
      *  JTG 071295 CHANGED TO HANDLE 7 OTHER INSURANCES
      ***********************************************************
       REBATE-FIND-INS-RATE SECTION.
           MOVE 0 TO REB-HUNDREDS.
           SET ISTP TO 1.

           IF SP-INS-RATE-FILE(REB-INSSUB)
              GO TO REBATE-FIND-INS-TABLE.

           IF NOT SP-TABLE-ROUND-UP(REB-INSSUB)
              GO TO REBATE-FIND-INS-RATE-INTERPO.
      *--------------------------------------
      *    FIND INSURANCE RE: NEXT HIGH
      *--------------------------------------
           SEARCH SP-INSRATETBL
                  END GO TO REBATE-FIND-INS-RATE-EXIT
             WHEN SP-INSTERM (REB-INSSUB,ISTP) NOT < REB-REMTERM
                  NEXT SENTENCE.

             MOVE SP-INSRATE(REB-INSSUB ISTP) TO REB-HUNDREDS
                                                 REB-MANYDECIMALS.
             GO TO REBATE-FIND-INS-RATE-EXIT.

      *--------------------------------------
      *    FIND INSURANCE RE: INTERPOLATION
      *--------------------------------------
       REBATE-FIND-INS-RATE-INTERPO.
           SEARCH SP-INSRATETBL
                 END GO TO REBATE-FIND-INS-RATE-EXIT
              WHEN SP-INSTERM(REB-INSSUB ISTP) = REB-REMTERM
                 MOVE SP-INSRATE(REB-INSSUB ISTP) TO REB-HUNDREDS
                                                     REB-MANYDECIMALS
              WHEN SP-INSTERM(REB-INSSUB ISTP) > REB-REMTERM
                 PERFORM REBATE-FIND-INS-RATE-COMPUTE.
           GO TO REBATE-FIND-INS-RATE-EXIT.

       REBATE-FIND-INS-TABLE.

           MOVE REB-REMTERM TO IRREBT-REMTERM.
           MOVE REB-INSSUB TO IRREBT-INSSUB.
           MOVE 0 TO IRREBT-MANYDECIMALS.
           MOVE "CL/IRREBT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH REB-COMMON LN-REC SP-REC.
           CANCEL FORM-PROGX.

           MOVE IRREBT-MANYDECIMALS TO REB-MANYDECIMALS REB-HUNDREDS.
           GO TO REBATE-FIND-INS-RATE-EXIT.

       REBATE-FIND-INS-RATE-COMPUTE.
           IF ISTP = 1
              COMPUTE REB-MANYDECIMALS ROUNDED =
                 SP-INSRATE(REB-INSSUB 1) / SP-INSTERM(REB-INSSUB 1)
                                                      * REB-REMTERM
           ELSE
              COMPUTE REB-MANYDECIMALS ROUNDED =
                 (SP-INSRATE(REB-INSSUB ISTP) -
                                  SP-INSRATE(REB-INSSUB ISTP - 1)) /
                 (SP-INSTERM(REB-INSSUB ISTP) -
                                  SP-INSTERM(REB-INSSUB ISTP - 1)) *
                 (REB-REMTERM - SP-INSTERM(REB-INSSUB ISTP - 1))
                                + SP-INSRATE(REB-INSSUB ISTP - 1).
           MOVE REB-MANYDECIMALS TO REB-HUNDREDS.

       REBATE-FIND-INS-RATE-EXIT.
           EXIT.

