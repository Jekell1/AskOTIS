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

