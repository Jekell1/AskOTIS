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

