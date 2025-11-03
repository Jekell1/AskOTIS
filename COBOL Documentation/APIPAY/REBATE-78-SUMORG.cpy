      ***************************
      * 78'S FOR ORIGINAL TERM:
      ***************************
       REBATE-78-SUMORG SECTION.
           IF (SP-RBFRMLA(REB-SUB) = "N")
               COMPUTE REB-SUMORG =
                    (REB-HOLD-ORGTERM * (REB-HOLD-ORGTERM + 1)) / 2
               COMPUTE REB-SUMORG = REB-SUMORG +
                    (REB-ORGTERM - REB-HOLD-ORGTERM) * REB-HOLD-ORGTERM
           ELSE
               COMPUTE REB-SUMORG =
                      (REB-ORGTERM * (REB-ORGTERM + 1)) / 2.

      ***************************
      * 78'S ROUTINE
      ***************************
