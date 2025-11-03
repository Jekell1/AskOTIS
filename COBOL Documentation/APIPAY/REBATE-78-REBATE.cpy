      ******************************************
      * UNIT CHARGE * 78'S FOR REMAINING TERM
      ******************************************
       REBATE-78-REBATE SECTION.
           IF SP-RBFRMLA(REB-SUB) = "N"
                         AND (REB-REMTERM > REB-HOLD-ORGTERM)
              COMPUTE REB-SUMREM =
                  (REB-HOLD-ORGTERM * (REB-HOLD-ORGTERM + 1)) / 2
              COMPUTE REB-SUMREM = REB-SUMREM +
                  (REB-REMTERM - REB-HOLD-ORGTERM) * REB-HOLD-ORGTERM
           ELSE
              COMPUTE REB-SUMREM =
                  (REB-REMTERM * (REB-REMTERM + 1)) / 2.
           COMPUTE REB-REBATE ROUNDED = REB-SUMORG * REB-SUMREM.

      *****************************************************************
      *                  ESTABLISH REB-SUB2
      * DETERMINE WHICH ADD DAYS, START DATE, YRTYPE APPLIES:
      *
      * NORMAL:
      *   WHEN REBATE IS DONE BEFORE TRIGGER DATE, REBSUB2 = (1)
      *   AND ON OR AFTER TRIGGER DATE, REBSUB2 = (2):
      * MISSISSIPPI OPT1 (4):
      * INDIANA:
      *   WHEN REBATE IS DONE ON OR BEFORE TRIGGER DATE, REBSUB2  = (1)
      *   AND AFTER TRIGGER DATE, REBSUB2 = (2):
      *
      *   NOTE:
      *        IF SP-RBEARLY() NOT = 8
      *           TRIGGER-DATE = 1STPAYDATE
      *        ELSE
      *           TRIGGER-DATE = 1 MONTH AFTER INTDATE
      *****************************************************************
