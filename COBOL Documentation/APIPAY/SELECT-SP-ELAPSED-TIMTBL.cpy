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
       SELECT-SP-ELAPSED-TIMTBL SECTION.
           PERFORM REBATE-TRIGGER-DATE.
           MOVE REB-PAYDATE TO NUM-DATE.
           MOVE REB-TRIGGER-DATE TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

           IF (NOT (SP-RBSPOPT1(REB-SUB) = 4)) AND
                                  (SP-RBEARLY(REB-SUB) NOT = 8)
              MOVE 1 TO REB-SUB2
           ELSE
              MOVE 0 TO REB-SUB2.

           IF JULIAN-DATE = 0
              MOVE 0 TO REB-SUB
           ELSE
              IF ELAPSED-DAYS NOT < REB-SUB2
                 MOVE 1 TO REB-SUB2
              ELSE
                 MOVE 2 TO REB-SUB2.

      * EXAMPLE FOR MS OPTION1 = 3
      * ON FIRST PAYDATE, WILL MOVE 1 TO REB-SUB2, THEN NEXT IF, ELAPSED
      * IS 0, SO IS 0 >= 1 ? NO, SO MOVE 2 TO REB-SUB2 WHICH IS NOT
      * CONSIDERED PODATE-B4-1STPYDATE AND THAT IS WHAT THEY WANT!

      * AFTER FIRST PAYDATE, WILL MOVE 1 TO REB-SUB2, THEN NEXT IF, ELAPSED
      * IS -1, SO IS -1 >= 1 ? NO, SO MOVE 2 TO REB-SUB2 WHICH IS NOT
      * CONSIDERED PODATE-B4-1STPYDATE AND THAT IS WHAT THEY WANT!

      * PRIOR FIRST PAYDATE, WILL MOVE 1 TO REB-SUB2, THEN NEXT IF, ELAPSED
      * IS 3, SO IS 3 >= 1 ? YES, SO MOVE 1 TO REB-SUB2 WHICH IS
      * CONSIDERED PODATE-B4-1STPYDATE AND THAT IS WHAT THEY WANT!

      ***************************************************
      *    DETERMINE TRIGGER DATE FOR CALC OF REB-SUB2
      *      AND IF SP-RBEARLY(8), CHANGE REB-ORGTERM
      ***************************************************
