      *********************************************************
      * ADVANCE LCAP-LCPDTH-DATE BY ONE PAYMENT FREQUENCY:
      *********************************************************
       LCAP-UPD-LCPD SECTION.
           MOVE LCAP-LCPDTH-DATE TO NDTE-DATE NUM-DATE.

      * CHANGED TO CORRECT CYCLE OF DUE ON 30 RE: FEB
           IF LN-UNITPER-CD = "M"
              MOVE LN-1STPYDA TO NDTE-DD.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.

      * NOTE: WHEN SEMI MONTHLY WITH FIRST PAYDATE OF 14TH OR 15TH,
      *       THE INCREMENT OF LCPDTH WILL GO TO 02/28/YY OR 02/29/YY
      *       WHEN LCPDTH WAS INCREMENTED FROM 02/14/YY OR 02/15/YY.
      *       THE FOLLOWING LOGIC RE-ESTABLISHES CYCLES BACK TO
      *       THE 14TH OR 15TH.
      *12/06/05 ADDED TESTS ON DUE DATE OF 29 OR 30 OR 31. WAS NOT SETTING
      *         LCPDTH CORRECT IN THE FOLLOWING EXAMPLE:
      *         1STPYDA = 30, LCPDTH 2/28, PDTHRU 2/15, CYCLE DATES 15/30
      *         POSTING A 'PY' ADVANCED 1 UNITPER TO 3/13 SHOULD BE 3/15
      *         TO CORRECT: NUM-DATE HOLDS LCPDTH PRIOR TO INCREMENT UNITPER
      *         NDTE-DATE IS LCPDTH AFTER INCREMENT. NUM-DATE IS 2/28, NDTE
      *         DATE IS 3/13... GETS # OF DAYS BETWEEN THE 28TH OR 29 & 1ST
      *         PYDA & ADDS THEM TO THE CALCULATED LCPDTH(3/13 + 2) TO RESET
      *         CYCLE.
           IF LN-UNITPER-CD = "S"
              IF (LN-1STPYDA = 14 OR 15)
                 IF (NUM-DA = 28 OR 29) AND NDTE-MM = 03
                    MOVE LN-1STPYDA TO NDTE-DD
                 END-IF
              ELSE
              IF (LN-1STPYDA = 29 OR 30 OR 31)
                 IF (NUM-DA = 28 OR 29) AND NDTE-MM = 03
                    SUBTRACT NUM-DA FROM LN-1STPYDA GIVING NDTE-HOLD
                    PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO LCAP-LCPDTH-DATE.


      ****************************************************
      * ASSURE LATE CHARGE PAID THRU HAS VALID DAY:
      ****************************************************
