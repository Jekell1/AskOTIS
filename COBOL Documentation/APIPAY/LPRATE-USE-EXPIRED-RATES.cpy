      ************************************************
      *    USE EXPIRED RATES
      ************************************************
       LPRATE-USE-EXPIRED-RATES SECTION.
           PERFORM LPRATE-SET-INTPDTH-DATE.
           IF IBPC-EXPIRED-FLAG = " "
              MOVE 0 TO RATE-EXP-INT
              PERFORM LPRATE-PREPARE-PMT-2
           ELSE
           IF IBPC-EXPIRED-FLAG = "E" AND IBPC-FG = "P"
              MOVE XDTE-DATE TO INDU-DATE-1
              MOVE 0 TO RATE-EXP-INT
              COMPUTE INDU-CURBAL = LN-CURBAL - LN-OT2BAL
              MOVE RATE-LPTRCD TO INDU-LPTRCD
              PERFORM LPRATE-PREPARE-PMT-2
           ELSE
           IF LN-STATUSFG = "J" AND IBPC-EXPIRED-FLAG = "E"
              MOVE INDU-DATE-1 TO NUM-DATE
              MOVE LN-JDDATE   TO SYS-DATE
              IF SYS-DATE > NUM-DATE
                 MOVE LN-JDDATE TO INDU-DATE-1
              END-IF
              PERFORM LPRATE-JUDGEMENT
              PERFORM LPRATE-CALC-IT
           ELSE
           IF LN-STATUSFG = "A" AND IBPC-EXPIRED-FLAG = "E"
              MOVE INDU-DATE-1  TO NUM-DATE
              MOVE LN-ACCELDATE TO SYS-DATE
              IF SYS-DATE > NUM-DATE
                 MOVE LN-ACCELDATE TO INDU-DATE-1
              END-IF
              PERFORM LPRATE-ACCELERATION
              PERFORM LPRATE-CALC-IT
           ELSE
           IF LN-STATUSFG = "R" AND IBPC-EXPIRED-FLAG = "E"
              MOVE INDU-DATE-1 TO NUM-DATE
              MOVE LN-REDUDATE TO SYS-DATE
              IF SYS-DATE > NUM-DATE
                 MOVE LN-REDUDATE TO INDU-DATE-1
              END-IF
              MOVE LN-REDURATE TO INDU-RATE
              PERFORM LPRATE-CALC-IT
           ELSE
           IF LN-STATUSFG = " " AND IBPC-EXPIRED-FLAG = "E"
              MOVE LN-OVRDRATE TO INDU-RATE
              PERFORM LPRATE-CALC-IT.


      ************************************************
      *    CALC IT ROUTINE
      ************************************************
