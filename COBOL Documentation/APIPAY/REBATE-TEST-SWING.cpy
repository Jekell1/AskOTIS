      ********************************************
      *    APPLICATION OF 'SWING RULE':
      *    ESTABLISH REM-TERM
      *    ADJUST TERM BY DEFERMENTS
      ********************************************
       REBATE-TEST-SWING SECTION.
           MOVE " " TO REB-SWING.
           IF (NOT
                  ( SP-RBSPOPT1(REB-SUB) = 7
                          AND ( (LN-REPOCD = "X" OR "S")
                                   OR (REB-LPTRCD = "PB" OR "RN"
                                       OR "RB" OR "RO")
                              )
                  ) OR
                  ( SP-RBSPOPT1(REB-SUB) = 18
                          AND ( (LN-REPOCD = "X" OR "S") OR
                                 (REB-LPTRCD = "PB" OR "RN" OR
                                               "RB" OR "RO") OR
                                  (REB-ELAPSED-DAYS NOT > 60)
                              )
                  )
              )
              IF (
                   (SP-RBDAYS(REB-SUB REB-SUB2) NOT = 0) AND
                                     (SP-RBEARLY(REB-SUB) NOT = 4)
                 )
               OR
                 (
                   (SP-RBEARLY(REB-SUB) = 4) AND
                         (NOT (REB-LPTRCD = "PB" OR "RN" OR "SC"
                                                OR "RB" OR "RO"))
                 )
               OR
                 (
                   (REB-EARN-FULL-MONTH = "B" OR "C")
                 )
                 MOVE "Y" TO REB-SWING
                 IF (REB-ELAPSED-REM NOT < SP-RBDAYS(REB-SUB REB-SUB2))
                           OR (REB-EARN-FULL-MONTH = "B" OR "C")
                    ADD 1 TO REB-ELAPSED-MONTHS
                    IF SP-RBEARLY(REB-SUB) NOT = 5
                       MOVE 0 TO REB-ELAPSED-REM
                    END-IF
                 ELSE
                    MOVE 0 TO REB-ELAPSED-REM.

      * SETUP REMAINING TERM:
           COMPUTE REB-REMTERM = REB-ORGTERM - REB-ELAPSED-MONTHS.

      * ADJUST TERMS BASED ON SP-RBDEFER CODE:
      * DEFERMENTS NOT CONSIDERED FOR INSURANCE:
           IF REB-SUB = REB-INTSUB
            OR (REB-SUB = REB-DEFSUB AND SP-RBFRMLA(REB-SUB) =
                1 OR "V" OR "W")
              IF SP-RBFRMLA(REB-SUB) NOT = "2"
                                  AND NOT = "C"
                                   AND NOT = "D"
                                    AND NOT = "F"
                                     AND NOT = "H"
                                      AND NOT = "I"
                                       AND NOT = "J"
                 IF SP-RBDEFER(REB-SUB) NOT = 2
                    ADD REB-LN-TOTNODEF TO REB-REMTERM
                    IF SP-RBDEFER(REB-SUB) = 0
                       ADD REB-LN-TOTNODEF TO REB-ORGTERM.

      *************************************************
      * REBATE AFTER REDUCTION AND %AGE COULD BE < 0
      *************************************************
