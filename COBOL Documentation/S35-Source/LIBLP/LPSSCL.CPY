      * COPYMEMBER: LIBLP/LPSSCL
      **********************************************************
      *         CLEAR CREDIT SCORE STATISTICS FILE
      * REV:
      **********************************************************
       CLEAR-SS-FILE SECTION.
           MOVE SPACES TO SS-REC.
           PERFORM VARYING
               SSSUB3 FROM 1 BY 1 UNTIL SSSUB3 > 5
                  AFTER SSSUB2 FROM 1 BY 1 UNTIL SSSUB2 > 2
                    AFTER SSSUB1 FROM 1 BY 1 UNTIL SSSUB1 > 8
                       MOVE 0 TO SS-UNITS(SSSUB1, SSSUB2, SSSUB3)
                       MOVE 0 TO SS-OUTBAL(SSSUB1, SSSUB2, SSSUB3)
                       IF SSSUB3 < 3
                          MOVE 0 TO SS-NO-PL(SSSUB1, SSSUB2, SSSUB3)
                          MOVE 0 TO SS-AMT-PL(SSSUB1, SSSUB2, SSSUB3)
                       END-IF
           END-PERFORM.
