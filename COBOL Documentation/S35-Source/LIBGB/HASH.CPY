      * COPYMEMBER: LIBGB/HASH

      ************************************************************
      *         HASH C ROUTINES
      *
      *   NAME: HASH
      *   DESC: THIS ROUTINE ALLOWS FOR HASH CALL TO C
      *         OPTIONS ARE IDENTIFIED BY A VALUE IN
      *         'HASH-ACTION-CODE'.
      *
      *   USED: IN ALL THE QUOTE PROGRAMS TO HASH THE SSNO
      *
      *     'H' - CALCULATE HASH TOTAL FOR CHARACTER BUFFER
      *           IN:
      *             HASH-HASH-BUF  = CHAR BUF TO HASH ON
      *             HASH-HASH-SIZE = SIZE OF CHAR BUF
      *           OUT:
      *             HASH-HASH-TOTAL = RETURNED HASH TOTAL
      *   COPY: HASHW
      * REV:
      * MJD 130220 ADDED VERYANT LOGIC TO REPLACE C CALL.
      ************************************************************
       HASH-ROUTINE  SECTION.
           MOVE "11" TO HASH-STAT.
      *--->CALL CMD USING HASH-STAT HASH-ROUTINE-WORK-AREA.

           IF HASH-ACTION-CODE = "H"
              PERFORM HASH-PROCESS-HASH
           END-IF.

       HASH-PROCESS-HASH SECTION.
           MOVE ZERO TO HASH-WK HASH-WK1 HASH-WK2.
           PERFORM VARYING HASH-INCR FROM 1 BY 1
                     UNTIL HASH-INCR > HASH-SIZE
              MOVE HASH-BUF(HASH-INCR:1) TO D2B-CHAR
      *MJD    CALL "DECTOBIN" USING D2B-CHAR D2B-BIN
      *MJD                          D2B-LB D2B-RB
              PERFORM HASH-DECTOBIN
      *       COMPUTE HASH-WK = ( D2B-LB * 10 ) + D2B-RB
              COMPUTE HASH-WK = ( D2B-RB * 10 ) + D2B-LB
              COMPUTE HASH-WK1 = HASH-WK1 + HASH-WK
              COMPUTE HASH-WK2 = HASH-WK2 + (HASH-WK * HASH-INCR)
           END-PERFORM.

      *    COMPUTE HASH-TOTAL = ( HASH-WK1 * 1000000 ) + HASH-WK2.
           COMPUTE H-TOTAL = ( HASH-WK1 * 1000000 ) + HASH-WK2.
           MOVE FUNCTION ABS(H-TOTAL) TO HASH-TOTAL.

       HASH-DECTOBIN SECTION.
           MOVE D2B-CHAR TO HLK-CHAR.
           MOVE SPACES TO HLK-BIN
           MOVE ZEROES TO HLK-LB HLK-RB

           MOVE FUNCTION ORD(HLK-CHAR) TO HASH-NUM-WK
           ADD -1 TO HASH-NUM-WK
           MOVE 1 TO D2B-BIN-IDX
           PERFORM VARYING D2B-IDX FROM 7 BY -1 UNTIL D2B-IDX < 0
              COMPUTE D2B-DIV-VAL-P2 = 2 ** D2B-IDX
              DIVIDE HASH-NUM-WK BY D2B-DIV-VAL-P2 GIVING D2B-DIV-RESULT
              IF D2B-DIV-RESULT = 1
                 COMPUTE HASH-NUM-WK = HASH-NUM-WK - D2B-DIV-VAL-P2
              END-IF
              MOVE D2B-DIV-RESULT TO HLK-BIN(D2B-BIN-IDX:1)
              ADD 1 TO D2B-BIN-IDX
           END-PERFORM

           IF HLK-BIN(1:1) = "1" COMPUTE HLK-LB = HLK-LB + 8 END-IF
           IF HLK-BIN(2:1) = "1" COMPUTE HLK-LB = HLK-LB + 4 END-IF
           IF HLK-BIN(3:1) = "1" COMPUTE HLK-LB = HLK-LB + 2 END-IF
           IF HLK-BIN(4:1) = "1" COMPUTE HLK-LB = HLK-LB + 1 END-IF
           IF HLK-BIN(5:1) = "1" COMPUTE HLK-RB = HLK-RB + 8 END-IF
           IF HLK-BIN(6:1) = "1" COMPUTE HLK-RB = HLK-RB + 4 END-IF
           IF HLK-BIN(7:1) = "1" COMPUTE HLK-RB = HLK-RB + 2 END-IF
           IF HLK-BIN(8:1) = "1" COMPUTE HLK-RB = HLK-RB + 1 END-IF

           MOVE HLK-BIN TO D2B-BIN
           MOVE HLK-RB TO D2B-RB
           MOVE HLK-LB TO D2B-LB
           EXIT SECTION
           .

