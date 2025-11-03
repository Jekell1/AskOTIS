      * COPYMEMBER: LIBGB/GRWORK
      *******************************************************
      *         CHECK GROUP MEMBER FOR INTEGRITY
      *
      *   NAME: GRWORK
      *   DESC: THIS ROUTINE CHECKS THE CONTENTS OF WGR-WORD
      *         FOR FORMAT AND TYPE
      *
      *   IN  : WGR-WORD
      *         WGR-MIN-LEVEL   - MINIMUM VALID LEVEL DESIRED
      *                           10 MEANS BRANCH NUMBER ONLY
      *   OUT : WGR-ERR:
      *            0   MEANS ALL OK
      *            1-4 POINT TO ERRORS IN WGR-MSG
      *         WGR-TYPE:
      *            "A" FOR ALPHA ENTERED
      *            "N" FOR NUMERIC ENTERED
      *         WGR-SRESULT:
      *            TELLS BRANCH NUMBER
      *            IF ENTERED LEVEL 9 GROUP OR BRANCH NUMBER
      *         WGR-LEVEL
      *            TELLS LEVEL OF ENTRY, 9 FOR BRANCH
      *
      *   USED:
      *   COPY: GRWORKW
      *
      * REV:
      *******************************************************
      * CHECK-MEMBER
      *******************************************************
       CHECK-MEMBER SECTION.
       CHECK-MEMBER-PARA.
           MOVE "A" TO WGR-TYPE.
           MOVE 0 TO WGR-CNT1 WGR-CNT2 WGR-ERR.
       CHECK-MEMBER-CHAR.
           ADD 1 TO WGR-CNT1.
           IF WGR-CNT1 > 4
              GO TO CHECK-NUMERIC-MEMBER.
           IF WGR-CHAR(WGR-CNT1) = " " AND WGR-CNT1 > 1
              GO TO CHECK-MEMBER-CHAR.
           IF WGR-CHAR(WGR-CNT1) NOT NUMERIC
              GO TO CHECK-ALPHA-MEMBER.
           COMPUTE WGR-CNT2 = (WGR-CNT2 * 10) + WGR-DIGIT(WGR-CNT1).
           GO TO CHECK-MEMBER-CHAR.
       CHECK-NUMERIC-MEMBER.
           MOVE WGR-CNT2 TO WGR-NUMB.
           MOVE "N" TO WGR-TYPE.
           PERFORM OPEN-BR-FILE.
           MOVE WGR-NUMB TO BR-NO WGR-SRESULT.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE 1 TO WGR-ERR.
           MOVE 9 TO WGR-LEVEL.
           PERFORM CLOSE-BR-FILE.
           GO TO EXIT-CHECK-MEMBER.
       CHECK-ALPHA-MEMBER.
           IF WGR-MIN-LEVEL = 10
              MOVE 0 TO WGR-LEVEL
              MOVE 2 TO WGR-ERR
              GO TO EXIT-CHECK-MEMBER.
           PERFORM OPEN-GR1-FILE.
           MOVE WGR-WORD TO GR-ID.
           PERFORM READ-GR1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WGR-LEVEL GR-LEVEL
              MOVE 3 TO WGR-ERR
           ELSE
              MOVE GR-LEVEL TO WGR-LEVEL
              IF GR-LEVEL < WGR-MIN-LEVEL
                 MOVE 4 TO WGR-ERR.
           IF GR-LEVEL = 9
              MOVE GR-MEMBER-LOC(1) TO WGR-SRESULT
           ELSE
              MOVE 0 TO WGR-SRESULT.
           PERFORM CLOSE-GR1-FILE.
       EXIT-CHECK-MEMBER.
           EXIT.

