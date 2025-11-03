      * COPYMEMBER: LIBGB/TSTCANID
      *******************************************************
      *         TEST VALID CAN LOAN GROUP ID
      *
      *   NAME: TSTCAN
      *   DESC: THIS ROUTINE CHECKS THE CONTENTS OF TCAN-WORD
      *         FOR FORMAT AND TYPE
      *
      *   IN  : TCAN-WORD
      *         TCAN-SKIP-CD-VERIFY
      *            Y   MEANS SKIP THE READ OF THE CD FILE FOR VERIFICATION
      *                  (USED FOR CODE CREATE IN MAINT)
      *   OUT : TCAN-WORD
      *         TCAN-ERR:
      *            0   MEANS ALL OK
      *            1   ERROR GROUP ID IS INVALID
      *         TCAN-TYPE:
      *            "A" FOR ALPHA ENTERED
      *            "N" FOR NUMERIC ENTERED
      *         TCAN-SRESULT:
      *            TELLS NUMBER IF NUMERIC
      *
      *    NUMERICS ARE RIGHT JUSTIFIED WITH LEADING ZEROS.
      *    EX.      IN:  "1   "       OUT: "0001"
      *                  " 1  "            "0001"
      *                  "0 10"            "0001" ...
      *
      *    ALPHA NUMERICS ARE LEFT JUSTIFIED WITH ALL EMBEDDED SPACES REMOVED.
      *    EX.      IN:  "AB  "       OUT: "AB  "
      *                  "A B "            "AB  "
      *                  " A B"            "AB  " ...
      *
      *   USED:
      *   COPY: TSTCANW
      *
      * REV:
      * KEC 2016.0309 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-CL???? TO BE CD-CL-????.
      * BAH 190122 SPLIT CDFILE
      * BAH 20200624 REMOVED OPEN-CD1-FILE AND CLOSE-CD1-FILE #TST001
      *
      *******************************************************
      * TEST CAN GROUP ID
      *******************************************************
       TEST-CAN-GROUP SECTION.
       TEST-CAN-GROUP-PARA.
           IF TCAN-WORD-SPACES
              MOVE 1 TO TCAN-ERR
              GO TO EXIT-TEST-CAN-GROUP.
           MOVE "A" TO TCAN-TYPE.
           MOVE 0 TO TCAN-CNT1 TCAN-CNT2 TCAN-ERR TCAN-SRESULT.
       TEST-CAN-GROUP-CHAR.
           ADD 1 TO TCAN-CNT1.
           IF TCAN-CNT1 > 4
              GO TO TEST-CAN-GROUP-NUMERIC.
           IF TCAN-CHAR(TCAN-CNT1) = " "
              GO TO TEST-CAN-GROUP-CHAR.
           IF TCAN-CHAR(TCAN-CNT1) NOT NUMERIC
              GO TO TEST-CAN-GROUP-ALPHA.
           COMPUTE TCAN-CNT2 = (TCAN-CNT2 * 10) + TCAN-DIGIT(TCAN-CNT1).
           GO TO TEST-CAN-GROUP-CHAR.
       TEST-CAN-GROUP-NUMERIC.
           MOVE TCAN-CNT2 TO TCAN-NUMB TCAN-SRESULT.
           MOVE "N" TO TCAN-TYPE.
           GO TO TEST-CAN-GROUP-VERIFY.
       TEST-CAN-GROUP-ALPHA.
           MOVE 1 TO TCAN-CNT2.
           MOVE SPACES TO TCAN-HOLD.
           PERFORM VARYING TCAN-CNT1 FROM 1 BY 1 UNTIL TCAN-CNT1 > 4
              IF TCAN-CHAR(TCAN-CNT1) NOT = " "
                 MOVE TCAN-CHAR(TCAN-CNT1) TO TCAN-HOLD-CHAR(TCAN-CNT2)
                 ADD 1 TO TCAN-CNT2
              END-IF
           END-PERFORM.
           MOVE TCAN-HOLD TO TCAN-WORD.
       TEST-CAN-GROUP-VERIFY.
           IF TCAN-SKIP-CD-VERIFY = "Y"
              GO TO EXIT-TEST-CAN-GROUP.

           MOVE "CL"      TO CD-TYPE.
           MOVE TCAN-WORD TO CD-CODE.
           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE 1 TO TCAN-ERR
              MOVE "NOT FOUND" TO CD-DESC.
       EXIT-TEST-CAN-GROUP.
           EXIT.
