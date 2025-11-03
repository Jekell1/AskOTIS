      * A30 CONVERSION COMPLETED.
      *PROGRAM-ID. BITWISEUTL.
       PROGRAM-ID. BITWIS.
      *********************************************************
      * VERYANT - MAY/2/2013
      *
      * UTILITIES TO PERFORM BITWISE OPERATIONS ON INTEGERS
      *
      * REV:
      * MJD 130510 NEW PROGRAM FROM VERYANT (BITWISEUTL.CBL).
      *            TRUNCATED NAME FOR USE IN PROG-NAME.
      *            ADDED "LIB__/" TO COPY STATEMENTS.
      *            CHANGED ALL "BW-" TO BE "BWARGS-" TO AVOID CONFUSION
      *            WITH OUR BW-REC FIELDS
      *********************************************************

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)GB/BITWIS A30 05/14/13-16:09:15 09/18/14-13:47:34 >".
       01 IDX           PIC S9(2).
       01 BITSIDX       PIC S9(2).
       01 TWOPWIDX      USAGE UNSIGNED-LONG.
       01 BITSTMP       PIC X(32).
       01 BITSINT       PIC X(32).
       01 BITSINT1      PIC X(32).
       01 BITSINT2      PIC X(32).
       01 WK-INT        USAGE UNSIGNED-INT.
       01 WK-DIV-RESULT PIC 9(1).
       01 WK-J          PIC 9(2).
       01 WK-JLIMIT     PIC 9(2).
       01 WK-SIZE       PIC 9(2).
       01 OUT-INT       USAGE UNSIGNED-INT.

       LINKAGE SECTION.
       COPY "LIBGB/BWARGS.CPY".

       PROCEDURE DIVISION USING BWARGS-ARGS.
       MAIN.
           EVALUATE TRUE
           WHEN BWARGS-RIGHTSHIFT PERFORM DO-RIGHTSHIFT
           WHEN BWARGS-LEFTSHIFT  PERFORM DO-LEFTSHIFT
           WHEN BWARGS-AND        PERFORM DO-AND
           WHEN BWARGS-OR         PERFORM DO-OR
           WHEN BWARGS-XOR        PERFORM DO-XOR
           END-EVALUATE
           GOBACK.

       DO-RIGHTSHIFT.
           IF BWARGS-TINT
              MOVE BWARGS-OP1-INT TO WK-INT
              PERFORM GETBITSINT
              PERFORM RSHIFTINT
              PERFORM COMPUTE-OUT-INT
           END-IF
           .

       DO-LEFTSHIFT.
           IF BWARGS-TINT
              MOVE BWARGS-OP1-INT TO WK-INT
              PERFORM GETBITSINT
              PERFORM LSHIFTINT
              PERFORM COMPUTE-OUT-INT
           END-IF
           .


       DO-AND.
           IF BWARGS-TINT
              PERFORM GET2INT
              PERFORM ANDINT
              PERFORM COMPUTE-OUT-INT
           END-IF
           .

       DO-OR.
           IF BWARGS-TINT
              PERFORM GET2INT
              PERFORM ORINT
              PERFORM COMPUTE-OUT-INT
           END-IF
           .

       DO-XOR.
           IF BWARGS-TINT
              PERFORM GET2INT
              PERFORM XORINT
              PERFORM COMPUTE-OUT-INT
           END-IF
           .

       GET2INT.
              MOVE BWARGS-OP1-INT TO WK-INT
              PERFORM GETBITSINT
              MOVE BITSINT TO BITSINT1
              MOVE BWARGS-OP2-INT TO WK-INT
              PERFORM GETBITSINT
              MOVE BITSINT TO BITSINT2
              .

       GETBITSINT.
           MOVE 1 TO BITSIDX
           PERFORM VARYING IDX FROM 31 BY -1 UNTIL IDX < 0
              COMPUTE TWOPWIDX = 2 ** IDX
              DIVIDE WK-INT BY TWOPWIDX GIVING WK-DIV-RESULT
              IF WK-DIV-RESULT = 1
                 COMPUTE WK-INT = WK-INT - TWOPWIDX
              END-IF
              MOVE WK-DIV-RESULT TO BITSINT(BITSIDX:1)
              ADD 1 TO BITSIDX
           END-PERFORM
           .

       LSHIFTINT.
           COMPUTE WK-J    =  1 + BWARGS-POSITIONS
           COMPUTE WK-SIZE = 32 - BWARGS-POSITIONS
           COMPUTE WK-JLIMIT = WK-J + WK-SIZE - 1
           MOVE SPACES TO BITSTMP
           PERFORM VARYING BITSIDX FROM 1 BY 1
                   UNTIL WK-J > WK-JLIMIT
              MOVE BITSINT(WK-J:1) TO BITSTMP(BITSIDX:1)
              ADD  1 TO WK-J
           END-PERFORM
           COMPUTE WK-J    = 32 - BWARGS-POSITIONS + 1
           PERFORM VARYING BITSIDX FROM WK-J BY 1
                   UNTIL BITSIDX > 32
             MOVE "0" TO BITSTMP(BITSIDX:1)
           END-PERFORM
           MOVE BITSTMP TO BITSINT
           .

       RSHIFTINT.
           COMPUTE WK-J    =  1 + BWARGS-POSITIONS
           COMPUTE WK-SIZE = 32 - BWARGS-POSITIONS
           MOVE SPACES TO BITSTMP
           PERFORM VARYING BITSIDX FROM 1 BY 1
                   UNTIL BITSIDX > WK-SIZE
             MOVE BITSINT(BITSIDX:1) TO BITSTMP(WK-J:1)
             ADD 1 TO WK-J
           END-PERFORM
           PERFORM VARYING BITSIDX FROM 1 BY 1
                   UNTIL BITSIDX > BWARGS-POSITIONS
             MOVE "0" TO BITSTMP(BITSIDX:1)
           END-PERFORM
           MOVE BITSTMP TO BITSINT
           .

       ANDINT.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 32
             IF BITSINT1(IDX:1) = "1" AND BITSINT2(IDX:1) = "1"
                MOVE "1" TO BITSINT(IDX:1)
             ELSE
                MOVE "0" TO BITSINT(IDX:1)
             END-IF
           END-PERFORM
           .

       ORINT.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 32
             IF BITSINT1(IDX:1) = "1" OR BITSINT2(IDX:1) = "1"
                MOVE "1" TO BITSINT(IDX:1)
             ELSE
                MOVE "0" TO BITSINT(IDX:1)
             END-IF
           END-PERFORM
           .

       XORINT.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 32
             IF BITSINT1(IDX:1) = BITSINT2(IDX:1)
                MOVE "0" TO BITSINT(IDX:1)
             ELSE
                MOVE "1" TO BITSINT(IDX:1)
             END-IF
           END-PERFORM
           .

       COMPUTE-OUT-INT.
           MOVE 31 TO WK-J
           MOVE ZERO TO OUT-INT
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 32
             IF BITSINT(IDX:1) = "1"
                COMPUTE OUT-INT = OUT-INT + (2 ** WK-J)
             END-IF
             ADD -1 TO WK-J
           END-PERFORM
           MOVE OUT-INT TO BWARGS-RES-INT
           .

