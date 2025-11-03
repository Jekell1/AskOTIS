       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULSIL.
      ******************************************************************
      * VERYANT - FEB/2013
      *
      * THIS ROUTINE WAS CONVERTED FROM C ROUTINE MULSIML.C
      * USED IN CALCZL
      *
      * REV:
      * MJD 130410 COPIED IN EDWINS LATEST REVISION.
      * JKC 220526 CHANGED 'USAGE FLOAT' TO BE 'USAGE DOUBLE'
      *            TO MATCH LP/MULSIM.  IN TESTING C# TO COBOL
      *            (A32) THE NUMBERS WERE NOT MATCHING. BY MAKING
      *            THIS CHANGE IT NOW MATCHES.
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/MULSIL S35 08/16/24-09:24:52 >".
       01  MESS PIC X(50).
       01 DEC-VARS.
          05 DEC-FILLER.
             10 FILLER     USAGE DOUBLE VALUE 0.01.
             10 FILLER     USAGE DOUBLE VALUE 0.01.
             10 FILLER     USAGE DOUBLE VALUE 0.00.
             10 FILLER     USAGE DOUBLE VALUE 1.00.
             10 FILLER     USAGE DOUBLE VALUE 0.01.
             10 FILLER     USAGE DOUBLE VALUE 0.01.
             10 FILLER     USAGE DOUBLE VALUE 1.00.
             10 FILLER     USAGE DOUBLE VALUE 0.0001.
             10 FILLER     USAGE DOUBLE VALUE 0.01.
          05 DEC-TABLE     REDEFINES DEC-FILLER.
             10 DEC        USAGE DOUBLE OCCURS 9 TIMES.
       01 WS-VARS.
          05 I-T           USAGE DOUBLE.
          05 I-C           USAGE DOUBLE.
          05 I-B           USAGE DOUBLE.
          05 I-P           USAGE DOUBLE.
          05 I-REGP        USAGE DOUBLE.
          05 I-NOR         USAGE DOUBLE.
          05 I-R           USAGE DOUBLE OCCURS 5 TIMES.
          05 I-BRK         USAGE DOUBLE OCCURS 5 TIMES.
          05 I-RATEIDX     PIC 9(2).
          05 I-RTIMESBRK   USAGE DOUBLE OCCURS 5 TIMES.
          05 P1            USAGE DOUBLE.
          05 P2            USAGE DOUBLE.
          05 B1            USAGE DOUBLE.
          05 B2            USAGE DOUBLE.
          05 AREG          USAGE DOUBLE.
          05 BREG          USAGE DOUBLE.
          05 TREG          PIC S9(9).

       LINKAGE SECTION.
       COPY "LIBLP/MULSIML.CPY".

       PROCEDURE DIVISION
                 USING MIR-STAT MIR-MANY-INTEREST-RATES-SIMPLE.
       MAIN.
      D    STOP MESS.
           PERFORM GET-ARGS
           PERFORM PROCESS-MULSIML
           GOBACK
           .

       GET-ARGS.
           INITIALIZE WS-VARS
           MOVE MIR-TERM           TO I-T
           COMPUTE I-C = MIR-INT-CHARGEABLE * DEC(5)
           MOVE    I-C TO I-B
           COMPUTE I-P = MIR-REG-PAYMENT * DEC(6)
           MOVE    I-P TO I-REGP
           MOVE MIR-NO-RATES       TO I-NOR
      *    COMPUTE I-NOR = MIR-NO-RATES + 1
           PERFORM VARYING I-RATEIDX FROM 1 BY 1
                     UNTIL I-RATEIDX > I-NOR
              COMPUTE I-R(I-RATEIDX) =
                      (MIR-RATE(I-RATEIDX) * DEC(8)) / 1200
              COMPUTE I-BRK(I-RATEIDX) =
                      MIR-BREAK(I-RATEIDX) * DEC(9)
           END-PERFORM
           ADD -1 TO I-RATEIDX
           MOVE 9999999999.0 TO I-BRK(I-RATEIDX)
           ADD -1 TO I-RATEIDX
           PERFORM UNTIL I-RATEIDX = 0
              IF I-RATEIDX > 1
                 COMPUTE I-BRK(I-RATEIDX) = I-BRK(I-RATEIDX) -
                                            I-BRK(I-RATEIDX - 1)
              END-IF
              COMPUTE I-RTIMESBRK(I-RATEIDX) = I-R(I-RATEIDX) *
                                               I-BRK(I-RATEIDX)
              ADD -1 TO I-RATEIDX
           END-PERFORM
           .

       PROCESS-MULSIML.
           IF I-P NOT = ZERO
              PERFORM FIND-BALANCEL
           ELSE
              PERFORM FIND-PAYMENTL
           .

       FIND-BALANCEL.
           COMPUTE P2 = I-P * I-T
           COMPUTE P1 = P2 * .5

           MOVE P1 TO I-B
           PERFORM FINAL-BALANCEL
           MOVE BREG TO B1

           MOVE P2 TO I-B
           PERFORM FINAL-BALANCEL
           MOVE BREG TO B2

           PERFORM UNTIL (FUNCTION ABS(B2) < 0.01)
             COMPUTE I-B = P2 - (B2 * (P2 - P1) / (B2 - B1))
             MOVE P2 TO P1
             MOVE B2 TO B1
             MOVE I-B TO P2
             PERFORM FINAL-BALANCEL
             MOVE BREG TO B2
           END-PERFORM
           IF FUNCTION INTEGER(I-B * 100) < (I-B * 100)
              COMPUTE I-B = (1 + FUNCTION INTEGER(I-B * 100)) * 0.01
           ELSE
              COMPUTE I-B = (FUNCTION INTEGER(I-B * 100)) * 0.01
           END-IF
           MOVE MIR-REG-PAYMENT TO MIR-PAYMENT
           COMPUTE MIR-BALANCE = I-B / DEC(1)
           .

       FIND-PAYMENTL.
           COMPUTE P1 = I-C / I-T
           COMPUTE P2 = P1 + P1
           MOVE P1 TO I-P
           PERFORM FINAL-BALANCEL
           MOVE BREG TO B1
           MOVE P2 TO I-P
           PERFORM FINAL-BALANCEL
           MOVE BREG TO B2
           PERFORM UNTIL (FUNCTION ABS(B2) < 0.01)
             COMPUTE I-P = P2 - (B2 * (P2 - P1) / (B2 - B1))
             MOVE P2 TO P1
             MOVE B2 TO B1
             MOVE I-P TO P2
             PERFORM FINAL-BALANCEL
             MOVE BREG TO B2
           END-PERFORM
           COMPUTE I-P = (FUNCTION INTEGER(I-P * 100)) * 0.01
           MOVE MIR-INT-CHARGEABLE TO MIR-BALANCE
           COMPUTE MIR-PAYMENT = I-P / DEC(2)
           .

       FINAL-BALANCEL.
           MOVE I-T TO TREG
           MOVE I-B TO AREG BREG
           PERFORM UNTIL TREG = ZERO
             MOVE 1 TO I-RATEIDX
             MOVE BREG TO AREG
             PERFORM UNTIL NOT (AREG > I-BRK(I-RATEIDX))
               COMPUTE BREG = BREG + I-RTIMESBRK(I-RATEIDX)
               COMPUTE AREG = AREG - I-BRK(I-RATEIDX)
               ADD 1 TO I-RATEIDX
             END-PERFORM
             COMPUTE AREG = AREG * I-R(I-RATEIDX)
             COMPUTE BREG = BREG + AREG - I-P
             ADD -1 TO TREG
           END-PERFORM
           .

