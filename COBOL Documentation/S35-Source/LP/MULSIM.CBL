       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULSIM.
      ******************************************************
      * VERYANT - MARCH/2013
      *
      * THIS ROUTINE WAS CONVERTED FROM C ROUTINE MULSIM.C
      * USED IN CALCZL AND CALCZ3	
      *
      * REV:
      * MJD 130410 COPIED IN EDWINS LATEST REVISION.
      ******************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/MULSIM A32 05/11/20-13:56:47 >".
       01 WS-VARS.
          05 I-BALLON-FG   PIC X.
          05 I-UPPY        USAGE DOUBLE.
          05 I-T           USAGE SIGNED-INT.
          05 I-C           USAGE DOUBLE.
          05 I-REGP        USAGE DOUBLE.
          05 I-P           USAGE DOUBLE.
          05 I-BALLONP     USAGE DOUBLE.
          05 I-B           USAGE DOUBLE.
          05 I-N-OF-R      USAGE DOUBLE.
          05 I-CHGABLE     USAGE DOUBLE.
          05 I-R           USAGE DOUBLE OCCURS 5 TIMES.
          05 I-BRK         USAGE DOUBLE OCCURS 5 TIMES.
          05 I-RATEIDX     PIC 9(2).
          05 I-RATE-X-BRK  USAGE DOUBLE OCCURS 5 TIMES.
          05 C2            USAGE DOUBLE.
          05 C1            USAGE DOUBLE.
          05 AR-RESULT     USAGE DOUBLE.
          05 WK            USAGE DOUBLE.
          05 WK2           USAGE DOUBLE.
          05 T-WK          USAGE SIGNED-INT.
          05 C-WK          USAGE DOUBLE.
          05 C-WK2         USAGE DOUBLE.
          05 B1            USAGE DOUBLE.
          05 B2            USAGE DOUBLE.
          05 P1            USAGE DOUBLE.
          05 P2            USAGE DOUBLE.


       LINKAGE SECTION.
       COPY "LIBLP/MULSIM.CPY".

       PROCEDURE DIVISION
                 USING MIR-STAT MIR-MANY-INTEREST-RATES-SIMPLE.
       MAIN.
           PERFORM GET-ARGS
           PERFORM PROCESS-MULSIM
           GOBACK
           .

       GET-ARGS.
           INITIALIZE WS-VARS
           MOVE MIR-BALLOON-FG TO I-BALLON-FG
           COMPUTE I-UPPY = MIR-UPPY * 0.000001
           MOVE MIR-TERM TO I-T
           COMPUTE I-CHGABLE = MIR-INT-CHARGEABLE * 0.01
           MOVE I-CHGABLE TO I-C
           COMPUTE I-REGP = MIR-REGPYAMT * 0.01
           MOVE I-REGP TO I-P

           COMPUTE I-BALLONP = MIR-LASTPYAMT * 0.01
           IF I-BALLONP = 999999.99
              MOVE 0 TO I-BALLONP
           END-IF
           MOVE I-BALLONP TO I-B
           MOVE MIR-NO-RATES TO I-N-OF-R

           PERFORM VARYING I-RATEIDX FROM 1 BY 1
                     UNTIL I-RATEIDX > I-N-OF-R
              COMPUTE I-R(I-RATEIDX) =
                      MIR-RATE(I-RATEIDX) * 0.0001 / I-UPPY / 100
              COMPUTE I-BRK(I-RATEIDX) =
                      MIR-BREAK(I-RATEIDX) * 0.01
           END-PERFORM
           ADD -1 TO I-RATEIDX
           MOVE 9999999999.0 TO I-BRK(I-RATEIDX)
           ADD -1 TO I-RATEIDX
           PERFORM UNTIL I-RATEIDX = 0
              IF I-RATEIDX > 1
                 COMPUTE I-BRK(I-RATEIDX) = I-BRK(I-RATEIDX) -
                                            I-BRK(I-RATEIDX - 1)
              END-IF
              COMPUTE I-RATE-X-BRK(I-RATEIDX) = I-R(I-RATEIDX) *
                                               I-BRK(I-RATEIDX)
              ADD -1 TO I-RATEIDX
           END-PERFORM
           .

       PROCESS-MULSIM.
           IF I-C = ZERO
              PERFORM FIND-CHARGEABLE
           ELSE
              IF I-P = ZERO
                 PERFORM FIND-PAYMENT
              ELSE
                 PERFORM FIND-BALLON
              END-IF
           END-IF
           .

       FIND-CHARGEABLE.
           COMPUTE C2 = I-BALLONP + (I-P * I-T)
           COMPUTE C1 = C2 * 0.5

           MOVE C1 TO I-C
           PERFORM AMORTIZE-ROUTINE
           MOVE AR-RESULT TO WK

           MOVE C2 TO I-C
           PERFORM AMORTIZE-ROUTINE
           MOVE AR-RESULT TO WK2

           PERFORM UNTIL (FUNCTION ABS(WK2) < 0.01 )
              COMPUTE I-C = C2 - ( WK2 * (C2 - C1) / (WK2 - WK) )
              MOVE C2 TO C1
              MOVE WK2 TO WK
              MOVE I-C TO C2
              PERFORM AMORTIZE-ROUTINE
              MOVE AR-RESULT TO WK2
           END-PERFORM

           IF FUNCTION INTEGER( (I-C + .005) * 100.0 ) <
                              ( (I-C + .005) * 100.0 )
              COMPUTE I-C = 0.001 *
                      ( 1 + FUNCTION INTEGER ( (I-C + .005) * 100.0 ))
           ELSE
              COMPUTE I-C = 0.001 *
                      ( FUNCTION INTEGER ( (I-C + .005) * 100.0 ))
           END-IF

           COMPUTE MIR-BALANCE = I-C * 1000
           MOVE MIR-LASTPYAMT TO MIR-BALLOON
           MOVE MIR-REGPYAMT TO MIR-PAYMENT
           .

       AMORTIZE-ROUTINE.
           MOVE I-T TO T-WK
           MOVE I-C TO C-WK2
           IF I-BALLON-FG = "N"
              PERFORM UNTIL T-WK = 0
                ADD -1 TO T-WK
                MOVE C-WK2 TO C-WK
                PERFORM VARYING I-RATEIDX FROM 1 BY 1
                          UNTIL C-WK NOT > I-BRK(I-RATEIDX)
                   COMPUTE C-WK2 = C-WK2 + I-RATE-X-BRK(I-RATEIDX)
                   COMPUTE C-WK  = C-WK  - I-BRK(I-RATEIDX)
                END-PERFORM
                COMPUTE C-WK = C-WK * I-R(I-RATEIDX)
                COMPUTE C-WK2 = C-WK2 + C-WK
                COMPUTE C-WK2 = C-WK2 - I-P
              END-PERFORM
              COMPUTE C-WK2 = C-WK2 - I-B
           ELSE
              ADD 1 TO T-WK
              PERFORM UNTIL T-WK = 0
                ADD -1 TO T-WK
                MOVE C-WK2 TO C-WK
                PERFORM VARYING I-RATEIDX FROM 1 BY 1
                          UNTIL C-WK NOT > I-BRK(I-RATEIDX)
                   COMPUTE C-WK2 = C-WK2 + I-RATE-X-BRK(I-RATEIDX)
                   COMPUTE C-WK  = C-WK  - I-BRK(I-RATEIDX)
                END-PERFORM
                COMPUTE C-WK  = C-WK  * I-R(I-RATEIDX)
                COMPUTE C-WK2 = C-WK2 + C-WK
                IF T-WK NOT = 0
                   COMPUTE C-WK2 = C-WK2 - I-P
                ELSE
                   COMPUTE C-WK2 = C-WK2 - I-B
                END-IF
              END-PERFORM
              ADD 0 TO T-WK
           END-IF
           MOVE C-WK2 TO AR-RESULT
           .


       FIND-BALLON.
           COMPUTE B1 = I-CHGABLE - (I-P * I-T)
           COMPUTE B2 = B1 + B1

           MOVE B1 TO I-B
           PERFORM AMORTIZE-ROUTINE
           MOVE AR-RESULT TO WK

           MOVE B2 TO I-B
           PERFORM AMORTIZE-ROUTINE
           MOVE AR-RESULT TO WK2

           PERFORM UNTIL (FUNCTION ABS(WK2) < 0.01)
              COMPUTE I-B = B2 - (WK2 * (B2 - B1) / (WK2 - WK))
              MOVE B2  TO B1
              MOVE WK2 TO WK
              MOVE I-B TO B2
              PERFORM AMORTIZE-ROUTINE
              MOVE AR-RESULT TO WK2
           END-PERFORM
           COMPUTE I-B = FUNCTION INTEGER( (I-B + 0.005) * 100) * 0.001

           COMPUTE MIR-BALLOON = I-B * 1000
           MOVE MIR-INT-CHARGEABLE TO MIR-BALANCE
           MOVE MIR-REGPYAMT TO MIR-PAYMENT
           .

       FIND-PAYMENT.
           COMPUTE P1 = (I-CHGABLE - I-BALLONP) / I-T
           COMPUTE P2 = P1 + P1

           MOVE P1 TO I-P
           PERFORM AMORTIZE-ROUTINE
           MOVE AR-RESULT TO WK

           MOVE P2 TO I-P
           PERFORM AMORTIZE-ROUTINE
           MOVE AR-RESULT TO WK2

           PERFORM UNTIL ( FUNCTION ABS(WK2) < 0.01)
              COMPUTE I-P = P2 - (WK2 * (P2 - P1) / (WK2 - WK))
              MOVE P2  TO P1
              MOVE WK2 TO WK
              MOVE I-P TO P2
              PERFORM AMORTIZE-ROUTINE
              MOVE AR-RESULT TO WK2
           END-PERFORM

           COMPUTE I-P = FUNCTION INTEGER( (I-P + 0.005) * 100) * 0.001

           COMPUTE I-P =  I-P * 1000
           MOVE    I-P TO MIR-PAYMENT
           MOVE MIR-LASTPYAMT TO MIR-BALLOON
           MOVE MIR-INT-CHARGEABLE TO MIR-BALANCE
           .


