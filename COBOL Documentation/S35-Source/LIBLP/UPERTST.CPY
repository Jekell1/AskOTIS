      * COPYMEMBER: LIBLP/UPERTST
      *****************************************************************
      *         TEST INPUT OF UNITPER-CODE
      *
      *   NAME: UPERTST
      *   DESC: THIS ROUTINE TEST THE VALIDITY OF UNIT PERIOD CODES
      *
      *   IN  : UPERTST-UNITPER-CD
      *
      *   OUT : UPERTST-ERRCD + SEND MESSAGES
      *
      *   COPY: UPERTSTW
      * REV :
      *****************************************************************
       UPERTST-UNITPER-CODE SECTION.
           MOVE " " TO UPERTST-ERRCD.

           IF BR-UNITPER-FG NOT = "Y"
              IF UPERTST-UNITPER-CD NOT = "M"
                 MOVE "M-MONTHLY AVAILABLE ONLY!" TO MESS
                 PERFORM SEND-MESS
                 MOVE "E" TO UPERTST-ERRCD
                 GO TO UPERTST-UNITPER-CD-EXIT
              ELSE
                 NEXT SENTENCE
           ELSE
              IF BR-UNITPER-Y-D-FG NOT = "Y"
                 IF NOT (UPERTST-UNITPER-CD = "M" OR "W" OR "S" OR "B")
                    MOVE
            "ENTER: M-MONTHLY, W-WEEKLY, B-BI WEEKLY OR S-SEMI MONTHLY"
                                                          TO MESS
                    PERFORM SEND-MESS
                    MOVE "E" TO UPERTST-ERRCD
                    GO TO UPERTST-UNITPER-CD-EXIT
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF NOT
                       (UPERTST-UNITPER-CD = "M" OR "W" OR "S"
                                                      OR "Y" OR "D")
                    MOVE UPERTST-FULL-MESS TO MESS
                    MOVE "E" TO UPERTST-ERRCD.


       UPERTST-UNITPER-CD-EXIT.
           EXIT.


      *****************************************************************
      *         TEST INPUT OF UNITPER-FREQUENCY
      *
      *   NAME: UPERTST
      *   DESC: THIS ROUTINE TEST THE VALIDITY OF UNIT PERIOD
      *              FREQUENCY OF PAYMENT
      *
      *   IN  : UPERTST-UNITPER-FREQ
      *         UPERTST-UNITPER-CD
      *
      *   OUT : UPERTST-ERRCD + SEND MESSAGES
      *
      *   COPY: UPERTSTW
      * REV :
      *****************************************************************
       UPERTST-UNITPER-FREQUENCY SECTION.
           MOVE " " TO UPERTST-ERRCD.

           IF UPERTST-UNITPER-FREQ = 0
              MOVE "E" TO UPERTST-ERRCD
              GO TO UPERTST-UNITPER-FREQ-EXIT.
           IF UPERTST-UNITPER-FREQ > 1
              IF BR-UNITPER-FG NOT = "Y"
                 MOVE "ENTER '1' ONLY" TO MESS
                 PERFORM SEND-MESS
                 MOVE "E" TO UPERTST-ERRCD
                 GO TO UPERTST-UNITPER-FREQ-EXIT
              ELSE
                 IF BR-UNITPER-Y-D-FG NOT = "Y"
                    MOVE "ENTER '1' ONLY" TO MESS
                    PERFORM SEND-MESS
                    MOVE "E" TO UPERTST-ERRCD
                    GO TO UPERTST-UNITPER-FREQ-EXIT.

           IF UPERTST-UNITPER-CD = "S"
              IF UPERTST-UNITPER-FREQ = 2
                 MOVE "USE A MONTHLY PAY PERIOD CODE INSTEAD!" TO MESS
                 PERFORM SEND-MESS
                 MOVE "E" TO UPERTST-ERRCD.

           IF UPERTST-UNITPER-CD = "B"
              IF UPERTST-UNITPER-FREQ = 2
                 MOVE "USE A BI-WEEKLY PAY PERIOD CODE INSTEAD!"
                                                               TO MESS
                 PERFORM SEND-MESS
                 MOVE "E" TO UPERTST-ERRCD.

       UPERTST-UNITPER-FREQ-EXIT.
           EXIT.

      ******************************************************************
      *         ESTABLISH MESSAGE FOR PAYS EVERY 'X'  ('Y', MONTHS, ETC)
      *
      *   NAME: UPERTST
      *   DESC: THIS ROUTINE ESTABLISHES A MESSAGE FOR PRINT OR DISPLAY
      *              IDENTIFING THE CYCLE OF PAYMENTS
      *
      *   IN  : UPERTST-UNITPER-FREQ
      *         UPERTST-UNITPER-CD
      *
      *   OUT : UPERTST-MESS
      *
      *   COPY: UPERTSTW
      * REV :
      *****************************************************************
       UPERTST-SET-MESS SECTION.
           MOVE " " TO UPERTST-MESSX.
           IF UPERTST-UNITPER-FREQ < 2
              IF UPERTST-UNITPER-CD = "M"
                 MOVE "MONTH" TO UPERTST-MESSX
              ELSE
              IF UPERTST-UNITPER-CD = "W"
                 MOVE "WEEK" TO UPERTST-MESSX
              ELSE
              IF UPERTST-UNITPER-CD = "S"
                 MOVE "SEMI MONTH" TO UPERTST-MESSX
              ELSE
              IF UPERTST-UNITPER-CD = "B"
                 MOVE "BI WEEK" TO UPERTST-MESSX
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE UPERTST-UNITPER-FREQ TO UPERTST-MESS-FREQ
              IF UPERTST-UNITPER-CD = "M"
                 MOVE "MONTHS" TO UPERTST-MESS-CODE
              ELSE
              IF UPERTST-UNITPER-CD = "W"
                 MOVE "WEEKS" TO UPERTST-MESS-CODE
              ELSE
              IF UPERTST-UNITPER-CD = "B"
                 MOVE "BI WEEKS" TO UPERTST-MESS-CODE
              ELSE
              IF UPERTST-UNITPER-CD = "S"
                 MOVE "SEMI MONTHS" TO UPERTST-MESS-CODE.

       UPERTST-SET-MESS-EXIT.
           EXIT.
