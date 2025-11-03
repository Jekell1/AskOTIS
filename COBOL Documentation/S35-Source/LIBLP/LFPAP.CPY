      * COPYMEMBER: LIBLP/LFPAP
      **************************************************************************
      *         APPLY A PAYMENT TO AN ACCOUNT UNDER AMORTIZED INTEREST
      *         SP-RATETYPE (Z)
      *
      *   NAME: LFPAP
      *   DESC: THIS ROUTINE DISTRIBUTES A PAYMENT APPLIED TO
      *         AN ACCOUNT UNDER AMORTIZED INTEREST SP-CAL-RATETYPE (Z)
      *
      *   IN  :
      *         LFPAP-TODAY
      *         LFPAP-TRCD
      *         LFPAP-TRAMT            AMOUNT OF PAYMENT
      *
      *         LFPAP-INTBAL
      *         LFPAP-CURBAL
      *         LFPAP-LCBAL
      *         LFPAP-OTHBAL
      *
      *         LFPAP-EXTRA-PRIN-LFP
      *
      *         LFPAP-REMAIN-INT-SINCE-LFP
      *         LFPAP-REMAIN-PRIN-SINCE-LFP
      *
      *   IN/OUT:
      *         LFPAP-TOTPAYMNTD
      *         LFPAP-TOTEXCPAYMNTD
      *
      *   OUT : ---------------------    AMOUNTS APPLIED TO LOAN BALANCES
      *         LFPAP-APINT
      *         LFPAP-APCUR
      *         LFPAP-APLC
      *         LFPAP-APOTH
      *         -----------------------    AMOUNTS APPLIED TO PAST DUE
      *         LFPAP-APPDUEINT
      *         LFPAP-APPDUEPRIN
      *         -----------------------    AMOUNTS APPLIED TO CURRENT DUE
      *         LFPAP-APCURINT
      *         LFPAP-APCURPRIN
      *         -----------------------    AMOUNTS APPLIED TO EXTRA PRIN
      *         LFPAP-EXTRA-PRIN
      *         LFPAP-INDU-INTEREST
      *         -----------------------    TOTPAYMNTD FIELDS
      *         LFPAP-TOTPAYMNTD
      *         LFPAP-TOTEXCPAYMNTD
      *
      *   USED:
      *
      *   COPY: LFPAPW
      *
      * REV :
      *  JTG 010917 CHANGED ROUTINE THAT DETERMINES EXTRA PRININCIPAL
      *             TO NOT SET NEXT SCHEDULED PAY UNLESS BOTH LFP'S = 0
      **************************************************************************
       LFPAP-ROUTINE SECTION.
      * CLEAR OUTPUT FIELDS:
           MOVE 0 TO LFPAP-APCUR
                     LFPAP-APLC
                     LFPAP-APOTH
                     LFPAP-APPDUEINT
                     LFPAP-APPDUEPRIN
                     LFPAP-APCURINT
                     LFPAP-APCURPRIN
                     LFPAP-EXTRA-PRIN
                     LFPAP-INDU-INTEREST.
      * CLEAR OUTPUT FIELDS:

      * SAVE LOAN FIELDS:
           MOVE LN-CURBAL       TO LFPAP-SAVE-LN-CURBAL.
           MOVE LN-INTPDTH-DATE TO LFPAP-SAVE-LN-INTPDTH-DATE.

      *    DISBURSE AGAINST INTEREST BALANCE:
           IF LFPAP-INTBAL <= LFPAP-TRAMT
              MOVE LFPAP-INTBAL TO LFPAP-APINT
                                   LFPAP-APPDUEINT
              SUBTRACT LFPAP-INTBAL FROM LFPAP-TRAMT
              MOVE 0 TO LFPAP-INTBAL
           ELSE
              MOVE LFPAP-TRAMT TO LFPAP-APINT
                                  LFPAP-APPDUEINT
              SUBTRACT LFPAP-TRAMT FROM LFPAP-INTBAL
              MOVE 0 TO LFPAP-TRAMT.

           PERFORM UNTIL LFPAP-TRAMT = 0
      *      DETERMINE IF SCHEDULED PAY IS IN CURRENT OR PASTDUE PERIOD:
              MOVE LN-1STPYDATE       TO PDTH-DATE-WORK
              PERFORM PAID-THRU-CALCULATION
              MOVE PDTH-CUR-SCHD-DATE TO SYS-DATE
              MOVE LFPAP-TODAY        TO NUM-DATE
              MOVE S-DD               TO NUM-DA
              IF SYS-DATE NOT < NUM-DATE
                 MOVE "C" TO LFPAP-PERIOD
              ELSE
                 MOVE "P" TO LFPAP-PERIOD
              END-IF
      *       - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *       APPLY TRAMT TO REMAIN-INT-SINCE-LFP:
      *       - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF LFPAP-TRAMT >= LFPAP-REMAIN-INT-SINCE-LFP
                 SUBTRACT LFPAP-REMAIN-INT-SINCE-LFP FROM
                                               LFPAP-TRAMT
                 IF LFPAP-PERIOD = "C"
                    ADD LFPAP-REMAIN-INT-SINCE-LFP TO LFPAP-APCURINT
                 ELSE
                    ADD LFPAP-REMAIN-INT-SINCE-LFP TO LFPAP-APPDUEINT
                 END-IF
                 ADD LFPAP-REMAIN-INT-SINCE-LFP    TO LFPAP-APINT
                                                      LFPAP-TOTPAYMNTD
                                                      LN-TOTPAYMNTD
                 MOVE 0          TO LFPAP-REMAIN-INT-SINCE-LFP
              ELSE
                 SUBTRACT LFPAP-TRAMT FROM LFPAP-REMAIN-INT-SINCE-LFP
                 IF LFPAP-PERIOD = "C"
                    ADD LFPAP-TRAMT TO LFPAP-APCURINT
                 ELSE
                    ADD LFPAP-TRAMT TO LFPAP-APPDUEINT
                 END-IF
                 ADD LFPAP-TRAMT    TO LFPAP-APINT
                                       LFPAP-TOTPAYMNTD
                                       LN-TOTPAYMNTD
                 MOVE 0             TO LFPAP-TRAMT
              END-IF
      *       - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *       APPLY TRAMT TO REMAIN-PRIN-SINCE-LFP:
      *       - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF LFPAP-TRAMT >= LFPAP-REMAIN-PRIN-SINCE-LFP
                 SUBTRACT LFPAP-REMAIN-PRIN-SINCE-LFP FROM
                                               LFPAP-TRAMT
                                               LN-CURBAL
                                               LFPAP-CURBAL
                 IF LFPAP-PERIOD = "C"
                    ADD LFPAP-REMAIN-PRIN-SINCE-LFP
                                                 TO LFPAP-APCURPRIN
                 ELSE
                    ADD LFPAP-REMAIN-PRIN-SINCE-LFP
                                                 TO LFPAP-APPDUEPRIN
                 END-IF
                 ADD LFPAP-REMAIN-PRIN-SINCE-LFP TO LFPAP-APCUR
                                                    LFPAP-TOTPAYMNTD
                                                    LN-TOTPAYMNTD
                 MOVE 0          TO LFPAP-REMAIN-PRIN-SINCE-LFP
              ELSE
                 SUBTRACT LFPAP-TRAMT FROM LFPAP-REMAIN-PRIN-SINCE-LFP
                                           LN-CURBAL
                                           LFPAP-CURBAL
                 IF LFPAP-PERIOD = "C"
                    ADD LFPAP-TRAMT TO LFPAP-APCURPRIN
                 ELSE
                    ADD LFPAP-TRAMT TO LFPAP-APPDUEPRIN
                 END-IF
                 ADD LFPAP-TRAMT    TO LFPAP-APCUR
                                       LFPAP-TOTPAYMNTD
                                       LN-TOTPAYMNTD
                 MOVE 0             TO LFPAP-TRAMT
              END-IF
              IF LFPAP-REMAIN-INT-SINCE-LFP  = 0 AND
                 LFPAP-REMAIN-PRIN-SINCE-LFP = 0
                 PERFORM LFPAP-NEXT-SCHED-PAY
              END-IF
      *      DETERMINE IF PAID EXTRA PRINCIPAL:
              IF LFPAP-TRAMT NOT = 0
                 MOVE PDTH-CUR-SCHD-DATE TO SYS-DATE
                 MOVE LFPAP-TODAY        TO NUM-DATE
                 MOVE S-DD               TO NUM-DA
                 IF SYS-DATE > NUM-DATE
                    MOVE LFPAP-TRAMT     TO LFPAP-EXTRA-PRIN
                    ADD  LFPAP-TRAMT     TO LFPAP-APCUR
                                            LFPAP-TOTEXCPAYMNTD
                                            LN-TOTEXCPAYMNTD
                    SUBTRACT LFPAP-TRAMT FROM LFPAP-CURBAL
                                              LN-CURBAL
                    MOVE 0               TO LFPAP-TRAMT
                    PERFORM LFPAP-NEXT-SCHED-PAY
                 END-IF
              END-IF
           END-PERFORM.

       LFPAP-ROUTINE-EXIT.
      *   RESTORE LOAN FIELDS:
           MOVE LFPAP-SAVE-LN-CURBAL       TO LN-CURBAL.
           MOVE LFPAP-SAVE-LN-INTPDTH-DATE TO LN-INTPDTH-DATE.

      ****************************************************
      *    DETERMINE NEXT SCHEDULED DUE DATE
      ****************************************************
       LFPAP-NEXT-SCHED-PAY SECTION.
           MOVE LN-1STPYDATE    TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL  TO NDTE-DATE
                                   LN-INTPDTH-DATE.
           MOVE 1               TO NDTE-HOLD.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE       TO RATE-DATE.
           MOVE LN-MAKERCD(1)   TO RATE-MAKERCD.
           MOVE "PY"            TO RATE-LPTRCD.
           PERFORM SET-PY-RATE.
           ADD INDU-INTEREST    TO LFPAP-INDU-INTEREST.
           MOVE INDU-INTEREST   TO LFPAP-REMAIN-INT-SINCE-LFP.
           COMPUTE LFPAP-REMAIN-PRIN-SINCE-LFP =
                            PDTH-CUR-SCHD-PAY - INDU-INTEREST.

       LFPAP-NEXT-SCHED-PAY-EXIT.
           EXIT.


      *====================================================================

