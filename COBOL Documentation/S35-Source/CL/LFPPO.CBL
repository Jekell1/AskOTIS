       IDENTIFICATION DIVISION.
       PROGRAM-ID.       LFPPO.
       DATE-WRITTEN.   09/21/01.
      **************************************************************************
      *                      (CL)
      *
      *    DESCRIPTION:
      *                 DETERMINE THE INTEREST REFUND OR INTEREST DUE FOR A
      *                 PAYOFF ON AN ACCOUNT UNDER AMORTIZED INTEREST
      *                 SP-CAL-RATETYPE (Z)
      *
      *                 CALL FROM LIBLP/LPPOFF
      *                           LIBLP/FAPOFF
      *
      *REV:
      * JTG 010921 ADDED LOGIC FOR PAYOFF ON SP-CAL-RATETYPE(Z)LENDMARK PR#1045
      * JTG 011207 CHANGED TO HANDLE PREPAYMENT OF INTEREST ON ACCOUNTS
      *                                                        LENDMARK PR#1045
      * JTG 020122 CORRECTED LOGIC FOR PAYOFFS BEFOR PAIDTHRU  LENDMARK PR#1045
      * JTG 020130 CHANGED SET OF LPRATE-LPTRCD TO BE POFF-LPTRCD
      *                                                        LENDMARK PR#1045
      * JTG 020219 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS   WORLD #288
      *   CS 151125 MOVE LN-OWNBR TO LP-BRNO, BRNO NOW PART OF LP1-KEY
      *   CS 151204 REMOVED LP-BRNO FROM KEY
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      * BAH 181219 GLOBAL LPFILE
      * BAH 2022.0310 ADDED SET OF LPFILE PATH (CINDY)
      * BAH 20220803 HARD CODED START
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)CL/LFPPO.CBL S35 01/31/22-15:33:57 >".

       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LTI.CPY".

       01  ERRCD                         PIC X VALUE " ".
       01  LFPPO-SAVE-LN-CURBAL          PIC S9(7)V99 COMP.
       01  LFPPO-SAVE-LN-INTPDTH-DATE    PIC 9(8)     COMP.
       01  LFPPO-APINT                   PIC S9(7)V99 COMP.
       01  LFPPO-PDTH-BAL                PIC S9(7)V99 COMP.
       01  LFPPO-WORKER                  PIC S9(7)V99 COMP.

       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01SP.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME POFF-WORKERS LN-REC SP-REC
                  EXIT-PATHNAME.

      ********************************************
       MAIN-MODULE SECTION.
      ********************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

      * CLEAR OUTPUT FIELDS:
           MOVE 0 TO POFF-INTDUE.

           PERFORM GET-GPENV.

      * GET BRANCH RECORD:
           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE "INVALID BRANCH RECORD" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE        TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH      TO FILPATH-DATA.
           MOVE "B"               TO LP-PATH-OVERRIDE.

      * DETERMINE IF REFUND OF INTEREST IS DUE OR ADDITIONAL CHARGES ARE NEEDED
           MOVE LN-1STPYDATE    TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           IF POFF-PAYDATE <= PDTH-DATE-FULL
              PERFORM LFPPO-REFUND
           ELSE
              PERFORM LFPPO-NEW-CHARGE.

      * BECAUSE THE ABOVE 2 ROUTINES OVERRIDE LN-CURBAL, THE COMPUTED
      * PENALTY IS INCORRECT AND MUST BE RECOMPUTED AGAIN:
      * DETERMINE THE INTEREST CHARGE DUE BY REDUCING IT BY THE BOGUS PENALTY

           COMPUTE POFF-INTDUE = POFF-INTDUE - POFF-O5-INT-REB.

      * NOW COMPUTE CORRECT PENALTY:
           MOVE LN-INTPDTH-DATE       TO LFPPO-SAVE-LN-INTPDTH-DATE.

           MOVE LN-1STPYDATE          TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL        TO LN-INTPDTH-DATE.
           MOVE POFF-PAYDATE          TO RATE-DATE.
           MOVE LN-MAKERCD(1)         TO RATE-MAKERCD.
           MOVE POFF-LPTRCD           TO RATE-LPTRCD.
           PERFORM SET-PY-RATE.

           MOVE LFPPO-SAVE-LN-INTPDTH-DATE TO LN-INTPDTH-DATE.

      * POFF-INTDUE    : AMOUNT OF INTEREST DUE + CORRECT PENALTY
      * POFF-O5-INT-REB: AMOUNT OF CORRECT PENALTY

           COMPUTE POFF-INTDUE     = POFF-INTDUE + INDU-PENALTY.
           COMPUTE POFF-O5-INT-REB = INDU-PENALTY.

           GO TO END-PROGRAM.

      ************************************************************
       LFPPO-REFUND SECTION.
      ************************************************************

      * PAYOFF IS ON OR BEFORE PAID THRU, SO A REFUND OF INTEREST IS DUE

           MOVE 0 TO LFPPO-APINT.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       LFPPO-REFUND-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF (IO-BAD) OR (LP-BRNO NOT = LP-PATH-OWNBR)
                        OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO LFPPO-REFUND-EXIT.

           IF (LP-TRCD = "RV") OR (LP-REV = "Y")
              GO TO LFPPO-REFUND-NEXT.

      * TEST FOR BALANCE FORWARD:
           IF LP-SEQNO = 1
              PERFORM LFPPO-INTDUE-THRU-POFF
              GO TO LFPPO-REFUND-EXIT.

           MOVE POFF-PAYDATE TO NUM-DATE.
           MOVE LN-1STPYDA   TO NUM-DA.
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WHILE PAID THRU IS ON OR AFTER PAYOFF DATE, ACCUMULATE LP-APINT
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF LP-PDTH-DATE >= NUM-DATE
              IF LP-TRCD = "PY" OR "PO" OR "PA" OR "PR" OR "PC"
                                OR "PP" OR "AH" OR "PN" OR "PZ"
                 ADD LP-APINT TO LFPPO-APINT
              END-IF
              GO TO LFPPO-REFUND-NEXT.

           PERFORM LFPPO-INTDUE-THRU-POFF.

       LFPPO-REFUND-EXIT.
           EXIT.

      ************************************************************
       LFPPO-INTDUE-THRU-POFF SECTION.
      ************************************************************

      * DETERMINE AMOUNT OF INTEREST DUE THRU PAYOFF DATE

           MOVE LN-CURBAL             TO LFPPO-SAVE-LN-CURBAL.
           MOVE LN-INTPDTH-DATE       TO LFPPO-SAVE-LN-INTPDTH-DATE.

           MOVE LP-CURBAL             TO LN-CURBAL.
           MOVE LP-PDTH-DATE          TO LN-INTPDTH-DATE.
           MOVE POFF-PAYDATE          TO RATE-DATE.
           MOVE LN-MAKERCD(1)         TO RATE-MAKERCD.
           MOVE POFF-LPTRCD           TO RATE-LPTRCD.
           PERFORM SET-PY-RATE.

           MOVE LFPPO-SAVE-LN-CURBAL       TO LN-CURBAL.
           MOVE LFPPO-SAVE-LN-INTPDTH-DATE TO LN-INTPDTH-DATE.

      * POFF-INTDUE    : AMOUNT OF INTEREST DUE + PENALTY - INT OVERPAID
      * POFF-O5-INT-REB: AMOUNT OF INTEREST PENALTY

           COMPUTE POFF-INTDUE     = INDU-INTEREST - LFPPO-APINT.
           COMPUTE POFF-O5-INT-REB = INDU-PENALTY.


      ************************************************************
       LFPPO-NEW-CHARGE SECTION.
      ************************************************************

      * PAYOFF IS AFTER PAID THRU SO, DETERMINE NEW INTEREST DUE

           MOVE 0 TO LFPPO-APINT.

      * SAVE LOAN FIELDS:
           MOVE LN-CURBAL       TO LFPPO-SAVE-LN-CURBAL.
           MOVE LN-INTPDTH-DATE TO LFPPO-SAVE-LN-INTPDTH-DATE.

      * GET BALANCE AS OF CURRENT PAID THRU SET:
           MOVE LN-CURBAL TO LFPPO-PDTH-BAL.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.

       LFPPO-NEW-CHARGE-READ-PREV.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF (IO-GOOD) AND (LP-BRNO = LP-PATH-OWNBR)
                         AND (LP-ACCTNO = LN-ACCTNO)
              IF (LP-TRCD = "RV") OR (LP-REV  = "Y" )
                 GO TO LFPPO-NEW-CHARGE-READ-PREV
              END-IF
              IF (LP-PDTH-DATE = PDTH-DATE-FULL)
                 MOVE LP-CURBAL TO LFPPO-PDTH-BAL
                 GO TO LFPPO-NEW-CHARGE-READ-PREV.

           PERFORM CLOSE-LP1-FILE.

      *    - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DETERMINE (INDU-INTEREST) THE AMOUNT OF INTEREST
      *    ESTABLISHED WHEN THE LAST PAID THRU WAS SET
      *    - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE PDTH-DATE-FULL  TO LN-INTPDTH-DATE
                                   NDTE-DATE.
           MOVE 1               TO NDTE-HOLD.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE       TO RATE-DATE.
           MOVE LN-MAKERCD(1)   TO RATE-MAKERCD.
           MOVE POFF-LPTRCD     TO RATE-LPTRCD.
           PERFORM SET-PY-RATE.

      *    - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DETERMINE INTEREST PAID SINCE PAID THRU WAS SET
      *    - - - - - - - - - - - - - - - - - - - - - - - - -
           COMPUTE LFPPO-WORKER =
                   INDU-INTEREST - INDU-PENALTY
                                 - LN-REMAIN-INT-SINCE-LFP.

      *    - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DETERMINE THE INTEREST DUE + ANY POFF PENALTY
      *    - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE PDTH-DATE-FULL  TO LN-INTPDTH-DATE.
           MOVE POFF-PAYDATE    TO RATE-DATE.
           MOVE LN-MAKERCD(1)   TO RATE-MAKERCD.
           MOVE POFF-LPTRCD     TO RATE-LPTRCD.
           PERFORM SET-PY-RATE.
      *    - - - - - - - - - - - - - - - - - - - - - - - - -
      *    POFF-INTDUE    : AMOUNT OF INTEREST DUE + PENALTY
      *    POFF-O5-INT-REB: AMOUNT OF INTEREST PENALTY
      *    - - - - - - - - - - - - - - - - - - - - - - - - -
           COMPUTE POFF-INTDUE =
                   INDU-INTEREST - LFPPO-WORKER.
           COMPUTE POFF-O5-INT-REB = INDU-PENALTY.

       LFPPO-NEW-CHARGE-EXIT.
           MOVE LFPPO-SAVE-LN-CURBAL       TO LN-CURBAL.
           MOVE LFPPO-SAVE-LN-INTPDTH-DATE TO LN-INTPDTH-DATE.

       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/GPENV.CPY".

      *********************************************
       IO-ROUTINE SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ************************************************************
       END-PROGRAM SECTION.
      ************************************************************
       ENDIT.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
