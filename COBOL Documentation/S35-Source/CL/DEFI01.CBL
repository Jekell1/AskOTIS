       IDENTIFICATION DIVISION.
       PROGRAM-ID.       DEFI01.
       DATE-WRITTEN.   04/16/90.
      **************************************************************************
      *                      (CL)
      *    DESCRIPTION:  DEFERMENT CALC FOR INDIANA
      *                  CALLED FROM LPDEF ROUTINE
      *
      *           INDIANA DEFERMENT CALCULATION AND REBATE
      * REV:
      *  JTG 071796 CHANGED FOR UNITPERIODS AND LN-LCPAID = MMDD.YY
      *  JTG 091897 ADDED TEST FOR TRCD = 'AH', WORLD A90
      *  JTG 990324 ADDED 'PN' NON CASH PAYMENT.WITH GL ENTRY           TFC #013
      *             ADDED 'PZ' NON CASH PAYMENT/NO RECDEL.WITH GL ENTRY TFC #012
      *  MG  990928 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *             LIKE "PE", REGACC PR# 0007
      *   CS 151120 MOVE LN-OWNBR TO LP-BRNO, BRNO NOW PART OF LP1-KEY
      *   CS 151204 REMOVED LP-BRNO FROM KEY
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 170403 FIX DATE VERSION OF LN-LCPAID = YYYYMMDD
      *  BAH 181221 GLOBAL LPFILE
      *  CS  20220307 ADD READ OF BR-FILE AND SETUP LP-PATH
      *  BAH 20220830 HARD CODED START
      *  BAH 2024.0610 INDEX OUT OF BOUNDS ERROR; ISSUE WITH LCPAID
      *                USE A WS FIELD AS LP-LCPAID IS NOT ALWAYS A DATE.
      *                NEED TO MAKE SURE IT IS A VALID DATE
      *                MMDD.YY LCPAID WAS LEFT LIKE THIS DUE TO A15 S35Q-63
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)CL/DEFI01 S35 06/10/24-11:28:21 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

       01  ERRCD                       PIC X          VALUE SPACES.
       01  CURRENT-LC-DEL              PIC S9(7)V99   VALUE +0.
       01  CURRENT-LC-DATE             PIC X          VALUE " ".
       01  LC-AMT-SINCE-DEF-PERIOD     PIC S9(7)V99   VALUE +0.
       01  WS-LCPAID                   PIC S9(7)V99   COMP-3.

       01  HOLD-PERIODS-TODAY          PIC S9(3)      VALUE +0.
       01  HOLD-PERIODS-PAIDTHRU       PIC S9(3)      VALUE +0.
       01  HOLD-MAX-ALLOWED-DEFS       PIC S9(3)      VALUE +0.
       01  HOLD-PERIOD-DEFERRED-DATE   PIC 9(8)       VALUE 0.
       01  HOLD-PAID-THRU-DATE         PIC 9(8)       VALUE 0.

       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
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
       01  DEF-COMMON.
           03  DEF-COMMON-RTNCD       PIC X.
           03  DEF-COMMON-DEFCHG      PIC S9(7)V99 COMP-3.
           03  DEF-COMMON-AREA        PIC X(40).
      * USED FOR INDIANA DEFERMENT FORMULA:
           03  FILLER                 REDEFINES DEF-COMMON-AREA.
               05  DEF-POST-DATE      PIC 9(8)     COMP-3.
               05  DEF-LC-REBATE      PIC S9(7)V99 COMP-3.

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01SP.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME DEF-COMMON LN-REC SP-REC EXIT-PATHNAME.

      ****************************************************
       MAIN-MODULE SECTION.
      ****************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
       MAIN-ROUTINE.
           MOVE SPACES TO DEF-COMMON-RTNCD.
           MOVE 0 TO DEF-COMMON-DEFCHG DEF-LC-REBATE.

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.
           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD 
              GO TO IO-ERROR.
     
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO HOLD-PAID-THRU-DATE.

      * DETERMINE IF PAIDTHRU IS ON OR PAST MATURITY?
      * IF SO, NO DEFERMENT ALLOWED:
           PERFORM MATURITY-DATE-CALCULATION.
           IF HOLD-PAID-THRU-DATE >=  MDTE-DATE
              MOVE "M" TO DEF-COMMON-RTNCD
              GO TO END-PROGRAM.

      * DETERMINE CONTRACTUAL ELAPSED PERIODS THRU PAIDTHRU:
           MOVE LN-1STPYDATE        TO CEPP1-DATE.
           MOVE HOLD-PAID-THRU-DATE TO CEPP2-DATE.
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
           MOVE CEPP-PERIODS TO HOLD-PERIODS-PAIDTHRU.

      * DETERMINE CONTRACTUAL ELAPSED PERIODS THRU TODAY:
           MOVE LN-1STPYDATE  TO CEPP1-DATE.
           MOVE DEF-POST-DATE TO CEPP2-DATE.
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
           MOVE CEPP-PERIODS TO HOLD-PERIODS-TODAY.

      * CALCULATE MAXIMUM ALLOWED DEFERMENTS:
           SUBTRACT HOLD-PERIODS-PAIDTHRU FROM
                HOLD-PERIODS-TODAY GIVING HOLD-MAX-ALLOWED-DEFS.

      * COMPUTE DEFERMENT CHARGE:
           COMPUTE DEF-COMMON-DEFCHG ROUNDED =
              (LN-CURBAL - LN-OT2BAL - LN-TOTLCHG)
                     * LN-APRATE / 100 / LN-UNITPER-PER-YEAR.

      ********************************************************
      *    FIND LATE CHARGE REBATE FOR PERIOD BEING DEFERRED:
      ********************************************************
           IF LN-TOTLCHG = 0
              GO TO END-PROGRAM.

      * DETERMINE PERIOD BEING DEFERRED:
           MOVE HOLD-PAID-THRU-DATE TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO HOLD-PERIOD-DEFERRED-DATE.

      * SEE IF LATE CHARGES COULD NOT HAVE BEEN ASSESSED
      * ON THE DEFERRED PERIOD:
           MOVE HOLD-PERIOD-DEFERRED-DATE TO NUM-DATE.
           MOVE LN-LCPDTH-DATE            TO SYS-DATE.
           PERFORM TIMUPER.
           IF ELAPSED-UNITPER < 0
              GO TO END-PROGRAM.
           MOVE EXT-FILPATH-BASE  TO FILPATH-BASE.
           MOVE BR-MACHINE        TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH      TO FILPATH-DATA.
           MOVE "B"               TO LP-PATH-OVERRIDE.
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
       NEXT-LP-REC.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = LN-ACCTNO)
                       OR (LP-SEQNO = 1)
              GO TO SET-LC-REBATE.

           IF LP-REV = "Y" OR LP-TRCD = "RV" OR LP-SSNO = 999999999
              GO TO NEXT-LP-REC.

           IF NOT (LP-TRCD = "PY" OR "PA" OR "PE" OR "SS" OR "AL"
                          OR "AH" OR "PR" OR "PC" OR "PP" OR "PN"
                          OR "PZ" OR "IN" )
              GO TO NEXT-LP-REC.

      * COMPARE DEFERRED PERIOD TO LC PAID THRU:

      * USE A WS FIELD AS LP-LCPAID IS NOT ALWAYS A DATE. NEED TO MAKE
      * SURE IT IS A VALID DATE
      * MMDD.YY LCPAID WAS LEFT LIKE THIS DUE TO A15
           MULTIPLY LP-LCPAID BY 100 GIVING DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
      * INVALID DATE
              MOVE 0             TO WS-LCPAID
           ELSE
              MOVE DATE-YYYYMMDD TO WS-LCPAID.

           MOVE HOLD-PERIOD-DEFERRED-DATE TO NUM-DATE.
           MOVE LP-LCPDTH-DATE            TO SYS-DATE.
           PERFORM TIMUPER.
      **** DETERMINE IF LCPDTH IS BEYOND DEFERRED PERIOD:
           IF ELAPSED-UNITPER NOT < 0
              ADD LP-APLC TO LC-AMT-SINCE-DEF-PERIOD
      ******* SEE IF DEFERRED PERIOD WAS EVER CURRENT:
              IF WS-LCPAID NOT = 0
                 MOVE HOLD-PERIOD-DEFERRED-DATE TO NUM-DATE
                 MOVE WS-LCPAID                 TO SYS-DATE
                 PERFORM TIMUPER
                 IF ELAPSED-UNITPER = 0
                    MOVE "Y" TO CURRENT-LC-DATE
                    MOVE LP-LCPARTIALS TO CURRENT-LC-DEL
                    GO TO NEXT-LP-REC
                 ELSE
                    GO TO NEXT-LP-REC
              ELSE
                 GO TO NEXT-LP-REC.

       SET-LC-REBATE.

      * USE A WS FIELD AS LP-LCPAID IS NOT ALWAYS A DATE. NEED TO MAKE
      * SURE IT IS A VALID DATE
      * MMDD.YY LCPAID WAS LEFT LIKE THIS DUE TO A15
           MULTIPLY LN-LCPAID BY 100 GIVING DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
      * INVALID DATE
              MOVE 0             TO WS-LCPAID
           ELSE
              MOVE DATE-YYYYMMDD TO WS-LCPAID.

      * CHECK FOR CURRENT PERIOD NEVER SET IN LP-REC'S
      * SINCE NO PAYMENTS OCCURED SINCE POSTING:
           IF CURRENT-LC-DATE = " "
              IF CURRENT-LC-DEL = 0
                 IF WS-LCPAID NOT = 0
                    MOVE HOLD-PERIOD-DEFERRED-DATE TO NUM-DATE
                    MOVE WS-LCPAID                 TO SYS-DATE
                    PERFORM TIMUPER
                    IF ELAPSED-UNITPER = 0
                       MOVE "Y" TO CURRENT-LC-DATE
                       MOVE LN-LCPARTIALS TO CURRENT-LC-DEL.

           IF CURRENT-LC-DATE = " "
              IF CURRENT-LC-DEL = 0
                 MOVE LN-REGPYAMT TO CURRENT-LC-DEL.

           COMPUTE DEF-LC-REBATE ROUNDED  =
                                  CURRENT-LC-DEL * SP-LCRATE / 100.
           IF DEF-LC-REBATE > SP-LCMAXAMT
              MOVE SP-LCMAXAMT TO DEF-LC-REBATE
              IF DEF-LC-REBATE < SP-LCMINCHG
                 MOVE SP-LCMINCHG TO DEF-LC-REBATE
              ELSE
                 NEXT SENTENCE
           ELSE
              IF DEF-LC-REBATE < SP-LCMINCHG
                 MOVE SP-LCMINCHG TO DEF-LC-REBATE.

           IF DEF-LC-REBATE > LC-AMT-SINCE-DEF-PERIOD
              MOVE LC-AMT-SINCE-DEF-PERIOD TO DEF-LC-REBATE.

           GO TO END-PROGRAM.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/GPENV.CPY".

      **********************************************
       IO-ROUTINE SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
      **********************************************
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-PROGRAM SECTION.
       ENDIT.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
