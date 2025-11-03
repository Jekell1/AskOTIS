       IDENTIFICATION DIVISION.
       PROGRAM-ID.       REBB01.
       DATE-WRITTEN.   03/02/89.
      **************************************************************
      *                      (CL)
      *    DESCRIPTION:  DEFERMENT REBATE CALC FOR TENNESSEE
      *                  CALLED FROM REBATE ROUTINE
      *  REBATE FORMULA'S  "E" AND "O"
      *
      *    N O T E:
      *            THIS PROGRAM ONLY WORKS WITH MONTHLY PAPER
      * REV:
      *  MG  990929 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *   CS 151123 MOVE LN-OWNBR TO LP-BRNO, BRNO NOW PART OF LP1-KEY
      *   CS 151204 REMOVED LP-BRNO FROM KEY
      *
      * KEC 2017.0307 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED, WITHIN REB-B01, B01-FRMLA FROM 1 TO 2 BYTES
      *          SO THAT B01-FRMLA MATCHES SP-RBFRMLA.
      * BAH 170405 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181219 GLOBAL LPFILE
      *  CS 20210811, ADDED READ OF BRANCH TO SET UP LP-BRANCH, WAS 
      *               USING EXT-FILPATH.
      * BAH 20220803 HARD CODED START
      **************************************************************
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./CL/REBB01.CBL S34 08/11/21-13:34:11 >".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
      
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ERRCD                PIC X VALUE " ".
       01  TRCD-WK.
           03  FILLER           PIC X.
           03  TRCD-NODEF       PIC 9.
       01  WRP                  PIC 9(3).
       01  NO-DEF               PIC 9(3).

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  REB-COMMON.
           03  REB-COMMON-RTNCD       PIC X.
           03  REB-COMMON-REBATE      PIC S9(7)V99 COMP-3.
           03  REB-COMMON-AREA        PIC X(40).

      * USED FOR TENNESSEE DEFERMENT REBATE FORMULA:
           03  REB-B01                REDEFINES REB-COMMON-AREA.
               05  B01-PAYDATE        PIC 9(8) COMP-3.
               05  B01-FRMLA          PIC X(2).
               05  B01-PAY-SCHLD-FG   PIC X.
               05  B01-DEF-STOP       PIC X.
               05  FILLER             PIC X(31).

       COPY "LIBLP/LP01LN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME REB-COMMON LN-REC EXIT-PATHNAME.

      **********************************************
       MAIN-MODULE SECTION.
      **********************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
       MAIN-ROUTINE.
           MOVE SPACES TO REB-COMMON-RTNCD.
           MOVE 0 TO REB-COMMON-REBATE
                     NO-DEF.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              MOVE "E" TO REB-COMMON-RTNCD
              GO TO END-PROGRAM.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
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
           MOVE 0             TO LP-SEQNO
                                 QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE.
       NEXT-LP-REC.
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = LN-ACCTNO)
              MOVE LN-TOTDEF TO REB-COMMON-REBATE
              IF B01-DEF-STOP = "Y"
                MOVE "PAYMENT RECORDS FOR ALL DEFERMENTS NOT FOUND"
                                                 TO MESS
                PERFORM SEND-MESS
                MOVE "!!STOP -- MANUAL REFUND IS REQUIRED -- STOP!!"
                                                 TO MESS
                PERFORM SEND-MESS
                MOVE "E" TO REB-COMMON-RTNCD
                GO TO END-PROGRAM
              ELSE
                GO TO END-PROGRAM.

           IF LP-REV = "Y"
              GO TO NEXT-LP-REC.

           IF LP-TRCD NOT = "DF"
                   AND NOT = "D2"
                    AND NOT = "D3"
                     AND NOT = "D4"
                      AND NOT = "D5"
                       AND NOT = "D6"
                        AND NOT = "D7"
                         AND NOT = "D8"
                          AND NOT = "D9"
              GO TO NEXT-LP-REC.

           PERFORM CALCULATE-DEF-REBATE.

           IF NO-DEF < LN-TOTNODEF
              GO TO NEXT-LP-REC
           ELSE
              GO TO END-PROGRAM.

      ***************************************
      *    CALCULATE DEFERMENT REBATE
      *
      * NOTE: ALL CHARGES ARE THE SAME FOR MULTIPLE
      *       POSTED DEFERMENTS
      **********************************************
       CALCULATE-DEF-REBATE SECTION.
           MOVE LP-TRCD TO TRCD-WK.
           IF TRCD-WK = "DF"
              MOVE 1 TO TRCD-NODEF.
           DIVIDE TRCD-NODEF INTO LP-TRAMT.
           IF (B01-FRMLA = "E")
                 OR (B01-FRMLA = "O" AND B01-PAY-SCHLD-FG = " ")
              PERFORM CALCULATE-REBATE-AMOUNT TRCD-NODEF TIMES
           ELSE
              PERFORM CALC-SCHD-REBATE-AMT TRCD-NODEF TIMES.

      ****************************************
      *    CALCULATE REBATE AMOUNT
      ****************************************
       CALCULATE-REBATE-AMOUNT SECTION.
      * COMPUTE DEFERMENT DUE DATE:
            MOVE LN-1STPYDATE TO NDTE-DATE.
            COMPUTE WRP = LP-CURBAL / LN-REGPYAMT.
            COMPUTE NDTE-HOLD = LN-ORGTERM - WRP + NO-DEF.
            PERFORM INCREMENT-MONTHS.

            MOVE NDTE-DATE   TO NUM-DATE.
      * DERIVE ELAPSED DAYS BETWEEN DEFERED PAYMENT DUE DATE
      * AND PAYODD DATE:
            MOVE B01-PAYDATE TO SYS-DATE.
            MOVE 360 TO ELAPSED-YRTYPE.
            PERFORM TIMALL.
            IF B01-FRMLA = "O"
               COMPUTE ELAPSED-DAYS = ELAPSED-MONTHS * 30
               IF ELAPSED-REM > 0
                  ADD 30 TO ELAPSED-DAYS.
            IF ELAPSED-DAYS < 0
               MOVE 0 TO ELAPSED-DAYS.

      * GET PRORATION OF DEFERMENT CHARGE BASED ON ELAPSED
      * DEFERRED DAYS TO ORIGINAL DEFERRED PERIOD:
            IF ELAPSED-DAYS NOT > (WRP * 30)
               COMPUTE REB-COMMON-REBATE ROUNDED =
                   REB-COMMON-REBATE + LP-TRAMT
                     - LP-TRAMT * (ELAPSED-DAYS / (WRP * 30)).

       CALCULATE-REBATE-AMOUNT-EXIT.
           ADD 1 TO NO-DEF.


      ****************************************
      *    CALCULATE REBATE AMOUNT
      *       RE: PAYMENT SCHEDULES
      ****************************************
       CALC-SCHD-REBATE-AMT SECTION.
      * COMPUTE DEFERMENT DUE DATE:
            MOVE LP-DEF-DUEDATE TO NUM-DATE.

      * DERIVE ELAPSED DAYS BETWEEN DEFERED PAYMENT DUE DATE
      * AND PAYODD DATE:
            MOVE B01-PAYDATE    TO SYS-DATE.
            MOVE 360 TO ELAPSED-YRTYPE.
            PERFORM TIMALL.
            IF B01-FRMLA = "O"
               COMPUTE ELAPSED-DAYS = ELAPSED-MONTHS * 30
               IF ELAPSED-REM > 0
                  ADD 30 TO ELAPSED-DAYS.
            IF ELAPSED-DAYS < 0
               MOVE 0 TO ELAPSED-DAYS.

      * GET PRORATION OF DEFERMENT CHARGE BASED ON ELAPSED
      * DEFERRED DAYS TO ORIGINAL DEFERRED PERIOD:
            IF ELAPSED-DAYS NOT > (LP-DEF-TERM * 30)
               COMPUTE REB-COMMON-REBATE ROUNDED =
                   REB-COMMON-REBATE
                     + LP-TRAMT
                      - LP-TRAMT
                       * (ELAPSED-DAYS / (LP-DEF-TERM * 30)).

       CALC-SCHD-REBATE-AMT-EXIT.
           ADD 1 TO NO-DEF.

       COPY "LIBGB/DATER.CPY".

      ********************************************
       IO-ROUTINE SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-LP1-FILE.
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
     
      ********************************************
       END-PROGRAM SECTION.
      ********************************************
       ENDIT.
           IF REB-COMMON-REBATE > LN-TOTDEF
              MOVE LN-TOTDEF TO REB-COMMON-REBATE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
