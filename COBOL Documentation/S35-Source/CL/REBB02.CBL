       IDENTIFICATION DIVISION.
       PROGRAM-ID.       REBB02.
       DATE-WRITTEN.   05/25/90.
      *********************************************************
      *                      (CL)
      *    DESCRIPTION:  DEFERMENT REBATE CALC FOR INDIANA
      *                  CALLED FROM REBATE ROUTINE
      * REBATE FORMULA "K"
      *   N O T E:  THIS PROGRAM WORKS WITH MONTHLY PAPER ONLY
      * REV:
      *  JTG 012097 CORRECTED PROBLEM WITH LP-PDTH-DATE MMYY LOGIC,
      *             SINCE LP-PDTH-DATE IS IN MMDDYY FORMAT IN A60
      *  MG  990929 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *
      *  KEC 000523 {PR#00000} MERCURY FINANCE [CINDY SHAFFER];
      *              ADDED 'B02-DEF-STOP' FOR THE PURPOSE OF NOT
      *              ALLOWING THE 'SEND-MESS' WITHIN CL/REBB02.C FROM
      *              HAPPENING SO THAT MERCURY CAN HAVE THEIR CUSTOMER
      *              STATEMENTS TO WORK (NOT STOPPING IN BATCH).
      *   CS 151124 MOVE LN-OWNBR TO LP-BRNO, BRNO NOW PART OF LP1-KEY
      *  BAH 170405 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181219 GLOBAL LPFILE
      *  CS 20210811, ADDED READ OF BRANCH TO SET UP LP-BRANCH, WAS
      *               USING EXT-FILPATH.
      *  BAH 20220803 HARD CODED START
      *********************************************************
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
           "@(#)./CL/REBB02.CBL S34 08/11/21-13:45:47 >".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
 
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".



       01  ERRCD                PIC X VALUE " ".
       01  NO-DEF               PIC 9(3).
       01  HOLD-CUR-1STPYDATE   PIC 9(8).
       01  WORKER               PIC S9(7)V99.
       01  HOLD-TRCD.
           03  FILLER           PIC X.
           03  HOLD-TRCD-NO-DEF PIC 9.
       01  HOLD-PDTH-DATE       PIC 9(8).
       01  HOLD-TRAMT           PIC S9(7)V99.

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

      * USED FOR INDIANA DEFERMENT REBATE FORMULA:
           03  REB-B02                REDEFINES REB-COMMON-AREA.
               05  B02-PAYDATE        PIC 9(8) COMP-3.
               05  B02-DEF-STOP       PIC X(1).
               05  FILLER             PIC X(34).
       COPY "LIBLP/LP01LN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME REB-COMMON LN-REC EXIT-PATHNAME.

      *********************************************
       MAIN-MODULE SECTION.
      *********************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE SPACES TO REB-COMMON-RTNCD.
           MOVE LN-1STPYDATE TO HOLD-CUR-1STPYDATE.
           MOVE 0 TO REB-COMMON-REBATE NO-DEF.

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
              MOVE 0   TO REB-COMMON-REBATE
              MOVE "E" TO REB-COMMON-RTNCD
              IF ( B02-DEF-STOP = "Y" )
                 MOVE "PAYMENT RECORDS FOR ALL DEFERMENTS NOT FOUND"
                      TO MESS
                 PERFORM SEND-MESS
                 MOVE "!!STOP  -- MANUAL REFUND IS REQUIRED --   STOP!!"
                      TO MESS
                 PERFORM SEND-MESS
                 GO TO END-PROGRAM
              ELSE
                 GO TO END-PROGRAM.

           IF LP-REV = "Y"
              GO TO NEXT-LP-REC.

           IF LP-TRCD = "DC"
              IF LP-NEW1ST-PYDATE NOT = 0
                 MOVE LP-NEW1ST-PYDATE TO LN-1STPYDATE
                 GO TO NEXT-LP-REC
              ELSE
                 GO TO NEXT-LP-REC.

           IF NOT (LP-TRCD = "DF" OR "D2" OR "D3" OR "D4" OR "D5"
                                  OR "D6" OR "D7" OR "D8" OR "D9")
              GO TO NEXT-LP-REC.

           MOVE LP-TRCD TO HOLD-TRCD.
           MOVE LP-PDTH-DATE TO HOLD-PDTH-DATE.
           PERFORM CALCULATE-DEF-REBATE.
           IF NO-DEF < LN-TOTNODEF
              GO TO NEXT-LP-REC
           ELSE
              GO TO END-PROGRAM.

      ****************************************************
      *    CALCULATE DEFERMENT REBATE
      *
      ****************************************************
       CALCULATE-DEF-REBATE SECTION.
           IF HOLD-TRCD = "DF"
              MOVE "D1" TO HOLD-TRCD.

      * CALCULATE CORRECT DEFERMENT CHARGE:
           COMPUTE HOLD-TRAMT = LP-TRAMT / HOLD-TRCD-NO-DEF.

       CALCULATE-DEF-REBATE-AGAIN.
           IF HOLD-TRCD-NO-DEF > 0
              PERFORM CALCULATE-REBATE-AMOUNT
              MOVE HOLD-PDTH-DATE TO NDTE-DATE
              MOVE -1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO HOLD-PDTH-DATE
              SUBTRACT 1 FROM HOLD-TRCD-NO-DEF
              GO TO CALCULATE-DEF-REBATE-AGAIN.

      ****************************************
      *    CALCULATE REBATE AMOUNT
      ****************************************
       CALCULATE-REBATE-AMOUNT SECTION.

      * DERIVE ELAPSED DAYS BETWEEN DEFERED PAYMENT DUE DATE
      * AND PAYODD DATE:
           MOVE HOLD-PDTH-DATE TO NUM-DATE.
           MOVE B02-PAYDATE    TO SYS-DATE.
           MOVE 360 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS < 0
              MOVE 0 TO ELAPSED-DAYS.

      * GET PRORATION OF DEFERMENT CHARGE BASED ON ELAPSED
      * DEFERRED DAYS TO ORIGINAL DEFERRED PERIOD:
           COMPUTE WORKER ROUNDED =
               HOLD-TRAMT - LN-REGPYAMT * ELAPSED-DAYS
                                    * LN-APRATE / 36000.
           IF WORKER > 0
              ADD WORKER TO REB-COMMON-REBATE.

       CALCULATE-REBATE-AMOUNT-EXIT.
           ADD 1 TO NO-DEF.

       COPY "LIBGB/DATER.CPY".

      *************************************
       IO-ROUTINE SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-LP1-FILE.
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".

      *************************************
       END-PROGRAM SECTION.
      *************************************
       ENDIT.
           IF REB-COMMON-REBATE > LN-TOTDEF
              MOVE LN-TOTDEF TO REB-COMMON-REBATE.
           PERFORM CLOSE-FILES.
           MOVE HOLD-CUR-1STPYDATE TO LN-1STPYDATE.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
