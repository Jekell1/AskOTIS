       IDENTIFICATION DIVISION.
       PROGRAM-ID.       REBB05.
       DATE-WRITTEN.   111695.
      *********************************************************
      *                      (CL)
      *    DESCRIPTION:  DEFERMENT REBATE CALC FOR ARIZONA
      *                   MERCURY FINANCE, ACTION SPDRF = 05
      *                  PAYOFF DURING DEFERMENT PERIOD OF 1 OR 2 MONTHS
      *         1-MONTH DEFERMENT- REFUND DEFERMENT CHARGE
      *         2-MONTH DEFERMENT- IF PAID OFF IN FIRST MONTH,
      *                            REFUND TOTAL DEFERMENT CHARGE.
      *                            IF PAID OFF IN SECOND MONTH, REFUND
      *                            1 MONTH OF DEFERMENT CHARGE.
      *                  CALLED FROM REBATE ROUTINE
      * REBATE FORMULA "Z"
      *
      *    N O T E:   THIS PROGRAM IS FOR MONTHLY ONLY
      * REV:
      *  JTG 012097 CORRECTED PROBLEM WITH LP-PDTH-DATE MMYY LOGIC,
      *             SINCE LP-PDTH-DATE IS IN MMDDYY FORMAT IN A60
      *  MG  990929 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *   CS 151120 MOVE LN-OWNBR TO LP-BRNO, BRNO NOW PART OF LP1-KEY
      * BAH 181219 GLOBAL LPFILE
      *  CS 20210811, ADDED READ OF BRANCH TO SET UP LP-BRANCH, WAS
      *               USING EXT-FILPATH.
      * BAH 20220803 HARD CODED START
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
           "@(#)./CL/REBB05.CBL S34 08/11/21-14:11:46 >".
       01  ERRCD                PIC X VALUE " ".
       01  NO-DEF               PIC 9(3).
       01  HOLD-CUR-1STPYDATE   PIC 9(8).
       01  HOLD-TRCD.
           03  FILLER           PIC X.
           03  HOLD-TRCD-NO-DEF PIC 9.
       01  HOLD-PDTH-DATE       PIC 9(8).

      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".


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
               05  B02-DEF-STOP       PIC X.
               05  FILLER             PIC X(34).
       COPY "LIBLP/LP01LN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME REB-COMMON LN-REC EXIT-PATHNAME.

      **********************************************
       MAIN-MODULE SECTION.
      **********************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
       MAIN-ROUTINE.
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
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       NEXT-LP-REC.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = LN-ACCTNO) 
               MOVE 0 TO REB-COMMON-REBATE
               GO TO END-PROGRAM.

           IF LP-REV = "Y"
              GO TO NEXT-LP-REC.

           IF NOT (LP-TRCD = "DF" OR "D2" OR "D3" OR "D4" OR "D5"
                                  OR "D6" OR "D7" OR "D8" OR "D9")
              GO TO NEXT-LP-REC.

           MOVE LP-TRCD TO HOLD-TRCD.
           MOVE LP-PDTH-DATE TO HOLD-PDTH-DATE.

           PERFORM CALCULATE-DEF-REBATE.

           GO TO END-PROGRAM.

      ****************************************************
      *    CALCULATE DEFERMENT REBATE
      *
      ****************************************************
       CALCULATE-DEF-REBATE SECTION.
           IF HOLD-TRCD = "DF"
              MOVE "D1" TO HOLD-TRCD.

      * COMPUTE DEFERMENT EXPIRATION DATE:
           MOVE HOLD-PDTH-DATE TO NDTE-DATE.
           MOVE 1              TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.

      * VERIFY PAYOFF IS BEFORE EXPIRATION DATE
           MOVE B02-PAYDATE TO NUM-DATE.
           MOVE NDTE-DATE   TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIM.

      * CALCULATE CORRECT DEFERMENT CHARGE:
      * IF ONE DEFERMENT - REFUND FULL DEFERMENT CHARGE IF PAID OFF WITHIN
      * THAT ONE MONTH
           IF HOLD-TRCD-NO-DEF = 1
              IF ELAPSED-DAYS > 0
                 MOVE LP-TRAMT TO REB-COMMON-REBATE
      * TEST IF THEY DID 2 "DF"'S IN A ROW, INSTEAD OF D2
                 IF LN-TOTNODEF > 1
                    PERFORM FIND-2-DEFERMENT-RECORDS
                 ELSE
                    GO TO EXIT-CALC-DEF-REBATE
              ELSE
                 GO TO EXIT-CALC-DEF-REBATE.

      * IF 2 DEFERMENTS -
      * IF PAID OFF IN FIRST MONTH, REFUND TOTAL CHARGE
      * IF PAID OFF IN SECOND MONTH, REFUND 1 MONTH OF TOTAL CHARGE
           IF HOLD-TRCD-NO-DEF = 2
              IF ELAPSED-DAYS NOT > 0
                 GO TO EXIT-CALC-DEF-REBATE
              ELSE
                 IF ELAPSED-DAYS > 60
                    GO TO EXIT-CALC-DEF-REBATE
                 ELSE
                    IF ELAPSED-DAYS > 30
                       MOVE LP-TRAMT TO REB-COMMON-REBATE
                    ELSE
                       COMPUTE REB-COMMON-REBATE = LP-TRAMT / 2.


       EXIT-CALC-DEF-REBATE.
           EXIT.

      ******************************************
       FIND-2-DEFERMENT-RECORDS SECTION.
      ******************************************
       FIND-2-NEXT-LP.
      * SEE IF LAST TRANSACTION WAS ALSO A DEFERMENT
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = LN-ACCTNO) 
               GO TO EXIT-FIND-2.

           IF LP-REV = "Y"
              GO TO FIND-2-NEXT-LP.

           IF NOT (LP-TRCD = "DF")
              GO TO FIND-2-NEXT-LP.

      * COMPUTE DEFERMENT EXPIRATION DATE:
           MOVE LP-PDTH-DATE TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.

      * VERIFY PAYOFF IS BEFORE EXPIRATION DATE
           MOVE B02-PAYDATE TO NUM-DATE.
           MOVE NDTE-DATE   TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIM.

           IF ELAPSED-DAYS NOT > 0
              GO TO EXIT-FIND-2
           ELSE
              IF ELAPSED-DAYS NOT > 30
                 ADD LP-TRAMT TO REB-COMMON-REBATE.

       EXIT-FIND-2.
           EXIT.

       COPY "LIBGB/DATER.CPY".

      **********************************************
       IO-ROUTINE SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-LP1-FILE.
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".

      **********************************************
       END-PROGRAM SECTION.
      **********************************************
       ENDIT.
           IF REB-COMMON-REBATE > LN-TOTDEF
              MOVE LN-TOTDEF TO REB-COMMON-REBATE.
           PERFORM CLOSE-FILES.
           MOVE HOLD-CUR-1STPYDATE TO LN-1STPYDATE.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
