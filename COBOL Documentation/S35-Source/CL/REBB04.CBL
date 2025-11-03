       IDENTIFICATION DIVISION.
       PROGRAM-ID.       REBB04.
       DATE-WRITTEN.   111695.
      ******************************************************************
      *                      (CL)
      *    DESCRIPTION:  DEFERMENT REBATE CALC FOR S. CAROLINA
      *                   MERCURY FINANCE, ACTION SPDRF = 03
      *                  LAST MONTHLY DEFERMENT PRO RATA
      *         REFUND THE UNEXPIRED PORTION OF THE LAST MONTHLY DF
      *         ON A DAILY PRO RATA BASIS
      *         REFUNDS THROUGH THE NEXT DUE DATE
      *                  CALLED FROM REBATE ROUTINE
      *
      * REBATE FORMULA "Y"
      *
      *    N O T E:   THIS PROGRAM IS FOR MONTHLY ONLY
      *
      * REV:
      *  JTG 012097 CORRECTED PROBLEM WITH LP-PDTH-DATE MMYY LOGIC,
      *             SINCE LP-PDTH-DATE IS IN MMDDYY FORMAT IN A60
      *  MG  990929 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *   CS 151123 MOVE LN-OWNBR TO LP-BRNO, BRNO NOW PART OF LP1-KEY
      *   CS 151204 REMOVED LP-BRNO FROM KEY
      *
      * KEC 20170307 CHANGED, WITHIN REB-B01, B01-FRMLA FROM 1 TO 2 BYTES
      *              SO THAT B01-FRMLA MATCHES SP-RBFRMLA.
      * BAH 20180510 REGENCY USING, WANTS MULTIPLE DEFERMENTS TAKEN INTO
      *              ACCOUNT. MULTIPLY BY NUMBER OF DEFERMENTS
      *              D2 X 2, D3 X 3, D7 X 7, ETC. PL#993
      * BAH 20181219 GLOBAL LPFILE
      *  CS 20210811, ADDED READ OF BRANCH TO SET UP LP-BRANCH, WAS
      *               USING EXT-FILPATH.
      * BAH 20220803 HARD CODED START
      ******************************************************************
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
           "@(#)./CL/REBB04.CBL S34 08/11/21-14:08:19 >".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ERRCD                PIC X VALUE " ".
       
       01  NO-DEF               PIC 9(3).
      *01  DF-WORKER.
      *    03  FILLER         PIC X.
      *    03  DF-FTST        PIC X.
      *    03  DF-NODF REDEFINES DF-FTST PIC 9.

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  REB-COMMON.
           03  REB-COMMON-RTNCD       PIC X.
           03  REB-COMMON-REBATE      PIC S9(7)V99 COMP-3.
           03  REB-COMMON-AREA        PIC X(40).

      * USED FOR CALIFORNIA DEFERMENT REBATE FORMULA
           03  REB-B03                REDEFINES REB-COMMON-AREA.
               05  B03-PAYDATE        PIC 9(8) COMP-3.

           03  REB-B01                REDEFINES REB-COMMON-AREA.
               05  B01-PAYDATE        PIC 9(8) COMP-3.
               05  B01-FRMLA          PIC X(2).
               05  B01-PAY-SCHLD-FG   PIC X.
               05  B01-DEF-STOP       PIC X.
               05  FILLER             PIC X(31).
       COPY "LIBLP/LP01LN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME REB-COMMON LN-REC EXIT-PATHNAME.

      ***********************************************
       MAIN-MODULE SECTION.
      ***********************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

      D    STOP MESS.
           MOVE SPACES TO REB-COMMON-RTNCD.
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

      * VERIFY PAYOFF IS BEFORE DEFERMENT EXPIRATION (PAID THRU + 1 MON)
      * REFUND THROUGH THE NEXT DUE DATE, ALWAYS ADD 1 TO GET NEXT DUE DATE
           MOVE LP-PDTH-DATE TO NDTE-DATE.
           MOVE 1            TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE B03-PAYDATE TO NUM-DATE.
           MOVE NDTE-DATE   TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIM.
           IF ELAPSED-DAYS NOT > 0
              MOVE 0 TO REB-COMMON-REBATE
              GO TO END-PROGRAM.


           COMPUTE REB-COMMON-REBATE = (LP-TRAMT / 30) * ELAPSED-DAYS.

           GO TO END-PROGRAM.

      * REGENCY ONLY
      * DIVIDE BY THE (# OF DEFERMENTS) * 30
      *    MOVE LP-TRCD TO DF-WORKER.
      *    IF LP-TRCD = "DF"
      *       MOVE 1 TO NO-DEF
      *    ELSE
      *       MOVE DF-NODF TO NO-DEF.

      *    COMPUTE REB-COMMON-REBATE ROUNDED =
      *          ( LP-TRAMT / (30 * NO-DEF) ) * ELAPSED-DAYS.

      * MAX REBATE SHOULD BE THE AMOUNT OF THE DEFERMENT CHARGE
      *    IF REB-COMMON-REBATE > LP-TRAMT
      *       MOVE LP-TRAMT TO REB-COMMON-REBATE.
      *       
      *    GO TO END-PROGRAM.


       COPY "LIBGB/DATER.CPY".

      ***********************************************
       IO-ROUTINE SECTION.
      ***********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-LP1-FILE.
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       END-PROGRAM SECTION.
       ENDIT.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
