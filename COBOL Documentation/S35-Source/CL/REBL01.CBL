       IDENTIFICATION DIVISION.
       PROGRAM-ID.      REBL01.
       DATE-WRITTEN.    111088.
      **************************************************************************
      *                 (CL)
      *  DESCRIPTION:   DIRECT LOANS (NON SALES FINANCE)
      *                 MISSISSIPPI PRORATA REBATE TEST/UPDATE
      *                 CALLED FROM LONPD0.C
      *
      * NOTE: THIS PROGRAM CALLS CL/ORIGRB
      *
      *    IF NOT (LN-SRCD = "PB" OR "RN" OR "SC"
      *                         OR "RB" OR "RO")
      *     IF SP-RBEARLY(7) = 3 OR 19 OR 16
      *
      *    19 -  SOUTH CAROLINA (61000) INTEREST:
      *          WHEN COMPANY PAYOFF WITHIN 90 DAYS, DO PRORATA
      *
      *          ORIGINAL "RI" WAS NOT PRORATA, SO DO PRORATA
      *          WITH TODAY'S DATE , IF NEW REBATE IS > OLD REBATE,
      *          CREATES AN ADJUSTMENT (AFFECTS NEW LOAN PROCEEDS),
      *          AND MEMO'S BOTH ACCOUNTS
      *REV:
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * JTG 051297 ADDED USE OF 'LIBLP/SETCTBLW' & 'LIBLP/SETCTBL'
      * BAH 040898 FIND-ORIG-REBATE WAS NOT SETTING TAH-IO-FG WHEN A
      *            'RI' WAS FOUND, SO IT SKIPPED THE LOAN, MADE LIKE
      *            A90
      * CS  042798 ADD TEST OF "RB" AND "RO" (RENEWALS)
      * MG  990930 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * JTG 020219 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS   WORLD #288
      *  CS 060630 ADD LT-REC TO LINKAGE  PR#534 CENTRAL ILL
      *  CS 151124 MOVE LN-OWNBR TO LP-BRNO, BRNO NOW PART OF LP1-KEY
      *   CS 151204 REMOVED LP-BRNO FROM KEY
      * BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      * BAH 181127 GLOBAL LMFILE
      * BAH 181203 GLOBAL LNFILE
      * BAH 181219 GLOBAL LPFILE
      * BAH 20200910 ADDED WHERE CONSTRAINT
      * BAH 20220803 HARD CODED START
      * BAH 20230711 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      * BAH 20231012 REMOVED REBL01-LTI-REC FROM THE LINKAGE, LONPD0 NEVER
      *              PASSED IT. DETERMINED IT IS NOT NEEDED.
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/REBL01.CBL S35 04/01/24-10:49:04 >".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

       01  TAA-IO-FG       PIC X(3).
       01  TAA-SUB         PIC 9(3).
       01  TAH-LN-OWNBR    PIC 9(4).
       01  TAH-LN-ACCTNO   PIC 9(8).
       01  TAH-IO-FG       PIC X(3).
       01  TAH-ORIG-REBATE PIC S9(7)V99.
       01  TAH-1ST-LP      PIC X.
       01  SAVE-LM-SEQ     PIC 9(3).

       01  MSEL-LIST       PIC X(5)    VALUE "MSEL.".

       01  WK-MEMO         PIC X(60).
       01  WK-AA-MEMO.
           03  FILLER              PIC X(3)  VALUE "ON ".
           03  WK-AA-DATE          PIC 99/99/99.
           03  FILLER              PIC X(5)  VALUE " RE: ".
           03  WK-AA-ACCTNO        PIC 999999/99.
           03  FILLER              PIC X(26) VALUE
               ", INTEREST REBATE ADJ FOR ".
           03  WK-AA-REB-ADJ       PIC $$$$$.99-.
       01  WK-BB-MEMO.
           03  FILLER              PIC X(24) VALUE
               "INTEREST REBATE ADJ FOR ".
           03  WK-BB-REB-ADJ       PIC $$$$$$.99-.
           03  FILLER              PIC X(6) VALUE
               ", RE: ".
           03  WK-BB-ACCTNO        PIC 999999/99.

       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBCL/ORIGRBW.CPY".

       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LP01SP.CPY".
      * MUST MATCH WHATS IN LP/LONPD0.CBL
       01  REBL-WORKERS.
           03  REBL-LN-ACCTNO           PIC 9(8).
           03  REBL-LN-SSNO             PIC 9(9).
           03  REBL-TRANS-DATE          PIC 9(8).
           03  REBL-REBATE-ADJ          PIC S9(7)V99.
           03  REBL-MEMO                PIC X(60).
           03  REBL-BR-NOT-OURS-REFUND  PIC X.

       PROCEDURE DIVISION
            USING FORM-PATHNAME SP-REC REBL-WORKERS 
                  EXIT-PATHNAME.

      /**********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "SEARCHING FOR PRIOR PAIDOFF LOANS....." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 0 TO REBL-REBATE-ADJ.

           MOVE REBL-LN-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE REBL-BR-NOT-OURS-REFUND TO BR-NOT-OURS-REFUND.

      * TEST 1ST MAKER FOR REBATE ADJUSTMENT:
           MOVE 1 TO TAA-SUB.
           PERFORM TAA-TEST-MAKER.
           IF TAA-IO-FG = "YES"
              GO TO PROCESS-REB-ADJUSTMENT.

      * TEST 2ND MAKER FOR REBATE ADJUSTMENT:
           MOVE 2 TO TAA-SUB.
           PERFORM TAA-TEST-MAKER.
           IF TAA-IO-FG = "YES"
              GO TO PROCESS-REB-ADJUSTMENT.

      * TEST 3RD MAKER FOR REBATE ADJUSTMENT:
      *    MOVE 3 TO TAA-SUB.
      *    PERFORM TAA-TEST-MAKER.
      *    IF TAA-IO-FG = "YES"
      *       GO TO PROCESS-REB-ADJUSTMENT.

      * TEST 4TH MAKER FOR REBATE ADJUSTMENT:
      *    MOVE 4 TO TAA-SUB.
      *    PERFORM TAA-TEST-MAKER.
      *    IF TAA-IO-FG = "YES"
      *       GO TO PROCESS-REB-ADJUSTMENT.

           GO TO UPDATE-EXIT.

       PROCESS-REB-ADJUSTMENT.
      * UPDATE OLD LOAN:
           PERFORM OPEN-LN1-FILE.
           MOVE TAH-LN-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE "Y" TO LN-STATE-FLAG.
           PERFORM REWRITE-LN1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      * UPDATE OLD LOAN MEMO:
           MOVE REBL-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO WK-AA-DATE.
           MOVE REBL-LN-ACCTNO  TO WK-AA-ACCTNO.
           INSPECT WK-AA-ACCTNO REPLACING ALL "/" BY "-".
           MOVE REBL-REBATE-ADJ TO WK-AA-REB-ADJ.
           MOVE WK-AA-MEMO      TO WK-MEMO.

           PERFORM UPDATE-LM1.

      * UPDATE NEW LOAN:
           PERFORM OPEN-LN1-FILE.
           MOVE REBL-LN-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE "A" TO LN-STATE-FLAG.

      * 05/20/94 USED TO BE LN-PRLNNO, BUT IF RENEWING A LOAN, WITH A DIFF
      * MAKER, AND THE LOAN RESTRICTED A DIFF MAKER, THIS REALLY MESSED UP
      * LN-PRLNNO FOR VOIDING THE PB !!

           MOVE TAH-LN-ACCTNO   TO LN-OLDACCTNO.
           MOVE REBL-REBATE-ADJ TO LN-STATE-WORKER.

           PERFORM REWRITE-LN1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      * UPDATE NEW LOAN MEMO:
           MOVE TAH-LN-ACCTNO   TO WK-BB-ACCTNO.
           INSPECT WK-BB-ACCTNO REPLACING ALL "/" BY "-".
           MOVE REBL-REBATE-ADJ TO WK-BB-REB-ADJ.
           MOVE WK-BB-MEMO      TO WK-MEMO REBL-MEMO.

           PERFORM UPDATE-LM1.

       UPDATE-EXIT.
           GO TO END-ROUTINE.

      *********************************
      *    TEST MAKER FOR LOANS
      *********************************
       TAA-TEST-MAKER SECTION.
           MOVE " " TO TAA-IO-FG.
           PERFORM OPEN-LN4-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE REBL-LN-SSNO  TO LN-SSNO(TAA-SUB).

           PERFORM TAC-START-LN-FILE.
       TAA-READ-NEXT.
           PERFORM TAD-READ-LN-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO TAA-EXIT.

           IF LN-SSNO(TAA-SUB) NOT = REBL-LN-SSNO
              GO TO TAA-EXIT.

           IF NOT (LN-MAKERCD(TAA-SUB) = "M" OR "E")
              GO TO TAA-READ-NEXT.

           IF LN-LOANTYPE NOT = "P"
              GO TO TAA-READ-NEXT.

           IF LN-CURBAL NOT = 0
              GO TO TAA-READ-NEXT.

           IF LN-SALEFIN NOT = "N"
              GO TO TAA-READ-NEXT.

           IF LN-STATE-FLAG = "Y"
              GO TO TAA-READ-NEXT.

           IF ( LN-POCD-NONLOAN )
              GO TO TAA-READ-NEXT.

      * SEE IF LOAN WAS MADE WITHIN LAST 90 DAYS:
      * OR 120 DAYS IF RBEARLY = 16 , ALABAMA
           MOVE LN-INTDATE      TO NUM-DATE.
           MOVE REBL-TRANS-DATE TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF SP-RBEARLY(7) = 16
              IF ELAPSED-DAYS > 120
                 GO TO TAA-READ-NEXT
              ELSE
                 NEXT SENTENCE
           ELSE
              IF ELAPSED-DAYS > 90
                 GO TO TAA-READ-NEXT.

      * TEST FOR CUSTOMER PAYOFF AND IF SO FIND
      * ORIGINAL INTEREST REBATE:
           MOVE LN-OWNBR  TO TAH-LN-OWNBR.
           MOVE LN-ACCTNO TO TAH-LN-ACCTNO.
           PERFORM TAH-FIND-ORIG-REBATE-AMOUNT.
           IF TAH-IO-FG NOT = "RI"
              IF LN-CONVERCD NOT = " "
                 PERFORM GET-ORIG-REBATE-WINDOW
              ELSE
                 IF TAH-IO-FG = " "
                    GO TO TAA-READ-NEXT.

      * COMPUTE-PRORATA-REBATE:
           MOVE REBL-TRANS-DATE TO REB-PAYDATE.
           MOVE LN-ORGTERM      TO REB-ORGTERM.
           MOVE LN-INTCHG       TO REB-TOTCHG.
           MOVE "PB"            TO REB-LPTRCD.
           MOVE 7               TO REB-SUB.
           PERFORM REBATE.
           IF REB-REBATE > TAH-ORIG-REBATE
              MOVE "YES" TO TAA-IO-FG
              COMPUTE REBL-REBATE-ADJ =
                              REB-REBATE - TAH-ORIG-REBATE.

       TAA-EXIT.
           PERFORM CLOSE-LN4-FILE.

      **********************************************
      *    START LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       TAC-START-LN-FILE SECTION.
           IF TAA-SUB = 1
              MOVE LN-PATH-OWNBR    TO QLN4-WBEG-OWNBR
                                       QLN4-WEND-OWNBR
              MOVE LN-SSNO(TAA-SUB) TO QLN4-WBEG-SSNO1
                                       QLN4-WEND-SSNO1
              PERFORM START-LN4-FILE
           ELSE
              IF TAA-SUB = 2
                 MOVE LN-PATH-OWNBR    TO QLN5-WBEG-OWNBR
                                          QLN5-WEND-OWNBR
                 MOVE LN-SSNO(TAA-SUB) TO QLN5-WBEG-SSNO2
                                          QLN5-WEND-SSNO2
                 PERFORM START-LN5-FILE.
      *       ELSE
      *          IF TAA-SUB = 3
      *             MOVE LN-PATH-OWNBR    TO QLN6-WBEG-OWNBR
      *                                      QLN6-WEND-OWNBR
      *             MOVE LN-SSNO(TAA-SUB) TO QLN6-WBEG-SSNO3
      *                                      QLN6-WEND-SSNO3
      *             PERFORM START-LN6-FILE
      *          ELSE
      *             IF TAA-SUB = 4
      *                MOVE LN-PATH-OWNBR    TO QLN7-WBEG-OWNBR
      *                                         QLN7-WEND-OWNBR
      *                MOVE LN-SSNO(TAA-SUB) TO QLN7-WBEG-SSNO4
      *                                         QLN7-WEND-SSNO4
      *                PERFORM START-LN7-FILE.

      **********************************************
      *    READ-NEXT LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       TAD-READ-LN-FILE-NEXT SECTION.
           IF TAA-SUB = 1
              PERFORM READ-LN4-FILE-NEXT
           ELSE
              IF TAA-SUB = 2
                 PERFORM READ-LN5-FILE-NEXT.
      *       ELSE
      *          IF TAA-SUB = 3
      *             PERFORM READ-LN6-FILE-NEXT
      *          ELSE
      *             IF TAA-SUB = 4
      *                PERFORM READ-LN7-FILE-NEXT.

      **********************************************
      *    FIND ORIGINAL REBATE AMOUNT & HOW PAIDOFF
      *
      *    IF ACCOUNT WAS PAID OFF OTHER THAN
      *    PB, RN OR SC SET REBATE AMOUNT
      **********************************************
       TAH-FIND-ORIG-REBATE-AMOUNT SECTION.
           MOVE " " TO TAH-IO-FG
                       TAH-1ST-LP.
           MOVE 0 TO TAH-ORIG-REBATE.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE TAH-LN-ACCTNO TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       TAH-READ-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = TAH-LN-ACCTNO)
              IF TAH-1ST-LP = "Y"
                 MOVE "YES" TO TAH-IO-FG
                 GO TO TAH-EXIT
              ELSE
                 GO TO TAH-EXIT.

           IF LP-REV = "Y" OR LP-TRCD = "RV"
              GO TO TAH-READ-NEXT.

           IF TAH-1ST-LP = " "
              MOVE "Y" TO TAH-1ST-LP
              IF (LP-CURBAL NOT = 0)
               OR (LP-TRCD = "PB" OR "RN" OR "SC"
                              OR "RB" OR "RO")
                  GO TO TAH-EXIT.

           IF LP-TRCD NOT = "RI"
              GO TO TAH-READ-NEXT.

           MOVE LP-TRAMT TO TAH-ORIG-REBATE.
           MOVE "RI"     TO TAH-IO-FG.

       TAH-EXIT.
           PERFORM CLOSE-LP1-FILE.

      **********************************************
      *    GET WINDOW FOR INPUT OF ORIGINAL REBATE AMOUNT
      *                     & HOW PAIDOFF
      *
      *    IF ACCOUNT WAS PAID OFF OTHER THAN
      *       PB, RN OR SC:
      *                    SET REBATE AMOUNT
      **********************************************
       GET-ORIG-REBATE-WINDOW SECTION.
           MOVE LN-INTCHG TO ORIGRB-INTCHG.
           MOVE LN-OWNBR  TO ORIGRB-OWNBR.
           MOVE LN-ACCTNO TO ORIGRB-ACCTNO.
           MOVE 0         TO ORIGRB-INT-REBATE.

           MOVE "CL/ORIGRB" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ORIGRB-BUF.
           CANCEL FORM-PROGX.

           MOVE ORIGRB-INT-REBATE TO TAH-ORIG-REBATE.
           MOVE "YES" TO TAH-IO-FG.

       GET-ORIG-REBATE-WINDOW-EXIT.
           EXIT.

      ***********************************
       UPDATE-LM1 SECTION.
      ***********************************

           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO LM-BRNO
                                 QLM1-WBEG-BRNO
                                 QLM1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LM-ACCTNO
                                 QLM1-WBEG-ACCTNO
                                 QLM1-WEND-ACCTNO.
           MOVE 0             TO LM-DATE
                                 QLM1-WBEG-DATE-REV.
           MOVE 0             TO LM-SEQ SAVE-LM-SEQ
                                 QLM1-WBEG-SEQ.
           MOVE ALL "9"       TO QLM1-WEND-DATE-REV
                                 QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE.
       UPDATE-LM1-NEXT.
           PERFORM READ-LM1-FILE-NEXT.
           IF IO-FG = 0
              IF (LM-ACCTNO = LN-ACCTNO) AND (LM-BRNO = LM-PATH-OWNBR)
                 IF LM-DATE = 0
                    MOVE LM-SEQ TO SAVE-LM-SEQ
                    GO TO UPDATE-LM1-NEXT.

           MOVE SPACES TO LM-REC.

           MOVE LM-PATH-OWNBR TO LM-BRNO.
           MOVE LN-ACCTNO     TO LM-ACCTNO.
           MOVE 0             TO LM-DATE.
           ADD 1 SAVE-LM-SEQ GIVING LM-SEQ.
           MOVE WK-MEMO       TO LM-MEMO.
           PERFORM WRITE-LM1-FILE.

       EXIT-UPDATE-LM1.
           PERFORM CLOSE-LM1-FILE.

       COPY "LIBGB/RBACT.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPMDTE.CPY".

      *********************************
       MISC-ROUTINES SECTION.
      *********************************
       SETUP-LINK-INFO.
       FORM-RESET.
       VDU-CONVERT-FIELD.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-ROUTINE SECTION.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
