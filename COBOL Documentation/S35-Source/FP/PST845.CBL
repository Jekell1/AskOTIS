       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST845.
       AUTHOR.        AMM.
       DATE-WRITTEN.  08-05-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST845.C
      *
      * DESCRIPTION   :  ONLINE DISCLOSURE
      *
      * STATE         :  ALABAMA    (AL)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0805 PR#1449 ***NEW*** CALLED FROM PRT01W
      * AMM 2020.1007 PR#1464 FIXED BORROWER PRINTING ON PAGE 2
      * BAH 2022.0503 START TRCFILE WITH LOANDATE LIKE A15, NOT TRANSDATE
      * BAH 20220901 HARD CODED START
      * BAH 2023.0802 ADDED CLOSING FEE AND RULE 78S DESCRIPTION #1619
      * BAH 2024.0104 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0521 CHANGED D01-ACCTNO AND D06-NUMBER BACK TO Z(5)9 S35Q-32
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "LIBFP/FPFSPST.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBFP/FPFDPST.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FP/PST845.CBL S35 05/02/22-14:36:52 >".

       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBFP/FPWSPST.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTD.CPY".
       COPY "LIBLP/LP01LTD_SQL.CPY".
       COPY "LIBLP/LPWSLTD.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01PC.CPY".
       COPY "LIBLP/LP01PC_SQL.CPY".
       COPY "LIBLP/LPWSPC.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01WK.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  PROC-WRK        PIC S9(7)V99 COMP-3 VALUE 0.
       01  PROCEED-TOT     PIC S9(7)V99 COMP-3 VALUE 0.
       01  ME-AMT          PIC S9(5)V99 COMP-3 VALUE 0.
       01  COMPANY-NAME    PIC X(40).
       01  JUSTIFY-TYPE    PIC X VALUE "L".
       01  CHECK-COUNT     PIC 99 VALUE 0.
       01  REC-FEE         PIC S9(7)V99 COMP-3 VALUE 0.
       01  C-S-Z           PIC X(85).

       01  TEXT4.
           03  T4-TABLE OCCURS 15 TIMES.
               05  T4-AMT      PIC S9(6)V99 COMP-3.
               05  T4-DESC     PIC X(21).

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                      PIC X(300)     VALUE SPACES.

      *=================================================================
      *
      *     D E T A I L - W O R L D - A C C E P T A N C E  ( W A C )
      *
      *-----------------------------------------------------------------
      *    CPI     : 12 CPI   [PIC X(96)]
      *    CUSTOMER: WORLD ACCEPTANCE
      *    FMNO    : 119
      *    STATE   : ALABAMA    (AL)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(13).
           03  D01-MAKER                    PIC X(27).
           03  FILLER                       PIC X(05).
           03  D01-ACCTNO                   PIC Z(5)9.
           03  FILLER                       PIC X(12).
           03  D01-LOANDATE                 PIC 99/99/99.
           03  FILLER                       PIC X(12).
           03  D01-BR-NAME                  PIC X(40).
           03  FILLER                       REDEFINES D01-BR-NAME.
               05  D01-BR-PHONE             PIC 999/999/9999.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(27).
           03  D02-APR                      PIC ZZ9.99       BLANK ZERO.
           03  FILLER                       PIC X(19).
           03  D02-FINCHG                   PIC ZZZ,ZZ9.99   BLANK ZERO.
           03  FILLER                       PIC X(16).
           03  D02-FINAMT                   PIC ZZZ,ZZ9.99   BLANK ZERO.
           03  FILLER                       PIC X(18).
           03  D02-TOTAL                    PIC ZZZ,ZZ9.99   BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(15).
           03  D03-ORGTERM                  PIC ZZZ.
           03  FILLER                       PIC X(20).
           03  D03-REGPYAMT                 PIC $$$$$$.99.
           03  FILLER                       PIC X(27).
           03  D03-1STPYDATE                PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(10).
           03  D04-SEE-NEXT                 PIC X(15).
           03  FILLER                       PIC X(52).
           03  D04-MDTE-DATE                PIC 99/99/99.
           03  D04-BR-CHECK                 PIC X(28).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(23).
           03  D05-AMOUNT                   PIC ZZZ,ZZ9.99.
           03  D05-MORE-CHECKS              PIC X.
           03  FILLER                       PIC X(07).
           03  D05-CHECK                    PIC X(75).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(22).
           03  D06-BORROWER                 PIC X(26).
           03  FILLER                       PIC X(10).
           03  D06-NUMBER                   PIC Z(6).
           03  FILLER                       PIC X(24).
           03  D06-LOANDATE                 PIC 99/99/99.
           03  FILLER                       PIC X(13).
           03  D06-TIME                     PIC 99/99/99     BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(80).
           03  D07-CLOSED-ADR               PIC X(40).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-08                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(04).
           03  D08-AMT-PAID                 PIC X(29).
           03  FILLER                       PIC X(01).
           03  D08-CHECK-PAYEE              PIC X(20).
           03  FILLER                       PIC X(02).
           03  D08-CHECK-AMT                PIC ZZZ,ZZ9.99 BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-09                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(04).
           03  D09-TOTAL-PRO                PIC X(50).
           03  FILLER                       PIC X(01).
           03  D09-DOLLAR                   PIC X(01).
           03  D09-PROCEEDS                 PIC ZZZ,ZZ9.99 BLANK ZERO.

       COPY "LIBFP/PST_COPYW.CPY".
       COPY "LIBFP/PST_INFOW.CPY".
       COPY "LIBFP/PST_FORMW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBFP/PST_SCREENW.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME PROG-BUF IP-BUF PRU-PATH.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PST-FILE MANIFEST-FILE.

       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-PC1-FILE.
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-FM2-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PST-FILE MANIFEST-FILE.

       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM PST-FORM-MAIN-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    P R I N T - R O U T I N E
      *
      ******************************************************************
       PRINT-ROUTINE SECTION.

           MOVE 02     TO PST-WS-FORM-PAGE-MAX.
           PERFORM PST-FR-FORM-INITIALIZATION.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ LOAD ACCOUNT INFO. }
           PERFORM PST-INFO-MODULE.

      *- - - - - - - - - - - - - - - - - - -{ NUMBER OF CHECKS ON FORM }
           MOVE ZEROES TO CHECK-MAX-CUSTOMIZE.
           PERFORM PST-INFO-CUSTOMIZE-CHECKS.

      *- - - - - - - - - - - - - - - - - - - - -{ SETUP PAGE COPY LOOP }
           MOVE ZEROES TO PAGE-COPY-ELE.
           MOVE 01     TO PAGE-COPY-MAX.

           PERFORM GET-FEES.

      * CHECK FOR NETSPEND DEBIT CARD PROCEEDS
           IF BR-USE-DEBIT-CARDS = "Y"
              PERFORM CHECK-FOR-DEBIT-CARD.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 15
              MOVE 0 TO T4-AMT(SUB)
              MOVE SPACES TO T4-DESC(SUB)
           END-PERFORM.

           MOVE 0 TO CHECK-COUNT.
           MOVE 1 TO SUB.
           PERFORM MANUAL-CHECKS-DISBURSEMENT.
           IF PROCEED-TOT = 0
              PERFORM OPEN-CX1-FILE
              MOVE LN-ORGBR TO CX-BRNO
                               QCX1-WBEG-BRNO
                               QCX1-WEND-BRNO
              MOVE "LN"     TO CX-TYPE
                               QCX1-WBEG-TYPE
                               QCX1-WEND-TYPE
              MOVE 0        TO CX-SEQNO
                               QCX1-WBEG-SEQNO
              MOVE ALL "9"  TO QCX1-WEND-SEQNO
              PERFORM START-CX1-FILE
              PERFORM SYSTEM-CHECKS-DISBURSEMENT.

       PRINT-ROUTINE-PAGE-COPY.

           ADD 01 TO PAGE-COPY-ELE.

           IF ( PAGE-COPY-ELE > PAGE-COPY-MAX )
              GO TO PRINT-ROUTINE-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ PRINT PAGES  }
           PERFORM PRINT-PAGE-01-ROUTINE.
           PERFORM PRINT-PAGE-02-ROUTINE.
           IF CHECK-COUNT > 1
              IF IP-FMNO NOT = 118
                 PERFORM PRINT-PAGE-03-ROUTINE.

           GO TO PRINT-ROUTINE-PAGE-COPY.

       PRINT-ROUTINE-END.

           PERFORM PRINT-PST-FILE.

       PRINT-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-01-ROUTINE SECTION.
      ******************************************************************

           MOVE 01 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-01-ROUTINE-EXIT.

      * CHANGE FONT SIZE
           MOVE 08                     TO PST-WS-FONT-SIZE.                     
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.                     
           PERFORM PST-FR-FONT.                                                 
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.             
           PERFORM PST-FR-LINE-SPACING.                                         
                                                                                
      * 118 BLANK
           IF IP-FMNO NOT = 118
              MOVE LN-ACCTNO                   TO D01-ACCTNO
              MOVE TRANS-DATE                  TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D01-LOANDATE
              MOVE BR-STMNT1                   TO D01-BR-NAME
              IF JOINT-NAME-FIRST-LAST NOT = SPACES
                STRING MAKER-NAME-FIRST-LAST, " & "
                   DELIMITED BY "  " INTO D01-MAKER
              ELSE
                MOVE MAKER-NAME-FIRST-LAST     TO D01-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 118
              MOVE BR-PHONE                    TO D01-BR-PHONE
              INSPECT D01-BR-PHONE REPLACING ALL "/" BY "-"
              IF JOINT-NAME-FIRST-LAST NOT = SPACES
                 MOVE JOINT-NAME-FIRST-LAST TO D01-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 118
              COMPUTE D02-APR ROUNDED = LN-APRATE * 1
              MOVE LN-FINCHG                   TO D02-FINCHG
              MOVE DISCLOSED-FINAMT            TO D02-FINAMT
              MOVE TOTAL-REPAY                 TO D02-TOTAL.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 118
              MOVE LN-1STPYDATE                TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D03-1STPYDATE 
              IF LN-1STPYAMT NOT = LN-REGPYAMT
                 MOVE 1                        TO D03-ORGTERM
                 MOVE LN-1STPYAMT              TO D03-REGPYAMT
              ELSE
                 MOVE LN-REGPYAMT              TO D03-REGPYAMT
                 IF LN-LASTPYAMT NOT = LN-REGPYAMT
                    COMPUTE D03-ORGTERM = LN-ORGTERM - 1
                 ELSE
                    MOVE LN-ORGTERM            TO D03-ORGTERM.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 118
              IF LN-1STPYAMT NOT = LN-REGPYAMT
                 COMPUTE D03-ORGTERM = LN-ORGTERM - 1
                 MOVE LN-REGPYAMT              TO D03-REGPYAMT
              ELSE
                 IF LN-LASTPYAMT NOT = LN-REGPYAMT
                    MOVE 1                     TO D03-ORGTERM
                    MOVE LN-LASTPYAMT          TO D03-REGPYAMT.
           IF NOT ( IP-FMNO = 118)
              MOVE MDTE-DATE                   TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D04-MDTE-DATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ACQUISITION CHARGE 
           IF IP-FMNO NOT = 118
              COMPUTE D05-AMOUNT = LN-SERCHG - LN-POINTS.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES                         TO WORDS-INPUT.
           MOVE BR-STMNT1                      TO WORDS-INPUT.
           MOVE 02 TO WORDS-WRAP-LINES.
           MOVE 28 TO WORDS-WRAP-LINE-SIZE(01).
           MOVE 25 TO WORDS-WRAP-LINE-SIZE(02).
           PERFORM WORDS-ROUTINE.
           MOVE WORDS-WRAP-LINE(01)            TO D04-BR-CHECK.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INSTALLMENT ACCOUNT HANDLING CHARGE
           IF IP-FMNO NOT = 118
              COMPUTE D05-AMOUNT = LN-INTCHG + LN-EXTCHG.
           MOVE GB-COMPANY-NAME TO COMPANY-NAME.
           CALL "C$JUSTIFY" USING COMPANY-NAME, JUSTIFY-TYPE.
           MOVE SPACES TO D05-CHECK.
           STRING WORDS-WRAP-LINE(02), ", DBA ",COMPANY-NAME
              DELIMITED BY "  " INTO D05-CHECK.
           CALL "C$JUSTIFY" USING D05-CHECK, "C".
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CLOSING FEE
           IF IP-FMNO NOT = 118
              MOVE LN-POINTS TO D05-AMOUNT.
           MOVE SPACES TO D05-CHECK.
           STRING BR-ADR1, ", ", BR-CITY, ", ", BR-STATE, " ", BR-ZIP
             DELIMITED BY "  " INTO D05-CHECK.
           CALL "C$JUSTIFY" USING D05-CHECK, "C".
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 1. AMOUNT PAID TO YOU OR ON YOUR BEHALF
           IF IP-FMNO NOT = 118
              MOVE LN-PROCEEDS                 TO D05-AMOUNT
              IF CHECK-COUNT > 1
                 MOVE "*" TO D05-MORE-CHECKS.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 118
              IF CHECK-COUNT > 1
                 MOVE "* SEE NEXT PAGE" TO D04-SEE-NEXT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 2. AMOUNT PAID ON YOUR PRIOR ACCOUNT
           IF IP-FMNO NOT = 118
              COMPUTE D05-AMOUNT = LN-PRBAL + LN-PRINTDUE
                                 - LN-PRFINCHGRB - LN-PRINSRB.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 3. RECORDING FEES
           IF IP-FMNO NOT = 118
              MOVE REC-FEE                     TO D05-AMOUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AMOUNT FINANCED (SUM OF LINES 1 AND 2)
           IF IP-FMNO NOT = 118
              MOVE DISCLOSED-FINAMT            TO D05-AMOUNT.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-01-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-02-ROUTINE SECTION.
      ******************************************************************

           MOVE 02 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-02-ROUTINE-EXIT.

      * CHANGE FONT SIZE 
           MOVE 08                     TO PST-WS-FONT-SIZE.                     
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.                     
           PERFORM PST-FR-FONT.                                                 
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.             
           PERFORM PST-FR-LINE-SPACING.                                         
                                                                                
      * 118 BLANK
           IF IP-FMNO NOT = 118
              MOVE LN-ACCTNO                   TO D06-NUMBER
              MOVE LN-LOANDATE                 TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D06-LOANDATE
              PERFORM GET-POSTTIME
              MOVE LP-POSTTIME                 TO D06-TIME
              INSPECT D06-TIME REPLACING ALL "/" BY ":"
              IF JOINT-NAME-FIRST-LAST NOT = SPACES
                 STRING MAKER-NAME-FIRST-LAST, " &"
                    DELIMITED BY "  " INTO D06-BORROWER
              ELSE
                 MOVE MAKER-NAME-FIRST-LAST TO D06-BORROWER.
           MOVE 28 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 118
              IF JOINT-NAME-FIRST-LAST NOT = SPACES
                 MOVE JOINT-NAME-FIRST-LAST TO D06-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 118
              MOVE MAKER-ADDRESS               TO D07-CLOSED-ADR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 118
              MOVE MAKER-CSZ                   TO D07-CLOSED-ADR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-02-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-03-ROUTINE SECTION.
      ******************************************************************

           MOVE 03 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-03-ROUTINE-EXIT.

      * CHANGE FONT SIZE 
           MOVE 08                     TO PST-WS-FONT-SIZE.                     
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.                     
           PERFORM PST-FR-FONT.                                                 
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.             
           PERFORM PST-FR-LINE-SPACING.                                         
                                                                                
           MOVE MAKER-NAME-FIRST-LAST       TO D01-MAKER.
           MOVE LN-ACCTNO                   TO D01-ACCTNO.
           MOVE TRANS-DATE                  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                 TO D01-LOANDATE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE JOINT-NAME-FIRST-LAST       TO D01-MAKER.
           MOVE BR-PHONE                    TO D01-BR-PHONE.
           INSPECT D01-BR-PHONE REPLACING ALL "/" BY "-".
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO SUB.
       NEXT-CHECK.
           MOVE "AMOUNT PAIUD ON YOUR BEHALF TO" TO D08-AMT-PAID.
           MOVE "$"                              TO D09-DOLLAR.
           MOVE T4-DESC(SUB)                     TO D08-CHECK-PAYEE.
           MOVE T4-AMT(SUB)                      TO D08-CHECK-AMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO SUB.
           IF SUB NOT > 15
              IF T4-AMT(SUB) NOT = 0
                 GO TO NEXT-CHECK.

           MOVE "TOTAL PROCEEDS DISBURSED.........................."
                            TO D09-TOTAL-PRO.
           MOVE "$"         TO D09-DOLLAR.
           MOVE LN-PROCEEDS TO D09-PROCEEDS.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-03-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      *******************************
       GET-POSTTIME SECTION.
      *******************************
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 1         TO LP-SEQNO.
           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           IF IO-FG = 9
              GO TO EXIT-GET-POSTTIME.

       EXIT-GET-POSTTIME.
           EXIT.

      *********************************************
       CHECK-FOR-DEBIT-CARD SECTION.
      *********************************************
           PERFORM OPEN-LTD1-FILE.
           MOVE LN-OWNBR  TO LTD-BRNO.
           MOVE LN-ACCTNO TO LTD-ACCTNO.
           MOVE 1         TO LTD-SEQNO.
           PERFORM READ-LTD1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-CHECK-FOR-DEBIT-CARD.

           ADD LTD-DEBIT-CARD-AMT TO ME-AMT.
       EXIT-CHECK-FOR-DEBIT-CARD.
           PERFORM CLOSE-LTD1-FILE.

      *******************************
       GET-FEES SECTION.
      *******************************
           MOVE 0 TO REC-FEE.
           MOVE 1 TO SUB.
       NEXT-LN-FEE.
           IF SUB > 10
              GO TO EXIT-FIND-FEE.

           IF LN-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-LN-FEE.

           IF LN-FEECODE(SUB) = "95"
              ADD LN-FEEAMT(SUB) TO REC-FEE.

           ADD 1 TO SUB.
           GO TO NEXT-LN-FEE.
       EXIT-FIND-FEE.
           EXIT.
 
      *********************************************
       MANUAL-CHECKS-DISBURSEMENT SECTION.
      *********************************************

           PERFORM OPEN-TRC1-FILE.
           PERFORM OPEN-CK1-FILE.

           MOVE LN-ORGBR    TO TC-BRNO
                               QTRC1-WBEG-BRNO
                               QTRC1-WEND-BRNO.
           MOVE LN-LOANDATE TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE
                                       QTRC1-WEND-KEY-DATE.
           MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
                                       QTRC1-WBEG-CKNO
                                       QTRC1-WBEG-CLASS
                                       QTRC1-WBEG-SEQ.
           MOVE ALL "9"             TO QTRC1-WEND-CKNO
                                       QTRC1-WEND-CLASS
                                       QTRC1-WEND-SEQ.

           PERFORM START-TRC1-FILE.
       NEXT-MAN-CHECK.
           PERFORM READ-TRC1-FILE-NEXT.
           IF (IO-FG = 9) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              IF (SUB < 2) 
                 GO TO END-MAN-CHECKS-DISBURSEMENT.

           IF (IO-FG = 9) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO REMAINING-MAN-DISBURSEMENT.

      * STARTS WITH BRNO AND DATE, IF DATE DOESNT MATCH, YOU DIDNT FIND ANY
           IF TC-KEY-DATE NOT = LN-LOANDATE
              GO TO REMAINING-MAN-DISBURSEMENT.

           IF TC-ACCTNO NOT = LN-ACCTNO
              GO TO NEXT-MAN-CHECK.

      * MUST CHECK IF IT HAS BEEN VOIDED !!
      * THEY COULD HAVE PRINTED, VOIDED, AND RE-ISSUED
           MOVE TC-BRNO TO CK-BRNO.
           MOVE TC-CKNO TO CK-NO.
           PERFORM READ-CK1-FILE.
           IF IO-FG NOT = 0
              GO TO NEXT-MAN-CHECK
           ELSE
              IF CK-VOID = "Y" OR "R"
                 GO TO NEXT-MAN-CHECK.

           IF ( TC-PAYEE = C-S-Z )
              ADD  TC-AMT   TO ME-AMT
              ADD TC-AMT TO PROCEED-TOT
              GO TO NEXT-MAN-CHECK.

           MOVE TC-PAYEE TO T4-DESC(SUB).
           MOVE TC-AMT   TO T4-AMT(SUB).
           MOVE TC-AMT   TO PROCEED-TOT.
           ADD 1 TO CHECK-COUNT.

           IF SUB < 15
              ADD 1 TO SUB
           ELSE
              ADD TC-AMT TO PROCEED-TOT
              ADD TC-AMT TO PROC-WRK.

           GO TO NEXT-MAN-CHECK.

       REMAINING-MAN-DISBURSEMENT.

           IF PROC-WRK NOT = 0
              IF PROC-WRK NOT = T4-AMT(SUB)
                 MOVE "OTHER"      TO T4-DESC(SUB)
                 MOVE PROC-WRK     TO T4-AMT(SUB).

       END-MAN-CHECKS-DISBURSEMENT.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-CK1-FILE.

       EXIT-MAN-CHECKS-DISBURSEMENT.
           EXIT.

      *************************************
       SYSTEM-CHECKS-DISBURSEMENT SECTION.
      *************************************
           MOVE 0 TO PROCEED-TOT.

       NEXT-SYS-CHECK.
           PERFORM READ-CX1-FILE-NEXT.
           IF IO-FG = 9 OR (CX-BRNO NOT = LN-ORGBR)      
              IF SUB < 2
                 GO TO EXIT-SYS-NEXT-DISBURSEMENT
              ELSE
                 GO TO REMAINING-SYS-DISBURSEMENT.

           IF ( CX-REF NOT = LN-ACCTNO )
              GO TO NEXT-SYS-CHECK.

           IF ( CX-BWPROC = "Y" )
              ADD CX-AMT TO ME-AMT
              GO TO NEXT-SYS-CHECK.

           MOVE CX-PAYEE TO T4-DESC(SUB).
           MOVE CX-AMT   TO T4-AMT(SUB).
           ADD 1 TO CHECK-COUNT.

           IF SUB < 15
              ADD 1 TO SUB
           ELSE
              ADD CX-AMT TO PROCEED-TOT.

           GO TO NEXT-SYS-CHECK.

       REMAINING-SYS-DISBURSEMENT.

           IF PROCEED-TOT NOT = 0
              IF PROCEED-TOT NOT = T4-AMT(SUB)
                 MOVE "OTHER "    TO T4-DESC(SUB)
                 MOVE PROCEED-TOT TO T4-AMT (SUB).

       EXIT-SYS-NEXT-DISBURSEMENT.
           EXIT.
 
      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPPCGS_SQL.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBFP/FPIOPST.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCK1RN.CPY".
       COPY "LIBLP/LPCX1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPFM2RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTD1RN.CPY".
       COPY "LIBLP/LPPC1RN.CPY".
       COPY "LIBLP/LPPD1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPTRC1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.
           PERFORM PRU-RESET.

       EXIT-PROG.

       STOP-RUN.
           EXIT PROGRAM.
