       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST830.
       AUTHOR.        AMM.
       DATE-WRITTEN.  01-23-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST830.C
      *
      * DESCRIPTION   :  LOAN AGREEMENT AND DISCLOSURE
      *
      * STATE         :  IDAHO    (ID)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0123 PR#1413 ***NEW*** CALLED FROM PRT04Z
      * AMM 2020.0218 PR#1413A CHANGED 'THE' TO 'THIS' IN CRYSTAL PAGE 2
      * BAH 2020.0713 NET BALANCE WAS ADDING LN-PRINSRB INSTEAD OF SUBTRACTING
      *               #1448
      * AMM 2020.1030 PR#1471 REMOVE DUPLEX CODE
      * AMM 2020.1202 PR#1476 ADDED INSURANCE LINES
      * BAH 2022.0503 START TRCFILE WITH LOANDATE LIKE A15, NOT TRANSDATE
      * BAH 2022.0901 HARD CODED START
      * BAH 2024.0108 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0521 CHANGED D01-ACCTNO BACK TO Z(5)9 S35Q-32
      * BAH 2024.1217 COMPLETE SECURITY LINE SAME AS "PP" FOR "SC" AND SAME AS
      *               "AP" FOR "AS", REMOVED TERM CHECK #1695 S35Q-154
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
           "@(#)FP/PST830.CBL S35 05/06/22-14:12:28 >".

       COPY "LIBFP/FPWSPST.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
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
       01  C-S-Z           PIC X(85).
       01  ADD-ON          PIC X(25).
       01  ME-AMT          PIC S9(5)V99 COMP-3 VALUE 0.
       01  PROC-WRK        PIC S9(7)V99 COMP-3 VALUE 0.
       01  PROCEED-TOT     PIC S9(7)V99 COMP-3 VALUE 0.
       01  CDF-SUB         PIC 9(3)            VALUE ZEROES.
       01  CDF-SUB-MAX     PIC 9(3)            VALUE ZEROES.
       01  CDF-AMT         PIC S9(6)V99 COMP-3  OCCURS 2.
       01  CDF-DESC        PIC X(20)            OCCURS 2.
       01  OTHER-FEE       PIC S9(5)V99 COMP-3 VALUE 0.
       01  MOTOR-CLUB      PIC S9(5)V99 COMP-3 VALUE 0.

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
      *    FMNO    : 134
      *    STATE   : IDAHO   (ID)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D01-ACCTNO                   PIC Z(5)9.
           03  FILLER                       PIC X(40).
           03  D01-LOANDATE                 PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(17).
           03  D02-CREDITOR                 PIC X(36).
           03  FILLER                       PIC X(11).
           03  D02-BORROWER                 PIC X(31).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(16).
           03  D03-APR                      PIC ZZ9.99     BLANK ZERO.
           03  FILLER                       PIC X(09).
           03  D03-FINCHG                   PIC Z(6).99    BLANK ZERO.
           03  FILLER                       PIC X(11).
           03  D03-FINAMT                   PIC Z(6).99    BLANK ZERO.
           03  FILLER                       PIC X(12).
           03  D03-TOTAL                    PIC Z(6).99    BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(12).
           03  D04-NOREGPY                  PIC ZZ9          BLANK ZERO.
           03  FILLER                       PIC X(12).
           03  D04-REGPYAMT                 PIC $$$$$.99     BLANK ZERO.
           03  FILLER                       PIC X(19).
           03  D04-BEGIN-DATE               PIC 99/99/99.
           03  FILLER                       PIC X(02).
           03  D04-FINAL-PAY                PIC $$$$$.99     BLANK ZERO.
           03  FILLER                       PIC X(07).
           03  D04-FINAL-DATE               PIC 99/99/99.
           03  D04-PERIOD                   PIC X.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(36).
           03  D05-LCMIN-1                  PIC $$$$.99.
           03  FILLER                       PIC X(11).
           03  D05-LCMIN-2                  PIC $$$$.99.
           03  FILLER                       PIC X(16).
           03  D05-COLLATERAL               PIC X(38).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(28).
           03  D06-AMOUNT-1                 PIC ZZZZ.99.
           03  FILLER                       PIC X(40).
           03  D06-AMOUNT-2                 PIC ZZZZ.99.
           03  FILLER                       REDEFINES D06-AMOUNT-2.
               05  D06-PRIOR                PIC Z(6).

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
           USE AFTER ERROR PROCEDURE ON PST-FILE MANIFEST-FILE.

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM GET-FEES.

           MOVE 1           TO CDF-SUB.
           MOVE 2           TO CDF-SUB-MAX.
           MOVE 0           TO CDF-AMT(1) CDF-AMT(2).
           MOVE SPACES      TO CDF-DESC(1) CDF-DESC(2).

           MOVE LN-PROCASH  TO ME-AMT.
           MOVE 0           TO PROCEED-TOT PROC-WRK.

      * CHECK FOR NETSPEND DEBIT CARD PROCEEDS
           IF BR-USE-DEBIT-CARDS = "Y"
              PERFORM CHECK-FOR-DEBIT-CARD.

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
           IF ( IP-FMNO = 135 )
              PERFORM PRINT-PAGE-BLANK-ROUTINE
           ELSE
              PERFORM PRINT-PAGE-01-ROUTINE.
           PERFORM PRINT-PAGE-02-ROUTINE.

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

      * CHANGE FONT SIZE TO 12 CPI 
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           MOVE LN-ACCTNO                      TO D01-ACCTNO.
           MOVE LN-LOANDATE                    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D01-LOANDATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-STMNT1                      TO D02-CREDITOR.
           MOVE MAKER-NAME-FIRST-LAST          TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR1                        TO D02-CREDITOR.
           MOVE JOINT-NAME-FIRST-LAST          TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( BR-ADR2 NOT = SPACES )
              MOVE BR-ADR2                     TO D02-CREDITOR
           ELSE
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           MOVE MAKER-ADDRESS-1                TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( BR-ADR2 NOT = SPACES )
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF ( MAKER-ADDRESS-2 NOT = SPACES )
              MOVE MAKER-ADDRESS-2   TO D02-BORROWER
           ELSE
              MOVE MAKER-CSZ         TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( MAKER-ADDRESS-2 NOT = SPACES )
              MOVE MAKER-CSZ         TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           COMPUTE D03-APR ROUNDED = LN-APRATE * 1.
           MOVE LN-FINCHG                      TO D03-FINCHG.
           MOVE DISCLOSED-FINAMT               TO D03-FINAMT.
           MOVE TOTAL-REPAY                    TO D03-TOTAL.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-1STPYDATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D04-BEGIN-DATE.
           IF LN-1STPYAMT NOT = LN-REGPYAMT
              MOVE 1                TO D04-NOREGPY
              MOVE LN-1STPYAMT      TO D04-REGPYAMT
           ELSE
              MOVE LN-REGPYAMT      TO D04-REGPYAMT
              IF LN-LASTPYAMT NOT = LN-REGPYAMT
                 COMPUTE D04-NOREGPY = LN-ORGTERM - 1
              ELSE
                 MOVE LN-ORGTERM    TO D04-NOREGPY.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-1STPYAMT NOT = LN-REGPYAMT
              COMPUTE D04-NOREGPY = LN-ORGTERM - 1
              MOVE LN-REGPYAMT      TO D04-REGPYAMT
           ELSE
              IF LN-LASTPYAMT NOT = LN-REGPYAMT
                 MOVE 1             TO D04-NOREGPY
                 MOVE LN-LASTPYAMT  TO D04-REGPYAMT.
           MOVE LN-LASTPYAMT        TO D04-FINAL-PAY.
           MOVE MDTE-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D04-FINAL-DATE.
           MOVE "."                 TO D04-PERIOD.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CHANGE FONT SIZE TO 14 CPI 
           MOVE 08                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * COLLATERAL 
           MOVE LN-COLLCD TO CD-CODE.
           MOVE "CO" TO CD-TYPE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO CD-DESC.
           IF LN-COLLCD = "PP" OR "SC"
              MOVE "PERSONAL PROPERTY-SEE EXHIBIT A"
                                  TO D05-COLLATERAL
           ELSE
              IF LN-COLLCD = "AP" OR "AS"
              MOVE "AUTO & PERSONAL PROPERTY-SEE EXHIBIT A"
                                  TO D05-COLLATERAL
           ELSE
              IF LN-COLLCD = "AU"
                 MOVE "AUTO - SEE EXHIBIT A" TO D05-COLLATERAL
           ELSE
                MOVE CD-DESC TO D05-COLLATERAL.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CHANGE FONT SIZE TO 12 CPI
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * LATE CHARGE
           MOVE SP-LCMINCHG                    TO D05-LCMIN-2.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PREVIOUS ACCT
           MOVE PRIOR-ACCTNO-NUMBER            TO D06-PRIOR.
           MOVE 10 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INTEREST - PREVIOUS BALANCE
           COMPUTE D06-AMOUNT-1 = LN-INTCHG + LN-EXTCHG.
           COMPUTE D06-AMOUNT-2 = LN-PRBAL + LN-PRINTDUE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LESS REFUND
           COMPUTE D06-AMOUNT-2 = LN-PRFINCHGRB + LN-PRINSRB.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ORIGINATION CHARGE - NET BALANCE 
           MOVE LN-SERCHG                      TO D06-AMOUNT-1.
           COMPUTE D06-AMOUNT-2 = LN-PRBAL + LN-PRINTDUE
                            - LN-PRFINCHGRB - LN-PRINSRB.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * OFFICIAL FEES
           MOVE OTHER-FEE                      TO D06-AMOUNT-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * FINANCE CHARGE - UNITED MOTOR CLUB 
           MOVE LN-FINCHG                      TO D06-AMOUNT-1.
           MOVE MOTOR-CLUB                     TO D06-AMOUNT-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CREDIT LIFE
           MOVE LN-INSPREM(1)                  TO D06-AMOUNT-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CREDIT DISABILITY
           MOVE LN-INSPREM(2)                  TO D06-AMOUNT-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INVOLUNTARY UNEMPLOYMENT 
           MOVE LN-INSPREM(4)                  TO D06-AMOUNT-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PROPERTY
           MOVE LN-INSPREM(3)                  TO D06-AMOUNT-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * COLLATERAL PROTECTION
           MOVE LN-INSPREM(6)                  TO D06-AMOUNT-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AMOUNT PAID TO YOU
           MOVE ME-AMT                         TO D06-AMOUNT-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AMOUNT TO OTHERS
           COMPUTE D06-AMOUNT-2 = LN-PROCEEDS - ME-AMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AMOUNT FINANCED
           MOVE DISCLOSED-FINAMT               TO D06-AMOUNT-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LATE CHARGE
           MOVE SP-LCMINCHG                    TO D05-LCMIN-1.
           MOVE 08 TO LN-FEED.
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

      * CHANGE FONT SIZE TO 12 CPI 
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           MOVE LN-ACCTNO                      TO D01-ACCTNO.
           MOVE LN-LOANDATE                    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D01-LOANDATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-02-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-BLANK-ROUTINE SECTION.
      ******************************************************************

           MOVE 01 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-02-ROUTINE-EXIT.

      * CHANGE FONT SIZE TO 12 CPI 
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           MOVE BR-STMNT1                      TO D02-CREDITOR.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR1                        TO D02-CREDITOR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( BR-ADR2 NOT = SPACES )
              MOVE BR-ADR2                     TO D02-CREDITOR
           ELSE
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( BR-ADR2 NOT = SPACES )
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-BLANK-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

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

      *************************************
       MANUAL-CHECKS-DISBURSEMENT SECTION.
      *************************************
           PERFORM OPEN-BW1-FILE.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
             MOVE " " TO BW-FNAME
             MOVE "NOT FOUND" TO BW-LNAME.

           MOVE BW-FNAME TO C-S-Z.
           MOVE BW-LNAME TO ADD-ON.
           INSPECT C-S-Z REPLACING FIRST "  " BY " +".
           INSPECT C-S-Z REPLACING FIRST
              "+                        " BY ADD-ON.

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
           IF ( IO-FG = 9 ) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
      * THIS MEANS THERE IS ROAD GARD BUT NOT CHECKS
              IF CDF-SUB NOT > CDF-SUB-MAX
                 GO TO END-MAN-CHECKS-DISBURSEMENT
              ELSE
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
              ADD TC-AMT TO ME-AMT
              ADD TC-AMT TO PROCEED-TOT
              GO TO NEXT-MAN-CHECK.

           MOVE TC-PAYEE TO CDF-DESC(CDF-SUB).
           MOVE TC-AMT   TO CDF-AMT(CDF-SUB).
           MOVE TC-AMT   TO PROCEED-TOT.

           IF CDF-SUB < CDF-SUB-MAX 
              ADD 1      TO CDF-SUB
           ELSE
              ADD TC-AMT TO PROCEED-TOT
              ADD TC-AMT TO PROC-WRK.

           GO TO NEXT-MAN-CHECK.

       REMAINING-MAN-DISBURSEMENT.
           IF ( PROC-WRK NOT = ZEROES )
              IF PROC-WRK NOT = CDF-AMT(CDF-SUB)
                 MOVE "OTHER"      TO CDF-DESC(CDF-SUB)
                 MOVE PROC-WRK     TO CDF-AMT(CDF-SUB).

       END-MAN-CHECKS-DISBURSEMENT.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-CK1-FILE.

       EXIT-MAN-CHECKS-DISBURSEMENT.
           EXIT.

      *********************************
       SYSTEM-CHECKS-DISBURSEMENT SECTION.
      *********************************
       NEXT-SYS-CHECK.
           PERFORM READ-CX1-FILE-NEXT.
           IF IO-BAD  OR (CX-BRNO NOT = LN-ORGBR)
              IF CDF-SUB NOT > CDF-SUB-MAX
                 GO TO EXIT-SYS-NEXT-DISBURSEMENT
              ELSE
                 GO TO REMAINING-SYS-DISBURSEMENT.

           IF CX-REF NOT = LN-ACCTNO
              GO TO NEXT-SYS-CHECK.

           IF CX-BWPROC = "Y"
              ADD CX-AMT TO ME-AMT
              GO TO NEXT-SYS-CHECK.

      * CHECK TO OTHERS
           MOVE CX-PAYEE TO CDF-DESC(CDF-SUB).
           MOVE CX-AMT   TO CDF-AMT(CDF-SUB).

           IF CDF-SUB < CDF-SUB-MAX
              ADD 1 TO CDF-SUB
           ELSE
              ADD CX-AMT TO PROCEED-TOT.

           GO TO NEXT-SYS-CHECK.

       REMAINING-SYS-DISBURSEMENT.
           IF PROCEED-TOT NOT = ZEROES
              IF PROCEED-TOT NOT = CDF-AMT(CDF-SUB)
                 MOVE "OTHER "    TO CDF-DESC(CDF-SUB)
                 MOVE PROCEED-TOT TO CDF-AMT(CDF-SUB).
       EXIT-SYS-NEXT-DISBURSEMENT.
           EXIT.

      *******************************
       GET-FEES SECTION.
      *******************************
           MOVE 0 TO OTHER-FEE MOTOR-CLUB.
           MOVE 1 TO SUB.
       NEXT-LN-FEE.
           IF SUB > 10
              GO TO EXIT-FIND-FEE.
           IF LN-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-LN-FEE.

           IF LN-FEECODE(SUB) = "93" OR "94"
              ADD LN-FEEAMT(SUB) TO MOTOR-CLUB
           ELSE
              IF LN-FEECODE(SUB) NOT = SPACES
                 ADD LN-FEEAMT(SUB) TO OTHER-FEE.

           ADD 1 TO SUB.
           GO TO NEXT-LN-FEE.
       EXIT-FIND-FEE.
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
       COPY "LIBFP/FPIOPST.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
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
