       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST825.
       AUTHOR.        AMM.
       DATE-WRITTEN.  10-01-2019.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST825.C
      *
      * DESCRIPTION   :  DISCLOSURE STATEMENT - SMALL LOAN
      *
      * STATE         :  MISSOURI    (MO)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2019.1001 PR#1388 CALLED FROM LONP8C
      * AMM 2020.0117 PR#1388B DON'T PRINT PRLNNO IF ZERO
      * AMM 2020.0207 PR#1420 ADDED BLANK COPY FMNO 96
      * AMM 2020.0305 PR#1425 ADDED COLLCD UL TO THE SECURITY LINE
      * BAH 2020.0713 NET BALANCE WAS ADDING LN-PRINSRB INSTEAD OF SUBTRACTING
      *                                                         #1448
      * AMM 2021.1005 PR#1517 LEGAL JARGON CHANGES ON PAGE 2
      * BAH 2022.0503 START TRCFILE WITH LOANDATE LIKE A15, NOT TRANSDATE
      * JKC 2022.0610 ADDED ADDITIONAL LOGIC TO POPULATE CDF FIELDS 
      *               WHEN THERE IS UMC FEE; MISSED LOGIC WHEN ADDING
      *               TRCFILE.
      * BAH 20220907 HARD CODED START
      * BAH 2024.0104 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0521 CHANGED D01-ACCTNO BACK TO Z(5)9 AND D05-PRLNNO
      *               BACK TO Z(6) S35Q-32
      * BAH 2024.1218 COMPLETE SECURITY LINE SAME AS "PP" FOR "SC" AND SAME AS
      *               "AP" FOR "AS" #1695 S35Q-154
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
           "@(#)FP/PST825.CBL S35 03/26/24-09:35:40 >".

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

       01  C-S-Z           PIC X(85).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).
       01  ADD-ON          PIC X(25).
       01  NON-FILE        PIC S9(5)V99 COMP-3 VALUE 0.
       01  UMC-FEE         PIC S9(5)V99 COMP-3 VALUE 0.
       01  OTHER-FEE       PIC S9(5)V99 COMP-3 VALUE 0.
       01  SUB1            PIC S9(6)    COMP-3.
       01  COMPANY-NAME    PIC X(40).
       01  ME-AMT          PIC S9(5)V99 COMP-3 VALUE 0.
       01  PROC-WRK        PIC S9(7)V99 COMP-3 VALUE 0.
       01  PROCEED-TOT     PIC S9(7)V99 COMP-3 VALUE 0.
       01  CDF-SUB         PIC 9(3)            VALUE ZEROES.
       01  CDF-SUB-MAX     PIC 9(3)            VALUE ZEROES.
       01  CDF-AMT         PIC S9(6)V99 COMP-3  OCCURS 2.
       01  CDF-DESC        PIC X(20)            OCCURS 2.

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                      PIC X(300)     VALUE SPACES.

      ******************************************************************
      *
      *     D E T A I L - W O R L D - A C C E P T A N C E  ( W A C )
      *
      *-----------------------------------------------------------------
      *    CPI     : 12 CPI   [PIC X(96)]
      *    CUSTOMER: WORLD ACCEPTANCE
      *    FMNO    : 97 
      *    STATE   : MISSOURI   (MO)
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D01-ACCTNO                   PIC Z(5)9.
           03  FILLER                       PIC X(40).
           03  D01-LOANDATE                 PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D02-CREDITOR                 PIC X(31).
           03  FILLER                       PIC X(14).
           03  D02-BORROWER                 PIC X(31).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(15).
           03  D03-APR                      PIC ZZ9.99.
           03  FILLER                       PIC X(06).
           03  D03-FINCHG                   PIC Z,ZZZ,ZZ9.99-.
           03  FILLER                       PIC X(10).
           03  D03-FINAMT                   PIC Z,ZZZ,ZZ9.99-.
           03  FILLER                       PIC X(11).
           03  D03-TOTAL                    PIC Z,ZZZ,ZZ9.99-.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(08).
           03  D04-NOREGPY                  PIC ZZ9          BLANK ZERO.
           03  FILLER                       PIC X(09).
           03  D04-REGPYAMT                 PIC $$$,$$9.99-  BLANK ZERO.
           03  FILLER                       PIC X(11).
           03  D04-BEGIN-DATE               PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(29).
           03  D05-AMOUNT-1                 PIC ZZZ,ZZ9.99.
           03  FILLER                       REDEFINES D05-AMOUNT-1.
               05  FILLER                   PIC X(03).
               05  D05-EFFRATE              PIC ZZ9.99.
           03  FILLER                       PIC X(31).
           03  D05-AMOUNT-2                 PIC ZZZ,ZZ9.99.
           03  FILLER                       REDEFINES D05-AMOUNT-2.
               05  D05-PRLNNO               PIC Z(6) BLANK ZERO.
           03  FILLER                       PIC X(03).
           03  D05-AMOUNT-3                 PIC ZZZ,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D06-BR-NAME                  PIC X(40).
           03  FILLER                       PIC X(04).
           03  D06-CHECK-PAYEE              PIC X(17).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(44).
           03  D07-MATURE-DATE              PIC 99/99/99.
           03  D07-COLLATERAL               PIC X(42).

       COPY "LIBFP/PST_COPYW.CPY".
       COPY "LIBFP/PST_INFOW.CPY".
       COPY "LIBFP/PST_FORMW.CPY".
       COPY "LIBGB/PHONE.CPY".
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
      *    THE FOLLOWING IS FROM THE ORIGINAL DOT MATRIX PROGRAM WHICH
      *    NEED TO RESTORE BACK SOME OF THE ORIGINAL VALUES THAT WERE
      *    ALTER FROM THE STANDARD LASER FORM ROUTINES PRIOR TO THIS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * PUT MAKER BACK IN THE BW-REC BUFFER
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM GET-BW1.
           PERFORM CLOSE-BW1-FILE.

           PERFORM GET-FEES.

           MOVE 1           TO CDF-SUB.
           MOVE 2           TO CDF-SUB-MAX.
           MOVE 0           TO CDF-AMT(1) CDF-AMT(2).

           MOVE SPACES      TO CDF-DESC(1) CDF-DESC(2).
           MOVE LN-PROCASH  TO ME-AMT.
           MOVE 0           TO PROCEED-TOT PROC-WRK.

           IF BR-USE-DEBIT-CARDS = "Y"
              PERFORM CHECK-FOR-DEBIT-CARD.

           IF UMC-FEE NOT = 0
              MOVE "UMC PREMIUM" TO CDF-DESC(1)
              MOVE UMC-FEE       TO CDF-AMT(1)
              ADD 1              TO CDF-SUB.

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

           IF NOT ( IP-FMNO = 96 )
              MOVE LN-ACCTNO                      TO D01-ACCTNO
              MOVE LN-LOANDATE                    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY            TO D01-LOANDATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF NOT ( IP-FMNO = 96 )
              MOVE MAKER-NAME-FIRST-LAST       TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE GB-COMPANY-NAME                TO COMPANY-NAME.
           CALL "C$JUSTIFY" USING COMPANY-NAME, "L".
           MOVE COMPANY-NAME TO D06-BR-NAME.
           IF NOT ( IP-FMNO = 96 )
              MOVE JOINT-NAME-FIRST-LAST       TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR1                        TO D02-CREDITOR.
           IF NOT ( IP-FMNO = 96 )
              MOVE MAKER-ADDRESS-1             TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              MOVE BR-ADR2           TO D02-CREDITOR
           ELSE
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF NOT ( IP-FMNO = 96 )
              IF MAKER-ADDRESS-2 NOT = SPACES
                 MOVE MAKER-ADDRESS-2   TO D02-BORROWER
              ELSE
                 MOVE MAKER-CSZ         TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF NOT ( IP-FMNO = 96 )
              IF MAKER-ADDRESS-2 NOT = SPACES
                 MOVE MAKER-CSZ         TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( IP-FMNO = 96 )
              GO TO PRINT-PAGE-01-ROUTINE-END.

           COMPUTE D03-APR ROUNDED = LN-APRATE * 1.
           MOVE LN-FINCHG                      TO D03-FINCHG.
           MOVE DISCLOSED-FINAMT               TO D03-FINAMT.
           MOVE TOTAL-REPAY                    TO D03-TOTAL.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-1STPYAMT NOT = LN-REGPYAMT
              MOVE 1                TO D04-NOREGPY
              MOVE LN-1STPYAMT      TO D04-REGPYAMT
           ELSE
              MOVE LN-REGPYAMT      TO D04-REGPYAMT
              IF LN-LASTPYAMT NOT = LN-REGPYAMT
                 COMPUTE D04-NOREGPY = LN-ORGTERM - 1
              ELSE
                 MOVE LN-ORGTERM    TO D04-NOREGPY.
           MOVE LN-1STPYDATE                   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D04-BEGIN-DATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-1STPYAMT NOT = LN-REGPYAMT
              COMPUTE D04-NOREGPY = LN-ORGTERM - 1
              MOVE LN-REGPYAMT      TO D04-REGPYAMT
           ELSE
              IF LN-LASTPYAMT NOT = LN-REGPYAMT
                 MOVE 1             TO D04-NOREGPY
                 MOVE LN-LASTPYAMT  TO D04-REGPYAMT.
           MOVE MDTE-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D07-MATURE-DATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * COLLATERAL 
           MOVE LN-COLLCD TO CD-CODE.
           MOVE "CO" TO CD-TYPE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO CD-DESC.

           IF LN-COLLCD = "PP" OR "SC"
              MOVE "PERSONAL PROPERTY - SEE EXHIBIT A"
                                  TO D07-COLLATERAL
           ELSE
              IF LN-COLLCD = "AP" OR "AS"
              MOVE "AUTO & PERSONAL PROPERTY-SEE EXHIBIT A"
                                  TO D07-COLLATERAL
           ELSE
              IF LN-COLLCD = "AU"
                 MOVE "AUTO - SEE EXHIBIT A" TO D07-COLLATERAL
           ELSE
              IF LN-COLLCD = "UL"
                 MOVE "UNSECURED"            TO D07-COLLATERAL
           ELSE
                MOVE CD-DESC TO D07-COLLATERAL.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SERVICE CHARGE / PREVIOUS ACCT 
           MOVE LN-SERCHG                      TO D05-AMOUNT-1.
           MOVE PRIOR-ACCTNO-NUMBER            TO D05-PRLNNO.
           MOVE 07 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INTEREST CHARGE / LATE CHARGE
           COMPUTE D05-AMOUNT-1 = LN-INTCHG + LN-EXTCHG.
           MOVE 0                              TO D05-AMOUNT-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * EFFECTIVE ANNUAL INTEREST / PREVIOUS BALANCE 
           COMPUTE D05-EFFRATE ROUNDED = LN-EFFRATE * 1
           COMPUTE D05-AMOUNT-2 = LN-PRBAL + LN-PRINTDUE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LESS REFUND
           COMPUTE D05-AMOUNT-2 = LN-PRFINCHGRB + LN-PRINSRB.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NET BALANCE
           COMPUTE D05-AMOUNT-3 = LN-PRBAL + LN-PRINTDUE
                            - LN-PRFINCHGRB - LN-PRINSRB.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NON-FILING 
           MOVE NON-FILE                       TO D05-AMOUNT-3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PUBLIC OFFICIALS 
           MOVE OTHER-FEE                      TO D05-AMOUNT-3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TO OTHERS (1)
           MOVE CDF-DESC(1)                    TO D06-CHECK-PAYEE.
           MOVE CDF-AMT(1)                     TO D05-AMOUNT-3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TO OTHERS (2)
           MOVE CDF-DESC(2)                    TO D06-CHECK-PAYEE.
           MOVE CDF-AMT(2)                     TO D05-AMOUNT-3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TO ME
           MOVE ME-AMT                         TO D05-AMOUNT-3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AMOUNT FINANCED
           MOVE DISCLOSED-FINAMT               TO D05-AMOUNT-3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-PAGE-01-ROUTINE-END.

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

           IF NOT ( IP-FMNO = 96 )
              MOVE LN-ACCTNO                      TO D01-ACCTNO
              MOVE LN-LOANDATE                    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                    TO D01-LOANDATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-02-ROUTINE-EXIT.
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
           MOVE 0 TO NON-FILE OTHER-FEE UMC-FEE.
           MOVE 1 TO SUB.
       NEXT-LN-FEE.
           IF SUB > 10
              GO TO EXIT-FIND-FEE.
           IF LN-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-LN-FEE.

           IF LN-FEECODE(SUB) = "91" OR "92" OR "95" OR
                                "96" OR "97" OR "98" OR "99"
              ADD LN-FEEAMT(SUB) TO OTHER-FEE
           ELSE
              IF LN-FEECODE(SUB) = "93" OR "94"
                 ADD LN-FEEAMT(SUB) TO UMC-FEE
              ELSE
                 IF LN-FEECODE(SUB) = "90"
                    ADD LN-FEEAMT(SUB) TO NON-FILE.

           ADD 1 TO SUB.
           GO TO NEXT-LN-FEE.
       EXIT-FIND-FEE.
           EXIT.
 
       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/PHONEIO.CPY".
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME.
           PERFORM APPEND-BW-NAMES.
       APPEND-BW-NAMES.
           MOVE BW-FNAME TO C-S-Z.
           MOVE BW-LNAME TO ADD-ON.
           PERFORM ADD-IT.
       ADD-IT.
           INSPECT C-S-Z REPLACING FIRST "  " BY " +".
           INSPECT C-S-Z REPLACING FIRST
              "+                        " BY ADD-ON.
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.

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
