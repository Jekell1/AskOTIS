       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST828.
       AUTHOR.        AMM.
       DATE-WRITTEN.  01-29-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST828.C
      *
      * DESCRIPTION   :  DISCLOSURE STATEMENT - SMALL LOAN
      *
      * STATE         :  TEXAS     (TX)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0129 PR#1418 NEW CALLED FROM LONP4Q
      * AMM 2020.0326 PR#1418B ADDED FMNO 31 BLANK COPY
      * BAH 2020.0713 NET BALANCE WAS ADDING LN-PRINSRB INSTEAD OF SUBTRACTING
      *               #1448
      * AMM 2021.0921 PR#1514 REPLACED SECOND PAGE WITH NEW ARBITRATION
      *               LANGUAGE
      * BAH 2024.0105 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0521 CHANGED D01-ACCTNO BACK TO Z(5)9 AND D05-PRLNNO
      *               BACK TO D05-PRLNNO S35Q-32
      * BAH 2024.0523 CORRECT 3 FIELDS PRINTING IN WRONG AREA S35Q-37
      * BAH 2024.1218 PRINT COLLATERAL THE SAME AS WI PST850 AND ID PST830 #1695
      *               ADDED SIGNIFICANT COLLATERAL "AS" AND "SC" S35Q-154
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
           "@(#)FP/PST828.CBL S34 06/17/21-13:33:09 >".

       COPY "LIBFP/FPWSPST.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
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
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
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

       01  COMPANY-NAME    PIC X(31).

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
      *    FMNO    : 30 
      *    STATE   : TEXAS     (TX)
      ******************************************************************
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D01-ACCTNO                   PIC Z(5)9.
           03  FILLER                       PIC X(40).
           03  D01-LOANDATE                 PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D02-CREDITOR                 PIC X(30).
           03  FILLER                       PIC X(15).
           03  D02-BORROWER                 PIC X(31).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(15).
           03  D03-APR                      PIC ZZ9.99.
           03  FILLER                       PIC X(07).
           03  D03-FINCHG                   PIC ZZZ,ZZ9.99   BLANK ZERO.
           03  FILLER                       PIC X(14).
           03  D03-FINAMT                   PIC ZZZ,ZZ9.99   BLANK ZERO.
           03  FILLER                       PIC X(13).
           03  D03-TOTAL                    PIC ZZZ,ZZ9.99   BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(08).
           03  D04-NOREGPY                  PIC ZZ9          BLANK ZERO.
           03  FILLER                       PIC X(09).
           03  D04-REGPYAMT                 PIC $$$,$$9.99   BLANK ZERO.
           03  FILLER                       PIC X(13).
           03  D04-BEGIN-DATE               PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(29).
           03  D05-AMOUNT-1                 PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(06).
           03  D05-MDTE-DATE                PIC 99/99/99.
           03  FILLER                       PIC X(18).
           03  D05-AMOUNT-2                 PIC ZZZ,ZZ9.99.
           03  FILLER                       REDEFINES D05-AMOUNT-2.
               05  FILLER                   PIC X(01).
               05  D05-PRLNNO               PIC Z(6) BLANK ZERO.
           03  FILLER                       PIC X(02).
           03  D05-AMOUNT-3                 PIC ZZZ,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D06-BR-NAME                  PIC X(31).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(62).
           03  D07-COLLATERAL               PIC X(42).

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
           PERFORM CLOSE-TRC1-FILE.
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
       PRINT-ROUTINE SECTION.
      ******************************************************************

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

           IF NOT ( IP-FMNO = 31 )
              MOVE LN-ACCTNO                   TO D01-ACCTNO
              MOVE LN-LOANDATE                 TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D01-LOANDATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF NOT ( IP-FMNO = 31 )
              MOVE MAKER-NAME-FIRST-LAST       TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE GB-COMPANY-NAME                TO COMPANY-NAME.
           CALL "C$JUSTIFY" USING COMPANY-NAME, "L".
           MOVE COMPANY-NAME                   TO D06-BR-NAME.
           IF NOT ( IP-FMNO = 31 )
              MOVE JOINT-NAME-FIRST-LAST       TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR1                        TO D02-CREDITOR.
           IF NOT ( IP-FMNO = 31 )
              MOVE MAKER-ADDRESS-1             TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              MOVE BR-ADR2           TO D02-CREDITOR
           ELSE
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF NOT ( IP-FMNO = 31 )
              IF MAKER-ADDRESS-2 NOT = SPACES
                 MOVE MAKER-ADDRESS-2   TO D02-BORROWER
              ELSE
                 MOVE MAKER-CSZ         TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF NOT ( IP-FMNO = 31 )
              IF MAKER-ADDRESS-2 NOT = SPACES
                 MOVE MAKER-CSZ         TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( IP-FMNO = 31 )
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
           MOVE LN-1STPYDATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D04-BEGIN-DATE.
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
           MOVE DATE-MMDDYY         TO D05-MDTE-DATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * COLLATERAL 

      * CHANGE FONT SIZE TO 14 CPI
           MOVE 08                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           IF LN-COLLCD = "PP" OR "SC"
              MOVE "PERSONAL PROPERTY-SEE EXHIBIT A" TO D07-COLLATERAL
           ELSE
              IF LN-COLLCD = "AP" OR "AS"
                 MOVE "AUTO & PERSONAL PROPERTY-SEE EXHIBIT A"
                                                     TO D07-COLLATERAL
              ELSE
                 IF LN-COLLCD = "AU"
                    MOVE "AUTO - SEE EXHIBIT A" TO D07-COLLATERAL
                 ELSE
                    MOVE CODE-DESC-OF-COLLCD TO D07-COLLATERAL.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CHANGE FONT SIZE TO 12 CPI
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * ACQUISITION CHARGE / PREVIOUS ACCT 
           MOVE LN-SERCHG                      TO D05-AMOUNT-1.
           MOVE PRIOR-ACCTNO-NUMBER            TO D05-PRLNNO.
           MOVE 07 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INSTALL CHARGE / LATE CHARGE 
           MOVE LN-INTCHG                      TO D05-AMOUNT-1.
           MOVE 0                              TO D05-AMOUNT-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PREVIOUS BALANCE 
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

      * TO ME
           MOVE LN-PROCEEDS                    TO D05-AMOUNT-3.
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

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-02-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".


      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
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
       COPY "LIBLP/LPTRCGS_SQL.CPY".
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
       COPY "LIBLP/LPTRC1RN.CPY".
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
