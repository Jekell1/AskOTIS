       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST843.
       AUTHOR.        AMM.
       DATE-WRITTEN.  07-23-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST843.C
      *
      * DESCRIPTION   :  LARGE LOAN CONTRACT
      * PRINTER       :  HP UNIVERSAL
      *
      * STATE         :  ILLINOIS     (IL)
      *
      *    FMNO MUST BE 71 - BLANK DISCLOSURE
      *                 72 - IL DISCLOSURE
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0723 PR#1436 NEW, CALLED FROM LONP2O FMNO 71/72
      * AMM 2021.0201 PR#1487 ADDED DATABASE FEE REPLACING DOC PREP FEE
      * AMM 2021.0211 PR#1487A REMOVED DATABASE FEE FROM OFFICIAL FEES LINE
      * AMM 2021.0602 PR#1499 ADDED WORD 'NONE' TO COBORROWER IF NOT JOINT
      * AMM 2022.0428 PR#1552 ADDED LOGIC TO CHECK SECURITY BOXES
      * AMM 2022.0719 PR#1561 ADDED THIRD PAGE
      * AMM 2022.0810 PR#1566 ADDED NEW LEGAL JARGON TO PAGE 2 CRYSTAL
      * BAH 2022.1003 INCLUDE EXTENSION CHARGE WITH INTEREST CHARGE
      *               IN THE ITEMIZATION OF THE FINANCE CHARGE #1572
      * BAH 2023.0627 ADDED TITLE SECURED CHANGES, REMOVE DATABASE FEE #1615
      * BAH 2023.0710 REMOVE COLLCD "AP" AND "WB", PRINT YEAR,MAKE,MODEL,
      *               VIN AND ADD WEBSITE #1615
      * BAH 2023.0725 MOVED ITEMS UP AND ADDED NEW WRITTEN STATEMENT SENTENCE
      *               #1617
      * BAH 2024.0104 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0521 CHANGED D01-ACCTNO BACK TO Z(5)9 S35Q-32
      * BAH 2024.0524 ADDED D05-CREDITOR TO MATCH A15 S35Q-46
      * BAH 2024.1217 CHECK ASSIGNMENT OF WAGES IF COLLCD "WS" AND CHECK
      *               PERSONAL PROPERTY IF "SC" OR "WS" #1695 S35Q-154
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
           "@(#)FP/PST843.CBL S35 08/10/22-09:37:30 >".

       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBFP/FPWSPST.CPY".
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

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                      PIC X(300)     VALUE SPACES.

       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D01-ACCTNO                   PIC Z(5)9.
           03  FILLER                       PIC X(39).
           03  D01-LOANDATE                 PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(4).
           03  D-BOX                        PIC X.
           03  FILLER                       PIC X(9).
           03  D02-NONE                     PIC X(04).
           03  D02-CREDITOR                 PIC X(35).
           03  FILLER                       PIC X(10).
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

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(23).
           03  D05-CREDITOR               PIC X(38).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(06).
           03  D06-AMOUNT-1                 PIC ZZZZ.99.
           03  D06-AMOUNT-1X REDEFINES D06-AMOUNT-1 PIC X(7).
           03  FILLER                       PIC X(22).
           03  D06-WAGE-BOX                 PIC X.
           03  FILLER                       PIC X(27).
           03  D06-PP-BOX                   PIC X.
           03  FILLER                       PIC X(09).
           03  D06-AMOUNT-2                 PIC ZZZZZ.99.
           03  FILLER                       REDEFINES D06-AMOUNT-2.
               05  D06-PRIOR                PIC Z(6).
           03  D06-PRIOR-NA REDEFINES D06-AMOUNT-2 PIC X(8).
           03  FILLER                       PIC X(02).
           03  D06-AMOUNT-3                 PIC ZZZZZZ.99.

       01  DETAIL-LINE-07                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(7).
           03  D07-MV-BOX                 PIC X.
           03  FILLER                     PIC X(37).
           03  D07-YEAR                   PIC X(4).
           03  FILLER                     PIC X(01).
           03  D07-MAKE                   PIC X(10).
           03  FILLER                     PIC X(01).
           03  D07-MODEL                  PIC X(10).
           03  FILLER                     PIC X(01).
           03  D07-VIN                    PIC X(17).

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
           PERFORM CLOSE-TRC1-FILE.
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

           MOVE 03     TO PST-WS-FORM-PAGE-MAX.
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

           MOVE BR-STMNT1                      TO D05-CREDITOR.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FONT-RESET.

      * CHANGE FONT SIZE TO 12 CPI - - - - - - - - - - - - - - - - - - -
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           IF NOT ( IP-FMNO = 71 )
              MOVE LN-ACCTNO                   TO D01-ACCTNO
              MOVE LN-LOANDATE                 TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D01-LOANDATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF NOT ( IP-FMNO = 71 )
              MOVE MAKER-NAME-FIRST-LAST       TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE GB-COMPANY-NAME                TO D02-CREDITOR.
           IF NOT ( IP-FMNO = 71 )
              IF ( JOINT-NAME-FIRST-LAST NOT = SPACES )
                 MOVE JOINT-NAME-FIRST-LAST    TO D02-BORROWER
              ELSE
                 MOVE "NONE"                   TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR1                        TO D02-CREDITOR.
           IF NOT ( IP-FMNO = 71 )
              MOVE MAKER-ADDRESS-1             TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( BR-ADR2 NOT = SPACES )
              MOVE BR-ADR2                     TO D02-CREDITOR
           ELSE
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF NOT ( IP-FMNO = 71 )
              IF ( MAKER-ADDRESS-2 NOT = SPACES )
                 MOVE MAKER-ADDRESS-2          TO D02-BORROWER
              ELSE
                 MOVE MAKER-CSZ                TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( BR-ADR2 NOT = SPACES )
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF NOT ( IP-FMNO = 71 )
              IF ( MAKER-ADDRESS-2 NOT = SPACES )
                 MOVE MAKER-CSZ         TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( IP-FMNO = 71 )
              GO TO PRINT-PAGE-01-ROUTINE-END.

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
           MOVE MDTE-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D04-BEGIN-DATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SECURITY BOXES 

      * A REVOCABLE ASSIGNMENT OF YOUR WAGES
           IF ( LN-COLLCD = "WG" OR "WP" OR "WA" )
              MOVE "X" TO D06-WAGE-BOX.
      * A SECURITY INTEREST IN PERSONAL PROPERTY
           IF ( LN-COLLCD = "PP" OR "WP" )
              MOVE "X" TO D06-PP-BOX.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * A SECURITY INTEREST IN A MOTOR VEHICLE
           IF ( LN-COLLCD = "AU" OR "WA" )
              MOVE "X"           TO D07-MV-BOX
              MOVE AUTO-YEAR(1)  TO D07-YEAR
              MOVE AUTO-MAKE(1)  TO D07-MAKE
              MOVE AUTO-MODEL(1) TO D07-MODEL
              MOVE AUTO-ID(1)    TO D07-VIN.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ITEMIZATION OF THE FINANCE CHARGE
      * ILLINOIS DATABASE FEE
           MOVE "  N/A"                  TO D06-AMOUNT-1X.
      * ITEMIZATION OF THE AMOUNT FINANCED
      * PREVIOUS ACCT #
           IF PRIOR-ACCTNO-NUMBER = 0
              MOVE "   N/A  "          TO D06-PRIOR-NA
           ELSE
              MOVE PRIOR-ACCTNO-NUMBER TO D06-PRIOR.
           MOVE 09 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ITEMIZATION OF THE FINANCE CHARGE
      * INTEREST 
           COMPUTE D06-AMOUNT-1 = LN-INTCHG + LN-EXTCHG.
      * ITEMIZATION OF THE AMOUNT FINANCED
      * LATE CHARGE ON PREVIOUS ACCOUNT
           MOVE 0                              TO D06-AMOUNT-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ITEMIZATION OF THE AMOUNT FINANCED
      * PREVIOUS BALANCE 
           COMPUTE D06-AMOUNT-2 = LN-PRBAL + LN-PRINTDUE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ITEMIZATION OF THE AMOUNT FINANCED
      * LESS REFUND
           COMPUTE D06-AMOUNT-2 = LN-PRFINCHGRB + LN-PRINSRB.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ITEMIZATION OF THE AMOUNT FINANCED
      * NET BALANCE RENEWED
           COMPUTE D06-AMOUNT-3 = LN-PRBAL + LN-PRINTDUE
                                - LN-PRFINCHGRB - LN-PRINSRB.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ITEMIZATION OF THE AMOUNT FINANCED
      * PAID TO YOU
           MOVE LN-PROCEEDS                    TO D06-AMOUNT-3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ITEMIZATION OF THE AMOUNT FINANCED
      * PAID TO
           MOVE 0                              TO D06-AMOUNT-3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ITEMIZATION OF THE AMOUNT FINANCED
      * OFFICIAL FEES
           MOVE NONMEMO-FEES TO D06-AMOUNT-3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ITEMIZATION OF THE AMOUNT FINANCED
      * AMOUNT FINANCED
           MOVE DISCLOSED-FINAMT               TO D06-AMOUNT-3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NON-TITLE SECURED
           IF NOT ( IP-FMNO = 71 )
              IF NOT (LN-CLASS = 5 AND LN-SUBCLASS = 10)
                 MOVE "X" TO D-BOX.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TITLE SECURED
           IF NOT ( IP-FMNO = 71 )
              IF (LN-CLASS = 5 AND LN-SUBCLASS = 10)
                 MOVE "X" TO D-BOX.
           MOVE 07 TO LN-FEED.
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

           IF NOT ( IP-FMNO = 71 )
              MOVE LN-ACCTNO                   TO D01-ACCTNO
              MOVE LN-LOANDATE                 TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D01-LOANDATE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF NOT ( IP-FMNO = 71 )
              IF ( JOINT-NAME-FIRST-LAST = SPACES )
                 MOVE "NONE"                   TO D02-NONE.
           MOVE 28 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NON-TITLE SECURED
           IF NOT ( IP-FMNO = 71 )
              IF NOT (LN-CLASS = 5 AND LN-SUBCLASS = 10)
                 MOVE "X" TO D-BOX.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TITLE SECURED
           IF NOT ( IP-FMNO = 71 )
              IF (LN-CLASS = 5 AND LN-SUBCLASS = 10)
                 MOVE "X" TO D-BOX.
           MOVE 23 TO LN-FEED.
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

           MOVE BR-STMNT1                      TO D05-CREDITOR.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FONT-RESET.

      * CHANGE FONT SIZE TO 12 CPI 
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           IF NOT ( IP-FMNO = 71 )
              MOVE LN-ACCTNO                   TO D01-ACCTNO
              MOVE LN-LOANDATE                 TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D01-LOANDATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF NOT ( IP-FMNO = 71 )
              MOVE MAKER-NAME-FIRST-LAST       TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE GB-COMPANY-NAME                TO D02-CREDITOR.
           IF NOT ( IP-FMNO = 71 )
              IF ( JOINT-NAME-FIRST-LAST NOT = SPACES )
                 MOVE JOINT-NAME-FIRST-LAST    TO D02-BORROWER
              ELSE
                 MOVE "NONE"                   TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR1                        TO D02-CREDITOR.
           IF NOT ( IP-FMNO = 71 )
              MOVE MAKER-ADDRESS-1             TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( BR-ADR2 NOT = SPACES )
              MOVE BR-ADR2                     TO D02-CREDITOR
           ELSE
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF NOT ( IP-FMNO = 71 )
              IF ( MAKER-ADDRESS-2 NOT = SPACES )
                 MOVE MAKER-ADDRESS-2          TO D02-BORROWER
              ELSE
                 MOVE MAKER-CSZ                TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( BR-ADR2 NOT = SPACES )
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF NOT ( IP-FMNO = 71 )
              IF ( MAKER-ADDRESS-2 NOT = SPACES )
                 MOVE MAKER-CSZ         TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF NOT ( IP-FMNO = 71 )
              IF ( JOINT-NAME-FIRST-LAST = SPACES )
                 MOVE "NONE"                   TO D02-NONE.
           MOVE 15 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-03-ROUTINE-EXIT.
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
       COPY "LIBLP/LPTRCGS_SQL.CPY".
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
       COPY "LIBLP/LPTRC1RN.CPY".
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
