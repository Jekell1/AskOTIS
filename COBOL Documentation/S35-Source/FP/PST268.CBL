       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST268.
       AUTHOR.        AMM.
       DATE-WRITTEN.  09-20-2019.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST268.C
      *
      * DESCRIPTION   :  CREDIT LIFE AND A&H
      *                  LIFE OF THE SOUTH
      *
      * STATE         :  MISSOURI   (MO)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2019.0920 PR#1388 ***NEW*** CALLED FROM PRT00T
      * AMM 2019.1218 PR#1388A CHANGES TO LEGAL JARGON IN CRYSTAL
      * BAH 2024.0104 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0520 WAS PRINTING BR-NAME FOR FIRST BENEIFICIARY, SHOULD BE
      *               GB-COMPANY-NAME, S35Q-19
      * BAH 2024.0521 CHANGED D01-ACCTNO BACK TO Z(5)9 S35Q-32
      * BAH 2024.0708 PRINT SECOND BENEFICIARY NAME & ADR, AND MAKER
      *               AND COMAKER DOB #1664 S35Q-80
      * BAH 2024.0717 DONT PRINT SSNO, PRINT X-XXXX INSTEAD #1664 S35Q-80
      * BAH 2024.0805 DONT PRINT JOINT DOB UNLESS JOINT INSURED #1668 S35Q-107
      * BAH 2024.1001 PRINT DATE OF DOCUMENT #1679 S35Q-135
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
           "@(#)FP/PST268.CBL S34 06/17/21-12:11:10 >".

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
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
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
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
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

       01  DETAIL-LINE                      PIC X(300)     VALUE SPACES.

      *=================================================================
      *
      *     D E T A I L - W O R L D - A C C E P T A N C E  ( W A C )
      *
      *-----------------------------------------------------------------
      *    CPI     : 12 CPI   [PIC X(96)]
      *    CUSTOMER: WORLD ACCEPTANCE
      *    FMNO    : 61 
      *    STATE   : MISSOURI   (MO)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(02).
           03  D01-INSURED                  PIC X(28).
           03  FILLER                       PIC X(02).
      *    03  D01-SSNO                     PIC 999/99/9999.
           03  D01-SSNO                     PIC X(11).
           03  FILLER                       PIC X(03).
           03  D01-ADDRESS                  PIC X(29).
           03  FILLER                       REDEFINES D01-ADDRESS.
               05  FILLER                   PIC X(06).
               05  D01-ADDRESS-2            PIC X(22).
           03  FILLER                       PIC X(02).
           03  D01-AGE                      PIC Z99.
           03  FILLER                       PIC X(03).
           03  D01-BRNO                     PIC 9(4).
           03  D01-DASH                     PIC X(01).
           03  D01-ACCTNO                   PIC ZZZZZ9.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(02).
           03  D02-CREDITOR                 PIC X(42).
           03  FILLER                       PIC X(42).
           03  D02-INT-RATE                 PIC ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(05).
           03  D03-DATE                     PIC 99/99/99.
           03  FILLER                       PIC X(05).
           03  D03-LIFE-TERM                PIC ZZ9.
           03  FILLER                       PIC X(06).
           03  D03-AH-TERM                  PIC ZZ9.
           03  FILLER                       PIC X(08).
           03  D03-ORG-AMT                  PIC ZZ,ZZ9.99.
           03  FILLER                       PIC X(10).
           03  D03-BALLOON                  PIC 9.
           03  FILLER                       PIC X(12).
           03  D03-AH-AMT                   PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(05).
           03  D03-PROCEEDS                 PIC ZZZ,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(22).
           03  D04-LIFE-COVR                PIC ZZ,ZZ9.99.
           03  FILLER                       PIC X(09).
           03  D04-AH-COVR                  PIC ZZ,ZZ9.99.
           03  FILLER                       PIC X(06).
           03  D04-1STPYDATE                PIC 99/99/99.
           03  FILLER                       PIC X(11).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D05-BOX1                     PIC X.
           03  FILLER                       PIC X(07).
           03  D05-BOX2                     PIC X.
           03  FILLER                       PIC X(36).
           03  D05-PREEXIST                 PIC X.
           03  FILLER                       PIC X(01).
           03  D05-BOX4                     PIC X.
           03  FILLER                       PIC X(08).
           03  D05-BOX5                     PIC X.
           03  FILLER                       PIC X(11).
           03  D05-BOX6                     PIC X.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(26).
           03  D06-MAKER-DOB                PIC 99/99/99.
           03  FILLER                       PIC X(33).
           03  D06-COMAKER-DOB              PIC 99/99/99.

       01  DETAIL-LINE-07                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(10).
           03  D07-DAY                      PIC Z9.
           03  FILLER                       PIC X(6).
           03  D07-MONTH                    PIC X(11).
           03  FILLER                       PIC X.
           03  D07-YEAR                     PIC 9(4).

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
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-PC1-FILE.
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LSM1-FILE.
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

           IF LN-INSPREM(1) = 0 AND LN-INSPREM(2) = 0
              GO TO PRINT-ROUTINE-EXIT.

           MOVE 08     TO PST-WS-FORM-PAGE-MAX.
           PERFORM PST-FR-FORM-INITIALIZATION.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ LOAD ACCOUNT INFO. }
           PERFORM PST-INFO-MODULE.

      *- - - - - - - - - - - - - - - - - - - - -{ SETUP PAGE COPY LOOP }
           MOVE ZEROES TO PAGE-COPY-ELE.
           MOVE 01     TO PAGE-COPY-MAX.

       PRINT-ROUTINE-PAGE-COPY.

           ADD 01 TO PAGE-COPY-ELE.

           IF ( PAGE-COPY-ELE > PAGE-COPY-MAX )
              GO TO PRINT-ROUTINE-END.

           PERFORM PRINT-PAGE-01-ROUTINE.
           PERFORM PRINT-PAGE-02-ROUTINE.
           PERFORM PRINT-PAGE-03-ROUTINE.
           PERFORM PRINT-PAGE-04-ROUTINE.
           PERFORM PRINT-PAGE-05-ROUTINE.
           PERFORM PRINT-PAGE-06-ROUTINE.
           PERFORM PRINT-PAGE-07-ROUTINE.
           PERFORM PRINT-PAGE-08-ROUTINE.

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

           PERFORM PRINT-PAGE-STD-ROUTINE.

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

           PERFORM PRINT-PAGE-STD-ROUTINE.

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

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-03-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-04-ROUTINE SECTION.
      ******************************************************************

           MOVE 04 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-04-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-04-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-05-ROUTINE SECTION.
      ******************************************************************

           MOVE 05 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-05-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-05-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-06-ROUTINE SECTION.
      ******************************************************************

           MOVE 06 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-06-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-06-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-07-ROUTINE SECTION.
      ******************************************************************

           MOVE 07 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-07-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-07-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-08-ROUTINE SECTION.
      ******************************************************************

           MOVE 08 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-08-ROUTINE-EXIT.

           PERFORM PRINT-PAGE-STD-ROUTINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-08-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-STD-ROUTINE SECTION.
      ******************************************************************

      * CHANGE FONT SIZE TO 12 CPI
           MOVE 10                      TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER     TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                   TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * INSURED DEBTOR 
           MOVE MAKER-ADDRESS                TO D01-ADDRESS-2.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-NAME-FIRST-LAST        TO D01-INSURED.
           MOVE "     X-XXXX"                TO D01-SSNO.
      *    MOVE MAKER-SSNO                   TO D01-SSNO.
      *    INSPECT D01-SSNO REPLACING ALL "/" BY "-".
           MOVE MAKER-CSZ                    TO D01-ADDRESS.
           MOVE MAKER-AGE                    TO D01-AGE.
           MOVE BR-NO                        TO D01-BRNO.
           MOVE "-"                          TO D01-DASH.
           MOVE LN-ACCTNO                    TO D01-ACCTNO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * JOINT INSURED
           IF ( LN-INSURED = "J" )
              MOVE JOINT-ADDRESS             TO D01-ADDRESS-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( LN-INSURED = "J" )
              MOVE JOINT-NAME-FIRST-LAST     TO D01-INSURED
              MOVE "     X-XXXX"             TO D01-SSNO
      *       MOVE JOINT-SSNO                TO D01-SSNO
      *       INSPECT D01-SSNO REPLACING ALL "/" BY "-"
              MOVE JOINT-CSZ                 TO D01-ADDRESS
              MOVE JOINT-AGE                 TO D01-AGE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * FIRST BENEFICIARY - CREDITOR - ADDRESS
           STRING BR-ADR1, " ", BR-ADR2
              DELIMITED BY "  " INTO D01-ADDRESS-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * FIRST BENEFICIARY - CREDITOR
           MOVE GB-COMPANY-NAME              TO D02-CREDITOR.
           STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO D01-ADDRESS.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM GET-SECOND-BENEFICIARY.

      * SECOND BENEFICIARY ADDRESS
           MOVE LSM-SECOND-BEN-ADR TO D01-ADDRESS-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SECOND BENEFICIARY ADDRESS
           MOVE LSM-SECOND-BEN-NAME TO D02-CREDITOR.
           IF LSM-SECOND-BEN-CITY NOT = SPACES OR
              LSM-SECOND-BEN-STATE NOT = SPACES
              STRING LSM-SECOND-BEN-CITY,", ",
                     LSM-SECOND-BEN-STATE," ",
                     LSM-SECOND-BEN-ZIP
                  DELIMITED BY "  " INTO D01-ADDRESS.
      * INTEREST RATE
           COMPUTE D02-INT-RATE ROUNDED = LN-APRATE * 1.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * EFFECTIVE DATE 
           IF ( LN-INSPREM(1) NOT = ZEROES )
              MOVE LN-INSEFF-DATE(1)       TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY             TO D03-DATE
           ELSE
              MOVE LN-INSEFF-DATE(2)       TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY             TO D03-DATE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TERM OF INS., ORGINAL AMOUNT OF LIFE, AH MONTHLY BEN 
           MOVE INS-TERM(1)                  TO D03-LIFE-TERM.
           IF ( LN-INSPREM(2) NOT = ZEROES )
              MOVE INS-TERM(2)  TO D03-AH-TERM.
           MOVE 0                            TO D03-BALLOON.
           MOVE LN-INSCOVR(1)                TO D03-ORG-AMT.
           IF ( LN-INSPREM(2) NOT = 0 )
              COMPUTE D03-AH-AMT = LN-REGPYAMT - LN-MAINTFEE.
      * NET CASH ADVANCE
           MOVE DISCLOSED-FINAMT             TO D03-PROCEEDS.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SCHEDULED MDTE
           MOVE MDTE-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO D03-DATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PREMIUMS, 1ST PYMT DUE, AMORT TERM 
           IF ( LN-INSTYPES-DL )
              MOVE LN-INSPREM(1) TO D04-LIFE-COVR.
      * LOAN PAYMENT 
           MOVE LN-REGPYAMT                  TO D03-PROCEEDS.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( LN-INSTYPES-LL )
              MOVE LN-INSPREM(1)  TO D04-LIFE-COVR.
           IF ( LN-INSPREM(2) NOT = ZEROES )
              MOVE LN-INSPREM(2)  TO D04-AH-COVR
           ELSE
              MOVE ZEROES         TO D04-AH-COVR.
           MOVE LN-1STPYDATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D04-1STPYDATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CHANGE FONT SIZE TO 10 CPI 
           MOVE 12                      TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER     TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                   TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * MONTHLY BOX
           IF ( LN-UNITPER-CD = "M" )
              MOVE "X"   TO D05-BOX6.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LIFE SINGLE BOX
           IF ( LN-INSURED NOT = "J" )
              MOVE "X" TO D05-BOX1.
      * DECREASING BOX 
           IF ( LN-INSTYPES-DL )
              MOVE "X"  TO D05-BOX2.
      * DISABILITY (SINGLE BOX)
           IF ( SP-INSAH-JOINT-FG NOT = "Y" )
              MOVE "X"   TO D05-BOX4.
      * RETRO BOX
           IF ( SP-INSAH-RETRO = "Y" )
              MOVE "X"   TO D05-BOX5.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * JOINT LIFE BOX 
           IF ( LN-INSURED = "J" )
              MOVE "X" TO D05-BOX1.
      * LEVEL BOX
           IF ( LN-INSTYPES-LL )
              MOVE "X" TO D05-BOX2.
      * NON-RETRO BOX
           IF ( SP-INSAH-RETRO NOT = "Y" )
              MOVE "X"   TO D05-BOX5.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PRE-EXISTING BOX
           IF ( LN-INSPREM(2) NOT = ZEROES )
              MOVE "X"   TO D05-PREEXIST.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DO NOT PRINT DATE ON CANCELLATION PAGE
           IF ( PST-WS-FORM-PAGE-NO = 8 )
              GO TO PRINT-PAGE-STD-ROUTINE-EXIT.

      * DATE THIS __ DAY OF __
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM         TO WS-MONTH-MM.
           PERFORM LOAD-WS-MONTH-TEXT.
           MOVE DATE-YYYYMMDD-DD         TO D07-DAY.
           MOVE WS-MONTH-TEXT            TO D07-MONTH.
           MOVE DATE-YYYYMMDD-YYYY       TO D07-YEAR.
           MOVE 30 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * MAKER DATE OF BIRTH
           MOVE MAKER-BDATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO D06-MAKER-DOB
           IF (JOINT-NAME-FIRST-LAST NOT = SPACES) AND
               LN-INSURED = "J"
              MOVE JOINT-BDATE            TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY            TO D06-COMAKER-DOB.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-PAGE-STD-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      **********************************************************
       MISC-ROUTINES SECTION.
      **********************************************************
       GET-SECOND-BENEFICIARY.
          MOVE SPACES TO LSM-SECOND-BEN-NAME
                         LSM-SECOND-BEN-ADR
                         LSM-SECOND-BEN-CITY
                         LSM-SECOND-BEN-STATE
                         LSM-SECOND-BEN-ZIP.
           IF LN-INSPREM(1) NOT = 0
               MOVE LN-OWNBR     TO LSM-BRNO
               MOVE LN-ACCTNO    TO LSM-ACCTNO
               MOVE 1            TO LSM-SEQNO
               PERFORM OPEN-LSM1-FILE
               PERFORM READ-LSM1-FILE
               PERFORM CLOSE-LSM1-FILE
               IF IO-FG = 9
                  MOVE SPACES TO LSM-SECOND-BEN-NAME
                                 LSM-SECOND-BEN-ADR
                                 LSM-SECOND-BEN-CITY
                                 LSM-SECOND-BEN-STATE
                                 LSM-SECOND-BEN-ZIP.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPPCGS_SQL.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
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
       COPY "LIBLP/LPLSM1RN.CPY".
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
