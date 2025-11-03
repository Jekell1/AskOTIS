       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST269.
       AUTHOR.        AMM.
       DATE-WRITTEN.  02-26-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST269.C
      *
      * DESCRIPTION   :  CREDIT LIFE AND A&H
      *                  LIFE OF THE SOUTH
      *
      * STATE         :  TEXAS    (TX)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0226 PR#1418 ***NEW*** CALLED FROM PRT00T
      * AMM 2020.0528 PR#1435 MISSED A PERIOD AFTER A&H PRINT, DATE TOO FAR UP
      * BAH 2024.0105 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0521 CHANGED D01-NUMBER BACK TO 9(6) S35Q-32
      * BAH 2024.0523 PAGE 15 NOT FILLING IN LOAN INFO S35Q-41
      * BAH 2024.0702 PRINT SECOND BENEFICIARY AND BIRTHDATE #1663 S35Q-79
      * BAH 2024.0809 DONT PRINT JOINT DOB UNLESS JOINT INSURED #1669 S35Q-113
      *               DONT PRINT JOINT NAME UNLESS JINT INSURED AND
      *               PRINT "X-XXXX" FOR SSNO.
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
           "@(#)FP/PST269.CBL S34 06/17/21-12:11:49 >".

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
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTD.CPY".
       COPY "LIBLP/LP01LTD_SQL.CPY".
       COPY "LIBLP/LPWSLTD.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
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

       01  HOLD-FIRST-BENEFICIARY-2    PIC X(46).

       01  DETAIL-LINE                      PIC X(300)     VALUE SPACES.

      *=================================================================
      *
      *     D E T A I L - W O R L D - A C C E P T A N C E  ( W A C )
      *
      *-----------------------------------------------------------------
      *    CPI     : 12 CPI   [PIC X(96)]
      *    CUSTOMER: WORLD ACCEPTANCE
      *    FMNO    : 103
      *    STATE   : TEXAS     (TX)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(79).
           03  D01-GROUP                    PIC X(13).
           03  D01-GROUPX REDEFINES D01-GROUP.
               05  D01-BRNO                 PIC 9(4).
               05  FILLER                   PIC X.
               05  D01-NUMBER               PIC 9(6).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(05).
           03  D02-BW-NAME                  PIC X(34).
           03  FILLER                       PIC X(02).
           03  D02-AGE                      PIC ZZZ.
           03  FILLER                       PIC X(03).
           03  D02-SSNO                     PIC X(11).
           03  FILLER                       PIC X(03).
           03  D02-CSZ                      PIC X(32).
           03  FILLER                       REDEFINES D02-CSZ.
               05  FILLER                   PIC X(07).
               05  D02-ADR                  PIC X(25).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(05).
           03  D03-BR-NAME                  PIC X(46).
           03  FILLER                       PIC X(02).
           03  D03-SECOND-BENE              PIC X(41).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(10).
           03  D04-RATE                     PIC ZZ9.99 BLANK ZERO.
           03  D04-AMORTIZED REDEFINES D04-RATE PIC Z(6).
           03  FILLER                       PIC X(22).
           03  D04-NET-CASH                 PIC Z(5).99.
           03  D04-1STPYDATE REDEFINES D04-NET-CASH PIC 99/99/99.
           03  FILLER                       PIC X(31).
           03  D04-PAY-PER-BOX              PIC X.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(05).
           03  D05-AH-BOX                   PIC X.
           03  FILLER                       PIC X(07).
           03  D05-LOAN-PAY                 PIC Z(5).99.
           03  FILLER                       PIC X(16).
           03  D05-BALLOON                  PIC X(3).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(19).
           03  D06-SINGLE                   PIC X.
           03  FILLER                       PIC X(08).
           03  D06-JOINT                    PIC X.
           03  FILLER                       PIC X(05).
           03  D06-EFFDATE                  PIC 99/99/99 BLANK ZERO.
           03  FILLER                       PIC X(03).
           03  D06-INSTERM                  PIC Z(3).
           03  FILLER                       PIC X(12).
           03  D06-EXPDATE                  PIC 99/99/99 BLANK ZERO.
           03  D06-INSCOVR                  PIC $(6).99 BLANK ZERO.
           03  D06-AHBENE                   PIC $(5).99 BLANK ZERO.
           03  FILLER                       PIC X(01).
           03  D06-INSPREM                  PIC $(5).99 BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(13).
           03  D07-DAY                      PIC Z9.
           03  FILLER                       PIC X(10).
           03  D07-MONTH                    PIC X(12).
           03  FILLER                       PIC X(02).
           03  D07-YEAR                     PIC 9(4).

       01  DETAIL-LINE-08                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(37).
           03  D08-MAKER-DOB                PIC 99/99/99 BLANK ZERO.
           03  FILLER                       PIC X(38).
           03  D08-COMAKER-DOB              PIC 99/99/99 BLANK ZERO.

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
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LSM1-FILE.
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

           MOVE 15     TO PST-WS-FORM-PAGE-MAX.
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
           PERFORM PRINT-PAGE-09-ROUTINE.
           PERFORM PRINT-PAGE-10-ROUTINE.
           PERFORM PRINT-PAGE-11-ROUTINE.
           PERFORM PRINT-PAGE-12-ROUTINE.
           PERFORM PRINT-PAGE-13-ROUTINE.
           PERFORM PRINT-PAGE-14-ROUTINE.
           PERFORM PRINT-PAGE-15-ROUTINE.

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

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-08-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-09-ROUTINE SECTION.
      ******************************************************************

           MOVE 09 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-09-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-09-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-10-ROUTINE SECTION.
      ******************************************************************

           MOVE 10 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-10-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-10-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-11-ROUTINE SECTION.
      ******************************************************************

           MOVE 11 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-11-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-11-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-12-ROUTINE SECTION.
      ******************************************************************

           MOVE 12 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-12-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-12-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-13-ROUTINE SECTION.
      ******************************************************************

           MOVE 13 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-13-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-13-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-14-ROUTINE SECTION.
      ******************************************************************

           MOVE 14 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-14-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-14-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-15-ROUTINE SECTION.
      ******************************************************************

           MOVE 15 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-15-ROUTINE-EXIT.

           PERFORM PRINT-PAGE-STD-ROUTINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-15-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-STD-ROUTINE SECTION.
      ******************************************************************

      * CHANGE FONT SIZE TO 12 CPI 
           MOVE 10                       TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER      TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                    TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * GROUP POLICY NUMBER
           MOVE "0020824"                TO D01-GROUP.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CERT. NO.
           MOVE LN-OWNBR                 TO D01-BRNO.
           MOVE LN-ACCTNO                TO D01-NUMBER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ADDRESS
           MOVE MAKER-ADDRESS            TO D02-ADR.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INSURED DEBTOR 
           MOVE MAKER-NAME-FIRST-LAST    TO D02-BW-NAME.
           MOVE "    X-XXXX"             TO D02-SSNO.
           MOVE MAKER-AGE                TO D02-AGE.
           MOVE MAKER-CSZ                TO D02-CSZ.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * JOINT ADDRESS
           IF LN-INSURED = "J"
              MOVE JOINT-ADDRESS            TO D02-ADR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * JOINT DEBTOR 
           IF LN-INSURED = "J"
              MOVE JOINT-NAME-FIRST-LAST    TO D02-BW-NAME
              MOVE "    X-XXXX"             TO D02-SSNO
              MOVE JOINT-AGE                TO D02-AGE
              MOVE JOINT-CSZ                TO D02-CSZ.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * FIRST BENEFICIARY
           MOVE SPACES TO WORDS-INPUT.
           IF ( BR-ADR2 NOT = SPACES )
              STRING GB-COMPANY-NAME ", ", BR-ADR1, ", ", BR-ADR2, ", ",
                  BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                  DELIMITED BY "  "      INTO WORDS-INPUT
           ELSE
              STRING GB-COMPANY-NAME ", ", BR-ADR1, ", ",
                  BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                  DELIMITED BY "  "      INTO WORDS-INPUT.
           MOVE 02     TO WORDS-WRAP-LINES.
           MOVE 46     TO WORDS-WRAP-LINE-SIZE(01).
           MOVE 46     TO WORDS-WRAP-LINE-SIZE(02).
           PERFORM WORDS-ROUTINE.
           MOVE WORDS-WRAP-LINE(01)      TO D03-BR-NAME.
      * SAVE SECOND LINE AS I NEED TO PERFORM FOR THE SECOND BENEFICIARY
           MOVE WORDS-WRAP-LINE(02) TO HOLD-FIRST-BENEFICIARY-2.

      * SECOND BENEFICIARY
           MOVE SPACES TO WORDS-INPUT.
           IF LN-INSPREM(1) NOT = 0
              PERFORM GET-SECOND-BENEFICIARY
              IF LSM-SECOND-BEN-NAME NOT = SPACES
                 STRING LSM-SECOND-BEN-NAME ", ",
                        LSM-SECOND-BEN-ADR,", ",
                        LSM-SECOND-BEN-CITY, ", ",
                        LSM-SECOND-BEN-STATE, " ",
                        LSM-SECOND-BEN-ZIP
                     DELIMITED BY "  "      INTO WORDS-INPUT
                 MOVE 02     TO WORDS-WRAP-LINES
                 MOVE 41     TO WORDS-WRAP-LINE-SIZE(01)
                 MOVE 41     TO WORDS-WRAP-LINE-SIZE(02)
                 PERFORM WORDS-ROUTINE
                 MOVE WORDS-WRAP-LINE(01)  TO D03-SECOND-BENE
              ELSE
                 MOVE SPACES               TO D03-SECOND-BENE
                                              WORDS-WRAP-LINE(02)
           ELSE
              MOVE SPACES               TO D03-SECOND-BENE
                                           WORDS-WRAP-LINE(02).
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HOLD-FIRST-BENEFICIARY-2  TO D03-BR-NAME.
           MOVE WORDS-WRAP-LINE(02)       TO D03-SECOND-BENE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * MONTHLY
           MOVE "X"                      TO D04-PAY-PER-BOX.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INTEREST RATE
           COMPUTE D04-RATE ROUNDED = LN-EFFRATE * 1.
           MOVE DISCLOSED-FINAMT         TO D04-NET-CASH.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AMORTIZED TERM
           MOVE LN-ORGTERM               TO D04-AMORTIZED.
           MOVE LN-1STPYDATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D04-1STPYDATE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LOAN PAYMENT 
           MOVE LN-REGPYAMT              TO D05-LOAN-PAY.
           MOVE "N/A"                    TO D05-BALLOON.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DECREASING LIFE
           IF LN-INSTYPES-DL
              MOVE LN-INSEFF-DATE(1)     TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY           TO D06-EFFDATE
              MOVE ITRM-INSTERM          TO D06-INSTERM
              MOVE LN-INSEXP-DATE(1)     TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY           TO D06-EXPDATE
              MOVE LN-INSCOVR(1)         TO D06-INSCOVR
              MOVE LN-INSPREM(1)         TO D06-INSPREM
              IF LN-INSURED = "J"
                 MOVE "X"                TO D06-JOINT
              ELSE
                 MOVE "X"                TO D06-SINGLE.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LEVEL LIFE 
           IF LN-INSTYPES-LL
              MOVE LN-INSEFF-DATE(1)     TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY           TO D06-EFFDATE
              MOVE ITRM-INSTERM          TO D06-INSTERM
              MOVE LN-INSEXP-DATE(1)     TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY           TO D06-EXPDATE
              MOVE LN-INSCOVR(1)         TO D06-INSCOVR
              MOVE LN-INSPREM(1)         TO D06-INSPREM
              IF LN-INSURED = "J"
                 MOVE "X"                TO D06-JOINT
              ELSE
                 MOVE "X"                TO D06-SINGLE.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DISABILITY 
           IF LN-INSTYPES-AH
              MOVE "X"                   TO D05-AH-BOX
              MOVE 2 TO ITRM-SUB
              PERFORM INS-TERM-CALCULATION
              MOVE LN-INSEFF-DATE(2)     TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY           TO D06-EFFDATE
              MOVE ITRM-INSTERM          TO D06-INSTERM
              MOVE LN-INSEXP-DATE(2)     TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY           TO D06-EXPDATE
              COMPUTE D06-AHBENE = LN-REGPYAMT - LN-MAINTFEE
              MOVE LN-INSPREM(2)         TO D06-INSPREM.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      * DO NOT PRINT DATE ON CANCELLATION PAGE 
           IF ( PST-WS-FORM-PAGE-NO = 15 )
              GO TO PRINT-PAGE-STD-ROUTINE-EXIT.


      * DATE THIS __ DAY OF __ 
           MOVE LN-LOANDATE             TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM        TO WS-MONTH-MM.
           PERFORM LOAD-WS-MONTH-TEXT. 
           MOVE DATE-YYYYMMDD-DD        TO D07-DAY.
           MOVE WS-MONTH-TEXT           TO D07-MONTH.
           MOVE DATE-YYYYMMDD-YYYY      TO D07-YEAR.
           MOVE 20 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * MAKER DATE OF BIRTH
           MOVE MAKER-BDATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO D08-MAKER-DOB.
           IF (JOINT-NAME-FIRST-LAST NOT = SPACES) AND
                (LN-INSURED = "J")
              MOVE JOINT-BDATE            TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY            TO D08-COMAKER-DOB.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-PAGE-STD-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       GET-SECOND-BENEFICIARY.
           MOVE LN-OWNBR     TO LSM-BRNO.
           MOVE LN-ACCTNO    TO LSM-ACCTNO.
           MOVE 1            TO LSM-SEQNO.
           PERFORM OPEN-LSM1-FILE.
           PERFORM READ-LSM1-FILE.
           PERFORM CLOSE-LSM1-FILE.
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
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
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
       COPY "LIBLP/LPLSM1RN.CPY".
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
