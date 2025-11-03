       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST263.
       AUTHOR.        AMM.
       DATE-WRITTEN.  12-07-2016.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST263.C
      *
      * DESCRIPTION   :  CL/AH POLICY
      *                  LIFE OF THE SOUTH
      *
      * STATE         :  SOUTH CAROLINA   (SC)
      *
      *=================================================================
      * REV:
      * AMM 2016.1207 PR#1207 CALLED FROM PRT00T
      * AMM 2018.0605 PR#1320A DECREASED FONT SIZE TO 12 CPI
      * BAH 2023.1110 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1630
      * BAH 2024.0109 PRINT SECOND BENEFICIARY FROM LS MISC FILE #1637
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
           "@(#)FP/PST263.CBL S35 08/07/23-12:31:18 >".

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


       01  COMPANY-NAME                PIC X(40).
       01  SUB1                        PIC 99.

       01  DETAIL-LINE                      PIC X(300)     VALUE SPACES.

      *=================================================================
      *
      *     D E T A I L - W O R L D - A C C E P T A N C E  ( W A C )
      *
      *-----------------------------------------------------------------
      *    CPI     : 12 CPI   [PIC X(96)]
      *    CUSTOMER: WORLD ACCEPTANCE CORP.
      *    FMNO    : ***
      *    STATE   : SOUTH CAROLINA  (SC)
      *=================================================================
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(05).
           03  D01-MAKER-INFO                 PIC X(31).
           03  FILLER                         PIC X(21).
           03  D01-JOINT-INFO                 PIC X(22).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(05).
           03  D02-EFFDATE                    PIC 99/99/99.
           03  FILLER                         PIC X(17).
           03  D02-TERM                       PIC ZZ9.
           03  FILLER                         PIC X(11).
           03  D02-BR-NAME                    PIC X(40).

       01  DETAIL-LINE-02A                    REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(42).
           03  D02-SECOND-BEN                 PIC X(50).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(37).
           03  D03-COUNTY                     PIC X(03).
           03  FILLER                         PIC X(02).
           03  D03-BW-AGE                     PIC ZZ9.
           03  FILLER                         PIC X(03).
           03  D03-DOB-MM                     PIC 99.
           03  FILLER                         PIC X(03).
           03  D03-DOB-YY                     PIC 99.
           03  FILLER                         PIC X(25).
           03  D03-CO-AGE                     PIC ZZ9.
           03  FILLER                         PIC X(03).
           03  D03-CO-DOB-MM                  PIC 99.
           03  FILLER                         PIC X(03).
           03  D03-CO-DOB-YY                  PIC 99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(13).
           03  D04-DAYS                       PIC Z9.
           03  FILLER                         PIC X(15).
           03  D04-BOX                        PIC X(01).
           03  FILLER                         PIC X(38).
           03  D04-COVERAGE                   PIC ZZ,ZZ9.99.
           03  FILLER                         PIC X(05).
           03  D04-PREMIUM                    PIC ZZ,ZZ9.99.

       COPY "LIBFP/PST_COPYW.CPY".
       COPY "LIBFP/PST_INFOW.CPY".
       COPY "LIBFP/PST_FORMW.CPY".

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
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LP1-FILE.
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

           MOVE 05     TO PST-WS-FORM-PAGE-MAX.
           PERFORM PST-FR-FORM-INITIALIZATION.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ LOAD ACCOUNT INFO. }
           PERFORM PST-INFO-MODULE.

      *- - - - - - - - - - - - - - - - - - -{ NUMBER OF CHECKS ON FORM }
           MOVE ZEROES TO CHECK-MAX-CUSTOMIZE.
           PERFORM PST-INFO-CUSTOMIZE-CHECKS.

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
      *
      *    P R I N T - P A G E - S T D - R O U T I N E
      *
      *    DESC:  USED TO PRINT STANDARD INFO TO MULTIPLE PAGES.
      ******************************************************************
       PRINT-PAGE-STD-ROUTINE SECTION.

           MOVE 10.00                   TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER     TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                   TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           MOVE GB-COMPANY-NAME            TO COMPANY-NAME.
           CALL "C$JUSTIFY" USING COMPANY-NAME, "L".
           MOVE COMPANY-NAME               TO D02-BR-NAME.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           STRING BR-ADR1, " ", BR-ADR2
              DELIMITED BY "  " INTO D02-BR-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO D02-BR-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE MAKER-NAME-FIRST-LAST      TO D01-MAKER-INFO.
           IF ( JOINT-NAME-FIRST-LAST NOT = SPACES)
              MOVE JOINT-NAME-FIRST-LAST   TO D01-JOINT-INFO.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE MAKER-ADDRESS              TO D01-MAKER-INFO.
           IF ( JOINT-NAME-FIRST-LAST NOT = SPACES)
              MOVE JOINT-ADDRESS           TO D01-JOINT-INFO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE MAKER-CSZ                  TO D01-MAKER-INFO.
           IF ( JOINT-NAME-FIRST-LAST NOT = SPACES)
              MOVE JOINT-CSZ               TO D01-JOINT-INFO.
           MOVE MAKER-COUNTY               TO D03-COUNTY.
           MOVE MAKER-AGE                  TO D03-BW-AGE.
           MOVE MAKER-BDATE-MM             TO D03-DOB-MM.
           MOVE MAKER-BDATE-YY             TO D03-DOB-YY.
           IF ( JOINT-NAME-FIRST-LAST NOT = SPACES)
              MOVE JOINT-AGE               TO D03-CO-AGE
              MOVE JOINT-BDATE-MM          TO D03-CO-DOB-MM
              MOVE JOINT-BDATE-YY          TO D03-CO-DOB-YY.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           IF ( LN-INSPREM(1) NOT = ZEROES )
              MOVE LN-INSEFF-DATE(1)       TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY             TO D02-EFFDATE
              MOVE INS-TERM(1)             TO D02-TERM
              PERFORM GET-SECOND-BENEFICIARY
              MOVE LSM-SECOND-BEN-NAME     TO D02-SECOND-BEN
           ELSE
              MOVE LN-INSEFF-DATE(2)       TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY             TO D02-EFFDATE
              MOVE INS-TERM(2)             TO D02-TERM.
       
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           IF ( LN-INSTYPES-DL ) AND
              ( LN-INSURED NOT = "J" )
              MOVE "X" TO D04-BOX.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           IF ( LN-INSTYPES-DL ) AND
              ( LN-INSURED = "J" )
              MOVE "X" TO D04-BOX.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           IF ( LN-INSTYPES-LL ) AND
              ( LN-INSURED NOT = "J" )
              MOVE "X" TO D04-BOX.
           IF ( LN-INSPREM(1) NOT = ZEROES )
              MOVE LN-INSCOVR(1) TO D04-COVERAGE
              MOVE LN-INSPREM(1) TO D04-PREMIUM.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           IF ( LN-INSTYPES-LL ) AND
              ( LN-INSURED = "J" )
              MOVE "X" TO D04-BOX.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE 14                         TO D04-DAYS.
           IF ( LN-INSPREM(2) NOT = ZEROES )
              MOVE LN-REGPYAMT             TO D04-COVERAGE
              MOVE LN-INSPREM(2)           TO D04-PREMIUM.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-PAGE-STD-ROUTINE-EXIT.
           EXIT.

      ******************************
       GET-SECOND-BENEFICIARY SECTION.
      ******************************
           MOVE LN-OWNBR     TO LSM-BRNO.
           MOVE LN-ACCTNO    TO LSM-ACCTNO.
           MOVE 1            TO LSM-SEQNO.
           PERFORM OPEN-LSM1-FILE.
           PERFORM READ-LSM1-FILE.
           PERFORM CLOSE-LSM1-FILE.
           IF IO-FG = 9
              MOVE SPACES TO LSM-SECOND-BEN-NAME.

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
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
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
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
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
