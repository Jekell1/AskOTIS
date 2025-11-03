       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST338.
       AUTHOR.        AMM.
       DATE-WRITTEN.  03-31-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST338.C
      *
      * DESCRIPTION   :  PERSONAL PROPERTY 
      *                  LYNDON SOUTHERN
      *
      * STATE         :  TENNESSEE    (TN )
      *
      * FMNO          :  205 - ONLINE AGREEMENT
      *                  213 - BLANK
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0331 PR#1428 CALLED FROM PRT00T
      * BAH 2023.1214 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1635
      *               ONLY PRINT IF PP OR O1 WAS WRITTEN
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
           "@(#)FP/PST338.CBL S35 04/25/22-13:23:54 >".

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
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
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

      *=================================================================
      *
      *    D E T A I L - W O R L D - A C C E P T A N C E ( W A C )
      *
      *-----------------------------------------------------------------
      *    CPI     : 12 CPI   [PIC X(96)]
      *    CUSTOMER: WORLD ACCEPTANCE CORP.
      *    FMNO    : 205
      *    STATE   : TENNESSEE   (TN)
      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D01-CREDITOR                 PIC X(31).
           03  FILLER                       PIC X(01).
           03  D01-DEBTOR                   PIC X(29).
           03  FILLER                       PIC X(19).
           03  D01-EFF-MO                   PIC 99.
           03  FILLER                       PIC X(03).
           03  D01-EFF-DA                   PIC 99.
           03  FILLER                       PIC X(04).
           03  D01-EFF-YR                   PIC 99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D02-SINGLE-PP                PIC X(01).
           03  FILLER                       PIC X(08).
           03  D02-DUAL-PP                  PIC X(01).
           03  FILLER                       PIC X(10).
           03  D02-SINGLE-INT               PIC X(01).
           03  FILLER                       PIC X(12).
           03  D02-PP-COVR                  PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(03).
           03  D02-VEH-COVR                 PIC ZZZZ9.99.
           03  FILLER                       PIC X(03).
           03  D02-PP-PREM                  PIC ZZ,ZZ9.99.
           03  FILLER                       PIC X(02).
           03  D02-VEH-PREM                 PIC ZZ,ZZ9.99.
           03  FILLER                       PIC X(04).
           03  D02-TOTAL-PREM               PIC ZZ,ZZ9.99.
           03  FILLER                       REDEFINES D02-TOTAL-PREM.
               05  FILLER                   PIC X(03).
               05  D02-TERM                 PIC ZZ9.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(04).
           03  D03-DEBT-AMT                 PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(10).
           03  D03-NUM-PAY                  PIC ZZ9.
           03  FILLER                       PIC X(21).
           03  D03-YEAR                     PIC 9999.
           03  FILLER                       PIC X(02).
           03  D03-MAKE                     PIC X(13).
           03  FILLER                       PIC X(01).
           03  D03-MODEL                    PIC X(12).
           03  FILLER                       PIC X(01).
           03  D03-VIN                      PIC X(17).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(28).
           03  D04-DESCRIPTION              PIC X(65).

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
           PERFORM CLOSE-DL1-FILE.
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
      *
      *    P R I N T - R O U T I N E
      *
      ******************************************************************
       PRINT-ROUTINE SECTION.

           IF LN-INSPREM(3) = 0 AND LN-INSPREM(4) = 0
              GO TO PRINT-ROUTINE-EXIT.

           MOVE 06     TO PST-WS-FORM-PAGE-MAX.
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
           PERFORM PRINT-PAGE-04-ROUTINE.
           PERFORM PRINT-PAGE-05-ROUTINE.
           PERFORM PRINT-PAGE-06-ROUTINE.

           GO TO PRINT-ROUTINE-PAGE-COPY.

       PRINT-ROUTINE-END.

           PERFORM PRINT-PST-FILE.

       PRINT-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - P A G E - 0 1 - R O U T I N E
      *
      ******************************************************************
       PRINT-PAGE-01-ROUTINE SECTION.
 
           MOVE 01 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-01-ROUTINE-EXIT.

           PERFORM PRINT-PAGE-STD-ROUTINE.
  
           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-01-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - P A G E - 0 2 - R O U T I N E
      *
      ******************************************************************
       PRINT-PAGE-02-ROUTINE SECTION.
 
           MOVE 02 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-02-ROUTINE-EXIT.

           PERFORM PRINT-PAGE-STD-ROUTINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-02-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - P A G E - 0 3 - R O U T I N E
      *
      ******************************************************************
       PRINT-PAGE-03-ROUTINE SECTION.
 
           MOVE 03 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-03-ROUTINE-EXIT.

           PERFORM PRINT-PAGE-STD-ROUTINE.
  
           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-03-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - P A G E - 0 4 - R O U T I N E
      *
      ******************************************************************
       PRINT-PAGE-04-ROUTINE SECTION.
 
           MOVE 04 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-04-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-04-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - P A G E - 0 5 - R O U T I N E
      *
      ******************************************************************
       PRINT-PAGE-05-ROUTINE SECTION.
 
           MOVE 05 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-05-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-05-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - P A G E - 0 6 - R O U T I N E
      *
      ******************************************************************
       PRINT-PAGE-06-ROUTINE SECTION.
 
           MOVE 06 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-06-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-06-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - P A G E - S T D - R O U T I N E
      *
      ******************************************************************
       PRINT-PAGE-STD-ROUTINE SECTION.

      * CHANGE FONT SIZE TO 12 CPI 
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           MOVE BR-NAME                      TO D01-CREDITOR.
           IF NOT ( IP-FMNO = 213 )
              MOVE MAKER-NAME-FIRST-LAST     TO D01-DEBTOR.
           MOVE 13 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE BR-ADR1                      TO D01-CREDITOR.
           IF NOT ( IP-FMNO = 213 )
              MOVE MAKER-ADDRESS             TO D01-DEBTOR
              IF ( LN-INSEFF-DATE(3) NOT = ZEROES )
                 MOVE LN-INSEFF-DATE(3)      TO DATE-YYYYMMDD
              ELSE
                 MOVE LN-INSEFF-DATE(4)      TO DATE-YYYYMMDD
              END-IF
              MOVE DATE-YYYYMMDD-MM          TO D01-EFF-MO
              MOVE DATE-YYYYMMDD-DD          TO D01-EFF-DA
              MOVE DATE-YYYYMMDD-YY          TO D01-EFF-YR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           IF ( BR-ADR2 NOT = SPACES )
              MOVE BR-ADR2                   TO D01-CREDITOR
           ELSE
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D01-CREDITOR.
           IF NOT ( IP-FMNO = 213 )
              MOVE MAKER-CSZ                 TO D01-DEBTOR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           IF ( BR-ADR2 NOT = SPACES )
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D01-CREDITOR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      ****************************************************
           IF ( IP-FMNO = 213 )
              GO TO PRINT-PAGE-STD-ROUTINE-EXIT.
      ****************************************************

           IF ( INS-TERM(3) NOT = ZEROES )
              MOVE INS-TERM(3)               TO D02-TERM
           ELSE
              MOVE INS-TERM(4)               TO D02-TERM.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      * TOTAL PROPERTY 
           COMPUTE D02-TOTAL-PREM = LN-INSPREM(3) + LN-INSPREM(4).
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           IF ( LN-INSTYPES-PP )
              MOVE "X"                       TO D02-SINGLE-PP
           ELSE
           IF ( LN-INSTYPES-PP2 )
              MOVE "X"                       TO D02-DUAL-PP.
           IF ( LN-INSTYPES-O1 )
              MOVE "X"                       TO D02-SINGLE-INT.
           MOVE LN-INSCOVR(3)                TO D02-PP-COVR.
           MOVE LN-INSCOVR(4)                TO D02-VEH-COVR.
           MOVE LN-INSPREM(3)                TO D02-PP-PREM.
           MOVE LN-INSPREM(4)                TO D02-VEH-PREM.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE LN-LNAMT                     TO D03-DEBT-AMT.
           MOVE LN-ORGTERM                   TO D03-NUM-PAY.
           IF ( LN-INSPREM(4) NOT = ZEROES )
              MOVE AUTO-YEAR(1)              TO D03-YEAR
              MOVE AUTO-MAKE(1)              TO D03-MAKE
              MOVE AUTO-MODEL(1)             TO D03-MODEL
              MOVE AUTO-ID(1)                TO D03-VIN.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE CODE-DESC-OF-COLLATERAL       TO D04-DESCRIPTION.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
       PRINT-PAGE-STD-ROUTINE-EXIT.
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
       COPY "LIBLP/LPDLGS_SQL.CPY".
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
       COPY "LIBLP/LPDL1RN.CPY".
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
