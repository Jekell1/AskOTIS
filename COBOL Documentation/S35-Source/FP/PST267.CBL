       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST267.
       AUTHOR.        AMM.
       DATE-WRITTEN.  07-23-2019.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST267.C
      *
      * DESCRIPTION   :  CREDIT LIFE AND A&H GILA
      *                  LIFE OF THE SOUTH
      *
      * STATE         :  GEORGIA     (GA)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2019.0723 PR#1378 ** CALLED FROM LONP9U
      * AMM 2019.1018 PR#1397 FIXED AMOUNT TO PRINT COVERAGE INSTEAD OF
      *               PREMIUM
      * AMM 2021.0826 PR#1511 UPDATED ADDRESS AND GILA ACRONYM
      * BAH 2023.1213 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1635
      *               ONLY PRINT IF LIFE OR A&H WAS WRITTEN
      * BAH 2024.0411 PRINT SECOND BENEFICIARY NAME #1653
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
           "@(#)FP/PST267.CBL S35 12/14/23-08:08:45 >".

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

       01  DETAIL-LINE                      PIC X(300)     VALUE SPACES.

      *=================================================================
      *
      *     D E T A I L - W O R L D - A C C E P T A N C E  ( W A C )
      *
      *-----------------------------------------------------------------
      *    CPI     : 12 CPI   [PIC X(96)]
      *    CUSTOMER: WORLD ACCEPTANCE
      *    FMNO    : ***
      *    STATE   : GEORGIA    (GA)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(07).
           03  D01-INSURED                  PIC X(43).
           03  FILLER                       PIC X(02).
           03  D01-AGE                      PIC Z99.
           03  FILLER                       PIC X(02).
           03  D01-CREDITOR                 PIC X(24).
           03  FILLER                       PIC X(03).
           03  D01-BOX                      PIC X(01).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(08).
           03  D02-CLCOVR                   PIC Z(6).99.
           03  FILLER                       PIC X(02).
           03  D02-AHBEN                    PIC Z(6).99.
           03  FILLER                       PIC X(01).
           03  D02-EFFDATE                  PIC 99/99/99.
           03  FILLER                       PIC X(05).
           03  D02-TERM                     PIC ZZ9.
           03  FILLER                       PIC X(05).
           03  D02-CLPREM                   PIC Z(6).99.
           03  FILLER                       PIC X(01).
           03  D02-AHPREM                   PIC Z(6).99.
           03  FILLER                       PIC X(02).
           03  D02-TOTPREM                  PIC Z(6).99.

       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(07).
           03  D03-SECBEN-ADR               PIC X(50).
           03  D03-X REDEFINES D03-SECBEN-ADR.
               05  FILLER                   PIC X(16).
               05  D03-SECBEN-NAME          PIC X(34).

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
       PRINT-PAGE-STD-ROUTINE SECTION.
      ******************************************************************

      * CHANGE FONT SIZE TO 12 CPI - - - - - - - - - - - - - - - - - - -
           MOVE 10                      TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER     TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                   TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * INSURED DEBTOR
           MOVE MAKER-NAME-FIRST-LAST        TO D01-INSURED.
           MOVE MAKER-AGE                    TO D01-AGE.
      * CREDITOR 
           MOVE BR-NAME                      TO D01-CREDITOR.
           MOVE 08 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           STRING BR-ADR1, " ", BR-ADR2
              DELIMITED BY "  " INTO D01-CREDITOR.
      * SINGLE DL BOX
           IF ( LN-INSURED NOT = "J" ) AND
              ( LN-INSTYPES-DL )
              MOVE "X" TO D01-BOX.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO D01-CREDITOR.
      * SINGLE LL BOX
           IF ( LN-INSURED NOT = "J" ) AND
              ( LN-INSTYPES-LL )
              MOVE "X" TO D01-BOX.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * JOINT INSURED
           IF ( LN-INSURED = "J" )
              MOVE JOINT-NAME-FIRST-LAST     TO D01-INSURED
              MOVE JOINT-AGE                 TO D01-AGE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * JOINT DL BOX
           IF ( LN-INSURED = "J" ) AND
              ( LN-INSTYPES-DL )
              MOVE "X" TO D01-BOX.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * JOINT LL BOX 
           IF ( LN-INSURED = "J" ) AND
              ( LN-INSTYPES-LL )
              MOVE "X" TO D01-BOX.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 3 DAY WAITING PERIOD BOX-
           MOVE "X"                          TO D01-BOX.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( LN-INSPREM(1) NOT = ZEROES )
              MOVE LN-INSCOVR(1)     TO D02-CLCOVR
              MOVE INS-TERM(1)       TO D02-TERM
              MOVE LN-INSEFF-DATE(1) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO D02-EFFDATE
              MOVE LN-INSPREM(1)     TO D02-CLPREM.
           IF ( LN-INSPREM(2) NOT = ZEROES )
              COMPUTE D02-AHBEN = LN-REGPYAMT - LN-MAINTFEE
              MOVE INS-TERM(2)       TO D02-TERM
              MOVE LN-INSEFF-DATE(2) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO D02-EFFDATE
              MOVE LN-INSPREM(2)     TO D02-AHPREM.
           INSPECT D02-EFFDATE REPLACING ALL "/" BY " ".
           COMPUTE D02-TOTPREM = LN-INSPREM(1) + LN-INSPREM(2).
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SECOND BENEFICIARY
           IF LN-INSPREM(1) NOT = 0
              PERFORM GET-SECOND-BENEFICIARY
              MOVE LSM-SECOND-BEN-NAME TO D03-SECBEN-NAME.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LSM-SECOND-BEN-ADR NOT = SPACES OR
               LSM-SECOND-BEN-CITY NOT = SPACES
              STRING LSM-SECOND-BEN-ADR,", ",
                     LSM-SECOND-BEN-CITY,", ",
                     LSM-SECOND-BEN-STATE, " ",
                     LSM-SECOND-BEN-ZIP
                 DELIMITED BY "  " INTO D03-SECBEN-ADR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-PAGE-STD-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      ******************************************************************
       MISC-ROUTINE SECTION.
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
