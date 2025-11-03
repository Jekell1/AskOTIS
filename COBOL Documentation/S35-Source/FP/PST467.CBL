       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST467.
       AUTHOR.        AMM.
       DATE-WRITTEN.  10-14-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST467.C
      *
      * DESCRIPTION   :  INVOLUNTARY UNEMPLOYMENT INSURANCE (IUI)
      *                  LYNDON SOUTHERN
      *
      * STATE         :  NEW MEXICO  (NM)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.1021 {PR#01467}
      *
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
           "@(#)FP/PST467.CBL S34 06/17/21-12:35:28 >".

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
      *     D E T A I L - W O R L D - A C C E P T A N C E  ( W A C )
      *
      *-----------------------------------------------------------------
      *    CPI     : 10 CPI   [PIC X(80)]
      *    CUSTOMER: WORLD ACCEPTANCE
      *    FMNO    : ***
      *    STATE   : NEW MEXICO   (NM)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(04).
           03  D01-ADDRESS                  PIC X(74).
           03  FILLER                       REDEFINES D01-ADDRESS.
               05  FILLER                   PIC X(18).
               05  D01-CREDITOR             PIC X(56).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(04).
           03  D02-ACCTNO                   PIC Z(6).
           03  FILLER                       PIC X(23).
           03  D02-TERM                     PIC Z(3).
           03  FILLER                       PIC X(12).
           03  D02-EFFDATE                  PIC 99/99/99.
           03  FILLER                       PIC X(10).
           03  D02-EXPDATE                  PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(04).
           03  D03-DEBTOR                   PIC X(25).
           03  FILLER                       PIC X(03).
           03  D03-MONTHLY                  PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(06).
           03  D03-PREMIUM                  PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(06).
           03  D03-DEBT                     PIC ZZZ,ZZ9.99.

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
           USE AFTER ERROR PROCEDURE ON PST-FILE.

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
           CLOSE PST-FILE.

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

           PERFORM PRINT-PAGE-STD-ROUTINE.

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

           PERFORM PRINT-PAGE-STD-ROUTINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-06-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-STD-ROUTINE SECTION.
      ******************************************************************

           MOVE BR-NAME                      TO D01-CREDITOR.
           MOVE 10 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           STRING BR-ADR1, " ", BR-ADR2, ", ", BR-CITY,
               ", ", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO D01-ADDRESS.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-ACCTNO                    TO D02-ACCTNO.
           MOVE INS-TERM(4)                  TO D02-TERM.
           MOVE LN-INSEFF-DATE(4)            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                  TO D02-EFFDATE.
           MOVE LN-INSEXP-DATE(4)            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                  TO D02-EXPDATE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-NAME-FIRST-LAST        TO D03-DEBTOR.
           MOVE LN-REGPYAMT                  TO D03-MONTHLY.
           MOVE LN-INSPREM(4)                TO D03-PREMIUM.
           MOVE LN-LNAMT                     TO D03-DEBT.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-ADDRESS                TO D03-DEBTOR.
           MOVE MDTE-DATE                    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                  TO D02-EXPDATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-CSZ                    TO D03-DEBTOR.
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
