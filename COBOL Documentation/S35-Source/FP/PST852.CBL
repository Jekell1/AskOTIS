       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST852.
       AUTHOR.        AMM.
       DATE-WRITTEN.  10-04-2021.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST852.C
      *
      * DESCRIPTION   :  WAGE DEMAND 
      *
      * STATE         :  ILLINOIS    (IL)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2021.1004 PR#1518
      * AMM 2022.0613 PR#1556 ADDED COMAKER TO 2ND COPY
      * JKC 2022.0616 PR#1556A
      *               MOVED THE `PERFORM PST-COPY-MISC-CRNORC.`
      *               BEFORE THE FIRST USE OF THE NOTICE RECORD
      *               FIELD SO THAT IT GET THE CORRECT NOTICE RECORD
      *               ON THE SECOND TIME THRU (CO-MAKER); NEED IT THERE
      *               SINCE FIRST TIME THRU CALLS CRNORC AFTER
      *               INCREMENTING THE DAYS BY -40.
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
           "@(#)FP/PST852.CBL S35 06/15/22-12:08:32 >".

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
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
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

       01  HOLD-SSNO.
           03   SSNO-5     PIC 9(5).
           03   SSNO-4     PIC 9(4).
       01  JUSTIFY-TYPE    PIC X VALUE "C".

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
      *    FMNO    : 132
      *    STATE   : ILLINOIS    (IL)
      *=================================================================
       01  DETAIL-LINE-01                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(15).
           03  D01-EMPLOYER               PIC X(30).
           03  FILLER                     PIC X(25).
           03  D01-BRNO                   PIC Z(5)9.
           03  D01-SLASH                  PIC X.
           03  D01-ACCTNO                 PIC Z(5)9.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(29).
           03  D02-EXECUTED-BY            PIC X(30).
           03  FILLER                     PIC X(09).
           03  D02-BORROWER               PIC X(30).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(30).
           03  D03-EXECUTED-DATE          PIC 99/99/99.
           03  FILLER                     PIC X(02).
           03  D03-MONTHS                 PIC ZZ9.
           03  FILLER                     PIC X(26).
           03  D03-CONTRACT-DATE          PIC 99/99/99.
           03  FILLER                     PIC X(04).
           03  D03-XX                     PIC X(07).
           03  D03-SSNO                   PIC 9(4).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(35).
           03  D04-DEBT-AMT               PIC $Z,ZZ9.99.
           03  FILLER                     PIC X(26).
           03  D04-PAY-AMT                PIC $Z,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(30).
           03  D05-SUM-OF                 PIC $Z,ZZ9.99.
           03  FILLER                     PIC X(36).
           03  D05-MADE-DATE              PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(14).
           03  D06-WHICH-AMT              PIC $Z,ZZ9.99.
           03  FILLER                     PIC X(54).
           03  D06-AMOUNT-OF              PIC $Z,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(28).
           03  D07-CREDITOR               PIC X(45).
           03  FILLER                     REDEFINES D07-CREDITOR.
               05  FILLER                 PIC X(16).
               05  D07-PHONE              PIC 999/999/9999.

       COPY "LIBFP/PST_COPYW.CPY".
       COPY "LIBFP/PST_INFOW.CPY".
       COPY "LIBFP/PST_FORMW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".

      ******************************************************************
      *
      *    P C L - S C R E E N - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPM_DEF.CPY".
       COPY "LIBLP/CHCKI_DEF.CPY".
       COPY "LIBLP/BCHKI_DEF.CPY".
       COPY "LIBLP/LONPM_WKS.CPY".
       COPY "LIBLP/CHCKI_WKS.CPY".
       COPY "LIBLP/BCHKI_WKS.CPY".

      ******************************************************************
      *    L I N K A G E - S E C T I O N
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/GETPRW.CPY".

      ******************************************************************
      *    S C R E E N - S E C T I O N
      ******************************************************************
       SCREEN SECTION.
       COPY "LIBLP/LONPM_SCN.CPY".
       COPY "LIBLP/CHCKI_SCN.CPY".
       COPY "LIBLP/BCHKI_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME PROG-BUF IP-BUF PRU-PATH.
      *-----------------------------------------------------------------
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PST-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-PC1-FILE.
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTP1-FILE.
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

           MOVE 01     TO PST-WS-FORM-PAGE-MAX.
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
           IF ( JOINT-SSNO NOT = ZEROES )
              MOVE 02     TO PAGE-COPY-MAX
           ELSE
              MOVE 01     TO PAGE-COPY-MAX.

       PRINT-ROUTINE-PAGE-COPY.

           ADD 01 TO PAGE-COPY-ELE.

           IF ( PAGE-COPY-ELE > PAGE-COPY-MAX )
              GO TO PRINT-ROUTINE-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ PRINT PAGES  }
           PERFORM PRINT-PAGE-01-ROUTINE.

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

           MOVE LN-OWNBR                    TO D01-BRNO.
           MOVE "/"                         TO D01-SLASH.
           MOVE LN-ACCTNO                   TO D01-ACCTNO.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( PAGE-COPY-ELE = 01 )
              MOVE MAKER-EMPLOYER           TO D01-EMPLOYER
              MOVE MAKER-NAME-FIRST-LAST    TO D02-BORROWER
           ELSE
              MOVE JOINT-EMPLOYER           TO D01-EMPLOYER
              MOVE JOINT-NAME-FIRST-LAST    TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "XXX-XX-"                   TO D03-XX.
           IF ( PAGE-COPY-ELE = 01 )
              MOVE MAKER-SSNO               TO HOLD-SSNO
              MOVE SSNO-4                   TO D03-SSNO
           ELSE
              MOVE JOINT-SSNO               TO HOLD-SSNO
              MOVE SSNO-4                   TO D03-SSNO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( PAGE-COPY-ELE = 01 )
              MOVE MAKER-NAME-FIRST-LAST    TO D02-EXECUTED-BY
           ELSE
              MOVE JOINT-NAME-FIRST-LAST    TO D02-EXECUTED-BY.
           CALL "C$JUSTIFY" USING D02-EXECUTED-BY, JUSTIFY-TYPE.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           MOVE DATE-MMDDYY                 TO D03-EXECUTED-DATE.
           MOVE LN-LOANDATE                 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                 TO D03-CONTRACT-DATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-COPY-MISC-CRNORC.

           MOVE NO-TDUEPOFF                 TO D04-DEBT-AMT.
           COMPUTE D04-PAY-AMT ROUNDED = LN-TOTPAYMNTD +
                                         LN-TOTEXCPAYMNTD.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-ORGTERM                  TO D03-MONTHS.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE NO-TDUEAMT                  TO D05-SUM-OF.
           MOVE LN-LAST-PYPA-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                 TO D05-MADE-DATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE NO-TDUEAMT                  TO D06-AMOUNT-OF.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NEED AMOUNT OF PAST DUE PAYMENTS THAT ARE MORE THAN 40 DAYS
      * PAST DUE

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD               TO NDTE-DATE.
           MOVE -40                         TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.

           MOVE LN-ACCTNO                   TO CRNOR2-ACCTNO.
           MOVE NDTE-DATE                   TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                          POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.
           MOVE NO-TDUEAMT                  TO D06-WHICH-AMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-NAME                     TO D07-CREDITOR.
           CALL "C$JUSTIFY" USING D07-CREDITOR, JUSTIFY-TYPE.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           STRING BR-ADR1, " ", BR-ADR2
              DELIMITED BY "  " INTO D07-CREDITOR.
           CALL "C$JUSTIFY" USING D07-CREDITOR, JUSTIFY-TYPE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO D07-CREDITOR.
           CALL "C$JUSTIFY" USING D07-CREDITOR, JUSTIFY-TYPE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-PHONE              TO D07-PHONE.
           INSPECT D07-PHONE REPLACING ALL "/" BY "-".
           CALL "C$JUSTIFY" USING D07-PHONE, JUSTIFY-TYPE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-01-ROUTINE-EXIT.
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
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
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
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
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
