       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST593.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-17-2017.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST593.C
      *
      * DESCRIPTION   :  NOTICE OF DEFAULT & RIGHT TO CURE 
      *                  610 - PRINTS PAGE 1, NEXT FORM 613
      *                  613 - PRINT EXACT PAGE AS PAGE 2
      *                  BOTH FORM #'S CALL PST593
      *
      * STATE         :  SOUTH CAROLINA   (SC)
      *
      *=================================================================
      * REV:
      * AMM 2017.0517 PR#1244 CALLED FROM PRT01U
      * AMM 2017.0713 PR#1244A ADDED 2 CHARACTERS TO PHONE NUMBER;
      *               TURNED ON MISCELLANEOUS CRNORC ROUTINE 
      * AMM 2017.0725 PR#1244B MOVED RECIPIENT LINE UP ONE LINE
      * BAH 2023.0227 SET PST-LM-MEMO-SWITCH TO ON, #1593
      * BAH 2024.0521 CHANGED D02-ACCTNO BACK TO ZZZZZ9 S35Q-32
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
           "@(#)FP/PST593.CBL S35 09/20/24-11:41:58 >".

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
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
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
      *    CUSTOMER: WORLD ACCEPTANCE CORP.
      *    FMNO    : ***
      *    STATE   : SOUTH CAROLINA  (SC)
      *=================================================================
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(08).
           03  D01-NAME                       PIC X(40).
           03  FILLER                         PIC X(20).
           03  D01-DATE                       PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(09).
           03  D02-LAST-DAY                   PIC 99/99/99.
           03  FILLER                         PIC X(16).
           03  D02-ACCTNO                     PIC ZZZZZ9.
           03  FILLER                         PIC X(04).
           03  D02-AMOUNT                     PIC Z,ZZ9.99.
           03  FILLER                         PIC X(04).
           03  D02-BALANCE                    PIC $ZZ,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(08).
           03  D03-PHONE                      PIC /999/999/9999.
           03  FILLER                         PIC X(24).
           03  D03-BRANCH                     PIC X(25).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
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
           USE AFTER ERROR PROCEDURE ON PST-FILE.

       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-PC1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
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

           MOVE 01     TO PST-WS-FORM-PAGE-MAX.
           PERFORM PST-FR-FORM-INITIALIZATION.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ LOAD ACCOUNT INFO. }
           PERFORM PST-INFO-MODULE.
           PERFORM PST-COPY-MISC-CRNORC.

           MOVE PST-LM-MEMO-SWITCH---ON    TO PST-LM-MEMO-SWITCH.

      *- - - - - - - - - - - - - - - - - - -{ NUMBER OF CHECKS ON FORM }
           MOVE ZEROES TO CHECK-MAX-CUSTOMIZE.
           PERFORM PST-INFO-CUSTOMIZE-CHECKS.

      *- - - - - - - - - - - - - - - - - - - - - - -{ REMOVE LASER TAG }
           MOVE PST-WS-TAG-MODE---OFF        TO PST-WS-TAG-MODE.

      *- - - - - - - - - - - - - - - - - - - - -{ SETUP PAGE COPY LOOP }
           MOVE ZEROES TO PAGE-COPY-ELE.
           MOVE 02     TO PAGE-COPY-MAX.

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

           MOVE EXT-CONAME-SYSDATE  TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D01-DATE.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE GB-COMPANY-NAME              TO D01-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR1                      TO D01-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO D01-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-PHONE TO D03-PHONE.
           INSPECT D03-PHONE REPLACING FIRST "/" BY "(".
           INSPECT D03-PHONE REPLACING FIRST "/" BY ")".
           INSPECT D03-PHONE REPLACING ALL "/" BY "-".
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-NAME-FIRST-LAST        TO D01-NAME.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-ADDRESS                TO D01-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-CSZ                    TO D01-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-ACCTNO                    TO D02-ACCTNO.
           COMPUTE D02-BALANCE = LN-CURBAL + LN-OTHBAL + LN-INTBAL
                               + LN-LCBAL.
           MOVE 11 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE EXT-CONAME-SYSDATE  TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO NDTE-DATE.
           MOVE 21                  TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D02-LAST-DAY.
           MOVE NO-TDUEAMT          TO D02-AMOUNT.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-NAME                      TO D03-BRANCH.
           MOVE 15 TO LN-FEED.
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
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
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
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
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
