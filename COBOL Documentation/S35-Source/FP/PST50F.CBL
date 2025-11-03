       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST50F.
       AUTHOR.        AMM.
       DATE-WRITTEN.  11-17-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST50F.C
      *
      * DESCRIPTION   :  FIRST NOTICE OF DEFAULT & RIGHT TO CURE
      *                  PRINTS SECOND PAGE IF COMAKER
      *
      * STATE         :  MISSOURI    (MO)
      *
      * FORMS         :  611 RTC FIRST NOTICE
      *                  612 RTC SECOND NOTICE
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.1118 PR#1473 CALLED FROM PRT01U
      * BAH 2023.0227 SET PST-LM-MEMO-SWITCH TO ON, #1593
      * BAH 2023.0915 IF A COMAKER, PRINT SECOND COPY WITH COMAKER INFO
      *               #1623
      * BAH 2024.0521 CHANGED D02-ACCTNO BACK TO Z(5)9 S35Q-32
      * BAH 2025.0318 CHANGED TOP HALF TO FIT IN WINDOW ENVELOPE #1704 S35Q-181
      * BAH 2025.0408 PRINT BRANCH ADR2 #1707 S35Q-195
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
           "@(#)FP/PST50F.CBL S35 03/18/25-09:32:37 >".

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

       01  WS-JUSTIFY                  PIC X VALUE "C".
       01  WS-BRANCH                   PIC X(80).
       01  C-S-Z                       PIC X(85).
       01  STATE                       PIC XX.
       01  ZIP                         PIC X(10).

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
      *    FMNO    : 611/612
      *    STATE   : MISSOURI   (MO)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(04).
           03  D01-MAKER                    PIC X(40).
           03  FILLER                       PIC X(13).
           03  D01-DATE                     PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(11).
           03  D02-ACCTNO                   PIC ZZZZZ9.
           03  FILLER                       PIC X(24).
           03  D02-LAST-DAY-2               PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(06).
           03  D03-AMOUNT                   PIC ZZZ,ZZ9.99.
           03  FILLER                       REDEFINES D03-AMOUNT.
               05  D03-LAST-DAY-1           PIC 99/99/99.
           03  FILLER                       PIC X(10).
           03  D03-NOTE-DATED               PIC 99/99/99.
           03  FILLER                       PIC X(03).
           03  D03-MANAGER                  PIC X(30).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(13).
           03  D04-ORGTERM                  PIC ZZ9.
           03  FILLER                       PIC X(12).
           03  D04-PAYMENT                  PIC Z,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(04).
           03  D05-PHONE                    PIC /999//999/9999.

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

           MOVE 02     TO PST-WS-FORM-PAGE-MAX.
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

      *- - - - - - - - - - - - - - - - - - - - -{ SETUP PAGE COPY LOOP }
           MOVE ZEROES TO PAGE-COPY-ELE.
           MOVE 01     TO PAGE-COPY-MAX.

       PRINT-ROUTINE-PAGE-COPY.

           ADD 01 TO PAGE-COPY-ELE.

           IF ( PAGE-COPY-ELE > PAGE-COPY-MAX )
              GO TO PRINT-ROUTINE-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ PRINT PAGES  }
           PERFORM PRINT-PAGE-01-ROUTINE.
      * PRINT COMAKER PAGE
           IF (JOINT-SSNO NOT = 0) OR (ENDR1-SSNO NOT = 0)
              PERFORM PRINT-PAGE-02-ROUTINE.

           GO TO PRINT-ROUTINE-PAGE-COPY.

       PRINT-ROUTINE-END.

           PERFORM PRINT-PST-FILE.

       PRINT-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-01-ROUTINE SECTION.
      ******************************************************************

      *  611 RTC FIRST NOTICE
      *  612 RTC SECOND NOTICE
           IF ( IP-FMNO = 611 )
              MOVE 01 TO PST-WS-FORM-PAGE-NO
           ELSE
              MOVE 02 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-01-ROUTINE-EXIT.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO D01-DATE.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR1 TO D01-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-CITY TO C-S-Z.
           MOVE BR-STATE TO STATE.
           MOVE BR-ZIP TO ZIP.
           PERFORM CAT-CSZ.
           IF BR-ADR2 NOT = SPACES
              MOVE BR-ADR2 TO D01-MAKER
           ELSE
              MOVE C-S-Z   TO D01-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              MOVE C-S-Z   TO D01-MAKER
           ELSE
              MOVE BR-PHONE TO D05-PHONE
              INSPECT D05-PHONE REPLACING FIRST "/" BY "("
              INSPECT D05-PHONE REPLACING FIRST "/" BY ")"
              INSPECT D05-PHONE REPLACING ALL "/" BY "-".
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              MOVE BR-PHONE TO D05-PHONE
              INSPECT D05-PHONE REPLACING FIRST "/" BY "("
              INSPECT D05-PHONE REPLACING FIRST "/" BY ")"
              INSPECT D05-PHONE REPLACING ALL "/" BY "-".
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-NAME-FIRST-LAST          TO D01-MAKER.
           MOVE 07 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-ADDRESS                  TO D01-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-CSZ                      TO D01-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-ACCTNO                      TO D02-ACCTNO.
           MOVE 08 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * GROSS NOTE
           MOVE TOTAL-REPAY         TO D03-AMOUNT.
      * DATED
           MOVE LN-LOANDATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D03-NOTE-DATED.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INSTALLMENTS ___ PAYMENTS
           MOVE LN-ORGTERM                     TO D04-ORGTERM.
      * OF $    / MONTH
           MOVE LN-REGPYAMT                    TO D04-PAYMENT.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * IS THE AMOUNT DUE NOW
           MOVE NO-TDUEAMT                     TO D03-AMOUNT.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * IS THE LAST DAY FOR PAYMENT
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE 20 TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE                      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D03-LAST-DAY-1.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * IF YOU PAY THE AMOUNT DUE NOW OF $
           MOVE NO-TDUEAMT                     TO D03-AMOUNT.
      * BY THE LAST DAY FOR PAYMENT ___
           MOVE DATE-MMDDYY                    TO D02-LAST-DAY-2.
           MOVE 09 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-MANAGER                     TO D03-MANAGER.
           MOVE 12 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-01-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-02-ROUTINE SECTION.
      ******************************************************************

      * SAME AS PAGE 1

      *  611 RTC FIRST NOTICE
      *  612 RTC SECOND NOTICE
           IF ( IP-FMNO = 611 )
              MOVE 01 TO PST-WS-FORM-PAGE-NO
           ELSE
              MOVE 02 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-02-ROUTINE-EXIT.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO D01-DATE.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR1 TO D01-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-CITY TO C-S-Z.
           MOVE BR-STATE TO STATE.
           MOVE BR-ZIP TO ZIP.
           PERFORM CAT-CSZ.
           IF BR-ADR2 NOT = SPACES
              MOVE BR-ADR2 TO D01-MAKER
           ELSE
              MOVE C-S-Z   TO D01-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              MOVE C-S-Z   TO D01-MAKER
           ELSE
              MOVE BR-PHONE TO D05-PHONE
              INSPECT D05-PHONE REPLACING FIRST "/" BY "("
              INSPECT D05-PHONE REPLACING FIRST "/" BY ")"
              INSPECT D05-PHONE REPLACING ALL "/" BY "-".
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              MOVE BR-PHONE TO D05-PHONE
              INSPECT D05-PHONE REPLACING FIRST "/" BY "("
              INSPECT D05-PHONE REPLACING FIRST "/" BY ")"
              INSPECT D05-PHONE REPLACING ALL "/" BY "-".
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( JOINT-SSNO NOT = ZEROES )
              MOVE JOINT-NAME-FIRST-LAST     TO D01-MAKER
           ELSE
           IF ( ENDR1-SSNO NOT = ZEROES )
              MOVE ENDR1-NAME-FIRST-LAST     TO D01-MAKER.
           MOVE 07 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( JOINT-SSNO NOT = ZEROES )
              MOVE JOINT-ADDRESS             TO D01-MAKER
           ELSE
           IF ( ENDR1-SSNO NOT = ZEROES )
              MOVE ENDR1-ADDRESS             TO D01-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( JOINT-SSNO NOT = ZEROES )
              MOVE JOINT-CSZ                 TO D01-MAKER
           ELSE
           IF ( ENDR1-SSNO NOT = ZEROES )
              MOVE ENDR1-CSZ                 TO D01-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-ACCTNO                      TO D02-ACCTNO.
           MOVE 08 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * GROSS NOTE
           MOVE TOTAL-REPAY         TO D03-AMOUNT.
      * DATED
           MOVE LN-LOANDATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D03-NOTE-DATED.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INSTALLMENTS ___ PAYMENTS
           MOVE LN-ORGTERM                     TO D04-ORGTERM.
      * OF $    / MONTH
           MOVE LN-REGPYAMT                    TO D04-PAYMENT.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * IS THE AMOUNT DUE NOW
           MOVE NO-TDUEAMT                     TO D03-AMOUNT.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * IS THE LAST DAY FOR PAYMENT
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE 20 TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE                      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D03-LAST-DAY-1.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * IF YOU PAY THE AMOUNT DUE NOW OF $
           MOVE NO-TDUEAMT                     TO D03-AMOUNT.
      * BY THE LAST DAY FOR PAYMENT ___
           MOVE DATE-MMDDYY                    TO D02-LAST-DAY-2.
           MOVE 09 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-MANAGER                     TO D03-MANAGER.
           MOVE 12 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-02-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      *********************************************
       MISC-ROUTINES SECTION.
      *********************************************
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.

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
