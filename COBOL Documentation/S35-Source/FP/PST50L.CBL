       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST50L.
       DATE-WRITTEN.  06-19-2023.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST50L.C
      *
      * DESCRIPTION   :  RIGHT TO CURE
      *                  PRINTS SECOND PAGE IF COMAKER
      *
      * STATE         :  WISCONSIN (WI)
      *
      * FORMS         :  609 RTC 
      *
      * REV:
      * BAH 2023.0619 NEW #1611
      * BAH 2023.0719 ADDED * TOTAL DUE AMOUNT SHOWN LINE AND SHIFTED
      *               SOME PRINTS DOWN, #1611A
      * BAH 2023.0915 IF A COMAKER, PRINT SECOND COPY WITH COMAKER INFO
      *               #1623
      * BAH 2025.0402 ALIGN THE TOP TO FIT IN WINDOW ENVELOPE AND CHANGE
      *               BR-NAME TO BR-STMNT1, #1706 S35Q-193
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
           "@(#)FP/PST50L.CBL S35 06/19/23-09:47:55 >".

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
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  WS-JUSTIFY                  PIC X VALUE "C".
       01  TODAYS-DATE-MMDDYY          PIC 9(6).
       01  C-S-Z                       PIC X(85).
       01  STATE                       PIC XX.
       01  ZIP                         PIC X(10).

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                      PIC X(300)     VALUE SPACES.

       01  DETAIL-LINE-02                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(6).
           03  D02-MAKER                      PIC X(40).
           03  FILLER                         PIC X(09).
           03  D-DATE                         PIC 99/99/99.
           03  D-NUMBER REDEFINES D-DATE      PIC Z(8).

       01  DETAIL-LINE-03                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(4).
           03  D-TOTALREPAY                   PIC $$$,$$9.99.
           03  FILLER                         PIC X(37).
           03  D-LOANDATE                     PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(5).
           03  D-BOX                          PIC X.
           03  FILLER                         PIC X(22).
           03  D-CURE-DATE                    PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(45).
           03  D-TABLE-DATE                   PIC 99/99/99.
           03  D-THRU  REDEFINES D-TABLE-DATE PIC X(8).
           03  FILLER                         PIC X(7).
           03  D-TABLE-AMT                    PIC $$$,$$9.99.

       01  DETAIL-LINE-06                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(12).
           03  D-AUTO                         PIC X(60).

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

           MOVE 01     TO PST-WS-FORM-PAGE-MAX.
           PERFORM PST-FR-FORM-INITIALIZATION.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-ROUTINE-EXIT.

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

           PERFORM PRINT-PAGE-01-ROUTINE.
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
           MOVE 01 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-01-ROUTINE-EXIT.

      * DATE:
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO TODAYS-DATE-MMDDYY D-DATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CREDITOR
      * THEY WANT "WORLD FINANCE CORPORATION" ON THE FIRST LINE
      * AND "OF WISCONSIN" ON THE SECOND LINE
           MOVE "WORLD FINANCE CORPORATION"  TO D02-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OF WISCONSIN"  TO D02-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              MOVE SPACES TO D02-MAKER
              STRING BR-ADR1," ",BR-ADR2 DELIMITED BY "  "
                 INTO D02-MAKER
           ELSE
              MOVE BR-ADR1 TO D02-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-CITY TO C-S-Z.
           MOVE BR-STATE TO STATE.
           MOVE BR-ZIP TO ZIP.
           PERFORM CAT-CSZ.
           MOVE C-S-Z                       TO D02-MAKER.
      * ACCOUNT#:
           MOVE LN-ACCTNO                    TO D-NUMBER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TO:
           MOVE MAKER-NAME-FIRST-LAST        TO D02-MAKER.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-ADDRESS                TO D02-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-CSZ                    TO D02-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * OUR RECORDS SHOW YOU ARE IN DEFAULT ON THE CREDIT TRANSACTION
      * WITH US DATED __
           MOVE LN-LOANDATE                  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                  TO D-LOANDATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * IN THE ORIGINAL AMOUNT OF $___
           MOVE TOTAL-REPAY                  TO D-TOTALREPAY.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NONPAYMENT OF AMOUNT(S) DUE
           MOVE "X" TO D-BOX.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * YOU MAY CURE THE DEFAULT ON OR BEFORE ___
           MOVE TODAYS-DATE-MMDDYY TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE 15            TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-CURE-DATE.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PAYING:
           MOVE "X" TO D-BOX.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * F11 DUE DATE
           MOVE NO-TRUE-DUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-TABLE-DATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "  THRU  " TO D-THRU.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE TODAYS-DATE-MMDDYY TO D-TABLE-DATE.
           COMPUTE D-TABLE-AMT = NO-TDUEAMT - NO-TDUELC.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DELINQUENCY CHARGE
           MOVE SPACES    TO D-THRU.
           MOVE NO-TDUELC TO D-TABLE-AMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TOTAL DUE
           MOVE SPACES     TO D-THRU.
           MOVE NO-TDUEAMT TO D-TABLE-AMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DOING THE FOLLOWING:
           MOVE "X" TO D-BOX.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NOTICE OF REPOSSESSION OF MOTOR VEHICLE
           IF AUTO-MAKE(1) = SPACES AND
               AUTO-MODEL(1) = SPACES AND
                AUTO-ID(1) = SPACES
              MOVE "N/A" TO D-AUTO
           ELSE
              STRING AUTO-YEAR(1)," ",AUTO-MAKE(1), " ",AUTO-MODEL(1),
                      " ",AUTO-ID(1) DELIMITED BY "   " INTO D-AUTO.
           MOVE 13 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-01-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-02-ROUTINE SECTION.
      ******************************************************************

      * THIS PAGE IS FOR A COMAKER/ENDORSER

      * SAME FORM AS PAGE 1
           MOVE 01 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-01-ROUTINE-EXIT.

      * DATE:
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO TODAYS-DATE-MMDDYY D-DATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CREDITOR
      * THEY WANT "WORLD FINANCE CORPORATION" ON THE FIRST LINE
      * AND "OF WISCONSIN" ON THE SECOND LINE
           MOVE "WORLD FINANCE CORPORATION"  TO D02-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OF WISCONSIN"  TO D02-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              MOVE SPACES TO D02-MAKER
              STRING BR-ADR1," ",BR-ADR2 DELIMITED BY "  "
                 INTO D02-MAKER
           ELSE
              MOVE BR-ADR1 TO D02-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-CITY TO C-S-Z.
           MOVE BR-STATE TO STATE.
           MOVE BR-ZIP TO ZIP.
           PERFORM CAT-CSZ.
           MOVE C-S-Z                       TO D02-MAKER.
      * ACCOUNT#:
           MOVE LN-ACCTNO                    TO D-NUMBER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TO:
           IF ( JOINT-SSNO NOT = ZEROES )
              MOVE JOINT-NAME-FIRST-LAST     TO D02-MAKER
           ELSE
           IF ( ENDR1-SSNO NOT = ZEROES )
              MOVE ENDR1-NAME-FIRST-LAST     TO D02-MAKER.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( JOINT-SSNO NOT = ZEROES )
              MOVE JOINT-ADDRESS             TO D02-MAKER
           ELSE
           IF ( ENDR1-SSNO NOT = ZEROES )
              MOVE ENDR1-ADDRESS             TO D02-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( JOINT-SSNO NOT = ZEROES )
              MOVE JOINT-CSZ                 TO D02-MAKER
           ELSE
           IF ( ENDR1-SSNO NOT = ZEROES )
              MOVE ENDR1-CSZ                 TO D02-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * OUR RECORDS SHOW YOU ARE IN DEFAULT ON THE CREDIT TRANSACTION
      * WITH US DATED __
           MOVE LN-LOANDATE                  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                  TO D-LOANDATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * IN THE ORIGINAL AMOUNT OF $___
           MOVE TOTAL-REPAY                  TO D-TOTALREPAY.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NONPAYMENT OF AMOUNT(S) DUE
           MOVE "X" TO D-BOX.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * YOU MAY CURE THE DEFAULT ON OR BEFORE ___
           MOVE TODAYS-DATE-MMDDYY TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE 15            TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-CURE-DATE.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PAYING:
           MOVE "X" TO D-BOX.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * F11 DUE DATE
           MOVE NO-TRUE-DUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-TABLE-DATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "  THRU  " TO D-THRU.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE TODAYS-DATE-MMDDYY TO D-TABLE-DATE.
           COMPUTE D-TABLE-AMT = NO-TDUEAMT - NO-TDUELC.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DELINQUENCY CHARGE
           MOVE SPACES    TO D-THRU.
           MOVE NO-TDUELC TO D-TABLE-AMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TOTAL DUE
           MOVE SPACES     TO D-THRU.
           MOVE NO-TDUEAMT TO D-TABLE-AMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DOING THE FOLLOWING:
           MOVE "X" TO D-BOX.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NOTICE OF REPOSSESSION OF MOTOR VEHICLE
           IF AUTO-MAKE(1) = SPACES AND
               AUTO-MODEL(1) = SPACES AND
                AUTO-ID(1) = SPACES
              MOVE "N/A" TO D-AUTO
           ELSE
              STRING AUTO-YEAR(1)," ",AUTO-MAKE(1), " ",AUTO-MODEL(1),
                      " ",AUTO-ID(1) DELIMITED BY "   " INTO D-AUTO.
           MOVE 13 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-02-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
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
