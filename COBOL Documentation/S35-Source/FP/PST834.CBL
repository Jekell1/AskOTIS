       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST834.
       AUTHOR.        AMM.
       DATE-WRITTEN.  03-26-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST834.C
      *
      * DESCRIPTION   :  SECURITY AGREEMENT
      *
      * STATE         :  TEXAS    (TX)
      *
      * FORM NUMBERS  : 100 - ONLINE AGREEMENT
      *                 101 - BLANK
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0326 PR#1418B ***NEW*** CALLED FROM PRT052
      * BAH 2024.0105 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0521 CHANGED D01-ACCTNO BACK TO Z(5)9 S35Q-32
      * BAH 2024.0809 DO NOT INCLUDE IN F3-ALL IF UNSECURED LOAN #1669 S35Q-113
      * BAH 2024.0904 PRINT BORROWER ACKNOWLEDGES DATE AND WFC LIMITED
      *               PARTNERSHIP WITH BRNO #1676 S35Q-123
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
           "@(#)FP/PST834.CBL S35 08/09/24-08:34:11 >".

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

       01  C-S-Z           PIC X(85).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).
       01  ADD-ON          PIC X(25).

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
      *    FMNO    : 100
      *    STATE   : TEXAS    (TX)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D01-ACCTNO                   PIC Z(5)9.
           03  FILLER                       PIC X(39).
           03  D01-LOANDATE                 PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D02-CREDITOR                 PIC X(35).
           03  FILLER                       PIC X(10).
           03  D02-BORROWER                 PIC X(31).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(67).
           03  D03-DAY                    PIC 99.
           03  FILLER                     PIC X(8).
           03  D03-MONTH                  PIC X(9).
           03  FILLER                     PIC X(4).
           03  D03-YEAR                   PIC 99.

       01  DETAIL-LINE-04                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(75).
           03  D04-BRNO                   PIC Z(4).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
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
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CK1-FILE.
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

      * IF F3-ALL IS TURNED ON, DO NOT INCLUDE IF LOAN IS UNSECURED
           IF BR-ALLOW-F3-ALL-DOCS = "Y"
              IF LN-COLLCD = "UL"
                 PERFORM GET-PST-OUTPUT-ENV
                 IF PST-WS-OUTPUT-SWITCH-ALL
                    GO TO PRINT-ROUTINE-EXIT.

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

           IF NOT ( IP-FMNO = 101 )
              MOVE LN-ACCTNO                   TO D01-ACCTNO
              MOVE LN-LOANDATE                 TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D01-LOANDATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-STMNT1                      TO D02-CREDITOR.
           IF NOT ( IP-FMNO = 101 )
              MOVE MAKER-NAME-FIRST-LAST       TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE GB-COMPANY-NAME                TO D02-CREDITOR.
           IF NOT ( IP-FMNO = 101 )
              MOVE JOINT-NAME-FIRST-LAST       TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR1                        TO D02-CREDITOR.
           IF NOT ( IP-FMNO = 101 )
              MOVE MAKER-ADDRESS-1             TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( BR-ADR2 NOT = SPACES )
              MOVE BR-ADR2                     TO D02-CREDITOR
           ELSE
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF NOT ( IP-FMNO = 101 )
              IF ( MAKER-ADDRESS-2 NOT = SPACES )
                 MOVE MAKER-ADDRESS-2   TO D02-BORROWER
           ELSE
                 MOVE MAKER-CSZ         TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( BR-ADR2 NOT = SPACES )
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF NOT ( IP-FMNO = 101 )
              IF ( MAKER-ADDRESS-2 NOT = SPACES )
                 MOVE MAKER-CSZ         TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * BORROWER ACKNOWLEDGES RECEIPT OF A SIGNED COPY
           IF NOT ( IP-FMNO = 101 )
              MOVE LN-LOANDATE TO DATE-YYYYMMDD
              MOVE DATE-YYYYMMDD-MM        TO WS-MONTH-MM
              PERFORM LOAD-WS-MONTH-TEXT
              MOVE DATE-YYYYMMDD-DD        TO D03-DAY
              MOVE WS-MONTH-TEXT           TO D03-MONTH
              MOVE DATE-YYYYMMDD-YY        TO D03-YEAR.

           MOVE 40 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * WFC LIMITED PARTNERSHIP #BRNO
           MOVE BR-NO TO D04-BRNO.
           MOVE 6 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-01-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       GET-PST-OUTPUT-ENV.
           MOVE "PST_OUTPUT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-BAD
              MOVE "N" TO PST-WS-OUTPUT-SWITCH
           ELSE
              MOVE GETENV-BUF TO PST-WS-OUTPUT-SWITCH.
           IF NOT (PST-WS-OUTPUT-SWITCH-VALID)
              MOVE "N" TO PST-WS-OUTPUT-SWITCH.
       ADD-IT.
           INSPECT C-S-Z REPLACING FIRST "  " BY " +".
           INSPECT C-S-Z REPLACING FIRST
              "+                        " BY ADD-ON.
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
