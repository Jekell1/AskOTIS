       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST50J.
       AUTHOR.        AMM.
       DATE-WRITTEN.  10-21-2021.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST50J.C
      *
      * DESCRIPTION   :  UNITED MOTOR CLUB UMC
      *                  ROAD GARD FEE CODE
      *
      * STATE         :  ALL STATES  
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2021.1021 PR#1520 
      * BAH 2023.1110 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1630
      * BAH 2023.1219 PRINT EXPIRATION DATE BASED ON CALCULATED TERM #1636
      * BAH 2024.0521 CHANGED D01-NUMBER BACK TO 9(6) S35Q-32
      * BAH 2024.0523 REMOVE EXPIRATION DATE ON CARD AND SEVERAL OTHER
      *               CRYSTAL CHANGES #1654 S35Q-5
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)FP/PST50J S35 11/13/23-13:32:13 >".

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
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
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
       01  ROAD-INS             PIC S9(5)V99  COMP-3 VALUE 0.
       01  ROAD-INS-FG          PIC X VALUE SPACES.
       01  HOLD-TERM            PIC 9(3)V99.
       01  HOLD-TERMX REDEFINES HOLD-TERM.
           03  HOLD-WHOLE       PIC 9(3).
           03  HOLD-DEC1        PIC 9.
           03  HOLD-DEC2        PIC 9.

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                      PIC X(300)     VALUE SPACES.

      *=================================================================
      *
      *    D E T A I L - W O R L D - A C C E P T A N C E 
      *
      *-----------------------------------------------------------------
      *    CPI     : 10 CPI   [PIC X(80)]
      *    CUSTOMER: WORLD ACCEPTANCE CORP.
      *    FMNO    : ***
      *    STATE   : LOUISIANA   (LA)
      *=================================================================
       01  DETAIL-LINE-01 REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(13). 
           03  D01-EFFDATE                    PIC 99/99/99.
           03  FILLER                         PIC X(43). 
           03  D01-BRNO                       PIC 9(04).
           03  D01-DASH                       PIC X.
           03  D01-NUMBER                     PIC 9(06).

      *----------------------------------------------------------------
       01  DETAIL-LINE-02 REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(15). 
           03  D02-BWNAME                     PIC X(22).
           03  FILLER                         PIC X(12). 
           03  D02-ASSOC                      PIC X(27).

      *----------------------------------------------------------------
       01  DETAIL-LINE-03 REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(10).
           03  D03-ADDRESS                    PIC X(67).

      *----------------------------------------------------------------
       01  DETAIL-LINE-04 REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(06).
           03  D04-CSZ                        PIC X(42).
           03  FILLER                         PIC X(05).
           03  D04-AMT                        PIC ZZZZ9.99 BLANK ZERO.
           03  FILLER                         PIC X(11).
           03  D04-TERM                       PIC ZZ9.

      *----------------------------------------------------------------
       01  DETAIL-LINE-05 REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(22).
           03  D05-BRNAME                     PIC X(40).
           03  FILLER                         PIC X(6).
           03  D05-EXPDATE                    PIC 99/99/99.

      *----------------------------------------------------------------
       01  DETAIL-LINE-06 REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(22).
           03  D06-DAY                        PIC 99.
           03  FILLER                         PIC X(06).
           03  D06-MONTH                      PIC X(12).
           03  FILLER                         PIC X(02).
           03  D06-YEAR                       PIC 9999.

      *----------------------------------------------------------------
       01  DETAIL-LINE-07 REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(06).
           03  D07-BRNO1                      PIC 9(04).
           03  D07-DASH1                      PIC X.
           03  D07-NUMBER1                    PIC 9(06).
           03  FILLER                         PIC X(10).
           03  D07-EXPDATE1                   PIC 99/99/99.
           03  FILLER                         PIC X(12).
           03  D07-BRNO2                      PIC 9(04).
           03  D07-DASH2                      PIC X.
           03  D07-NUMBER2                    PIC 9(06).
           03  FILLER                         PIC X(10).
           03  D07-EXPDATE2                   PIC 99/99/99.

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
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BX1-FILE.
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
      D    STOP MESS.

           PERFORM PST-FORM-MAIN-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    P R I N T - R O U T I N E
      *
      ******************************************************************
       PRINT-ROUTINE SECTION.

           PERFORM GET-FEES-WORLD.
           IF ROAD-INS = 0
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
       PRINT-PAGE-01-ROUTINE SECTION.
      ******************************************************************

           MOVE 01 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-01-ROUTINE-EXIT.

           PERFORM GET-FEES-WORLD.

      * MEMBERSHIP NO.
           MOVE LN-OWNBR                   TO D01-BRNO.
           MOVE "-"                        TO D01-DASH.
           MOVE LN-ACCTNO                  TO D01-NUMBER.
           MOVE 07 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * MASTER MEMBER
           MOVE MAKER-NAME-FIRST-LAST      TO D02-BWNAME.
           IF ROAD-INS-FG = "F"
              MOVE JOINT-NAME-FIRST-LAST   TO D02-ASSOC.
           MOVE 08 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ADDRESS
           MOVE MAKER-ADDRESS              TO D03-ADDRESS.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-CSZ                  TO D04-CSZ.
           MOVE ROAD-INS                   TO D04-AMT.

      * TERM
      *  IF 93, THE TERM SHOULD BE DIVIDED BY 8
      *  IF 94, THE TERM SHOULD BE DIVIDED BY 10
      *  ROUND REMAINDER
      
           IF ( ROAD-INS-FG = "S" )
              COMPUTE HOLD-TERM = ROAD-INS / 8
           ELSE
              COMPUTE HOLD-TERM = ROAD-INS / 10.

      * IF NO REMAINER, THEN A GOOD TERM HAS BEEN FOUND
           IF ( HOLD-DEC1 = 0 AND HOLD-DEC2 = 0 )
              MOVE HOLD-TERM TO D04-TERM
           ELSE
      * IF REMAINDER, BUMP UP 1 MONTH
              ADD 1 TO HOLD-TERM
              MOVE HOLD-TERM TO D04-TERM.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * EFFECTIVE DATE 
           MOVE LN-LOANDATE                  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                  TO D01-EFFDATE.
      * EXPIRATION DATE
      *    MOVE HOLD-TERM                    TO NDTE-HOLD.
      *    PERFORM INCREMENT-MONTHS.
      *    MOVE NDTE-DATE TO DATE-YYYYMMDD.
      *    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
      *    MOVE DATE-MMDDYY                  TO D05-EXPDATE
      *                                         HOLD-EXPDATE-MMDDYY.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * THE UNDERSIGNED REQUESTS
           MOVE GB-COMPANY-NAME              TO D05-BRNAME.
           MOVE 09 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * WITNESS MY HAND
           MOVE LN-LOANDATE             TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM        TO WS-MONTH-MM.
           PERFORM LOAD-WS-MONTH-TEXT. 
           MOVE DATE-YYYYMMDD-DD            TO D06-DAY.
           MOVE WS-MONTH-TEXT               TO D06-MONTH.
           MOVE DATE-YYYYMMDD-YYYY          TO D06-YEAR.
           MOVE 16 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

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

      * MEMBER NUMBER CARD 1
           MOVE LN-OWNBR                   TO D07-BRNO1.
           MOVE "-"                        TO D07-DASH1.
           MOVE LN-ACCTNO                  TO D07-NUMBER1.
      * MEMBER NUMBER CARD 2
           MOVE LN-OWNBR                   TO D07-BRNO2.
           MOVE "-"                        TO D07-DASH2.
           MOVE LN-ACCTNO                  TO D07-NUMBER2.
           MOVE 42 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * EXPIRATION DATE
      *    MOVE HOLD-EXPDATE-MMDDYY TO D07-EXPDATE1
      *                                D07-EXPDATE2.
           MOVE 5 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-06-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      *****************
       GET-FEES-WORLD SECTION.
      *****************
           MOVE SPACES TO ROAD-INS-FG.
           MOVE 0 TO ROAD-INS.
           MOVE 1 TO SUB.
       NEXT-LN-FEE-WORLD.
           IF SUB > 10
              GO TO EXIT-FIND-FEE-WORLD.
           IF LN-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-LN-FEE-WORLD.

           IF LN-FEECODE(SUB) = BR-ROAD-G-SGL-FEECODE OR
                LN-FEECODE(SUB) = "93"
              MOVE "S" TO ROAD-INS-FG
              ADD LN-FEEAMT(SUB) TO ROAD-INS
           ELSE
             IF LN-FEECODE(SUB) = BR-ROAD-G-FAM-FEECODE OR
                 LN-FEECODE(SUB) = "94"
                MOVE "F" TO ROAD-INS-FG
                ADD LN-FEEAMT(SUB) TO ROAD-INS.

           ADD 1 TO SUB.
           GO TO NEXT-LN-FEE-WORLD.
       EXIT-FIND-FEE-WORLD.
           EXIT.

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
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBFP/FPIOPST.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
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
