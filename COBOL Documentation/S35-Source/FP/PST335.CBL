       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST335.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-17-2018.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST335.C
      *
      * DESCRIPTION   :  PERSONAL PROPERTY 
      *                  LYNDON SOUTHERN
      *
      * STATE         :  KENTUCKY    (KY)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2018.0517 PR#1314 CALLED FROM PRT00T
      * AMM 2018.1115 PR#1349 ADDED PAGE 5
      * AMM 2019.0805 PR#1382 PROPERTY ADDRESS WAS NOT PRINTING
      *               ENTIRE STREET, ADDED STRING WORDS-OUTPUT TO GET
      *               ALL WORDS
      * BAH 2023.1214 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1635
      *               ONLY PRINT IF PROP INSURANCE WAS WRITTEN
      * BAH 2024.0521 CHANGED D03-ACCTNO BACK TO Z(5)9 S35Q-32
      * BAH 2024.0814 CHECK BOX REQUESTING INSURANCE ON P5 #1672 S35Q-117
      * BAH 2024.1001 PRINT BORROWER NAME ON PRINTED NAME LINE #1679 S35Q-135
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
           "@(#)FP/PST335.CBL S34 06/17/21-12:16:32 >".

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

       01  TC-FEE          PIC S9(7)V99 COMP-3 VALUE 0.
       01  TA-FEE          PIC S9(7)V99 COMP-3 VALUE 0.
       01  TP-FEE          PIC S9(7)V99 COMP-3 VALUE 0.
       01  WS-SURCHARGE    PIC S9(7)V99 COMP-3 VALUE 0.
       01  T1-FEE          PIC S9(7)V99 COMP-3 VALUE 0.
       01  T2-FEE          PIC S9(7)V99 COMP-3 VALUE 0.
       01  T3-FEE          PIC S9(7)V99 COMP-3 VALUE 0.

       01  TEST-ADDRESS    PIC X(03) VALUE SPACES.

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
      *    CPI     : 12 CPI   [PIC X(92)]
      *    CUSTOMER: WORLD ACCEPTANCE CORP.
      *    FMNO    : ***
      *    STATE   : KENTUCKY    (KY)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D01-BW-LNAME                 PIC X(28).
           03  FILLER                       PIC X(01).
           03  D01-BW-FNAME                 PIC X(12).
           03  FILLER                       PIC X(02).
           03  D01-CREDITOR                 PIC X(48).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D02-EFFDATE                  PIC 99/99/99.
           03  FILLER                       PIC X(25).
           03  D02-TERM                     PIC ZZ9.
           03  FILLER                       PIC X(30).
           03  D02-MAX-AMT                  PIC ZZZ,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(22).
           03  D03-ACCTNO                   PIC ZZZZZ9.
           03  FILLER                       PIC X(39).
           03  D03-PREMIUM                  PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(01).
           03  D03-PP-TEXT                  PIC X(20).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(07).
           03  D04-CITY-CODE                PIC X(03).
           03  FILLER                       PIC X(14).
           03  D04-TAX                      PIC ZZ,ZZ9.99.
           03  FILLER                       PIC X(08).
           03  D04-SURCHG                   PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(13).
           03  D04-TOTAL-PREM               PIC ZZZ,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D05-ADR-NO                   PIC X(06).
           03  FILLER                       PIC X(10).
           03  D05-STREET                   PIC X(29).
           03  FILLER                       PIC X(02).
           03  D05-CITY                     PIC X(27).
           03  FILLER                       PIC X(03).
           03  D05-STATE                    PIC X(02).

       01  DETAIL-LINE-06                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(11).
           03  D06-BOX                      PIC X.

       01  DETAIL-LINE-07                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(12).
           03  D07-PRINTED-NAME             PIC X(30).

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

           IF LN-INSPREM(3) = 0
              GO TO PRINT-ROUTINE-EXIT.

           MOVE 05    TO PST-WS-FORM-PAGE-MAX.
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

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF ( IO-BAD )
             GO TO IO-ERROR. 

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ PRINT PAGES  }
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

           MOVE 10.00                   TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER     TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                   TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * I WISH TO PURCHASE CREDIT PROPERTY
           MOVE "X" TO D06-BOX.
           MOVE 29 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PRINTED NAME
           MOVE MAKER-NAME-FIRST-LAST TO D07-PRINTED-NAME.
           MOVE 11 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-05-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - P A G E - S T D - R O U T I N E
      *
      ******************************************************************
       PRINT-PAGE-STD-ROUTINE SECTION.
           MOVE 10.00                   TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER     TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                   TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           MOVE MAKER-ADDRESS                TO WORDS-INPUT.
           PERFORM WORDS-ROUTINE.
           MOVE MAKER-LNAME                  TO D01-BW-LNAME.
           MOVE MAKER-FNAME                  TO D01-BW-FNAME.
           MOVE BR-NAME                      TO D01-CREDITOR.
           MOVE 11 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE LN-INSEFF-DATE(3)            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                  TO D02-EFFDATE. 
           MOVE INS-TERM(3)                  TO D02-TERM.
           MOVE LN-INSCOVR(3)                TO D02-MAX-AMT.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE LN-ACCTNO                    TO D03-ACCTNO.
           MOVE LN-INSPREM(3)                TO D03-PREMIUM.
           IF ( LN-COLLCD = "PP" OR "AP" )
              MOVE "PERSONAL PROPERTY"       TO D03-PP-TEXT.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           PERFORM GET-FEES.

      * CITY-CODE / MUNICIPAL TAX / F& P SURCHG
      * TAX HAS SURCHARGE INCLUDED IN IT...
           IF TP-FEE NOT = 0
              MOVE BW-CITY-TAX-CD            TO D04-CITY-CODE
              COMPUTE WS-SURCHARGE = LN-INSPREM(3) * .018
              MOVE WS-SURCHARGE              TO D04-SURCHG
              COMPUTE D04-TAX = TP-FEE - WS-SURCHARGE
           ELSE
              MOVE 0                         TO D04-TAX D04-SURCHG.
      * TOTAL-PREMIUM              
           COMPUTE D04-TOTAL-PREM = TP-FEE + LN-INSPREM(3).
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           IF ( LN-COLLCD = "PP" OR "AP" )
              PERFORM FIND-GOOD-LOCATION.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
       PRINT-PAGE-STD-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      ******************************
       FIND-GOOD-LOCATION SECTION.
      ******************************
           MOVE MAKER-ADDRESS-1 TO TEST-ADDRESS.

           IF TEST-ADDRESS NOT = "PO " AND
                            NOT = "P.O" AND
                             NOT = "P O" AND
                              NOT = SPACES
              MOVE WORDS-OUTPUT(1) TO D05-ADR-NO
              STRING WORDS-OUTPUT(2), " ", WORDS-OUTPUT(3), " ",
                     WORDS-OUTPUT(4), " ", WORDS-OUTPUT(5), " ",
                     WORDS-OUTPUT(6), " ", WORDS-OUTPUT(7) 
                     DELIMITED BY "  " INTO D05-STREET
              MOVE MAKER-CITY      TO D05-CITY
              MOVE MAKER-STATE     TO D05-STATE
           ELSE
              MOVE MAKER-ADDRESS-2 TO TEST-ADDRESS
              IF TEST-ADDRESS NOT = "PO " AND
                               NOT = "P.O" AND
                                NOT = "P O" AND
                                 NOT = SPACES
                 MOVE WORDS-OUTPUT(1) TO D05-ADR-NO
                 STRING WORDS-OUTPUT(2), " ", WORDS-OUTPUT(3), " ",
                        WORDS-OUTPUT(4), " ", WORDS-OUTPUT(5), " ",
                        WORDS-OUTPUT(6), " ", WORDS-OUTPUT(7) 
                        DELIMITED BY "  " INTO D05-STREET
                 MOVE MAKER-CITY      TO D05-CITY
                 MOVE MAKER-STATE     TO D05-STATE
              ELSE
                 MOVE SPACES TO D05-ADR-NO D05-STREET 
                                D05-CITY D05-STATE.

      *************************
       GET-FEES SECTION.
      *************************
           MOVE 0 TO TC-FEE TA-FEE TP-FEE T1-FEE T2-FEE T3-FEE.
           MOVE 1 TO SUB.
       NEXT-LN-FEE.
           IF SUB > 10
              GO TO EXIT-FIND-FEE.
           IF LN-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-LN-FEE.

           IF LN-FEECODE(SUB) = "TC"
              ADD LN-FEEAMT(SUB) TO TC-FEE
           ELSE
           IF LN-FEECODE(SUB) = "TA"
              ADD LN-FEEAMT(SUB) TO TA-FEE
           ELSE
           IF LN-FEECODE(SUB) = "TP"
              ADD LN-FEEAMT(SUB) TO TP-FEE
           ELSE
           IF LN-FEECODE(SUB) = "T1"
              ADD LN-FEEAMT(SUB) TO T1-FEE
           ELSE
           IF LN-FEECODE(SUB) = "T2"
              ADD LN-FEEAMT(SUB) TO T2-FEE
           ELSE
           IF LN-FEECODE(SUB) = "T3"
              ADD LN-FEEAMT(SUB) TO T3-FEE.

           ADD 1 TO SUB.
           GO TO NEXT-LN-FEE.
       EXIT-FIND-FEE.
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
