       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST340.
       AUTHOR.        AMM.
       DATE-WRITTEN.  08-07-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST340.C
      *
      * DESCRIPTION   :  PERSONAL PROPERTY AND VEHICLE
      *                  LYNDON SOUTHERN
      *                  INSPREM(3) AND INSPREM(6)
      *
      * STATE         :  ALABAMA    (AL)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0807 PR#1449 CALLED FROM PRT01C
      * AMM 2020.0915 PR#1449B ADDRESS AUTO INFO AND EFFDATE FOR O3
      * BAH 2024.0104 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0814 PRINT COUNTERSIGNED DATE AT BOTTOM #1671 S35Q-116
      *               ALSO CHANGE THE CRYSTAL PAGE 3 TO ADJUST THE LINE
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
           "@(#)FP/PST340.CBL S35 05/02/22-14:58:22 >".

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

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(02).
           03  D01-ADDRESS                  PIC X(75).
           03  FILLER                       REDEFINES D01-ADDRESS.
               05  FILLER                   PIC X(25).
               05  D01-INSURED              PIC X(50).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(37).
           03  D02-EFFDATE                  PIC 99/99/99.
           03  FILLER                       PIC X(15).
           03  D02-TERM                     PIC ZZ9.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(02).
           03  D03-PP-SINGLE                PIC X.
           03  FILLER                       PIC X(07).
           03  D03-PP-DUAL                  PIC X.
           03  FILLER                       PIC X(07).
           03  D03-AUTO-SINGLE              PIC X.
           03  FILLER                       PIC X(08).
           03  D03-PP-COVR                  PIC ZZ,ZZ9.99.
           03  FILLER                       PIC X(02).
           03  D03-O3-COVR                  PIC ZZ,ZZ9.99.
           03  FILLER                       PIC X(02).
           03  D03-PP-PREM                  PIC ZZ,ZZ9.99.
           03  FILLER                       PIC X(02).
           03  D03-O3-PREM                  PIC ZZ,ZZ9.99.
           03  FILLER                       PIC X(02).
           03  D03-TOTAL-PREM               PIC ZZ,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D04-DEBT-AMT                 PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(05).
           03  D04-NUM-PAY                  PIC ZZ9.
           03  FILLER                       PIC X(16).
           03  D04-YEAR                     PIC X(04).
           03  FILLER                       PIC X(01).
           03  D04-MAKE                     PIC X(11).
           03  FILLER                       PIC X(01).
           03  D04-MODEL                    PIC X(08).
           03  FILLER                       PIC X(01).
           03  D04-MOTOR                    PIC X(17).

       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(15).
           03  D05-COUNTER-DATE             PIC X(14).
           03  FILLER                       PIC X(4).
           03  D05-COUNTER-YEAR             PIC 9(2).

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

           IF LN-INSPREM(3) = 0 AND LN-INSPREM(6) = 0
              GO TO PRINT-ROUTINE-EXIT.

           MOVE 08     TO PST-WS-FORM-PAGE-MAX.
           PERFORM PST-FR-FORM-INITIALIZATION.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ LOAD ACCOUNT INFO. }
           PERFORM PST-INFO-MODULE.

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
           PERFORM PRINT-PAGE-07-ROUTINE.
           PERFORM PRINT-PAGE-08-ROUTINE.

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

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-06-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-07-ROUTINE SECTION.
      ******************************************************************

           MOVE 07 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-07-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-07-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-08-ROUTINE SECTION.
      ******************************************************************

           MOVE 08 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-08-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-08-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-STD-ROUTINE SECTION.
      ******************************************************************
           MOVE MAKER-NAME-FIRST-LAST        TO D01-INSURED.
           MOVE 08 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE MAKER-ADDRESS-CSZ            TO D01-ADDRESS.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE BR-NAME                      TO D01-INSURED.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           STRING BR-ADR1, ", ", BR-CITY, ", ", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO D01-ADDRESS.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           IF ( LN-INSTYPES-PP )
              MOVE "X"                       TO D03-PP-SINGLE
           ELSE
           IF ( LN-INSTYPES-PP2 )
              MOVE "X"                       TO D03-PP-DUAL.
           IF ( LN-INSPREM(6) NOT = 0 )
              MOVE "X"                       TO D03-AUTO-SINGLE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           IF ( LN-INSTYPES-PP )
              MOVE LN-INSCOVR(3)                TO D03-PP-COVR
              MOVE LN-INSPREM(3)                TO D03-PP-PREM.
           IF ( LN-INSPREM(6) NOT = 0 )
              MOVE LN-INSCOVR(6)                TO D03-O3-COVR
              MOVE LN-INSPREM(6)                TO D03-O3-PREM.
           COMPUTE D03-TOTAL-PREM = LN-INSPREM(3) + LN-INSPREM(6).
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE LN-LNAMT                     TO D04-DEBT-AMT.
           MOVE LN-ORGTERM                   TO D04-NUM-PAY.
           IF ( LN-INSPREM(3) NOT = ZEROES ) OR
              ( LN-INSPREM(6) NOT = ZEROES )
              MOVE AUTO-YEAR(1)              TO D04-YEAR
              MOVE AUTO-MAKE(1)              TO D04-MAKE
              MOVE AUTO-MODEL(1)             TO D04-MODEL
              MOVE AUTO-ID(1)                TO D04-MOTOR.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           IF ( LN-INSPREM(3) NOT = ZEROES )
              MOVE LN-INSEFF-DATE(3)         TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY               TO D02-EFFDATE
              MOVE INS-TERM(3)               TO D02-TERM
           ELSE
              MOVE LN-INSEFF-DATE(6)         TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY               TO D02-EFFDATE
              MOVE INS-TERM(3)               TO D02-TERM.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ONLY PRINT COUNTERSIGNED ON PAGE 3
           IF ( PST-WS-FORM-PAGE-NO = 3 )
              MOVE EXT-CONAME-SYSDATE  TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-MMDDYY
              MOVE DATE-MMDDYY-MM         TO WS-MONTH-MM
              PERFORM LOAD-WS-MONTH-TEXT
              STRING WS-MONTH-TEXT," ",DATE-MMDDYY-DD DELIMITED BY
                  "  " INTO D05-COUNTER-DATE
              MOVE DATE-MMDDYY-YY TO D05-COUNTER-YEAR
              MOVE 40 TO LN-FEED
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
