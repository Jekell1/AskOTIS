       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST470.
       DATE-WRITTEN.  11-11-2022.
      ******************************************************************
      *
      * IDENTIFICATION:  LP/PST470.C
      *
      * DESCRIPTION   :  IUI CERTIFICATE
      *                  LYNDON SOUTHERN
      *                  INSPREM(4) OTHER 1
      *
      * STATE         :  ALABAMA      (AL)
      *
      * REV:
      * BAH 2022.1111 NEW #1576
      * BAH 2022.1205 REMOVED JOINT APPLICANT DATE AND TIME ON PAGE 1 AND 3
      *               PAGES 2,4,AND 8 REMOVED COBORROWER NAME AND SECOND
      *               INSURED BIRTHDATE AND DATE  #1576A
      * BAH 2024.0104 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
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
           "@(#)FP/PST470.CBL S34 06/17/21-12:32:17 >".

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

      * PAGE 1 & PAGE 3
      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(36).
           03  D01-DATE                       PIC 99/99/99 BLANK ZERO.
           03  FILLER                         PIC X(16).
           03  D01-TIME                       PIC 99/99/99 BLANK ZERO.

      * PAGE 2, 4, AND 8
      *-----------------------------------------------------------------
       01  DETAIL-LINE-02-1                   REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(4).
           03  D02-NAME-ADR.
               05  FILLER                     PIC X(17).
               05  D02-BRNAME                 PIC X(38).
           03  FILLER                         PIC X(9).
           03  D02-NUMBER                     PIC Z(8).

       01  DETAIL-LINE-02-2                   REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(4).
           03  D02-BW-NAME                    PIC X(30).
           03  FILLER                         PIC X(2).
           03  FILLER                         PIC X(11).
           03  FILLER                         PIC X(3).
           03  D02-EFFDATE                    PIC 99/99/99.
           03  FILLER                         PIC X(4).
           03  D02-EXPDATE                    PIC 99/99/99.
           03  FILLER                         PIC X.
           03  D02-MDTE                       PIC 99/99/99.

       01  DETAIL-LINE-02-3                   REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(36).
           03  D02-PREMIUM                    PIC ZZ,ZZ9.99.
           03  FILLER                         PIC X(6).
           03  D02-MONTHLY                    PIC ZZ,ZZ9.99.
           03  FILLER                         PIC X(8).
           03  D02-INSTERM                    PIC ZZ9.

       01  DETAIL-LINE-02-4                   REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(42).
           03  D02-BDATE                      PIC 99/99/99.
           03  FILLER                         PIC X(12).
           03  D02-LOANDATE                   PIC 99/99/99.


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

           IF LN-INSPREM(4) = 0
              GO TO PRINT-ROUTINE-EXIT.

           MOVE 09     TO PST-WS-FORM-PAGE-MAX.
           PERFORM PST-FR-FORM-INITIALIZATION.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-ROUTINE-EXIT.

           PERFORM PST-INFO-MODULE.

      *- - - - - - - - - - - - - - - - - - - - -{ SETUP PAGE COPY LOOP }
           MOVE ZEROES TO PAGE-COPY-ELE.
           MOVE 01     TO PAGE-COPY-MAX.

       PRINT-ROUTINE-PAGE-COPY.

           ADD 01 TO PAGE-COPY-ELE.

           IF ( PAGE-COPY-ELE > PAGE-COPY-MAX )
              GO TO PRINT-ROUTINE-END.

           MOVE 10.00                   TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER     TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                   TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           PERFORM PRINT-PAGE-01-ROUTINE.
           PERFORM PRINT-PAGE-02-ROUTINE.
           PERFORM PRINT-PAGE-03-ROUTINE.
           PERFORM PRINT-PAGE-04-ROUTINE.
           PERFORM PRINT-PAGE-05-ROUTINE.
           PERFORM PRINT-PAGE-06-ROUTINE.
           PERFORM PRINT-PAGE-07-ROUTINE.
           PERFORM PRINT-PAGE-08-ROUTINE.
           PERFORM PRINT-PAGE-09-ROUTINE.


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

           PERFORM GET-POSTTIME.
      * DATE
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D01-DATE.
           MOVE LP-POSTTIME TO D01-TIME.
           INSPECT D01-TIME REPLACING ALL "/" BY ":".
           MOVE 48          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * JOINT APPLICANT/INSURED * REMOVED 12/6/22 #1576A
      * DATE
      *    IF JOINT-SSNO NOT = 0
      *       MOVE LN-LOANDATE TO D01-DATE
      *       MOVE LP-POSTTIME TO D01-TIME
      *       INSPECT D01-TIME REPLACING ALL "/" BY ":".
           MOVE 3           TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AGENT
      * DATE
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D01-DATE.
           MOVE LP-POSTTIME TO D01-TIME.
           INSPECT D01-TIME REPLACING ALL "/" BY ":".
           MOVE 3          TO LN-FEED.
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

      ************************************************
      * IDENTICAL TO PAGE 4 - SHARES DETAIL LINE
      * SHARED WITH PAGE 8
      ************************************************

      * NAME AND ADDRESS OF CREDITOR
           MOVE BR-NAME                        TO D02-BRNAME.
           MOVE 7 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
             STRING BR-ADR1,", ", BR-ADR2,", ",BR-CITY,", ",
                    BR-STATE, " ", BR-ZIP DELIMITED BY "  "
                     INTO D02-NAME-ADR
           ELSE
             STRING BR-ADR1,", ",BR-CITY,", ",BR-STATE," ",BR-ZIP
                    DELIMITED BY "  " INTO D02-NAME-ADR.

      * CERTIFICATE AND ACCOUNT NUMBER
           MOVE LN-ACCTNO TO D02-NUMBER.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NAME AND ADDRESS OF DEBTOR
           MOVE MAKER-NAME-FIRST-LAST          TO D02-BW-NAME.

           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * REMOVED COMAKER 12/6/2022 #1576A
           MOVE MAKER-ADDRESS-1             TO D02-BW-NAME.

      * LEAVE SOC SEC BOX BLANK, ANDRIA
      *    MOVE MAKER-SSNO                     TO D02-SSNO.
      *    INSPECT D02-SSNO REPLACING ALL "/" BY "-".
           MOVE LN-INSEFF-DATE(4)              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D02-EFFDATE.
           MOVE LN-INSEXP-DATE(4)              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D02-EXPDATE.
           MOVE MDTE-DATE                      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D02-MDTE.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF MAKER-ADDRESS-2 NOT = SPACES
              MOVE MAKER-ADDRESS-2          TO D02-BW-NAME
           ELSE
              MOVE MAKER-CSZ                TO D02-BW-NAME.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF MAKER-ADDRESS-2 NOT = SPACES
              MOVE MAKER-CSZ                TO D02-BW-NAME.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PREMIUM
           MOVE LN-INSPREM(4)                  TO D02-PREMIUM.
      * MONTHLY INDEMNITY
           IF ( LN-REGPYAMT - LN-MAINTFEE ) > 1000.00
              MOVE 1000.00 TO D02-MONTHLY
           ELSE
              COMPUTE D02-MONTHLY = LN-REGPYAMT - LN-MAINTFEE.

      * TERM OF INSURANCE
           MOVE INS-TERM(4)                    TO D02-INSTERM.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * FIRST INSURED SIGNATURE
      * BIRTHDATE
           MOVE MAKER-BDATE                   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D02-BDATE.
           MOVE LN-LOANDATE                   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D02-LOANDATE.
           MOVE 6 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SECOND INSURED   REMOVED 12/6/22 #1576A
      * BIRTHDATE
      *    IF JOINT-BDATE NOT = 0
      *       MOVE JOINT-BDATE                   TO D02-BDATE
      *       MOVE LN-LOANDATE                   TO D02-LOANDATE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

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

      ************************************************
      * IDENTICAL TO PAGE 1 - SHARES DETAIL LINE
      ************************************************

      * APPLICANT/INSURED
      * DATE
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D01-DATE.
           MOVE LP-POSTTIME TO D01-TIME.
           INSPECT D01-TIME REPLACING ALL "/" BY ":".
           MOVE 48          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * JOINT APPLICANT/INSURED * REMOVED 12/6/22 #1576A
      * DATE
      *    IF JOINT-SSNO NOT = 0
      *       MOVE LN-LOANDATE TO D01-DATE
      *       MOVE LP-POSTTIME TO D01-TIME
      *       INSPECT D01-TIME REPLACING ALL "/" BY ":".
           MOVE 3           TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AGENT
      * DATE
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D01-DATE.
           MOVE LP-POSTTIME TO D01-TIME.
           INSPECT D01-TIME REPLACING ALL "/" BY ":".
           MOVE 3          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

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

      ************************************************
      * IDENTICAL TO PAGE 2 - SHARES DETAIL LINE
      ************************************************

      * NAME AND ADDRESS OF CREDITOR
           MOVE BR-NAME                        TO D02-BRNAME.
           MOVE 7 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
             STRING BR-ADR1,", ", BR-ADR2,", ",BR-CITY,", ",
                    BR-STATE, " ", BR-ZIP DELIMITED BY "  "
                     INTO D02-NAME-ADR
           ELSE
             STRING BR-ADR1,", ",BR-CITY,", ",BR-STATE," ",BR-ZIP
                    DELIMITED BY "  " INTO D02-NAME-ADR.
      * CERTIFICATE AND ACCOUNT NUMBER
           MOVE LN-ACCTNO TO D02-NUMBER.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NAME AND ADDRESS OF DEBTOR
           MOVE MAKER-NAME-FIRST-LAST          TO D02-BW-NAME.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * REMOVED COMAKER 12/6/2022 #1576A
           MOVE MAKER-ADDRESS-1                TO D02-BW-NAME.
           MOVE LN-INSEFF-DATE(4)              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D02-EFFDATE.
           MOVE LN-INSEXP-DATE(4)              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D02-EXPDATE.
           MOVE MDTE-DATE                      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D02-MDTE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF MAKER-ADDRESS-2 NOT = SPACES
              MOVE MAKER-ADDRESS-2          TO D02-BW-NAME
           ELSE
              MOVE MAKER-CSZ                TO D02-BW-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF MAKER-ADDRESS-2 NOT = SPACES
              MOVE MAKER-CSZ                TO D02-BW-NAME.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PREMIUM
           MOVE LN-INSPREM(4)                  TO D02-PREMIUM.
      * MONTHLY INDEMNITY
           IF ( LN-REGPYAMT - LN-MAINTFEE ) > 1000.00
              MOVE 1000.00 TO D02-MONTHLY
           ELSE
              COMPUTE D02-MONTHLY = LN-REGPYAMT - LN-MAINTFEE.
      * TERM OF INSURANCE
           MOVE INS-TERM(4)                    TO D02-INSTERM.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * FIRST INSURED SIGNATURE
      * BIRTHDATE
           MOVE MAKER-BDATE                   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D02-BDATE.
           MOVE LN-LOANDATE                   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D02-LOANDATE.
           MOVE 6 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SECOND INSURED   REMOVED 12/6/22 #1576A
      * BIRTHDATE
      *    IF JOINT-BDATE NOT = 0
      *       MOVE JOINT-BDATE                   TO D02-BDATE
      *       MOVE LN-LOANDATE                   TO D02-LOANDATE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


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

      ************************************************
      * SHARES DETAIL LINE WITH PAGE 2 AND 4
      ************************************************

      * NAME AND ADDRESS OF CREDITOR
           MOVE BR-NAME                        TO D02-BRNAME.
           MOVE 7 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
             STRING BR-ADR1,", ", BR-ADR2,", ",BR-CITY,", ",
                    BR-STATE, " ", BR-ZIP DELIMITED BY "  "
                     INTO D02-NAME-ADR
           ELSE
             STRING BR-ADR1,", ",BR-CITY,", ",BR-STATE," ",BR-ZIP
                    DELIMITED BY "  " INTO D02-NAME-ADR.

      * CERTIFICATE AND ACCOUNT NUMBER
           MOVE LN-ACCTNO TO D02-NUMBER.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NAME AND ADDRESS OF DEBTOR
           MOVE MAKER-NAME-FIRST-LAST          TO D02-BW-NAME.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * REMOVED COMAKER 12/6/2022 #1576A
           MOVE MAKER-ADDRESS-1             TO D02-BW-NAME.
           MOVE LN-INSEFF-DATE(4)              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D02-EFFDATE.
           MOVE LN-INSEXP-DATE(4)              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D02-EXPDATE.
           MOVE MDTE-DATE                      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D02-MDTE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF MAKER-ADDRESS-2 NOT = SPACES
              MOVE MAKER-ADDRESS-2          TO D02-BW-NAME
           ELSE
              MOVE MAKER-CSZ                TO D02-BW-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF MAKER-ADDRESS-2 NOT = SPACES
              MOVE MAKER-CSZ                TO D02-BW-NAME.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PREMIUM
           MOVE LN-INSPREM(4)                  TO D02-PREMIUM.
      * MONTHLY INDEMNITY
           IF ( LN-REGPYAMT - LN-MAINTFEE ) > 1000.00
              MOVE 1000.00 TO D02-MONTHLY
           ELSE
              COMPUTE D02-MONTHLY = LN-REGPYAMT - LN-MAINTFEE.
      * TERM OF INSURANCE
           MOVE INS-TERM(4)                    TO D02-INSTERM.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * FIRST INSURED SIGNATURE
      * BIRTHDATE
           MOVE MAKER-BDATE                   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D02-BDATE.
           MOVE LN-LOANDATE                   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D02-LOANDATE.
           MOVE 6 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SECOND INSURED   REMOVED 12/6/22 #1576A
      * BIRTHDATE
      *    IF JOINT-BDATE NOT = 0
      *       MOVE JOINT-BDATE                   TO D02-BDATE
      *       MOVE LN-LOANDATE                   TO D02-LOANDATE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-08-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-09-ROUTINE SECTION.
      ******************************************************************

           MOVE 09 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-09-ROUTINE-EXIT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-09-ROUTINE-EXIT.
           EXIT.

      **********************
       GET-POSTTIME SECTION.
      **********************
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 1         TO LP-SEQNO.
           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           IF IO-FG = 9
               GO TO EXIT-GET-POSTTIME.

       EXIT-GET-POSTTIME.
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
