       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST591.
       AUTHOR.        AMM.
       DATE-WRITTEN.  12-08-2016.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST591.C
      *
      * DESCRIPTION   :  OPT-IN/OPT-OUT FORM
      *
      *                  OPT-IN   FMNO 46  PAGE 1
      *                  OPT-OUT  FMNO 47  PAGE 2
      *
      * STATE         :  SOUTH CAROLINA   (SC)
      *
      *=================================================================
      * REV:
      * AMM 2016.1208 PR#1208 CALLED FROM PRT04Q
      * BAH 2017.0424 ADDED "MO" TO PRINT IUI AS OTHER 1 (4), PR#1241
      * AMM 2017.0427 PR#1208A ADDED SECOND COPY PRINT
      * AMM 2017.0620 PR#1208B ADDED CO TO CO-BORROWER SIGNATURE LINE; PG1
      * AMM 2017.0809 PR#1267 REMOVED SECOND COPY PRINT
      * AMM 2021.0308 PR#1492 ADDED LOGIC FOR NM, ID, AND UT TO USE FORM FOR
      *                       IUI AND CPP INSURANCES
      * BAH 2022.1208 "AL" PRINT IUI AS INSPREM(4) OTHER 1 #1576B
      * BAH 2023.1110 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1630
      * BAH 2024.0520 CHANGED TO JUSTIFY NAME INSTEAD OF SHIFT-SPACES AND
      *               MISSED THE FACT THAT THE NAME SHOULD ONLY BE 29 LENGTH
      *               EVEN THOUGH THE PIC WAS 35, S35Q-6
      * BAH 2024.0521 CHANGED D01-NUMBER BACK TO 9(6) S35Q-32
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
           "@(#)FP/PST591.CBL S35 03/26/24-08:11:34 >".

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

       01  COMPANY-NAME                PIC X(40).
       01  ANY-INSURANCE               PIC 99 VALUE 0.
       01  AD-D-INSURANCE              PIC 99 VALUE 0.

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
           03  FILLER                         PIC X(20).
           03  D01-BRNO                       PIC 9(4).
           03  D01-SLASH                      PIC X.
           03  D01-NUMBER                     PIC 9(6).
           03  FILLER                         PIC X(18).
           03  D01-LOANDATE                   PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(12).
           03  D02-BR-NAME                    PIC X(29).
           03  FILLER                         PIC X(10).
           03  D02-MAKER                      PIC X(30).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(17).
           03  D03-BR-ADDR1                   PIC X(24).
           03  FILLER                         PIC X(12).
           03  D03-CO-MAKER                   PIC X(23).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(07).
           03  D04-BR-ADDR2                   PIC X(34).
           03  FILLER                         REDEFINES D04-BR-ADDR2.
               05  D04-BR-PHONE               PIC 999/999/9999.
           03  FILLER                         PIC X(09).
           03  D04-ADDR1                      PIC X(26).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(44).
           03  D05-ADDR2                      PIC X(32).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(62).
           03  D06-PREMIUM                    PIC ZZZ,ZZ9.99.
           03  D06-PREMIUM-NA           REDEFINES D06-PREMIUM PIC X(10).

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

           MOVE 0 TO ANY-INSURANCE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              IF LN-INSPREM(SUB) NOT = 0
                 ADD 1 TO ANY-INSURANCE
              END-IF
           END-PERFORM.
           MOVE 0 TO AD-D-INSURANCE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF LN-FEECODE(SUB) = "97"
                 ADD 1 TO AD-D-INSURANCE
              END-IF
           END-PERFORM.

      * OPT-IN   FMNO 46
      * IF ANY INSURANCE PREMIUMS, THEY GET AN OPT-IN

           IF ( IP-FMNO = 46 )
              IF (ANY-INSURANCE = 0 AND AD-D-INSURANCE = 0)
                 GO TO PRINT-ROUTINE-EXIT.

      * OPT-OUT  FMNO 47
      * IF NO INSURANCES, THEY GET AN OPT-OUT
           IF ( IP-FMNO = 47 )
              IF ANY-INSURANCE NOT = 0 OR
                   AD-D-INSURANCE NOT = 0
                 GO TO PRINT-ROUTINE-EXIT.


           MOVE 02     TO PST-WS-FORM-PAGE-MAX.
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
           IF ( IP-FMNO = 46 )
              PERFORM PRINT-PAGE-01-ROUTINE
           ELSE
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

           MOVE LN-OWNBR                       TO D01-BRNO.
           MOVE "/"                            TO D01-SLASH.
           MOVE LN-ACCTNO                      TO D01-NUMBER.
           MOVE LN-LOANDATE                    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D01-LOANDATE. 
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE GB-COMPANY-NAME                TO COMPANY-NAME.
           CALL "C$JUSTIFY" USING COMPANY-NAME, "L".
           MOVE COMPANY-NAME                   TO D02-BR-NAME.
           MOVE MAKER-NAME-FIRST-LAST          TO D02-MAKER.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE BR-ADR1                        TO D03-BR-ADDR1.
           MOVE JOINT-NAME-FIRST-LAST          TO D03-CO-MAKER.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR2                        TO D04-BR-ADDR2.
           MOVE MAKER-ADDRESS-1                TO D04-ADDR1.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           STRING BR-CITY, " ,", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO D04-BR-ADDR2.
           IF ( MAKER-ADDRESS-2 NOT = SPACES )
              MOVE MAKER-ADDRESS-2             TO D05-ADDR2
           ELSE
              MOVE MAKER-CSZ                   TO D05-ADDR2.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-PHONE                       TO D04-BR-PHONE.
           INSPECT D04-BR-PHONE REPLACING ALL "/" BY "-".
           IF ( MAKER-ADDRESS-2 NOT = SPACES )
              MOVE MAKER-CSZ                   TO D05-ADDR2.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-INSPREM(1) NOT = 0
              MOVE LN-INSPREM(1) TO D06-PREMIUM
           ELSE
              MOVE " N/A" TO D06-PREMIUM-NA.
           MOVE 17 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-INSPREM(2) NOT = 0
              MOVE LN-INSPREM(2) TO D06-PREMIUM
           ELSE
              MOVE " N/A" TO D06-PREMIUM-NA.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INVOLUNTARY UNEMPLOYMENT IUI 
           MOVE " N/A" TO D06-PREMIUM-NA.
           IF LN-OWNST = "IN" OR "KY" OR "TX" OR "MO" OR "NM" OR
                         "ID" OR "UT" OR "AL"
              IF LN-INSPREM(4) NOT = 0
                 MOVE LN-INSPREM(4) TO D06-PREMIUM
              ELSE
                 MOVE " N/A" TO D06-PREMIUM-NA.
           IF LN-OWNST = "OK" OR "SC" OR "TN"
              IF LN-INSPREM(6) NOT = 0
                 MOVE LN-INSPREM(6) TO D06-PREMIUM
              ELSE
                 MOVE " N/A" TO D06-PREMIUM-NA.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AD&D 
           MOVE " N/A" TO D06-PREMIUM-NA.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF LN-FEECODE(SUB) = "97"
                 MOVE LN-FEEAMT(SUB) TO D06-PREMIUM
              END-IF
           END-PERFORM.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PERSONAL PROPERTY 
           IF LN-INSPREM(3) NOT = 0
              MOVE LN-INSPREM(3) TO D06-PREMIUM
           ELSE
              MOVE " N/A" TO D06-PREMIUM-NA.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SINGLE INTEREST VEHICLE SIA - - - - - - - - - - - - - - - - - -
           MOVE " N/A" TO D06-PREMIUM-NA.
           IF LN-OWNST = "GA" OR "IN" OR "KY" OR "AL" OR "TX" OR
                         "NM" OR "ID" OR "UT"
              IF LN-INSPREM(6) NOT = 0
                 MOVE LN-INSPREM(6) TO D06-PREMIUM
              ELSE
                 MOVE " N/A" TO D06-PREMIUM-NA.
           IF LN-OWNST = "TN" OR "MS" OR "OK"
              IF LN-INSPREM(4) NOT = 0
                 MOVE LN-INSPREM(4) TO D06-PREMIUM
              ELSE
                 MOVE " N/A" TO D06-PREMIUM-NA.
           MOVE 02 TO LN-FEED.
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

           MOVE LN-OWNBR                       TO D01-BRNO.
           MOVE "/"                            TO D01-SLASH.
           MOVE LN-ACCTNO                      TO D01-NUMBER.
           MOVE LN-LOANDATE                    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D01-LOANDATE. 
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE GB-COMPANY-NAME                TO COMPANY-NAME.
           CALL "C$JUSTIFY" USING COMPANY-NAME, "L".
           MOVE COMPANY-NAME                   TO D02-BR-NAME.
           MOVE MAKER-NAME-FIRST-LAST          TO D02-MAKER.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE BR-ADR1                        TO D03-BR-ADDR1.
           MOVE JOINT-NAME-FIRST-LAST          TO D03-CO-MAKER.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR2                        TO D04-BR-ADDR2.
           MOVE MAKER-ADDRESS-1                TO D04-ADDR1.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           STRING BR-CITY, " ,", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO D04-BR-ADDR2.
           IF ( MAKER-ADDRESS-2 NOT = SPACES )
              MOVE MAKER-ADDRESS-2             TO D05-ADDR2
           ELSE
              MOVE MAKER-CSZ                   TO D05-ADDR2.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-PHONE                       TO D04-BR-PHONE.
           INSPECT D04-BR-PHONE REPLACING ALL "/" BY "-".
           IF ( MAKER-ADDRESS-2 NOT = SPACES )
              MOVE MAKER-CSZ                   TO D05-ADDR2.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-02-ROUTINE-EXIT.
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
