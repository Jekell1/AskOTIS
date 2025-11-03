       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST820.
       AUTHOR.        AMM.
       DATE-WRITTEN.  03-12-2018.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST820.C
      *
      * DESCRIPTION   :  LOAN AGREEMENT (SA/DS)
      *
      * STATE         :  LOUISIANA
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2018.0312 PR#1307 CALLED FROM LONP5C
      * AMM 2018.0509 PR#1307B REPLACED SPOUSE WITH JOINT BW
      * BAH 2018.1022 PRINT NETSPEND DEBIT CARD AS PROCEEDS TO ME, IN
      *               PST.INFO, WORLD #1344
      * BAH 2024.0105 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0521 CHANGED D01-NUMBER BACK TO Z(6) S35Q-32
      * BAH 2024.0911 CHECK COLLATERAL BOXES AND PRINT PHYSICAL ADDRESS
      *               OF COLLATERAL #1677 S35Q-127
      * BAH 2024.1217 CHECK MOTOR VEHICLE IF COLLCD "AS" AND CHECK CONSUMER
      *               GOODS IF "SC" OR "AS", PAGE 2 ALSO #1695 S35Q-154
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
           "@(#)FP/PST820.CBL S35 12/20/21-11:51:48 >".

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

       01  PRIOR-AMT                   PIC S9(5)V99  COMP-3 VALUE 0.
       01  NON-REC                     PIC S9(5)V99  COMP-3 VALUE 0.
       01  AD-D-INS                    PIC S9(5)V99  COMP-3 VALUE 0.
       01  ROAD-GARD                   PIC S9(5)V99  COMP-3 VALUE 0.
       01  ROAD-TYPE                   PIC X VALUE SPACES.
       01  OT-FEE                      PIC S9(5)V99  COMP-3 VALUE 0.
       01  DOC-FEE                     PIC S9(5)V99  COMP-3 VALUE 0.
       01  WS-PHYSICAL-ADR             PIC X(33).
       01  WS-PHYSICAL-CSZ             PIC X(33).

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
      *    CUSTOMER: WORLD ACCEPTANCE CORP.
      *    FMNO    : 80 
      *    STATE   : LOUISIANA   (LA)
      *=================================================================
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(04).
           03  D01-BR-NAME                    PIC X(50).
           03  FILLER                         REDEFINES D01-BR-NAME.
               05  FILLER                     PIC X(01).
               05  D01-NUMBER                 PIC Z(06).
               05  FILLER                     PIC X(03).
               05  D01-CLASS                  PIC Z9.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(04).
           03  D02-BW-NAME                    PIC X(30).
           03  FILLER                         REDEFINES D02-BW-NAME.
               05  D02-ADDR                   PIC X(24).
           03  FILLER                         PIC X(01).
           03  D02-COMAKER                     PIC X(30).
           03  FILLER                         PIC X(04).
           03  D02-SEC-BOX                    PIC X.
           03  FILLER                         PIC X(5).
           03  D02-UNSECURED                  PIC X(9).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(04).
           03  D03-LOANDATE                   PIC 99/99/99.
           03  FILLER                         PIC X(01).
           03  D03-1STPYDATE                  PIC 99/99/99.
           03  FILLER                         PIC X(07).
           03  D03-MDTE                       PIC 99/99/99.
           03  FILLER                         PIC X(02).
           03  D03-ODDPYAMT                   PIC ZZZZZ.99.
           03  FILLER                         PIC X(03).
           03  D03-REGPYAMT                   PIC ZZZZZ.99.
           03  FILLER                         PIC X(02).
           03  D03-TERM                       PIC ZZ9.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(44).
           03  D04-TO-TEXT                    PIC X(25).
           03  FILLER                         REDEFINES D04-TO-TEXT.
               05  FILLER                     PIC X(07).
               05  D04-INSPREM                PIC Z(5).99.
           03  D04-TO-ASTERISK                PIC X.
           03  FILLER                         PIC X(01).
           03  D04-AMT4                       PIC ZZZ,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(05).
           03  D05-ASTERISK                   PIC X.
           03  D05-AMT1                       PIC ZZZ,ZZ9.99.
           03  FILLER                         REDEFINES D05-AMT1.
               05  FILLER                     PIC X(02).
               05  D05-NONE1                  PIC X(04).
           03  FILLER                         PIC X(08).
           03  D05-AMT2                       PIC ZZZZZ.99.
           03  FILLER                         REDEFINES D05-AMT2.
               05  FILLER                     PIC X(02).
               05  D05-NONE2                  PIC X(04).
           03  FILLER                         PIC X(04).
           03  D05-AMT3                       PIC ZZZZZ.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(67).
           03  D06-RETAIN-TEXT                PIC X(25).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(06).
           03  D07-PPCOVR                     PIC ZZZ,ZZ9.99.
           03  FILLER                         PIC X(26).
           03  D07-PPPREM                     PIC ZZ,ZZ9.99.
           03  FILLER                         PIC X(12).
           03  D07-PPTERM                     PIC ZZ9.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-08                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(03).
           03  D08-BOX                        PIC X(01).
           03  FILLER                         PIC X(01).
           03  D08-YEAR                       PIC X(04).
           03  FILLER                         PIC X(05).
           03  D08-MAKE                       PIC X(20).
           03  FILLER                         PIC X(02).
           03  D08-MODEL                      PIC X(22).
           03  FILLER                         PIC X(02).
           03  D08-VIN                        PIC X(20).
           03  FILLER                         PIC X(09).
           03  D08-NO                         PIC X(03).

       01  DETAIL-LINE-09                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(25).
           03  D09-PHYSICAL-ADR               PIC X(36).
           03  FILLER                         PIC X.
           03  D09-PHYSICAL-CSZ               PIC X(30).

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

           MOVE 03     TO PST-WS-FORM-PAGE-MAX.
           PERFORM PST-FR-FORM-INITIALIZATION.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ LOAD ACCOUNT INFO. }
           PERFORM PST-INFO-MODULE.

      *- - - - - - - - - - - - - - - - - - -{ NUMBER OF CHECKS ON FORM }
           MOVE 06     TO CHECK-MAX-CUSTOMIZE.
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

           MOVE 10.00                      TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER        TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                      TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           MOVE GB-COMPANY-NAME            TO D01-BR-NAME. 
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR1                    TO D01-BR-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO D01-BR-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-ACCTNO                  TO D01-NUMBER.
           MOVE LN-CLASS                   TO D01-CLASS.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-NAME-FIRST-LAST      TO D02-BW-NAME.
           MOVE JOINT-NAME-FIRST-LAST      TO D02-COMAKER.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE MAKER-ADDRESS              TO D02-ADDR.
           IF JOINT-NAME-FIRST-LAST NOT = SPACES
              MOVE JOINT-ADDRESS           TO D02-COMAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE MAKER-CSZ                  TO D02-ADDR.
           IF JOINT-NAME-FIRST-LAST NOT = SPACES
              MOVE JOINT-CSZ               TO D02-COMAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      * THE GOODS OR PROPERTY BEING PURCHASED
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CONSUMER GOODS
           IF LN-COLLCD = "AP" OR "PP" OR "SC" OR "AS"
              MOVE "X" TO D02-SEC-BOX.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * MOTOR VEHICLE
           IF LN-COLLCD = "AU" OR "AP" OR "AS"
              MOVE "X" TO D02-SEC-BOX.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-COLLCD = "UL"
              MOVE "X" TO D02-SEC-BOX
              MOVE "UNSECURED" TO D02-UNSECURED.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-LOANDATE                TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D03-LOANDATE. 
           MOVE LN-1STPYDATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D03-1STPYDATE.
           MOVE MDTE-DATE                  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D03-MDTE.
           MOVE LN-1STPYAMT                TO D03-ODDPYAMT.
           MOVE LN-SUM-REGPYAMT            TO D03-REGPYAMT.
           MOVE LN-SUM-ORGTERM             TO D03-TERM.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM GET-FEES.

      * 1. AMOUNT PAID ON YOUR ACCT
           COMPUTE PRIOR-AMT = LN-PRBAL + LN-PRINTDUE
                          - LN-PRFINCHGRB - LN-PRINSRB.
           IF PRIOR-AMT = 0
              MOVE "NONE"                  TO D05-NONE1
           ELSE
              MOVE PRIOR-AMT               TO D05-AMT1.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 2. CERTAIN AMOUNTS PAID
           IF (LN-PROCEEDS + AD-D-INS + ROAD-GARD) = 0
              MOVE "NONE"                  TO D05-NONE1
           ELSE
              COMPUTE D05-AMT1 = LN-PROCEEDS + AD-D-INS + ROAD-GARD.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 3. NET PROCEEDS
           IF LN-PROCEEDS = 0 AND PRIOR-AMT = 0 AND AD-D-INS = 0 AND
                                  ROAD-GARD = 0
              MOVE "NONE"                  TO D05-NONE1
           ELSE
              COMPUTE D05-AMT1 = LN-PROCEEDS + AD-D-INS + ROAD-GARD
                               + PRIOR-AMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 4. DISABILITY INS
           IF LN-INSPREM(2) = 0
              MOVE "NONE"                  TO D05-NONE1
           ELSE
              MOVE "*"                     TO D05-ASTERISK
              MOVE LN-INSPREM(2)           TO D05-AMT1.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      * 5. LIFE INS
           IF LN-INSPREM(1) = 0
              MOVE "NONE"                  TO D05-NONE1 D05-NONE2
           ELSE
              MOVE "*"                     TO D05-ASTERISK
              MOVE LN-INSPREM(1)           TO D05-AMT1
              MOVE LN-INSCOVR(1)           TO D05-AMT2.
      * TO: BORROWER 
           MOVE CHECK-AMT-TO-BORROWER      TO D04-AMT4.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 6. FIRE & MISC.
           IF LN-INSPREM(3) = 0
              MOVE "NONE"                  TO D05-NONE1 D05-NONE2
           ELSE
              MOVE "*"                     TO D05-ASTERISK
              MOVE LN-INSPREM(3)           TO D05-AMT1
              MOVE LN-INSCOVR(3)           TO D05-AMT2.
      * TO:
           IF AD-D-INS NOT = 0
              MOVE BR-ADD-INS-FEECODE      TO CDB-CODE
              PERFORM GET-FEECODE
              MOVE CDB-DESC                TO D04-TO-TEXT
              MOVE "*"                     TO D04-TO-ASTERISK
              MOVE AD-D-INS                TO D04-AMT4.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 7. S.I. AUTO
           IF LN-INSPREM(6) = 0
              MOVE "NONE"                  TO D05-NONE1 D05-NONE2
           ELSE
              MOVE "*"                     TO D05-ASTERISK
              MOVE LN-INSPREM(6)           TO D05-AMT1
              MOVE LN-INSCOVR(6)           TO D05-AMT2.
      * TO:
           IF ROAD-GARD NOT = 0
              IF ROAD-TYPE = "S"
                 MOVE BR-ROAD-G-SGL-FEECODE TO CDB-CODE
              ELSE
                 MOVE BR-ROAD-G-FAM-FEECODE TO CDB-CODE
              END-IF
              PERFORM GET-FEECODE
              MOVE CDB-DESC                 TO D04-TO-TEXT
              MOVE "*"                      TO D04-TO-ASTERISK
              MOVE ROAD-GARD                TO D04-AMT4.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 8. NON-FILING INS.
           IF NON-REC = 0
              MOVE "NONE"                  TO D05-NONE1
           ELSE
              MOVE NON-REC                 TO D05-AMT1.
           MOVE CHECK-TXT(1)               TO D04-TO-TEXT
           MOVE CHECK-AMT(1)               TO D04-AMT4.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 9. OFFICIAL FEES
           IF OT-FEE = 0
              MOVE "NONE"                  TO D05-NONE1
           ELSE
              MOVE OT-FEE                  TO D05-AMT1.
           MOVE CHECK-TXT(2)               TO D04-TO-TEXT
           MOVE CHECK-AMT(2)               TO D04-AMT4.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AMOUNT FINANCED- - - - - - - - - - - - - - - - - - - - - - - - -
           COMPUTE D05-AMT1 = LN-PROCEEDS + PRIOR-AMT + AD-D-INS
                         + LN-INSPREM(2) + LN-INSPREM(1) + LN-INSPREM(3)
                         + LN-INSPREM(6) + NON-REC + OT-FEE + ROAD-GARD.
           MOVE CHECK-TXT(3)               TO D04-TO-TEXT
           MOVE CHECK-AMT(3)               TO D04-AMT4.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DOC FEE
           IF DOC-FEE = 0
              MOVE "NONE"                  TO D05-NONE1
           ELSE
              MOVE DOC-FEE                 TO D05-AMT1.
           MOVE CHECK-TXT(4)               TO D04-TO-TEXT
           MOVE CHECK-AMT(4)               TO D04-AMT4.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ORIG FEE
           IF LN-SERCHG = 0
              MOVE "NONE"                  TO D05-NONE1
           ELSE
             MOVE LN-SERCHG                TO D05-AMT1.
      * TOTAL PAID 
           COMPUTE D04-AMT4 = LN-PROCEEDS + AD-D-INS + ROAD-GARD.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INTEREST CHARGE
           IF (LN-INTCHG + LN-EXTCHG) = 0
              MOVE "NONE"                  TO D05-NONE1
           ELSE
              COMPUTE D05-AMT1 = LN-INTCHG + LN-EXTCHG.
           COMPUTE D05-AMT3 ROUNDED = LN-EFFRATE * 1.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * FINANCE CHARGE 
           MOVE LN-FINCHG                  TO D05-AMT1.
           COMPUTE D05-AMT3 ROUNDED = LN-APRATE * 1.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TOTAL OF PAYMENTS
           MOVE TOTAL-REPAY                TO D05-AMT1.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           IF LN-INSURED NOT = "N" OR LN-INSPREM(6) NOT = 0 OR
                ROAD-GARD NOT = 0 OR AD-D-INS NOT = 0
              MOVE "* WE MAY RETAIN A PORTION" TO D06-RETAIN-TEXT.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-INSURED NOT = "N" OR LN-INSPREM(6) NOT = 0 OR
                ROAD-GARD NOT = 0 OR AD-D-INS NOT = 0
              MOVE "  OF THESE CHARGES." TO D06-RETAIN-TEXT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SINGLE LEVEL
           IF LN-INSTYPES-LL
              IF LN-INSURED NOT = "J"
                 MOVE LN-INSPREM(1) TO D04-INSPREM.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * JOINT LEVEL
           IF LN-INSTYPES-LL
              IF LN-INSURED = "J"
                 MOVE LN-INSPREM(1)        TO D04-INSPREM.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CREDIT DISABILITY
           IF LN-INSTYPES-AH
               MOVE LN-INSPREM(2)          TO D04-INSPREM.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-INSPREM(3)              TO D07-PPPREM.
           MOVE INS-TERM(3)                TO D07-PPTERM.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-INSCOVR(3)              TO D07-PPCOVR.
           MOVE 01 TO LN-FEED.
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

           MOVE 10.00                      TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER        TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                      TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * THE FOLLOWING DESCRIBED MOTOR VEHICLE WITH ALL EQUIPMENT
           IF LN-COLLCD = "AU" OR "AP" OR "AS"
              MOVE "X" TO D08-BOX.
           MOVE 16 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE AUTO-MAKE(1)               TO D08-MAKE.
           MOVE AUTO-MODEL(1)              TO D08-MODEL.
           MOVE AUTO-YEAR(1)               TO D08-YEAR.
           MOVE AUTO-ID(1)                 TO D08-VIN.
           IF AUTO-MAKE(1) NOT = SPACES OR AUTO-MAKE(2) NOT = SPACES
              IF ( LN-INSPREM(6) NOT = 0 )
                 MOVE "YES"                   TO D08-NO
              ELSE
                 MOVE "NO"                    TO D08-NO.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
     
           MOVE AUTO-MAKE(2)               TO D08-MAKE.
           MOVE AUTO-MODEL(2)              TO D08-MODEL.
           MOVE AUTO-YEAR(2)               TO D08-YEAR.
           MOVE AUTO-ID(2)                 TO D08-VIN.
           IF AUTO-MAKE(2) NOT = SPACES OR AUTO-MAKE(2) NOT = SPACES
              IF ( LN-INSPREM(6) NOT = 0 )
                 MOVE "YES"                   TO D08-NO
              ELSE
                 MOVE "NO"                    TO D08-NO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * THE FOLLOWING CONSUMER GOODS DESCRIBED BELOW
           IF LN-COLLCD = "PP" OR "AP" OR "SC" OR "AS"
              MOVE "X" TO D08-BOX.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-COLLCD = "PP" OR "AP" OR "SC" OR "AS"
      * LOCATED IN OR ABOUT YOUR RESIDENCE AT;
              PERFORM GET-PHYSICAL-ADDRESS
              MOVE WS-PHYSICAL-ADR TO D09-PHYSICAL-ADR
              MOVE WS-PHYSICAL-CSZ TO D09-PHYSICAL-CSZ.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * OTHER (DESCRIBE)
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * THIS LOAN IS UNSECURED
           IF LN-COLLCD = "UL"
              MOVE "X" TO D08-BOX
           MOVE 01 TO LN-FEED.
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

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-03-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      ******************************************************************
       GET-PHYSICAL-ADDRESS SECTION.
      ******************************************************************

           PERFORM OPEN-BX1-FILE.
           MOVE LN-SSNO(1) TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO WS-PHYSICAL-ADR
                             WS-PHYSICAL-CSZ
              GO TO EXIT-GET-PHYSICAL-ADDRESS.

           IF BX-ALT-ADR2 NOT = SPACES
              STRING BX-ALT-ADR1,", ",BX-ALT-ADR2 DELIMITED BY "   "
               INTO WS-PHYSICAL-ADR
           ELSE
              MOVE BX-ALT-ADR1 TO WS-PHYSICAL-ADR.

           IF BX-ALT-CITY = SPACES AND
               BX-ALT-STATE = SPACES AND
                 BX-ALT-ZIP = SPACES
              GO TO EXIT-GET-PHYSICAL-ADDRESS.

           MOVE BX-ALT-CITY TO WS-PHYSICAL-CSZ.
           INSPECT WS-PHYSICAL-CSZ REPLACING FIRST "      "
                                             BY ", ++ +".
           INSPECT WS-PHYSICAL-CSZ REPLACING FIRST "++"
                                             BY BX-ALT-STATE.
           INSPECT WS-PHYSICAL-CSZ REPLACING FIRST "+         "
                                             BY BX-ALT-ZIP.

       EXIT-GET-PHYSICAL-ADDRESS.
           EXIT.

      ********************************
       GET-FEES SECTION.
      *************************************
           MOVE 0 TO NON-REC DOC-FEE OT-FEE AD-D-INS ROAD-GARD.
           MOVE SPACES TO ROAD-TYPE.
           MOVE 1 TO SUB.
       NEXT-LN-FEE.
           IF SUB > 10
              GO TO EXIT-FIND-FEE.
           IF LN-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-LN-FEE.

           IF LN-FEECODE(SUB) = "90"
              ADD LN-FEEAMT(SUB) TO NON-REC
           ELSE
           IF LN-FEECODE(SUB) = "91"
              ADD LN-FEEAMT(SUB) TO DOC-FEE
           ELSE
              IF LN-FEECODE(SUB) = BR-ADD-INS-FEECODE
                 ADD LN-FEEAMT(SUB) TO AD-D-INS
              ELSE
                 IF LN-FEECODE(SUB) = BR-ROAD-G-SGL-FEECODE
                    ADD LN-FEEAMT(SUB) TO ROAD-GARD
                    MOVE "S" TO ROAD-TYPE
                 ELSE
                    IF LN-FEECODE(SUB) = BR-ROAD-G-FAM-FEECODE
                       ADD LN-FEEAMT(SUB) TO ROAD-GARD
                       MOVE "F" TO ROAD-TYPE
                    ELSE
                       ADD LN-FEEAMT(SUB) TO OT-FEE.
           ADD 1 TO SUB.
           GO TO NEXT-LN-FEE.
       EXIT-FIND-FEE.
           EXIT.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       GET-FEECODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE LN-OWNBR TO CDB-BRANCH.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO CDB-DESC.

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
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBFP/FPIOPST.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBX1RN.CPY".
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
