       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST833.
       DATE-WRITTEN.  03-05-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST833.C
      * PRINT DRIVER  :  HP UNIVERSAL
      *
      * DESCRIPTION   :  DISCLOSURE STATEMENT - SMALL LOAN
      *
      *                  FORM # 67 = BLANK
      *
      * STATE         :  TENNESSEE  (TN)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0305 PR#1428***NEW*** CALLED FROM LONPML
      * BAH 2020.0710 NET BALANCE WAS ADDING LN-PRINSRB INSTEAD OF SUBTRACTING
      *                                                         #1448
      * BAH 2022.0503 START TRCFILE WITH LOANDATE LIKE A15, NOT TRANSDATE
      * BAH 20220901 HARD CODED START
      * BAH 2023.0106 NEW SECURITY BOXES, MOVED FORM AROUND SOME. ADDED
      *               2 CHECK DISBURSEMENTS LINES AND REMOVED PAGE 2 #1584
      * BAH 2023.0609 ADDED CLOSING FEE (LN-POINTS) #1608
      * BAH 2023.1214 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1635
      * BAH 2024.1121 HARD CODED WORLD DBA AT THE TOP OF BOTH PAGES AND HAD
      *               TO MOVE DATE OVER AND BRPHONE DOWN A LINE #1688 S35Q-147
      * BAH 2024.1218 CHECK MOTOR VEHICLE IF COLLCD "AS" AND CHECK PERSONAL
      *               PROPERTY IF "SC" OR "AS" #1695 S35Q-154
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
           "@(#)FP/PST833.CBL S35 01/11/23-14:48:01 >".

       COPY "LIBFP/FPWSPST.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
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
       01  PROCEED-TOT     PIC S9(7)V99 COMP-3 VALUE 0.
       01  BORROWER-ME     PIC X(41).
       01  ME-AMT          PIC S9(5)V99 COMP-3 VALUE 0.

       01  C-S-Z           PIC X(85).
       01  ADD-ON          PIC X(25).
       01  CHECK-ADDRESS   PIC X(120).

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
      *    FMNO    : 66 
      *    STATE   : TENNESSEE  (TN)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(04).
           03  D01-JOINT                    PIC X(30).
           03  FILLER                       REDEFINES D01-JOINT.
               05  FILLER                   PIC X(06).
               05  D01-MAKER                PIC X(24).
           03  FILLER                       PIC X(01).
           03  D01-ACCTNO                   PIC Z(5)9.
           03  FILLER                       PIC X(6).
           03  D01-LOANDATE                 PIC 99/99/99.
           03  FILLER                       PIC X(14).
           03  D01-BRPHONE                  PIC 999/999/9999.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(15).
           03  D02-APR                      PIC ZZ9.99       BLANK ZERO.
           03  FILLER                       PIC X(07).
           03  D02-FINCHG                   PIC ZZZ,ZZ9.99   BLANK ZERO.
           03  FILLER                       PIC X(14).
           03  D02-FINAMT                   PIC ZZZ,ZZ9.99   BLANK ZERO.
           03  FILLER                       PIC X(13).
           03  D02-TOTAL                    PIC ZZZ,ZZ9.99   BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(12).
           03  D03-ORGTERM                  PIC ZZ9          BLANK ZERO.
           03  FILLER                       PIC X(12).
           03  D03-REGPYAMT                 PIC $$$$$$.99    BLANK ZERO.
           03  FILLER                       PIC X(14).
           03  D03-1STPYDATE                PIC 99/99/99     BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03A                REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(5).
           03  D03-SEC-BOX1               PIC X.
           03  FILLER                     PIC X(42).
           03  D03-SEC-BOX2               PIC X.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(52).
           03  D04-MDTE-DATE                PIC 99/99/99     BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(47).
           03  D05-CKAMT                  PIC Z(4)9.99.
           03  FILLER                     PIC X(2).
           03  D05-AMOUNT                 PIC Z(5)9.99.

      * THIS LINE PRINTS WITH FONT OF 8, 12CPI
       01  DETAIL-LINE-05A                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(5).
           03  D05-CHECK-ADR              PIC X(120).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(17).
           03  D06-BORROWER                 PIC X(20).
           03  FILLER                       PIC X(09).
           03  D06-NUMBER                   PIC Z(6).
           03  FILLER                       PIC X(18).
           03  D06-LOANDATE                 PIC 99/99/99.
           03  FILLER                       PIC X(09).
           03  D06-TIME                     PIC 99/99/99 BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(29).
           03  FILLER                       PIC X(36).
           03  D07-CLOSED-ADR               PIC X(30).

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
           PERFORM CLOSE-TRC1-FILE.
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

           MOVE 02     TO PST-WS-FORM-PAGE-MAX.
           PERFORM PST-FR-FORM-INITIALIZATION.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ LOAD ACCOUNT INFO. }
           PERFORM PST-INFO-MODULE.

      *- - - - - - - - - - - - - - - - - - - - -{ SETUP PAGE COPY LOOP }
           MOVE ZEROES TO PAGE-COPY-ELE.
           MOVE 01     TO PAGE-COPY-MAX.

      * SETUP TO TEST FOR "ME"
           MOVE BW-FNAME TO BORROWER-ME.
           MOVE BW-LNAME TO ADD-ON.
           INSPECT BORROWER-ME REPLACING FIRST "  " BY " +".
           INSPECT BORROWER-ME REPLACING FIRST
              "+                        " BY ADD-ON.

           MOVE LN-PROCASH TO ME-AMT.
           MOVE 0          TO PROCEED-TOT.
           MOVE 1          TO SUB.

      * CHECK FOR NETSPEND DEBIT CARD PROCEEDS
           IF BR-USE-DEBIT-CARDS = "Y"
              PERFORM CHECK-FOR-DEBIT-CARD.

           PERFORM MANUAL-CHECKS-DISBURSEMENT.
           IF PROCEED-TOT = 0
              PERFORM OPEN-CX1-FILE
              MOVE LN-ORGBR TO CX-BRNO
                               QCX1-WBEG-BRNO
                               QCX1-WEND-BRNO
              MOVE "LN"     TO CX-TYPE
                               QCX1-WBEG-TYPE
                               QCX1-WEND-TYPE
              MOVE 0        TO CX-SEQNO
                               QCX1-WBEG-SEQNO
              MOVE ALL "9"  TO QCX1-WEND-SEQNO
              PERFORM START-CX1-FILE
              PERFORM SYSTEM-CHECKS-DISBURSEMENT.

       PRINT-ROUTINE-PAGE-COPY.

           ADD 01 TO PAGE-COPY-ELE.

           IF ( PAGE-COPY-ELE > PAGE-COPY-MAX )
              GO TO PRINT-ROUTINE-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ PRINT PAGES  }
           PERFORM PRINT-PAGE-01-ROUTINE.
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

           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * 67 BLANK
           IF ( IP-FMNO NOT = 67 )
              MOVE MAKER-NAME-FIRST-LAST       TO D01-MAKER
              MOVE LN-ACCTNO                   TO D01-ACCTNO
              MOVE TRANS-DATE                  TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D01-LOANDATE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( IP-FMNO NOT = 67 )
              MOVE JOINT-NAME-FIRST-LAST       TO D01-JOINT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-PHONE                       TO D01-BRPHONE.
           INSPECT D01-BRPHONE REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           IF ( IP-FMNO NOT = 67 )
              COMPUTE D02-APR ROUNDED= LN-APRATE * 1
              MOVE LN-FINCHG                   TO D02-FINCHG
              MOVE DISCLOSED-FINAMT            TO D02-FINAMT
              MOVE TOTAL-REPAY                 TO D02-TOTAL.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( IP-FMNO NOT = 67 )
              MOVE LN-1STPYDATE                TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D03-1STPYDATE
              IF ( LN-1STPYAMT NOT = LN-REGPYAMT )
                 MOVE 1                        TO D03-ORGTERM
              ELSE 
                 MOVE LN-REGPYAMT              TO D03-REGPYAMT
                 IF ( LN-LASTPYAMT NOT = LN-REGPYAMT )
                    COMPUTE D03-ORGTERM = LN-ORGTERM - 1
                 ELSE
                    MOVE LN-ORGTERM            TO D03-ORGTERM.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( IP-FMNO NOT = 67 )
              MOVE MDTE-DATE                   TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D04-MDTE-DATE
              IF ( LN-1STPYAMT NOT = LN-REGPYAMT )
                 COMPUTE D03-ORGTERM = LN-ORGTERM - 1
                 MOVE LN-REGPYAMT              TO D03-REGPYAMT
              ELSE 
                 IF ( LN-LASTPYAMT NOT = LN-REGPYAMT )
                    MOVE 1                     TO D03-ORGTERM
                    MOVE LN-LASTPYAMT          TO D03-REGPYAMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 67
              IF LN-COLLCD = "AP" OR "PP" OR "SC" OR "AS"
      * PERSONAL PROPERTY
                 MOVE "X" TO D03-SEC-BOX1.
      * CERTAIN HOUSEHOLD GOODS, NOT USED
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 67
              IF LN-COLLCD = "AP" OR "AU" OR "AS"
      * MOTOR VEHICLE(S)
                 MOVE "X" TO D03-SEC-BOX1
              ELSE
      * UNSECURED
              IF LN-COLLCD = "UL"
                 MOVE "X" TO D03-SEC-BOX2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * OTHER (SPECIFIED IN EXHIBIT A), NOT USED
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ACQUISITION CHARGE
           IF ( IP-FMNO NOT = 67 )
              COMPUTE D05-AMOUNT = LN-SERCHG - LN-POINTS.
           MOVE 08 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INSTALLMENT ACCOUNT HANDLING CHARGE 
           IF ( IP-FMNO NOT = 67 )
              COMPUTE D05-AMOUNT = LN-INTCHG + LN-EXTCHG.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CLOSING FEE
           IF IP-FMNO NOT = 67
              MOVE LN-POINTS          TO D05-AMOUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 1. AMOUNT PAID TO YOU OR ON YOUR BEHALF
           IF IP-FMNO NOT = 67
              MOVE LN-PROCEEDS                 TO D05-AMOUNT.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AMOUNT PAID TO YOU
           IF IP-FMNO NOT = 67
              MOVE ME-AMT                      TO D05-CKAMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AMOUNT PAID TO OTHERS
           IF IP-FMNO NOT = 67
              COMPUTE D05-CKAMT = LN-PROCEEDS - ME-AMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 2. AMOUNT PAID ON YOUR PRIOR ACCOUNT 
           IF IP-FMNO NOT = 67
              COMPUTE D05-AMOUNT = LN-PRBAL + LN-PRINTDUE
                                 - LN-PRFINCHGRB - LN-PRINSRB.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AMOUNT FINANCED
           IF IP-FMNO NOT = 67
              MOVE DISCLOSED-FINAMT TO D05-AMOUNT.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PRINT THE CHECK ADDRESS IN 12 CPI
           MOVE 08                      TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER     TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.

      * CHECK ADDRESS
           MOVE SPACES TO CHECK-ADDRESS D05-CHECK-ADR.
           STRING BR-ADR1, ", ",  BR-CITY, ", ", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO CHECK-ADDRESS.
           STRING BR-STMNT1,
                  ", DBA ",
                  GB-COMPANY-NAME,
                  ", ",
                  CHECK-ADDRESS
                      DELIMITED BY "  " INTO D05-CHECK-ADR.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 10                      TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER     TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-01-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-02-ROUTINE SECTION.
      ******************************************************************

           MOVE 02 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.
           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-01-ROUTINE-EXIT.

           MOVE 10                         TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER        TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                      TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * 67 BLANK
           IF IP-FMNO NOT = 67
              MOVE MAKER-NAME-FIRST-LAST      TO D01-MAKER
              MOVE LN-ACCTNO                  TO D01-ACCTNO
              MOVE TRANS-DATE                  TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D01-LOANDATE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           IF IP-FMNO NOT = 67
              MOVE JOINT-NAME-FIRST-LAST      TO D01-JOINT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-PHONE                      TO D01-BRPHONE.
           INSPECT D01-BRPHONE REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

      * BORROWER'S NAME
           MOVE MAKER-NAME-FIRST-LAST          TO D06-BORROWER.
      * ACCOUNT NO:
           MOVE LN-ACCTNO                      TO D06-NUMBER.
      * DATE OF LOAN
           MOVE LN-LOANDATE                    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D06-LOANDATE.
      * TIME
           PERFORM GET-POSTTIME.
           MOVE LP-POSTTIME                    TO D06-TIME.
           INSPECT D06-TIME REPLACING ALL "/" BY ":".
           MOVE 18 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           IF ( JOINT-NAME-FIRST-LAST NOT = SPACES )
              STRING "& ", JOINT-NAME-FIRST-LAST
                 DELIMITED BY "  " INTO D06-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-ADDRESS                  TO D07-CLOSED-ADR.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE MAKER-CSZ                      TO D07-CLOSED-ADR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-02-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      *******************************
       GET-POSTTIME SECTION.
      *******************************
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 1         TO LP-SEQNO.
           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO LP-POSTTIME.

       EXIT-GET-POSTTIME.
           EXIT.

      *********************************************
       CHECK-FOR-DEBIT-CARD SECTION.
      *********************************************
           PERFORM OPEN-LTD1-FILE.
           MOVE LN-OWNBR  TO LTD-BRNO.
           MOVE LN-ACCTNO TO LTD-ACCTNO.
           MOVE 1         TO LTD-SEQNO.
           PERFORM READ-LTD1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-CHECK-FOR-DEBIT-CARD.

           ADD LTD-DEBIT-CARD-AMT TO ME-AMT.
       EXIT-CHECK-FOR-DEBIT-CARD.
           PERFORM CLOSE-LTD1-FILE.

      *************************************
       MANUAL-CHECKS-DISBURSEMENT SECTION.
      *************************************
           PERFORM OPEN-TRC1-FILE.
           PERFORM OPEN-CK1-FILE.

           MOVE BW-FNAME TO C-S-Z.
           MOVE BW-LNAME TO ADD-ON.
           INSPECT C-S-Z REPLACING FIRST "  " BY " +".
           INSPECT C-S-Z REPLACING FIRST
              "+                        " BY ADD-ON.

           MOVE LN-ORGBR    TO TC-BRNO
                               QTRC1-WBEG-BRNO
                               QTRC1-WEND-BRNO.
           MOVE LN-LOANDATE TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE
                                       QTRC1-WEND-KEY-DATE.
           MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
                                       QTRC1-WBEG-CKNO
                                       QTRC1-WBEG-CLASS
                                       QTRC1-WBEG-SEQ.
           MOVE ALL "9"             TO QTRC1-WEND-CKNO
                                       QTRC1-WEND-CLASS
                                       QTRC1-WEND-SEQ.

           PERFORM START-TRC1-FILE.

       NEXT-MAN-CHECK.
           PERFORM READ-TRC1-FILE-NEXT.
           IF ( IO-FG = 9 ) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO END-MAN-CHECKS-DISBURSEMENT.

      * STARTS WITH BRNO AND DATE, IF DATE DOESNT MATCH, YOU DIDNT FIND ANY
           IF TC-KEY-DATE NOT = LN-LOANDATE
              GO TO END-MAN-CHECKS-DISBURSEMENT.

           IF TC-ACCTNO NOT = LN-ACCTNO
              GO TO NEXT-MAN-CHECK.

      * MUST CHECK IF IT HAS BEEN VOIDED !!
      * THEY COULD HAVE PRINTED, VOIDED, AND RE-ISSUED
           MOVE TC-BRNO TO CK-BRNO.
           MOVE TC-CKNO TO CK-NO.
           PERFORM READ-CK1-FILE.
           IF IO-FG NOT = 0
              GO TO NEXT-MAN-CHECK
           ELSE
              IF CK-VOID = "Y" OR "R"
                 GO TO NEXT-MAN-CHECK.

           IF TC-PAYEE = BORROWER-ME
              ADD TC-AMT TO ME-AMT PROCEED-TOT
              GO TO NEXT-MAN-CHECK.

           ADD TC-AMT TO PROCEED-TOT.

           GO TO NEXT-MAN-CHECK.

       END-MAN-CHECKS-DISBURSEMENT.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-CK1-FILE.

       EXIT-MAN-CHECKS-DISBURSEMENT.
           EXIT.

      *********************************
       SYSTEM-CHECKS-DISBURSEMENT SECTION.
      *********************************
       NEXT-SYS-CHECK.
           PERFORM READ-CX1-FILE-NEXT.
           IF IO-FG = 9 OR (CX-BRNO NOT = LN-ORGBR)
              GO TO EXIT-SYS-NEXT-DISBURSEMENT.

           IF CX-REF NOT = LN-ACCTNO
              GO TO NEXT-SYS-CHECK.

           IF CX-BWPROC = "Y"
              ADD CX-AMT TO ME-AMT PROCEED-TOT
              GO TO NEXT-SYS-CHECK.

           ADD CX-AMT TO PROCEED-TOT.

           GO TO NEXT-SYS-CHECK.

       EXIT-SYS-NEXT-DISBURSEMENT.
           EXIT.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
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
       COPY "LIBLP/LPTRC1RN.CPY".

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
