       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST826.
       AUTHOR.        AMM.
       DATE-WRITTEN.  02-04-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST826.C
      *
      * DESCRIPTION   :  DISCLOSURE STATEMENT - LARGE LOAN
      *
      * STATE         :  TEXAS     (TX)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0204 PR#1418 NEW, CALLED FROM LONP3F 
      * AMM 2020.0407 PR#1418C ADDED PARAGRAPH TO ARBITRATION AGREEMENT
      * BAH 2022.0503 START TRCFILE WITH LOANDATE LIKE A15, NOT TRANSDATE
      * BAH 20220901 HARD CODED START
      * BAH 2024.0105 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0521 CHANGED D01-ACCTNO BACK TO Z(5)9 S35Q-32
      * BAH 2024.0523 BORROWER-ME WAS NOT GETTING SET FOR PROCEEDS GIVEN TO
      *               ME S35Q-40
      * BAH 2024.1219 CHECK MOTOR VEHICLE IF COLLCD "AS" AND CHECK PERSONAL
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
           "@(#)FP/PST826.CBL S35 12/17/21-14:16:39 >".

       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       COPY "LIBFP/FPWSPST.CPY".

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


       01  NON-FILE        PIC S9(7)V99 COMP-3 VALUE 0.
       01  MOTOR-CLUB      PIC S9(7)V99 COMP-3 VALUE 0.
       01  95-FEE          PIC S9(7)V99 COMP-3 VALUE 0.
       01  OTHER-FEE       PIC S9(7)V99 COMP-3 VALUE 0.

       01  CL-REF          PIC S9(5)V99  COMP-3 VALUE 0.
       01  AH-REF          PIC S9(5)V99  COMP-3 VALUE 0.
       01  PP-REF          PIC S9(5)V99  COMP-3 VALUE 0.
       01  O3-REF          PIC S9(5)V99  COMP-3 VALUE 0.
       01  O2-REF          PIC S9(5)V99  COMP-3 VALUE 0.
       01  O1-REF          PIC S9(5)V99  COMP-3 VALUE 0.
       01  MF-REF          PIC S9(5)V99 COMP-3 VALUE 0.
       01  SER-REF         PIC S9(5)V99 COMP-3 VALUE 0.
       01  INT-REF         PIC S9(5)V99 COMP-3 VALUE 0.

       01  PROC-WRK        PIC S9(7)V99 COMP-3 VALUE 0.
       01  PROCEED-BAL     PIC S9(7)V99 COMP-3 VALUE 0.
       01  PROCEED-TOT     PIC S9(7)V99 COMP-3 VALUE 0.
       01  ME-AMT          PIC S9(5)V99 COMP-3 VALUE 0.
       01  BORROWER-ME     PIC X(41).

       01  C-S-Z           PIC X(85).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).
       01  ADD-ON          PIC X(25).

      ***********************************
      *      CONTRACT VARIABLE TEXT     *
      ***********************************
       01  TEXT4.
           03  T4-TABLE OCCURS 3 TIMES.
               05  T4-AMT     PIC S9(6)V99 COMP-3.
               05  T4-DESC    PIC X(20).

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
      *    FMNO    : 108
      *    STATE   : TEXAS     (TX)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D01-ACCTNO                   PIC Z(5)9.
           03  FILLER                       PIC X(40).
           03  D01-LOANDATE                 PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D02-CREDITOR                 PIC X(30).
           03  FILLER                       PIC X(15).
           03  D02-BORROWER                 PIC X(31).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(15).
           03  D03-APR                      PIC ZZ9.99.
           03  FILLER                       PIC X(06).
           03  D03-FINCHG                   PIC ZZ,ZZZ,ZZ9.99.
           03  FILLER                       PIC X(10).
           03  D03-PP-BOX                   PIC X(01).
           03  D03-FINAMT                   PIC ZZ,ZZZ,ZZ9.99.
           03  FILLER                       PIC X(10).
           03  D03-TOTAL                    PIC ZZ,ZZZ,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(04).
           03  D04-PP-BOX                 PIC X(01).
           03  D04-MV-BOX                 PIC X(01).
           03  FILLER                     PIC X(03).
           03  D04-NOREGPY                PIC ZZ9            BLANK ZERO.
           03  FILLER                     PIC X(10).
           03  D04-REGPYAMT               PIC $$$,$$9.99     BLANK ZERO.
           03  FILLER                     PIC X(25).
           03  D04-PAY-DATE               PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(49).
           03  D05-SECURITY               PIC X(43).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(49).
           03  D06-CHECK-PAYEE            PIC X(25).
           03  FILLER                     PIC X(02).
           03  D06-CHECK-AMT              PIC ZZ,ZZZ.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(80).
           03  D07-INS-AMT                PIC ZZZ,ZZZ.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-08                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(04).
           03  D08-SPR-AMOUNTS            PIC X(36).
           03  FILLER                     REDEFINES D08-SPR-AMOUNTS.
               05  FILLER                 PIC X(04).
               05  D08-THROUGH-2          PIC $$$,$$9.
               05  FILLER                 PIC X(05).
               05  D08-UNPAID             PIC $$$,$$9.
           03  FILLER                     REDEFINES D08-SPR-AMOUNTS.
               05  D08-GREATER-1          PIC $$$,$$9.
               05  FILLER                 PIC X(06).
               05  D08-THROUGH-1          PIC $$$,$$9.
               05  FILLER                 PIC X(03).
               05  D08-GREATER-2          PIC $$$,$$9.
           03  FILLER                     PIC X(42).
           03  D08-AMOUNT                 PIC ZZZ,ZZZ.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-09                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(26).
           03  D09-PP-PREM                PIC Z(5).99   BLANK ZERO.
           03  FILLER                     PIC X(05).
           03  D09-PP-TERM                PIC Z(3).
           03  FILLER                     PIC X(06).
           03  D09-INS-BOX                PIC X(01).
           03  FILLER                     PIC X(23).
           03  D09-INS-PREM               PIC Z(5).99   BLANK ZERO.
           03  FILLER                     PIC X(07).
           03  D09-INS-TERM               PIC Z(3).

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

      * NEED GET-FEES BEFORE CHECKS, BECAUSE WE ARE GOING TO PRINT
      * MOTOR CLUB (A FEE) IN THE DISBURSEMENTS

           PERFORM GET-FEES.

           MOVE LN-PROCASH TO ME-AMT PROCEED-BAL.
           MOVE 0 TO PROC-WRK.

           MOVE 1 TO SUB.
           MOVE " " TO T4-DESC(1) T4-DESC(2) T4-DESC(3).
           MOVE 0 TO T4-AMT(1) T4-AMT(2) T4-AMT(3).
           IF MOTOR-CLUB NOT = 0
              MOVE "UNITED MOTOR CLUB" TO T4-DESC(1)
              MOVE MOTOR-CLUB TO T4-AMT(1)
              ADD 1 TO SUB.

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

      * CHANGE FONT SIZE TO 12 CPI
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           IF NOT ( IP-FMNO = 109 )
              MOVE LN-ACCTNO                   TO D01-ACCTNO
              MOVE LN-LOANDATE                 TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D01-LOANDATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF NOT ( IP-FMNO = 109 )
              MOVE MAKER-NAME-FIRST-LAST       TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE GB-COMPANY-NAME                TO D02-CREDITOR.
           IF NOT ( IP-FMNO = 109 )
              MOVE JOINT-NAME-FIRST-LAST       TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-ADR1                        TO D02-CREDITOR.
           IF NOT ( IP-FMNO = 109 )
              MOVE MAKER-ADDRESS-1             TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              MOVE BR-ADR2           TO D02-CREDITOR
           ELSE
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF NOT ( IP-FMNO = 109 )
              IF MAKER-ADDRESS-2 NOT = SPACES
                 MOVE MAKER-ADDRESS-2   TO D02-BORROWER
              ELSE
                 MOVE MAKER-CSZ         TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D02-CREDITOR.
           IF NOT ( IP-FMNO = 109 )
              IF MAKER-ADDRESS-2 NOT = SPACES
                 MOVE MAKER-CSZ         TO D02-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( IP-FMNO = 109 )
              GO TO PRINT-PAGE-01-ROUTINE-EXIT.

           COMPUTE D03-APR ROUNDED = LN-APRATE * 1.
           MOVE LN-FINCHG                      TO D03-FINCHG.
           MOVE DISCLOSED-FINAMT               TO D03-FINAMT.
           MOVE TOTAL-REPAY                    TO D03-TOTAL.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-1STPYAMT NOT = LN-REGPYAMT
              MOVE 1                TO D04-NOREGPY
              MOVE LN-1STPYAMT      TO D04-REGPYAMT
           ELSE
              MOVE LN-REGPYAMT      TO D04-REGPYAMT
              IF LN-LASTPYAMT NOT = LN-REGPYAMT
                 COMPUTE D04-NOREGPY = LN-ORGTERM - 1
              ELSE
                 MOVE LN-ORGTERM    TO D04-NOREGPY.
           MOVE LN-1STPYDATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D04-PAY-DATE.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-1STPYAMT NOT = LN-REGPYAMT
              COMPUTE D04-NOREGPY = LN-ORGTERM - 1
              MOVE LN-REGPYAMT      TO D04-REGPYAMT
           ELSE
              IF LN-LASTPYAMT NOT = LN-REGPYAMT
                 MOVE 1             TO D04-NOREGPY
                 MOVE LN-LASTPYAMT  TO D04-REGPYAMT.
           MOVE MDTE-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D04-PAY-DATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * COLLATERAL 
           IF LN-COLLCD = "PP" OR "SC"
              MOVE "SEE EXHIBIT A" TO D05-SECURITY
           ELSE
           IF LN-COLLCD = "AU"
              STRING AUTO-YEAR(1), " ", AUTO-MAKE(1), " ", AUTO-MODEL(1)
                 DELIMITED BY "  " INTO D05-SECURITY
           ELSE
           IF LN-COLLCD = "AP" OR "AS"
              STRING AUTO-YEAR(1), " ", AUTO-MAKE(1), " ",
                     AUTO-MODEL(1), ", ", "SEE EX. A"
                        DELIMITED BY "  " INTO D05-SECURITY.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * MOTOR VEHICLE
           IF LN-COLLCD = "AP" OR "AU" OR "AS"
              MOVE "X" TO D04-MV-BOX.

      * PERSONAL PROPERTY
           IF LN-COLLCD = "PP" OR "AP" OR "SC" OR "AS"
              MOVE "X" TO D03-PP-BOX.

           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 1. AMOUNT FINANCED 
           MOVE DISCLOSED-FINAMT               TO D08-AMOUNT.

           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 2. AMOUNT GIVEN TO ME
           MOVE ME-AMT                         TO D08-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-PRLNNO NOT = 0
              PERFORM FIND-PRIOR-REFUNDS.
           COMPUTE D08-AMOUNT = LN-PRBAL + LN-PRINTDUE
                              - LN-PRFINCHGRB - LN-PRINSRB.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 4. AMOUNT PAID TO OTHERS ON MY BEHALF
           COMPUTE D08-AMOUNT = LN-INSPREM(3) + LN-INSPREM(6)
                              + LN-INSPREM(1) + LN-INSPREM(2)
                              + LN-INSPREM(4) + NON-FILE + 95-FEE
                              + OTHER-FEE
                              + T4-AMT(1) + T4-AMT(2) + T4-AMT(3).
      * DO NOT INCLUDE MOTOR CLUB FEE IN THE ADDITION HERE BECAUSE
      * IT IS IN T4-AMT(1) !!
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 4A. COST OF PERSONAL PROPERTY
           MOVE LN-INSPREM(3)                  TO D07-INS-AMT.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 4B. COST OF SINGLE-INTEREST INSURANCE
           MOVE LN-INSPREM(6)                  TO D07-INS-AMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * UNPAID CASH ADVANCE THAT IS  ___ OR LESS;
           MOVE SP-CAL-BREAK(1,1) TO D08-UNPAID.
      * 4C. COST OF OPTIONAL CREDIT INSURANCE 
      * LIFE
           MOVE LN-INSPREM(1)                  TO D06-CHECK-AMT.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DISABILITY 
           MOVE LN-INSPREM(2)                  TO D06-CHECK-AMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ___ THROUGH ___; AND (3)
           MOVE SP-CAL-BREAK(1,1)              TO D08-GREATER-1.
           MOVE SP-CAL-BREAK(1,2)              TO D08-THROUGH-1.
      * INVOLUNTARY UNEMPLOYMENT 
           MOVE LN-INSPREM(4)                  TO D06-CHECK-AMT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * GREATER THAN ___
           MOVE SP-CAL-BREAK(1,2)              TO D08-GREATER-2.
      * TOTAL C: 
           COMPUTE D08-AMOUNT = LN-INSPREM(1) + LN-INSPREM(2)
                              + LN-INSPREM(4).
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *THROUGH __ . YOU FIGURE...
           MOVE 17750                          TO D08-THROUGH-2.
      * D. NON-FILING
           MOVE NON-FILE                       TO D08-AMOUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * E. OFFICIAL FEES
           MOVE 95-FEE                         TO D08-AMOUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * F. PAYABLE TO: 
           MOVE T4-AMT(1)                      TO D06-CHECK-AMT.
           MOVE T4-DESC(1)                     TO D06-CHECK-PAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE T4-AMT(2)                      TO D06-CHECK-AMT.
           MOVE T4-DESC(2)                     TO D06-CHECK-PAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE T4-AMT(3)                      TO D06-CHECK-AMT.
           MOVE T4-DESC(3)                     TO D06-CHECK-PAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TOTAL F: 
           COMPUTE D08-AMOUNT = T4-AMT(1) + T4-AMT(2) + T4-AMT(3).
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 5. PREPAID FINANCE CHARGE
           MOVE LN-SERCHG                      TO D08-AMOUNT.
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

           IF ( IP-FMNO = 109 )
              GO TO PRINT-PAGE-02-ROUTINE-EXIT.

      * CHANGE FONT SIZE TO 12 CPI 
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * PERSONAL PROPERTY
           IF LN-INSPREM(3) NOT = 0
              MOVE 3             TO ITRM-SUB
              MOVE TRANS-DATE    TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              MOVE "X"           TO D04-PP-BOX
              MOVE LN-INSPREM(3) TO D09-PP-PREM
              MOVE ITRM-INSTERM  TO D09-PP-TERM.
      * SINGLE INTEREST INSURANCE (VEHICLE)
           IF LN-INSPREM(6) NOT = 0
              MOVE 6             TO ITRM-SUB
              MOVE TRANS-DATE    TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              MOVE "X"           TO D09-INS-BOX
              MOVE LN-INSPREM(6) TO D09-INS-PREM
              MOVE ITRM-INSTERM  TO D09-INS-TERM.
           MOVE 08 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CREDIT LIFE
           IF LN-INSPREM(1) NOT = 0
              MOVE INS-TERM(1)      TO D09-INS-TERM
      * BOTH BORROWERS
              IF LN-INSURED = "J"
                 MOVE "X"           TO D09-INS-BOX
                 MOVE LN-INSPREM(1) TO D09-INS-PREM
              ELSE
      * ONE BORROWER
                 MOVE "X"           TO D04-PP-BOX
                 MOVE LN-INSPREM(1) TO D09-PP-PREM.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CREDIT DISABILITY
           IF LN-INSPREM(2) NOT = 0
              MOVE INS-TERM(2)      TO D09-INS-TERM
      * BOTH BORROWERS
              IF SP-INSAH-JOINT-FG = "Y"
                 MOVE "X"           TO D09-INS-BOX
                 MOVE LN-INSPREM(2) TO D09-INS-PREM
              ELSE
      * ONE BORROWER
                 MOVE "X"           TO D04-PP-BOX
                 MOVE LN-INSPREM(2) TO D09-PP-PREM.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CREDIT IUI, ONE BORROWER
           IF LN-INSPREM(4) NOT = 0
              MOVE "X"              TO D04-PP-BOX
              MOVE LN-INSPREM(4)    TO D09-INS-PREM
              MOVE INS-TERM(4)      TO D09-INS-TERM.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-02-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

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

      ******************************
       FIND-PRIOR-REFUNDS SECTION.
      ******************************
           MOVE 0 TO O1-REF CL-REF AH-REF PP-REF O3-REF MF-REF
                        INT-REF O2-REF.
           PERFORM OPEN-LP1-FILE.
           MOVE LN-OWNBR  TO LP-BRNO
                             QLP1-WBEG-BRNO
                             QLP1-WEND-BRNO.
           MOVE LN-PRLNNO TO LP-ACCTNO
                             QLP1-WBEG-ACCTNO
                             QLP1-WEND-ACCTNO.
           MOVE 99999     TO LP-SEQNO
                             QLP1-WEND-SEQNO.
           MOVE ALL "0"   TO QLP1-WBEG-SEQNO
           PERFORM START-LP1-FILE-NOT-GREATER.
       NEXT-PRIOR-REFUND.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG = 9
              GO TO EXIT-FIND-PRIOR-REFUNDS.
           IF LP-ACCTNO NOT = LN-PRLNNO
              GO TO EXIT-FIND-PRIOR-REFUNDS.
           IF LP-REV = "Y"
              GO TO NEXT-PRIOR-REFUND.
           IF LP-TRCD = "RC"
              ADD LP-TRAMT TO CL-REF
           ELSE
           IF LP-TRCD = "RA"
              ADD LP-TRAMT TO AH-REF
           ELSE
           IF LP-TRCD = "RP"
              ADD LP-TRAMT TO PP-REF
           ELSE
           IF LP-TRCD = "R1"
              ADD LP-TRAMT TO O1-REF
           ELSE
           IF LP-TRCD = "R3"
              ADD LP-TRAMT TO O3-REF
           ELSE
           IF LP-TRCD = "R2"
              ADD LP-TRAMT TO O2-REF
           ELSE
           IF LP-TRCD = "RI"
              ADD LP-TRAMT TO INT-REF
           ELSE
           IF LP-TRCD = "RS"
              ADD LP-TRAMT TO SER-REF
           ELSE
           IF LP-TRCD = "RM"
              ADD LP-TRAMT TO MF-REF.

           GO TO NEXT-PRIOR-REFUND.

       EXIT-FIND-PRIOR-REFUNDS.
           PERFORM CLOSE-LP1-FILE.

      *************************************
       MANUAL-CHECKS-DISBURSEMENT SECTION.
      *************************************
           PERFORM OPEN-TRC1-FILE.
           PERFORM OPEN-CK1-FILE.

           MOVE BW-FNAME TO BORROWER-ME.
           MOVE BW-LNAME TO ADD-ON.
           INSPECT BORROWER-ME REPLACING FIRST "  " BY " +".
           INSPECT BORROWER-ME REPLACING FIRST
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
           IF ( IO-FG = 9 )
      * THIS MEANS THERE IS ROAD GARD BUT NOT CHECKS
              IF SUB = 2 AND MOTOR-CLUB NOT = 0
                 GO TO END-MAN-CHECKS-DISBURSEMENT
              ELSE
                 GO TO REMAINING-MAN-DISBURSEMENT.

           IF ( TC-KEY-DATE NOT = LN-LOANDATE )
              GO TO REMAINING-MAN-DISBURSEMENT.

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

           IF ( TC-PAYEE = BORROWER-ME )
              ADD  TC-AMT TO ME-AMT
              ADD TC-AMT TO PROCEED-TOT
              GO TO NEXT-MAN-CHECK.

           MOVE TC-PAYEE TO T4-DESC(SUB).
           MOVE TC-AMT   TO T4-AMT(SUB).
           MOVE TC-AMT TO PROCEED-TOT.

           IF SUB < 3
              ADD 1 TO SUB
           ELSE
              ADD TC-AMT TO PROCEED-TOT
              ADD TC-AMT TO PROC-WRK.

           GO TO NEXT-MAN-CHECK.

       REMAINING-MAN-DISBURSEMENT.
           IF ( PROC-WRK NOT = 0 )
              IF PROC-WRK NOT = T4-AMT(SUB)
                 MOVE "OTHER"      TO T4-DESC(SUB)
                 MOVE PROC-WRK     TO T4-AMT(SUB).

       END-MAN-CHECKS-DISBURSEMENT.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-CK1-FILE.

       EXIT-MAN-CHECKS-DISBURSEMENT.
           EXIT.

      *********************************
       SYSTEM-CHECKS-DISBURSEMENT SECTION.
      *********************************
           MOVE 0 TO PROCEED-TOT.

       NEXT-SYS-CHECK.
           PERFORM READ-CX1-FILE-NEXT.
           IF IO-FG = 9
      * THIS MEANS THERE IS MOTOR CLUB BUT NOT CHECKS
              IF SUB = 2 AND MOTOR-CLUB NOT = 0
                 GO TO EXIT-SYS-NEXT-DISBURSEMENT
              ELSE
                 GO TO REMAINING-SYS-DISBURSEMENT.

           IF CX-REF NOT = LN-ACCTNO
              GO TO NEXT-SYS-CHECK.

           IF CX-BWPROC = "Y"
              ADD CX-AMT TO ME-AMT
              GO TO NEXT-SYS-CHECK.

           MOVE CX-PAYEE TO T4-DESC(SUB).
           MOVE CX-AMT TO T4-AMT(SUB).
           IF SUB < 3
              ADD 1 TO SUB
           ELSE
              ADD CX-AMT TO PROCEED-TOT.

           GO TO NEXT-SYS-CHECK.
       REMAINING-SYS-DISBURSEMENT.
           IF PROCEED-TOT NOT = 0
              MOVE "OTHER " TO T4-DESC(SUB)
              MOVE PROCEED-TOT TO T4-AMT(SUB).
       EXIT-SYS-NEXT-DISBURSEMENT.
           EXIT.

      ***********************************
       GET-FEES SECTION.
      ***********************************
           MOVE 0 TO NON-FILE 95-FEE MOTOR-CLUB OTHER-FEE.
           MOVE 1 TO SUB.
       NEXT-LN-FEE.
           IF SUB > 10
              GO TO EXIT-FIND-FEE.
           IF LN-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-LN-FEE.

           IF LN-FEECODE(SUB) = "90"
              ADD LN-FEEAMT(SUB) TO NON-FILE
           ELSE
           IF LN-FEECODE(SUB) = "95"
              ADD LN-FEEAMT(SUB) TO 95-FEE
           ELSE
           IF LN-FEECODE(SUB) = "93" OR "94"
              ADD LN-FEEAMT(SUB) TO MOTOR-CLUB
           ELSE
              ADD LN-FEEAMT(SUB) TO OTHER-FEE.

           ADD 1 TO SUB.
           GO TO NEXT-LN-FEE.
       EXIT-FIND-FEE.
           EXIT.

      *****************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
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
