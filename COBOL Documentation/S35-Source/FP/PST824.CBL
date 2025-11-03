       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST824.
       DATE-WRITTEN.  09-24-2019.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST824.C
      *
      * DESCRIPTION   :  LARGE LOAN DISCLOSURE STATEMENT
      *
      * STATE         :  MISSOURI    (MO)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2019.0924 PR#1388  ***NEW*** CALLED FROM PRT058
      * AMM 2020.0117 PR#1388B DON'T PRINT AGE IF ZERO
      * AMM 2020.1030 PR#1471 REMOVE DUPLEX CODE
      * BAH 2022.0503 START TRCFILE WITH LOANDATE LIKE A15, NOT TRANSDATE
      * BAH 20220901 HARD CODED START
      * BAH 2024.0104 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0521 CHANGED D02-NUMBER BACK TO 9(6) S35Q-32
      * BAH 2024.0703 PRINT SECOND BENEFICIARY NAME #1664 S35Q-80
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
           "@(#)FP/PST824.CBL S35 05/15/23-14:40:58 >".

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
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
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
       01  BORROWER-AGE    PIC 9(4)     COMP-3 VALUE 0.
       01  COSIGNER-AGE    PIC 9(4)     COMP-3 VALUE 0.
       01  ADD-ON          PIC X(25).
       01  NON-FILE        PIC S9(5)V99 COMP-3 VALUE 0.
       01  AD-D-INS        PIC S9(5)V99 COMP-3 VALUE 0.
       01  UMC-PREM        PIC S9(5)V99 COMP-3 VALUE 0.
       01  OTHER-FEE       PIC S9(5)V99 COMP-3 VALUE 0.
       01  SUB1            PIC S9(6)    COMP-3.
      * NEEDED FOR PHONEIO
       01  DEBTOR-PHONE    PIC 9(10) COMP-3  VALUE 0.
       01  DEBTOR-TABLE    VALUE " ".
           03  DEBTOR-LN   PIC X(40) OCCURS 5.
       01  COMPANY-NAME    PIC X(40).
       01  ME-AMT          PIC S9(5)V99 COMP-3 VALUE 0.
       01  PROC-WRK        PIC S9(7)V99 COMP-3 VALUE 0.
       01  PROCEED-TOT     PIC S9(7)V99 COMP-3 VALUE 0.
       01  CDF-SUB         PIC 9(3)            VALUE ZEROES.
       01  CDF-SUB-MAX     PIC 9(3)            VALUE ZEROES.
       01  CDF-AMT         PIC S9(6)V99 COMP-3  OCCURS 2.
       01  CDF-DESC        PIC X(20)            OCCURS 2.
       01  BORROWER-ME     PIC X(41).
       01  CL-REF          PIC S9(5)V99  COMP-3 VALUE 0.
       01  AH-REF          PIC S9(5)V99  COMP-3 VALUE 0.
       01  PP-REF          PIC S9(5)V99  COMP-3 VALUE 0.
       01  O3-REF          PIC S9(5)V99  COMP-3 VALUE 0.
       01  O2-REF          PIC S9(5)V99  COMP-3 VALUE 0.
       01  O1-REF          PIC S9(5)V99  COMP-3 VALUE 0.
       01  MF-REF          PIC S9(5)V99 COMP-3 VALUE 0.
       01  SER-REF         PIC S9(5)V99 COMP-3 VALUE 0.
       01  INT-REF         PIC S9(5)V99 COMP-3 VALUE 0.

      *************************
      *  INTERMEDIATE PROG    *
      *************************
       01  TEXT1.
           03  FILLER        PIC X(10)  VALUE "BEGINNING ".
           03  T1-1STPYDATE  PIC 99/99/99.
           03  FILLER        PIC X(7)   VALUE " EVERY ".
           03  FILLER        PIC X(7)   VALUE " MONTH,".

       01  TEXT2.
           03  FILLER        PIC X(23)  VALUE
           "WITH FINAL PAYMENT DUE ".
           03  T2-MATURITY   PIC 99/99/99.

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
      *    FMNO    : 91 
      *    STATE   : MISSOURI   (MO)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D01-BW-NAME                  PIC X(40).
           03  FILLER                       PIC X(07).
           03  D01-BR-NAME.
               05  D01-BR-BUF               PIC X OCCURS 40.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D02-CO-SIGNER                PIC X(25).
           03  FILLER                       PIC X(05).
           03  D02-AGE                      PIC ZZ9 BLANK ZERO.
           03  FILLER                       PIC X(09).
           03  D02-LOANDATE                 PIC 99/99/99.
           03  FILLER                       PIC X(06).
           03  D02-NUMBER                   PIC 9(6).
           03  FILLER                       PIC X(16).
           03  D02-PHONE                    PIC 999/999/9999.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(15).
           03  D03-APR                      PIC ZZ9.99.
           03  FILLER                       PIC X(03).
           03  D03-FINCHG                   PIC ZZZ,ZZZ,ZZ9.99-.
           03  FILLER                       PIC X(09).
           03  D03-AMT-FINANCE              PIC ZZZ,ZZZ,ZZ9.99-.
           03  FILLER                       PIC X(07).
           03  D03-TOTAL-PAY                PIC ZZZ,ZZZ,ZZ9.99-.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(08).
           03  D04-NOREGPY                  PIC ZZ9.
           03  FILLER                       PIC X(09).
           03  D04-REGPYAMT                 PIC $$$,$$9.99-.
           03  FILLER                       PIC X(07).
           03  D04-DUE-DESC                 PIC X(48).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D05-BOX1                     PIC X.
           03  FILLER                       PIC X(29).
           03  D05-BOX2                     PIC X.
           03  FILLER                       PIC X(24).
           03  D05-BOX3                     PIC X.
           03  FILLER                       PIC X(19).
           03  D05-BOX4                     PIC X.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(04).
           03  D06-PP-SINGLE                PIC X.
           03  FILLER                       PIC X(08).
           03  D06-PP-DUAL                  PIC X.
           03  FILLER                       PIC X(04).
           03  D06-INSBOX                   PIC X.
           03  FILLER                       PIC X(07).
           03  D06-INSTERM                  PIC ZZ9.
           03  FILLER                       PIC X(04).
           03  D06-INSCOVR                  PIC Z(6).99.
           03  FILLER                       PIC X(05).
           03  D06-INSPREM                  PIC Z(5).99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                   REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(03).
           03  D07-SECOND-BEN             PIC X(32).
           03  FILLER                     PIC X(02).
           03  D07-AHEXP                    PIC 99/99/99.
           03  FILLER                       PIC X(04).
           03  D07-AHPREM                   PIC Z(4).99.
           03  FILLER                       PIC X(06).
           03  D07-FIRST-AGE                PIC ZZZ BLANK ZERO.
           03  FILLER                       PIC X(08).
           03  D07-JOINT-AGE                PIC ZZZ BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-08                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(16).
 
      *-----------------------------------------------------------------
       01  DETAIL-LINE-09                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(31).
           03  D09-INSCOVR3                 PIC Z(6).99.
           03  FILLER                       PIC X(10).
           03  D09-INSTERM3                 PIC ZZ9.
           03  FILLER                       PIC X(17).
           03  D09-INSPREM3                 PIC Z(5).99.
 
      *-----------------------------------------------------------------
       01  DETAIL-LINE-10                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(17).
           03  D10-AMT1                     PIC ZZZZZ.99.
           03  FILLER                       PIC X(09).
           03  D10-AMT2                     PIC ZZZ,ZZZ.99.
           03  FILLER                       PIC X(13).
           03  D10-AMT3                     PIC ZZ,ZZZ.99.
           03  FILLER                       PIC X(04).
           03  D10-CXPAYEE                  PIC X(23).
 
      *-----------------------------------------------------------------
       01  DETAIL-LINE-11                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(35).
           03  D11-AMT4                     PIC ZZ,ZZZ.99.
           03  FILLER                       PIC X(24).
           03  D11-AMT5                     PIC ZZZ,ZZZ.99.
 
      *-----------------------------------------------------------------
       01  DETAIL-LINE-12                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(73).
           03  D12-AMT6                     PIC ZZZ,ZZZ.99.
           03  FILLER                       REDEFINES D12-AMT6.
               05  FILLER                   PIC X.
               05  D12-EFFRATE              PIC ZZ9.99.

       COPY "LIBFP/PST_COPYW.CPY".
       COPY "LIBFP/PST_INFOW.CPY".
       COPY "LIBFP/PST_FORMW.CPY".
       COPY "LIBGB/PHONE.CPY".
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
           PERFORM CLOSE-LSM1-FILE.
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

           MOVE 03     TO PST-WS-FORM-PAGE-MAX.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM SETUP-DEBTORS.
           IF BW-REC = " "
              MOVE "BORROWER RECORD MISSING" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           PERFORM LOAN-CALCULATIONS.

           PERFORM GET-FEES.

           MOVE 1           TO CDF-SUB.
           MOVE 2           TO CDF-SUB-MAX.
           MOVE 0           TO CDF-AMT(1) CDF-AMT(2).

           MOVE SPACES      TO CDF-DESC(1) CDF-DESC(2).
           MOVE LN-PROCASH  TO ME-AMT.
           MOVE 0           TO PROCEED-TOT PROC-WRK.

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

      * CHANGE FONT SIZE TO 12 CPI 
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           MOVE DEBTOR-LN(1)           TO D01-BW-NAME.
           MOVE GB-COMPANY-NAME        TO COMPANY-NAME.
           CALL "C$JUSTIFY" USING COMPANY-NAME, "L".
           MOVE COMPANY-NAME           TO D01-BR-NAME.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DEBTOR-LN(2)           TO D01-BW-NAME.
           MOVE BR-ADR1                TO D01-BR-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DEBTOR-LN(3)           TO D01-BW-NAME.
           MOVE BR-CITY                TO C-S-Z.
           MOVE BR-STATE               TO STATE.
           MOVE BR-ZIP                 TO ZIP. 
           PERFORM CAT-CSZ.
           IF BR-ADR2 NOT = " "
              MOVE BR-ADR2 TO D01-BR-NAME
           ELSE
              MOVE C-S-Z TO D01-BR-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DEBTOR-LN(4)           TO D01-BW-NAME.
           MOVE BR-PHONE               TO TEL-BUF.
           PERFORM SEND-PHONE.
           IF BR-ADR2 NOT = " "
              MOVE C-S-Z     TO D01-BR-NAME
           ELSE
              MOVE TEL-EDIT  TO D01-BR-NAME.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = " "
              MOVE TEL-EDIT            TO D01-BR-NAME.
           MOVE BORROWER-AGE           TO D02-AGE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DEBTOR-LN(5)           TO D02-CO-SIGNER.
           MOVE COSIGNER-AGE           TO D02-AGE.
           MOVE LN-LOANDATE            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D02-LOANDATE.
           MOVE LN-ACCTNO              TO D02-NUMBER.
           IF DEBTOR-PHONE NOT = 0
              MOVE DEBTOR-PHONE TO D02-PHONE
              INSPECT D02-PHONE REPLACING ALL "/" BY "-".
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-FINCHG              TO D03-FINCHG.
           MOVE DISCLOSED-FINAMT       TO D03-AMT-FINANCE.
           MOVE TOTAL-REPAY            TO D03-TOTAL-PAY.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           ADD 0 LN-APRATE GIVING D03-APR ROUNDED.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-1STPYAMT NOT = LN-REGPYAMT
              MOVE 1            TO D04-NOREGPY
              MOVE LN-1STPYAMT  TO D04-REGPYAMT
           ELSE
              MOVE LN-REGPYAMT  TO D04-REGPYAMT
              IF LN-LASTPYAMT NOT = LN-REGPYAMT
                 COMPUTE D04-NOREGPY = LN-ORGTERM - 1
              ELSE
                 MOVE LN-ORGTERM TO D04-NOREGPY.
           MOVE LN-1STPYDATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO T1-1STPYDATE.
           MOVE TEXT1            TO D04-DUE-DESC.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-1STPYAMT NOT = LN-REGPYAMT
              COMPUTE D04-NOREGPY = LN-ORGTERM - 1
              MOVE LN-REGPYAMT      TO D04-REGPYAMT
           ELSE
              IF LN-LASTPYAMT NOT = LN-REGPYAMT
                 MOVE 1             TO D04-NOREGPY
                 MOVE LN-LASTPYAMT  TO D04-REGPYAMT.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO T2-MATURITY.
           MOVE TEXT2               TO D04-DUE-DESC.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * MOTOR VEHICLE
           IF LN-COLLCD = "AP" OR "AU" OR "AS"
                 MOVE "X" TO D05-BOX2.
      * UNSECURED
           IF LN-COLLCD = "UL" 
              MOVE "X" TO D05-BOX4.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PERSONAL PROPERTY
           IF LN-COLLCD = "AP" OR "PP" OR "SC" OR "AS"
              MOVE "X" TO D05-BOX2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DECREASING TERM LIFE SINGLE
           IF LN-INSPREM(1) NOT = 0
              MOVE 1 TO ITRM-SUB
              MOVE TRANS-DATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF LN-INSTYPES-DL
                 IF LN-INSURED NOT = "J" 
                 MOVE "X"            TO D06-INSBOX
                 MOVE ITRM-INSTERM   TO D06-INSTERM
                 MOVE LN-INSCOVR(1)  TO D06-INSCOVR
                 MOVE LN-INSPREM(1)  TO D06-INSPREM.
           MOVE 10 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DECREASING LIFE JOINT
           IF LN-INSTYPES-DL
              IF LN-INSURED = "J" 
                 MOVE "X"            TO D06-INSBOX
                 MOVE ITRM-INSTERM   TO D06-INSTERM
                 MOVE LN-INSCOVR(1)  TO D06-INSCOVR
                 MOVE LN-INSPREM(1)  TO D06-INSPREM.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LEVEL LIFE SINGLE
           IF LN-INSTYPES-LL
              IF LN-INSURED NOT = "J"
                 MOVE "X"            TO D06-INSBOX
                 MOVE ITRM-INSTERM   TO D06-INSTERM
                 MOVE LN-INSCOVR(1)  TO D06-INSCOVR
                 MOVE LN-INSPREM(1)  TO D06-INSPREM.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LEVEL LIFE JOINT
           IF LN-INSTYPES-LL
              IF LN-INSURED = "J"
                 MOVE "X"            TO D06-INSBOX
                 MOVE ITRM-INSTERM   TO D06-INSTERM
                 MOVE LN-INSCOVR(1)  TO D06-INSCOVR
                 MOVE LN-INSPREM(1)  TO D06-INSPREM.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DISABILITY INSURANCE SINGLE
           IF LN-INSPREM(2) NOT = 0
              MOVE 2 TO ITRM-SUB
              MOVE TRANS-DATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF SP-INSAH-JOINT-FG NOT = "Y"
                 MOVE "X"            TO D06-INSBOX
                 MOVE ITRM-INSTERM   TO D06-INSTERM
                 MOVE LN-INSPREM(2)  TO D06-INSPREM
                 MOVE LN-REGPYAMT    TO D06-INSCOVR
                 IF LN-REGPYAMT > SP-INSMAXBENIF(5)
                    MOVE SP-INSMAXBENIF(5) TO D06-INSCOVR.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DISABILITY INSURANCE JOINT
           IF LN-INSPREM(2) NOT = 0
              IF SP-INSAH-JOINT-FG = "Y"
                 MOVE "X"            TO D06-INSBOX
                 MOVE ITRM-INSTERM   TO D06-INSTERM
                 MOVE LN-INSPREM(2)  TO D06-INSPREM
                 MOVE LN-REGPYAMT    TO D06-INSCOVR
                 IF LN-REGPYAMT > SP-INSMAXBENIF(5)
                    MOVE SP-INSMAXBENIF(5) TO D06-INSCOVR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SECOND BENEFICIARY 
           IF LN-INSPREM(1) NOT = 0
              PERFORM GET-SECOND-BENEFICIARY
              MOVE LSM-SECOND-BEN-NAME TO D07-SECOND-BEN.
      * EXPIRE DATE  / MAX BENEF / FIRST INSURED AGE / JOINT INSURED AGE
           MOVE MDTE-DATE           TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY         TO D07-AHEXP
           MOVE LN-INSPREM(1)       TO D07-AHPREM.
           MOVE BORROWER-AGE        TO D07-FIRST-AGE
           MOVE COSIGNER-AGE        TO D07-JOINT-AGE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * IUI
           IF LN-INSPREM(4) NOT = 0
              MOVE 4 TO ITRM-SUB
              MOVE TRANS-DATE     TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              MOVE "X"            TO D06-INSBOX
              MOVE ITRM-INSTERM   TO D06-INSTERM
              MOVE LN-INSPREM(4)  TO D06-INSPREM
              MOVE LN-INSCOVR(4)  TO D06-INSCOVR.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PROPERTY INSURANCE 
           IF LN-INSPREM(3) NOT = 0
              MOVE 3 TO ITRM-SUB
              MOVE TRANS-DATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF LN-INSTYPES-PP2
                 MOVE "X" TO D06-PP-DUAL
              ELSE
                 MOVE "X" TO D06-PP-SINGLE
              END-IF
              MOVE ITRM-INSTERM TO D09-INSTERM3
              MOVE LN-INSPREM(3) TO D09-INSPREM3
              MOVE LN-INSCOVR(3) TO D09-INSCOVR3.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SINGLE INTEREST AUTO INSURANCE 
           MOVE 6 TO ITRM-SUB.
           IF LN-INSPREM(ITRM-SUB) NOT = 0
              MOVE TRANS-DATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              MOVE LN-INSCOVR(ITRM-SUB) TO D09-INSCOVR3
              MOVE ITRM-INSTERM TO D09-INSTERM3
              MOVE LN-INSPREM(ITRM-SUB) TO D09-INSPREM3.
           MOVE 03 TO LN-FEED.
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

      * CHANGE FONT SIZE TO 12 CPI 
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

      * AMOUNT FINANCED
           IF DISCLOSED-FINAMT NOT = 0
              MOVE DISCLOSED-FINAMT    TO D11-AMT4
           ELSE
              MOVE ZEROES              TO D11-AMT4.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      * 1. CREDIT LIFE 
           IF LN-INSPREM(1) NOT = 0
              MOVE LN-INSPREM(1)       TO D10-AMT2
           ELSE
              MOVE ZEROES              TO D10-AMT2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 2. CREDIT DISABILITY
           IF LN-INSPREM(2) NOT = 0
              MOVE LN-INSPREM(2)       TO D10-AMT2
           ELSE
              MOVE ZEROES              TO D10-AMT2.
      * TO
           MOVE CDF-AMT(1)             TO D10-AMT3.
           MOVE CDF-DESC(1)            TO D10-CXPAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      * 3. IUI PREM
           IF LN-INSPREM(4) NOT = 0
              MOVE LN-INSPREM(4)       TO D10-AMT2
           ELSE
              MOVE ZEROES              TO D10-AMT2.
      * TO
           MOVE CDF-AMT(2)             TO D10-AMT3.
           MOVE CDF-DESC(2)            TO D10-CXPAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 4. PROPERTY
           IF LN-INSPREM(3) NOT = 0
              MOVE LN-INSPREM(3)       TO D10-AMT2
           ELSE
              MOVE ZEROES              TO D10-AMT2.
      * TO YOU
           MOVE ME-AMT                 TO D10-AMT3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 5. S.I. AUTO 
           IF LN-INSPREM(6) NOT = 0
              MOVE LN-INSPREM(6)       TO D10-AMT2
           ELSE
              MOVE ZEROES              TO D10-AMT2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 6. MOTOR CLUB
           IF UMC-PREM NOT = 0
              MOVE UMC-PREM            TO D10-AMT2
           ELSE
              MOVE ZEROES              TO D10-AMT2.
      * 11. FINANCE CHARGE
           MOVE LN-FINCHG              TO D12-AMT6.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 7. NON-FILING
           IF NON-FILE NOT = 0
              MOVE NON-FILE            TO D10-AMT2
           ELSE
              MOVE ZEROES              TO D10-AMT2.
      * INTEREST CHARGE
           MOVE LN-INTCHG              TO D11-AMT5.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 8. TO PUBLIC OFFICIALS 
           IF OTHER-FEE NOT = 0
              MOVE OTHER-FEE           TO D10-AMT2
           ELSE
              MOVE ZEROES              TO D10-AMT2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 9. AMOUNT PAID ON PRIOR
           IF LN-PRLNNO NOT = 0
              PERFORM FIND-PRIOR-REFUNDS.
           COMPUTE D10-AMT2 = LN-PRBAL + LN-PRINTDUE
                            - LN-PRFINCHGRB - LN-PRINSRB.
           IF LN-PRLNNO = 0
              MOVE ZEROES TO D10-AMT2.
      * SERVICE CHARGE
           MOVE LN-SERCHG              TO D11-AMT5.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * REFUNDS ON PRIOR ACCOUNT 
           MOVE CL-REF                 TO D10-AMT1.
           MOVE AH-REF                 TO D11-AMT4.
      * 12. TOTAL OF PAYMENTS
           MOVE TOTAL-REPAY            TO D12-AMT6.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PROPERTY 
           MOVE PP-REF                 TO D10-AMT1.
      * AUTO
           MOVE O3-REF                 TO D11-AMT4.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * OTHER INS
           MOVE O1-REF                 TO D10-AMT1.
      * EFFECTIVE ANNUAL INTEREST RATE
           COMPUTE D12-EFFRATE ROUNDED = LN-SMPRATE *1.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INTEREST AND SERVICE REFUND
           MOVE INT-REF                TO D10-AMT1.
      * AUTO
           MOVE SER-REF                TO D11-AMT4.
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

      ********************************
       SETUP-DEBTORS SECTION.
      ********************************
           MOVE SPACES TO DEBTOR-TABLE.
           PERFORM OPEN-BW1-FILE.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM GET-BW1.
           IF IO-FG = 9
              GO TO END-SETUP-DEBTORS.

           MOVE C-S-Z TO DEBTOR-LN(1).

           MOVE BW-ADR1 TO DEBTOR-LN(2).
           MOVE BW-ADR2 TO DEBTOR-LN(3).
           MOVE BW-CITY TO C-S-Z.
           MOVE BW-STATE TO STATE.
           MOVE BW-ZIP TO ZIP.
           PERFORM CAT-CSZ.
           MOVE C-S-Z TO DEBTOR-LN(4).
           MOVE BW-PHONE TO DEBTOR-PHONE.

           PERFORM GET-AGE.
           MOVE BAGE-AGE TO BORROWER-AGE.

      * SETUP CX-PAYEE TO TEST FOR "ME"
           MOVE BW-FNAME TO BORROWER-ME.
           MOVE BW-LNAME TO ADD-ON.
           INSPECT BORROWER-ME REPLACING FIRST "  " BY " +".
           INSPECT BORROWER-ME REPLACING FIRST
              "+                        " BY ADD-ON.

           MOVE LN-SSNO(2) TO BW-SSNO.
           IF BW-SSNO = 0
              GO TO SHIFT-BW-LINES-UP.
           PERFORM GET-BW1.
           MOVE C-S-Z TO DEBTOR-LN(5).

           PERFORM GET-AGE.
           MOVE BAGE-AGE TO COSIGNER-AGE.

       SHIFT-BW-LINES-UP.
           IF DEBTOR-LN(3) = " "
              MOVE 4 TO SUB1
              MOVE 3 TO SUB
              PERFORM SCROLL-BW-UP.
           IF DEBTOR-LN(2) = " "
              MOVE 3 TO SUB1
              MOVE 2 TO SUB
              PERFORM SCROLL-BW-UP.
       END-SETUP-DEBTORS.
      * PUT MAKER BACK IN THE BW-REC BUFFER
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM GET-BW1.
           PERFORM CLOSE-BW1-FILE.

      *********************************
       SCROLL-BW-UP SECTION.
      *********************************
       NEXT-BW-UP.
           MOVE DEBTOR-LN(SUB1) TO DEBTOR-LN(SUB).
           ADD 1 TO SUB SUB1.
           IF SUB < 4
              GO TO NEXT-BW-UP.
           MOVE " " TO DEBTOR-LN(4).
 
      ********************************
       FIND-PRIOR-REFUNDS SECTION.
      ********************************
           MOVE 0 TO O1-REF CL-REF AH-REF PP-REF O3-REF MF-REF
                     SER-REF INT-REF O2-REF.

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
      * THIS MEANS THERE IS ROAD GARD BUT NOT CHECKS
              IF CDF-SUB NOT > CDF-SUB-MAX
                 GO TO END-MAN-CHECKS-DISBURSEMENT
              ELSE
                 GO TO REMAINING-MAN-DISBURSEMENT.

      * STARTS WITH BRNO AND DATE, IF DATE DOESNT MATCH, YOU DIDNT FIND ANY
           IF TC-KEY-DATE NOT = LN-LOANDATE
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

           IF ( TC-PAYEE = C-S-Z )
              ADD TC-AMT TO ME-AMT
              ADD TC-AMT TO PROCEED-TOT
              GO TO NEXT-MAN-CHECK.

           MOVE TC-PAYEE TO CDF-DESC(CDF-SUB).
           MOVE TC-AMT   TO CDF-AMT(CDF-SUB).
           MOVE TC-AMT   TO PROCEED-TOT.

           IF CDF-SUB < CDF-SUB-MAX 
              ADD 1      TO CDF-SUB
           ELSE
              ADD TC-AMT TO PROCEED-TOT
              ADD TC-AMT TO PROC-WRK.

           GO TO NEXT-MAN-CHECK.

       REMAINING-MAN-DISBURSEMENT.
           IF ( PROC-WRK NOT = ZEROES )
              IF PROC-WRK NOT = CDF-AMT(CDF-SUB)
                 MOVE "OTHER"      TO CDF-DESC(CDF-SUB)
                 MOVE PROC-WRK     TO CDF-AMT(CDF-SUB).

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
              IF CDF-SUB NOT > CDF-SUB-MAX
                 GO TO EXIT-SYS-NEXT-DISBURSEMENT
              ELSE
                 GO TO REMAINING-SYS-DISBURSEMENT.

           IF CX-REF NOT = LN-ACCTNO
              GO TO NEXT-SYS-CHECK.

           IF CX-BWPROC = "Y"
              ADD CX-AMT TO ME-AMT
              GO TO NEXT-SYS-CHECK.

      *- - - - - - - - - - - - - - - - - - - - - - CHECK TO OTHERS
           MOVE CX-PAYEE TO CDF-DESC(CDF-SUB).
           MOVE CX-AMT   TO CDF-AMT(CDF-SUB).

           IF CDF-SUB < CDF-SUB-MAX
              ADD 1 TO CDF-SUB
           ELSE
              ADD CX-AMT TO PROCEED-TOT.

           GO TO NEXT-SYS-CHECK.

       REMAINING-SYS-DISBURSEMENT.
           IF PROCEED-TOT NOT = ZEROES
              IF PROCEED-TOT NOT = CDF-AMT(CDF-SUB)
                 MOVE "OTHER "    TO CDF-DESC(CDF-SUB)
                 MOVE PROCEED-TOT TO CDF-AMT(CDF-SUB).
       EXIT-SYS-NEXT-DISBURSEMENT.
           EXIT.

      *******************************
       GET-FEES SECTION.
      *******************************
           MOVE 0 TO NON-FILE AD-D-INS UMC-PREM.
           MOVE 1 TO SUB.
       NEXT-LN-FEE.
           IF SUB > 10
              GO TO EXIT-FIND-FEE.
           IF LN-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-LN-FEE.

           IF LN-FEECODE(SUB) = "91" OR "92" OR "95" OR
                                "96" OR "97" OR "98" OR "99"
              ADD LN-FEEAMT(SUB) TO OTHER-FEE
           ELSE
              IF LN-FEECODE(SUB) = "93" OR "94"
                 ADD LN-FEEAMT(SUB) TO UMC-PREM
              ELSE
                 IF LN-FEECODE(SUB) = "90"
                    ADD LN-FEEAMT(SUB) TO NON-FILE.

           ADD 1 TO SUB.
           GO TO NEXT-LN-FEE.
       EXIT-FIND-FEE.
           EXIT.
 
      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/PHONEIO.CPY".
       GET-SECOND-BENEFICIARY.
           MOVE LN-OWNBR     TO LSM-BRNO.
           MOVE LN-ACCTNO    TO LSM-ACCTNO.
           MOVE 1            TO LSM-SEQNO.
           PERFORM OPEN-LSM1-FILE.
           PERFORM READ-LSM1-FILE.
           PERFORM CLOSE-LSM1-FILE.
           IF IO-FG = 9
              MOVE SPACES TO LSM-SECOND-BEN-NAME
                             LSM-SECOND-BEN-ADR
                             LSM-SECOND-BEN-CITY
                             LSM-SECOND-BEN-STATE
                             LSM-SECOND-BEN-ZIP.
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME.
           PERFORM APPEND-BW-NAMES.
       APPEND-BW-NAMES.
           MOVE BW-FNAME TO C-S-Z.
           MOVE BW-LNAME TO ADD-ON.
           PERFORM ADD-IT.
       ADD-IT.
           INSPECT C-S-Z REPLACING FIRST "  " BY " +".
           INSPECT C-S-Z REPLACING FIRST
              "+                        " BY ADD-ON.
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.
       GET-AGE.
           MOVE BW-BDATE   TO BAGE-DATE.
           MOVE BW-REFDATE TO BAGE-REFDATE.
           MOVE TRANS-DATE TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.

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
       COPY "LIBLP/LPLSMGS_SQL.CPY".
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
       COPY "LIBLP/LPLSM1RN.CPY".
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
