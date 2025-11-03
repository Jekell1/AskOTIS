       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST821.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-23-2018.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST821.C
      *
      * DESCRIPTION   :  DISCLOSURE, PROMISSORY NOTE
      *
      * STATE         :  KENTUCKY    (KY)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2018.0523 PR#1314 CALLED FROM LONP4V
      * AMM 2018.0606 PR#1314A ADDED TWO MORE PAYEE LINES ON PAGE 2
      * AMM 2018.0614 PR#1314B CORRECTED PP FACTOR; CHANGE (4) TO (3)
      * AMM 2018.1129 PR#1351 CORRECTED TYPO IN CRYSTAL
      * AMM 2019.0110 PR#1358 ADD BR-PHONE TO BRANCH INFO
      * AMM 2019.0611 PR#1377 CHANGED LEGAL JARGON IN CRYSTAL
      * AMM 2019.1118 PR#1400 ADDED POST MATURITY INT JARGON AND APRATE ON PG3 
      * AMM 2020.0616 PR#1443 ADDED PRINT OF NETSPEND DEBIT CARD AS PROCEEDS
      *                       TO ME
      * BAH 2022.0503 START TRCFILE WITH LOANDATE LIKE A15, NOT TRANSDATE
      * BAH 2022.1205 CHANGED 5. S.I. AUTO INS. PREM. TO UNITED MOTOR CLUB,
      *               FEE CODES 93/94 FROM BRANCH FILE.
      *               CHANGED 6. OTHER INS. PREM TO UMC SALES TAX, FEE
      *               CODE "TM"
      *               CHANGED REFUND ON PRIOR ACCOUNT UNEMP. INS. FROM
      *               A COBOL FILLED IN PROMPT TO BE IN CRYSTAL
      *               REMOVED UNITED MOTOR CLUB FROM THE CHECK DISBURSEMENT
      *               CHANGED ALL REV DATES TO 12/22 IN CRYSTAL #1582
      * BAH 2023.0901 CHANGE CRYSTAL ONLY; DEDUCT PRE-CHARGE FOR LOAN PROCESSING
      *               FEE FROM 50 TO $75, #1621
      * BAH 2023.1218 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1635
      * BAH 2024.0410 PRINT SECOND BENEFICIARY NAME #1653
      * BAH 2024.0521 CHANGED D02-ACCTNO BACK TO Z(5)9 S35Q-32
      * BAH 2024.0822 ONLY PRINT JOINT AGE IF JOINT INSURED #1672 S35Q-117
      * BAH 2024.1217 CHECK MOTOR VEHICLE IF COLLCD "AS" AND CHECK PERSONAL
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
           "@(#)FP/PST821.CBL S35 12/17/21-14:19:56 >".

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
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
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

       01  NON-FILE                    PIC S9(7)V99 COMP-3 VALUE 0.
       01  ROAD-GARD                   PIC S9(7)V99 COMP-3 VALUE 0.
       01  ROAD-TYPE                   PIC X VALUE SPACES.
       01  OTHER-FEE                   PIC S9(7)V99 COMP-3 VALUE 0.
       01  TC-FEE                      PIC S9(7)V99 COMP-3 VALUE 0.
       01  TA-FEE                      PIC S9(7)V99 COMP-3 VALUE 0.
       01  TP-FEE                      PIC S9(7)V99 COMP-3 VALUE 0.
       01  T1-FEE                      PIC S9(7)V99 COMP-3 VALUE 0.
       01  T2-FEE                      PIC S9(7)V99 COMP-3 VALUE 0.
       01  T3-FEE                      PIC S9(7)V99 COMP-3 VALUE 0.
       01  TM-FEE                      PIC S9(7)V99 COMP-3 VALUE 0.
       01  WK-BENEFIT                  PIC S9(7)V99 COMP-3 VALUE 0.

       01  CL-REF                      PIC S9(5)V99  COMP-3 VALUE 0.
       01  AH-REF                      PIC S9(5)V99  COMP-3 VALUE 0.
       01  PP-REF                      PIC S9(5)V99  COMP-3 VALUE 0.
       01  O3-REF                      PIC S9(5)V99  COMP-3 VALUE 0.
       01  O2-REF                      PIC S9(5)V99  COMP-3 VALUE 0.
       01  O1-REF                      PIC S9(5)V99  COMP-3 VALUE 0.
       01  MF-REF                      PIC S9(5)V99 COMP-3 VALUE 0.
       01  SER-REF                     PIC S9(5)V99 COMP-3 VALUE 0.
       01  INT-REF                     PIC S9(5)V99 COMP-3 VALUE 0.
       01  PROC-WRK                    PIC S9(7)V99 COMP-3 VALUE 0.
       01  PROCEED-BAL                 PIC S9(7)V99 COMP-3 VALUE 0.
       01  PROCEED-TOT                 PIC S9(7)V99 COMP-3 VALUE 0.
       01  ME-AMT                      PIC S9(5)V99 COMP-3 VALUE 0.
       01  BORROWER-ME                 PIC X(41).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  TEXT1.
           03  T1-TABLE OCCURS 6 TIMES.
              05  T1-AMT      PIC S9(6)V99 COMP-3.
              05  T1-DESC.
                  07  T1-ASTERISK PIC X.
                  07  FILLER      PIC X.
                  07  T1-ROADG    PIC X(18).

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
      *    STATE   : KENTUCKY    (KY)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D01-MAKER-INFO               PIC X(35).
           03  FILLER                       PIC X(01).
           03  D01-BRANCH-INFO              PIC X(35).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(38).
           03  D02-LOANDATE                 PIC 99/99/99.
           03  FILLER                       PIC X(05).
           03  D02-CLASS                    PIC 99.
           03  D02-HYPHEN                   PIC X.
           03  D02-ACCTNO                   PIC ZZZZZ9.
           03  FILLER                       PIC X(06).
           03  D02-PHONE                    PIC 999/999/9999.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(12).
           03  D03-APRATE                   PIC ZZ9.99.
           03  FILLER                       PIC X(07).
           03  D03-FINCHG                   PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(08).
           03  D03-FINAMT                   PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(09).
           03  D03-TOTAL-PAY                PIC ZZZ,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(07).
           03  D04-TERM                     PIC ZZ9.
           03  FILLER                       PIC X(09).
           03  D04-REGPYAMT                 PIC ZZ,ZZ9.99.
           03  FILLER                       PIC X(05).
           03  D04-PAY-TEXT                 PIC X(35).
           03  FILLER                       REDEFINES D04-PAY-TEXT.
               05  D04-BEGIN                PIC X(09).
               05  FILLER                   PIC X(01).
               05  D04-BEGIN-DATE           PIC 99/99/99.
               05  FILLER                   PIC X(05).
               05  D04-MDTE-DATE            PIC 99/99/99.
           03  FILLER                       REDEFINES D04-PAY-TEXT.
               05  FILLER                   PIC X(19).
               05  D04-EVERY-MO             PIC X(16).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(26).
           03  D05-BOX-2                    PIC X(01).
           03  FILLER                       PIC X(37).
           03  D05-BOX-4                    PIC X(01).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(14).
           03  D06-INS-BOX                  PIC X(01).
           03  FILLER                       PIC X(05).
           03  D06-INS-TERM                 PIC ZZ9.
           03  FILLER                       PIC X(02).
           03  D06-COVERAGE                 PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(01).
           03  D06-PREMIUM                  PIC ZZ,ZZ9.99.
           03  D06-INSTAX                   PIC ZZZZ9.99.
           03  FILLER                       PIC X(01).
           03  D06-TOTPREM                  PIC ZZZZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D07-SECBEN                   PIC X(24).
           03  FILLER                       PIC X.
           03  D07-AHEXP                    PIC 99/99/99.
           03  FILLER                       PIC X(01).
           03  D07-AHPREM                   PIC Z,ZZ9.99 BLANK ZERO.
           03  FILLER                       PIC X(03).
           03  D07-FIRST-AGE                PIC ZZZ.
           03  FILLER                       PIC X(06).
           03  D07-JOINT-AGE                PIC ZZZ.
           03  FILLER                       PIC X(05).
           03  D07-MUNIC-CITY               PIC X(03).
           03  FILLER                       PIC X(02).
           03  D07-MUNIC-SLASH              PIC X.
           03  FILLER                       PIC X(02).
           03  D07-MUNIC-COUNTY             PIC X(03).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-08                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(17).
           03  D08-IUI-BENEFIT              PIC Z(4).99 BLANK ZERO.
           03  FILLER                       PIC X(03).
           03  D08-IUI-TERM                 PIC ZZZ.
           03  FILLER                       PIC X(03).
           03  D08-IUI-TOTAL                PIC Z(6).99 BLANK ZERO.
           03  FILLER                       PIC X(05).
           03  D08-IUI-FACTOR               PIC Z(5).99 BLANK ZERO.
           03  FILLER                       PIC X(05).
           03  D08-IUI-SURCHARGE            PIC Z(4).99 BLANK ZERO.
           03  FILLER                       PIC X.
           03  D08-IUI-TOTPREM              PIC Z(6).99 BLANK ZERO.
 
      *-----------------------------------------------------------------
       01  DETAIL-LINE-09                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(04).
           03  D09-SINGLE                   PIC X.
           03  FILLER                       PIC X(6).
           03  D09-DUAL                     PIC X.
           03  FILLER                       PIC X(12).
           03  D09-INSCOVR3                 PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(07).
           03  D09-INSTERM3                 PIC ZZ9.
           03  FILLER                       PIC X(11).
 
      *-----------------------------------------------------------------
       01  DETAIL-LINE-10                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(30).
           03  D10-PG2-FINAMT               PIC ZZZZZZ.99.
           03  FILLER                       PIC X(04).
           03  D10-RETAIN-TEXT              PIC X(25).   
           03  FILLER                       REDEFINES D10-RETAIN-TEXT.
               05  FILLER                   PIC X(10).
               05  D10-EFFRATE              PIC ZZ.99.
 
      *-----------------------------------------------------------------
       01  DETAIL-LINE-11                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D11-UNEMP2                   PIC X(11).
           03  FILLER                       PIC X(30).
           03  D11-AMT6                     PIC ZZZ,ZZZ.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-12                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(12).
           03  D12-AMT1                     PIC ZZ,ZZZ.99.
           03  FILLER                       PIC X(06).
           03  D12-AMT2                     PIC ZZZ,ZZZ.99.
           03  D12-ASTERISK                 PIC X.
           03  FILLER                       PIC X(07).
           03  D12-AMT3                     PIC ZZ,ZZZ.99 BLANK ZERO.
           03  FILLER                       PIC X(03).
           03  D12-CXPAYEE                  PIC X(21).
           03  FILLER                       REDEFINES D12-CXPAYEE.
               05  FILLER                   PIC X(01).
               05  D12-CXPAYEE2             PIC X(20).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-13                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(29).
           03  D13-AMT4                     PIC ZZZZZ.99.
           03  FILLER                       PIC X(19).
           03  D13-AMT5                     PIC ZZZ,ZZZ.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-14                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(57).
           03  D14-PG3-RATE                 PIC ZZ9.99.

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

           MOVE 04     TO PST-WS-FORM-PAGE-MAX.
           PERFORM PST-FR-FORM-INITIALIZATION.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ LOAD ACCOUNT INFO. }
           PERFORM PST-INFO-MODULE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ CHECKS }
           PERFORM GET-FEES.

           MOVE LN-PROCASH TO ME-AMT PROCEED-BAL.
           MOVE 0 TO PROC-WRK.

           MOVE 1 TO SUB.
           MOVE " " TO T1-DESC(1) T1-DESC(2) T1-DESC(3) T1-DESC(4).
           MOVE 0 TO T1-AMT(1) T1-AMT(2) T1-AMT(3) T1-AMT(4).

      * CHECK FOR NETSPEND DEBIT CARD PROCEEDS
           IF BR-USE-DEBIT-CARDS = "Y"
              PERFORM CHECK-FOR-DEBIT-CARD.

           PERFORM MANUAL-CHECKS-DISBURSEMENT.
           IF ( PROCEED-TOT = 0 )
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

           MOVE MAKER-NAME-FIRST-LAST        TO D01-MAKER-INFO.
           MOVE BR-NAME                      TO D01-BRANCH-INFO.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-ADDRESS                TO D01-MAKER-INFO.
           MOVE BR-ADR1                      TO D01-BRANCH-INFO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-CSZ                    TO D01-MAKER-INFO.
           STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO D01-BRANCH-INFO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-PHONE                     TO PST-PHONE-UNEDIT.
           PERFORM PST-CONVERT-PHONE-TO-EDIT.
           MOVE PST-PHONE-EDIT               TO D01-BRANCH-INFO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( JOINT-SSNO NOT = ZEROES )
              MOVE JOINT-NAME-FIRST-LAST     TO D01-MAKER-INFO
           ELSE
           IF ( ENDR1-SSNO NOT = ZEROES )
              MOVE ENDR1-NAME-FIRST-LAST     TO D01-MAKER-INFO.
           MOVE LN-LOANDATE                  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                  TO D02-LOANDATE.
           MOVE LN-CLASS                     TO D02-CLASS.
           MOVE "-"                          TO D02-HYPHEN.
           MOVE LN-ACCTNO                    TO D02-ACCTNO.
           MOVE MAKER-HOMEPHONE              TO D02-PHONE.
           INSPECT D02-PHONE REPLACING ALL "/" BY "-".
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-FINCHG                    TO D03-FINCHG.
           MOVE DISCLOSED-FINAMT             TO D03-FINAMT.
           MOVE TOTAL-REPAY                  TO D03-TOTAL-PAY.
           MOVE 05 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           ADD 0 LN-APRATE GIVING D03-APRATE ROUNDED.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-1STPYAMT NOT = LN-REGPYAMT
              MOVE 1           TO D04-TERM
              MOVE LN-1STPYAMT TO D04-REGPYAMT
           ELSE
              MOVE LN-REGPYAMT TO D04-REGPYAMT
              IF LN-LASTPYAMT NOT = LN-REGPYAMT
                 COMPUTE D04-TERM = LN-ORGTERM - 1
              ELSE
                 MOVE LN-ORGTERM TO D04-TERM.
           MOVE "BEGINNING"                  TO D04-BEGIN.
           MOVE LN-1STPYDATE                 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                  TO D04-BEGIN-DATE.
           IF LN-ORGTERM = 1
              MOVE "EVERY MONTH"             TO D04-EVERY-MO
           ELSE
              MOVE "EVERY 1 MONTHS"          TO D04-EVERY-MO.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-1STPYAMT NOT = LN-REGPYAMT
              COMPUTE D04-TERM = LN-ORGTERM - 1
              MOVE LN-REGPYAMT TO D04-REGPYAMT
           ELSE
              IF LN-LASTPYAMT NOT = LN-REGPYAMT
                 MOVE 1 TO D04-TERM
                 MOVE LN-LASTPYAMT TO D04-REGPYAMT.
           MOVE "WITH FINAL PAYMENT DUE"     TO D04-PAY-TEXT.
           MOVE MDTE-DATE                    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                  TO D04-MDTE-DATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * MOTOR VEHICLE BOX
           IF ( LN-COLLCD = "AP" OR "AU" OR "AS")
              IF ( LN-ORGTERM > 6 )
                 MOVE "X" TO D05-BOX-2.
      * UNSECURED BOX
           IF ( LN-COLLCD = "UL" )
              MOVE "X" TO D05-BOX-4.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PERSONAL PROPERTY BOX
           IF ( LN-COLLCD = "AP" OR "PP" OR "SC" OR "AS")
              MOVE "X" TO D05-BOX-2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DECREASING LIFE SINGLE
           IF ( LN-INSTYPES-DL )
              IF ( LN-INSURED = "S" OR "M" )
                 MOVE "X"            TO D06-INS-BOX
                 MOVE INS-TERM(1)    TO D06-INS-TERM
                 MOVE LN-INSCOVR(1)  TO D06-COVERAGE
                 MOVE LN-INSPREM(1)  TO D06-PREMIUM
                 MOVE TC-FEE         TO D06-INSTAX
                 COMPUTE D06-TOTPREM = LN-INSPREM(1) + TC-FEE.
           MOVE 12 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DECREASING LIFE JOINT
           IF ( LN-INSTYPES-DL )
              IF ( LN-INSURED = "J" )
                 MOVE "X"            TO D06-INS-BOX
                 MOVE INS-TERM(1)    TO D06-INS-TERM
                 MOVE LN-INSCOVR(1)  TO D06-COVERAGE
                 MOVE LN-INSPREM(1)  TO D06-PREMIUM
                 MOVE TC-FEE         TO D06-INSTAX
                 COMPUTE D06-TOTPREM = LN-INSPREM(1) + TC-FEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LEVEL LIFE SINGLE
           IF ( LN-INSTYPES-LL )
              IF ( LN-INSURED = "S" OR "M" )
                 MOVE "X"            TO D06-INS-BOX
                 MOVE INS-TERM(1)    TO D06-INS-TERM
                 MOVE LN-INSCOVR(1)  TO D06-COVERAGE
                 MOVE LN-INSPREM(1)  TO D06-PREMIUM
                 MOVE TC-FEE         TO D06-INSTAX
                 COMPUTE D06-TOTPREM = LN-INSPREM(1) + TC-FEE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LEVEL LIFE JOINT
           IF ( LN-INSTYPES-LL )
              IF ( LN-INSURED = "J" )
                 MOVE "X"            TO D06-INS-BOX
                 MOVE INS-TERM(1)    TO D06-INS-TERM
                 MOVE LN-INSCOVR(1)  TO D06-COVERAGE
                 MOVE LN-INSPREM(1)  TO D06-PREMIUM
                 MOVE TC-FEE         TO D06-INSTAX
                 COMPUTE D06-TOTPREM = LN-INSPREM(1) + TC-FEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * A&H BENEFIT
      * ANY CODES THAT DONT INCLUDE MAINT FEE IN CHARGEABLE
           IF SP-INS-PRINCIPAL(5)
            OR SP-INS-TOTAL-NOTE(5)
             OR SP-INS-CASH-ADVANCE(5)
              OR SP-INS-SP-CASH-ADVANCE(5)
               OR SP-INS-TOTAL-NOTE-EXT(5)
                OR SP-INS-ADJ-PRINCIPAL(5)
                 OR SP-INS-ADJ-PRIN1INT(5)
                    COMPUTE WK-BENEFIT = LN-REGPYAMT - LN-MAINTFEE
                 ELSE
                    MOVE LN-REGPYAMT TO WK-BENEFIT.

           IF ( LN-INSPREM(2) NOT = ZEROES ) AND
              ( SP-INSAH-JOINT-FG NOT = "Y" )
              MOVE "X"                        TO D06-INS-BOX.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( LN-INSPREM(2) NOT = ZEROES ) 
              MOVE INS-TERM(2)    TO D06-INS-TERM
              MOVE WK-BENEFIT     TO D06-COVERAGE
              MOVE LN-INSPREM(2)  TO D06-PREMIUM
              MOVE TA-FEE         TO D06-INSTAX
              COMPUTE D06-TOTPREM = LN-INSPREM(2) + TA-FEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SECOND BENEFICIARY 
           IF LN-INSPREM(1) NOT = 0
              PERFORM GET-SECOND-BENEFICIARY
              MOVE LSM-SECOND-BEN-NAME TO D07-SECBEN.
      * EXPIRE DATE  / MAX BENEF / FIRST INSURED AGE / JOINT INSURED AGE
           IF LN-INSPREM(2) NOT = 0
              MOVE LN-INSEXP-DATE(2)   TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY         TO D07-AHEXP
              MOVE SP-INSMAXBENIF(5)   TO D07-AHPREM.
           MOVE MAKER-AGE    TO D07-FIRST-AGE.
           IF LN-INSURED = "J"
              MOVE JOINT-AGE    TO D07-JOINT-AGE.
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
              IF BW-CITY-TAX-CD NOT = SPACES OR 
                  BW-COUNTY NOT = SPACES
                 MOVE BW-CITY-TAX-CD TO D07-MUNIC-CITY
                 MOVE "/" TO D07-MUNIC-SLASH
                 MOVE BW-COUNTY TO D07-MUNIC-COUNTY.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * IUI LINE 1 
           IF LN-INSPREM(4) NOT = 0
              MOVE "X"           TO D06-INS-BOX
              MOVE INS-TERM(4)   TO D06-INS-TERM
              MOVE LN-INSCOVR(4) TO D09-INSCOVR3
              MOVE LN-INSPREM(4) TO D06-PREMIUM.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * IUI MO. BENEFIT
           IF LN-INSPREM(4) NOT = 0
              MOVE LN-REGPYAMT  TO D08-IUI-BENEFIT
              MOVE INS-TERM(4)  TO D08-IUI-TERM
              COMPUTE D08-IUI-TOTAL ROUNDED = LN-REGPYAMT * INS-TERM(4)
              MOVE T1-FEE       TO D08-IUI-SURCHARGE
              COMPUTE D08-IUI-TOTPREM = LN-INSPREM(4) + T1-FEE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PP FACTOR
           IF ( LN-INSTYPES-PP2 )
              MOVE LN-INSPREM(3) TO D08-IUI-FACTOR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PP FOR KY IS ONLY PP2
           IF ( LN-INSTYPES-PP2 )
              MOVE "X" TO D09-DUAL
              MOVE LN-INSCOVR(3) TO D09-INSCOVR3
              MOVE INS-TERM(3)   TO D09-INSTERM3
              MOVE TP-FEE        TO D08-IUI-SURCHARGE
              COMPUTE D08-IUI-TOTPREM = LN-INSPREM(3) + TP-FEE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SINGLE INTEREST AUTO INSURANCE   (THEY DONT WRITE)
           IF LN-INSPREM(6) NOT = 0
              MOVE LN-INSPREM(6) TO D08-IUI-FACTOR.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SINGLE INTEREST AUTO (THEY DONT WRITE)
           IF LN-INSPREM(6) NOT = 0
              MOVE LN-INSCOVR(6) TO D09-INSCOVR3
              MOVE INS-TERM(6)   TO D09-INSTERM3
              MOVE T3-FEE        TO D08-IUI-SURCHARGE
              COMPUTE D08-IUI-TOTPREM = LN-INSPREM(6) + T3-FEE.
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

           MOVE DISCLOSED-FINAMT             TO D10-PG2-FINAMT.
           MOVE 08 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      * 1. CREDIT LIGE INS. PREM.
           COMPUTE D12-AMT2 = LN-INSPREM(1) + TC-FEE.
           IF LN-INSPREM(1) NOT = 0
              MOVE "*"                       TO D12-ASTERISK.
      * TO
           MOVE T1-AMT(1)                    TO D12-AMT3.
           MOVE T1-DESC(1)                   TO D12-CXPAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 2. CREDIT DISABILITY INS. PREM.
           COMPUTE D12-AMT2 = LN-INSPREM(2) + TA-FEE.
           IF LN-INSPREM(2) NOT = 0
              MOVE "*"                       TO D12-ASTERISK.
      * TO
           MOVE T1-AMT(2)                    TO D12-AMT3.
           MOVE T1-DESC(2)                   TO D12-CXPAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      * 3. INVOLUNTARY UNEMPLOYMENT INS. PREM
           COMPUTE D12-AMT2 = LN-INSPREM(4) + T1-FEE.
           IF LN-INSPREM(4) NOT = 0
              MOVE "*"                       TO D12-ASTERISK.
      * TO
           MOVE T1-AMT(3)                    TO D12-AMT3.
           MOVE T1-DESC(3)                   TO D12-CXPAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 4. PROPERTY INS. PREM. 
           COMPUTE D12-AMT2 = LN-INSPREM(3) + TP-FEE.
           IF LN-INSPREM(3) NOT = 0
              MOVE "*"                       TO D12-ASTERISK.
      * TO
           MOVE T1-AMT(4)                    TO D12-AMT3.
           MOVE T1-DESC(4)                   TO D12-CXPAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 5. UNITED MOTOR CLUB (FEE CODE 93/94) #1582
           MOVE ROAD-GARD                    TO D12-AMT2.
           IF ROAD-GARD NOT = 0
              MOVE "*"                       TO D12-ASTERISK.
      * TO YOU
           MOVE ME-AMT                       TO D12-AMT3.
           IF ME-AMT NOT = 0
              MOVE MAKER-NAME-FIRST-LAST     TO D12-CXPAYEE2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 6. UMC SALES TAX
           MOVE TM-FEE                       TO D12-AMT2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 7. NON-FILING INS PREM (FEECODE 90)
           MOVE NON-FILE                     TO D12-AMT2.
           MOVE LN-FINCHG                    TO D11-AMT6.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 8. TO PUBLIC OFFICIALS
           MOVE OTHER-FEE                    TO D12-AMT2.
           COMPUTE D13-AMT5 = LN-INTCHG + LN-EXTCHG.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 9. AMOUNT PAID ON PRIOR ACCOUNT
           IF LN-PRLNNO NOT = 0
              PERFORM FIND-PRIOR-REFUNDS.
           COMPUTE D12-AMT2 = LN-PRBAL + LN-PRINTDUE
                         - LN-PRFINCHGRB - LN-PRINSRB.
           MOVE LN-SERCHG                    TO D13-AMT5.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      *  REFUNDS ON PRIOR ACCOUNT
           MOVE CL-REF                       TO D12-AMT1.
           MOVE AH-REF                       TO D13-AMT4.
           MOVE TOTAL-REPAY                  TO D11-AMT6.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PROPERTY REFUND
           MOVE PP-REF                       TO D12-AMT1.
      * AUTO REFUND
           MOVE O3-REF                       TO D13-AMT4.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * OTHER REFUND 
           MOVE 0                            TO D12-AMT1.
      * CREDIT INV. FEE REFUND
           MOVE SER-REF                      TO D13-AMT4.
      * AGREED RATE (CONTRACT RATE)
           COMPUTE D10-EFFRATE ROUNDED = LN-EFFRATE * 1.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
         
      * INTEREST REFUND
           MOVE INT-REF                      TO D12-AMT1.
           IF LN-INSURED NOT = "N" OR LN-INSPREM(6) NOT = 0 OR
               LN-INSPREM(5) NOT = 0 OR ROAD-GARD NOT = 0
              MOVE "* WE MAY RETAIN A PORTION" TO D10-RETAIN-TEXT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * UNEMP.INS. REFUND ON PRIOR ACCOUNT
           MOVE O1-REF                       TO D13-AMT4.
           IF LN-INSURED NOT = "N" OR LN-INSPREM(6) NOT = 0 OR
               LN-INSPREM(5) NOT = 0 OR ROAD-GARD NOT = 0
              MOVE "  OF THESE CHARGES."     TO D10-RETAIN-TEXT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * B. PRECOMPUTED 
           MOVE "X"                          TO D01-MAKER-INFO.
           MOVE 05 TO LN-FEED.
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

           COMPUTE D14-PG3-RATE ROUNDED = LN-EFFRATE * 1.
           MOVE 08 TO LN-FEED.
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

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-04-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      ********************************
       FIND-PRIOR-REFUNDS SECTION.
      ********************************
           MOVE 0 TO O1-REF CL-REF AH-REF PP-REF O3-REF MF-REF
                        INT-REF O2-REF.
           PERFORM OPEN-LP1-FILE.

           MOVE LN-OWNBR  TO LP-BRNO
                             QLP1-WBEG-BRNO
                             QLP1-WEND-BRNO.
           MOVE LN-PRLNNO TO LP-ACCTNO
                             QLP1-WBEG-ACCTNO
                             QLP1-WEND-ACCTNO.
           MOVE ALL "9"   TO LP-SEQNO
                             QLP1-WEND-SEQNO.
           MOVE ALL "0"   TO QLP1-WBEG-SEQNO

           PERFORM START-LP1-FILE-NOT-GREATER.
       NEXT-PRIOR-REFUND.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG = 9 OR (LP-BRNO NOT = LN-OWNBR)
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
           IF ( IO-FG = 9 ) OR ( TRC-PATH-OWNBR NOT = TC-BRNO )
      * THIS MEANS THERE IS ROAD GARD BUT NOT CHECKS
              IF SUB = 2 AND ROAD-GARD NOT = 0
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

           MOVE PIBW-NAME-FIRST-LAST(1) TO BORROWER-ME.
           IF ( TC-PAYEE = BORROWER-ME )
              ADD  TC-AMT TO ME-AMT
              ADD TC-AMT TO PROCEED-TOT 
              GO TO NEXT-MAN-CHECK.

          
           MOVE TC-PAYEE TO T1-DESC(SUB).
           MOVE TC-AMT   TO T1-AMT(SUB).
           MOVE TC-AMT TO PROCEED-TOT.

           IF SUB < 4
              ADD 1 TO SUB
           ELSE
              ADD TC-AMT TO PROCEED-TOT
              ADD TC-AMT TO PROC-WRK.

           GO TO NEXT-MAN-CHECK.

       REMAINING-MAN-DISBURSEMENT.

           IF ( PROC-WRK NOT = 0 )
              IF PROC-WRK NOT = T1-AMT(SUB)
                 MOVE "OTHER"      TO T1-DESC(SUB)
                 MOVE PROC-WRK     TO T1-AMT(SUB).

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
           IF IO-FG = 9 OR (CX-BRNO NOT = LN-ORGBR)      
      * THIS MEANS THERE IS ROAD GARD BUT NOT CHECKS
              IF SUB = 4 AND ROAD-GARD NOT = 0
                 GO TO EXIT-SYS-NEXT-DISBURSEMENT
              ELSE
                 GO TO REMAINING-SYS-DISBURSEMENT.

           IF CX-REF NOT = LN-ACCTNO 
              GO TO NEXT-SYS-CHECK.

           IF CX-BWPROC = "Y"
              ADD CX-AMT TO ME-AMT
              GO TO NEXT-SYS-CHECK.

           MOVE CX-PAYEE TO T1-DESC(SUB).
           MOVE CX-AMT TO T1-AMT(SUB).
           IF SUB < 4
              ADD 1 TO SUB
           ELSE
              ADD CX-AMT TO PROCEED-TOT.

           GO TO NEXT-SYS-CHECK.
       REMAINING-SYS-DISBURSEMENT.
           IF PROCEED-TOT NOT = 0
              MOVE "OTHER " TO T1-DESC(SUB)
              MOVE PROCEED-TOT TO T1-AMT(SUB).
       EXIT-SYS-NEXT-DISBURSEMENT.
           EXIT.

      ************************************
       GET-FEES SECTION.
      ************************************

           MOVE 0 TO NON-FILE OTHER-FEE ROAD-GARD.
           MOVE 0 TO TC-FEE TA-FEE TP-FEE T1-FEE T2-FEE T3-FEE
                     TM-FEE.
           MOVE SPACES TO ROAD-TYPE.
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
              IF LN-FEECODE(SUB) = BR-ROAD-G-SGL-FEECODE
                 ADD LN-FEEAMT(SUB) TO ROAD-GARD
                 MOVE "S" TO ROAD-TYPE
              ELSE
                 IF LN-FEECODE(SUB) = BR-ROAD-G-FAM-FEECODE
                    ADD LN-FEEAMT(SUB) TO ROAD-GARD
                    MOVE "F" TO ROAD-TYPE
                 ELSE
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
                       ADD LN-FEEAMT(SUB) TO T3-FEE
                    ELSE
                    IF LN-FEECODE(SUB) = "TM"
                       ADD LN-FEEAMT(SUB) TO TM-FEE
                    ELSE
                       ADD LN-FEEAMT(SUB) TO OTHER-FEE.

           ADD 1 TO SUB.
           GO TO NEXT-LN-FEE.
       EXIT-FIND-FEE.
           EXIT.
 
      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       GET-SECOND-BENEFICIARY.
           MOVE LN-OWNBR     TO LSM-BRNO.
           MOVE LN-ACCTNO    TO LSM-ACCTNO.
           MOVE 1            TO LSM-SEQNO.
           PERFORM OPEN-LSM1-FILE.
           PERFORM READ-LSM1-FILE.
           PERFORM CLOSE-LSM1-FILE.
           IF IO-FG = 9
              MOVE SPACES TO LSM-SECOND-BEN-NAME.

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
       COPY "LIBLP/LPLTD1RN.CPY".
       COPY "LIBLP/LPLSM1RN.CPY".
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
