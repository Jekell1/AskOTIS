       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST847.
       AUTHOR.        AMM.
       DATE-WRITTEN.  09-01-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST847.C
      *
      * DESCRIPTION   :  DISCLOSURE
      *
      * STATE         :  MISSISSIPPI   (MS)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0901 PR#1454 CALLED FROM LONP4V
      * BAH 2022.0503 START TRCFILE WITH LOANDATE LIKE A15, NOT TRANSDATE
      * BAH 20220901 HARD CODED START
      * BAH 2024.0104 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0315 PERSONAL PROPERTY LINE FEED OFF A LINE, CHANGED FROM
      *               4 TO 3,  FIX INSPREM(6) TO BE (4)  #1649
      * BAH 2024.0521 CHANGED D02-ACCTNO BACK TO 999999 S35Q-32
      * BAH 2024.0813 PRINT SECOND BENEFICIARY #1671 S35Q-116
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
           "@(#)FP/PST847.CBL S35 05/02/22-13:58:28 >".

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
           03  FILLER         PIC X(10)  VALUE "BEGINNING ".
           03  T1-1STPYDATE   PIC 99/99/99.
           03  FILLER         PIC X(07)  VALUE " EVERY ".
           03  T1-MONTH.
               05  T1-MONTHS  PIC 99.
               05  FILLER     PIC X(07) VALUE " MONTHS".

       01  TEXT2.
           03  FILLER         PIC X(23)  VALUE
           "WITH FINAL PAYMENT DUE ".
           03  T2-MATURITY    PIC 99/99/99.

       01  TEXT4.
           03  T4-TABLE OCCURS 2 TIMES.
              05  T4-AMT      PIC S9(6)V99 COMP-3.
              05  T4-DESC.
                  07  T4-ASTERISK PIC X.
                  07  FILLER      PIC X.
                  07  T4-ROADG    PIC X(18).

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
      *    FMNO    : 200
      *    STATE   : MISSISSIPPI   (MS)
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
           03  D02-ACCTNO                   PIC 999999.
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
           03  D04-DUE-DESC                 PIC X(40).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D05-BOX-1                    PIC X(01).
           03  FILLER                       PIC X(22).
           03  D05-BOX-2                    PIC X(01).
           03  FILLER                       PIC X(22).
           03  D05-BOX-3                    PIC X(01).
           03  FILLER                       PIC X(14).
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
           03  FILLER                       PIC X(08).
           03  D06-TOTPREM                  PIC ZZZZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(3).
           03  D07-SECOND-BEN               PIC X(23).
           03  FILLER                       PIC X(2).
           03  D07-AHEXP                    PIC 99/99/99.
           03  D07-AHPREM                   PIC ZZ,ZZ9.99.
           03  FILLER                       PIC X(03).
           03  D07-FIRST-AGE                PIC ZZZ.
           03  FILLER                       PIC X(06).
           03  D07-JOINT-AGE                PIC ZZZ.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-08                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(19).
           03  D08-IUI-TERM                 PIC ZZZ.
 
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
           03  D09-INSPREM3                 PIC ZZ,ZZ9.99.
 
      *-----------------------------------------------------------------
       01  DETAIL-LINE-10                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(30).
           03  D10-PG2-FINAMT               PIC ZZZZZZ.99.
           03  FILLER                       PIC X(05).
           03  D10-PG2-APR                  PIC ZZ9.99.
           03  FILLER                       PIC X(08).
           03  D10-PG2-FINCHG               PIC ZZZ,ZZZ.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-11                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(11).
           03  D11-REF-AMT                  PIC ZZ,ZZZ.99.
           03  FILLER                       PIC X(07).
           03  D11-AMT1                     PIC ZZZ,ZZZ.99.
           03  FILLER                       PIC X(08).
           03  D11-CHECK-AMT                PIC ZZ,ZZZ.99.
           03  FILLER                       PIC X(03).
           03  D11-CXPAYEE                  PIC X(21).
	   03  FILLER                       REDEFINES D11-CXPAYEE.
               05  FILLER                   PIC X(01).
               05  D11-CXPAYEE2             PIC X(20).

       COPY "LIBFP/PST_COPYW.CPY".
       COPY "LIBFP/PST_INFOW.CPY".
       COPY "LIBFP/PST_FORMW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
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

      * NEED GET-FEES BEFORE CHECKS, BECAUSE WE ARE GOING TO PRINT
      * ROADGARD (A FEE) IN THE DISBURSEMENTS

           PERFORM GET-FEES.

           MOVE LN-PROCASH TO ME-AMT PROCEED-BAL.
           MOVE 0 TO PROC-WRK.

      * CHECK FOR NETSPEND DEBIT CARD PROCEEDS
           IF BR-USE-DEBIT-CARDS = "Y"
              PERFORM CHECK-FOR-DEBIT-CARD.

           MOVE 1 TO SUB.
           MOVE " " TO T4-DESC(1) T4-DESC(2).
           MOVE 0 TO T4-AMT(1) T4-AMT(2).
           IF ROAD-GARD NOT = 0
              MOVE ROAD-GARD TO T4-AMT(1)
              IF ROAD-TYPE = "S"
                 MOVE BR-ROAD-G-SGL-FEECODE TO CDB-CODE
              ELSE
                 MOVE BR-ROAD-G-FAM-FEECODE TO CDB-CODE
              END-IF
              PERFORM GET-FEECODE
              MOVE CDB-DESC TO T4-ROADG(1)
              MOVE "*" TO T4-ASTERISK(1)
              ADD 1 TO SUB.

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
           MOVE GB-COMPANY-NAME              TO D01-BRANCH-INFO.
           MOVE 07 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-ADDRESS                TO D01-MAKER-INFO.
           MOVE BR-ADR1                      TO D01-BRANCH-INFO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-CSZ                    TO D01-MAKER-INFO.
           IF BR-ADR2 NOT = SPACES
              MOVE BR-ADR2                   TO D01-BRANCH-INFO
           ELSE
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D01-BRANCH-INFO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                 DELIMITED BY "  " INTO D01-BRANCH-INFO
           ELSE
              MOVE BR-PHONE                     TO PST-PHONE-UNEDIT
              PERFORM PST-CONVERT-PHONE-TO-EDIT
              MOVE PST-PHONE-EDIT               TO D01-BRANCH-INFO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-ADR2 NOT = SPACES
              MOVE BR-PHONE                     TO PST-PHONE-UNEDIT
              PERFORM PST-CONVERT-PHONE-TO-EDIT
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

           COMPUTE D03-APRATE ROUNDED = LN-APRATE * 1.
           MOVE LN-FINCHG                    TO D03-FINCHG.
           MOVE DISCLOSED-FINAMT             TO D03-FINAMT.
           MOVE TOTAL-REPAY                  TO D03-TOTAL-PAY.
           MOVE 05 TO LN-FEED.
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
           MOVE LN-1STPYDATE                 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                  TO T1-1STPYDATE.
           IF LN-ORGTERM = 1
              MOVE "MONTH"                   TO T1-MONTH
           ELSE
              MOVE 1                         TO T1-MONTHS.
           MOVE TEXT1                        TO D04-DUE-DESC.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-1STPYAMT NOT = LN-REGPYAMT
              COMPUTE D04-TERM = LN-ORGTERM - 1
              MOVE LN-REGPYAMT TO D04-REGPYAMT
           ELSE
              IF LN-LASTPYAMT NOT = LN-REGPYAMT
                 MOVE 1 TO D04-TERM
                 MOVE LN-LASTPYAMT TO D04-REGPYAMT.
           MOVE MDTE-DATE                    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                  TO T2-MATURITY.
           MOVE TEXT2                        TO D04-DUE-DESC.
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
                 MOVE LN-INSPREM(1)  TO D06-TOTPREM.
           MOVE 11 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DECREASING LIFE JOINT
           IF ( LN-INSTYPES-DL )
              IF ( LN-INSURED = "J" )
                 MOVE "X"            TO D06-INS-BOX
                 MOVE INS-TERM(1)    TO D06-INS-TERM
                 MOVE LN-INSCOVR(1)  TO D06-COVERAGE
                 MOVE LN-INSPREM(1)  TO D06-TOTPREM.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LEVEL LIFE SINGLE
           IF ( LN-INSTYPES-LL )
              IF ( LN-INSURED = "S" OR "M" )
                 MOVE "X"            TO D06-INS-BOX
                 MOVE INS-TERM(1)    TO D06-INS-TERM
                 MOVE LN-INSCOVR(1)  TO D06-COVERAGE
                 MOVE LN-INSPREM(1)  TO D06-TOTPREM.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LEVEL LIFE JOINT 
           IF ( LN-INSTYPES-LL )
              IF ( LN-INSURED = "J" )
                 MOVE "X"            TO D06-INS-BOX
                 MOVE INS-TERM(1)    TO D06-INS-TERM
                 MOVE LN-INSCOVR(1)  TO D06-COVERAGE
                 MOVE LN-INSPREM(1)  TO D06-TOTPREM.
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
              MOVE LN-INSPREM(2)  TO D06-TOTPREM.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SECOND BENEFICIARY 
           IF LN-INSPREM(1) NOT = 0
              PERFORM GET-SECOND-BENEFICIARY
              MOVE LSM-SECOND-BEN-NAME TO D07-SECOND-BEN.
      * EXPIRE DATE  / MAX BENEF / FIRST INSURED AGE / JOINT INSURED AGE
           IF LN-INSPREM(2) NOT = 0
              MOVE LN-INSEXP-DATE(2)   TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY         TO D07-AHEXP
              MOVE TOTAL-NOTE          TO D07-AHPREM.
           MOVE MAKER-AGE       TO D07-FIRST-AGE.
           IF LN-INSURED = "J"
              MOVE JOINT-AGE    TO D07-JOINT-AGE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PROPERTY 
           IF ( LN-INSTYPES-PP OR LN-INSTYPES-PP2 )
              MOVE LN-INSCOVR(3) TO D09-INSCOVR3
              MOVE INS-TERM(3)   TO D09-INSTERM3
              MOVE LN-INSPREM(3) TO D06-TOTPREM
              IF LN-INSTYPES-PP
                 MOVE "X" TO D09-SINGLE
              ELSE
              IF LN-INSTYPES-PP2
                 MOVE "X" TO D09-DUAL.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SINGLE INTEREST AUTO INSURANCE 
           IF LN-INSPREM(4) NOT = 0
              MOVE LN-INSCOVR(4) TO D09-INSCOVR3
              MOVE INS-TERM(4)   TO D09-INSTERM3
              MOVE LN-INSPREM(4) TO D06-TOTPREM.
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

           MOVE DISCLOSED-FINAMT             TO D10-PG2-FINAMT.
           MOVE 09 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      * 1. CREDIT LIFE INS. PREM.
           MOVE LN-INSPREM(1)                TO D11-AMT1.
      * TO
           MOVE T4-AMT(1)                    TO D11-CHECK-AMT.
           MOVE T4-DESC(1)                   TO D11-CXPAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 2. CREDIT DISABILITY INS. PREM.
           MOVE LN-INSPREM(2)                TO D11-AMT1.
      * TO
           MOVE T4-AMT(2)                    TO D11-CHECK-AMT.
           MOVE T4-DESC(2)                   TO D11-CXPAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      * 3. PROPERTY INS. PREM. 
           MOVE LN-INSPREM(3)                TO D11-AMT1.
      * TO
           MOVE ME-AMT                       TO D11-CHECK-AMT.
           IF ME-AMT NOT = 0
              MOVE MAKER-NAME-FIRST-LAST     TO D11-CXPAYEE2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 4. SI AUTO INS PREM
           MOVE LN-INSPREM(4)                TO D11-AMT1.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 5. OTHER INS.PREM
           MOVE ZEROES                       TO D11-AMT1.
      * 9. FINANCE CHARGE
           MOVE LN-FINCHG                    TO D10-PG2-FINCHG.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 6. TO PUBLIC OFFICIALS -
           MOVE OTHER-FEE                    TO D11-AMT1.
      * INTEREST CHARGE
           COMPUTE D10-PG2-FINCHG = LN-INTCHG + LN-EXTCHG.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 7. AMOUNT PAID ON PRIOR ACCOUNT
           IF LN-PRLNNO NOT = 0
              PERFORM FIND-PRIOR-REFUNDS.
           COMPUTE D11-AMT1 = LN-PRBAL + LN-PRINTDUE
                         - LN-PRFINCHGRB - LN-PRINSRB.
      * CLOSING FEE
           MOVE LN-SERCHG                    TO D10-PG2-FINCHG.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      *  REFUNDS ON PRIOR ACCOUNT
           MOVE CL-REF                       TO D11-REF-AMT.
           MOVE AH-REF                       TO D11-AMT1.
      * TOTAL OF PAYMENTS
           MOVE TOTAL-REPAY                  TO D10-PG2-FINCHG.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PROPERTY REFUND
           MOVE PP-REF                       TO D11-REF-AMT.
      * AUTO REFUND
           MOVE O1-REF                       TO D11-AMT1.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AGREED RATE OF INTEREST
           COMPUTE D10-PG2-APR ROUNDED = LN-EFFRATE * 1.
           MOVE 02 TO LN-FEED.
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
       COPY "LIBLP/LPUPWK.CPY".

      ********************************
       FIND-PRIOR-REFUNDS SECTION.
      ********************************
           MOVE 0 TO O1-REF CL-REF AH-REF PP-REF O3-REF
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

           MOVE TC-PAYEE TO T4-DESC(SUB).
           MOVE TC-AMT   TO T4-AMT(SUB).
           MOVE TC-AMT TO PROCEED-TOT.

           IF SUB < 2
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
           IF IO-FG = 9 OR (CX-BRNO NOT = LN-ORGBR)      
              GO TO REMAINING-SYS-DISBURSEMENT.

           IF CX-REF NOT = LN-ACCTNO 
              GO TO NEXT-SYS-CHECK.

           IF CX-BWPROC = "Y"
              ADD CX-AMT TO ME-AMT
              GO TO NEXT-SYS-CHECK.

           MOVE CX-PAYEE TO T4-DESC(SUB).
           MOVE CX-AMT TO T4-AMT(SUB).
           IF SUB < 2
              ADD 1 TO SUB
           ELSE
              ADD CX-AMT TO PROCEED-TOT.

           GO TO NEXT-SYS-CHECK.
       REMAINING-SYS-DISBURSEMENT.
           IF PROCEED-TOT NOT = 0
              IF PROC-WRK NOT = T4-AMT(SUB)
                 MOVE "OTHER " TO T4-DESC(SUB)
                 MOVE PROCEED-TOT TO T4-AMT(SUB).
       EXIT-SYS-NEXT-DISBURSEMENT.
           EXIT.

      ************************************
       GET-FEES SECTION.
      ************************************

           MOVE 0 TO NON-FILE OTHER-FEE ROAD-GARD.
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
                       ADD LN-FEEAMT(SUB) TO OTHER-FEE.

           ADD 1 TO SUB.
           GO TO NEXT-LN-FEE.
       EXIT-FIND-FEE.
           EXIT.
 
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       GET-FEECODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE LN-OWNBR TO CDB-BRANCH.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO CDB-DESC.
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
