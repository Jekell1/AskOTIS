       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST846.
       AUTHOR.        AMM.
       DATE-WRITTEN.  08-12-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST846.C
      *
      * DESCRIPTION   :  MINI CODE DISCLOSURE
      *
      * STATE         :  ALABAMA    (AL) 
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0812 PR#1449 CALLED FROM LONP2H  ***NEW***
      * BAH 2022.0503 START TRCFILE WITH LOANDATE LIKE A15, NOT TRANSDATE
      * BAH 20220803 HARD CODED START
      * BAH 20221109 ADDED INVOLUNTARY UNEMPLOYMENT INSURANCE OTHER 1
      *              INSURANCE LN-INSPREM(4) #1576
      * BAH 20221208 EXTCHG WAS NOT SUBTRACTED OUT OF TOTAL CASH ADVANCE #1576B
      * BAH 2024.0104 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0521 CHANGED D02-ACCTNO BACK TO Z(5)9 S35Q-32
      * BAH 2024.0524 HHGS WAS TESTING WRONG FIELD AND TEXT1 WAS MISSING A
      *               SPACE BEFORE " MONTHS" S35Q-44
      * BAH 2024.0813 PRINT SECOND BENEFICIARY NAME #1671 S35Q-116
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
           "@(#)FP/PST846.CBL S35 05/02/22-15:04:09 >".

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

       01  FILING-FEE                  PIC S9(7)V99 COMP-3 VALUE 0.
       01  MOTOR-CLUB                  PIC S9(7)V99 COMP-3 VALUE 0.
       01  NON-FILE                    PIC S9(7)V99 COMP-3 VALUE 0.
       01  OTHER-FEE                   PIC S9(7)V99 COMP-3 VALUE 0.
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
       01  PRIOR-APLC                  PIC S9(5)V99 COMP-3 VALUE 0.
       01  PROC-WRK                    PIC S9(7)V99 COMP-3 VALUE 0.
       01  PROCEED-BAL                 PIC S9(7)V99 COMP-3 VALUE 0.
       01  PROCEED-TOT                 PIC S9(7)V99 COMP-3 VALUE 0.
       01  ME-AMT                      PIC S9(5)V99 COMP-3 VALUE 0.
       01  BORROWER-ME                 PIC X(41).

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
           03  T4-TABLE OCCURS 3 TIMES.
              05  T4-AMT      PIC S9(6)V99 COMP-3.
              05  T4-DESC     PIC X(20).

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
      *    FMNO    : 115
      *    STATE   : ALABAMA   (AL)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D01-MAKER-INFO               PIC X(35).
           03  FILLER                       PIC X(01).
           03  D01-BRANCH-INFO              PIC X(39).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(27).
           03  D02-AGE                      PIC ZZ9.
           03  FILLER                       PIC X(08).
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

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D07-SEC-BEN                  PIC X(21).
           03  FILLER                       PIC X.
           03  D07-AHEXP                    PIC 99/99/99.
           03  FILLER                       PIC X(01).
           03  D07-TOTPREM                  PIC Z,ZZ9.99 BLANK ZERO.
           03  FILLER                       PIC X(01).
           03  D07-AHPREM                   PIC Z,ZZ9.99 BLANK ZERO.
           03  FILLER                       PIC X(01).
           03  D07-NO-DAYS                  PIC ZZ9.
           03  FILLER                       PIC X(07).
           03  D07-TERM-LIFE                PIC ZZ9.
           03  FILLER                       PIC X(06).
           03  D07-TERM-AH                  PIC ZZ9.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-08                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(04).
           03  D08-7-DAYS                   PIC X(01).
           03  FILLER                       PIC X(20).
           03  D08-SINGLE-PP                PIC X(01).
           03  FILLER                       PIC X(01).
           03  D08-RETRO                    PIC X(01).
           03  FILLER                       PIC X(07).
           03  D08-DUAL-PP                  PIC X(01).
           03  FILLER                       PIC X(32).
           03  D08-AH-BENEFIT               PIC Z(06).99 BLANK ZERO.
 
      *-----------------------------------------------------------------
       01  DETAIL-LINE-09                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(09).
           03  D09-INSCOVR2                 PIC Z(5).99.
           03  FILLER                       PIC X(06).
           03  D09-INSTERM2                 PIC ZZ9.
           03  FILLER                       PIC X(12).
           03  D09-INSPREM2                 PIC Z(5).99.
 
      *-----------------------------------------------------------------
       01  DETAIL-LINE-10                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(24).
           03  D10-AMT1                     PIC ZZZZZ.99.
           03  FILLER                       PIC X(27).
           03  D10-AMT3                     PIC ZZ,ZZZ.99.
           03  D10-AMT4                     PIC ZZZ,ZZZ.99.
 
      *-----------------------------------------------------------------
       01  DETAIL-LINE-11                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(29).
           03  D11-AMT2                     PIC ZZZ,ZZZ.99.
           03  FILLER                       PIC X(04).
           03  D11-CXPAYEE                  PIC X(23).

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
           MOVE " " TO T4-DESC(1) T4-DESC(2) T4-DESC(3).
           MOVE 0 TO T4-AMT(1) T4-AMT(2) T4-AMT(3).
           IF MOTOR-CLUB NOT = 0
              MOVE "UNITED MOTOR CLUB" TO T4-DESC(1)
              MOVE MOTOR-CLUB TO T4-AMT(1)
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
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-ADDRESS                TO D01-MAKER-INFO.
           MOVE GB-COMPANY-NAME              TO D01-BRANCH-INFO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-CSZ                 TO D01-MAKER-INFO.
           MOVE BR-ADR1                      TO D01-BRANCH-INFO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
              DELIMITED BY "  " INTO D01-BRANCH-INFO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE MAKER-AGE                    TO D02-AGE.
           MOVE BR-PHONE                     TO PST-PHONE-UNEDIT.
           PERFORM PST-CONVERT-PHONE-TO-EDIT.
           MOVE PST-PHONE-EDIT               TO D01-BRANCH-INFO.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( JOINT-SSNO NOT = ZEROES )
              MOVE JOINT-NAME-FIRST-LAST     TO D01-MAKER-INFO
              MOVE JOINT-AGE                 TO D02-AGE
           ELSE
           IF ( ENDR1-SSNO NOT = ZEROES )
              MOVE ENDR1-NAME-FIRST-LAST     TO D01-MAKER-INFO
              MOVE ENDR1-AGE                 TO D02-AGE.
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
      * CERTAIN HOUSEHOLD GOODS
           IF HHGS-INFO(1) NOT = SPACES
              MOVE "X" TO D05-BOX-3.
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
                 MOVE LN-INSPREM(1)  TO D06-PREMIUM.
           MOVE 11 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * DECREASING LIFE JOINT
           IF ( LN-INSTYPES-DL )
              IF ( LN-INSURED = "J" )
                 MOVE "X"            TO D06-INS-BOX
                 MOVE INS-TERM(1)    TO D06-INS-TERM
                 MOVE LN-INSCOVR(1)  TO D06-COVERAGE
                 MOVE LN-INSPREM(1)  TO D06-PREMIUM.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LEVEL LIFE SINGLE
           IF ( LN-INSTYPES-LL )
              IF ( LN-INSURED = "S" OR "M" )
                 MOVE "X"            TO D06-INS-BOX
                 MOVE INS-TERM(1)    TO D06-INS-TERM
                 MOVE LN-INSCOVR(1)  TO D06-COVERAGE
                 MOVE LN-INSPREM(1)  TO D06-PREMIUM.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LEVEL LIFE JOINT
           IF ( LN-INSTYPES-LL )
              IF ( LN-INSURED = "J" )
                 MOVE "X"            TO D06-INS-BOX
                 MOVE INS-TERM(1)    TO D06-INS-TERM
                 MOVE LN-INSCOVR(1)  TO D06-COVERAGE
                 MOVE LN-INSPREM(1)  TO D06-PREMIUM.
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
           IF ( LN-INSPREM(2) NOT = ZEROES )
              MOVE INS-TERM(2)    TO D06-INS-TERM
              MOVE WK-BENEFIT     TO D06-COVERAGE
              MOVE LN-INSPREM(2)  TO D06-PREMIUM.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * EXPIRE DATE  / MAX BENEF / FIRST INSURED AGE / JOINT INSURED AGE
           IF LN-INSPREM(1) NOT = 0
              MOVE INS-TERM(1)    TO D07-TERM-LIFE.
           IF LN-INSPREM(2) NOT = 0
              MOVE INS-TERM(2)    TO D07-TERM-AH.
           MOVE 04 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SECOND BENEFICIARY
           IF LN-INSPREM(1) NOT = 0
              PERFORM GET-SECOND-BENEFICIARY
              MOVE LSM-SECOND-BEN-NAME TO D07-SEC-BEN.
           IF LN-INSPREM(2) NOT = 0
              MOVE LN-INSEXP-DATE(2)   TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY         TO D07-AHEXP
              MOVE LN-INSPREM(2)       TO D07-AHPREM
           ELSE
              MOVE LN-INSEXP-DATE(1)   TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY         TO D07-AHEXP.
           COMPUTE D07-TOTPREM = LN-INSPREM(1) + LN-INSPREM(2).
           MOVE LN-LOANDATE TO NUM-DATE.
           MOVE LN-1STPYDATE TO SYS-DATE.
           PERFORM TIM.
           MOVE ELAPSED-DAYS TO D07-NO-DAYS.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * 7 DAYS 
           IF LN-INSTYPES-AH
              MOVE "X" TO D08-7-DAYS D08-RETRO
      * ANY CODES THAT DONT INCLUDE MAINT FEE IN CHARGEABLE
              IF SP-INS-PRINCIPAL(5)
               OR SP-INS-TOTAL-NOTE(5)
                OR SP-INS-CASH-ADVANCE(5)
                 OR SP-INS-SP-CASH-ADVANCE(5)
                  OR SP-INS-TOTAL-NOTE-EXT(5)
                   OR SP-INS-ADJ-PRINCIPAL(5)
                    OR SP-INS-ADJ-PRIN1INT(5)
                 COMPUTE D08-AH-BENEFIT = LN-REGPYAMT - LN-MAINTFEE
           MOVE 04 TO LN-FEED.
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

      * PROPERTY 
      * DUAL INTEREST
           IF LN-INSTYPES-PP2
              MOVE "X" TO D08-DUAL-PP
           ELSE
      * SINGLE INTEREST
           IF LN-INSTYPES-PP
              MOVE "X" TO D08-SINGLE-PP.
           MOVE 07 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-INSCOVR(3) TO D09-INSCOVR2
           MOVE INS-TERM(3)   TO D09-INSTERM2
           MOVE LN-INSPREM(3) TO D09-INSPREM2.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * MOTOR VEHICLE INSURANCE LIMITED PHYSICAL DAMAGE COVERAGE
           MOVE 0 TO D09-INSCOVR2 D09-INSTERM2 D09-INSPREM2.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * MOTOR VEHICLE  SINGLE INTEREST  FOR LENDER'S INTEREST
           MOVE LN-INSCOVR(6)  TO D09-INSCOVR2.
           MOVE INS-TERM(6)    TO D09-INSTERM2.
           MOVE LN-INSPREM(6)  TO D09-INSPREM2.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INVOLUNTARY UNEMPLOYMENT INSURANCE
           MOVE LN-INSCOVR(4)  TO D09-INSCOVR2.
           MOVE INS-TERM(4)    TO D09-INSTERM2.
           MOVE LN-INSPREM(4)  TO D09-INSPREM2.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TOTAL OF PAYMENTS
           MOVE TOTAL-REPAY TO D11-AMT2.
      * THEY WERE ENTERING A PRIOR LOAN # ON NEW LOANS...
           IF LN-SRCD = "PB" OR "RN" OR "SC" OR "RO" OR "RB"
              IF LN-PRLNNO NOT = 0
                 PERFORM FIND-PRIOR-REFUNDS.
      * BALANCE OLD ACCOUNT
           COMPUTE D10-AMT3 = LN-PRBAL + LN-PRINTDUE - PRIOR-APLC.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INTEREST
           COMPUTE D11-AMT2 = LN-INTCHG + LN-EXTCHG.
      * LATE CHARGE PREVIOUS ACCOUNT
           MOVE PRIOR-APLC TO D10-AMT3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TOTAL CASH ADVANCE
           COMPUTE D11-AMT2 = TOTAL-REPAY - LN-INTCHG - LN-EXTCHG.
      * INTEREST REFUND
           MOVE INT-REF TO D10-AMT3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INTEREST SURCHARGE REFUND
           MOVE SER-REF TO D10-AMT3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ACCOUNT MAINTENANCE FEE
           MOVE LN-SUM-MAINTFEE TO D10-AMT1.
      * CREDIT LIFE INS. REFUND
           MOVE CL-REF          TO D10-AMT3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INTEREST SURCHARGE
           MOVE LN-SERCHG TO D10-AMT1.
      * CREDIT DISABILITY INS. REFUND
           MOVE AH-REF    TO D10-AMT3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AMOUNT FINANCED
           MOVE DISCLOSED-FINAMT TO D11-AMT2.
      * PROPERTY INSURANCE REFUND
           MOVE PP-REF TO D10-AMT3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INV. UNEMP. INSURANCE REFUND
           MOVE O1-REF TO D10-AMT3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * MAINTENANCE FEE REFUND
           MOVE MF-REF TO D10-AMT3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CREDIT LIFE INS. PREM
           MOVE LN-INSPREM(1) TO D10-AMT1.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * CREDIT DISABILITY INS. PREM
           MOVE LN-INSPREM(2) TO D10-AMT1.
      * MOTOR VEHICLE INSURANCE LIMITED PHYS. DAMAGE REFUND
           MOVE 0             TO D10-AMT3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PROP. INS (LEVEL TERM) PREM
           MOVE LN-INSPREM(3) TO D10-AMT1.
      * SINGLE INTEREST INS. REFUND
           MOVE O3-REF        TO D10-AMT3.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INV. UNEMP. INSURANCE PREM
           MOVE LN-INSPREM(4) TO D10-AMT1.
      * NET AMOUNT OF PAYOFF
           COMPUTE D10-AMT3 = LN-PRBAL + LN-PRINTDUE
                            - LN-PRFINCHGRB - LN-PRINSRB.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * MOTOR VEHICLE INSURANCE LIMITED PHYS. DAMAGE
           MOVE 0          TO D10-AMT1.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SINGLE INTEREST INS. PREM
           MOVE LN-INSPREM(6) TO D10-AMT1.
      * TO
           MOVE T4-AMT(1)     TO D10-AMT4.
           MOVE T4-DESC(1)    TO D11-CXPAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * NON-FILING INSURANCE
           MOVE NON-FILE   TO D10-AMT1.
      * TO 
           MOVE T4-AMT(2)  TO D10-AMT4.
           MOVE T4-DESC(2) TO D11-CXPAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * OFFICIAL FILING FEES
           MOVE FILING-FEE TO D10-AMT1.
      * TO
           MOVE T4-AMT(3)  TO D10-AMT4.
           MOVE T4-DESC(3) TO D11-CXPAYEE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * SUB-TOTAL 
           COMPUTE D11-AMT2 =  LN-INSPREM(1) + LN-INSPREM(2)
            + LN-INSPREM(3) + LN-INSPREM(4) + LN-INSPREM(6) + NON-FILE +
               FILING-FEE + OTHER-FEE.
      * CASH RECEIVED BY DEBTOR
           MOVE ME-AMT TO D10-AMT4.
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
           MOVE 0 TO O1-REF CL-REF AH-REF PP-REF O3-REF MF-REF
                        INT-REF O2-REF.
           MOVE 0 TO PRIOR-APLC.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-PRLNNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       NEXT-PRIOR-REFUND.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG = 9 OR (LP-BRNO NOT = LN-OWNBR)
              GO TO EXIT-FIND-PRIOR-REFUNDS.

           IF LP-ACCTNO NOT = LN-PRLNNO
              GO TO EXIT-FIND-PRIOR-REFUNDS.

           IF LP-REV = "Y"
              GO TO NEXT-PRIOR-REFUND.

           IF LP-TRCD = "PB" OR "RN" OR "SC" OR "RO" OR "RB"
              ADD LP-APLC TO PRIOR-APLC.

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

           MOVE LN-ORGBR            TO TC-BRNO
                                       QTRC1-WBEG-BRNO
                                       QTRC1-WEND-BRNO.
           MOVE LN-LOANDATE         TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
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

      ***************************************
       SYSTEM-CHECKS-DISBURSEMENT SECTION.
      ***************************************
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
           IF SUB < 3
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

           MOVE 0 TO NON-FILE OTHER-FEE MOTOR-CLUB.
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
              ADD LN-FEEAMT(SUB) TO FILING-FEE
           ELSE
           IF LN-FEECODE(SUB) = "93" OR "94"
              ADD LN-FEEAMT(SUB) TO MOTOR-CLUB
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
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBFP/FPIOPST.CPY".
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
