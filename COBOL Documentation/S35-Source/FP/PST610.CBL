       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST610.
       AUTHOR.        AMM.
       DATE-WRITTEN.  07-14-2022.
      ******************************************************************
      *
      * IDENTIFICATION:  LP/PST610.C
      *
      * DESCRIPTION   :  RECEIPT WITH RECRUITING COUPON
      *
      *                  PAYMENT RECEIPT PRINT
      *
      * *** NOTE ***
      *       FMNO = 2 = WORLD "LA", PRINTS LCHG ASSESSED MESSAGE
      *       FMNO = 6 = WORLD 3RD PARTY, OMITS SOME FIELDS
      *
      *=================================================================
      * REV:
      * AMM 2022.0714 PR#1560
      * BAH 2022.0901 HARD CODED START
      * BAH 2023.0109 FIXED THE TRANS-DATE ISSUE WITH WRONG AVAILABLE CREDIT
      *              WHEN PRINTING FROM BATCH #1586
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
           "@(#)FP/PST610.CBL S35 03/26/24-14:12:39 >".

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
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
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

       01  LCHG-MESS-PRINTED    PIC X VALUE "N".
       01  C-S-Z                PIC X(85).
       01  STATE                PIC XX.
       01  ZIP                  PIC X(10).
       01  COMPANY-NAME         PIC X(40).

       01  HOLD-INTEREST-REBATE PIC S9(6)V99 VALUE 0.

       01  SAVE-LM1-KEY                          VALUE SPACES.
           03  SAVE-LM-BRNO        PIC 9(04).
           03  SAVE-LM-ACCTNO      PIC 9(08).
           03  SAVE-LM-DATE        PIC 9(08).
           03  SAVE-LM-SEQ         PIC 9(03).
       01  SAVE-LM-MEMO.
           03  FILLER           PIC X(7)  VALUE "FORM # ".
           03  MEMO-FMNO        PIC 9(4)  VALUE 0.
           03  FILLER           PIC X(3)  VALUE ",  ".
           03  MEMO-DESC        PIC X(25) VALUE SPACES.
           03  MEMO-CHG-CD      PIC X(22) VALUE SPACES.

       01  AV-CREDIT            PIC $(6)9.99.
       01  R-CREDIT REDEFINES AV-CREDIT
                                PIC X(10).
       01  T-MESSAGE-1          PIC X(30).
       01  T-MESSAGE-2          PIC X(30).


       78  CROSS-DATA-PATHS---OFF                    VALUE SPACES.
       78  CROSS-DATA-PATHS---ON                     VALUE "Y".
       77  CROSS-DATA-PATHS            PIC X(01)     VALUE SPACES.
           88  CROSS-DATA-PATHS-OFF                  VALUE SPACES.
           88  CROSS-DATA-PATHS-ON                   VALUE "Y".

       01  CROSS-DATA-PATHS-BRNO        PIC 9(04)     VALUE ZEROES.

       01  HOLD-FM1-KEY                PIC X(28).
       01  HOLD-FM2-KEY.
           03  HOLD-FM2-BRNO PIC 9(4).
           03  HOLD-FM2-FMNO PIC 9(4).
           03  HOLD-FM2-ZIP  PIC X(15).

       01  IS-BATCH-PRINT              PIC X         VALUE SPACES.
       01  MSEL-LIST                   PIC X(05)     VALUE "MSEL.".

       01  PAGE-COPY-MAX               PIC 9(03)     VALUE ZEROES.
       01  PAGE-COPY-ELE               PIC 9(03)     VALUE ZEROES.
       01  COPY-COUNTER                REDEFINES PAGE-COPY-ELE
                                       PIC 9(03).

       78  PST-PRIMARY-FILE---LN                     VALUE "LN".
       78  PST-PRIMARY-FILE---PD                     VALUE "PD".
       77  PST-PRIMARY-FILE            PIC X(06)     VALUE "LN".
           88  PST-PRIMARY-FILE-LN                   VALUE "LN".
           88  PST-PRIMARY-FILE-PD                   VALUE "PD".

       01  PLW-FEED                PIC 9(03)     VALUE ZEROES.
       01  LN-FEED                 REDEFINES PLW-FEED
                                   PIC 9(03).


       01  SUB                         PIC 9(06)     VALUE ZEROES.

      **************************************
      *        RECEIPT VARIABLE TEXT       *
      **************************************
       01  TEXT1.
           03  FILLER           PIC X(5)     VALUE SPACES.
           03  FILLER           PIC X(9)     VALUE
               "ACCOUNT #".

       01  TEXT2.
           03  FILLER           PIC X(31)    VALUE SPACES.
           03  FILLER           PIC X(27)    VALUE
               "-------- APPLIED ---------".
           03  FILLER           PIC X(8)     VALUE SPACES.
           03  FILLER           PIC X(15)    VALUE "NEW      PAST".

       01  TEXT3.
           03  FILLER           PIC X(8)     VALUE SPACES.
           03  TEXT3-A          PIC X(53)    VALUE
               "DATE      PAYMENT      OTHER  INTEREST  PRINCIPAL".
           03  TEXT3-B          REDEFINES TEXT3-A.
               05  FILLER       PIC X(30).
               05  TEXT3-C      PIC X(8).
           03  FILLER           PIC X(1)     VALUE SPACES.
           03  FILLER           PIC X(17)    VALUE
               "BALANCE       DUE".
       01  TEXT3A.
           03  FILLER           PIC X(8)     VALUE SPACES.
           03  FILLER           PIC X(77)    VALUE
               "DATE      PAYMENT".

       01  TEXT4.
           03  FILLER           PIC X(33)    VALUE SPACES.
           03  TEXT4-A          PIC X(11)    VALUE SPACES.
           03  TEXT4-B          PIC X(10)    VALUE SPACES.
           03  TEXT4-C          PIC X(10)    VALUE SPACES.
       01  TEXT5.
           03  FILLER           PIC X(31)    VALUE SPACES.
           03  FILLER           PIC X(26)    VALUE
               "THANK YOU FOR YOUR PAYMENT".
       01  TEXT6.
           03  FILLER           PIC X(10)    VALUE SPACES.
           03  FILLER           PIC X(40)    VALUE
               "PLEASE BE ADVISED THAT A LATE CHARGE OF ".
           03  T6-LCHG          PIC $$$9.99 BLANK ZERO.
           03  FILLER           PIC X(18)     VALUE
               " HAS BEEN ASSESSED".
       01  TEXT7.
           03  FILLER           PIC X(05)    VALUE SPACES.
           03  FILLER           PIC X(42)    VALUE
               "TO YOUR ACCOUNT. THIS CHARGE WAS ASSESSED ".
           03  FILLER           PIC X(22)    VALUE
               "BECAUSE YOUR SCHEDULED".
       01  TEXT8.
           03  FILLER           PIC X(05)    VALUE SPACES.
           03  FILLER           PIC X(50)    VALUE
               "PAYMENT BECAME TEN OR MORE DAYS PAST DUE.".

      * WORLD
       01  TEXT9.
           03  FILLER           PIC X(34)    VALUE  SPACES.
           03  FILLER           PIC X(51)    VALUE
               "***IMPORTANT NOTICE***".

       01  TEXT10.
           03  FILLER           PIC X(44)    VALUE
               "   NOW OFFERING MANY DIFFERENT SIZE LOANS TO".
           03  FILLER           PIC X(35)    VALUE
               " ASSIST WITH YOUR FINANCIAL NEEDS  ".
       01  TEXT11.
           03  FILLER           PIC X(45)    VALUE
               "   AS WELL AS YEAR-ROUND TAX PREPARATION AND".
           03  FILLER           PIC X(35)    VALUE
               " ELECTRONIC FILING. PLEASE TELL    ".
       01  TEXT12.
           03  FILLER           PIC X(51)    VALUE
               "   YOUR FRIENDS! WE APPRECIATE YOUR BUSINESS.".

       01  TEXT10-WI.
           03  FILLER           PIC X(49)    VALUE
               "   NOW OFFERING MANY DIFFERENT SIZE LOANS TO".
           03  FILLER           PIC X(35)    VALUE
               " ASSIST WITH YOUR FINANCIAL NEEDS. ".
       01  TEXT12-WI.
           03  FILLER           PIC X(52)    VALUE
               "   PLEASE TELL YOUR FRIENDS! WE APPRECIATE YOUR".
           03  FILLER           PIC X(10)    VALUE
               " BUSINESS.".

       01  TEXT15.
           03  FILLER           PIC X(20)    VALUE  " ".
           03  FILLER           PIC X(38)    VALUE
               " *** INTEREST INCLUDES LATE CHARGE OF ".
           03  TEXT-APLC        PIC $$$.99.
           03  FILLER           PIC X(5)     VALUE "  ***".

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
      *    FMNO    : 4
      *    STATE   : ***
      *=================================================================
       01  DETAIL-LINE-01                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(05).
           03  W-BR-NAME                  PIC X(28).
           03  FILLER                     PIC XX.
           03  W-BR-ADR1                  PIC X(24).
           03  FILLER                     PIC X.
           03  W-BR-ADR2                  PIC X(24).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(11).
           03  W-BR-CSZ                   PIC X(30).
           03  FILLER                     PIC X(15).
           03  W-PHONE-HEAD               PIC X(7).
           03  W-BR-PHONE                 PIC 999/999/9999.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(05).
           03  D-CLASS-1                  PIC 99.
           03  D-HYPHEN-1                 PIC X.
           03  D-ACCTNO-1                 PIC 9(6).
           03  FILLER                     PIC X(70).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(06).
           03  D-PAYDATE                  PIC 99/99/99.
           03  FILLER                     PIC X(2).
           03  D-TRAMT                    PIC $$$$$9.99-     BLANK ZERO.
           03  FILLER                     PIC X(1).
           03  D-REST-OF-LINE.
               05  D-APOTH                PIC $$$$$9.99-     BLANK ZERO.
               05  D-APINT                PIC $$$$$9.99-     BLANK ZERO.
               05  D-APCUR                PIC $$$$$$9.99-    BLANK ZERO.
               05  D-CURBAL               PIC $$$$$$$9.99-.
               05  D-AMOUNT               PIC $$$$$$.99-.
               05  FILLER                 PIC X(5).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(8).
           03  D-BW-NAME                  PIC X(34).
           03  FILLER                     PIC X.
           03  D-T-TEXT                   PIC X(11).
           03  D-T-DATE                   PIC 99/99/99 BLANK ZERO.
           03  FILLER                     PIC X.
           03  D-T-LCHG                   PIC $$9.99 BLANK ZERO.
           03  FILLER                     PIC X.
           03  D-T-AMT                    PIC $$$$$$.99-.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-08                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(44).
           03  D-MESSAGE                  PIC X(30).
           03  FILLER                     PIC X(11).

       01  DETAIL-LINE-09                   REDEFINES DETAIL-LINE.
           03  FILLER         PIC X(43).
           03  D-REBATE-TEXT  PIC X(17).
           03  FILLER         PIC X(10).
           03  D-REBATE-AMT   PIC $$$$$$.99-.

       COPY "LIBFP/PST_COPYXW.CPY".
       COPY "LIBFP/PST_INFOW.CPY".
       COPY "LIBLP/PST610_CMD.CPY".
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
           USE AFTER ERROR PROCEDURE ON PST-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-PC1-FILE.
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTP1-FILE.
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
           CLOSE PST-FILE.
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

           MOVE 01     TO PST-WS-FORM-PAGE-MAX.
           PERFORM PST-FR-FORM-INITIALIZATION.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ LOAD ACCOUNT INFO. }
           PERFORM PST-INFO-MODULE.
           PERFORM PST-COPY-XTRA-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NOTE: NEED TO GET LAST LP-REC BEFORE GOING INTO
      *       PRINT-PAGE-01-ROUTINE (WHICH IS COPY OF RECEIPT-PRINT
      *       IN LONP3W).  COMING OUT OF PST-COPY-XTRA-ROUTINE WE DO
      *       NOT KNOW WHAT LP-REC IS DUE TO ADDITIONAL READS ON THE 
      *       LPFILE SETTING WORKERS.  SO NEED TO EMULATE WHAT LONP3W
      *       DOES WHICH JUST HAS LP-REC SET FROM LPCRNO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM OPEN-LP1-FILE.
           MOVE LN-OWNBR  TO LP-BRNO
                             QLP1-WBEG-BRNO
                             QLP1-WEND-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO
                             QLP1-WBEG-ACCTNO
                             QLP1-WEND-ACCTNO.
           MOVE 99999     TO LP-SEQNO
                             QLP1-WEND-SEQNO.
           MOVE ALL "0"   TO QLP1-WBEG-SEQNO

           PERFORM START-LP1-FILE-NOT-GREATER.
           PERFORM READ-LP1-FILE-PREVIOUS.
           PERFORM CLOSE-LP1-FILE.
           IF ( IO-BAD ) OR
              ( LN-OWNBR  NOT = LP-BRNO   ) OR
              ( LN-ACCTNO NOT = LP-ACCTNO )
              PERFORM CLEAR-LP-FILE.

      * NEED TO GET CORRECT SSNO FROM BW FILE

           IF FM-STMT-COUNTER < 1 OR > 4
              MOVE LN-SSNO(1) TO BW-SSNO
           ELSE
              MOVE LN-SSNO(FM-STMT-COUNTER) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 9
              GO TO PRINT-ROUTINE-EXIT.

      * CREATE NOTICE RECORD AND GET BORROWER RECORD:

           MOVE LN-ACCTNO     TO CRNOR2-ACCTNO.
           MOVE TRANS-DATE    TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE 0 TO HOLD-INTEREST-REBATE.

      * NEED TO SAVE IF YOU HAVE TO GO FIND INTEREST REFUND

           IF LN-INT-REBATE NOT = " " AND LN-ORGST = "IL"
              PERFORM FIND-INTEREST-REBATE.

           MOVE IP-FMNO TO FD-FMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF IO-FG = 9
              GO TO PRINT-ROUTINE-EXIT.

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
           PERFORM PRINT-PAGE-01-ROUTINE.

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

           MOVE BR-CITY TO C-S-Z.
           MOVE BR-STATE TO STATE.
           MOVE BR-ZIP TO ZIP.
           PERFORM CAT-CSZ.

      * CHANGE FONT SIZE TO 12 CPI 
           MOVE 10                     TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.
           PERFORM PST-FR-FONT.
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

           MOVE GB-COMPANY-NAME TO COMPANY-NAME.
           CALL "C$JUSTIFY" USING COMPANY-NAME, "L".
           MOVE COMPANY-NAME TO W-BR-NAME.
           MOVE BR-ADR1 TO W-BR-ADR1.
           MOVE BR-ADR2 TO W-BR-ADR2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE C-S-Z     TO W-BR-CSZ.
           MOVE "PHONE: " TO W-PHONE-HEAD.
           MOVE BR-PHONE  TO W-BR-PHONE.
           INSPECT W-BR-PHONE REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           MOVE TEXT1 TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE LN-CLASS TO D-CLASS-1.
           MOVE "-" TO D-HYPHEN-1.
           MOVE LN-ACCTNO TO D-ACCTNO-1.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           IF IP-FMNO = 6
              MOVE SPACES TO DETAIL-LINE
           ELSE
              MOVE TEXT2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
 
           IF LP-IBPC = "I"
              MOVE "INTEREST" TO TEXT3-C
            ELSE
              MOVE "LATE CHG" TO TEXT3-C.
           IF IP-FMNO = 6
              MOVE TEXT3A TO DETAIL-LINE
           ELSE
              MOVE TEXT3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
 
      * IF VERY LAST LP WAS "OV", GO GET THE PREVIOUS AND PRINT
      * THAT INFO FIRST, THEN PRINT THE "OV" INFO.
           IF LP-TRCD = "OV"
              PERFORM FIND-LAST-PAYMENT-AND-PRINT
           ELSE
              MOVE LP-PAYDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-PAYDATE
              MOVE LP-TRAMT TO D-TRAMT
              MOVE LP-APOTH TO D-APOTH
              ADD LP-APINT LP-APLC GIVING D-APINT
              MOVE LP-APCUR TO D-APCUR
              MOVE LN-CURBAL TO D-CURBAL
              MOVE NO-TDUEAMT TO D-AMOUNT
              IF IP-FMNO = 6
                 MOVE SPACES TO D-REST-OF-LINE
              END-IF
              PERFORM WRITE-DETAIL-LINE.

      * "LA" LAW CHANGE 7/15/98
      * MUST DISCLOSE FIRST TIME LATE CHARGE APPLIED OR ASSESSED
      *  PY  LC 25.72   UP ARROW CHANGE 0    LCBAL = 25.72 APINTOWE = 25.72
      *  PY  LC 32.15   UP ARROW CHANGE 0    LCBAL = 32.15 APINTOWE =  6.43
      *  PY  LC 32.15   UP ARROW CHANGE 5.00 LCBAL = 27.15 APINTOWE =     0

      *  PY LC 25.72  UP ARROW CHANGE 5.00  LCBAL = 20.72 APINTOWE = 25.72

           MOVE "N" TO LCHG-MESS-PRINTED.
      * FIRST APPLIED
           IF IP-FMNO = 2
              IF LP-APLC > 0 
                 IF (LP-APLC = LN-TOTLCHG) AND (LN-LCBAL = 0)
                    MOVE "Y" TO LCHG-MESS-PRINTED
                    MOVE LP-APLC TO T6-LCHG
                    MOVE TEXT6 TO DETAIL-LINE.
      * FIRST ASSESSMENT
           IF IP-FMNO = 2
              IF LP-APINTOWE > 0 
      * THIS 'IF' NEEDED FOR PART APPLIED PART ASSESSED, APINTOWE = TOTAL
                 IF LP-APINTOWE = (LP-LCBAL + LP-APLC)
      * COULD HAVE PART APPLIED AND PART ASSESSED SO ALWAYS ADD
                    MOVE "Y" TO LCHG-MESS-PRINTED
                    MOVE LP-APINTOWE TO T6-LCHG
                    MOVE TEXT6 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *  "MO" INTEREST BEARING LOANS WITH LATE CHARGES  12/27/01
           IF IP-FMNO NOT = 2
              IF LN-LOANTYPE = "I"
                 IF LP-APLC NOT = 0
                    MOVE LP-APLC TO TEXT-APLC
                    MOVE TEXT15 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE NO-AVAILCREDIT TO AV-CREDIT.
           MOVE BR-RECP1 TO T-MESSAGE-1.
           IF BR-AVAIL-CREDIT = "Y" OR "S"
              INSPECT T-MESSAGE-1 REPLACING FIRST "$$$$$$$$$$" 
                 BY R-CREDIT.
           MOVE BR-RECP2 TO T-MESSAGE-2.
           IF BR-AVAIL-CREDIT = "Y" OR "S"
              INSPECT T-MESSAGE-2 REPLACING FIRST "$$$$$$$$$$" 
                 BY R-CREDIT.
              
      * REMOVED THIS 09.21.2017 #1280
      * CLEAR T-MESSAGE-1 AND T-MESSAGE-2 IF WORLD CRITERIA, PR# 806
      *    IF GP-I-AM-WORLD AND LN-CLASS = 5
      *       MOVE LN-LOANDATE TO NUM-DATE
      *       MOVE TRANS-DATE TO SYS-DATE
      *       PERFORM TIM
      * THEY ALWAYS WANT IT ROUNDED UP
      * EXAMPLE 15 * .25 = 3.75 + .5 = 4.25 ROUNDED 4
      * EXAMPLE  5 * .25 = 1.25 + .5 = 1.75 ROUNDED 2
      *       IF LN-ORGST = "TN" OR "IL" OR "KY" OR "MO" OR "WI"
      *          COMPUTE HOLD-AVAIL-MONTHS ROUNDED =
      *                            (LN-ORGTERM * .25) + .5
      *       ELSE
      *          COMPUTE HOLD-AVAIL-MONTHS ROUNDED = 
      *                            (LN-ORGTERM * .30) + .5
      *       END-IF
      * IF CASH AVAILABLE MONTHS ARE GREATER THAN ELAPSED MONTHS, DONT
      * PRINT CASH AVAILABLE 
      * ELAPSED = 2, CASH MONTHS = 5, DONT PRINT
      * ELAPSED = 10, CASH MONTHS = 5, PRINT
      *       IF HOLD-AVAIL-MONTHS > ELAPSED-MONTHS
      *          MOVE SPACES TO T-MESSAGE-1 T-MESSAGE-2.

           MOVE T-MESSAGE-1 TO D-MESSAGE.
      * IF TODAY IS LESS THAN 90 DAYS FROM LOANDATE, DONT PRINT CASH
      * AVAILABILITY MESSAGE FROM BRANCH RECORD FOR SUNBELT...
           IF IP-FMNO = 2
              IF LCHG-MESS-PRINTED = "Y"
                 MOVE TEXT7 TO DETAIL-LINE.
           IF IP-FMNO = 6
              MOVE SPACES TO D-MESSAGE.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE T-MESSAGE-2 TO D-MESSAGE.
      * IF TODAY IS 90 DAYS OR LESS FROM LOANDATE, DONT PRINT CASH
           IF IP-FMNO = 2
              IF LCHG-MESS-PRINTED = "Y"
                 MOVE TEXT8 TO DETAIL-LINE.
           IF IP-FMNO = 6
              MOVE SPACES TO D-MESSAGE.
           PERFORM WRITE-DETAIL-LINE.
     
           MOVE SPACES TO D-BW-NAME.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                  INTO D-BW-NAME.
           IF NOT IP-FMNO = 6
              MOVE "LAST  DUE:" TO D-T-TEXT
              MOVE NO-LDUE-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-T-DATE
              MOVE NO-LDUELC TO D-T-LCHG
              MOVE NO-LDUEAMT TO D-T-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BW-ADR1 TO D-BW-NAME.
           IF NOT IP-FMNO = 6
              MOVE "TODAY DUE:" TO D-T-TEXT
              MOVE NO-TDUE-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-T-DATE
              MOVE NO-TDUEAMT TO D-T-AMT
              MOVE NO-TDUELC TO D-T-LCHG.
           PERFORM WRITE-DETAIL-LINE.

           IF BW-ADR2 NOT = SPACES
              MOVE BW-ADR2 TO D-BW-NAME
              IF NOT IP-FMNO = 6
                 MOVE "NEXT  DUE:" TO D-T-TEXT
                 MOVE NO-NXDUE-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO D-T-DATE
                 MOVE NO-NXDUEAMT TO D-T-AMT
                 MOVE NO-NXDUELC TO D-T-LCHG
                 PERFORM WRITE-DETAIL-LINE
              ELSE
                 PERFORM WRITE-DETAIL-LINE.

           PERFORM RECEIPT-BW-CITY-STATE.
           IF BW-ADR2 NOT = SPACES
              IF HOLD-INTEREST-REBATE NOT = 0
                 MOVE "INTEREST REBATE:" TO  D-REBATE-TEXT
                 MOVE HOLD-INTEREST-REBATE TO D-REBATE-AMT.
           IF BW-ADR2 = SPACES
              IF NOT IP-FMNO = 6
                 MOVE "NEXT  DUE:" TO D-T-TEXT
                 MOVE NO-NXDUE-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO D-T-DATE
                 MOVE NO-NXDUEAMT TO D-T-AMT
                 MOVE NO-NXDUELC TO D-T-LCHG.
           PERFORM WRITE-DETAIL-LINE.

           IF BW-ADR2 = SPACES
              IF HOLD-INTEREST-REBATE NOT = 0
                 MOVE "INTEREST REBATE:" TO  D-REBATE-TEXT
                 MOVE HOLD-INTEREST-REBATE TO D-REBATE-AMT
                 PERFORM WRITE-DETAIL-LINE.

           MOVE NO-AVAILCREDIT TO AV-CREDIT.
           MOVE BR-RECP1 TO T-MESSAGE-1.
           IF BR-AVAIL-CREDIT = "Y"
              INSPECT T-MESSAGE-1 REPLACING FIRST "$$$$$$$$$$" 
                 BY R-CREDIT.
           MOVE BR-RECP2 TO T-MESSAGE-2.
           IF BR-AVAIL-CREDIT = "Y"
              INSPECT T-MESSAGE-2 REPLACING FIRST "$$$$$$$$$$" 
                 BY R-CREDIT.

      * ONLY PRINT AVAILABLE CREDIT MESSAGE HERE IF LATE CHARGE MESSAGE
      * PRINTED UP ABOVE
           IF LCHG-MESS-PRINTED = "Y"
              MOVE T-MESSAGE-1 TO D-MESSAGE
           ELSE
              IF NOT ( LN-OWNBR = 589 OR 497 ) AND
                      (LN-OWNBR NOT > 2999)
                 MOVE TEXT9 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ONLY PRINT AVAILABLE CREDIT MESSAGE HERE IF LATE CHARGE MESSAGE
      * PRINTED UP ABOVE
           IF LCHG-MESS-PRINTED = "Y"
              MOVE T-MESSAGE-2 TO D-MESSAGE
           ELSE
      * WORLD - WISCONSIN - DO NOT PRINT TAX MESSAGE
              IF LN-OWNBR > 1899 AND < 2000
                 MOVE TEXT10-WI TO DETAIL-LINE
              ELSE
              IF LN-OWNBR NOT > 2999
                 MOVE TEXT10 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      * THANK YOU FOR YOUR PAYMENT
           MOVE TEXT5 TO DETAIL-LINE.
           IF LCHG-MESS-PRINTED NOT = "Y"
      * WORLD - WISCONSIN - DO NOT PRINT TAX MESSAGE
              IF LN-OWNBR > 1899 AND < 2000
                 MOVE TEXT12-WI TO DETAIL-LINE
              ELSE
                 IF LN-OWNBR NOT > 2999
                    MOVE TEXT11 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
 
           IF LCHG-MESS-PRINTED NOT = "Y"
              IF (LN-OWNBR NOT > 2999) AND
                  NOT (LN-OWNBR > 1899 AND < 2000)
                 MOVE TEXT12 TO DETAIL-LINE
                 PERFORM WRITE-DETAIL-LINE
                 SUBTRACT 1 FROM FD-BOTTOM.
                
       RECEIPT-BOTTOM.
           MOVE FD-BOTTOM TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *
           PERFORM WRITE-LEGAL-TEXT.
      *

       PRINT-PAGE-01-ROUTINE-END.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-01-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBLP/LPLPCL.CPY".
       COPY "LIBFP/PST_COPYX.CPY".
       COPY "LIBFP/PST_INFO.CPY".

      ************************************
       PST-FORM-MAIN-MODULE SECTION.
      ************************************

           PERFORM SQL-CONNECT.
           PERFORM PST-FORM-INITIALIZATION.

      * CHECK TO SEE IF CROSSING DATA PATHS IS NEEDED
      
           IF ( BRANCH = ZEROES )
              MOVE CROSS-DATA-PATHS---ON   TO CROSS-DATA-PATHS
           ELSE
              MOVE CROSS-DATA-PATHS---OFF  TO CROSS-DATA-PATHS.

           MOVE ZEROES                     TO CROSS-DATA-PATHS-BRNO.

      * VERIFY BATCH PRINT 

           IF ( LOAN-KEY NOT = ZEROES )
              GO TO PST-FORM-READ-PRIMARYS
           ELSE
           IF ( BORROWER-KEY NOT = ZEROES )
              GO TO PST-FORM-READ-PRIMARYS.

           MOVE "Y"                        TO IS-BATCH-PRINT.

           MOVE FM-PATH-OWNBR      TO FM-BRNO
                                      QFM2-WBEG-BRNO
                                      QFM2-WEND-BRNO.
           MOVE IP-FMNO            TO FM-FMNO
                                      QFM2-WBEG-FMNO
                                      QFM2-WEND-FMNO.
           MOVE SPACES             TO FM-ZIP
                                      QFM2-WBEG-ZIP.
           MOVE ALL "Z"            TO QFM2-WEND-ZIP.
           PERFORM START-FM2-FILE.

       PST-FORM-FM2-READ-NEXT.

           MOVE ZEROES                     TO LOAN-KEY.
           PERFORM READ-FM2-FILE-NEXT.
           IF ( IO-BAD ) OR ( FM-BRNO NOT = FM-PATH-OWNBR )
              GO TO PST-FORM-MAIN-MOD-EXIT.

           IF ( FM-FMNO NOT = IP-FMNO )
              GO TO PST-FORM-MAIN-MOD-EXIT.

           IF ( BRANCH  NOT = ZEROES ) AND
              ( FM-BRNO NOT = BRANCH )
              GO TO PST-FORM-FM2-READ-NEXT.

           IF ( PRINT-REPRINT = "P" ) AND
              ( FM-PRINT      = "Y" )
              GO TO PST-FORM-FM2-READ-NEXT.

           IF ( PRINT-REPRINT = "R" ) AND
              ( FM-PRINT  NOT = "Y" )
              GO TO PST-FORM-FM2-READ-NEXT.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO LN-ACCTNO LOAN-KEY.

       PST-FORM-READ-PRIMARYS.

           PERFORM PST-FORM-CROSS-DATA-PATHS.

           IF ( IS-BATCH-PRINT NOT = "Y" )
              IF IP-FMNO = 6 
                 MOVE 1 TO FM-STMT-COUNTER
              ELSE
                 MOVE FM-PATH-OWNBR       TO FM-BRNO
                 MOVE SPACES              TO LPWSFM-EDIT-FM-INITKEY
                 MOVE LOAN-KEY            TO LPWSFM-EDIT-FM-ACCTNO
                 MOVE IP-FMNO             TO LPWSFM-EDIT-FM-FORMNO
                                          FM-FMNO
                 MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY
                 PERFORM READ-FM1-FILE.
      *          IF ( IO-GOOD )
      *             UNLOCK FM-FILE.

       PST-FORM-READ-PRIMARY-LN.

           IF NOT ( PST-PRIMARY-FILE-LN )
              GO TO PST-FORM-READ-PRIMARY-PD.

           IF ( LOAN-KEY = ZEROES )
              GO TO PST-FORM-MAIN-MOD-EXIT.

           MOVE LOAN-KEY                   TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

       PST-FORM-READ-PRIMARY-PD.

           IF NOT ( PST-PRIMARY-FILE-PD )
              GO TO PST-FORM-PROCESS-RECS.

           IF ( BORROWER-KEY = ZEROES )
              GO TO PST-FORM-MAIN-MOD-EXIT.

           MOVE "L"                        TO PD-ID.
           MOVE BRANCH                     TO PD-BRANCH.
           MOVE BORROWER-KEY               TO PD-SOC-SEC.
           PERFORM OPEN-PD1-FILE.
           PERFORM READ-PD1-FILE.
           PERFORM CLOSE-PD1-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE PD-RECORD                  TO LN-REC.
           MOVE PD-SOC-SEC                 TO LN-SSNO(1).

       PST-FORM-PROCESS-RECS.

           MOVE LN-OWNBR                   TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE IP-FMNO                    TO FD-FMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           PERFORM OPEN-GB-FILE.
           MOVE LN-OWNBR TO GB-BRNO.
           MOVE 1        TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.

           MOVE LN-ORGST                   TO SP-ORGST.
           MOVE LN-SPRCLASS                TO SP-SPRCLASS.
           MOVE LN-SUBCLASS                TO SP-SUBCLASS.
           MOVE LN-LAWCODE                 TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           PERFORM PRINT-ROUTINE.

           IF IP-FMNO = 6 
              GO TO PST-FORM-MAIN-MOD-EXIT.

           IF ( IS-BATCH-PRINT = "Y" )
              MOVE FM-BRNO TO HOLD-FM2-BRNO
              MOVE FM-FMNO TO HOLD-FM2-FMNO
              MOVE FM-ZIP  TO HOLD-FM2-ZIP
              MOVE FM1-KEY TO HOLD-FM1-KEY
              PERFORM READ-FM1-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR
              ELSE
                 MOVE "Y"                  TO FM-PRINT
                 PERFORM REWRITE-FM1-FILE
                 GO TO PST-FORM-FM2-READ-NEXT.

           IF ( FM-PRINT = "Y" )
              GO TO PST-FORM-MAIN-MOD-EXIT.

           PERFORM READ-FM1-FILE.
           IF ( IO-GOOD )
              MOVE "Y"                     TO FM-PRINT
              PERFORM REWRITE-FM1-FILE.

           IF ( ERRCD NOT = "L" )
              MOVE SPACES                  TO ERRCD.

       PST-FORM-MAIN-MOD-EXIT.
           EXIT.

      ******************************************************************
       PST-FORM-INITIALIZATION SECTION.
      ******************************************************************

           MOVE FORM-PATHNAME              TO FORM-PATH.

           MOVE 0                          TO Q-POS.
           MOVE 0                          TO C-POS.
           MOVE LONPM-PRU-POS              TO P-POS.
           MOVE "N"                        TO Q-BUF.
           MOVE 00                         TO P-BUF.

           MOVE SPACES                     TO IP-PROG.
           PERFORM GET-GPENV.

           IF BATCH-PR = "Y"
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO TRANS-DATE.

           PERFORM OPEN-FM1-FILE.

           MOVE "PRINT IN PROGRESS................." TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       PST-FORM-INITIAL-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F O R M - C R O S S - D A T A - P A T H S
      *
      *=================================================================
      *    IN  :  CROSS-DATA-PATHS (ON/OFF)
      *           CROSS-DATA-PATHS-BRNO
      *    OUT :  CROSS-DATA-PATHS-BRNO
      *    DESC:  ROUTINE TO SET LOCAL FILE PATHS FOR CROSSING
      *           DATA PATHS IF NEEDED.
      ******************************************************************
       PST-FORM-CROSS-DATA-PATHS SECTION.

           IF ( CROSS-DATA-PATHS-OFF            ) OR
              ( FM-BRNO = CROSS-DATA-PATHS-BRNO )
              GO TO PST-FORM-C-D-PATHS-EXIT.

           MOVE FM-BRNO               TO BR-NO   CROSS-DATA-PATHS-BRNO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              CK-PATH-OVERRIDE
                                              CX-PATH-OVERRIDE
                                              GB-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE
                                              PC-PATH-OVERRIDE
                                              PD-PATH-OVERRIDE.

       PST-FORM-C-D-PATHS-EXIT.
           EXIT.



      ***************************
       WRITE-LEGAL-TEXT SECTION.
      ***************************

      * THESE STATES DONT EVEN SELL INSURANCE, SO SKIP IT

           IF LN-OWNST = "TX" OR "IL"
               GO TO WRITE-LEGAL-EXIT.

           IF TRANS-DATE < 20001009
              GO TO WRITE-LEGAL-EXIT.

           IF TRANS-DATE > 20001115
              GO TO WRITE-LEGAL-EXIT.

           MOVE LEGAL-TEXT1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LEGAL-TEXT2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LEGAL-TEXT3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LEGAL-TEXT4 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LEGAL-TEXT5 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LEGAL-TEXT6 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LEGAL-TEXT7 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LEGAL-TEXT8 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LEGAL-TEXT9 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 11 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM UPDATE-MEMO-RECORD.

       WRITE-LEGAL-EXIT.
           EXIT.
         
      ************************************
       FIND-LAST-PAYMENT-AND-PRINT SECTION.
      ************************************
           PERFORM OPEN-LP1-FILE.
           MOVE LN-OWNBR  TO LP-BRNO
                             QLP1-WBEG-BRNO
                             QLP1-WEND-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO
                             QLP1-WBEG-ACCTNO
                             QLP1-WEND-ACCTNO.
           MOVE 99999     TO LP-SEQNO
                             QLP1-WEND-SEQNO.
           MOVE ALL "0"   TO QLP1-WBEG-SEQNO
           PERFORM START-LP1-FILE-NOT-GREATER.
      * ALREADY GOT THIS ONE, ITS AN "OV"
           PERFORM READ-LP1-FILE-PREVIOUS.
      * NOW GET THE PREVIOUS ONE
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0 OR
               LN-OWNBR NOT = LP-BRNO OR
               LN-ACCTNO NOT = LP-ACCTNO
                  MOVE SPACES TO DETAIL-LINE
                  PERFORM WRITE-DETAIL-LINE
                  GO TO EXIT-FIND-LAST-PAYMENT.

           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PAYDATE.
           MOVE LP-TRAMT TO D-TRAMT.
           MOVE LP-APOTH TO D-APOTH.
           ADD LP-APINT LP-APLC GIVING D-APINT.
           MOVE LP-APCUR TO D-APCUR.
           MOVE LN-CURBAL TO D-CURBAL.
           MOVE NO-TDUEAMT TO D-AMOUNT.
           IF IP-FMNO = 6
              MOVE SPACES TO D-REST-OF-LINE.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-FIND-LAST-PAYMENT.
      * LEAVE THE PAYOFF LINE IN THE LP BUFFER FOR LATER PRINTS
           PERFORM CLOSE-LP1-FILE.

      ************************************
       FIND-INTEREST-REBATE SECTION.
      ************************************

      * ONLY LOOKING IF LN-INT-REBATE IS SET

           MOVE 0 TO HOLD-INTEREST-REBATE.

           PERFORM OPEN-LP1-FILE.

           MOVE LN-OWNBR  TO LP-BRNO
                             QLP1-WBEG-BRNO
                             QLP1-WEND-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO
                             QLP1-WBEG-ACCTNO
                             QLP1-WEND-ACCTNO.
           MOVE 99999     TO LP-SEQNO
                             QLP1-WEND-SEQNO.
           MOVE ALL "0"   TO QLP1-WBEG-SEQNO
           PERFORM START-LP1-FILE-NOT-GREATER.
       FIND-INTEREST-REBATE-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0 OR
              (LN-OWNBR NOT = LP-BRNO) OR (LN-ACCTNO NOT = LP-ACCTNO)
              GO TO EXIT-FIND-INTEREST-REBATE.

           IF LP-TRCD NOT = "RI"
              GO TO FIND-INTEREST-REBATE-NEXT.

           IF LP-REV = "Y"
              GO TO FIND-INTEREST-REBATE-NEXT.

           IF LP-TRDATE NOT = TRANS-DATE
              GO TO FIND-INTEREST-REBATE-NEXT.

      * ONLY HERE IF YOU ARE AN "RI" AND NOT REVERSED
           COMPUTE HOLD-INTEREST-REBATE = HOLD-INTEREST-REBATE
                                        + LP-TRAMT.

           GO TO FIND-INTEREST-REBATE-NEXT.

       EXIT-FIND-INTEREST-REBATE.
           EXIT.

      ******************************************************************
       UPDATE-MEMO-RECORD SECTION.
      ******************************************************************

           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO SAVE-LM-BRNO.
           MOVE FM-INITKEY    TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - TRANS-DATE.
           MOVE 1             TO SAVE-LM-SEQ.

       CREATE-MEMO.

           MOVE SAVE-LM1-KEY TO LM1-KEY.
           PERFORM READ-LM1-FILE.
           IF ( IO-FG = 0 )
              IF ( LM-SEQ < 999 )
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO CREATE-MEMO
              ELSE
                 GO TO UPDATE-MEMO-EXIT.

           MOVE " "          TO LM-REC.
           MOVE SAVE-LM1-KEY TO LM1-KEY.
           MOVE IP-FMNO     TO MEMO-FMNO.
           MOVE "RECEIPT WITH LEGAL DISCLOSURE" TO MEMO-DESC.
           MOVE " "          TO MEMO-CHG-CD.
                        
           MOVE SAVE-LM-MEMO TO LM-MEMO.
           PERFORM WRITE-LM1-FILE.

       UPDATE-MEMO-EXIT.
           PERFORM CLOSE-LM1-FILE.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.
       RECEIPT-BW-CITY-STATE.
           MOVE BW-CITY TO MESS.
           INSPECT MESS REPLACING FIRST "   " BY ", +".
           INSPECT MESS REPLACING FIRST "+ " BY BW-STATE.
           INSPECT MESS REPLACING FIRST "            "
              BY "  ++++++++++".
           INSPECT MESS REPLACING FIRST "++++++++++" BY BW-ZIP.
           MOVE MESS TO D-BW-NAME.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPPCGS_SQL.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
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
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
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
