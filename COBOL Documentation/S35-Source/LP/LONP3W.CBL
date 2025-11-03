       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONP3W.
       DATE-WRITTEN.    062593.
      ******************************************************************
      *
      *                 (LP)
      *  DESCRIPTION:   PAYMENT RECEIPT PRINT
      *                 NOTE: NO FOLLOW UP PORTION PRINTS
      *      21 LINES EACH RECEIPT
      *      BODY 17  TOP 1 BOTTOM 3  CONT "Y"
      *
      * *** NOTE ***
      *       FMNO = 2 = WORLD "LA", PRINTS LCHG ASSESSED MESSAGE
      *       FMNO = 6 = WORLD 3RD PARTY, OMITS SOME FIELDS
      *
      *
      * PRINTS BR NAME, ADDR, CSZ AND PHONE ABOVE ACCT #
      *
      *=================================================================
      * REV:
      *  062593 DID FOR WORLD, TN   SAME AS 15
      *  101794 BAH REMOVED ALL OCCURANCES OF LP2
      *  081596 BAH ADDED TEST OF BR-AVAIL-CREDIT = "S" FOR WDT (355)
      *  JTG 072897 ADDED LPWDEL/W  MERCURY #E135
      *  GLF 090297 REMOVED ALL USE OF GLOBAL
      *  GLF 101697 ADDED COPY MEMBER: LIBLP/LPPDUE LIBLP/LPPDUEW
      *  JTG 111197 ADDED LOGIC FOR LIBLP/DEFPOL RE: BR-NODEF-1ST-X-MONTHS #167
      *  BAH 032698 ADDED WORLDS ORIGINAL CODE BACK IN, LAST, TODAY, AND
      *             NEXT DUE
      *  BAH 001010 I HAD THE STUPID DATES BACKWARDS FOR ELAPSED-DAYS...ABOVE
      *  CS  011112 THIS RECEIPT PRINTED IN A STANDARD ENVELOPE IN A48.  ADDED
      *             TEST OF UUCP-HOME TO PRINT THE BORROWER NAME & ADDRESS INFO
      *             INDENTED TO FIT STANDARD ENVELOPE [ANN ARBOR]
      *  CS  011206 AFTER ABOVE CHANGE WAS NOT PRINTING BW-ADR2 FOR ANYBODY
      *             BUT WORLD
      *  BLV 020822 WAS NOT PRINTING TEXT5 (THANK YOU FOR YOU PAYMENT) AFTER
      *             ABOVE CHANGE, BF MOTOR CREDIT
      *  BAH 040916 IF "OV" WAS LAST PAYMENT, GET PRIOR PAYMENT TO THAT
      *             REGWAN, LORI, BETH
      *  BAH 041217 PRINT INCOME TAX MESSAGE IF BRANCH 467, WORLD
      *  BAH 050103 PRINT INCOME TAX MESSAGE IF BRANCH 606, WORLD
      *  BAH 050825 PRINT INCOME TAX MESSAGE IF BRANCH 438, WORLD
      *  BAH 050920 DO NOT PRINT INCOME TAX MESSAGE FOR MEXICO
      *               (LN-OWNBR NOT > 2999)
      *  BAH 060103 REMOVED BRANCH 576 FROM LIST OF BRANCHES WHO CANT
      *             DO TAXES...WORLD PR# 448
      *  BAH 070724 BRANCH 497 AND 589 CAN NOT DO TAXES, WORLD PR# 548
      *  BAH 090112 ADDED LET US DO YOUR TAXES AND WEBSITE FOR REGMGM PR# 3306
      *  BAH 100618 CHANGED IMPORTANT NOTICE AND REMOVED 497 AND 589 BRANCHES
      *             TO NOT BE SKIPPED ANYMORE, WORLD PR# 694
      *  BAH 110105 DO NOT PRINT TAX MESSAGE FOR "WI" WORLD PR# 735
      *  BAH 120517 SUPPRESS PRINTING OF CASH AVAILABLE FOR WORLD CLASS 5
      *             IF LOAN IS NOT AT LEAST XX% INTO THE TERM OF THE LOAN PR#806
      *  BAH 121204 REMOVED LET US DO YOUR TAXES FOR REGMGM, PR# 4165
      *  BAH 130731 ADDED IP-FMNO 6 FOR WORLD 3RD PARTY RECEIPT, WORLD PR# 915
      *             OMITS PRINTING OF SOME FIELDS
      *  BAH 130930 WORLD WANTS ADR2 ON LINE 1, MOVE CSZ TO LINE 2 WITH PHONE
      *             JUST LIKE REGMGM ALREADY DOES, WORLD PR# 935
      *  BAH 150803 CUSTOM CHANGES FOR CASHCO, CASHCO PR# 4818
      *  BAH 150819 REMOVE DASHED LINE FOR CASHCO, CASHCO PR# 4840
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  KEC 2016.0211 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *                FIXED KEYS & RECS FOR FD-REC.
      *                CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *  BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      *  BAH 161103 MOVED READ-BW1-FILE ABOVE CREATE-NOTICE SO AVAIL CREDIT
      *             HAS THE BW-LAST-RN-FOR10 DATE WORLD PR#1199
      *  BAH 170921 REMOVE THE SUPPRESS PRINTING OF CASH AVAILABLE FOR WORLD
      *             CLASS 5 IF LOAN IS NOT AT LEAST XX% INTO THE TERM OF
      *             THE LOAN, ORIG PR#806, THIS PR#1280
      * 20171101 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181114 GLOBAL FMFILE
      * BAH 181128 GLOBAL LMFILE
      * BAH 181210 GLOBAL LNFILE
      * 20190130 BAH ADDED READ PREVIOUS OF LPFILE, USED TO TAKE PLACE
      *              IN CRNORC CREATE-NOTICE BUT CHANGED THAT TO CRNOR2 AND
      *              THE PAYMENT RECORD DOES NOT GET PASSED.
      * BAH 190313 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      *  BAH 2022.0711 IF ACCOUNT IS "IL" ORIGINATED, PRINT INTEREST REBATE
      *                ONLY IF ONE EXIST. #1559
      * BAH 20220823 HARD CODED START
      * BAH 20240202 REMOVED HARD CODED MEXICO BRANCH TESTING >2999
      * BAH 2024.0322 DONT PRINT RECEIPT FOR LP-SEQNO 1, THEY WERE PRINTING
      *               RECEIPTS ON NEW LOANS AND POCKETING MONEY #1651A
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/LONP3W S35 10/12/22-11:41:26 >".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

       01  HOLD-INTEREST-REBATE PIC S9(6)V99 VALUE 0.

       01  SAVE-LM1-KEY         VALUE SPACES.
           03  SAVE-LM-BRNO     PIC 9(4).
           03  SAVE-LM-ACCTNO   PIC 9(8).
           03  SAVE-LM-DATE     PIC 9(8).
           03  SAVE-LM-SEQ      PIC 9(3).

       01  SAVE-LM-MEMO.
           03  FILLER           PIC X(7)  VALUE "FORM # ".
           03  MEMO-FMNO        PIC 9(4)  VALUE 0.
           03  FILLER           PIC X(3)  VALUE ",  ".
           03  MEMO-DESC        PIC X(25) VALUE SPACES.
           03  MEMO-CHG-CD      PIC X(22) VALUE SPACES.

       01  LCHG-MESS-PRINTED    PIC X VALUE "N".
       01  C-S-Z                PIC X(85).
       01  STATE                PIC XX.
       01  ZIP                  PIC X(10).
       01  HOLD-TRANS-DATE      PIC 9(8)     COMP-3 VALUE 0.
       01  SUB                  PIC 9(6)     COMP-3 VALUE 0.
       01  SUB1                 PIC 9(6)     COMP-3 VALUE 0.
       01  COMPANY-NAME         PIC X(40).

       01  IS-BATCH-PRINT       PIC X        VALUE SPACES.
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".
       01  HOLD-FM2-KEY.
           03  HOLD-FM2-BRNO PIC 9(4).
           03  HOLD-FM2-FMNO PIC 9(4).
           03  HOLD-FM2-ZIP  PIC X(15).
       01  HOLD-FM1-KEY         PIC X(28).
       01  AV-CREDIT            PIC $(6)9.99.
       01  R-CREDIT REDEFINES AV-CREDIT
                                PIC X(10).
       01  T-MESSAGE-1          PIC X(30).
       01  T-MESSAGE-2          PIC X(30).

      **************************************
      *        RECEIPT VARIABLE TEXT       *
      **************************************
       01  TEXT1.
           03  FILLER           PIC X(1)     VALUE SPACES.
           03  FILLER           PIC X(9)     VALUE
               "ACCOUNT #".

       01  TEXT2.
           03  FILLER           PIC X(26)    VALUE SPACES.
           03  FILLER           PIC X(27)    VALUE
               "-------- APPLIED ---------".
           03  FILLER           PIC X(8)     VALUE SPACES.
           03  FILLER           PIC X(15)    VALUE "NEW      PAST".

       01  TEXT3.
           03  FILLER           PIC X(3)     VALUE SPACES.
           03  TEXT3-A          PIC X(53)    VALUE
               "DATE      PAYMENT      OTHER  INTEREST  PRINCIPAL".
           03  TEXT3-B          REDEFINES TEXT3-A.
               05  FILLER       PIC X(30).
               05  TEXT3-C      PIC X(8).
           03  FILLER           PIC X(1)     VALUE SPACES.
           03  FILLER           PIC X(17)    VALUE
               "BALANCE       DUE".

       01  TEXT3A.
           03  FILLER           PIC X(3)     VALUE SPACES.
           03  FILLER           PIC X(77)    VALUE
               "DATE      PAYMENT".

       01  TEXT5.
           03  FILLER           PIC X(26)    VALUE SPACES.
           03  FILLER           PIC X(26)    VALUE
               "THANK YOU FOR YOUR PAYMENT".
       01  TEXT6.
           03  FILLER           PIC X(5)     VALUE SPACES.
           03  FILLER           PIC X(40)    VALUE
               "PLEASE BE ADVISED THAT A LATE CHARGE OF ".
           03  T6-LCHG          PIC $$$9.99 BLANK ZERO.
           03  FILLER           PIC X(18)     VALUE
               " HAS BEEN ASSESSED".
       01  TEXT7.
           03  FILLER           PIC X(5)     VALUE SPACES.
           03  FILLER           PIC X(42)    VALUE
               "TO YOUR ACCOUNT. THIS CHARGE WAS ASSESSED ".
           03  FILLER           PIC X(22)    VALUE
               "BECAUSE YOUR SCHEDULED".
       01  TEXT8.
           03  FILLER           PIC X(5)     VALUE SPACES.
           03  FILLER           PIC X(50)    VALUE
               "PAYMENT BECAME TEN OR MORE DAYS PAST DUE.".

      * WORLD
       01  TEXT9.
           03  FILLER           PIC X(29)    VALUE  SPACES.
           03  FILLER           PIC X(51)    VALUE
               "***IMPORTANT NOTICE***".

       01  TEXT10.
           03  FILLER           PIC X(44)    VALUE
               "   NOW OFFERING MANY DIFFERENT SIZE LOANS TO".
           03  FILLER           PIC X(35)    VALUE
               " ASSIST WITH YOUR FINANCIAL NEEDS  ".
       01  TEXT11.
           03  FILLER           PIC X(44)    VALUE
               "   AS WELL AS YEAR-ROUND TAX PREPARATION AND".
           03  FILLER           PIC X(35)    VALUE
               " ELECTRONIC FILING. PLEASE TELL    ".
       01  TEXT12.
           03  FILLER           PIC X(46)    VALUE
               "   YOUR FRIENDS! WE APPRECIATE YOUR BUSINESS.".

       01  TEXT10-WI.
           03  FILLER           PIC X(44)    VALUE
               "   NOW OFFERING MANY DIFFERENT SIZE LOANS TO".
           03  FILLER           PIC X(35)    VALUE
               " ASSIST WITH YOUR FINANCIAL NEEDS. ".
       01  TEXT12-WI.
           03  FILLER           PIC X(47)    VALUE
               "   PLEASE TELL YOUR FRIENDS! WE APPRECIATE YOUR".
           03  FILLER           PIC X(10)    VALUE
               " BUSINESS.".

       01  TEXT15.
           03  FILLER           PIC X(15)    VALUE  " ".
           03  FILLER           PIC X(38)    VALUE
               " *** INTEREST INCLUDES LATE CHARGE OF ".
           03  TEXT-APLC        PIC $$$.99.
           03  FILLER           PIC X(5)     VALUE "  ***".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  SAVE-LN-FEED     PIC 99       VALUE 0.

      *********************************************
      *      R E C E I P T   B U F F E R          *
      *********************************************

       01  DETAIL-LINE          VALUE SPACES.
           03  PRINT-AREA       PIC X(80).

           03  LINE1-WORLD REDEFINES PRINT-AREA.
               05  FILLER       PIC X.
               05  W-BR-NAME    PIC X(28).
               05  FILLER       PIC XX.
               05  W-BR-ADR1    PIC X(24).
               05  FILLER       PIC X.
               05  W-BR-ADR2    PIC X(24).

           03  LINE2-WORLD REDEFINES PRINT-AREA.
               05  FILLER       PIC X(6).
               05  W-BR-CSZ     PIC X(30).
               05  FILLER       PIC X(15).
               05  W-PHONE-HEAD PIC X(7).
               05  W-BR-PHONE   PIC 999/999/9999.

           03  LINE1 REDEFINES PRINT-AREA.
               05  FILLER       PIC X.
               05  D-CLASS-1    PIC 99.
               05  D-HYPHEN-1   PIC X.
               05  D-ACCTNO-1   PIC 9(6).
               05  FILLER       PIC X(70).

           03  LINE2 REDEFINES PRINT-AREA.
               05  FILLER       PIC X.
               05  D-PAYDATE    PIC 99/99/99.
               05  FILLER       PIC X(2).
               05  D-TRAMT      PIC $$$$$9.99-     BLANK ZERO.
               05  FILLER       PIC X(1).
               05  D-REST-OF-LINE.
                   07  D-APOTH      PIC $$$$$9.99-     BLANK ZERO.
                   07  D-APINT      PIC $$$$$9.99-     BLANK ZERO.
                   07  D-APCUR     PIC $$$$$$9.99-    BLANK ZERO.
                   07  D-CURBAL     PIC $$$$$$$9.99-.
                   07  D-AMOUNT     PIC $$$$$$.99-.
                   07  FILLER       PIC X(5).

           03  LINE3 REDEFINES PRINT-AREA.
               05  FILLER       PIC X(3).
               05  D-BW-NAME    PIC X(34).
               05  FILLER       PIC X.
               05  D-T-TEXT     PIC X(11).
               05  D-T-DATE     PIC 99/99/99 BLANK ZERO.
               05  FILLER       PIC X.
               05  D-T-LCHG     PIC $$9.99 BLANK ZERO.
               05  FILLER       PIC X.
               05  D-T-AMT      PIC $$$$$$.99-.

           03  LINE4 REDEFINES PRINT-AREA.
               05  FILLER       PIC X(39).
               05  D-MESSAGE    PIC X(30).
               05  FILLER       PIC X(11).

           03  LINE5 REDEFINES PRINT-AREA.
               05  FILLER         PIC X(38).
               05  D-REBATE-TEXT  PIC X(17).
               05  FILLER         PIC X(10).
               05  D-REBATE-AMT   PIC $$$$$$.99-.

      *********************************************

       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/PHONE.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LONP3W_CMD.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPF_DEF.CPY".
       COPY "LIBLP/LONPM_DEF.CPY".
       COPY "LIBLP/LONPF_WKS.CPY".
       COPY "LIBLP/LONPM_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/GETPRW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPF_SCN.CPY".
       COPY "LIBLP/LONPM_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME PROG-BUF IP-BUF PRU-PATH EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-FM2-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           IF NEW-PROG = "M0"
              MOVE "LONPM" TO VDU-FORM-NAME
           ELSE
              MOVE "LONPF" TO VDU-FORM-NAME.

           IF BATCH-PR = "Y"
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO TRANS-DATE.

           MOVE SPACES TO IP-PROG PR-REC.

           MOVE "RECEIPT PRINT IN PROGRESS........." TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-FM1-FILE.

           MOVE 0 TO Q-POS.
           MOVE 0 TO C-POS.
           MOVE LONPM-PRU-POS TO P-POS.
           MOVE "N" TO Q-BUF.
           MOVE 00 TO P-BUF.
           PERFORM OPEN-PR-FILE.

           MOVE TRANS-DATE TO HOLD-TRANS-DATE.
       VERIFY-BATCH-PRINT.
           IF LOAN-KEY NOT = 0
              GO TO READ-PRIMARY-FILES.

           MOVE "Y" TO IS-BATCH-PRINT.

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
       READ-FM2FILE-NEXT.
           MOVE 0 TO LOAN-KEY.
           PERFORM READ-FM2-FILE-NEXT.
           IF IO-FG NOT = 0 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO END-ROUTINE.

           IF FM-FMNO NOT = IP-FMNO
              GO TO END-ROUTINE.
           IF BRANCH NOT = 0
              IF FM-BRNO NOT = BRANCH
                 GO TO READ-FM2FILE-NEXT.
           IF PRINT-REPRINT = "P"
              IF FM-PRINT = "Y"
                 GO TO READ-FM2FILE-NEXT.
           IF PRINT-REPRINT = "R"
              IF FM-PRINT NOT = "Y"
                 GO TO READ-FM2FILE-NEXT.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO LN-ACCTNO LOAN-KEY.

       READ-PRIMARY-FILES.
           IF IS-BATCH-PRINT NOT = "Y"
              IF IP-FMNO = 6 
                 MOVE 1 TO FM-STMT-COUNTER
              ELSE
                 MOVE FM-PATH-OWNBR          TO FM-BRNO
                 MOVE SPACES                 TO LPWSFM-EDIT-FM-INITKEY
                 MOVE LOAN-KEY               TO LPWSFM-EDIT-FM-ACCTNO
                 MOVE IP-FMNO                TO LPWSFM-EDIT-FM-FORMNO
                                                FM-FMNO
                 MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY
                 PERFORM READ-FM1-FILE
                 IF IO-FG NOT = 0
                    GO TO END-ROUTINE.
      *          ELSE
      *             UNLOCK FM-FILE.

           MOVE LOAN-KEY TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           PERFORM OPEN-GB-FILE.
           MOVE LN-OWNST TO GB-BRNO.
           MOVE 1        TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF FM-STMT-COUNTER < 1 OR > 4
              MOVE LN-SSNO(1) TO BW-SSNO
           ELSE
              MOVE LN-SSNO(FM-STMT-COUNTER) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.

      * CREATE NOTICE RECORD AND GET BORROWER RECORD:
           MOVE LN-ACCTNO     TO CRNOR2-ACCTNO.
           MOVE TRANS-DATE    TO CRNOR2-SYSDATE.
           MOVE "N"           TO CRNOR2-LPREC-FG.
           MOVE "CL/CRNOR2"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE 0 TO HOLD-INTEREST-REBATE.
      * NEED TO SAVE IF YOU HAVE TO GO FIND INTEREST REFUND
           IF LN-INT-REBATE NOT = " " AND LN-ORGST = "IL"
              PERFORM FIND-INTEREST-REBATE.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           PERFORM READ-LP1-FILE-PREVIOUS.
           PERFORM CLOSE-LP1-FILE.
           IF IO-FG NOT = 0 OR LP-BRNO NOT = LN-OWNBR OR
                                  LP-ACCTNO NOT = LN-ACCTNO
              MOVE SPACES TO LP-REC.

      * DO NOT ALLOW RECEIPTS FOR NEW LOANS ONLY. MUST POST A TRANSACTION
           IF LP-SEQNO = 1
              GO TO END-ROUTINE.

           MOVE IP-FMNO TO FD-FMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           PERFORM RECEIPT-PRINT.

           IF IP-FMNO = 6 
              GO TO END-PROGRAM.

           IF IS-BATCH-PRINT = "Y"
              MOVE FM-BRNO TO HOLD-FM2-BRNO
              MOVE FM-FMNO TO HOLD-FM2-FMNO
              MOVE FM-ZIP  TO HOLD-FM2-ZIP
              MOVE FM1-KEY TO HOLD-FM1-KEY
              PERFORM READ-FM1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
               ELSE
                 MOVE "Y" TO FM-PRINT
                 PERFORM REWRITE-FM1-FILE
                 GO TO READ-FM2FILE-NEXT.

           IF FM-PRINT = "Y"
              GO TO END-PROGRAM.

           PERFORM READ-FM1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "Y" TO FM-PRINT.
           PERFORM REWRITE-FM1-FILE.

           IF ERRCD NOT = "L"
              MOVE SPACES TO ERRCD.
       END-PROGRAM.
           GO TO END-ROUTINE.

      *********************************
       RECEIPT-PRINT SECTION.
      *********************************

           MOVE BR-CITY TO C-S-Z.
           MOVE BR-STATE TO STATE.
           MOVE BR-ZIP TO ZIP.
           PERFORM CAT-CSZ.

      * WORLD WANTS BR-ADR1 AND BR-ADR2 ON LINE 1 WITH COMPANY NAME THEN
      *       PRINT CSZ AND PHONE ON NEXT LINE
           MOVE GB-COMPANY-NAME TO COMPANY-NAME.
           CALL "C$JUSTIFY" USING COMPANY-NAME, "L".
           MOVE COMPANY-NAME TO W-BR-NAME.
           MOVE BR-ADR1 TO W-BR-ADR1.
           MOVE BR-ADR2 TO W-BR-ADR2.

           IF CONT-PRINT = "1"
              MOVE 2 TO LN-FEED
           ELSE
              MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE C-S-Z TO W-BR-CSZ.
           MOVE "PHONE: " TO W-PHONE-HEAD.
           MOVE BR-PHONE TO W-BR-PHONE.
           INSPECT W-BR-PHONE REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           MOVE TEXT1 TO PRINT-AREA.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE LN-CLASS TO D-CLASS-1.
           MOVE "-" TO D-HYPHEN-1.
           MOVE LN-ACCTNO TO D-ACCTNO-1.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           IF IP-FMNO = 6
              MOVE SPACES TO PRINT-AREA
           ELSE
              MOVE TEXT2 TO PRINT-AREA.
           PERFORM WRITE-DETAIL-LINE.
 
           IF LP-IBPC = "I"
              MOVE "INTEREST" TO TEXT3-C
            ELSE
              MOVE "LATE CHG" TO TEXT3-C.
           IF IP-FMNO = 6
              MOVE TEXT3A TO PRINT-AREA
           ELSE
              MOVE TEXT3 TO PRINT-AREA.
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
                    MOVE TEXT6 TO PRINT-AREA.
      * FIRST ASSESSMENT
           IF IP-FMNO = 2
              IF LP-APINTOWE > 0 
      * THIS 'IF' NEEDED FOR PART APPLIED PART ASSESSED, APINTOWE = TOTAL
                 IF LP-APINTOWE = (LP-LCBAL + LP-APLC)
      * COULD HAVE PART APPLIED AND PART ASSESSED SO ALWAYS ADD
                    MOVE "Y" TO LCHG-MESS-PRINTED
                    MOVE LP-APINTOWE TO T6-LCHG
                    MOVE TEXT6 TO PRINT-AREA.
           PERFORM WRITE-DETAIL-LINE.

      *  "MO" INTEREST BEARING LOANS WITH LATE CHARGES  12/27/01
           IF IP-FMNO NOT = 2
              IF LN-LOANTYPE = "I"
                 IF LP-APLC NOT = 0
                    MOVE LP-APLC TO TEXT-APLC
                    MOVE TEXT15 TO PRINT-AREA.
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
              
           MOVE T-MESSAGE-1 TO D-MESSAGE.
           IF IP-FMNO = 2
              IF LCHG-MESS-PRINTED = "Y"
                 MOVE TEXT7 TO PRINT-AREA.
           IF IP-FMNO = 6
              MOVE SPACES TO D-MESSAGE.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE T-MESSAGE-2 TO D-MESSAGE.
           IF IP-FMNO = 2
              IF LCHG-MESS-PRINTED = "Y"
                 MOVE TEXT8 TO PRINT-AREA.
           IF IP-FMNO = 6
              MOVE SPACES TO D-MESSAGE.
           PERFORM WRITE-DETAIL-LINE.
     
           MOVE SPACES TO D-BW-NAME
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
                 MOVE NO-NXDUELC  TO D-T-LCHG.
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
              IF NOT ( LN-OWNBR = 589 OR 497 )
                 MOVE TEXT9 TO PRINT-AREA.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ONLY PRINT AVAILABLE CREDIT MESSAGE HERE IF LATE CHARGE MESSAGE
      * PRINTED UP ABOVE
           IF LCHG-MESS-PRINTED = "Y"
              MOVE T-MESSAGE-2 TO D-MESSAGE
           ELSE
      * WORLD - WISCONSIN - DO NOT PRINT TAX MESSAGE
              IF LN-OWNBR > 1899 AND < 2000
                 MOVE TEXT10-WI TO PRINT-AREA
              ELSE
                 MOVE TEXT10 TO PRINT-AREA.
           PERFORM WRITE-DETAIL-LINE.

      * THANK YOU FOR YOUR PAYMENT
           MOVE TEXT5 TO PRINT-AREA.
           IF LCHG-MESS-PRINTED NOT = "Y"
      * WORLD - WISCONSIN - DO NOT PRINT TAX MESSAGE
              IF LN-OWNBR > 1899 AND < 2000
                 MOVE TEXT12-WI TO PRINT-AREA
              ELSE
                 MOVE TEXT11 TO PRINT-AREA.
           PERFORM WRITE-DETAIL-LINE.
 
           IF LCHG-MESS-PRINTED NOT = "Y"
              IF (LN-OWNBR > 1899 AND < 2000)
                 MOVE TEXT12 TO PRINT-AREA
                 PERFORM WRITE-DETAIL-LINE
                 SUBTRACT 1 FROM FD-BOTTOM.
                
           MOVE FD-BOTTOM TO LN-FEED.
           MOVE SPACES TO PRINT-AREA.
           PERFORM WRITE-DETAIL-LINE.

            PERFORM WRITE-LEGAL-TEXT.

       RECEIPT-PRINT-EXIT.
           EXIT.

      ************************************
       FIND-INTEREST-REBATE SECTION.
      ************************************

      * ONLY LOOKING IF LN-INT-REBATE IS SET

           MOVE 0 TO HOLD-INTEREST-REBATE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

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

      ************************************
       FIND-LAST-PAYMENT-AND-PRINT SECTION.
      ************************************
           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

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

           MOVE SPACES       TO LM-REC.
           MOVE SAVE-LM1-KEY TO LM1-KEY.
           MOVE IP-FMNO      TO MEMO-FMNO.
           MOVE "RECEIPT WITH LEGAL DISCLOSURE" TO MEMO-DESC.
           MOVE SPACES       TO MEMO-CHG-CD.

           MOVE SAVE-LM-MEMO TO LM-MEMO.
           PERFORM WRITE-LM1-FILE.

       UPDATE-MEMO-EXIT.
           PERFORM CLOSE-LM1-FILE.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
       WRITE-DET-LINE.
           IF LN-FEED > 15
              MOVE SPACES TO PR-REC
              MOVE LN-FEED TO SAVE-LN-FEED
              MOVE 15 TO LN-FEED
              PERFORM WRITE-PR-REC
              SUBTRACT 15 FROM SAVE-LN-FEED GIVING LN-FEED
              GO TO WRITE-DET-LINE.
           IF CONT-PRINT NOT = "1"
              MOVE DETAIL-LINE TO PR-REC-CUTSHEET
            ELSE
              MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE LONPM-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPM-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPM_EVA.CPY".
              COPY "LIBLP/LONPF_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       FORM-RESET.
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".
       FORM-RESET.
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

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPFM2RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************
       END-ROUTINE SECTION.
      ****************************************
           MOVE HOLD-TRANS-DATE TO TRANS-DATE.
           PERFORM CLOSE-FILES.
           PERFORM PRU-RESET.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
