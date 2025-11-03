       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BANKRT.
       DATE-WRITTEN.    10/14/2008.
      *****************************************************************
      *                      (LP)
      *    DESCRIPTION:  BANKRUPTCY REPORT
      *
      * REV:
      *  081014 BAH NEW REGACC PR# 223
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 170508 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181203 GLOBAL LNFILE
      *  BAH 20190930 REMOVED ACCESS-CALL LNFILE
      *  BAH 20200827 REMOVED RESET ON BR1 
      *  BAH 2023.1117 REMOVED CRNOR2 AND ONLY PERFORMED WHAT WAS NEEDED
      *****************************************************************
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

      *******************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/BANKRT.CBL J35 12/21/23-08:43:28 >".
      *******************************
       77  SCCS-IDENTIFIER PIC X(56) VALUE
           "@(#)LP/BANKRT.C A15 10/14/08-16:15:51 NOT-YET-COMPILED >".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

       01  TOTAL-CURBAL    PIC 9(15)V99 COMP VALUE ZERO.
       01  GRAND-CURBAL    PIC 9(15)V99 COMP VALUE ZERO.
       01  TOTAL-COUNT     PIC 9(15) COMP VALUE ZERO.
       01  GRAND-COUNT     PIC 9(15) COMP VALUE ZERO.
       01  LN-FEED         PIC 99       VALUE 0.
       01  LN-COUNT        PIC 99       VALUE 61.
       01  LN-WK           PIC 99       VALUE 0.
       01  PAGE-NUMBER     PIC 9(4)     VALUE 0.

       01  WS-AMOUNT       PIC S9(7)V99 COMP-3 VALUE 0.
       01  TODAYS-DATE          PIC 9(8).
       01  TEST-NAME            PIC X(4)     VALUE SPACES.
       01  DEALER-FG            PIC X(5)     VALUE "     ".
           88  DEALER-BEG                    VALUE "DLBEG".
           88  DEALER-END                    VALUE "DLEND".

       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  BEG-CLASS              PIC 9(3).
           03  END-CLASS              PIC 9(3).
           03  BEG-LNAME              PIC X(4).
           03  END-LNAME              PIC X(4).
           03  BEG-DEALER             PIC 9(4)V99.
           03  END-DEALER             PIC 9(4)V99.
           03  BEG-DAY                PIC 9(2).
           03  END-DAY                PIC 9(2).
           03  BEG-BAL                PIC 9(6).
           03  END-BAL                PIC 9(6).
           03  BEG-CAGE               PIC 9(4).
           03  END-CAGE               PIC 9(4).
           03  BEG-BNKDATE            PIC 9(8).
           03  END-BNKDATE            PIC 9(8).
           03  BEG-DISMISS-DATE       PIC 9(8).
           03  END-DISMISS-DATE       PIC 9(8).

           03  CHAPTER                PIC 99.
           03  WHO-FILED              PIC X.
           03  ACTIVE-FLAG            PIC X.
           03  PL                     PIC X.
           03  PLACTIVE               PIC X.
           03  MATURED                PIC X.
           03  PROMISED               PIC X.
           03  ZBAL                   PIC X.
           03  STATE-CODE             PIC XX.
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).
           03  LOCAL-PRINT-FG         PIC X(1).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.


       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(36)    VALUE " ".
           03  H-TITLE          PIC X(35)    VALUE
           "BANKRUPTCY REPORT".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER      PIC X(10)    VALUE "BRANCH   :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC X(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER      PIC X(119)    VALUE SPACES.
           03  FILLER      PIC X         VALUE "M".
      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER      PIC X(49)    VALUE
           "ACCOUNT #       DUE NAME            BK     DLR   ".
           03  FILLER      PIC X(53)    VALUE
           "LOAN      APR  CURRENT   LAST     PROM     AMOUNT    ".
           03  FILLER      PIC X(30)    VALUE
           "TODAY A PDTH CON S  BNKRPT  ST".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER      PIC X(49)    VALUE
           "                DAY                 TY           ".
           03  FILLER      PIC X(53)    VALUE
           "DATE           BALANCE   PAID     DATE       PAID    ".
           03  FILLER      PIC X(30)    VALUE
           "  DUE I          B   DATP     ".


      **********************
      *   MISC WORKERS     *
      **********************
       01  HOLD-TODAY-DATE      PIC 9(8)     COMP.
       01  LN-ACCUM-BAL         PIC S9(7)V99 COMP VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  RUN-DISPLAY-MESS.
           03  FILLER           PIC X(24)    VALUE
           "REPORT IN PROGRESS......".
           03  RUN-MESS-BR      PIC 9(4).
           03  FILLER           PIC X(10).

       01  HOLD-BRNO            PIC 9(4)    VALUE 0.

       01  RS-DEALER            PIC 9(4)V99.
       01  RS-DLR               REDEFINES RS-DEALER.
           03  FILLER           PIC 9(4).
           03  FILLER           PIC 99.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM  PIC X(16).
           03  FILLER      PIC X.
           03  D-DAY       PIC 99.
           03  FILLER      PIC X.
           03  D-NAME      PIC X(15).
           03  FILLER      PIC X.
           03  D-CHAPTER   PIC Z9.
           03  FILLER      PIC X.
           03  D-DLNO      PIC Z(4).99     BLANK ZERO.
           03  FILLER      PIC X.
           03  D-LOANDATE  PIC 99/99/99.
           03  FILLER      PIC X.
           03  D-APR       PIC ZZ9.99.
           03  FILLER      PIC X.
           03  D-CURBAL    PIC Z(5).99.
           03  FILLER      PIC X.
           03  D-LAST-PAID PIC 99/99/99 BLANK ZERO.
           03  FILLER      PIC X.
           03  D-PROMDATE  PIC 99/99/99 BLANK ZERO.
           03  FILLER      PIC X.
           03  D-AMT-PAID  PIC Z(5).99- BLANK ZERO.
           03  D-DUE       PIC Z(5).99- BLANK ZERO.
           03  D-ACTIVE    PIC X.
           03  FILLER      PIC X.
           03  D-PDTH-MM   PIC 9(2).
           03  D-PDTH-YY   PIC 9(2).
           03  FILLER      PIC X.
           03  D-CON       PIC Z(3).
           03  FILLER      PIC X.
           03  D-WHO       PIC X.
           03  FILLER      PIC X.
           03  D-BKDATE    PIC 99/99/99 BLANK ZERO.
           03  FILLER      PIC X.
           03  D-STATE     PIC XX.


       01  TOTAL-LINE1 REDEFINES DETAIL-LINE.
           03  D-TOTAL-TAG  PIC X(25).
           03  FILLER       PIC X.
           03  D-TOTAL-COUNT PIC ZZZ,ZZZ,ZZZ.
           03  FILLER       PIC XX.
           03  D-TOTAL      PIC $$,$$$,$$$,$$$.99.


       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(20).
           03  RANGE-SELECTION   PIC X(112).
           03  RANGE-SELNUM     REDEFINES RANGE-SELECTION.
               05  RANGE-NUM     PIC Z(6).99.
               05  FILLER        PIC X(103).
           03  RANGE-SELDATE     REDEFINES RANGE-SELECTION.
               05  RANGE-DATE    PIC 9999/99/99.
               05  FILLER        PIC X(102).


       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BANKRT_DEF.CPY".
       COPY "LIBLP/BANKRT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BANKRT_SCN.CPY".

      *************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************
       MAIN-MODULE SECTION.
      ******************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BANKRT" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           IF ( LOCAL-PRINT-FG = "Y" )
              PERFORM CREATE-SPOOL-DIR
              IF ( STAT NOT = "00" )
                 GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTB-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              MOVE BR-NO TO RUN-MESS-BR
              MOVE RUN-DISPLAY-MESS TO MESS
              PERFORM SEND-LEGEND.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

      * CLASS RANGE
           IF LN-CLASS < BEG-CLASS
              GO TO NEXT-LN.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-BNKRPTDATE = 0
              GO TO NEXT-LN.

      * DEALER RANGE
           IF LN-DLNO < BEG-DEALER OR LN-DLNO > END-DEALER
              GO TO NEXT-LN.

      * DAY DUE RANGE
           IF LN-1STPYDA < BEG-DAY OR > END-DAY
              GO TO NEXT-LN.

      * P & L
           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.

           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "A"
                 IF LN-PLCD NOT = "P"
                    GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "I"
                 IF LN-PLCD NOT = "I"
                    IF LN-PLCD NOT = "W"
                       GO TO NEXT-LN.

      * ZERO BALANCES
           IF ZBAL = "O"
              IF LN-CURBAL NOT = 0 OR
                  LN-INTBAL NOT = 0 OR
                   LN-LCBAL NOT = 0 OR
                    LN-OTHBAL NOT = 0
                      GO TO NEXT-LN.

           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                       GO TO NEXT-LN.

      * P & L
           IF PROMISED = "S"
              IF LN-PROMDATE NOT = 0
                 GO TO NEXT-LN.

           IF PROMISED = "O"
              IF LN-PROMDATE = 0
                 GO TO NEXT-LN.

      * BALANCE DUE RANGE
           COMPUTE LN-ACCUM-BAL = LN-CURBAL + LN-OTHBAL + LN-INTBAL
                                  + LN-LCBAL.

           IF LN-ACCUM-BAL < BEG-BAL OR
               LN-ACCUM-BAL > END-BAL
                  GO TO NEXT-LN.


      * CHAPTER (7,11,13) OR F3-ALL (99)
           IF CHAPTER NOT = 99
              IF LN-BNKRPT-CHAPTER NOT = CHAPTER
                 GO TO NEXT-LN.

      * BNKRPT DATE RANGE
           IF (LN-BNKRPTDATE < BEG-BNKDATE) OR
                (LN-BNKRPTDATE > END-BNKDATE)
              GO TO NEXT-LN.

      * DISMISSAL DATE RANGE
           IF LN-BNKRPT-DISMISS-DATE = 0
              IF (BEG-DISMISS-DATE NOT = 0 AND
                  BEG-DISMISS-DATE NOT = EXT-JULIAN-BEG)
                 GO TO NEXT-LN.

           IF LN-BNKRPT-DISMISS-DATE NOT = 0
              IF (LN-BNKRPT-DISMISS-DATE < BEG-DISMISS-DATE) OR
                   (LN-BNKRPT-DISMISS-DATE > END-DISMISS-DATE)
                 GO TO NEXT-LN.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
               GO TO NEXT-LN.

      * NAME RANGE
           IF NOT (BEG-LNAME = "    " AND END-LNAME = "ZZZZ")
              MOVE BW-LNAME TO TEST-NAME
              IF TEST-NAME < BEG-LNAME
                 OR TEST-NAME > END-LNAME
                    GO TO NEXT-LN.

           PERFORM GET-SP.

      * CONTRACT AGE RANGE
           MOVE TODAYS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           IF CONTRACTUAL < BEG-CAGE OR CONTRACTUAL > END-CAGE
              GO TO NEXT-LN.

      *  MATURED ACCOUNTS
           IF MATURED = "I"
              GO TO CONTINUE-ON.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE HOLD-TODAY-DATE TO NUM-DATE.

           IF LN-TOTNOALDEL NOT = 0
              MOVE MDTE-DATE TO NDTE-DATE
              MOVE LN-TOTNOALDEL TO NDTE-HOLD
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE TO MDTE-DATE.

           MOVE MDTE-DATE TO SYS-DATE.
           PERFORM TIM.

           IF ELAPSED-DAYS NOT > 0
              IF MATURED = "S"
                 GO TO NEXT-LN.

           IF ELAPSED-DAYS NOT < 0
              IF MATURED = "O"
                 GO TO NEXT-LN.

      *----------------------------------------------------------------
       CONTINUE-ON.

      * LEAVE THIS OPEN HERE, I THINK A ROUTINE IS CLOSING IT
           PERFORM OPEN-LTB1-FILE.
           MOVE LN-OWNBR  TO LTB-BRNO.
           MOVE LN-ACCTNO TO LTB-ACCTNO.
           MOVE 1         TO LTB-SEQNO.
           PERFORM READ-LTB1-FILE.
           IF IO-FG NOT = 0
              GO TO NEXT-LN.

      * TRUSTEE (3)
      * STATE CODE  **=ALL
           IF STATE-CODE NOT = "**"
              IF LTB-STATE(3) NOT = STATE-CODE
                 GO TO NEXT-LN.

           IF WHO-FILED NOT = "*"
              IF WHO-FILED NOT = LTB-WHO-FILED
                 GO TO NEXT-LN.

      * THEY WANT "A" TO INCLUDE LTB-ACTIVE-INACTIVE SPACES
      * SO ONLY TEST THAT THE I'S MATCH.
           IF ACTIVE-FLAG NOT = "B"
              IF ACTIVE-FLAG = "I"
                 IF LTB-ACTIVE-INACTIVE NOT = ACTIVE-FLAG
                    GO TO NEXT-LN
                 END-IF
              ELSE
              IF ACTIVE-FLAG = "A"
                 IF LTB-ACTIVE-INACTIVE NOT = SPACES
                    IF LTB-ACTIVE-INACTIVE NOT = ACTIVE-FLAG
                       GO TO NEXT-LN.


           IF FIRST-TIME = "Y"
              MOVE LN-OWNBR TO HOLD-BRNO
              MOVE HOLD-BRNO TO H-BRNO
              MOVE BR-NAME TO H-BRDESC.
           IF LN-OWNBR NOT = HOLD-BRNO
              MOVE LN-OWNBR TO HOLD-BRNO
              MOVE HOLD-BRNO TO H-BRNO
              MOVE BR-NAME TO H-BRDESC
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

           PERFORM DETAIL-LINE-PROCESS.
           ADD LN-CURBAL TO TOTAL-CURBAL GRAND-CURBAL.
           ADD 1 TO TOTAL-COUNT GRAND-COUNT.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-LN.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN3-FILE.

           IF TOTAL-CURBAL NOT = 0
              MOVE "TOTAL CURRENT BALANCE:" TO D-TOTAL-TAG
              MOVE TOTAL-COUNT TO D-TOTAL-COUNT
              MOVE TOTAL-CURBAL TO D-TOTAL
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE 0 TO TOTAL-CURBAL TOTAL-COUNT.

           IF ( LOCAL-PRINT-FG = "Y" )
              MOVE 0 TO PAGE-NUMBER
              PERFORM CLOSE-PR-FILE.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE "GRAND TOTAL   BALANCE:" TO D-TOTAL-TAG.
           MOVE GRAND-COUNT TO D-TOTAL-COUNT .
           MOVE GRAND-CURBAL TO D-TOTAL.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           IF ( LOCAL-PRINT-FG = "N" )
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ****************************
       INITIALIZATION SECTION.
      ****************************
           MOVE BANKRT-QUEUE-POS TO Q-POS.
           MOVE BANKRT-COPIES-POS TO C-POS.
           MOVE BANKRT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE LOCAL-PRINT-FG TO SV-LOCAL-PRINT-FG
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG LOCAL-PRINT-FG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS...................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ********************************************
       EO-EXEC-AUDIT SECTION.
      ********************************************

      ********************************************
       ENTER-PARAMETERS SECTION.
      ********************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO ENTER-STATE-CODE.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S NA000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.

           MOVE VDUNUM0 TO BEG-CLASS.

      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.

           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.

           GO TO ALPHA-01.

      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ALPHA-01.
           MOVE "E  A040ALP1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-ALPHA.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ALPHA-01.
           MOVE VDUBUF TO BEG-LNAME.
       ALPHA-02.
           MOVE "E  A040ALP2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ALPHA-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ALPHA-02.
           MOVE VDUBUF TO END-LNAME.
           IF BEG-LNAME > END-LNAME
              MOVE "INVALID ALPHA RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ALPHA-01.

           GO TO DL-01.

       ALL-ALPHA.
           MOVE "S  A000ALP1." TO VDU.
           MOVE "   " TO BEG-LNAME VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000ALP2." TO VDU.
      * LOWER "ZZZZ"
           MOVE X'7A7A7A7A' TO END-LNAME VDUBUF.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       DL-01.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO ALPHA-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "DLBEG" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF IO-BAD
                 GO TO DL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DL-01.

           MOVE VDUNUM2 TO BEG-DEALER.

      *----------------------------------------------------------------
       DL-02.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "DLEND" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF IO-BAD
                 GO TO DL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DL-02.

           MOVE VDUNUM2 TO END-DEALER.

           IF BEG-DEALER > END-DEALER
              PERFORM INVALID-RANGE
              GO TO DL-01.

           GO TO DAY-01.

      *----------------------------------------------------------------
       ALL-DL.
           MOVE "SNNN042DL1." TO VDU.
           MOVE 0 TO BEG-DEALER VDUNUM2.
           PERFORM SCREENIO.

           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99 TO END-DEALER VDUNUM2.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DAY-01.
           MOVE "ENNN020DAY1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALLDAYS.
           IF VDUSTATUS = "1U" GO TO DL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DAY-01.

           MOVE VDUNUM0 TO BEG-DAY.

      *----------------------------------------------------------------
       DAY-02.
           MOVE "ENNN020DAY2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO DAY-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DAY-02.

           MOVE VDUNUM0 TO END-DAY.

           IF BEG-DAY > END-DAY
              PERFORM INVALID-RANGE
              GO TO DAY-01.

           GO TO BAL-01.

      *----------------------------------------------------------------
       ALLDAYS.
           MOVE "SNNN020DAY1." TO VDU.
           MOVE 1 TO BEG-DAY VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020DAY2." TO VDU.
           MOVE 31 TO END-DAY VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BAL-01.
           MOVE "ENNN060BAL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BAL.
           IF VDUSTATUS = "1U" GO TO DAY-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BAL-01.

           MOVE VDUNUM0 TO BEG-BAL.

      *----------------------------------------------------------------
       BAL-02.
           MOVE "ENNN060BAL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BAL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BAL-02.

           MOVE VDUNUM0 TO END-BAL.

           IF BEG-BAL > END-BAL
              PERFORM INVALID-RANGE
              GO TO BAL-01.

           GO TO CAGE-01.

      *----------------------------------------------------------------
       ALL-BAL.
           MOVE "SNNN060BAL1." TO VDU.
           MOVE 0 TO BEG-BAL VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060BAL2." TO VDU.
           MOVE 999999 TO END-BAL VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       CAGE-01.
           MOVE "ENNN040CAGE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CAGE.
           IF VDUSTATUS = "1U" GO TO BAL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CAGE-01.

           MOVE VDUNUM0 TO BEG-CAGE.

      *----------------------------------------------------------------
       CAGE-02.
           MOVE "ENNN040CAGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CAGE-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CAGE-02.

           MOVE VDUNUM0 TO END-CAGE.

           IF BEG-CAGE > END-CAGE
              PERFORM INVALID-RANGE
              GO TO CAGE-01.

           GO TO BEG-BNKRPT.

      *----------------------------------------------------------------
       ALL-CAGE.
           MOVE "SNNN040CAGE1." TO VDU.
           MOVE 0 TO BEG-CAGE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040CAGE2." TO VDU.
           MOVE 9999 TO END-CAGE VDUNUM0.
           PERFORM SCREENIO.


      *----------------------------------------------------------------
       BEG-BNKRPT.
           MOVE "ERNM060BNK1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CAGE-01.
           IF VDUSTATUS = "16" GO TO ALL-BNKRPT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEG-BNKRPT.

           MOVE VDU-DATE-YYYYMMDD TO BEG-BNKDATE.
      *-----------------------------------------------------------
       END-BNKRPT.
           MOVE "ERNM060BNK2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BEG-BNKRPT.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO END-BNKRPT.

           MOVE VDU-DATE-YYYYMMDD TO END-BNKDATE.

           IF END-BNKDATE < BEG-BNKDATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-BNKRPT.

           GO TO BEG-DISM.

      *----------------------------------------------------------
       ALL-BNKRPT.
           MOVE "S  8080BNK1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-BNKDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080BNK2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-BNKDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BEG-DISM.
           MOVE "ERNM060DIS1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BEG-BNKRPT.
           IF VDUSTATUS = "16" GO TO ALL-DISM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEG-DISM.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DISMISS-DATE.
      *-----------------------------------------------------------
       END-DISM.
           MOVE "ERNM060DIS2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BEG-DISM.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO END-DISM.

           MOVE VDU-DATE-YYYYMMDD TO END-DISMISS-DATE.

           IF END-DISMISS-DATE < BEG-DISMISS-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-DISM.

           GO TO ENTER-CHAP.

      *----------------------------------------------------------
       ALL-DISM.
           MOVE "S  8080DIS1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DISMISS-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DIS2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-DISMISS-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------
       ENTER-CHAP.
           MOVE "EN N020CHAPTER." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO END-DISM.
           IF VDUSTATUS = "16" GO TO ALL-CHAP.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENTER-CHAP.

           MOVE VDUNUM0 TO CHAPTER.
           IF CHAPTER NOT = 7 AND NOT = 11 AND NOT = 13
              GO TO ENTER-CHAP.

           GO TO ENTER-WHO.

       ALL-CHAP.
           MOVE "S  N020CHAPTER." TO VDU.
           MOVE 99 TO CHAPTER VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------
       ENTER-WHO.
           MOVE "E  A010WHO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ENTER-CHAP.
           IF VDUSTATUS = "16" GO TO ALL-WHO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENTER-WHO.

           MOVE VDUBUF TO WHO-FILED.
           IF WHO-FILED NOT = "M" AND NOT = "S" AND NOT = "B"
              GO TO ENTER-WHO.

           GO TO ENTER-ACTIVE.

       ALL-WHO.
           MOVE "S  A010WHO." TO VDU.
           MOVE "*" TO WHO-FILED VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------
       ENTER-ACTIVE.
           MOVE "E  A010CASES." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ENTER-WHO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-ACTIVE.

           MOVE VDUBUF TO ACTIVE-FLAG.
           IF ACTIVE-FLAG NOT = "A" AND NOT = "I" AND NOT = "B"
              GO TO ENTER-ACTIVE.

      *----------------------------------------------------------
       PLYN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ENTER-ACTIVE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PLYN.

           MOVE VDUBUF TO PL.

           IF PL NOT = "I" AND NOT = "O" AND NOT = "S"
              GO TO PLYN.

           IF PL = "S"
              MOVE "B" TO PLACTIVE
              GO TO MATYN.

      *----------------------------------------------------------------
       ACTVYN.
           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PLYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ACTVYN.

           MOVE VDUBUF TO PLACTIVE.

           IF PLACTIVE NOT = "I" AND NOT = "A" AND NOT = "B"
              GO TO ACTVYN.

      *----------------------------------------------------------------
       MATYN.
           MOVE "E  A010MAT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ACTVYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO MATYN.

           MOVE VDUBUF TO MATURED.

           IF MATURED NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO MATYN.

      *----------------------------------------------------------------
       PROMYN.
           MOVE "E  A010PROM." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO MATYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PROMYN.

           MOVE VDUBUF TO PROMISED.

           IF PROMISED NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO PROMYN.

      *----------------------------------------------------------------
       ZBALYN.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PROMYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ZBALYN.

           MOVE VDUBUF TO ZBAL.

           IF ZBAL NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO ZBALYN.

      *----------------------------------------------------------------
       ENTER-STATE-CODE.
           MOVE "E  A020STATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF VDUSTATUS = "16" GO TO ALL-STATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-STATE-CODE.

           MOVE VDUBUF TO STATE-CODE.

           GO TO LOCAL-PRINT.

       ALL-STATE.
           MOVE "S  A020STATE." TO VDU.
           MOVE "**" TO STATE-CODE VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO EXIT-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO EXIT-PARM.

      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-STATE-CODE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

       EXIT-PARM.
           EXIT.

      ************************************************
       DISPLAY-PARAMETERS SECTION.
      ************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000ALP1." TO VDU.
           MOVE BEG-LNAME TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000ALP2." TO VDU.
           MOVE END-LNAME TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN042DL1." TO VDU.
           MOVE BEG-DEALER TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN042DL2." TO VDU.
           MOVE END-DEALER TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020DAY1." TO VDU.
           MOVE BEG-DAY TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020DAY2." TO VDU.
           MOVE END-DAY TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060BAL1." TO VDU.
           MOVE BEG-BAL TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060BAL2." TO VDU.
           MOVE END-BAL TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040CAGE1." TO VDU.
           MOVE BEG-CAGE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040CAGE2." TO VDU.
           MOVE END-CAGE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8000BNK1." TO VDU.
           MOVE BEG-BNKDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8000BNK2." TO VDU.
           MOVE END-BNKDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8000DIS1." TO VDU.
           MOVE BEG-DISMISS-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8000DIS2." TO VDU.
           MOVE END-DISMISS-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N020CHAPTER." TO VDU.
           MOVE CHAPTER TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010WHO." TO VDU.
           MOVE WHO-FILED TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010CASES." TO VDU.
           MOVE ACTIVE-FLAG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PL." TO VDU.
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ACTIVE." TO VDU.
           MOVE PLACTIVE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010MAT." TO VDU.
           MOVE MATURED TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PROM." TO VDU.
           MOVE PROMISED TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A020STATE." TO VDU.
           MOVE STATE-CODE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"         TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ********************************
       DETAIL-LINE-PROCESS SECTION.
      ********************************
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.

           MOVE LN-1STPYDA TO D-DAY.

           MOVE SPACES TO D-NAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                INTO D-NAME.

           MOVE LN-BNKRPT-CHAPTER TO D-CHAPTER.
           MOVE LN-DLNO           TO D-DLNO.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-LOANDATE.

           COMPUTE D-APR ROUNDED = LN-APRATE * 1.

           MOVE LN-CURBAL TO D-CURBAL.

           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LAST-PAID.
           MOVE LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PROMDATE.

           MOVE LN-AMT-PAID-LAST TO D-AMT-PAID.


           MOVE LTB-ACTIVE-INACTIVE TO D-ACTIVE.

           MOVE LTB-WHO-FILED TO D-WHO.

           MOVE LN-BNKRPTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-BKDATE.

      * (3) TRUSTEE
           MOVE LTB-STATE(3) TO D-STATE.

           MOVE TODAYS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           MOVE CONTRACTUAL TO D-CON.

           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE TODAYS-DATE  TO CEPP2-DATE.
           PERFORM CONTRACTUAL-DELINQUENCY.
           IF CDEL-DELINQUENT < 0
              SUBTRACT CDEL-DELINQUENT FROM 0 GIVING WS-AMOUNT
           ELSE
               MOVE 0 TO WS-AMOUNT.
           MOVE WS-AMOUNT      TO D-DUE.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO NUM-DATE.
           MOVE NUM-MO         TO D-PDTH-MM.
           MOVE NUM-YR         TO D-PDTH-YY.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LINE-PROCESS.
           EXIT.

      ************************************
       WRITE-DETAIL-LINE SECTION.
      ************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *************************************
       PRINT-RANGE-LINES SECTION.
      *************************************
           MOVE 2 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING CLASS       :" TO RANGE-LINE.
           MOVE END-CLASS           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LAST NAME:" TO RANGE-LINE.
           MOVE BEG-LNAME TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING LAST NAME   :" TO RANGE-LINE.
           MOVE END-LNAME TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DEALER   :" TO RANGE-LINE.
           MOVE BEG-DEALER TO RS-DEALER.
           MOVE RS-DLR                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING DEALER      :" TO RANGE-LINE.
           MOVE END-DEALER TO RS-DEALER.
           MOVE RS-DLR                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DAY DUE  :" TO RANGE-LINE.
           MOVE BEG-DAY                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING DAY DUE     :" TO RANGE-LINE.
           MOVE END-DAY                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING BALANCE  :" TO RANGE-LINE.
           MOVE BEG-BAL          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING BALANCE     :" TO RANGE-LINE.
           MOVE END-BAL          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN CONTRACTUAL  :" TO RANGE-LINE.
           MOVE BEG-CAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END   CONTRACTUAL  :" TO RANGE-LINE.
           MOVE END-CAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN BNKRPT DATE  :" TO RANGE-LINE.
           MOVE BEG-BNKDATE            TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END   BNKRPT DATE  :" TO RANGE-LINE.
           MOVE END-BNKDATE            TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN DISMISS DATE :" TO RANGE-LINE.
           MOVE BEG-DISMISS-DATE          TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END   DISMISS DATE :" TO RANGE-LINE.
           MOVE END-DISMISS-DATE          TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CHAPTER            :" TO RANGE-LINE.
           MOVE CHAPTER                TO RANGE-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "WHO FILED          :" TO RANGE-LINE.
           MOVE WHO-FILED              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACTIVE/INACTIVE    :" TO RANGE-LINE.
           MOVE ACTIVE-FLAG            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P & L              :" TO RANGE-LINE.
           MOVE PL                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P&L ACTIVE         :" TO RANGE-LINE.
           MOVE PLACTIVE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MATURED            :" TO RANGE-LINE.
           MOVE MATURED                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROMISED           :" TO RANGE-LINE.
           MOVE PROMISED               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ZERO BALANCES      :" TO RANGE-LINE.
           MOVE ZBAL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STATE CODE         :" TO RANGE-LINE.
           MOVE STATE-CODE              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

      *****************************
       HEAD-PAGE SECTION.
      *****************************

           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD6 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *----------------------------------------------------------------
       END-HEAD-PAGE.
           MOVE 8 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


      *****************************************************************
      *    D E A L E R    S C A N   R O U T I N E
      *****************************************************************
       DEALER-SCAN SECTION.
           MOVE 0 TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DLSCAN-STATUS NOT = "00" OR DLSCAN-NUM = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.

           IF ( DEALER-FG = "DLBEG" )
              MOVE "SNNN042DL1." TO VDU
           ELSE
              MOVE "SNNN042DL2." TO VDU.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.


       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO BANKRT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BANKRT-SYSDATE.
           DISPLAY BANKRT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BANKRT-FORM-FIELDS TO WR-BUF.
           MOVE BANKRT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE BANKRT-HELP--FILE TO HELP-LINK-FILE.
           MOVE BANKRT-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BANKRT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************
       MISC-PARA SECTION.
      *************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-BAD
                 GO TO IO-ERROR.


      ****************************
       IO-ROUTINE SECTION.
      ****************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTB1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ************************************
       END-PROGRAM SECTION.
      ************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *----------------------------------------------------------------
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
                 MOVE SV-LOCAL-PRINT-FG TO LOCAL-PRINT-FG
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPR2MU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BANKRT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
