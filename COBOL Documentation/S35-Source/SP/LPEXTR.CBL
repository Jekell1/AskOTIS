       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPEXTR.
       DATE-WRITTEN.    012202.
      **************************************************************************
      *                      (SP)
      *    DESCRIPTION:  DATA WAREHOUSE PAYMENT EXTRACTION
      *                     DRIVEN FROM LPFILE
      *
      * CREATES AN OUTPUT EXTRACT FILE  /USR/EXTRACT/PAYMENT
      *                           F3-ALL       = PAYMENT
      *                           BRANCH RANGE = PAYMENT.(END-BRANCH)
      *                           GROUP        = PAYMENT.(GROUP)
      * RSZ 623 INCLUDING CR,LF
      *
      * REV:
      *     020122 REGIONAL ACCEPTANCE, PR 1675
      * BLV 020208 ADDED TRANSACTION DATE RANGE
      *  CS 040907 ADD BR-REGION-NUMBER TO OUTPUT XLP-REGION-NUMBER[PR#76 REGACC
      * BAH 140729 EXPANDED XLP-KEY FROM 9(7) TO 9(9), REGMGM PL# 853
      * BAH 160201 REMOVED LTFILE, PD0003
      * BAH 180104 8 DIGIT DATES
      *  CS FIXED XLP-ACCTNO TO MATCH A15 WITH 01 SEQNO
      * BAH 2018.1108 REMOVE LOAN FILE, DRIVE OFF LPFILE, REMOVE CLASS,P&L,
      *               AND ZBAL, ADD REVERSE ONLY, WORLD #1346
      * BAH 2018.1116 XLP-TILLNO TO MATCH A15 WITH ONLY 1 ALPHA, LORI
      * BAH 2019.0709 WAS MISSING TEST OF REV-ONLY AFTER READ-LP
      * BAH 20190722 ADDED SUFFIX TO PATH SO CAN MULTIPLE AT SAME TIME, #1381
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      * RMZ 20231020 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      * BAH 20240311 ADDED XLP-REPAY-TRANS-ID #1641
      * BAH 20250115 ADDED XLP-CLOSFEE-REB-AMT GA CLOSING FEE #1697 S35Q-157
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBEX/EXFSXLP.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBEX/EXFDXLP.CPY".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/LPEXTR.CBL S35 01/15/25-12:15:51 >".

      ******************************************************************
      *
      *     F I L E   S P E C S
      *
      ******************************************************************
      *********************************************************
      *   LIBEX/EXWSXLP: PAYMENT EXTRACTION FILE PATH WORKERS
      *********************************************************
       01  XLP-PATH.
           03  XLP-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLP-FILENAME       PIC X(7) VALUE "PAYMENT".
           03  XLP-EXTENSION.
               05  XLP-PATH-DOT       PIC X.
               05  XLP-PATH-GROUP     PIC X(4).
               05  XLP-PATH-BRNO REDEFINES XLP-PATH-GROUP PIC 9(4).


       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
      *----------------------------------------------------------------
       01  XLP-KEY       PIC 9(9)     VALUE 0.

       01  WS-BRACCTNO.
           03  WS-OWNBR    PIC 9(4).
           03  WS-NUMBER   PIC 9(6).
       01  WS-A15-ACCTNO.
           03  WS-A15-NUMBER PIC 9(6).
           03  WS-A15-SEQNO  PIC 9(2).

       01  WS-A30-TILL       PIC 99.
       01  WS-A30-TILLX REDEFINES WS-A30-TILL.
           03  WS-A30-TILL-1 PIC 9.
           03  WS-A30-TILL-2 PIC 9.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-BEG-DATE         PIC X(10). 
       01  QWS-END-DATE         PIC X(10).
       01  QWS-REV-ONLY         PIC X.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LP1-CONN-OPEN-SW     PIC X        VALUE "N".

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  AGE-DATE               PIC 9(8).
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).
           03  BEG-DATE               PIC 9(8).
           03  END-DATE               PIC 9(8).
           03  REV-ONLY               PIC X.

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
      *-----------------------------------------------------------------
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
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
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE          PIC X(23)    VALUE
           "PAYMENT FILE EXTRACTION".
           03  FILLER           PIC X(6)     VALUE " FOR: ".
           03  H-AGEDATE        PIC X(10).

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "N".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(29)    VALUE
           "BUILDING EXTRACTION FILE....".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10).

       01  RECORD-COUNT           PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(21).
           03  RANGE-SELECTION   PIC X(111).
           03  RANGE-SELNUM     REDEFINES RANGE-SELECTION.
               05  RANGE-DATE PIC 9999/99/99.
               05  FILLER        PIC X(101).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/LPEXTR_DEF.CPY".
       COPY "LIBSP/LPEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/LPEXTR_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE XLP-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LXG1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               XLP-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LPEXTR" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "EXTRACTION IN PROGRESS...........", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LP-PATH-OVERRIDE
                              LXG-PATH-OVERRIDE.

           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-LXG1-FILE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE WK-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.


           MOVE LP-PATH-OWNBR TO LP-BRNO.
           MOVE 0             TO LP-ACCTNO.
           MOVE 0             TO LP-SEQNO.

           PERFORM START-LP1-FILE. *> SSP - LP,LXG,LX
       NEXT-LP.
           PERFORM READ-LP1-FILE-NEXT. *> EMBEDDED - LP,LXG,LX
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF REV-ONLY = "Y"
              IF LP-REV NOT = "Y"
                 GO TO NEXT-LP.

           IF LP-POSTDATE < BEG-DATE OR LP-POSTDATE > END-DATE
              GO TO NEXT-LP.

           MOVE SPACES TO XLP-REC.

           MOVE BR-NO                   TO WS-OWNBR.
           MOVE LP-ACCTNO                TO WS-NUMBER.
           MOVE WS-BRACCTNO              TO XLP-BRACCTNO.
           MOVE BR-REGION-NUMBER         TO XLP-REGION-NUMBER.

      *NEED THE 01 SEQ TO MATCH A15
      *    MOVE LP-ACCTNO                TO XLP-ACCTNO.
           MOVE LP-ACCTNO                TO WS-A15-NUMBER.
           MOVE 01                       TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO            TO XLP-ACCTNO.          

           MOVE LP-SEQNO                 TO XLP-SEQNO.
           MOVE LP-USERID                TO XLP-USERID.

           MOVE LP-POSTDATE              TO XLP-POSTDATE.
           MOVE LP-POSTTIME              TO XLP-POSTTIME.

           MOVE LP-TRDATE                TO XLP-TRDATE.
           MOVE LP-PAYDATE               TO XLP-PAYDATE.

           MOVE LP-TRCD                  TO XLP-TRCD.
           MOVE LP-REFNO                 TO XLP-REFNO.
           MOVE LP-TRAMT                 TO XLP-TRAMT.
           INSPECT XLP-TRAMT REPLACING LEADING "+" BY " ".
           MOVE LP-APOTH                 TO XLP-APOTH.
           INSPECT XLP-APOTH REPLACING LEADING "+" BY " ".
           MOVE LP-APOT2                 TO XLP-APOT2.
           INSPECT XLP-APOT2 REPLACING LEADING "+" BY " ".
           MOVE LP-APINT                 TO XLP-APINT.
           INSPECT XLP-APINT REPLACING LEADING "+" BY " ".
           MOVE LP-APLC                  TO XLP-APLC.
           INSPECT XLP-APLC REPLACING LEADING "+" BY " ".
           MOVE LP-APCUR                 TO XLP-APCUR.
           INSPECT XLP-APCUR REPLACING LEADING "+" BY " ".
           MOVE LP-OTHBAL                TO XLP-OTHBAL.
           INSPECT XLP-OTHBAL REPLACING LEADING "+" BY " ".
           MOVE LP-OT2BAL                TO XLP-OT2BAL.
           INSPECT XLP-OT2BAL REPLACING LEADING "+" BY " ".
           MOVE LP-INTBAL                TO XLP-INTBAL.
           INSPECT XLP-INTBAL REPLACING LEADING "+" BY " ".
           MOVE LP-LCBAL                 TO XLP-LCBAL.
           INSPECT XLP-LCBAL REPLACING LEADING "+" BY " ".
           MOVE LP-CURBAL                TO XLP-CURBAL.
           INSPECT XLP-CURBAL REPLACING LEADING "+" BY " ".
           MOVE LP-INTDUE                TO XLP-INTDUE.
           INSPECT XLP-INTDUE REPLACING LEADING "+" BY " ".
           MOVE LP-DLPROC                TO XLP-DLPROC.
           INSPECT XLP-DLPROC REPLACING LEADING "+" BY " ".
           MOVE LP-EARNED(1)             TO XLP-EARNED1.
           INSPECT XLP-EARNED1 REPLACING LEADING "+" BY " ".
           MOVE LP-EARNED(2)             TO XLP-EARNED2.
           INSPECT XLP-EARNED2 REPLACING LEADING "+" BY " ".
           MOVE LP-EARNED(3)             TO XLP-EARNED3.
           INSPECT XLP-EARNED3 REPLACING LEADING "+" BY " ".
           MOVE LP-INTPAID               TO XLP-INTPAID.
           INSPECT XLP-INTPAID REPLACING LEADING "+" BY " ".
           MOVE LP-SSNO                  TO XLP-SSNO.

           MOVE LP-PDTH-DATE             TO XLP-PDTH.
           MOVE LP-LCPDTH-DATE           TO XLP-LCPDTH.

      * A15 LP-TILLNO = PIC X, A30 LP-TILLNO = PIC 99
           MOVE LP-TILLNO                TO WS-A30-TILL.
           MOVE WS-A30-TILL-2            TO XLP-TILLNO.
           MOVE LP-APINTOWE              TO XLP-APINTOWE.
           INSPECT XLP-APINTOWE REPLACING LEADING "+" BY " ".
           MOVE LP-DLPARVHELD-ADJ        TO XLP-DLPARVHELD-ADJ.
           INSPECT XLP-DLPARVHELD-ADJ REPLACING LEADING "+" BY " ".
           MOVE LP-IBPC                  TO XLP-IBPC.
           MOVE LP-MANUAL-RB             TO XLP-MANUAL-RB.
           MOVE LP-REV                   TO XLP-REV.
           MOVE LP-REVTRCD               TO XLP-REVTRCD.
           MOVE LP-REVSEQNO              TO XLP-REVSEQNO.
           MOVE LP-STATUSFG              TO XLP-STATUSFG.
           MOVE LP-NEWSTAT-RATE          TO XLP-NEWSTAT-RATE.
           MOVE LP-LCPARTIALS            TO XLP-LCPARTIALS.
           INSPECT XLP-LCPARTIALS REPLACING LEADING "+" BY " ".
           MOVE LP-LCPAID               TO XLP-LCPAID.
           INSPECT XLP-LCPAID REPLACING LEADING "+" BY " ".
           MOVE LP-DLRVCHGS-ADJ(1)       TO XLP-DLRVCHGS-ADJ1.
           INSPECT XLP-DLRVCHGS-ADJ1 REPLACING LEADING "+" BY " ".
           MOVE LP-DLRVCHGS-ADJ(2)       TO XLP-DLRVCHGS-ADJ2.
           INSPECT XLP-DLRVCHGS-ADJ2 REPLACING LEADING "+" BY " ".
           MOVE LP-DLRVCHGS-ADJ(3)       TO XLP-DLRVCHGS-ADJ3.
           INSPECT XLP-DLRVCHGS-ADJ3 REPLACING LEADING "+" BY " ".
           MOVE LP-DLPARVPAID-ADJ        TO XLP-DLPARVPAID-ADJ.
           INSPECT XLP-DLPARVPAID-ADJ REPLACING LEADING "+" BY " ".
           MOVE LP-DLNO                  TO XLP-DLNO.
           MOVE LP-PREPAY-PENALTY        TO XLP-PREPAY-PENALTY.
           INSPECT XLP-PREPAY-PENALTY REPLACING LEADING "+" BY " ".
           MOVE LP-PRORATED-INS-REB      TO XLP-PRORATED-INS-REB.
           INSPECT XLP-PRORATED-INS-REB REPLACING LEADING "+" BY " ".
           MOVE LP-POSTING-BR            TO XLP-POSTING-BR.
           MOVE LP-NOLCHG                TO XLP-NOLCHG.
           INSPECT XLP-NOLCHG REPLACING LEADING "+" BY " ".
           MOVE LP-COLLECTOR             TO XLP-COLLECTOR.
           MOVE LP-EXTRA-PRIN-LFP        TO XLP-EXTRA-PRIN-LFP.
           INSPECT XLP-EXTRA-PRIN-LFP REPLACING LEADING "+" BY " ".
           MOVE LP-REMAIN-INT-SINCE-LFP  TO XLP-REMAIN-INT-SINCE-LFP.
           INSPECT XLP-REMAIN-INT-SINCE-LFP REPLACING LEADING "+" BY " ".
           MOVE LP-REMAIN-PRIN-SINCE-LFP TO XLP-REMAIN-PRIN-SINCE-LFP.
           INSPECT XLP-REMAIN-PRIN-SINCE-LFP
                       REPLACING LEADING "+" BY " ".
           MOVE LP-REPAY-TRANS-ID        TO XLP-REPAY-TRANS-ID.


      *    MOVE LP-BRNO   TO LXG-BRNO.
      *    MOVE LP-ACCTNO TO LXG-ACCTNO.
      *    MOVE LP-SEQNO  TO LXG-SEQNO.
      *    PERFORM READ-LXG1-FILE.
      *    IF IO-GOOD
      *       MOVE LXG-GLNO(1) TO XLP-GLNO1
      *       MOVE LXG-GLAMT(1) TO XLP-GLAMT1
      *       INSPECT XLP-GLAMT1 REPLACING LEADING "+" BY " "
      *       MOVE LXG-GLNO(2) TO XLP-GLNO2
      *       MOVE LXG-GLAMT(2) TO XLP-GLAMT2
      *       INSPECT XLP-GLAMT2 REPLACING LEADING "+" BY " "
      *       MOVE LXG-GLNO(3) TO XLP-GLNO3
      *       MOVE LXG-GLAMT(3) TO XLP-GLAMT3
      *       INSPECT XLP-GLAMT3 REPLACING LEADING "+" BY " "
      *    ELSE
      *       MOVE 0 TO XLP-GLNO1 XLP-GLAMT1
      *       INSPECT XLP-GLAMT1 REPLACING LEADING "+" BY " "
      *       MOVE 0 TO XLP-GLNO2 XLP-GLAMT2
      *       INSPECT XLP-GLAMT2 REPLACING LEADING "+" BY " "
      *       MOVE 0 TO XLP-GLNO3 XLP-GLAMT3
      *       INSPECT XLP-GLAMT3 REPLACING LEADING "+" BY " ".

           MOVE LXG-GLNO(1)   TO XLP-GLNO1.
           MOVE LXG-GLAMT(1)  TO XLP-GLAMT1.
           INSPECT XLP-GLAMT1 REPLACING LEADING "+" BY " ".
           MOVE LXG-GLNO(2)   TO XLP-GLNO2.
           MOVE LXG-GLAMT(2)  TO XLP-GLAMT2.
           INSPECT XLP-GLAMT2 REPLACING LEADING "+" BY " ".
           MOVE LXG-GLNO(3)   TO XLP-GLNO3.
           MOVE LXG-GLAMT(3)  TO XLP-GLAMT3.
           INSPECT XLP-GLAMT3 REPLACING LEADING "+" BY " ".
           IF ( LXR-REB-AMT NOT = 2020202.02
                        AND NOT = 0202020.20
                        AND NOT = 5389762.88 )
              MOVE LXR-REB-AMT TO XLP-CLOSFEE-REB-AMT
              INSPECT XLP-CLOSFEE-REB-AMT REPLACING
                                        LEADING "+" BY " ".

           MOVE X"0D" TO XLP-CR.
           MOVE X"0A" TO XLP-LF.

           ADD 1 TO XLP-KEY.
           PERFORM WRITE-XLP-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-LP.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LXG1-FILE.

           MOVE "Y" TO FIRST-TIME.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.


           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE LPEXTR-QUEUE-POS TO Q-POS.
           MOVE LPEXTR-COPIES-POS TO C-POS.
           MOVE LPEXTR-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           CALL "C$TOLOWER" USING XLP-DIRECTORY, VALUE 13.
           MOVE XLP-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "EXTRACTION DIRECTORY DOES NOT EXIST!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE AGE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE TO H-AGEDATE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION IN PROGRESS..........." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

      * USE BRANCH/GROUP EXTENSIONS, #1381
           MOVE SPACES TO XLP-EXTENSION.
           IF GROUP-FLAG = "Y"
              MOVE "."       TO XLP-PATH-DOT
              MOVE ENT-GROUP TO XLP-PATH-GROUP
           ELSE
              IF NOT (BEG-BRANCH = 0 AND END-BRANCH = 9999)
                 MOVE "."       TO XLP-PATH-DOT
                 MOVE END-BRANCH TO XLP-PATH-BRNO.

           PERFORM OPEN-XLP-FILE-OUTPUT.
           PERFORM CLOSE-XLP-FILE.
           PERFORM OPEN-XLP-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO AGE-DATE END-DATE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF VDUSTATUS = "1U"
              GO TO ENTER-REV-ONLY.

      *----------------------------------------------------------------
       AGEDT.

      * BATCH WILL USE DATE RAN

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060AGEDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGEDT.

           MOVE VDU-DATE-YYYYMMDD TO AGE-DATE.

           IF AGE-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000AGEDATE." TO VDU
              MOVE DATE-YYYYMMDD TO AGE-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

       BEGDT.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGEDT.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDT.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

           IF BEG-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000BEGDATE." TO VDU
              MOVE DATE-YYYYMMDD TO BEG-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENDDT.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDT.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000ENDDATE." TO VDU
              MOVE DATE-YYYYMMDD TO END-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO ENDDT.
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
              MOVE "S  A160DISPLAY-USER." TO VDU
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

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NA160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       ENTER-REV-ONLY.
           MOVE "E  A010REV." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-REV-ONLY.
           MOVE VDUBUF TO REV-ONLY.
           IF REV-ONLY NOT = "Y" AND NOT = "N"
              GO TO ENTER-REV-ONLY.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNA040BR2." TO VDU
              MOVE SPACES TO VDUNUM0
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      
           MOVE "S  A010REV." TO VDU.
           MOVE REV-ONLY TO VDUBUF.
           PERFORM SCREENIO.
      

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
      
           MOVE "AGEING DATE   :" TO RANGE-LINE.
           MOVE  AGE-DATE         TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEG TRANS DATE:" TO RANGE-LINE.
           MOVE  BEG-DATE         TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END TRANS DATE:" TO RANGE-LINE.
           MOVE  END-DATE         TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE .
      
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
      
           MOVE "REVERSED PAYS ONLY :" TO RANGE-LINE.
           MOVE REV-ONLY               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "STARTING TIME      :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# RECORDS EXTRACTED:" TO RANGE-LINE.
           MOVE RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
      
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO LPEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LPEXTR-SYSDATE.
           DISPLAY LPEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LPEXTR-FORM-FIELDS TO WR-BUF.
           MOVE LPEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE LPEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPEXTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/LPEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       CONDITIONAL-PRINT.
           IF DETAIL-LINE NOT = " "
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBEX/EXXLPI.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPLP1RN.CPY".
      * COPYMEMBER: LIBLP/LPLP1RN
       LOAD-LP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH.

      *-----------------------------------------------------------------
       OPEN-LP1-FILE.
           PERFORM LOAD-LP1-FILE.
           PERFORM OPEN-IT.
           MOVE LP-PATH-SQL TO E-FILE.

           IF LP1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LP1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LP1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.
   

      *-----------------------------------------------------------------
      * ***NOTE*** THIS 'START' IS NOT FOR DISPLAYING MULTIPLE PAGES.
      * ONLY SHOULD BE USED ONCE PER PROGRAM/BRANCH/ACCOUNT LIKE IN
      * A STANDARD REPORT.  USE START-LP1-FILE-SCAN FOR DISPLAYING
      * MULTIPLE PAGES.
      *-----------------------------------------------------------------
       START-LP1-FILE. *> SSP - LP,LXG,LX
           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.

           IF ( LP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QLP1-WEND-SEQNO = ZEROES    ) OR
              ( QLP1-WEND-SEQNO NOT NUMERIC )
              MOVE LP-PATH-OWNBR        TO QLP1-WBEG-BRNO
              MOVE LP-PATH-OWNBR        TO QLP1-WEND-BRNO
              MOVE LP-ACCTNO            TO QLP1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLP1-WEND-ACCTNO
              MOVE LP-SEQNO             TO QLP1-WBEG-SEQNO
              MOVE ALL "9"              TO QLP1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
      *
      *  STORED PROCEDURE:
      *
      *    SELECT LPFILE.LP_BRNO,
      *           LPFILE.LP_ACCTNO,
      *           LPFILE.LP_SEQNO,
      *           LPFILE.LP_USERID, 
      *           CAST(LPFILE.LP_POSTDATE AS VARCHAR(10)),
      *           LPFILE.LP_POSTTIME,
      *           CAST(LPFILE.LP_TRDATE   AS VARCHAR(10)), 
      *           CAST(LPFILE.LP_PAYDATE  AS VARCHAR(10)), 
      *           LPFILE.LP_TRCD, 
      *           LPFILE.LP_REFNO, 
      *           LPFILE.LP_TRAMT,
      *           LPFILE.LP_APOTH,
      *           LPFILE.LP_APOT2,
      *           LPFILE.LP_APINT,
      *           LPFILE.LP_APLC,
      *           LPFILE.LP_APCUR,
      *           LPFILE.LP_OTHBAL,
      *           LPFILE.LP_OT2BAL,
      *           LPFILE.LP_INTBAL,
      *           LPFILE.LP_LCBAL,
      *           LPFILE.LP_CURBAL,
      *           LPFILE.LP_INTDUE,
      *           LPFILE.LP_DLPROC,
      *           LPFILE.LP_EARNED_1,
      *           LPFILE.LP_EARNED_2,
      *           LPFILE.LP_EARNED_3,
      *           LPFILE.LP_INTPAID,
      *           LPFILE.LP_SSNO,
      *           CAST(LPFILE.LP_PDTH_DATE   AS VARCHAR(10)), 
      *           CAST(LPFILE.LP_LCPDTH_DATE AS VARCHAR(10)), 
      *           LPFILE.LP_TILLNO,
      *           LPFILE.LP_APINTOWE,
      *           LPFILE.LP_DLPARVHELD_ADJ,
      *           LPFILE.LP_IBPC, 
      *           LPFILE.LP_MANUAL_RB, 
      *           LPFILE.LP_REV, 
      *           LPFILE.LP_REVTRCD, 
      *           LPFILE.LP_REVSEQNO,
      *           LPFILE.LP_STATUSFG, 
      *           LPFILE.LP_NEWSTAT_RATE,
      *           LPFILE.LP_LCPARTIALS,
      *           LPFILE.LP_LCPAID,
      *           LPFILE.LP_DLRVCHGS_ADJ_1,
      *           LPFILE.LP_DLRVCHGS_ADJ_2,
      *           LPFILE.LP_DLRVCHGS_ADJ_3,
      *           LPFILE.LP_DLPARVPAID_ADJ,
      *           LPFILE.LP_DLNO,
      *           LPFILE.LP_PREPAY_PENALTY,
      *           LPFILE.LP_PRORATED_INS_REB,
      *           LPFILE.LP_POSTING_BR,
      *           LPFILE.LP_NOLCHG,
      *           LPFILE.LP_COLLECTOR,
      *           LPFILE.LP_EXTRA_PRIN_LFP,
      *           LPFILE.LP_REMAIN_INT_SINCE_LFP,
      *           LPFILE.LP_REMAIN_PRIN_SINCE_LFP,
      *           ISNULL(LXGFILE.LXG_BRNO, 0),
      *           ISNULL(LXGFILE.LXG_ACCTNO, 0),
      *           ISNULL(LXGFILE.LXG_SEQNO, 0),
      *           ISNULL(LXGFILE.LXG_GLNO_1, 0),
      *           ISNULL(LXGFILE.LXG_GLNO_2, 0),
      *           ISNULL(LXGFILE.LXG_GLNO_3, 0),
      *           ISNULL(LXGFILE.LXG_GLAMT_1, 0),
      *           ISNULL(LXGFILE.LXG_GLAMT_2, 0),
      *           ISNULL(LXGFILE.LXG_GLAMT_3, 0),
      *           ISNULL(LXFILE.LXR_REB_AMT, 0)
      * 
      *      FROM DBO.LPFILE LPFILE
      * 	 
      *      LEFT OUTER JOIN
      *           DBO.LXGFILE LXGFILE
      *        ON (  LPFILE.LP_BRNO           = LXGFILE.LXG_BRNO
      *       AND    LPFILE.LP_ACCTNO         = LXGFILE.LXG_ACCTNO
      *       AND    LPFILE.LP_SEQNO          = LXGFILE.LXG_SEQNO )
      * 
      *      LEFT OUTER JOIN
      *           DBO.LXFILE LXFILE
      *        ON (  LPFILE.LP_BRNO           = LXFILE.LX_BRNO
      *       AND    LPFILE.LP_ACCTNO         = LXFILE.LX_ACCTNO
      *       AND    LPFILE.LP_SEQNO          = LXFILE.LX_SEQNO )
      *   
      *     WHERE (  LPFILE.LP_BRNO           = @QLP1_WBEG_BRNO  
      *       AND    LPFILE.LP_POSTDATE BETWEEN @QWS_BEG_DATE
      *                                     AND @QWS_END_DATE 
      *       AND (( @QWS_REV_ONLY            = 'N' )
      *        OR  ( @QWS_REV_ONLY            = 'Y'
      *       AND    LPFILE.LP_REV            = 'Y' )))
      * 
      *     ORDER BY LPFILE.LP_BRNO,
      *              LPFILE.LP_ACCTNO,
      *              LPFILE.LP_SEQNO;


           MOVE BEG-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QWS-BEG-DATE.
           MOVE END-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QWS-END-DATE.
           MOVE REV-ONLY            TO QWS-REV-ONLY.

           EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LP1_CURSOR CURSOR FOR
                :LP1-RETURN-CODE = EXEC SSP_LPEXTR_LPFILE_SELECT 
              ( :QLP1-WBEG-BRNO, 
                :QWS-BEG-DATE,   
                :QWS-END-DATE,
                :QWS-REV-ONLY )
           END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WEND-BRNO
                          QLP1-WBEG-ACCTNO
                          QLP1-WEND-ACCTNO
                          QLP1-WBEG-SEQNO
                          QLP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-LP1-FILE-NEXT. *> EMBEDDED - LP,LXG,LX
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.
           INITIALIZE QLXG-REC.
           INITIALIZE QLX-REC.

           EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC.

           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH LP1_CURSOR 
                  INTO 
                    :QLP-BRNO,
                    :QLP-ACCTNO,
                    :QLP-SEQNO,
                    :QLP-USERID, 
                    :QLP-POSTDATE,
                    :QLP-POSTTIME,
                    :QLP-TRDATE, 
                    :QLP-PAYDATE, 
                    :QLP-TRCD, 
                    :QLP-REFNO, 
                    :QLP-TRAMT,
                    :QLP-APOTH,
                    :QLP-APOT2,
                    :QLP-APINT,
                    :QLP-APLC,
                    :QLP-APCUR,
                    :QLP-OTHBAL,
                    :QLP-OT2BAL,
                    :QLP-INTBAL,
                    :QLP-LCBAL,
                    :QLP-CURBAL,
                    :QLP-INTDUE,
                    :QLP-DLPROC,
                    :QLP-EARNED-1,
                    :QLP-EARNED-2,
                    :QLP-EARNED-3,
                    :QLP-INTPAID,
                    :QLP-SSNO,
                    :QLP-PDTH-DATE, 
                    :QLP-LCPDTH-DATE, 
                    :QLP-TILLNO,
                    :QLP-APINTOWE,
                    :QLP-DLPARVHELD-ADJ,
                    :QLP-IBPC, 
                    :QLP-MANUAL-RB, 
                    :QLP-REV, 
                    :QLP-REVTRCD, 
                    :QLP-REVSEQNO,
                    :QLP-STATUSFG, 
                    :QLP-NEWSTAT-RATE,
                    :QLP-LCPARTIALS,
                    :QLP-LCPAID,
                    :QLP-DLRVCHGS-ADJ-1,
                    :QLP-DLRVCHGS-ADJ-2,
                    :QLP-DLRVCHGS-ADJ-3,
                    :QLP-DLPARVPAID-ADJ,
                    :QLP-DLNO,
                    :QLP-PREPAY-PENALTY,
                    :QLP-PRORATED-INS-REB,
                    :QLP-POSTING-BR,
                    :QLP-NOLCHG,
                    :QLP-COLLECTOR,
                    :QLP-EXTRA-PRIN-LFP,
                    :QLP-REMAIN-INT-SINCE-LFP,
                    :QLP-REMAIN-PRIN-SINCE-LFP,
                    :QLXG-BRNO, 
                    :QLXG-ACCTNO,  
                    :QLXG-SEQNO, 
                    :QLXG-GLNO-1,  
                    :QLXG-GLNO-2,  
                    :QLXG-GLNO-3,  
                    :QLXG-GLAMT-1, 
                    :QLXG-GLAMT-2, 
                    :QLXG-GLAMT-3,
                    :QLXR-REB-AMT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS
              PERFORM GET-LXG-FIELDS
              PERFORM GET-LX-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LP1-FILE.

           PERFORM CLOSE-IT.
           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LP1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LP1-CONNECTION.

           IF LP1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LP1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LP1-CONN-OPEN-SW.

       COPY "LIBLP/LPLXG1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
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
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LPEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
