       IDENTIFICATION DIVISION.
       PROGRAM-ID.   AVGRPT.
       DATE-WRITTEN. 061913.
      ******************************************************************
      *                      (LP)
      *
      *    DESCRIPTION:  OPEN LOAN AVG TERM/AMT/WEIGHT REPORT
      *                  WORLD PR# 899
      *
      * REV:
      * BAH 170508 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181203 GLOBAL LNFILE
      * BAH 20190924 REMOVED ACCESS-CALL LNFILE
      * BAH 20200827 REMOVED RESET ON BR1 
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/AVGRPT.CBL S35 02/20/24-13:49:56 >".

      ******************************************************************
      *
      *     F I L E   I F N
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
      *-----------------------------------------------------------------
      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB-1                PIC 9(2)      COMP-3.
       01  SUB-1-MAX            PIC 9(2)      COMP-3.
       01  SUB-2                PIC 9(2)      COMP-3.
       01  SUB-2-MAX            PIC 9(2)      COMP-3.
       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
       01  WEIGHT-AMT           PIC S9(9)V99 VALUE 0.

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".

       01  LEGEND               PIC X(70).

      ******************************************************************
      *
      *     S C R E E N - F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH                       PIC 9(4).
           03  BEG-CLASS                        PIC 9(3).
           03  END-CLASS                        PIC 9(3).
           03  SUMMARY-ONLY                     PIC X.
           03  GTT-ISO                          PIC X(1).
           03  GROUP-FLAG                       PIC X.
           03  ENT-GROUP                        PIC X(4).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE            PIC X(1)     OCCURS 10.

       01  VERSION-CONTROL                      PIC X        VALUE "G".

       01  SV-BEG-BR                            PIC 9999     VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *
      *    H E A D E R S
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
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(18)    VALUE " ".
           03  FILLER           PIC X(38)    VALUE
               " OPEN LOAN AVG TERM / LOAN AMT REPORT ".
           03  FILLER           PIC X(50)     VALUE  SPACES.

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(46)    VALUE
               "BRNO ENT DATE LOAN #         TERM     LOAN AMT".
           03  FILLER           PIC X(14)    VALUE
               "    TERM X AMT".

      ******************************************************************
      *    D E T A I L - L I N E
      ******************************************************************
       01  DETAIL-LINE                PIC X(132).
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO                 PIC ZZZ9.
           03  FILLER                 PIC X.
           03  D-ENTDATE              PIC 99/99/99.
           03  FILLER                 PIC X.
           03  D-LOAN-NUM             PIC X(14).
           03  FILLER                 PIC X.
           03  D-ORGTERM              PIC ZZ9.
           03  FILLER                 PIC X(4).
           03  D-LNAMT                PIC ZZZ,ZZ9.99.
           03  FILLER                 PIC X.
           03  D-WEIGHT-AMT           PIC ZZZ,ZZZ,ZZ9.99.
       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  D1-GRNAME              PIC X(12).
           03  FILLER REDEFINES D1-GRNAME.
               05  D1-BR-PROMPT       PIC X(8).
               05  D1-BRNO            PIC ZZZ9.
           03  FILLER                 PIC X(2).
           03  D-TOT-PROMPT1          PIC X(11).
           03  D-TOTAL-OPEN           PIC Z,ZZZ,ZZZ,ZZZ.
           03  D-TOT-PROMPT2          PIC X(12).
           03  D-TOTAL-TERM           PIC ZZZ,ZZZ,ZZZ,ZZZ.
           03  D-TOT-PROMPT3          PIC X(13).
           03  D-TOTAL-LNAMT          PIC ZZZ,ZZZ,ZZZ,ZZZ.99-.
           03  D-TOT-PROMPT4          PIC X(16).
           03  D-TOTAL-WEIGHT         PIC ZZZ,ZZZ,ZZZ,ZZZ.99-.

      ******************************************************************
      *
      *    W S - G T T - T A B L E
      *
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      *    WS-GTT-TABLE - TOTALS TABLULATED FOR BORROWERS IN
      *                   REFERENCE DATE RANGE:
      *    TRACK TOTAL BORROWERS WITH OPEN LOAN AS OF BEG DATE
      *    TRACK BORROWERS WITH DEL LOAN IN SELECTED DATE RANGE
      ******************************************************************
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
               05  WS-GTT-TOT-OPEN       PIC S9(11)    COMP.
               05  WS-GTT-TOT-TERM       PIC S9(11)    COMP.
               05  WS-GTT-TOT-LNAMT      PIC S9(11)V99 COMP.
               05  WS-GTT-TOT-WEIGHT     PIC S9(13)V99 COMP.

      *-----------------------------------------------------------------
      *01  WS-GTT-ACTION-SUB               PIC 9(1)     VALUE ZEROES.
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/RANGE_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/AVGRPT_DEF.CPY".
       COPY "LIBLP/AVGRPT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/AVGRPT_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************************
       MAIN-MODULE SECTION.
      *************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "AVGRPT" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              GO TO GET-BR.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
       NEXT-GROUP-LOC.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              MOVE ZEROS TO WGR-SCURR-TABLE
              MOVE 15 TO WGR-TPLUS
              PERFORM GTT-TOTAL-LEVEL-CHECK
              GO TO END-ROUTINE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WGR-SCURR-TABLE TO GTT-PREV-SCURR-TABLE
           ELSE
              PERFORM GTT-TOTAL-LEVEL-CHECK.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.
           GO TO MOVE-BRANCH-PATH-INFO.
       GET-BR.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.


           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.

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
           IF LN-OWNBR NOT = BR-NO
              GO TO END-OF-GROUP.

           IF LN-POCD-NONLOAN
              GO TO NEXT-LN.
           IF LN-POFFDATE NOT = 0
              GO TO NEXT-LN.
           IF LN-PLDATE NOT = 0
              GO TO NEXT-LN.
           IF LN-CURBAL = 0
              GO TO NEXT-LN.

           MOVE "Y" TO RECORDS-FOUND.
       UPDATE-TOTALS.
           PERFORM DETAIL-LINE-PROCESS.

           IF GROUP-FLAG = "N"
              MOVE 9 TO GTT-TOTAL-SUB
              PERFORM GTT-ADD-TOTALS
           ELSE
              PERFORM GTT-CHECK-ADD-TOTALS.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF GROUP-FLAG = "Y"
              GO TO NEXT-GROUP-LOC
           ELSE
              PERFORM GTT-PRINT-GROUP-BR-TOTALS
              GO TO END-ROUTINE.

      *=================================================================
       END-ROUTINE.

           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           GO TO END-PROGRAM.

      *************************************************
       DETAIL-LINE-PROCESS SECTION.
      *************************************************
           COMPUTE WEIGHT-AMT ROUNDED = LN-ORGTERM * LN-LNAMT.
           IF SUMMARY-ONLY = "Y"
              GO TO EXIT-DETAIL-LINE-PROCESS.

           MOVE LN-OWNBR TO D-BRNO.
           MOVE LN-ENTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-ENTDATE.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO BW-SOLICIT.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LN-ORGTERM TO D-ORGTERM.
           MOVE LN-LNAMT TO D-LNAMT.
           MOVE WEIGHT-AMT TO D-WEIGHT-AMT.
           PERFORM WRITE-DETAIL-LINE.
       EXIT-DETAIL-LINE-PROCESS.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE AVGRPT-QUEUE-POS TO Q-POS.
           MOVE AVGRPT-COPIES-POS TO C-POS.
           MOVE AVGRPT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.
           MOVE 0      TO PAGE-NUMBER.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "OPEN LOAN AVG TERM/LNAMT REPORT IN PROGRESS"
                             TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF BEG-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           MOVE 1  TO SUB-1.
           MOVE 1  TO SUB-2.
           MOVE 10 TO SUB-1-MAX.
           MOVE 7  TO SUB-2-MAX.
           MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE.

           IF ( GROUP-FLAG = "Y" )
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y"    TO TOTAL-LEVEL-TABLE(9).

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF ( VDUSTATUS = "1U" )
              GO TO GTT-ISO-PARAGRAPH.

      *-----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
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
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              GO TO CL-01.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CL-01.
           MOVE LEGEND-CL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       CL-02.
           MOVE LEGEND-CL-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.

           GO TO SUMMARY-ONLY-YN.

      *-----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       SUMMARY-ONLY-YN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO CL-02.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO SUMMARY-ONLY-YN.
           MOVE VDUBUF TO SUMMARY-ONLY.
           IF SUMMARY-ONLY NOT = "Y"
              IF SUMMARY-ONLY NOT = "N"
                 GO TO SUMMARY-ONLY-YN.
      *-----------------------------------------------------------------
       GTT-ISO-PARAGRAPH.

           MOVE "I" TO GTT-ISO.

           IF GROUP-FLAG = "Y"
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(9).

      *=================================================================
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *    --------------------------------------
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *    --------------------------------------
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

      *******************************************
       WRITE-DETAIL-LINE SECTION.
      *******************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************************
       HEAD-PAGE SECTION.
      *******************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE  HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 2 TO LN-FEED.
           MOVE HEAD5 TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************
           ADD 1 TO WS-GTT-TOT-OPEN(GTT-TOTAL-SUB).
           ADD LN-ORGTERM TO WS-GTT-TOT-TERM(GTT-TOTAL-SUB).
           ADD LN-LNAMT TO WS-GTT-TOT-LNAMT(GTT-TOTAL-SUB).
           ADD WEIGHT-AMT TO WS-GTT-TOT-WEIGHT(GTT-TOTAL-SUB).
       EXIT-GTT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-BR-TOTALS SECTION.
      ******************************************************************

           MOVE "BRANCH #" TO D1-BR-PROMPT.
           MOVE BR-NO TO D1-BRNO.
           MOVE 9                     TO GTT-TOTAL-SUB.
           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      ******************************************************************

           MOVE GR-NAME TO D1-GRNAME.
           PERFORM GTT-PRINT-TOTALS.

      ******************************************************************
       GTT-PRINT-TOTALS SECTION.
      ******************************************************************
           MOVE 2 TO LN-FEED.

       GTT-PRT-TOTALS-NEXT.
           MOVE "TOTAL OPEN:" TO D-TOT-PROMPT1.
           MOVE WS-GTT-TOT-OPEN(GTT-TOTAL-SUB) TO D-TOTAL-OPEN.
           MOVE " TOTAL TERM:" TO D-TOT-PROMPT2.
           MOVE WS-GTT-TOT-TERM(GTT-TOTAL-SUB) TO D-TOTAL-TERM.
           MOVE " TOTAL LNAMT:" TO D-TOT-PROMPT3.
           MOVE WS-GTT-TOT-LNAMT(GTT-TOTAL-SUB) TO D-TOTAL-LNAMT.
           MOVE " TOTAL WEIGHTED:" TO D-TOT-PROMPT4.
           MOVE WS-GTT-TOT-WEIGHT(GTT-TOTAL-SUB) TO D-TOTAL-WEIGHT.

           PERFORM WRITE-DETAIL-LINE.
           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      ******************************************************************
      *
      *   CHECK FOR LEVEL SUMMARY TOTALS:
      *   WGR-SCURR IS TABLE OF POINTERS FOR CURRENT GROUP WHICH POINT
      *   TO WGR-SID TABLE TO SHOW CURRENT GROUP HIERARCHY:
      *   WGR-SCURR(1) POINTS TO WGR-SID OF LOW LEVEL (SELECTED GROUP);
      *   WGR-SCURR(WGR-TPLUS) POINTS TO THE WGR-SID OF THE HIGHEST
      *   LEVEL WHICH IS THE BRANCH NUMBER CURRENTLY BEING PROCESSED.
      *   THIS ROUTINE COMPARES THE WGR-SCURR TABLE TO THE TABLE SAVED
      *   FROM THE PREVIOUS GROUP, PRINTING & ZEROING TOTALS FOR EACH
      *   LEVEL WHICH HAS CHANGED STARTING WITH THE HIGHEST (BRANCH)
      *   LEVEL & WORKING THROUGH HIERARCHY.  GRAND TOTALS ARE FORCED BY
      *   MOVING 0 TO WGR-SCURR TABLE & 15 TO WGR-TPLUS.
      *
      ******************************************************************
      *
      *    G T T - T O T A L - L E V E L - C H E C K
      *
      ******************************************************************
       GTT-TOTAL-LEVEL-CHECK SECTION.

           MOVE 15 TO GTT-GROUP-SUB.

       GTT-LEVEL-TOTAL-LOOP.

           IF ( GTT-GROUP-SUB = 0 )
              GO TO GTT-EXIT-TOTAL-LEVEL-CHECK.

           IF (WGR-SCURR(GTT-GROUP-SUB) = GTT-PREV-SCURR(GTT-GROUP-SUB))
              GO TO GTT-CONTINUE-LOOP.

           MOVE GTT-PREV-SCURR(GTT-GROUP-SUB) TO GTT-PREV-SUB.

           IF ( GTT-PREV-SUB = 0 )
              GO TO GTT-CONTINUE-LOOP.

           IF ( WGR-SID(GTT-PREV-SUB) NUMERIC )
              GO TO GTT-CHECK-BRANCH-SID.

           MOVE WGR-SID(GTT-PREV-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.

           IF ( TOTAL-LEVEL-TABLE(GR-LEVEL) NOT = "Y" )
              GO TO GTT-CONTINUE-LOOP.

           MOVE GR-LEVEL TO GTT-TOTAL-SUB.

       GTT-DO-GROUP-TOTALS.

           PERFORM GTT-PRINT-GROUP-TOTALS.
           GO TO GTT-CONTINUE-LOOP.

       GTT-CHECK-BRANCH-SID.

           IF ( TOTAL-LEVEL-TABLE(9) NOT = "Y" )
              GO TO GTT-CONTINUE-LOOP.

           PERFORM GTT-PRINT-GROUP-BR-TOTALS.

       GTT-CONTINUE-LOOP.

           SUBTRACT 1 FROM GTT-GROUP-SUB.

           IF ( GTT-GROUP-SUB < WGR-SSTABLE )
              GO TO GTT-EXIT-TOTAL-LEVEL-CHECK.

           GO TO GTT-LEVEL-TOTAL-LOOP.

       GTT-EXIT-TOTAL-LEVEL-CHECK.
           MOVE WGR-SCURR-TABLE TO GTT-PREV-SCURR-TABLE.

      ******************************************************************
       GTT-CHECK-ADD-TOTALS SECTION.
      ******************************************************************

           MOVE WGR-TPLUS TO GTT-GROUP-SUB.

       GTT-CHECK-TOTAL-LOOP.

           IF ( GTT-GROUP-SUB = 0 )
              GO TO GTT-EXIT-CHECK-TOTALS.

           MOVE WGR-SCURR(GTT-GROUP-SUB) TO GTT-LEVEL-SUB.

           IF ( WGR-SID(GTT-LEVEL-SUB) IS NUMERIC )
              MOVE 9 TO GTT-TOTAL-SUB
              GO TO GTT-CHECK-TOTAL-FLAG.

           MOVE WGR-SID(GTT-LEVEL-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.
           MOVE GR-LEVEL TO GTT-TOTAL-SUB.

       GTT-CHECK-TOTAL-FLAG.

           IF ( TOTAL-LEVEL-TABLE(GTT-TOTAL-SUB) NOT = "Y" )
              GO TO GTT-CONTINUE-CHECK-TOTALS.

           PERFORM GTT-ADD-TOTALS.

       GTT-CONTINUE-CHECK-TOTALS.

           SUBTRACT 1 FROM GTT-GROUP-SUB.

           IF ( GTT-GROUP-SUB > 0 )
              GO TO GTT-CHECK-TOTAL-LOOP.

       GTT-EXIT-CHECK-TOTALS.
           EXIT.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO AVGRPT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO AVGRPT-SYSDATE.
           DISPLAY AVGRPT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE AVGRPT-FORM-FIELDS TO WR-BUF.
           MOVE AVGRPT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE AVGRPT-HELP--FILE TO HELP-LINK-FILE.
           MOVE AVGRPT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/AVGRPT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPMENU " TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY AVGRPT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
