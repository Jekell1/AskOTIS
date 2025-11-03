       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DNSRPT.
       DATE-WRITTEN. 040705.
      ****************************************************************
      *                      (LP)
      *    DESCRIPTION:  BORROWER DO NOT SOLICIT REPORT
      *                  WORLD PR# 393
      *
      * REV:
      * BLF 050414 FIXED TO SKIP CHARGE-OFFS
      * 20171026 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181025 GLOBAL BWFILE
      * BAH 181207 GLOBAL LNFILE
      * BAH 20190925 REMOVED ACCESS-CALL BWFILE
      * BAH 20200827 REMOVED RESET ON BR1 
      * MB  20240305 PERFORMANCE TUNING.  ADDED STORED PROCEDURE
      *                TO RETREIVE LN AND BW DATA.
      *              REMOVED WORK FILE USE TO SORT BY BRANCH/BW-LNAME
      *              SSP SORTS BY BW-LNAME
      ****************************************************************
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

      ***********************************
      * SORTED LNFILE BY BRANCH, NAME
      ***********************************
      * WORKFILE USE REMOVED WITH 20240305 CHANGE
      * FD  WK-FILE
      *     LABEL RECORDS ARE STANDARD.
      * 01  WK-REC.
      *     03  WK-KEY.
      *         05  WK-BRANCH           PIC 9(4).
      *         05  WK-NAME             PIC X(30).
      *         05  WK-CREDIT           PIC 9(6)V99  COMP-3.
      *         05  WK-CLASS            PIC 9(3).
      *         05  WK-NUMBER           PIC 9(6).
      *         05  WK-POFFDATE         PIC 9(8)     COMP-3.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/DNSRPT.CBL J35 12/21/23-09:01:31 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       01  WK-IFN               PIC X(3)     VALUE "WK".


      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-ZBAL                   PIC X.
       01  PASS-BEGCREDITLIM           PIC 9(7).
       01  PASS-ENDCREDITLIM           PIC 9(7).
       01  PASS-BEGPODATE              PIC X(10).
       01  PASS-ENDPODATE              PIC X(10).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW            PIC X           VALUE "N".

      **********************
      *   MISC WORKERS     *
      **********************
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
           88  SKIP-RECORDS-FOUND         VALUE " ".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  NAME-WK              PIC X(70).

       01  TOTAL-COUNT          PIC S9(9)    COMP-3 VALUE 0.
       01  TOTAL-CREDIT         PIC S9(9)V99 COMP-3 VALUE 0.

       01  LEGEND               PIC X(70).
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  ZBAL            PIC X.
           03  BEG-CREDIT      PIC 9(6).
           03  END-CREDIT      PIC 9(6).
           03  BEG-POFFDATE    PIC 9(8).
           03  END-POFFDATE    PIC 9(8).

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(5)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(10)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(3)  VALUE " ".
           03  FILLER           PIC X(35) VALUE
               "  BORROWER DO NOT SOLICIT REPORT  ".

       01  HEAD3.
           03  FILLER           PIC X(39)  VALUE
           "BRNO  BORROWER NAME".
           03  FILLER           PIC X(42)  VALUE
           "ACCOUNT NO.        HI CREDIT   PAYOFF DATE".

       01  DETAIL-LINE          PIC X(80)  VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC XXX.
           03  D-NAME           PIC X(30).
           03  FILLER           PIC X(3).
           03  D-CLASS          PIC 99.
           03  D-DASH           PIC X.
           03  D-NUMBER         PIC Z(6).
           03  FILLER           PIC X(8).
           03  D-CREDIT         PIC ZZZ,ZZ9.99.
           03  FILLER           PIC X(4).
           03  D-POFFDATE       PIC 99/99/99 BLANK ZERO.

       01  SUMLINE1 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(30).
           03  S-TEXT           PIC X(5).
           03  FILLER           PIC X(3).
           03  S-NOLOANS        PIC ZZZ,ZZZ,Z99.
           03  FILLER           PIC X(4).
           03  S-CREDIT         PIC ZZZ,ZZZ,ZZ9.99.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(60).

       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/DNSRPT_DEF.CPY".
       COPY "LIBLP/DNSRPT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/DNSRPT_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                PR-FILE.           *> WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN3-FILE.
      *     PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
      *     CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      **************************************************
       MAIN-MODULE SECTION.
      **************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DNSRPT" TO VDU-FORM-NAME.

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
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE "Y" TO NEW-BRANCH.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

      *     PERFORM OPEN-BW1-FILE.

           PERFORM SORT-ROUTINE.
           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           GO TO END-OF-GROUP.

      *     PERFORM OPEN-WK-FILE.

      *     MOVE 0 TO WK-BRANCH WK-CLASS WK-NUMBER WK-CREDIT WK-POFFDATE.
      *     MOVE SPACES TO WK-NAME.

      *     PERFORM START-WK-FILE.
      * NEXT-LN-REPORT.
      *     PERFORM READ-WK-FILE-NEXT.
      *     IF IO-FG = 9
      *        GO TO END-OF-GROUP.

      *    MOVE WK-BRANCH TO D-BRNO.
      *    MOVE WK-CLASS TO D-CLASS.
      *    MOVE "-" TO D-DASH.
      *    MOVE WK-NUMBER TO D-NUMBER.
      *    MOVE WK-CREDIT TO D-CREDIT.
      *    MOVE WK-NAME TO D-NAME.
      *    MOVE WK-POFFDATE TO DATE-YYYYMMDD.
      *    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
      *    MOVE DATE-MMDDYY TO D-POFFDATE.
      *    IF NEW-BRANCH = "Y"
      *       MOVE "N" TO NEW-BRANCH
      *       MOVE 3 TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.
      *
      *    ADD WK-CREDIT TO TOTAL-CREDIT.
      *    ADD 1 TO TOTAL-COUNT.
      *
      *     GO TO NEXT-LN-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
      *     PERFORM CLOSE-BW1-FILE.
      *     PERFORM CLOSE-WK-FILE.
      *     PERFORM OPEN-WK-FILE-OUTPUT.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM
           ELSE
              MOVE "Y" TO RECORDS-FOUND.

           PERFORM GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE DNSRPT-QUEUE-POS  TO Q-POS.
           MOVE DNSRPT-COPIES-POS TO C-POS.
           MOVE DNSRPT-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO TOTAL-COUNT TOTAL-CREDIT.

           MOVE 0 TO PAGE-NUMBER.

      * WORKFILE USE REMOVED WITH 20240305 CHANGE
      *     MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
      *     PERFORM MKTEMP-CALL.
      *     PERFORM OPEN-WK-FILE-OUTPUT.
      *     PERFORM CLOSE-WK-FILE.
      *     PERFORM OPEN-WK-FILE.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.
           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ********************************************
       EO-EXEC-AUDIT SECTION.
      ********************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-POFFDATE.
           MOVE EXT-EOBUF-DATE2 TO END-POFFDATE.


      ********************************************
       ENTER-PARAMETERS SECTION.
      ********************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO ZBALYN.

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
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
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CL-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
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
           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE LEGEND-CL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
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
           GO TO CREDIT-01.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       CREDIT-01.
           MOVE LEGEND-CREDIT-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060CREDIT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CREDIT.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CREDIT-01.
           MOVE VDUNUM0 TO BEG-CREDIT.
       CREDIT-02.
           MOVE LEGEND-CREDIT-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060CREDIT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CREDIT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CREDIT-02.
           MOVE VDUNUM0 TO END-CREDIT.
           IF BEG-CREDIT > END-CREDIT
              MOVE "INVALID CREDIT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CREDIT-01.
           GO TO ZBALYN.
       ALL-CREDIT.
           MOVE "SNNN060CREDIT1." TO VDU.
           MOVE 0 TO BEG-CREDIT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060CREDIT2." TO VDU.
           MOVE 999999 TO END-CREDIT VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       ZBALYN.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZBALYN.

           MOVE VDUBUF TO ZBAL.

           IF ZBAL = "I"     GO TO POFFDATE-01.
           IF ZBAL = "O"     GO TO POFFDATE-01.
           IF ZBAL = "S"     GO TO END-PARM.
           GO TO ZBALYN.

      *----------------------------------------------------------------
       POFFDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO END-PARM.

           MOVE "ERNM060POFFDATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO POFFDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-POFFDATE.
      *-----------------------------------------------------------
       POFFDATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO END-PARM.

           MOVE "ERNM060POFFDATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO POFFDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO POFFDATE-02.

           MOVE VDU-DATE-YYYYMMDD TO END-POFFDATE.

           IF END-POFFDATE < BEG-POFFDATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO POFFDATE-01.

           GO TO END-PARM.

      *----------------------------------------------------------
       ALL-DATE.

           MOVE "S  8080POFFDATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-POFFDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080POFFDATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-POFFDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.
       EXIT-PARM.
           EXIT.

      ********************************************
       DISPLAY-PARAMETERS SECTION.
      ********************************************
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

           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL           TO VDUBUF.
           PERFORM SCREENIO.

      ********************************************
       SORT-ROUTINE SECTION.
      ********************************************
      D    STOP MESS.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS............", BR-NO
                      DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-LN3-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
      *     MOVE 0         TO LN-ACCTNO
      *                       QLN3-WBEG-ACCTNO.
      *     MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           MOVE BEG-CREDIT   TO PASS-BEGCREDITLIM.
           MOVE END-CREDIT   TO PASS-ENDCREDITLIM.
           MOVE ZBAL         TO PASS-ZBAL.

           IF ZBAL = "S"
              MOVE '1900-01-01' TO PASS-BEGPODATE
              MOVE '2049-12-31' TO PASS-ENDPODATE
           ELSE 
              MOVE BEG-POFFDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO PASS-BEGPODATE
              MOVE END-POFFDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO PASS-ENDPODATE.

           PERFORM START-LN3-FILE.  *> SSP LN BW

       NEXT-LN3.
           PERFORM READ-LN3-FILE-NEXT.  *> SSP LN BW
           IF IO-BAD                     *>OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO SORT-ROUTINE-EXIT.

      * ALSO CHECKED IN SSP
           IF LN-POCD = "**"
              GO TO NEXT-LN3.
           IF LN-PLDATE NOT = 0
              GO TO NEXT-LN3.

           PERFORM GET-BW.

      * ALSO CHECKED IN SSP
           IF BW-SOLICIT NOT = "N"
              GO TO NEXT-LN3.


      * ALSO CHECKED IN SSP
      *     IF LN-OWNBR  NOT = BR-NO
      *        GO TO NEXT-LN3.
      *     IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
      *        GO TO NEXT-LN3.

      * ALSO CHECKED IN SSP
           IF ZBAL = "O"
              IF ( LN-CURBAL NOT = 0 ) OR
                 ( LN-INTBAL NOT = 0 ) OR
                 ( LN-LCBAL  NOT = 0 ) OR
                 ( LN-OTHBAL NOT = 0 )
                     GO TO NEXT-LN3.

      * ALSO CHECKED IN SSP
           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                          GO TO NEXT-LN3.

      * ALSO CHECKED IN SSP
           IF LN-CREDITLIM < BEG-CREDIT OR LN-CREDITLIM > END-CREDIT
              GO TO NEXT-LN3.

           IF LN-POFFDATE = 0
              GO TO CONTINUE-SORT.

      * ALSO CHECKED IN SSP
           IF LN-POFFDATE < BEG-POFFDATE OR
               LN-POFFDATE > END-POFFDATE
              GO TO NEXT-LN3.

       CONTINUE-SORT.

      * WORKFILE USE REMOVED WITH 20240305 CHANGE
      *     MOVE LN-OWNBR     TO WK-BRANCH.
      *     MOVE LN-CLASS     TO WK-CLASS.
      *     MOVE LN-ACCTNO    TO WK-NUMBER.
      *     MOVE LN-CREDITLIM TO WK-CREDIT.
      *     MOVE LN-POFFDATE  TO WK-POFFDATE.
      *     MOVE NAME-WK      TO WK-NAME.
      *     PERFORM WRITE-WK-FILE.
      *     IF IO-FG NOT = 0
      *        GO TO IO-ERROR.

      * COPIED FROM NEXT-LN-REPORT - CHANGED TO USE LN FIELDS

           MOVE LN-OWNBR     TO D-BRNO.
           MOVE LN-CLASS     TO D-CLASS.
           MOVE "-"          TO D-DASH.
           MOVE LN-ACCTNO    TO D-NUMBER.
           MOVE LN-CREDITLIM TO D-CREDIT.
           MOVE NAME-WK      TO D-NAME.

           MOVE LN-POFFDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-POFFDATE.

           IF NEW-BRANCH = "Y"
              MOVE "N" TO NEW-BRANCH
              MOVE 3   TO LN-FEED.

           PERFORM WRITE-DETAIL-LINE.

           ADD LN-CREDITLIM TO TOTAL-CREDIT.
           ADD 1            TO TOTAL-COUNT.

           GO TO NEXT-LN3.

       SORT-ROUTINE-EXIT.
      *     PERFORM CLOSE-WK-FILE.

      ******************************************
       GET-BW SECTION.
      ******************************************
      * NEEDED BW DATA IN SSP
      *     MOVE LN-SSNO(1) TO BW-SSNO.
      *     PERFORM READ-BW1-FILE.
      *     IF IO-FG = 9
           IF BW-OWNBR = 0 
              MOVE "* * NOT FOUND * *" TO BW-LNAME
              MOVE " " TO BW-FNAME.

           MOVE BW-LNAME TO NAME-WK.
           INSPECT NAME-WK REPLACING FIRST "   " BY ", +".
           INSPECT NAME-WK REPLACING FIRST
               "+                                  " BY BW-FNAME.

      ********************************************
       WRITE-DETAIL-LINE SECTION.
      ********************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ********************************************
       GRAND-TOTALS SECTION.
      ********************************************
           MOVE "TOTAL" TO S-TEXT.
           MOVE TOTAL-COUNT TO S-NOLOANS.
           MOVE TOTAL-CREDIT TO S-CREDIT.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ********************************************
       PRINT-RANGE-LINES SECTION.
      ********************************************

           MOVE 4                      TO LN-FEED.
      *-------------------------------------------------------------
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING HI CREDIT:" TO RANGE-LINE.
           MOVE BEG-CREDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-CREDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ZERO BALANCE ACCTS :" TO RANGE-LINE.
           MOVE ZBAL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN PAYOFF DATE  :" TO RANGE-LINE.
           MOVE BEG-POFFDATE           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-POFFDATE           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ********************************************
       HEAD-PAGE SECTION.
      ********************************************
           MOVE "N" TO FIRST-TIME.
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
           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE EXT-CONAME-CONAME TO DNSRPT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DNSRPT-SYSDATE.
           DISPLAY DNSRPT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DNSRPT-FORM-FIELDS TO WR-BUF.
           MOVE DNSRPT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE DNSRPT-HELP--FILE TO HELP-LINK-FILE.
           MOVE DNSRPT-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/DNSRPT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN3-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LN3-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.   *>  SSP LN BW LC
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SSP_DNSRPT_LNFILE_SELECT
                  ( :QLN3-WBEG-OWNBR,   :QLN3-WBEG-CLASS, 
                    :QLN3-WEND-CLASS,   :PASS-ZBAL,
                    :PASS-BEGCREDITLIM, :PASS-ENDCREDITLIM,
                    :PASS-BEGPODATE,    :PASS-ENDPODATE) 
                END-EXEC.

      * SELECT LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SSNO1,
      *            LNFILE.LN_MAKERCD1,
      *            LNFILE.LN_SSNO2,
      *            LNFILE.LN_MAKERCD2,
      *            LNFILE.LN_SSNO3,
      *            LNFILE.LN_MAKERCD3,
      *            LNFILE.LN_SSNO4,
      *            LNFILE.LN_MAKERCD4,
      *            LNFILE.LN_POCD,
      *            CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_INTBAL,
      *            LNFILE.LN_LCBAL,
      *            LNFILE.LN_OTHBAL,
      *            LNFILE.LN_CREDITLIM,
      *            ISNULL(BWFILE.BW_OWNBR,0),
      *            ISNULL(BWFILE.BW_SSNO,0),
      *            ISNULL(BWFILE.BW_LNAME,''''),
      *            ISNULL(BWFILE.BW_FNAME,''''),
      *            ISNULL(BWFILE.BW_SOLICIT,'''')
      *   FROM DBO.LNFILE LNFILE
      *    LEFT OUTER JOIN DBO.BWFILE BWFILE
      *        ON BWFILE.BW_OWNBR = LNFILE.LN_OWNBR 
      *       AND BWFILE.BW_SSNO  = LNFILE.LN_SSNO1
      *    WHERE LNFILE.LN_OWNBR = @QLN3_WBEG_OWNBR
      *      AND LNFILE.LN_CLASS BETWEEN @QLN3_WBEG_CLASS AND @QLN3_WEND_CLASS'
      *      AND LNFILE.LN_PLDATE = ''1900-01-01'' 
      *       AND LNFILE.LN_CREDITLIM BETWEEN @BEGCREDITLIM AND @ENDCREDITLIM
      *       AND LNFILE.LN_POCD != ''**'' 
      *       AND (LNFILE.LN_POFFDATE = ''1900-01-01'' 
      *              OR (LNFILE.LN_POFFDATE BETWEEN @BEGPODATE AND @ENDPODATE))
      *       AND BWFILE.BW_SOLICIT IS NULL OR BWFILE.BW_SOLICIT = ''N'' '

      *   IF @ZBAL = 'O'
      *    SELECT @SQL += ' AND (LNFILE.LN_CURBAL = 0 AND LNFILE.LN_INTBAL = 0 AND LNFILE.LN_LCBAL = 0 AND LNFILE.LN_OTHBAL = 0)'  
      *   IF @ZBAL = 'S'
      *    SELECT @SQL += ' AND (LNFILE.LN_CURBAL <> 0 OR LNFILE.LN_INTBAL <> 0 OR LNFILE.LN_LCBAL <> 0 OR LNFILE.LN_OTHBAL <> 0)'  
      *   ORDER BY BWFILE.BW_NAME, BWFILE.BW_FNAME, LNFILE.LN_CREDITLIM, LNFILE.LN_CLASS, LNFILE.LN_ACCTNO

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.  *>  EMBEDDED LN BW  
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           INITIALIZE QLN-REC QBW-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
               FETCH LN3_CURSOR INTO 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1,
                   :QLN-SSNO2,
                   :QLN-MAKERCD2,
                   :QLN-SSNO3,
                   :QLN-MAKERCD3,
                   :QLN-SSNO4,
                   :QLN-MAKERCD4,
                   :QLN-POCD,
                   :QLN-PLDATE,
                   :QLN-POFFDATE,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-CREDITLIM,
                   :QBW-OWNBR,
                   :QBW-SSNO,
                   :QBW-LNAME,
                   :QBW-FNAME,
                   :QBW-SOLICIT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LN3-CONN-OPEN-SW.
      *-----------------------------------------------------------------

      * COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ********************************************
       END-PROGRAM SECTION.
      ********************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF ( SKIP-RECORDS-FOUND )
              GO TO EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

      * WORKFILE USE REMOVED WITH 20240305 CHANGE
      *     MOVE MKTEMP-BUF TO ACCESS-BUF.
      *     PERFORM REMOVE-WORKFILE.

           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPR2MU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DNSRPT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
