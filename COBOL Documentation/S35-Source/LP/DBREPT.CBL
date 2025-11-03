       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DBREPT.
       DATE-WRITTEN. 02/28/2018.
      ****************************************************************
      *                      (LP)
      *    DESCRIPTION:  DEBIT CARD PROCEEDS NETSPEND REPORT
      *            IN END OF DAY BATCH (3/13/2023)
      *
      * REV:
      * BAH 180222 NEW, WORLD #1306
      * BAH 181207 GLOBAL LNFILE
      *  CS 181220 GLOBAL LTDFILE
      * BAH 2019.0515 DETAIL LINE WAS MISSING CLEARED-AMT, QA0013
      * BAH 2019.0524 SHOULD ONLY BE PRINTING BW-LNAME, QA0013
      * BAH 20190920 REMOVED ACCESS-CALL ON LNFILE
      * BAH 2020.0611 MISSING FIELD VOIDS IN BATCH DISPLAY-FIELDS,
      *               SHOULD BE "VOID" NOT "VOIDS". #1442
      * BAH 20200827 REMOVED RESET ON BR1 
      * BAH 20211123 CHANGED H-TITLE TO DEBIT CARD PROCEEDS REPORT #1527
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/DBREPT.CBL S35 02/20/24-13:40:02 >".
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
       COPY "LIBLP/LP01LTD.CPY".
       COPY "LIBLP/LP01LTD_SQL.CPY".
       COPY "LIBLP/LPWSLTD.CPY".
       01  PR-IFN               PIC X(2)     VALUE "PR".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      **********************
      *   MISC WORKERS     *
      **********************
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
           88  SKIP-RECORDS-FOUND         VALUE " ".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  BRANCH-TOTAL-COUNT      PIC S9(9)     COMP-3 VALUE 0.
       01  BRANCH-TOTAL-AMOUNT     PIC S9(11)V99 COMP-3 VALUE 0.
       01  GRAND-TOTAL-COUNT       PIC S9(9)     COMP-3 VALUE 0.
       01  GRAND-TOTAL-AMOUNT      PIC S9(11)V99 COMP-3 VALUE 0.

       01  LEGEND               PIC X(70).
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-LOANDATE    PIC 9(8).
           03  END-LOANDATE    PIC 9(8).
           03  INCLUDE-VOIDS   PIC X.
           03  INCLUDE-CLEARED PIC X.
           03  PAGEBR-FG       PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

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
           03  FILLER           PIC X(16)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(16)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(18)  VALUE " ".
           03  FILLER           PIC X(30) VALUE
               "DEBIT CARD PROCEEDS REPORT".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC X(2)     VALUE " ".
           03  HD-BRNO     PIC 9999.
           03  FILLER      PIC X        VALUE " ".
           03  HD-BRNAME   PIC X(40).

       01  HEAD4.
           03  FILLER           PIC X(38)  VALUE
           "NUMBER  BORROWER LNAME    LOANDATE    ".
           03  FILLER           PIC X(43)  VALUE
           "CARD #        AMOUNT    VOIDED    CLEARED".

       01  DETAIL-LINE          PIC X(96)  VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM       PIC Z(6).
           03  FILLER           PIC X(2).
           03  D-LNAME          PIC X(16).
           03  FILLER           PIC X(2).
           03  D-LOANDATE       PIC 99/99/99 BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-DEBIT-CARD-NUM PIC Z(10).
           03  FILLER           PIC X(2).
           03  D-DEBIT-CARD-AMT PIC Z(7).99-.
           03  FILLER           PIC X(2).
           03  D-VOID           PIC 99/99/99 BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-CLEARED        PIC 99/99/99 BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-CLEARED-AMT    PIC Z(7).99-.

       01  SUMLINE1 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(30).
           03  S-TEXT           PIC X(10).
           03  FILLER           PIC X(3).
           03  S-NOLOANS        PIC ZZZ,ZZZ,Z99.
           03  FILLER           PIC X(4).
           03  S-AMOUNT          PIC ZZZ,ZZZ,ZZ9.99.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(60).
           03  RANGE-DATE REDEFINES RANGE-SELECTION PIC 9999/99/99.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/PHON400.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/DBREPT_DEF.CPY".
       COPY "LIBLP/DBREPT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/DBREPT_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      **************************************************************
       MAIN-MODULE SECTION.
      **************************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DBREPT" TO VDU-FORM-NAME.

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
      D    STOP MESS.

           MOVE "Y" TO NEW-BRANCH.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTD-PATH-OVERRIDE

           PERFORM OPEN-LTD1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE LTD-PATH-OWNBR TO LTD-BRNO.
           MOVE 0              TO LTD-ACCTNO LTD-SEQNO.


           PERFORM START-LTD1-FILE.

       NEXT-LTD-REPORT.
           PERFORM READ-LTD1-FILE-NEXT.
           IF IO-FG = 9 OR (LTD-BRNO NOT = LTD-PATH-OWNBR)
              GO TO END-OF-GROUP.

      *    UNLOCK LTD-FILE.

           IF LTD-LOANDATE < BEG-LOANDATE OR LTD-LOANDATE > END-LOANDATE
              GO TO NEXT-LTD-REPORT.

           IF INCLUDE-VOIDS = "O"
              IF LTD-VOID-DATE = 0
                 GO TO NEXT-LTD-REPORT.
           IF INCLUDE-VOIDS = "S"
              IF LTD-VOID-DATE NOT = 0
                 GO TO NEXT-LTD-REPORT.

           IF INCLUDE-CLEARED = "O"
              IF LTD-CLEARED-DATE = 0
                 GO TO NEXT-LTD-REPORT.
           IF INCLUDE-CLEARED = "S"
              IF LTD-CLEARED-DATE NOT = 0
                 GO TO NEXT-LTD-REPORT.

           MOVE LTD-BRNO   TO LN-OWNBR.
           MOVE LTD-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO NEXT-LTD-REPORT.

           IF NEW-BRANCH = "Y"
              MOVE "N" TO NEW-BRANCH
              IF PAGEBR-FG = "Y"
                 PERFORM HEAD-PAGE
              ELSE
                 PERFORM HEAD-BRANCH.

           MOVE LN-ACCTNO  TO D-LOAN-NUM.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 0
              MOVE BW-LNAME TO D-LNAME.

           MOVE LTD-LOANDATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO D-LOANDATE.
           MOVE LTD-DEBIT-CARD-NUMBER TO D-DEBIT-CARD-NUM.
           MOVE LTD-DEBIT-CARD-AMT    TO D-DEBIT-CARD-AMT.
           MOVE LTD-VOID-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO D-VOID.
           MOVE LTD-CLEARED-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO D-CLEARED.
           MOVE LTD-CLEARED-AMT       TO D-CLEARED-AMT.
           PERFORM WRITE-DETAIL-LINE.

           ADD 1                  TO BRANCH-TOTAL-COUNT.
           ADD LTD-DEBIT-CARD-AMT TO BRANCH-TOTAL-AMOUNT.

           GO TO NEXT-LTD-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LTD1-FILE.
 
           IF BRANCH-TOTAL-COUNT NOT = 0
              PERFORM BRANCH-TOTALS.

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
           MOVE DBREPT-QUEUE-POS TO Q-POS.
           MOVE DBREPT-COPIES-POS TO C-POS.
           MOVE DBREPT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO BRANCH-TOTAL-COUNT BRANCH-TOTAL-AMOUNT.
           MOVE 0 TO GRAND-TOTAL-COUNT GRAND-TOTAL-AMOUNT.

           MOVE 0 TO PAGE-NUMBER.

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
           MOVE EXT-EOBUF-DATE1 TO BEG-LOANDATE.
           MOVE EXT-EOBUF-DATE2 TO END-LOANDATE.

      ********************************************
       ENTER-PARAMETERS SECTION.
      ********************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO PAGE-ON-BRANCH.

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
              GO TO LOANDATE-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
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
           GO TO LOANDATE-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
      *----------------------------------------------------------------
       LOANDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-VOIDS-ISO.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "16" GO TO ALL-LN-DATES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LOANDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-LOANDATE.
      *-----------------------------------------------------------
       LOANDATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-VOIDS-ISO.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LOANDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO LOANDATE-02.

           MOVE VDU-DATE-YYYYMMDD TO END-LOANDATE.

           IF END-LOANDATE < BEG-LOANDATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO LOANDATE-01.

           GO TO ENTER-VOIDS-ISO.

      *----------------------------------------------------------
       ALL-LN-DATES.

           MOVE "S  8080DATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-LOANDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-LOANDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-VOIDS-ISO.
           MOVE "E NA010VOID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LOANDATE-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENTER-VOIDS-ISO.
           IF VDUBUF NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO ENTER-VOIDS-ISO.
           MOVE VDUBUF TO INCLUDE-VOIDS.
      *----------------------------------------------------------------
       ENTER-CLEARED-ISO.
           MOVE "E NA010CLEARED." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-VOIDS-ISO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-CLEARED-ISO.
           IF VDUBUF NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO ENTER-CLEARED-ISO.
           MOVE VDUBUF TO INCLUDE-CLEARED.
      *----------------------------------------------------------------
       PAGE-ON-BRANCH.
           MOVE "E NA010PAGEBR."    TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-CLEARED-ISO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PAGE-ON-BRANCH.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              GO TO PAGE-ON-BRANCH.
           MOVE VDUBUF TO PAGEBR-FG.

           GO TO EXIT-PARM.

       EXIT-PARM.
           EXIT.

      ***************************************
       DISPLAY-PARAMETERS SECTION.
      ***************************************
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

           MOVE "S  A010VOID." TO VDU.
           MOVE INCLUDE-VOIDS   TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010CLEARED." TO VDU.
           MOVE INCLUDE-CLEARED   TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010PAGEBR." TO VDU.
           MOVE PAGEBR-FG        TO VDUBUF.
           PERFORM SCREENIO.


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

      ********************************
       BRANCH-TOTALS SECTION.
      ********************************
           MOVE "TOTAL" TO S-TEXT.
           MOVE BRANCH-TOTAL-COUNT TO S-NOLOANS.
           MOVE BRANCH-TOTAL-AMOUNT TO S-AMOUNT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           ADD BRANCH-TOTAL-COUNT  TO GRAND-TOTAL-COUNT.
           ADD BRANCH-TOTAL-AMOUNT TO GRAND-TOTAL-AMOUNT.

           MOVE 0 TO BRANCH-TOTAL-COUNT BRANCH-TOTAL-AMOUNT.

      ********************************
       GRAND-TOTALS SECTION.
      ********************************
           MOVE "GRAND" TO S-TEXT.
           MOVE GRAND-TOTAL-COUNT TO S-NOLOANS.
           MOVE GRAND-TOTAL-AMOUNT TO S-AMOUNT.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ************************************
       PRINT-RANGE-LINES SECTION.
      ************************************

           MOVE 4                      TO LN-FEED.
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
           MOVE "BEGIN LOAN DATE  :" TO RANGE-LINE.
           MOVE BEG-LOANDATE         TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING           :" TO RANGE-LINE.
           MOVE END-LOANDATE         TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "INCLUDE VOIDS    :" TO RANGE-LINE.
           MOVE INCLUDE-VOIDS        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "INCLUDE CLEARED  :" TO RANGE-LINE.
           MOVE INCLUDE-CLEARED      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PAGE ON BRANCH     :" TO RANGE-LINE.
           MOVE PAGEBR-FG              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
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
           MOVE BR-NO TO HD-BRNO.
           MOVE BR-NAME TO HD-BRNAME.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ***********************
       HEAD-BRANCH SECTION.
      ***********************
           MOVE BR-NO       TO HD-BRNO.
           MOVE BR-NAME     TO HD-BRNAME.
           MOVE HEAD3       TO DETAIL-LINE.
           MOVE 2           TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2           TO LN-FEED.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE EXT-CONAME-CONAME TO DBREPT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DBREPT-SYSDATE.
           DISPLAY DBREPT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DBREPT-FORM-FIELDS TO WR-BUF.
           MOVE DBREPT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE DBREPT-HELP--FILE TO HELP-LINK-FILE.
           MOVE DBREPT-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/DBREPT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC-PARA SECTION.
      ********************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTD1RN.CPY".
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
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DBREPT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
