       IDENTIFICATION DIVISION.
       PROGRAM-ID.   BOREMP.
       DATE-WRITTEN. 022208.
      ****************************************************************
      *                      (LP)
      *    DESCRIPTION:  BORROWER EMPLOYER REPORT
      *                  WORLD PR# 571	
      *
      * REV:
      * BLF 080227 REMOVE DUPLICATE BORROWERS
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      * BAH 170227 FIXED ERROR 22, WK-FILE WAS NOT OPEN IF YOU HIT A BRANCH
      *            WITH NO LOANS.
      * BAH 170509 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181031 GLOBAL BWFILE
      * BAH 181203 GLOBAL LNFILE
      * BAH 20190924 REMOVED ACCESS-CALL BWFILE
      * BAH 20200827 REMOVED RESET ON BR1 
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ***********************************
      * SORTED LNFILE BY BRANCH, NAME
      ***********************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-LAST-FIRST   PIC X(30).
               05  WK-SSNO         PIC 9(9).
           03  WK-LOAN-NUM         PIC X(16).
           03  WK-BRANCH           PIC 9(4).
           03  WK-FIRST-LAST       PIC X(30).
           03  WK-CONX.
               05  WK-CON          PIC ZZZ9.
           03  WK-RECENCYX.
               05  WK-RECENCY      PIC ZZZ9.
           03  WK-PHONE            PIC 9(10).
           03  WK-EMPLOYER         PIC X(24).
           03  WK-POFFDATE         PIC 9(8).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/BOREMP.CBL S35 02/20/24-13:20:48 >".
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
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       01  PR-IFN               PIC X(2)     VALUE "PR".
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

      **********************
      *   MISC WORKERS     *
      **********************
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".


       01  TODAYS-DATE          PIC 9(8).
       01  CREATE-FG            PIC X.

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
           88  SKIP-RECORDS-FOUND         VALUE " ".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  HOLD-FIRST-LAST      PIC X(30).
       01  HOLD-LAST-FIRST      PIC X(30).

       01  TOTAL-COUNT          PIC S9(9)    COMP-3 VALUE 0.

       01  LEGEND               PIC X(70).
       COPY "LIBLP/EMSFRG_CMD.CPY".
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  PDOUTS          PIC X.
           03  BEG-POFFDATE    PIC 9(8).
           03  END-POFFDATE    PIC 9(8).
           03  BEG-PLDATE      PIC 9(8).
           03  END-PLDATE      PIC 9(8).
           03  PAGEBR-FG        PIC X.

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
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(36)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(34) VALUE " ".
           03  FILLER           PIC X(24) VALUE
               "BORROWER EMPLOYER REPORT".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC X(2)     VALUE " ".
           03  HD-BRNO     PIC 9999.
           03  FILLER      PIC X        VALUE " ".
           03  HD-BRNAME   PIC X(40).

       01  HEAD4.
           03  FILLER           PIC X(59)  VALUE
           "BRNO   ACCT NUMBER        BORROWER NAME".
           03  FILLER           PIC X(57)  VALUE
           "PHONE           CON     REC     EMPLOYER".

       01  DETAIL-LINE          PIC X(132)  VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC XXX.
           03  D-LOAN-NUM       PIC X(16).
           03  FILLER           PIC X(3).
           03  D-NAME           PIC X(30).
           03  FILLER           PIC X(3).
           03  D-PHONE          PIC X(12).
           03  FILLER           PIC X(4).
           03  D-CONX           PIC X(4).
           03  FILLER           PIC X(4).
           03  D-RECX           PIC X(4).
           03  FILLER           PIC X(4).
           03  D-EMPLOYER       PIC X(24).

       01  SUMLINE1 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(30).
           03  S-TEXT           PIC X(5).
           03  FILLER           PIC X(3).
           03  S-NOLOANS        PIC ZZZ,ZZZ,Z99.
           03  FILLER           PIC X(18).

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
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/PHON400.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BOREMP_DEF.CPY".
       COPY "LIBLP/BOREMP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BOREMP_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      **************************************************************
       MAIN-MODULE SECTION.
      **************************************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BOREMP" TO VDU-FORM-NAME.

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
                                              LN-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.

           PERFORM SORT-ROUTINE.

           PERFORM OPEN-WK-FILE.

           MOVE SPACES TO WK-REC.
           MOVE 0 TO WK-SSNO.
           PERFORM START-WK-FILE.
       NEXT-LN-REPORT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           MOVE WK-BRANCH TO D-BRNO.
           MOVE WK-LOAN-NUM TO D-LOAN-NUM.
           MOVE WK-FIRST-LAST TO D-NAME.
           IF WK-PHONE NOT = 0
              MOVE WK-PHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO D-PHONE.
           MOVE WK-CONX TO D-CONX.
           MOVE WK-RECENCYX TO D-RECX.
           MOVE WK-EMPLOYER TO D-EMPLOYER.
           IF NEW-BRANCH = "Y"
              MOVE "N" TO NEW-BRANCH
              IF PAGEBR-FG = "Y"
                 PERFORM HEAD-PAGE
              ELSE
                 PERFORM HEAD-BRANCH.
           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO TOTAL-COUNT.

           GO TO NEXT-LN-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
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

      /**********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
           MOVE BOREMP-QUEUE-POS TO Q-POS.
           MOVE BOREMP-COPIES-POS TO C-POS.
           MOVE BOREMP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO TOTAL-COUNT.

           MOVE 0 TO PAGE-NUMBER.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
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

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-SP1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ********************************************
      *    EO EXEC AUDIT
      ********************************************
       EO-EXEC-AUDIT SECTION.
           MOVE EXT-EOBUF-DATE1 TO BEG-POFFDATE.
           MOVE EXT-EOBUF-DATE2 TO END-POFFDATE.


      ********************************************
      *    ENTER PARAMETERS
      ********************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO PAGE-ON-BRANCH.

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
              GO TO PAIDOUTSYN.
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
           GO TO PAIDOUTSYN.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
      *----------------------------------------------------------------
       PAIDOUTSYN.
           MOVE "E  A010PDOUTS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PAIDOUTSYN.

           MOVE VDUBUF TO PDOUTS.

           IF PDOUTS = "Y"     GO TO POFFDATE-01.
           IF PDOUTS = "N"
              MOVE 0 TO BEG-POFFDATE END-POFFDATE
              GO TO PLDATE-01.
           GO TO PAIDOUTSYN.

      *----------------------------------------------------------------
       POFFDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO END-PARM.

           MOVE "ERNM060POFFDATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PAIDOUTSYN.
           IF VDUSTATUS = "16" GO TO ALL-PO-DATES.
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

           GO TO PLDATE-01.

      *----------------------------------------------------------
       ALL-PO-DATES.

           MOVE "S  8080POFFDATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-POFFDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080POFFDATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-POFFDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       PLDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO END-PARM.

           MOVE "ERNM060PLDATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF PDOUTS = "N"
                 GO TO PAIDOUTSYN
              ELSE
                 GO TO POFFDATE-01.

           IF VDUSTATUS = "16" GO TO ALL-PL-DATES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-PLDATE.
      *-----------------------------------------------------------
       PLDATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO END-PARM.

           MOVE "ERNM060PLDATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO PLDATE-02.

           MOVE VDU-DATE-YYYYMMDD TO END-PLDATE.

           IF END-PLDATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000PLDATE2." TO VDU
              MOVE DATE-YYYYMMDD TO END-PLDATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

           IF END-PLDATE < BEG-PLDATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO PLDATE-01.

           GO TO PAGE-ON-BRANCH.

      *----------------------------------------------------------
       ALL-PL-DATES.

           MOVE "S  8080PLDATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-PLDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080PLDATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-PLDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       PAGE-ON-BRANCH.
           MOVE "E NA010PAGEBR."    TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLDATE-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PAGE-ON-BRANCH.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              GO TO PAGE-ON-BRANCH.
           MOVE VDUBUF TO PAGEBR-FG.

           GO TO EXIT-PARM.


       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.
       EXIT-PARM.
           EXIT.

       DISPLAY-PARAMETERS SECTION.
       DISPLAY-PARM.
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

           MOVE "S  A010PDOUTS." TO VDU.
           MOVE PDOUTS           TO VDUBUF.
           PERFORM SCREENIO.

      ***************************************
      *      SORT ROUTINE
      ***************************************
       SORT-ROUTINE SECTION.

           PERFORM OPEN-LN3-FILE.

           MOVE BR-NO TO LN-OWNBR.
           MOVE 0     TO LN-CLASS.
           MOVE 0     TO LN-ACCTNO.

           PERFORM START-LN3-FILE.
       NEXT-LN3.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-FG NOT = 0 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO SORT-ROUTINE-EXIT.

           IF LN-POCD-NONLOAN
              GO TO NEXT-LN3.

           IF LN-BNKRPTDATE NOT = 0
              GO TO NEXT-LN3.

           IF LN-CONFIDENTCD = "Y"
              GO TO NEXT-LN3.

           IF LN-PLDATE NOT = 0
              IF LN-PLTYPE = "3"
                 GO TO NEXT-LN3.

           IF PDOUTS = "N"
              IF LN-POFFDATE NOT = 0
                 GO TO NEXT-LN3.

           IF LN-PLDATE NOT = 0
              IF (LN-PLDATE < BEG-PLDATE) OR (LN-PLDATE > END-PLDATE)
                 GO TO NEXT-LN3.

           IF LN-POFFDATE NOT = 0
              IF (LN-POFFDATE < BEG-POFFDATE) OR
                 (LN-POFFDATE > END-POFFDATE)
              GO TO NEXT-LN3.

           PERFORM GET-BW.

           IF BW-CONFIDENTIAL = "Y"
              GO TO NEXT-LN3.

           MOVE HOLD-LAST-FIRST TO WK-LAST-FIRST.
           MOVE LN-SSNO(1) TO WK-SSNO.
           PERFORM READ-WK-FILE.
           IF IO-BAD
              MOVE "W" TO CREATE-FG
              GO TO CREATE-WORK.

           IF WK-POFFDATE = 0
              GO TO NEXT-LN3.

           MOVE "R" TO CREATE-FG.

       CREATE-WORK.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * GET STATE-PARAMETER-RECORD:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM GET-SP.

           MOVE HOLD-LAST-FIRST TO WK-LAST-FIRST.
           MOVE LN-SSNO(1) TO WK-SSNO.
           MOVE HOLD-FIRST-LAST TO WK-FIRST-LAST.
           MOVE LN-OWNBR TO WK-BRANCH.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO WK-LOAN-NUM.
           MOVE BW-EMPLOYER TO WK-EMPLOYER.
           MOVE BW-PHONE TO WK-PHONE.

           MOVE TODAYS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-RECENCY.
           PERFORM COMPUTE-CONTRACTUAL.

           IF CPOT30 = "Y"
              MOVE "<P>" TO WK-CONX
           ELSE
           IF CPOT30 = "M"
              MOVE "<M>" TO WK-CONX
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "<PC>" TO WK-CONX
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "<PL>" TO WK-CONX
           ELSE
              MOVE CONTRACTUAL TO WK-CON.

           IF RDPOT30 = "Y"
              MOVE "<P>" TO WK-RECENCYX
           ELSE
           IF RDPOT30 = "M"
              MOVE "<M>" TO WK-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "Y"
              MOVE "<PC>" TO WK-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "M"
              MOVE "<PL>" TO WK-RECENCYX
           ELSE
              MOVE RECDEL TO WK-RECENCY.
           MOVE LN-POFFDATE TO WK-POFFDATE.

           IF CREATE-FG = "R"
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           GO TO NEXT-LN3.

       SORT-ROUTINE-EXIT.
           PERFORM CLOSE-WK-FILE.

      **************************************
       GET-BW SECTION.
      **************************************
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE "* * NOT FOUND * *" TO BW-FNAME
              MOVE " " TO BW-LNAME
              MOVE SPACES TO BW-EMPLOYER
              MOVE 0 TO BW-PHONE.

           MOVE SPACES TO HOLD-FIRST-LAST.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                INTO HOLD-FIRST-LAST.

           MOVE SPACES TO HOLD-LAST-FIRST.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                INTO HOLD-LAST-FIRST.

      ****************************
      *    WRITE DETAIL LINE     *
      ****************************
       WRITE-DETAIL-LINE SECTION.
       WRITE-DET-LINE.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ********************************
      *  GRAND TOTALS
      ********************************
       GRAND-TOTALS SECTION.
           MOVE "TOTAL" TO S-TEXT.
           MOVE TOTAL-COUNT TO S-NOLOANS.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *       RANGE LINES           *
      ******************************************************************
       PRINT-RANGE-LINES SECTION.

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
           MOVE "PAID-OUT ACCTS     :" TO RANGE-LINE.
           MOVE PDOUTS                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN PAYOFF DATE  :" TO RANGE-LINE.
           MOVE BEG-POFFDATE           TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-POFFDATE           TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN PL DATE      :" TO RANGE-LINE.
           MOVE BEG-PLDATE             TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-PLDATE             TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PAGE ON BRANCH     :" TO RANGE-LINE.
           MOVE PAGEBR-FG              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      *******************************
      *        HEAD PAGE ROUTINE    *
      *******************************
       HEAD-PAGE SECTION.
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
           MOVE BR-NO TO HD-BRNO.
           MOVE BR-NAME  TO HD-BRNAME.
           MOVE HEAD3       TO DETAIL-LINE.
           MOVE 2           TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPSTAT.CPY".

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE EXT-CONAME-CONAME TO BOREMP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BOREMP-SYSDATE.
           DISPLAY BOREMP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BOREMP-FORM-FIELDS TO WR-BUF.
           MOVE BOREMP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE BOREMP-HELP--FILE TO HELP-LINK-FILE.
           MOVE BOREMP-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BOREMP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
      *    M I S C   P A R A G R A P H S   *
      **************************************
       MISC-PARA SECTION.
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
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


      *****************************************
       IO-ROUTINES SECTION.
      *****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

       END-PROGRAM SECTION.
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
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPR2MU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BOREMP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
