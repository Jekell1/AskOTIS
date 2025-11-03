       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CRNOFL.
       DATE-WRITTEN.    01/16/2024.
      **************************************************************************
      *                      (SP)
      *    DESCRIPTION:  CREATE NOTICE FILE FOR ALL NON 0 BALANCE ACCOUNTS
      *                  WORLD WILL RUN NIGHTLY TO CREATE NEW NOTICE RECORDS
      *
      * CREATE NOFILE RECORDS FOR EVERY LOAN WITH A BALANCE
      *
      * RUNCBL -C /$REL/SH/XCBLCONFIG +E /$FIL/../LG/ACU_TCLP /$REL/CO/EOCRON         *     NOF +00000 +00000 0001 0001
      *
      * REV:
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

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/CRNOF2 S35 03/11/24-11:47:22 >".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LPWSLM.CPY".
      *----------------------------------------------------------------

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

       01  LN3-CONN-OPEN-SW     PIC X        VALUE "N".

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  AGE-DATE               PIC 9(8).
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       COPY "LIBGB/PASSWDW.CPY".

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
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-TITLE          PIC X(20)    VALUE
           "NOTICE FILE CREATE".
           03  FILLER           PIC X(6)     VALUE " FOR: ".
           03  H-AGEDATE        PIC X(10).

      *-----------------------------------------------------------------

      **********************
      *   MISC WORKERS     *
      **********************
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(29)    VALUE
           "BUILDING NOTICE FILE....".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10).

       01  RECORD-READ-COUNT    PIC 9(9)      VALUE 0.
       01  RECORD-WRITE-COUNT   PIC 9(9)      VALUE 0.
       01  RECORD-REWRITE-COUNT PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(31).
           03  RANGE-SELECTION   PIC X(101).

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".

       COPY "LIBLP/LPCRNOW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPDEFW.CPY".
      
       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CRNOFL_DEF.CPY".
       COPY "LIBSP/CRNOFL_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CRNOFL_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
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
           MOVE "CRNOFL" TO FORM-PROG VDU-FORM-NAME.

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

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "PROGRAM IN PROGRESS................", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.   

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              NO-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              LP-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE
                              LTI-PATH-OVERRIDE
                              GB-PATH-OVERRIDE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO END-OF-GROUP.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-NO1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE BR-NO TO DISPLAY-MESS-BR
              MOVE WK-DISPLAY-MESS TO MESS
              PERFORM SEND-LEGEND.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE 0         TO LN-ACCTNO
                             LN-CLASS
                             QLN3-WBEG-CLASS
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO
                             QLN3-WEND-CLASS.

           PERFORM START-LN3-FILE. 

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT. 
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           ADD 1 TO RECORD-READ-COUNT.

           PERFORM NPR-TRAP-INTERRUPT.

           IF LN-CURBAL = 0
              IF LN-INTBAL = 0
                 IF LN-LCBAL = 0
                    IF LN-OTHBAL = 0
                       GO TO NEXT-LN.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
      * DO NOT USE A HOLD-SP1-KEY, MUST RE-READ BECAUSE OF LN-LC-CODE
      * IN THE LATE CHARGE ROUTINES, WIPES OUT IMPORTANT SPR FIELDS !
      *    PERFORM READ-SP1-FILE.
           PERFORM ARRAYSP-READ.
           IF IO-FG NOT = 0
              GO TO NEXT-LN.

      * AVAILABLE CREDIT NEEDS THE BORROWER, INSIDE CREATE-NOREC
      *    MOVE LN-SSNO(1) TO BW-SSNO.
      *    PERFORM READ-BW1-FILE.
      *    IF IO-FG NOT = 0
           IF BW-SSNO = 0
               GO TO NEXT-LN.

           IF LN-1STPYDATE = 0
              MOVE LN-LOANDATE TO LN-1STPYDATE.
           IF LN-ORIG-1STPYDATE = 0
              MOVE LN-LOANDATE TO LN-ORIG-1STPYDATE.

           MOVE LN-MAKERCD(1)   TO CRNO-MAKERCD.
           MOVE AGE-DATE        TO CRNO-PAYDATE.
           MOVE " "             TO CRNO-LPREC-FG.

      *     MOVE LN-OWNBR  TO NO-BRNO.
      *     MOVE LN-ACCTNO TO NO-ACCTNO.

      * CREATE-NOREC HAS A CLEAR-NO-FILE 

      *     PERFORM READ-NO1-FILE.
      *     IF IO-FG = 0
            IF NO-ACCTNO > 0 
              PERFORM CREATE-NOREC
              PERFORM REWRITE-NO1-FILE
              ADD 1 TO RECORD-REWRITE-COUNT
           ELSE 
              PERFORM CREATE-NOREC
              MOVE LN-OWNBR  TO NO-BRNO
              MOVE LN-ACCTNO TO NO-ACCTNO 
              PERFORM WRITE-NO1-FILE
              ADD 1 TO RECORD-WRITE-COUNT.
 
           GO TO NEXT-LN.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.

      *    PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
      *
      *   I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
           MOVE CRNOFL-QUEUE-POS TO Q-POS.
           MOVE CRNOFL-COPIES-POS TO C-POS.
           MOVE CRNOFL-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE AGE-DATE           TO H-AGEDATE.

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

           IF EXT-ROUTE-BUF = "00"
              STRING "PROGRAM IN PROGRESS................", "...."
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.   

           MOVE 0 TO RECORD-READ-COUNT   
                     RECORD-WRITE-COUNT  
                     RECORD-REWRITE-COUNT.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.

           PERFORM OPEN-PR-FILE.

           PERFORM SQL-OPEN-UPDATE-CONNECTION. 

           PERFORM ENABLE-INTERRUPT.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
      *
      *    EO-EXEC-AUDIT SECTION.
      *
      ******************************************************************
       EO-EXEC-AUDIT SECTION.
           MOVE EXT-EOBUF-DATE2 TO AGE-DATE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
      *
      *    ENTER-PARAMETERS SECTION.
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF VDUSTATUS = "1U"
              GO TO BR-01.

       AGEDT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BATCH WILL USE DATE RAN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD AGE-DATE 
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO AGEDT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
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

       EXIT-PARM.
           EXIT.

      ******************************************************************
      *
      *     DISPLAY-PARAMETERS SECTION.
      *
      ******************************************************************
       DISPLAY-PARAMETERS SECTION.

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

      ******************************************************************
      *
      *     W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
      *
      *     P R I N T - R A N G E - L I N E S
      *
      ******************************************************************
       PRINT-RANGE-LINES SECTION.
           MOVE 4 TO LN-FEED.
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
           MOVE "STARTING TIME      :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "# LNFILE RECORDS READ:" TO RANGE-LINE.
           MOVE RECORD-READ-COUNT        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "# NOFILE RECORDS INSERTED:" TO RANGE-LINE.
           MOVE RECORD-WRITE-COUNT        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "# NOFILE RECORDS UPDATED:" TO RANGE-LINE.
           MOVE RECORD-REWRITE-COUNT        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.



           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *        HEAD PAGE ROUTINE
      *
      ******************************************************************
       HEAD-PAGE SECTION.

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
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/DEFPOL.CPY".

       COPY "LIBUT/LPCRNO.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBLP/LPDEF.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".

       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBLP/LPNOCL.CPY".
       COPY "LIBLP/ADDONSPR.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CRNOFL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CRNOFL-SYSDATE.
           DISPLAY CRNOFL-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CRNOFL-FORM-FIELDS TO WR-BUF.
           MOVE CRNOFL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CRNOFL-HELP--FILE TO HELP-LINK-FILE.
           MOVE CRNOFL-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CRNOFL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *     M I S C E L L E N O U S    P A R A G R A P H S
      ******************************************************************
       MISC-PARA SECTION.
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS      TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *    I/O   R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".   
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
      * COPY "LIBLP/LPNO1IN.CPY".
      * COPYMEMBER: LIBLP/LPNO1IN
      *
      * BAH 2024.0201 REMOVED NO-FMNO, NO-PRINTED, NO-WAGE-ASSIGN-DELINQ
      *               ADDED FIELDS AT THE END OF RECORD
       LOAD-NO1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( NO-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( NO-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO NO-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO NO-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO NO-ALL-DATA
              MOVE NO-ALL-PATH             TO NO-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( NO-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO NO-ALL-BASE
              MOVE FILPATH-MACHINE         TO NO-ALL-MACHINE
              MOVE FILPATH-DATA            TO NO-ALL-DATA
              MOVE NO-ALL-PATH             TO NO-PATH.

      *-----------------------------------------------------------------
       OPEN-NO1-FILE.
           PERFORM LOAD-NO1-FILE.
           PERFORM OPEN-IT.
           MOVE NO-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       WRITE-NO1-FILE.

           ACCEPT NO-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE NO-PATH-SQL    TO E-FILE.
           MOVE NO1-KEY        TO E-KEYX.
           PERFORM SET-NO-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.NOFILE
             (NOFILE.NO_BRNO,
              NOFILE.NO_ACCTNO,
              NOFILE.NO_LTOUCH_DATE,
              NOFILE.NO_TDUE_DATE,
              NOFILE.NO_TDUELC,
              NOFILE.NO_TDUEAMT,
              NOFILE.NO_TDUEPRIN,
              NOFILE.NO_TDUETBAL,
              NOFILE.NO_TDUEPOFF,
              NOFILE.NO_LDUE_DATE,
              NOFILE.NO_LDUELC,
              NOFILE.NO_LDUEAMT,
              NOFILE.NO_DATE_PAID_LAST,
              NOFILE.NO_AMT_PAID_LAST,
              NOFILE.NO_LSTLPKEY,
              NOFILE.NO_NXDUE_DATE,
              NOFILE.NO_NXDUEAMT,
              NOFILE.NO_NXDUELC,
              NOFILE.NO_TRUCONTR,
              NOFILE.NO_RECENCY,
              NOFILE.NO_PDTH_DATE,
              NOFILE.NO_LCPDTH_DATE,
              NOFILE.NO_LCHG_INTDUE,
              NOFILE.NO_AVAILCREDIT,
              NOFILE.NO_TDUEDELINQ,
              NOFILE.NO_DEFERMENT,
              NOFILE.NO_TRUE_DUE_DATE,
              NOFILE.NO_SCHED_PAYMENT,
              NOFILE.NO_TDUECONTR,
              NOFILE.NO_LDUECONTR,
              NOFILE.NO_NXDUECONTR,
              NOFILE.NO_CURRENT_AVAIL,
              NOFILE.NO_LATE_CHG,
              NOFILE.NO_PAST_DUE_DAYS,
              NOFILE.NO_RECDEL,
              NOFILE.NO_AVCR_RESTRICT_FG,
              NOFILE.NO_INDU_PENALTY,
              NOFILE.NO_CPOT30,
              NOFILE.NO_RDPOT30,
              NOFILE.NO_CPOTCHGOFF,
              NOFILE.NO_RDPOTCHGOFF,
              NOFILE.NO_CSUB,
              NOFILE.NO_RSUB)
            VALUES
             (:QNO-BRNO,
              :QNO-ACCTNO,
              :QNO-LTOUCH-DATE,
              :QNO-TDUE-DATE,
              :QNO-TDUELC,
              :QNO-TDUEAMT,
              :QNO-TDUEPRIN,
              :QNO-TDUETBAL,
              :QNO-TDUEPOFF,
              :QNO-LDUE-DATE,
              :QNO-LDUELC,
              :QNO-LDUEAMT,
              :QNO-DATE-PAID-LAST,
              :QNO-AMT-PAID-LAST,
              :QNO-LSTLPKEY,
              :QNO-NXDUE-DATE,
              :QNO-NXDUEAMT,
              :QNO-NXDUELC,
              :QNO-TRUCONTR,
              :QNO-RECENCY,
              :QNO-PDTH-DATE,
              :QNO-LCPDTH-DATE,
              :QNO-LCHG-INTDUE,
              :QNO-AVAILCREDIT,
              :QNO-TDUEDELINQ,
              :QNO-DEFERMENT,
              :QNO-TRUE-DUE-DATE,
              :QNO-SCHED-PAYMENT,
              :QNO-TDUECONTR,
              :QNO-LDUECONTR,
              :QNO-NXDUECONTR,
              :QNO-CURRENT-AVAIL,
              :QNO-LATE-CHG,
              :QNO-PAST-DUE-DAYS,
              :QNO-RECDEL,
              :QNO-AVCR-RESTRICT-FG,
              :QNO-INDU-PENALTY,
              :QNO-CPOT30,
              :QNO-RDPOT30,
              :QNO-CPOTCHGOFF,
              :QNO-RDPOTCHGOFF,
              :QNO-CSUB,
              :QNO-RSUB)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-NO1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-NO1-FILE.

           MOVE NO-PATH-OWNBR  TO NO-BRNO.
           ACCEPT NO-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE NO-PATH-SQL    TO E-FILE.
           MOVE NO1-KEY        TO E-KEYX.
           PERFORM SET-NO-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.NOFILE
             SET
              NOFILE.NO_BRNO = :QNO-BRNO,
              NOFILE.NO_ACCTNO = :QNO-ACCTNO,
              NOFILE.NO_LTOUCH_DATE =
                 :QNO-LTOUCH-DATE,
              NOFILE.NO_TDUE_DATE =
                 :QNO-TDUE-DATE,
              NOFILE.NO_TDUELC = :QNO-TDUELC,
              NOFILE.NO_TDUEAMT = :QNO-TDUEAMT,
              NOFILE.NO_TDUEPRIN = :QNO-TDUEPRIN,
              NOFILE.NO_TDUETBAL = :QNO-TDUETBAL,
              NOFILE.NO_TDUEPOFF = :QNO-TDUEPOFF,
              NOFILE.NO_LDUE_DATE =
                 :QNO-LDUE-DATE,
              NOFILE.NO_LDUELC = :QNO-LDUELC,
              NOFILE.NO_LDUEAMT = :QNO-LDUEAMT,
              NOFILE.NO_DATE_PAID_LAST =
                 :QNO-DATE-PAID-LAST,
              NOFILE.NO_AMT_PAID_LAST = :QNO-AMT-PAID-LAST,
              NOFILE.NO_LSTLPKEY = :QNO-LSTLPKEY,
              NOFILE.NO_NXDUE_DATE =
                 :QNO-NXDUE-DATE,
              NOFILE.NO_NXDUEAMT = :QNO-NXDUEAMT,
              NOFILE.NO_NXDUELC = :QNO-NXDUELC,
              NOFILE.NO_TRUCONTR = :QNO-TRUCONTR,
              NOFILE.NO_RECENCY = :QNO-RECENCY,
              NOFILE.NO_PDTH_DATE =
                 :QNO-PDTH-DATE,
              NOFILE.NO_LCPDTH_DATE =
                 :QNO-LCPDTH-DATE,
              NOFILE.NO_LCHG_INTDUE = :QNO-LCHG-INTDUE,
              NOFILE.NO_AVAILCREDIT = :QNO-AVAILCREDIT,
              NOFILE.NO_TDUEDELINQ = :QNO-TDUEDELINQ,
              NOFILE.NO_DEFERMENT = :QNO-DEFERMENT,
              NOFILE.NO_TRUE_DUE_DATE = :QNO-TRUE-DUE-DATE,
              NOFILE.NO_SCHED_PAYMENT = :QNO-SCHED-PAYMENT,
              NOFILE.NO_TDUECONTR = :QNO-TDUECONTR,
              NOFILE.NO_LDUECONTR = :QNO-LDUECONTR,
              NOFILE.NO_NXDUECONTR = :QNO-NXDUECONTR,
              NOFILE.NO_CURRENT_AVAIL = :QNO-CURRENT-AVAIL,
              NOFILE.NO_LATE_CHG = :QNO-LATE-CHG,
              NOFILE.NO_PAST_DUE_DAYS = :QNO-PAST-DUE-DAYS,
              NOFILE.NO_RECDEL = :QNO-RECDEL,
              NOFILE.NO_AVCR_RESTRICT_FG = :QNO-AVCR-RESTRICT-FG,
              NOFILE.NO_INDU_PENALTY = :QNO-INDU-PENALTY,
              NOFILE.NO_CPOT30 = :QNO-CPOT30,
              NOFILE.NO_RDPOT30 = :QNO-RDPOT30,
              NOFILE.NO_CPOTCHGOFF = :QNO-CPOTCHGOFF,
              NOFILE.NO_RDPOTCHGOFF = :QNO-RDPOTCHGOFF,
              NOFILE.NO_CSUB = :QNO-CSUB,
              NOFILE.NO_RSUB = :QNO-RSUB
            WHERE NOFILE.NO_BRNO = :QNO-BRNO
              AND NOFILE.NO_ACCTNO = :QNO-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-NO1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-NO1-FILE.

           PERFORM CLOSE-IT.


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
              MOVE "Y"         TO LN3-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

      *     IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
      *        ( QLN3-WEND-ACCTNO NOT NUMERIC )
      *        MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
      *        MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
      *        MOVE LN-CLASS             TO QLN3-WBEG-CLASS
      *        MOVE ALL "9"              TO QLN3-WEND-CLASS
      *        MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
      *        MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = EXEC SP_CRNOFL_LNFILE_SELECT 
              ( :QLN3-WBEG-OWNBR )
           END-EXEC.
 
           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WEND-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS
                          QLN3-WBEG-ACCTNO
                          QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.
           INITIALIZE QBW-REC.
           INITIALIZE QNO-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH LN3_CURSOR 
                 INTO 
                  :QLN-OWNBR,
                  :QLN-ACCTNO,
                  :QLN-CLASS,
                  :QLN-SUBCLASS,
                  :QLN-LAWCODE,
                  :QLN-SSNO1,
                  :QLN-MAKERCD1,
                  :QLN-SSNO2,
                  :QLN-MAKERCD2,
                  :QLN-SSNO3,
                  :QLN-MAKERCD3,
                  :QLN-SSNO4,
                  :QLN-MAKERCD4,
                  :QLN-ORGST,
                  :QLN-SPRCLASS,
                  :QLN-CONVERCD,
                  :QLN-PLCD,
                  :QLN-CONFIDENTCD,
                  :QLN-ALLOTCD, 
                  :QLN-STATUSFG,  
                  :QLN-SALEFIN, 
                  :QLN-LOANTYPE, 
                  :QLN-ADDON-FG, 
                  :QLN-PAY-SCHLD-FG, 
                  :QLN-LC-CODE,
                  :QLN-LOANDATE, 
                  :QLN-PURDATE, 
                  :QLN-INTDATE, 
                  :QLN-CASHDATE, 
                  :QLN-ORIG-1STPYDATE, 
                  :QLN-1STPYDATE, 
                  :QLN-JDDATE, 
                  :QLN-PLDATE, 
                  :QLN-ACCELDATE, 
                  :QLN-REDUDATE, 
                  :QLN-POFFDATE, 
                  :QLN-BNKRPTDATE, 
                  :QLN-APRATE,
                  :QLN-EFFRATE,
                  :QLN-SMPRATE,
                  :QLN-OVRDRATE,
                  :QLN-ENDORATE,
                  :QLN-JDRATE,
                  :QLN-ACCRATE,
                  :QLN-REDURATE,
                  :QLN-LNAMT,
                  :QLN-FINCHG,
                  :QLN-INTCHG,
                  :QLN-SERCHG,
                  :QLN-EXTCHG,
                  :QLN-MAINTFEE,
                  :QLN-POINTS,
                  :QLN-ORGTERM,
                  :QLN-1STPYAMT,
                  :QLN-NOREGPY,
                  :QLN-REGPYAMT,
                  :QLN-LASTPYAMT,
                  :QLN-FEECODE-1, 
                  :QLN-FEECODE-2, 
                  :QLN-FEECODE-3, 
                  :QLN-FEECODE-4, 
                  :QLN-FEECODE-5, 
                  :QLN-FEECODE-6, 
                  :QLN-FEECODE-7, 
                  :QLN-FEECODE-8, 
                  :QLN-FEECODE-9, 
                  :QLN-FEECODE-10, 
                  :QLN-FEECODE-11, 
                  :QLN-FEECODE-12, 
                  :QLN-FEECODE-13, 
                  :QLN-FEECODE-14, 
                  :QLN-FEECODE-15, 
                  :QLN-FEECODE-16, 
                  :QLN-FEECODE-17, 
                  :QLN-FEECODE-18, 
                  :QLN-FEECODE-19, 
                  :QLN-FEECODE-20, 
                  :QLN-FEEAMT-1,
                  :QLN-FEEAMT-2,
                  :QLN-FEEAMT-3,
                  :QLN-FEEAMT-4,
                  :QLN-FEEAMT-5,
                  :QLN-FEEAMT-6,
                  :QLN-FEEAMT-7,
                  :QLN-FEEAMT-8,
                  :QLN-FEEAMT-9,
                  :QLN-FEEAMT-10,
                  :QLN-FEEAMT-11,
                  :QLN-FEEAMT-12,
                  :QLN-FEEAMT-13,
                  :QLN-FEEAMT-14,
                  :QLN-FEEAMT-15,
                  :QLN-FEEAMT-16,
                  :QLN-FEEAMT-17,
                  :QLN-FEEAMT-18,
                  :QLN-FEEAMT-19,
                  :QLN-FEEAMT-20,
                  :QLN-FEEOURS-1, 
                  :QLN-FEEOURS-2, 
                  :QLN-FEEOURS-3, 
                  :QLN-FEEOURS-4, 
                  :QLN-FEEOURS-5, 
                  :QLN-FEEOURS-6, 
                  :QLN-FEEOURS-7, 
                  :QLN-FEEOURS-8, 
                  :QLN-FEEOURS-9, 
                  :QLN-FEEOURS-10, 
                  :QLN-FEEOURS-11, 
                  :QLN-FEEOURS-12, 
                  :QLN-FEEOURS-13, 
                  :QLN-FEEOURS-14, 
                  :QLN-FEEOURS-15, 
                  :QLN-FEEOURS-16, 
                  :QLN-FEEOURS-17, 
                  :QLN-FEEOURS-18, 
                  :QLN-FEEOURS-19, 
                  :QLN-FEEOURS-20, 
                  :QLN-FEEFINCHG-1, 
                  :QLN-FEEFINCHG-2, 
                  :QLN-FEEFINCHG-3, 
                  :QLN-FEEFINCHG-4, 
                  :QLN-FEEFINCHG-5, 
                  :QLN-FEEFINCHG-6, 
                  :QLN-FEEFINCHG-7, 
                  :QLN-FEEFINCHG-8, 
                  :QLN-FEEFINCHG-9, 
                  :QLN-FEEFINCHG-10, 
                  :QLN-FEEFINCHG-11, 
                  :QLN-FEEFINCHG-12, 
                  :QLN-FEEFINCHG-13, 
                  :QLN-FEEFINCHG-14, 
                  :QLN-FEEFINCHG-15, 
                  :QLN-FEEFINCHG-16, 
                  :QLN-FEEFINCHG-17, 
                  :QLN-FEEFINCHG-18, 
                  :QLN-FEEFINCHG-19, 
                  :QLN-FEEFINCHG-20, 
                  :QLN-INSURED,
                  :QLN-INSTYPES,
                  :QLN-INSCOMP-1,
                  :QLN-INSCOMP-2,
                  :QLN-INSCOMP-3,
                  :QLN-INSCOMP-4,
                  :QLN-INSCOMP-5,
                  :QLN-INSCOMP-6,
                  :QLN-INSCOMP-7,
                  :QLN-INSCOMP-8,
                  :QLN-INSCOMP-9,
                  :QLN-INSCOMP-10,
                  :QLN-INSOURS-1, 
                  :QLN-INSOURS-2, 
                  :QLN-INSOURS-3, 
                  :QLN-INSOURS-4, 
                  :QLN-INSOURS-5, 
                  :QLN-INSOURS-6, 
                  :QLN-INSOURS-7, 
                  :QLN-INSOURS-8, 
                  :QLN-INSOURS-9, 
                  :QLN-INSOURS-10, 
                  :QLN-INSEFF-DATE-1, 
                  :QLN-INSEFF-DATE-2, 
                  :QLN-INSEFF-DATE-3, 
                  :QLN-INSEFF-DATE-4, 
                  :QLN-INSEFF-DATE-5, 
                  :QLN-INSEFF-DATE-6, 
                  :QLN-INSEFF-DATE-7, 
                  :QLN-INSEFF-DATE-8, 
                  :QLN-INSEFF-DATE-9, 
                  :QLN-INSEFF-DATE-10, 
                  :QLN-INSEXP-DATE-1, 
                  :QLN-INSEXP-DATE-2, 
                  :QLN-INSEXP-DATE-3, 
                  :QLN-INSEXP-DATE-4, 
                  :QLN-INSEXP-DATE-5, 
                  :QLN-INSEXP-DATE-6, 
                  :QLN-INSEXP-DATE-7, 
                  :QLN-INSEXP-DATE-8, 
                  :QLN-INSEXP-DATE-9, 
                  :QLN-INSEXP-DATE-10, 
                  :QLN-INSPREM-1,
                  :QLN-INSPREM-2,
                  :QLN-INSPREM-3,
                  :QLN-INSPREM-4,
                  :QLN-INSPREM-5,
                  :QLN-INSPREM-6,
                  :QLN-INSPREM-7,
                  :QLN-INSPREM-8,
                  :QLN-INSPREM-9,
                  :QLN-INSPREM-10,
                  :QLN-INSCOVR-1,
                  :QLN-INSCOVR-2,
                  :QLN-INSCOVR-3,
                  :QLN-INSCOVR-4,
                  :QLN-INSCOVR-5,
                  :QLN-INSCOVR-6,
                  :QLN-INSCOVR-7,
                  :QLN-INSCOVR-8,
                  :QLN-INSCOVR-9,
                  :QLN-INSCOVR-10,
                  :QLN-INSFINCHG-1, 
                  :QLN-INSFINCHG-2, 
                  :QLN-INSFINCHG-3, 
                  :QLN-INSFINCHG-4, 
                  :QLN-INSFINCHG-5, 
                  :QLN-INSFINCHG-6, 
                  :QLN-INSFINCHG-7, 
                  :QLN-INSFINCHG-8, 
                  :QLN-INSFINCHG-9, 
                  :QLN-INSFINCHG-10, 
                  :QLN-PY-YYYYMM-RECDEL,
                  :QLN-MTD-PAYS-RECDEL,
                  :QLN-MIP-FG, 
                  :QLN-MIP-CLPDTH-DATE,
                  :QLN-MIP-AHPDTH-DATE,
                  :QLN-LCPDTH-DATE,
                  :QLN-INTPDTH-DATE,
                  :QLN-CURBAL,
                  :QLN-INTBAL,
                  :QLN-LCBAL,
                  :QLN-OTHBAL,
                  :QLN-OT2BAL,
                  :QLN-LCPAID,
                  :QLN-LCPARTIALS,
                  :QLN-ANTICERN-1,
                  :QLN-ANTICERN-2,
                  :QLN-ANTICERN-3,
                  :QLN-ANTICERN-4,
                  :QLN-ANTICERN-5,
                  :QLN-ANTICERN-6,
                  :QLN-ANTICERN-7,
                  :QLN-ANTICERN-8,
                  :QLN-ANTICERN-9,
                  :QLN-ANTICERN-10,
                  :QLN-ANTICADJ-1,
                  :QLN-ANTICADJ-2,
                  :QLN-ANTICADJ-3,
                  :QLN-ANTICADJ-4,
                  :QLN-ANTICADJ-5,
                  :QLN-ANTICADJ-6,
                  :QLN-ANTICADJ-7,
                  :QLN-ANTICADJ-8,
                  :QLN-ANTICADJ-9,
                  :QLN-ANTICADJ-10,
                  :QLN-UFERN-1,
                  :QLN-UFERN-2,
                  :QLN-UFERN-3,
                  :QLN-UFERN-4,
                  :QLN-UFERN-5,
                  :QLN-UFERN-6,
                  :QLN-UFERN-7,
                  :QLN-UFERN-8,
                  :QLN-UFERN-9,
                  :QLN-UFERN-10,
                  :QLN-TOTINT,
                  :QLN-TOTLCHG,
                  :QLN-TOTDEF,
                  :QLN-TOTPAYMNTD,
                  :QLN-TOTNODEF,
                  :QLN-YTDNODEF,
                  :QLN-TOTNODEFACT,
                  :QLN-YTDNODEFACT,
                  :QLN-TOTNOALDEL,
                  :QLN-CL-REBATE, 
                  :QLN-AH-REBATE, 
                  :QLN-PP-REBATE, 
                  :QLN-INT-REBATE, 
                  :QLN-SC-REBATE, 
                  :QLN-MF-REBATE, 
                  :QLN-DF-REBATE, 
                  :QLN-EX-REBATE, 
                  :QLN-O1-REBATE, 
                  :QLN-O2-REBATE, 
                  :QLN-O3-REBATE, 
                  :QLN-O4-REBATE, 
                  :QLN-O5-REBATE, 
                  :QLN-O6-REBATE, 
                  :QLN-O7-REBATE, 
                  :QLN-UN-O7-REB, 
                  :QLN-UN-O8-REB, 
                  :QLN-UN-O9-REB, 
                  :QLN-UN-O10-REB, 
                  :QLN-ESCROW-FG, 
                  :QLN-DATE-PAID-LAST,
                  :QLN-CREDITLIM,
                  :QLN-OLDACCTNO,
                  :QLN-PRLNNO,
                  :QLN-SUM-FINCHG,
                  :QLN-SUM-ORGTERM,
                  :QLN-UNITPER-CD,
                  :QLN-UNITPER-FREQ,
                  :QLN-DAYS-IN-UNITPER,
                  :QLN-UNITPER-PER-YEAR,
                  :QLN-REPOCD,
                  :QLN-LBOX,
                  :QBW-OWNBR, 
                  :QBW-SSNO, 
                  :QBW-LNAME,
                  :QBW-LAST-RN-FOR10-DATE,
      *            :QNO-BRNO,  
                  :QNO-ACCTNO 
      *           :QNO-LTOUCH-DATE,
      *           :QNO-TDUE-DATE,
      *           :QNO-TDUELC, 
      *           :QNO-TDUEAMT, 
      *           :QNO-TDUEPRIN, 
      *           :QNO-TDUETBAL, 
      *           :QNO-TDUEPOFF,  
      *           :QNO-LDUE-DATE,
      *           :QNO-LDUELC,  
      *           :QNO-LDUEAMT,  
      *           :QNO-DATE-PAID-LAST,
      *           :QNO-AMT-PAID-LAST, 
      *           :QNO-LSTLPKEY, 
      *           :QNO-NXDUE-DATE,
      *           :QNO-NXDUEAMT,  
      *           :QNO-NXDUELC,  
      *           :QNO-TRUCONTR,  
      *           :QNO-RECENCY,  
      *           :QNO-PDTH-DATE ,
      *           :QNO-LCPDTH-DATE,
      *           :QNO-LCHG-INTDUE, 
      *           :QNO-AVAILCREDIT, 
      *           :QNO-TDUEDELINQ, 
      *           :QNO-DEFERMENT, 
      *           :QNO-TRUE-DUE-DATE,
      *           :QNO-SCHED-PAYMENT, 
      *           :QNO-TDUECONTR, 
      *           :QNO-LDUECONTR, 
      *           :QNO-NXDUECONTR, 
      *           :QNO-CURRENT-AVAIL, 
      *           :QNO-LATE-CHG, 
      *           :QNO-PAST-DUE-DAYS, 
      *           :QNO-RECDEL, 
      *           :QNO-AVCR-RESTRICT-FG,
      *           :QNO-INDU-PENALTY, 
      *           :QNO-CPOT30,
      *           :QNO-RDPOT30,
      *           :QNO-CPOTCHGOFF,
      *           :QNO-RDPOTCHGOFF, 
      *           :QNO-CSUB,  
      *           :QNO-RSUB  
              END-EXEC
      *    ELSE
      *    IF ( LN3-CURSOR-GR-STAT-GOOD )
      *       EXEC SQL
      *            FETCH LN3_CURSOR_GR INTO :QLN-REC
      *       END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS
              MOVE QNO-ACCTNO TO NO-ACCTNO.
      *        PERFORM GET-NO-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

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



       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D    P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

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
              MOVE FORM-PATH TO FORM-PATHNAME
           ELSE
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE SPACES TO MESS
                 PERFORM SEND-LEGEND.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CRNOFL-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
