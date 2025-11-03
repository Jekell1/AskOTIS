       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DELCHK.
       DATE-WRITTEN.   10-10-84
      ***********************************************************************
      *                      (LP)
      *   DESC:  CHECK MASTER DELETE
      *          MUST BE CLEARED PRIOR TO DELETING !!
      * NOT BATCHABLE !
      *
      *   REV:
      *   GLB 990114 ADD EXITPROG LOGIC, SHARE WITH NOTES
      *   GLF 990629 ADD BEGINNING AND EINDING DATE RANGES
      *   BAH 990903 ADDED ERRLOGW, WRITES TO THE CHG ERRLOG
      *              MULLEN KEEPS COMPLAINING ABOUT CHECKS DELETING
      *              THAT SHOULD NOT, AND WE DONT KNOW WHAT THEY ARE
      *              DOING WRONG, SO I AM LOGGING THE SCREEN-BUF ENTRY
      *   GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *              MEETING (082599)
      *   BAH 020417 RENAMED FROM CKDEL. MUST HAVE CK-CLEARED-FG AND DATE
      *              PRIOR TO DELETING, REGMGM CHECK REC PR# 1726
      *   BLF 060810 IF GLOBAL PARAMETER FOR PAYEE PROCESSING IS ON, DISPLAY
      *              A WARNING THAT DELETING CHECKS MAY MEAN YOU CAN'T
      *              REGENERATE PAYEE YTD STATS, DEVELOPMENT PR# 2633
      *   BLM 100112 IF CHECK IF VOIDED (CK-VOID-DATE NON-ZERO), GO AHEAD
      *              AND DELETE EVEN IF NOT CLEARED.  IF VOIDED, CHECK
      *              CK-VOID-DATE IN DATE RANGE INSTEAD OF CK-CLEARED-DATE,
      *              WORLD
      *   BAH 171026 ACTIVATE 8 DIGIT DATES, PD0006
      *   BAH 181008 GLOBAL CKFILE
      *   BAH 2019.0925 REMOVED ACCESS-CALL CKFILE
      *   BAH 2020.0818 ADD BRANCH RANGE/GROUP AND CKECK ISSUE DATE RANGE
      *                 ADDED PRINTING OF EXCEPTIONS #1457
      *   BAH 2020.0827 REMOVED RESET ON BR1 
      ***********************************************************************
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
           "@(#)LP/DELCHK.CBL A30 11/07/14-15:27:37 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".


      *****************************
      *     MISC. WORKERS         *
      *****************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  TODAYS-DATE         PIC 9(8).
       01  RETENTION-DATE      PIC 9(8).
       01  FIRST-TIME          PIC X VALUE "Y".
       01  RECORDS-FOUND       PIC X VALUE " ".
           88 NO-RECORDS-FOUND       VALUE "N".
       01  MESS-LIST           PIC X(5) VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(29)    VALUE
           "DELETING CHECKS...........".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10).

       01  RECORD-COUNT        PIC 9(6) VALUE 0.
       01  VOID-COUNT          PIC 9(6) VALUE 0.
       01  EXCEPT-COUNT        PIC 9(6) VALUE 0.


      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CK          PIC 9(6).
           03  END-CK          PIC 9(6).
           03  BEG-ISSUE-DATE  PIC 9(8).
           03  END-ISSUE-DATE  PIC 9(8).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

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
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(12)    VALUE " ".
           03  H-TITLE          PIC X(28)    VALUE
           "CHECK MASTER DELETE".

       01  HEAD3.
           03  FILLER           PIC X(6)    VALUE "BRNO".
           03  FILLER           PIC X(8)    VALUE "CKNO".
           03  FILLER           PIC X(10)   VALUE "ISSUE".
           03  FILLER           PIC X(10)   VALUE "CLEAR".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC 9(4).
           03  FILLER           PIC XX.
           03  D-CKNO           PIC 9(6).
           03  FILLER           PIC XX.
           03  D-ISSUE-DATE     PIC 99/99/99.
           03  FILLER           PIC XX.
           03  D-CLEAR-DATE     PIC 99/99/99.
           03  FILLER           PIC XX.
           03  D-TEXT           PIC X(46).

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE         PIC X(22).
           03  RANGE-SELECTION    PIC X(60).
           03  RANGE-SELNUM     REDEFINES RANGE-SELECTION.
               05  RANGE-NUM      PIC ZZZ,ZZZ,ZZ9.
               05  FILLER REDEFINES RANGE-NUM.
                   07  RANGE-DATE PIC 99/99/99.
                   07  FILLER     PIC X(3).


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
       COPY "LIBLP/DELCHK_DEF.CPY".
       COPY "LIBLP/DELCHK_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/DELCHK_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.

       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DELCHK" TO FORM-PROG VDU-FORM-NAME.

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

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO CK-PATH-OVERRIDE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE WK-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

           PERFORM OPEN-CK1-FILE.

           MOVE BR-NO  TO CK-BRNO
                          QCK1-WBEG-BRNO
                          QCK1-WEND-BRNO.
           MOVE BEG-CK TO CK-NO
                          QCK1-WBEG-CKNO.
           MOVE END-CK TO QCK1-WEND-CKNO.
           PERFORM START-CK1-FILE.

       NEXT-CK.
           PERFORM READ-CK1-FILE-NEXT.
           IF (IO-BAD) OR (CK-PATH-OWNBR NOT = CK-BRNO)
              GO TO END-OF-GROUP.

           IF CK-NO > END-CK
              GO TO END-OF-GROUP.

      * CHECK DATE RANGE
           IF CK-DATE < BEG-ISSUE-DATE OR CK-DATE > END-ISSUE-DATE
      *       UNLOCK CK-FILE
              GO TO NEXT-CK.

           IF CK-VOID-DATE NOT = 0
              ADD 1 TO VOID-COUNT
              GO TO READY-TO-DELETE.

           IF CK-CLEARED-FG NOT = "Y"
              MOVE "MISSING CLEARED FLAG" TO D-TEXT
              PERFORM FILL-DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE
              ADD 1 TO EXCEPT-COUNT
      *       UNLOCK CK-FILE
              GO TO NEXT-CK.

           IF CK-CLEARED-DATE = 0
              MOVE "MISSING CLEARED DATE" TO D-TEXT
              PERFORM FILL-DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE
              ADD 1 TO EXCEPT-COUNT
      *       UNLOCK CK-FILE
              GO TO NEXT-CK.

       READY-TO-DELETE.

           PERFORM DELETE-CK1-FILE.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-CK.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-CK1-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.


      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE DELCHK-QUEUE-POS  TO Q-POS.
           MOVE DELCHK-COPIES-POS TO C-POS.
           MOVE DELCHK-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

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
              MOVE "PROGRAM IN PROGRESS............." TO WR-BUF
              MOVE MESS-LIST                          TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       EXIT-PARM.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      **********************************
       ENTER-PARAMETERS SECTION.
      **********************************
           IF VDUSTATUS = "1U"
              GO TO CK2.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE.

           MOVE TODAYS-DATE TO NDTE-DATE.
           MOVE -7          TO NDTE-HOLD.
           PERFORM INCREMENT-YEARS.
           MOVE NDTE-DATE   TO RETENTION-DATE.

       BR1.
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
                 GO TO BR1
              ELSE
                 MOVE "S NA000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR1.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR1.
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

       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR1.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR1.

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
              GO TO BR1.

       CK1.
           MOVE "ENNN060BEGCK." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR1.
           IF VDUSTATUS = "16" GO TO ALL-CK.
           IF VDUSTATUS = "1U" GO TO BR1.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO CK1.

           MOVE VDUNUM0 TO BEG-CK.
       CK2.
           MOVE "ENNN060ENDCK." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR1.
           IF VDUSTATUS = "1U" GO TO CK1.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO CK2.

           MOVE VDUNUM0 TO END-CK.
           IF BEG-CK > END-CK
              MOVE "INVALID CHECK RANGE !!! " TO MESS
              PERFORM SEND-MESS
              GO TO CK1.

           GO TO ISSUE-DT1.
       ALL-CK.
           MOVE "SN N060BEGCK." TO VDU.
           MOVE 0 TO BEG-CK VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N060ENDCK." TO VDU.
           MOVE 999999 TO END-CK VDUNUM0.
           PERFORM SCREENIO.

       ISSUE-DT1.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060BEGCKDT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR1.
           IF VDUSTATUS = "1U" GO TO CK2.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO ISSUE-DT1.

           MOVE VDU-DATE-YYYYMMDD TO BEG-ISSUE-DATE.
       ISSUE-DT2.
           MOVE "ERNM060ENDCKDT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR1.
           IF VDUSTATUS = "1U" GO TO ISSUE-DT1.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO ISSUE-DT2.

           MOVE VDU-DATE-YYYYMMDD TO END-ISSUE-DATE.

           IF END-ISSUE-DATE < BEG-ISSUE-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ISSUE-DT1.

      * TEST AGAINST RETENTION DATE WHICH IS 7 YEARS
           IF END-ISSUE-DATE > RETENTION-DATE
              MOVE "DATE WITHIN RETENTION PERIOD" TO MESS
              PERFORM SEND-MESS
              GO TO ISSUE-DT2.

       EXIT-PARM.
           EXIT.

      ******************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "SNNA040BR2." TO VDU
              MOVE SPACES TO VDUBUF
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

           MOVE "SN N060BEGCK." TO VDU.
           MOVE BEG-CK          TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N060ENDCK." TO VDU.
           MOVE END-CK          TO VDUNUM0.
           PERFORM SCREENIO.


      ******************************
       EO-EXEC-AUDIT SECTION.
      ******************************
           MOVE EXT-EOBUF-DATE1 TO BEG-ISSUE-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-ISSUE-DATE.

           MOVE TODAYS-DATE TO NDTE-DATE.
           MOVE -7          TO NDTE-HOLD.
           PERFORM INCREMENT-YEARS.
           MOVE NDTE-DATE   TO RETENTION-DATE.

      * TEST AGAINST RETENTION DATE WHICH IS 7 YEARS
           IF END-ISSUE-DATE > RETENTION-DATE
              MOVE "DATE WITHIN RETENTION PERIOD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO END-PROGRAM.


      ***********************************
       WRITE-DETAIL-LINE SECTION.
      ***********************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************
           MOVE 4 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH    :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH       :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH    :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH       :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING ISSUE DATE:"   TO RANGE-LINE.
           MOVE BEG-ISSUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ISSUE DATE:"   TO RANGE-LINE.
           MOVE END-ISSUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE "CLEAR EXCEPTIONS    : "  TO RANGE-LINE.
           MOVE EXCEPT-COUNT              TO RANGE-NUM.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL VOIDS DELETED : "  TO RANGE-LINE.
           MOVE VOID-COUNT              TO RANGE-NUM.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL ALL DELETED   : "  TO RANGE-LINE.
           MOVE RECORD-COUNT              TO RANGE-NUM.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ***********************
       HEAD-PAGE SECTION.
      ***********************

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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       EXIT-HEAD-PAGE.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME  TO DELCHK-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DELCHK-SYSDATE.
           DISPLAY DELCHK-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DELCHK-FORM-FIELDS TO WR-BUF.
           MOVE DELCHK-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM. 


      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE DELCHK-HELP--FILE TO HELP-LINK-FILE.
           MOVE DELCHK-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/DELCHK_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************
       MISC-PARA SECTION.
      ******************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FILL-DETAIL-LINE.
           MOVE CK-BRNO         TO D-BRNO.
           MOVE CK-NO           TO D-CKNO.
           MOVE CK-DATE         TO D-ISSUE-DATE.
           MOVE CK-CLEARED-DATE TO D-CLEAR-DATE.



      ******************************************************
       IO-ROUTINES SECTION.
      ******************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPCK1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************
       END-PROGRAM SECTION.
      ******************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

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
              DESTROY DELCHK-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
