       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CKEXTR.
       DATE-WRITTEN.    010807.
      **************************************************************************
      *                      (SP)
      *    DESCRIPTION:  CHECK MASTER EXTRACTION
      *                  WORLD MEXICO, PR 517
      *
      *
      * CREATES OUTPUT FILE /USR/EXTRACT/CHECK,      RSZ 290 INCLUDING CR, LF
      *
      * REV:
      * BAH 010807 NEW FOR WORLD PR# 517
      * BAH 090916 ADDED CK-PAYEE-NO, WORLD PR# 654
      * BAH 111021 ADDED CK-VOID AND MORE FIELDS FROM PAFILE, WORLD PR# 775
      * BAH 121203 ADDED CK-CLEARED-FG AND CK-CLEARED-DATE, WORLD PR# 832
      * BAH 170815 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 20190624 FOUND BUG IN A30, COULDNT ENTER A BRANCH RANGE IN BATCH,
      *              CHANGED ROUTE-BUF TEST IN DT1 TO GO TO ENTER-MS
      * BAH 20190930 REMOVED ACCESS-CALL CKFILE
      * BAH 20200831 REMOVED RESET ON BR1 
      * BAH 20230908 REMOVED PA-FILE
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT XCK-FILE ASSIGN TO XCK-PATH
                  ORGANIZATION RELATIVE
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY XCK-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  XCK-FILE
           LABEL RECORDS ARE STANDARD.
       01  XCK-REC.
           03  XCK-BRNO                  PIC 9(4).
           03  XCK-NO                    PIC 9(6).
           03  XCK-DATE                  PIC 9(8).
           03  XCK-AMT                   PIC +9(7).99.
           03  XCK-PAYEE                 PIC X(30).
           03  XCK-REF                   PIC 9(6).
           03  XCK-REASON                PIC X(30).
           03  XCK-GLNO1                 PIC 9(7).
           03  XCK-GLAMT1                PIC +9(7).99.
           03  XCK-GLNO2                 PIC 9(7).
           03  XCK-GLAMT2                PIC +9(7).99.
           03  XCK-GLNO3                 PIC 9(7).
           03  XCK-GLAMT3                PIC +9(7).99.
           03  XCK-GLNO4                 PIC 9(7).
           03  XCK-GLAMT4                PIC +9(7).99.
           03  XCK-GLNO5                 PIC 9(7).
           03  XCK-GLAMT5                PIC +9(7).99.
           03  XCK-PAYEE-NO              PIC 9(6).
           03  XCK-VOID                  PIC X.
           03  XCK-PA-RFC                PIC X(13).
           03  XCK-PA-NAME               PIC X(25).
           03  XCK-PA-1099-FLAG          PIC X.
           03  XCK-PA-VAT-PERC           PIC 99.99.
           03  XCK-CLEARED-FG            PIC X.
           03  XCK-CLEARED-DATE          PIC 9(8).

           03  FILLER                    PIC X(43).
           03  XCK-CR                    PIC X(01).
           03  XCK-LF                    PIC X(01).



      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CKEXTR.CBL J35 12/21/23-09:36:56 >".

      ******************************************************************
      *
      *     F I L E   S P E C S
      *
      ******************************************************************
       01  XCK-PATH.
           03  XCK-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XCK-FILENAME       PIC X(5)  VALUE "CHECK".

       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       01  XCK-KEY       PIC 9(9)     VALUE 0.


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

       01  EXITPROG             PIC X(9)     VALUE "SP/DWEXMU".
       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
       
           03  BEG-DATE               PIC 9(8).
           03  END-DATE               PIC 9(8).
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  ONLY-MS                PIC X.
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

       
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(23)    VALUE " ".
           03  H-TITLE          PIC X(28)    VALUE
           "CHECK MASTER FILE EXTRACTION".


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

       01  CK-RECORD-COUNT      PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(22).
           03  RANGE-SELECTION   PIC X(110).
           03  RANGE-DATE REDEFINES RANGE-SELECTION PIC 9999/99/99.

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
       COPY "LIBSP/CKEXTR_DEF.CPY".
       COPY "LIBSP/CKEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CKEXTR_SCN.CPY".

       
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE XCK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               XCK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
       MAIN-ROUTINE.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CKEXTR" TO FORM-PROG VDU-FORM-NAME.

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

           PERFORM OPEN-CK1-FILE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE WK-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

           MOVE CK-PATH-OWNBR TO CK-BRNO
           MOVE 0             TO CK-NO.

           PERFORM START-CK1-FILE.
       
       NEXT-CK.
           PERFORM READ-CK1-FILE-NEXT.
           IF IO-FG = 9 OR (CK-PATH-OWNBR NOT = CK-BRNO)
              GO TO END-OF-GROUP.

           IF CK-VOID = "Y"
              GO TO NEXT-CK.

           IF ONLY-MS = "Y"
              IF CK-TYPE NOT = "MS"
                 GO TO NEXT-CK.

           IF CK-DATE < BEG-DATE OR CK-DATE > END-DATE
              GO TO NEXT-CK.

           MOVE SPACES TO XCK-REC.

           MOVE CK-BRNO              TO XCK-BRNO.
           MOVE CK-NO                TO XCK-NO.
           MOVE CK-DATE              TO XCK-DATE.
           MOVE CK-AMT               TO XCK-AMT.
           MOVE CK-PAYEE             TO XCK-PAYEE.
           MOVE CK-REF               TO XCK-REF.
           MOVE CK-REASON            TO XCK-REASON.
           MOVE CK-GLNO(1)           TO XCK-GLNO1.
           MOVE CK-GLAMT(1)          TO XCK-GLAMT1.
           MOVE CK-GLNO(2)           TO XCK-GLNO2.
           MOVE CK-GLAMT(2)          TO XCK-GLAMT2.
           MOVE CK-GLNO(3)           TO XCK-GLNO3.
           MOVE CK-GLAMT(3)          TO XCK-GLAMT3.
           MOVE CK-GLNO(4)           TO XCK-GLNO4.
           MOVE CK-GLAMT(4)          TO XCK-GLAMT4.
           MOVE CK-GLNO(5)           TO XCK-GLNO5.
           MOVE CK-GLAMT(5)          TO XCK-GLAMT5.
           MOVE CK-VOID              TO XCK-VOID.
           MOVE CK-PAYEE-NO          TO XCK-PAYEE-NO.
           MOVE CK-CLEARED-FG        TO XCK-CLEARED-FG.
           MOVE CK-CLEARED-DATE      TO XCK-CLEARED-DATE.

           MOVE X"0D"                TO XCK-CR.
           MOVE X"0A"                TO XCK-LF.

           ADD 1 TO XCK-KEY.
           PERFORM WRITE-XCK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO CK-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-CK.
       
       END-OF-GROUP.
           PERFORM CLOSE-CK1-FILE.

           MOVE "Y" TO FIRST-TIME.

           GO TO NEXT-GROUP-LOC.
       
       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      *************************************************
       INITIALIZATION SECTION.
      *************************************************
           MOVE CKEXTR-QUEUE-POS TO Q-POS.
           MOVE CKEXTR-COPIES-POS TO C-POS.
           MOVE CKEXTR-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           CALL "C$TOLOWER" USING XCK-DIRECTORY, VALUE 13.
           MOVE XCK-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "EXTRACTION DIRECTORY DOES NOT EXIST!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION IN PROGRESS................" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-XCK-FILE-OUTPUT.
           PERFORM CLOSE-XCK-FILE.
           PERFORM OPEN-XCK-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      *************************************************
       EO-EXEC-AUDIT SECTION.
      *************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.


      *************************************************
       ENTER-PARAMETERS SECTION.
      *************************************************
       ENTER-PARM.

      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF VDUSTATUS = "1U"
              GO TO BR-02.

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
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

       
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

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

       
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

       
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       
       DT1.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-MS.

           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "16" GO TO ENTER-ALL-DATES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT1.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

       DT2.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT2.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT1.

           GO TO ENTER-MS.

       ENTER-ALL-DATES.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       ENTER-MS.
           MOVE "E  A010MS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT2.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-MS.

           MOVE VDUBUF TO ONLY-MS.
           IF ONLY-MS NOT = "Y" AND NOT = "N"
              GO TO ENTER-MS.

       EXIT-PARM.
           EXIT.

      ******************************
       DISPLAY-PARAMETERS SECTION.
      ******************************

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

           MOVE "S  A010MS." TO VDU.
           MOVE ONLY-MS TO VDUBUF.
           PERFORM SCREENIO.

      *******************************
       WRITE-DETAIL-LINE SECTION.
      *******************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      **************************
       PRINT-RANGE-LINES SECTION.
      **************************
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
       
           MOVE "BEGINNING DATE   :" TO RANGE-LINE.
           MOVE BEG-DATE             TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING DATE      :" TO RANGE-LINE.
           MOVE END-DATE             TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "ONLY MS   :" TO RANGE-LINE.
           MOVE ONLY-MS       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "STARTING TIME      :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "# CHECK RECORDS      :" TO RANGE-LINE.
           MOVE CK-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.


      ***********************
       HEAD-PAGE SECTION.
      ***********************

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
           MOVE EXT-CONAME-CONAME TO CKEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CKEXTR-SYSDATE.
           DISPLAY CKEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CKEXTR-FORM-FIELDS TO WR-BUF.
           MOVE CKEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CKEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE CKEXTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CKEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************
       MISC-PARA SECTION.
      *****************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       CONDITIONAL-PRINT.
           IF DETAIL-LINE NOT = " "
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       OPEN-XCK-FILE.
           PERFORM OPEN-IT.
           MOVE XCK-PATH TO E-FILE.
           OPEN I-O XCK-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCK-FILE.
           IF IO-FG = 7
             CLOSE XCK-FILE
             GO TO OPEN-XCK-FILE.
      *    UNLOCK XCK-FILE.
       OPEN-XCK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XCK-PATH TO E-FILE.
           OPEN OUTPUT XCK-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCK-FILE.
           IF IO-FG = 7
             CLOSE XCK-FILE
             GO TO OPEN-XCK-FILE.
      *    UNLOCK XCK-FILE.
       WRITE-XCK-FILE.
           PERFORM WRITE-IT.
           MOVE XCK-PATH TO E-FILE.
           MOVE XCK-KEY TO E-KEYX.
           WRITE XCK-REC.
           IF IO-FG = 8
             GO TO WRITE-XCK-FILE.
      *    UNLOCK XCK-FILE.
       CLOSE-XCK-FILE.
           PERFORM CLOSE-IT.
           CLOSE XCK-FILE.
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPCK1RN.CPY".
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
              MOVE EXITPROG TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CKEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
