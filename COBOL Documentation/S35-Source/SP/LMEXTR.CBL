       IDENTIFICATION     DIVISION.
       PROGRAM-ID.          LMEXTR.
       DATE-WRITTEN.    08/13/1999.
      ******************************************************************
      *
      * DESCRIPTION:  LOAM MEMO EXTRACTION
      *               DATA WAREHOUSE
      *
      * CREATES OUTPUT FILE /USR/EXTRACT/LOANMEMO, RSZ = 93, INCLUDING CR,LF
      *
      *=================================================================
      * REV:
      * 011206 BAH ADD MEMO DATE RANGE, AND I,S,O FOR PRIORITY, MERC PR# 264
      * 020711 CS  IF PRIORITY MEMO = "O" OR "I" SKIP THE DATE CHECK, FOR
      *            SOME REASON IT DID NOT WORK ON MERCURY (AIX)
      * 041204 BAH TURNED THIS MERCURY EXTRACTION INTO A DATA WAREHOUSE
      *            EXTRACTION FOR CENTRIX, PR# 2416
      * 150615 BAH EXPANDED XAE-KEY TO 9(9) FROM 9(8), WORLD PR# 1088
      * KEC 2017.0525 {PL#00973} PARADATA <A15> [JAY CISZEWSKI]
      *         USED SP/LMEXTR TO CREATE SP/BMEXTR.  IN TESTING BMEXTR,
      *         FOUND AN ERROR DUE TO XBM-FILE NOT DECLARE IN THE
      *         PROCEDURE DIVISION.  THIS WAS BECAUSE LMEXTR DID NOT
      *         HAVE XLM-FILE DECLARED.  SO THIS FIXED THAT.
      *  CS 180530 CHANGED OUTPUT XLM-ACCTNO TO INCLUDE 01 TO MATCH A15
      * BAH 20200831 REMOVED RESET ON BR1 
      * BAH 20220803 HARD CODED START
      * RMZ 20250319 PERFORMANCE TUNING - ADDED STORED PROCEDURE. 
      *              (S35Q-182) 
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBEX/EXFSXLM.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBEX/EXFDXLM.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/LMEXTR.CBL J35 12/21/23-09:37:29 >".
      *********************************************************
      *   LIBEX/EXWSXLM: LOAN MEMO EXTRACTION FILE PATH WORKERS
      *********************************************************
       01  XLM-PATH.
           03  XLM-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLM-FILENAME       PIC X(8) VALUE "LOANMEMO".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

      *=================================================================
      *    S Q L - W O R K E R S
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QWS-WBEG-DATE-REV           PIC 9(8).
       01  QWS-WEND-DATE-REV           PIC 9(8).  
       01  QWS-ZBAL                    PIC X      VALUE SPACE. 
       01  QWS-PL                      PIC X      VALUE SPACE. 
       01  QWS-PRIORITY-ISO            PIC X      VALUE SPACE.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW            PIC X      VALUE "N".

      *-----------------------------------------------------------------

       01  XLM-KEY              PIC 9(9)     VALUE 0.

       01  WS-BRACCTNO.
           03  WS-OWNBR    PIC 9(4).
           03  WS-ACCTNO   PIC 9(6).

       01  WS-A15-ACCTNO.
           03  WS-A15-NUMBER    PIC 9(6).
           03  WS-A15-SEQNO     PIC 9(2).

      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(04)     VALUE 0.

       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

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
           03  ZBAL                   PIC X.
           03  PL                     PIC X.
           03  BEG-DATE               PIC 9(8).
           03  END-DATE               PIC 9(8).
           03  PRIORITY-FG            PIC X.
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BRANCH           PIC 9999 VALUE 9999.
       01  SV-END-BRANCH           PIC 9999 VALUE 9999.
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
           03  FILLER           PIC X(23)    VALUE " ".
           03  H-TITLE          PIC X(28)    VALUE
           "LOAN MEMO FILE EXTRACTION".
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

       01  RECORD-COUNT      PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(22).
           03  RANGE-SELECTION   PIC X(110).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/LMEXTR_DEF.CPY".
       COPY "LIBSP/LMEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/LMEXTR_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 XLM-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE XLM-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ***********************************************
       MAIN-MODULE SECTION.
      ***********************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LMEXTR" TO FORM-PROG VDU-FORM-NAME.

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

           MOVE "B"        TO LN-PATH-OVERRIDE
                              LM-PATH-OVERRIDE.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LM1-FILE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE WK-DISPLAY-MESS TO MESS
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

           PERFORM START-LN3-FILE. *> SSP - LN, LM

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT. *> EMBEDDED
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.
           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.

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


      *    MOVE LM-PATH-OWNBR TO LM-BRNO
      *                          QLM1-WBEG-BRNO
      *                          QLM1-WEND-BRNO.
      *    MOVE LN-ACCTNO     TO LM-ACCTNO
      *                          QLM1-WBEG-ACCTNO
      *                          QLM1-WEND-ACCTNO.
      *    MOVE 0             TO LM-DATE
      *                          QLM1-WBEG-DATE-REV.
      *    MOVE 0             TO LM-SEQ 
      *                          QLM1-WBEG-SEQ.
      *    MOVE ALL "9"       TO QLM1-WEND-DATE-REV
      *                          QLM1-WEND-SEQ.

      *    PERFORM START-LM1-FILE.
       NEXT-LM.
      *    PERFORM READ-LM1-FILE-NEXT.
      *    IF IO-FG = 9 OR (LM-BRNO NOT = LM-PATH-OWNBR)
      *       GO TO NEXT-LN.

           IF (LM-ACCTNO NOT = LN-ACCTNO)
              GO TO NEXT-LN.
      *       GO TO NEXT-LM.

           SUBTRACT LM-DATE FROM 99999999 GIVING DATE-YYYYMMDD.

           IF PRIORITY-FG = "O"
              IF DATE-YYYYMMDD NOT = 99999999
                 GO TO NEXT-LN
      *          GO TO NEXT-LM
              ELSE
                 GO TO CREATE-WK.

           IF PRIORITY-FG = "S"
              IF DATE-YYYYMMDD = 99999999
                 GO TO NEXT-LN.
      *          GO TO NEXT-LM.

           IF PRIORITY-FG = "I"
              IF DATE-YYYYMMDD = 99999999
                 GO TO CREATE-WK.

      * BECAUSE LM-REVERSE-DATE'S GO BACK TO '1925-00-00', BYPASSING THIS
      * IF STATEMENT, SO THE RECORDS CAN BE WRITTEN TO THE OUTPUT FILE 
      * LIKE A15 DOES. 
      *
      *    IF DATE-YYYYMMDD < BEG-DATE OR DATE-YYYYMMDD > END-DATE
      *       GO TO NEXT-LM.

       CREATE-WK.
           MOVE SPACES TO XLM-REC.

           MOVE LN-OWNBR         TO WS-OWNBR.
           MOVE LN-ACCTNO        TO WS-ACCTNO.
           MOVE WS-BRACCTNO      TO XLM-BRACCTNO.
 
      *** TO MAKE REGION NUMBER MATCH A15 FILE FOR SPACES, *** 
      *** THE FOLLOWING "IF" STATEMENT IS PERFORMED        ***
           IF BR-REGION-NUMBER = 00
              MOVE SPACES TO XLM-REGION-NUMBER
           ELSE
              MOVE BR-REGION-NUMBER TO XLM-REGION-NUMBER.

      *    MOVE LM-ACCTNO TO XLM-ACCTNO.
      *NEEDS TO HAVE 01SEQ TO MATCH A15.
           MOVE LM-ACCTNO     TO WS-A15-NUMBER.
           MOVE 01            TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO TO XLM-ACCTNO.


           SUBTRACT LM-DATE FROM 99999999 GIVING DATE-YYYYMMDD.

      *** TO MAKE DATE-YYYYMMDD NUMBER MATCH A15 FILE FOR ZEROES, *** 
      *** THE FOLLOWING "IF" STATEMENT IS PERFORMED               ***
           IF DATE-YYYYMMDD = 19700101
              MOVE ZEROES     TO XLM-DATE
           ELSE
              MOVE DATE-YYYYMMDD TO XLM-DATE.

           MOVE LM-SEQ        TO XLM-SEQ.
           MOVE LM-MEMO       TO XLM-MEMO.
           MOVE LM-MEMO-TYPE  TO XLM-MEMO-TYPE.

           MOVE X"0D"         TO XLM-CR.
           MOVE X"0A"         TO XLM-LF.

           ADD 1 TO XLM-KEY.
           PERFORM WRITE-XLM-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-LN.
      *    GO TO NEXT-LM.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LM1-FILE.

           MOVE "Y" TO FIRST-TIME.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE LMEXTR-QUEUE-POS TO Q-POS.
           MOVE LMEXTR-COPIES-POS TO C-POS.
           MOVE LMEXTR-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           CALL "C$TOLOWER" USING XLM-DIRECTORY, VALUE 13.
           MOVE XLM-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "EXTRACTION DIRECTORY DOES NOT EXIST!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-SYSDATE TO H-AGEDATE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BRANCH
              MOVE END-BRANCH TO SV-END-BRANCH
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BRANCH
                 MOVE END-BRANCH TO SV-END-BRANCH
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

           PERFORM OPEN-XLM-FILE-OUTPUT.
           PERFORM CLOSE-XLM-FILE.
           PERFORM OPEN-XLM-FILE.

           GO TO INITIALIZE-EXIT.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.

      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE
           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF VDUSTATUS = "1U"
              GO TO ENTER-PL.

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
              GO TO BEGCLS.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           GO TO CHECK-BRANCH-SECURITY.

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

       BEGCLS.
           MOVE "ENNN020CLASS1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLCLS.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO BEGCLS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGCLS.
           MOVE VDUNUM0 TO BEG-CLASS.

       ENDCLS.
           MOVE "ENNN020CLASS2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLCLS.
           IF VDUSTATUS = "1U" GO TO BEGCLS.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO ENDCLS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDCLS.
           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGCLS.

           GO TO ENTER-ZBAL.

      *----------------------------------------------------------------
       ALLCLS.
           MOVE "SNNN020CLASS1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CLASS2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-ZBAL.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGCLS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ZBAL.
           IF NOT (VDUBUF = "I" OR "S" OR "O") GO TO ENTER-ZBAL.
           MOVE VDUBUF TO ZBAL QWS-ZBAL.

      *----------------------------------------------------------------
       ENTER-PL.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ZBAL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-PL.
           IF NOT (VDUBUF = "I" OR "S" OR "O") GO TO ENTER-PL.
           MOVE VDUBUF TO PL QWS-PL.

      *----------------------------------------------------------------
       BEGDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-PRIORITY.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-PL.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

           IF BEG-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000DATE1." TO VDU
              MOVE DATE-YYYYMMDD TO BEG-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

           IF BEG-DATE = 19500101
              MOVE 99999999                       TO QWS-WEND-DATE-REV
           ELSE 
              SUBTRACT BEG-DATE FROM 99999999 GIVING QWS-WEND-DATE-REV.

      *-----------------------------------------------------------
       ENDDATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-PRIORITY.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO ENDDATE-02.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000DATE2." TO VDU
              MOVE DATE-YYYYMMDD TO END-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGDATE-01.
 
           IF END-DATE = 20491231   
              MOVE 00000001                       TO QWS-WBEG-DATE-REV
           ELSE  
              SUBTRACT END-DATE FROM 99999999 GIVING QWS-WBEG-DATE-REV.

           GO TO ENTER-PRIORITY.

      *----------------------------------------------------------
       ALL-DATE.

           MOVE "S  8080DATE1." TO VDU.
           MOVE EXT-JULIAN-BEG  TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2." TO VDU.
           MOVE EXT-JULIAN-END  TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *    SUBTRACT BEG-DATE FROM 99999999 GIVING QWS-WEND-DATE-REV.
      *    SUBTRACT END-DATE FROM 99999999 GIVING QWS-WBEG-DATE-REV.
 
           MOVE 00000001       TO QWS-WBEG-DATE-REV.
           MOVE 99999999       TO QWS-WEND-DATE-REV.  

      *----------------------------------------------------------
       ENTER-PRIORITY.
           MOVE "E  A010PRIORITY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENDDATE-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-PRIORITY.
           IF NOT (VDUBUF = "I" OR "S" OR "O") GO TO ENTER-PRIORITY.
           MOVE VDUBUF TO PRIORITY-FG
                          QWS-PRIORITY-ISO.

       EXIT-PARM.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
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

           MOVE "SNNN020CLASS1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CLASS2." TO VDU
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010ZBAL." TO VDU
           MOVE ZBAL TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PL." TO VDU
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRIORITY." TO VDU
           MOVE PRIORITY-FG TO VDUBUF.
           PERFORM SCREENIO.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
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
              MOVE BEG-BRANCH                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE END-BRANCH                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING CLASS       :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ZERO BALANCES      :" TO RANGE-LINE.
           MOVE ZBAL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "P & L              :" TO RANGE-LINE.
           MOVE PL                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGINNINGG MEMODATE:" TO RANGE-LINE.
           MOVE  BEG-DATE              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    MEMODATE:"  TO RANGE-LINE.
           MOVE  END-DATE              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE .
      
           MOVE "PRIORITY (I,S,O)  :"  TO RANGE-LINE..
           MOVE PRIORITY-FG            TO RANGE-SELECTION.
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
      

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

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

       END-HEAD-PAGE.
           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ************************************************************
       CLASS-SCAN SECTION.
      ************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
             MOVE "SNNN020CLASS1." TO VDU
           ELSE
             MOVE "SNNN020CLASS2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO LMEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LMEXTR-SYSDATE.
           DISPLAY LMEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LMEXTR-FORM-FIELDS TO WR-BUF.
           MOVE LMEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE LMEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE LMEXTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/LMEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBEX/EXXLMI.CPY".
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
              MOVE "Y"         TO LN3-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.  *> SSP - LN, LM
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
              ( QLN3-WEND-ACCTNO NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
              MOVE LN-CLASS             TO QLN3-WBEG-CLASS
              MOVE ALL "9"              TO QLN3-WEND-CLASS
              MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

      *  STORED PROCEDURE
      *
      *  SELECT LNFILE.LN_OWNBR, 
      *         LNFILE.LN_ACCTNO,
      *         LNFILE.LN_CLASS,
      *         ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0')
      *                                      AS LN_PLDATE,
      *         LNFILE.LN_CURBAL,
      *         LNFILE.LN_INTBAL,
      *         LNFILE.LN_LCBAL,
      *         LNFILE.LN_OTHBAL,
      *         LMFILE.LM_BRNO,
      *         LMFILE.LM_ACCTNO,
      *         LMFILE.LM_DATE_REVERSED,
      *         LMFILE.LM_SEQ,
      *         LMFILE.LM_MEMO,
      *         LMFILE.LM_MEMO_TYPE
      * 
      *   FROM  DBO.LMFILE LMFILE
      *  INNER
      *     JOIN 
      *         DBO.LNFILE LNFILE
      * 
      *      ON (  LNFILE.LN_OWNBR               = @QLN3_WBEG_BRNO
      *     AND    LNFILE.LN_CLASS         BETWEEN @QLN3_WBEG_CLASS
      *                                        AND @QLN3_WEND_CLASS )
      * 
      *     AND ( LMFILE.LM_BRNO                 = LNFILE.LN_OWNBR
      *     AND   LMFILE.LM_ACCTNO               = LNFILE.LN_ACCTNO )
      * 
      *     AND ( LMFILE.LM_DATE_REVERSED  BETWEEN @QWS_WBEG_DATE_REV
      *                                        AND @QWS_WEND_DATE_REV
      *      OR   LMFILE.LM_DATE_REVERSED        = 0 ) 
      * 
      *     AND  (  LMFILE.LM_DATE_REVERSED     != 99999999 )
      * 
      *     AND ((  @QWS_PL                      = 'I' )
      *      OR  (  @QWS_PL                      = 'O'
      *     AND     LNFILE.LN_PLDATE            != '1900-01-01' )
      *      OR  (  @QWS_PL                      = 'S'
      *     AND     LNFILE.LN_PLDATE             = '1900-01-01' )) 
      * 
      *     AND ((  @QWS_ZBAL                    = 'I' )
      *      OR (   @QWS_ZBAL                    = 'O' 
      *     AND     LNFILE.LN_CURBAL             = 0
      *     AND     LNFILE.LN_INTBAL             = 0
      *     AND     LNFILE.LN_LCBAL              = 0 
      *     AND     LNFILE.LN_OTHBAL             = 0 )
      *      OR (   @QWS_ZBAL                    = 'S' 
      *     AND  (  LNFILE.LN_CURBAL            != 0
      *      OR     LNFILE.LN_INTBAL            != 0
      *      OR     LNFILE.LN_LCBAL             != 0 
      *      OR     LNFILE.LN_OTHBAL            != 0 ))) 
      * 
      *     AND  (( @QWS_PRIORITY_ISO            = 'S'         
      *     AND     LMFILE.LM_DATE_REVERSED     != 0)
      *      OR   ( @QWS_PRIORITY_ISO            = 'O'         
      *     AND     LMFILE.LM_DATE_REVERSED      = 0 ) 
      *      OR  (  @QWS_PRIORITY_ISO            = 'I' ))
      * 
      *   ORDER BY LMFILE.LM_BRNO,
      *            LMFILE.LM_ACCTNO,
      *            LMFILE.LM_DATE_REVERSED,
      *            LMFILE.LM_SEQ;  

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = EXEC SSP_LMEXTR_LNFILE_SELECT 
              ( :QLN3-WBEG-OWNBR,          
                :QLN3-WBEG-CLASS,         
                :QLN3-WEND-CLASS,         
                :QWS-WBEG-DATE-REV,       
                :QWS-WEND-DATE-REV,      
                :QWS-ZBAL,                
                :QWS-PL,                  
                :QWS-PRIORITY-ISO )
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
       READ-LN3-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-CLASS  TO QLN-CLASS.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   LNFILE.LN_MAKERCD1,
                   LNFILE.LN_SSNO2,
                   LNFILE.LN_MAKERCD2,
                   LNFILE.LN_SSNO3,
                   LNFILE.LN_MAKERCD3,
                   LNFILE.LN_SSNO4,
                   LNFILE.LN_MAKERCD4,
                   LNFILE.LN_ORGST,
                   LNFILE.LN_ORGBR,
                   LNFILE.LN_OWNST,
                   LNFILE.LN_SPRCLASS,
                   LNFILE.LN_CONVERCD,
                   LNFILE.LN_MIDSTREAM,
                   LNFILE.LN_PURCD,
                   LNFILE.LN_SRCD,
                   LNFILE.LN_COLLCD,
                   LNFILE.LN_COLLID,
                   LNFILE.LN_POCD,
                   LNFILE.LN_PLCD,
                   LNFILE.LN_PLTYPE,
                   LNFILE.LN_CONFIDENTCD,
                   LNFILE.LN_ALLOTCD,
                   LNFILE.LN_RECASTCD,
                   LNFILE.LN_SKIP_TRW,
                   LNFILE.LN_STATUSFG,
                   LNFILE.LN_SMALLOAN,
                   LNFILE.LN_SALEFIN,
                   LNFILE.LN_SERVACCT,
                   LNFILE.LN_LOANTYPE,
                   LNFILE.LN_ADDON_FG,
                   LNFILE.LN_PAY_SCHLD_FG,
                   LNFILE.LN_LC_CODE,
                   LNFILE.LN_CAN_LOAN_NO,
                   LNFILE.LN_STATFLAGS,
                   CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   LNFILE.LN_STATE_FLAG,
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   LNFILE.LN_FEECODE_1,
                   LNFILE.LN_FEEAMT_1,
                   LNFILE.LN_FEEOURS_1,
                   LNFILE.LN_FEEFINCHG_1,
                   LNFILE.LN_FEECODE_2,
                   LNFILE.LN_FEEAMT_2,
                   LNFILE.LN_FEEOURS_2,
                   LNFILE.LN_FEEFINCHG_2,
                   LNFILE.LN_FEECODE_3,
                   LNFILE.LN_FEEAMT_3,
                   LNFILE.LN_FEEOURS_3,
                   LNFILE.LN_FEEFINCHG_3,
                   LNFILE.LN_FEECODE_4,
                   LNFILE.LN_FEEAMT_4,
                   LNFILE.LN_FEEOURS_4,
                   LNFILE.LN_FEEFINCHG_4,
                   LNFILE.LN_FEECODE_5,
                   LNFILE.LN_FEEAMT_5,
                   LNFILE.LN_FEEOURS_5,
                   LNFILE.LN_FEEFINCHG_5,
                   LNFILE.LN_FEECODE_6,
                   LNFILE.LN_FEEAMT_6,
                   LNFILE.LN_FEEOURS_6,
                   LNFILE.LN_FEEFINCHG_6,
                   LNFILE.LN_FEECODE_7,
                   LNFILE.LN_FEEAMT_7,
                   LNFILE.LN_FEEOURS_7,
                   LNFILE.LN_FEEFINCHG_7,
                   LNFILE.LN_FEECODE_8,
                   LNFILE.LN_FEEAMT_8,
                   LNFILE.LN_FEEOURS_8,
                   LNFILE.LN_FEEFINCHG_8,
                   LNFILE.LN_FEECODE_9,
                   LNFILE.LN_FEEAMT_9,
                   LNFILE.LN_FEEOURS_9,
                   LNFILE.LN_FEEFINCHG_9,
                   LNFILE.LN_FEECODE_10,
                   LNFILE.LN_FEEAMT_10,
                   LNFILE.LN_FEEOURS_10,
                   LNFILE.LN_FEEFINCHG_10,
                   LNFILE.LN_FEECODE_11,
                   LNFILE.LN_FEEAMT_11,
                   LNFILE.LN_FEEOURS_11,
                   LNFILE.LN_FEEFINCHG_11,
                   LNFILE.LN_FEECODE_12,
                   LNFILE.LN_FEEAMT_12,
                   LNFILE.LN_FEEOURS_12,
                   LNFILE.LN_FEEFINCHG_12,
                   LNFILE.LN_FEECODE_13,
                   LNFILE.LN_FEEAMT_13,
                   LNFILE.LN_FEEOURS_13,
                   LNFILE.LN_FEEFINCHG_13,
                   LNFILE.LN_FEECODE_14,
                   LNFILE.LN_FEEAMT_14,
                   LNFILE.LN_FEEOURS_14,
                   LNFILE.LN_FEEFINCHG_14,
                   LNFILE.LN_FEECODE_15,
                   LNFILE.LN_FEEAMT_15,
                   LNFILE.LN_FEEOURS_15,
                   LNFILE.LN_FEEFINCHG_15,
                   LNFILE.LN_FEECODE_16,
                   LNFILE.LN_FEEAMT_16,
                   LNFILE.LN_FEEOURS_16,
                   LNFILE.LN_FEEFINCHG_16,
                   LNFILE.LN_FEECODE_17,
                   LNFILE.LN_FEEAMT_17,
                   LNFILE.LN_FEEOURS_17,
                   LNFILE.LN_FEEFINCHG_17,
                   LNFILE.LN_FEECODE_18,
                   LNFILE.LN_FEEAMT_18,
                   LNFILE.LN_FEEOURS_18,
                   LNFILE.LN_FEEFINCHG_18,
                   LNFILE.LN_FEECODE_19,
                   LNFILE.LN_FEEAMT_19,
                   LNFILE.LN_FEEOURS_19,
                   LNFILE.LN_FEEFINCHG_19,
                   LNFILE.LN_FEECODE_20,
                   LNFILE.LN_FEEAMT_20,
                   LNFILE.LN_FEEOURS_20,
                   LNFILE.LN_FEEFINCHG_20,
                   LNFILE.LN_INSURED,
                   LNFILE.LN_INSTYPES,
                   LNFILE.LN_INSELIGIBLE,
                   LNFILE.LN_INSCOMP_1,
                   LNFILE.LN_INSOURS_1,
                   CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   LNFILE.LN_INSFINCHG_1,
                   LNFILE.LN_INSCOMP_2,
                   LNFILE.LN_INSOURS_2,
                   CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   LNFILE.LN_INSFINCHG_2,
                   LNFILE.LN_INSCOMP_3,
                   LNFILE.LN_INSOURS_3,
                   CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   LNFILE.LN_INSFINCHG_3,
                   LNFILE.LN_INSCOMP_4,
                   LNFILE.LN_INSOURS_4,
                   CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   LNFILE.LN_INSFINCHG_4,
                   LNFILE.LN_INSCOMP_5,
                   LNFILE.LN_INSOURS_5,
                   CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   LNFILE.LN_INSFINCHG_5,
                   LNFILE.LN_INSCOMP_6,
                   LNFILE.LN_INSOURS_6,
                   CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   LNFILE.LN_INSFINCHG_6,
                   LNFILE.LN_INSCOMP_7,
                   LNFILE.LN_INSOURS_7,
                   CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   LNFILE.LN_INSFINCHG_7,
                   LNFILE.LN_INSCOMP_8,
                   LNFILE.LN_INSOURS_8,
                   CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   LNFILE.LN_INSFINCHG_8,
                   LNFILE.LN_INSCOMP_9,
                   LNFILE.LN_INSOURS_9,
                   CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   LNFILE.LN_INSFINCHG_9,
                   LNFILE.LN_INSCOMP_10,
                   LNFILE.LN_INSOURS_10,
                   CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   LNFILE.LN_INSFINCHG_10,
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   LNFILE.LN_GARNISH_EXTR,
                   LNFILE.LN_SHIP_SAME_FG,
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   LNFILE.LN_LPO,
                   LNFILE.LN_REP_CD,
                   LNFILE.LN_EMP_ID,
                   LNFILE.LN_PROCESS_CD,
                   LNFILE.LN_WCBC_CREDITLIM,
                   LNFILE.LN_FOLLOWUP_CD,
                   LNFILE.LN_MIP_FG,
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   LNFILE.LN_RESCISSION,
                   LNFILE.LN_INTERIM_TRW,
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   LNFILE.LN_DLRECOURSE,
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   LNFILE.LN_FROZEN_REASON,
                   LNFILE.LN_COUPON_STATEMENT,
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   LNFILE.LN_ERNDELQ_STOP,
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   LNFILE.LN_CL_REBATE,
                   LNFILE.LN_AH_REBATE,
                   LNFILE.LN_PP_REBATE,
                   LNFILE.LN_INT_REBATE,
                   LNFILE.LN_SC_REBATE,
                   LNFILE.LN_MF_REBATE,
                   LNFILE.LN_DF_REBATE,
                   LNFILE.LN_EX_REBATE,
                   LNFILE.LN_O1_REBATE,
                   LNFILE.LN_O2_REBATE,
                   LNFILE.LN_O3_REBATE,
                   LNFILE.LN_O4_REBATE,
                   LNFILE.LN_O5_REBATE,
                   LNFILE.LN_O6_REBATE,
                   LNFILE.LN_O7_REBATE,
                   LNFILE.LN_UN_O7_REB,
                   LNFILE.LN_UN_O8_REB,
                   LNFILE.LN_UN_O9_REB,
                   LNFILE.LN_UN_O10_REB,
                   LNFILE.LN_PLREASON,
                   LNFILE.LN_COLLECTOR,
                   LNFILE.LN_ACTIONCD,
                   LNFILE.LN_ACTIONCD2,
                   LNFILE.LN_ATCODE,
                   LNFILE.LN_RECODE,
                   LNFILE.LN_ESCROW_FG,
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   LNFILE.LN_PROMID,
                   CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),
                   LNFILE.LN_PROMAMT,
                   CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),
                   CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),
                   LNFILE.LN_SOLCNT,
                   LNFILE.LN_MESSAGE,
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   LNFILE.LN_PRPOCD,
                   CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   LNFILE.LN_UNITPER_CD,
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   LNFILE.LN_REPOCD,
                   LNFILE.LN_BR_TFR_FG,
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   LNFILE.LN_BMO_CON_STATUS,
                   LNFILE.LN_BMO_REC_STATUS,
                   LNFILE.LN_APP_NUMBER,
                   LNFILE.LN_GIFTNET,
                   LNFILE.LN_GIFTNET_OUR_PO,
                   LNFILE.LN_PROC_DISBURSED,
                   LNFILE.LN_NOODDPY,
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   LNFILE.LN_TRW_STATUS,
                   LNFILE.LN_ORIGIN,
                   LNFILE.LN_PO_TRCD,
                   LNFILE.LN_NEXIS_CRSCORE,
                   LNFILE.LN_TRW_B_CONSUMER_IND,
                   LNFILE.LN_PARVHELD_PAID_FG,
                   LNFILE.LN_ACCT_FROZEN,
                   LNFILE.LN_TRW_SPECIAL_CODE,
                   LNFILE.LN_CREATION_STATUS,
                   LNFILE.LN_TOTNOLCHG,
                   LNFILE.LN_LBOX,
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   LNFILE.LN_INTERIM_COUPON,
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   LNFILE.LN_TRW_J_CONSUMER_IND,
                   LNFILE.LN_ACH,
                   LNFILE.LN_1099_C_FG,
                   LNFILE.LN_TRW_COMPLIANCE_CODE,
                   LNFILE.LN_UNWIND_REASON_CODE,
                   LNFILE.LN_PBI_PBD_FG,
                   LNFILE.LN_OLD_ACCT_FORMAT_FG,
                   LNFILE.LN_APPROVED_BY_USER,
                   LNFILE.LN_APPROVED_BY_CODE,
                   CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),
                   LNFILE.LN_APPROVED_BY_TIME,
                   CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LNFILE.LN_LTOUCH_TIME,
                   LNFILE.LN_LOS_APP_ID
            INTO :QLN-REC
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
              AND LNFILE.LN_CLASS  = :QLN-CLASS
              AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT. *> EMBEDDED
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.
           INITIALIZE QLM-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH LN3_CURSOR 
                 INTO 
                  :QLN-OWNBR, 
                  :QLN-ACCTNO,
                  :QLN-CLASS,
                  :QLN-PLDATE,
                  :QLN-CURBAL,
                  :QLN-INTBAL,
                  :QLN-LCBAL,
                  :QLN-OTHBAL,
                  :QLM-BRNO,
                  :QLM-ACCTNO,
                  :QLM-DATE-REVERSED,
                  :QLM-SEQ,
                  :QLM-MEMO,
                  :QLM-MEMO-TYPE
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
              PERFORM GET-LM-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *    IF ( LN3-CURSOR-GR-STAT-GOOD )
      *       EXEC SQL
      *            CLOSE LN3_CURSOR_GR
      *       END-EXEC
      *       MOVE STAT---BAD TO LN3-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN3-CONN-OPEN-SW.

      *-----------------------------------------------------------------

       COPY "LIBLP/LPLM1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF SV-BEG-BRANCH NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BRANCH TO BEG-BRANCH
                 MOVE SV-END-BRANCH TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH TO BEG-BRANCH
                 MOVE SV-END-BRANCH TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LMEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
