       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FX1421.
       DATE-WRITTEN.    02/12/2020.
      ******************************************************************
      *
      *  DESCRIPTION:  FIX LN-ORIGIN AND BW-ORIGIN
      *                CLEAR ANY VALUE THAT IS NOT "SPAN"
      * REV:
      *  02/12/2020 BAH CLEAR BW-ORIGIN AND LN-ORIGIN OF ANY VALUE
      *                 THAT IS NOT "SPAN" #1421
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/FX1421.CBL A32 02/13/20-10:56:00 >".
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

       01  PR-IFN            PIC X(2) VALUE "PR".
       01  BEG-BRANCH        PIC 9(4).
       01  END-BRANCH        PIC 9(4).
      **********************
      *   MISC WORKERS     *
      **********************
       01  GROUP-FLAG           PIC X(1)     VALUE SPACES.
       01  ENT-GROUP            PIC X(4)     VALUE SPACES.
       01  FIRST-TIME           PIC X        VALUE "Y".

       01  ERRCD           PIC X.
       
       01  TOTAL-COUNT          PIC 9(9) VALUE 0.
       01  TOTAL-BW-COUNT       PIC 9(9) VALUE 0.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  MESS-LIST         PIC X(5)  VALUE "MESS.".

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
           03  FILLER           PIC X(5)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(5)    VALUE " ".
           03  H-TITLE          PIC X(40)  VALUE
           "  ORIGIN FIX ".

       01  HEAD3.
           03  FILLER           PIC X(50) VALUE
               "BRNO    KEY        OLD VALUE   ".

       01  DETAIL-LINE       PIC X(80) VALUE SPACE.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO            PIC ZZZ9.
           03  FILLER            PIC X(3).
           03  D-NUMBER          PIC 9(9).
           03  FILLER            PIC X(2).
           03  D-TYPE            PIC XX.
           03  FILLER            PIC X(2).
           03  D-ORIGIN          PIC X(4).

       01  D-TOT REDEFINES DETAIL-LINE.
           03  D-TOTAL-TEXT      PIC X(10).
           03  FILLER            PIC X(3).
           03  D-TOTAL           PIC Z(7)9.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  FX1421-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FX1421_DEF.CPY".
       COPY "LIBFX/FX1421_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/FX1421_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **********************************
      *       MAIN PROGRAM CONTROL
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FX1421" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FX1421-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
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

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM GLOBAL-UPDATE-LN.
           PERFORM GLOBAL-UPDATE-BW.

       END-OF-GROUP.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF TOTAL-COUNT NOT = 0 OR TOTAL-BW-COUNT NOT = 0
              MOVE "TOTAL LN:" TO D-TOTAL-TEXT
              MOVE TOTAL-COUNT TO D-TOTAL
              MOVE 4 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "TOTAL BW:" TO D-TOTAL-TEXT
              MOVE TOTAL-BW-COUNT TO D-TOTAL
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           GO TO END-PROGRAM.

      ******************************************
       GLOBAL-UPDATE-LN SECTION.
      ******************************************

           STRING "PROGRAM IN PROGRESS....BRNO: " BR-NO
                       DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.

       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              PERFORM CLOSE-LN1-FILE
              GO TO EXIT-GLOBAL-UPDATE-LN.

      * SKIP DELETED LOANS
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-ORIGIN = SPACES OR "SPAN"
              GO TO NEXT-LN.

           IF FIRST-TIME = "Y"
              PERFORM HEAD-PAGE.

           MOVE BR-NO     TO D-BRNO.
           MOVE LN-ACCTNO TO D-NUMBER.
           MOVE "LN"      TO D-TYPE.
           MOVE LN-ORIGIN TO D-ORIGIN.
           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO TOTAL-COUNT.

           MOVE SPACES TO LN-ORIGIN.
           PERFORM REWRITE-LN1-FILE.

           GO TO NEXT-LN.

       EXIT-GLOBAL-UPDATE-LN.
           EXIT.

      ***************************************************
       GLOBAL-UPDATE-BW SECTION.
      ***************************************************
           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.

           PERFORM START-BW1-FILE.
       NEXT-BW.
           PERFORM READ-BW1-FILE-NEXT.
           IF IO-FG = 9 OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              PERFORM CLOSE-BW1-FILE
              GO TO EXIT-GLOBAL-UPDATE-BW.

           IF BW-ORIGIN = SPACES OR "SPAN"
              GO TO NEXT-BW.

           IF FIRST-TIME = "Y"
              PERFORM HEAD-PAGE.

           MOVE BR-NO     TO D-BRNO.
           MOVE BW-SSNO   TO D-NUMBER.
           MOVE "BW"      TO D-TYPE.
           MOVE BW-ORIGIN TO D-ORIGIN.
           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO TOTAL-BW-COUNT.
           MOVE SPACES TO BW-ORIGIN.
           PERFORM REWRITE-BW1-FILE.

           GO TO NEXT-BW.

       EXIT-GLOBAL-UPDATE-BW.
           EXIT.

      **********************************
       WRITE-DETAIL-LINE SECTION.
      **********************************
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ********************************
       INITIALIZATION SECTION.
      ********************************
           MOVE " " TO ERRCD.

       SETRP.
           MOVE FX1421-QUEUE-POS TO Q-POS.
           MOVE FX1421-COPIES-POS TO C-POS.
           MOVE FX1421-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00" GO TO ENDIT.

       BR-01.
           MOVE "R  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO ENDIT.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BEG-BRANCH." TO VDU
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
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           IF ( BEG-BRANCH = ZEROES )
              GO TO BR-01.

       BR-02.
           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
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
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *****************************************
      *    R - REPORT ONLY
      *    U - UPDATE RECORDS WITH FIX
      *    B - BOTH
      *****************************************
      *INIT-ACT.
      *    MOVE "E  A010ACT." TO VDU.
      *    PERFORM SCREENIO.
      *    IF VDUSTATUS = "1>" GO TO ENDIT.
      *    IF VDUSTATUS = "1U" GO TO BR-01.
      *    IF VDUSTATUS NOT = "00" GO TO INIT-ACT.
      *    IF NOT (VDUBUF = "R" OR "U" OR "B")
      *       GO TO INIT-ACT.
      *    MOVE VDUBUF TO ACT.


       INIT-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO BR-01
           IF VDUSTATUS NOT = "00" GO TO INIT-ACCEPT.
           IF VDUBUF = "N" GO TO ENDIT.
           IF VDUBUF NOT = "Y" GO TO INIT-ACCEPT.

           MOVE "PROGRAM IN PROGRESS..........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 6 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
           MOVE "N" TO FIRST-TIME.
       END-HEAD-PAGE.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO FX1421-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FX1421-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FX1421-SCREEN UPON FX1421-WINDOW-HANDLE.
           MOVE FX1421-FORM-FIELDS TO WR-BUF.
           MOVE FX1421-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE FX1421-HELP--FILE TO HELP-LINK-FILE.
           MOVE FX1421-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FX1421_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARA SECTION.
      ******************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS      TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************************
       END-PROGRAM SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FX1421-SCREEN.
           DESTROY FX1421-WINDOW-HANDLE.
           EXIT PROGRAM.
