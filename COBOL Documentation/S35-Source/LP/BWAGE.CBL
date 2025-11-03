       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWAGE.
       DATE-WRITTEN. 11-08-94
      *****************************************************
      *    DIR :  LP
      *    DESC:  BORROWER AGE DISTRIBUTION REPORT
      *  REV:
      *  KEC 091096 FIXED PRINTING "NO REPORT GENERATED" WHEN EXITING.
      *  MJD 011598 BROUGHT OVER FROM A90.
      *             ADDED A10 DATE ROUTINES, CHANGED USERID TO PIC 10
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BAH 170510 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 180928 GLOBAL FILES
      *  BAH 20190926 REMOVED ACCESS-CALL BWFILE
      *  BAH 20200827 REMOVED RESET ON BR1 
      *****************************************************
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

      *************************************
      *   SUMMARY TOTALS WORK RECORD      *
      *************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-STATE  PIC XX.
               05  WK-BRNO   PIC 9(4).
           03  WK-QTY        PIC 9(8)     COMP-3 OCCURS 13.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)LP/BWAGE.CBL S35 02/21/24-11:42:31 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  WK-IFN        PIC XX       VALUE "WK".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED              PIC 99    VALUE 0.
       01  LN-COUNT             PIC 999   VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)  VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
           88  SKIP-RECORDS-FOUND            VALUE " ".

       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  TODAYS-DATE          PIC 9(8)     VALUE 0.

      ********************************************
      *               T O T A L S                *
      ********************************************
       01  TOTAL-TABLE            VALUE ZEROS.
           03  TOT-TAB            OCCURS 2.
               05  AGE-TOT PIC 9(8) OCCURS 13 TIMES.

       01  SUB                    PIC 9(6) COMP-3.
       01  WS-TEST-ZERO           PIC 9(8) COMP-3 VALUE 0.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9999.
           03  END-BRANCH      PIC 9999.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "D".

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
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(32)    VALUE " ".
           03  FILLER           PIC X(25)    VALUE
           "BORROWER AGE DISTRIBUTION".

       01  HEAD3.
           03  FILLER      PIC X(28)    VALUE
                "ST  BR# BRANCH NAME".
           03  FILLER      PIC X(32)    VALUE
                "UNDER 21   21-25   26-30   31-35".
           03  FILLER      PIC X(32)    VALUE
                "   36-40   41-45   46-50   51-55".
           03  FILLER      PIC X(40)    VALUE
                "   56-60   61-65   66-70 OVER 70   TOTAL".


       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-STATE     PIC X(3).
           03  D-BRNO      PIC ZZZZ.
           03  FILLER      PIC X.
           03  D-BRNAME    PIC X(20).
           03  D-NUMBER    PIC ZZZZ,ZZZ OCCURS 13.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BWAGE_DEF.CPY".
       COPY "LIBLP/BWAGE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BWAGE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWAGE" TO FORM-PROG VDU-FORM-NAME.

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
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.
           MOVE "Y" TO NEW-BRANCH.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.

           PERFORM START-BW1-FILE.
       NEXT-BW.
           PERFORM READ-BW1-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF BW-LOANS = 0
              IF BW-LIASMLN = 0
                 IF BW-LIAOTH = 0
                    GO TO NEXT-BW.

           IF (BW-BDATE = 0)
              GO TO NEXT-BW.

           MOVE BW-BDATE    TO BAGE-DATE.
           MOVE BW-REFDATE  TO BAGE-REFDATE.
           MOVE TODAYS-DATE TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.

           IF BAGE-AGE < 21
              ADD 1 TO AGE-TOT(1, 1)
           ELSE IF BAGE-AGE < 26
              ADD 1 TO AGE-TOT(1, 2)
           ELSE IF BAGE-AGE < 31
              ADD 1 TO AGE-TOT(1, 3)
           ELSE IF BAGE-AGE < 36
              ADD 1 TO AGE-TOT(1, 4)
           ELSE IF BAGE-AGE < 41
              ADD 1 TO AGE-TOT(1, 5)
           ELSE IF BAGE-AGE < 46
              ADD 1 TO AGE-TOT(1, 6)
           ELSE IF BAGE-AGE < 51
              ADD 1 TO AGE-TOT(1, 7)
           ELSE IF BAGE-AGE < 56
              ADD 1 TO AGE-TOT(1, 8)
           ELSE IF BAGE-AGE < 61
              ADD 1 TO AGE-TOT(1, 9)
           ELSE IF BAGE-AGE < 66
              ADD 1 TO AGE-TOT(1, 10)
           ELSE IF BAGE-AGE < 71
              ADD 1 TO AGE-TOT(1, 11)
           ELSE
              ADD 1 TO AGE-TOT(1, 12).
           ADD 1 TO AGE-TOT(1, 13).

           GO TO NEXT-BW.

       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.

      * TRYING TO MATCH A15 WHICH SKIPPED IF BORROWER FILE DIDNT EXIST.
      * BUT WITH A GROUP BORROWER FILE, IT WAS STILL CREATING AN EMPTY
      * WORK RECORD FOR ALL BRANCHES. SKIP IF ZERO TOTALS
           MOVE 0 TO WS-TEST-ZERO.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 13
              ADD AGE-TOT(1, SUB) TO WS-TEST-ZERO
           END-PERFORM.
           
           IF WS-TEST-ZERO NOT = 0
              PERFORM CREATE-WORK-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           PERFORM DETAIL-LINE-PROCESS.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM
           ELSE
              MOVE "Y" TO RECORDS-FOUND.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE BWAGE-QUEUE-POS TO Q-POS.
           MOVE BWAGE-COPIES-POS TO C-POS.
           MOVE BWAGE-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE.

      * CREATE TOTALS WORK FILE
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

           IF EXT-ROUTE-BUF = "00"
             MOVE "BORROWER AGE DISTRIBUTION IN PROGRESS..." TO WR-BUF
             MOVE MESS-LIST TO WR-LIST
             PERFORM CALL-WRFORM.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                   END-BRANCH.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-WK-FILE.


      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************

      ****************************************
       ENTER-PARAMETERS SECTION.
      ****************************************
       ENTER-PARM.
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
              GO TO EXIT-PARM.
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
              PERFORM INVALID-RANGE
              GO TO BR-01.
           GO TO EXIT-PARM.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       EXIT-PARM.
           EXIT.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************
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

      ****************************************
       CREATE-WORK-FILE SECTION.
      ****************************************

           MOVE BR-STATE TO WK-STATE.
           MOVE BR-NO TO WK-BRNO.
           PERFORM READ-WK-FILE.
           IF NOT IO-GOOD
              PERFORM ZERO-WORK-TABLE
              MOVE BR-STATE TO WK-STATE
              MOVE BR-NO    TO WK-BRNO.

           MOVE 1 TO SUB.
       ADD-BRANCH-TOTALS.
           ADD AGE-TOT(1, SUB) TO WK-QTY(SUB).
           ADD 1 TO SUB.
           IF SUB < 14
              GO TO ADD-BRANCH-TOTALS.
           IF IO-GOOD
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           MOVE BR-STATE TO WK-STATE.
           MOVE 9999 TO WK-BRNO.
           PERFORM READ-WK-FILE.
           IF NOT IO-GOOD
              PERFORM ZERO-WORK-TABLE
              MOVE BR-STATE TO WK-STATE
              MOVE 9999 TO WK-BRNO.
           MOVE 1 TO SUB.
       ADD-STATE-TOTALS.
           ADD AGE-TOT(1, SUB) TO WK-QTY(SUB).
           ADD 1 TO SUB.
           IF SUB < 14 GO TO ADD-STATE-TOTALS.
           IF IO-GOOD
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           PERFORM ZERO-WS-TOTALS.

           
      ******************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME.

           MOVE SPACES TO WK-STATE.
           MOVE 0      TO WK-BRNO.
           PERFORM START-WK-FILE.
       NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              PERFORM PRINT-GRAND-TOTALS
              GO TO END-DETAIL-LN-PROCESS.

           MOVE WK-STATE TO D-STATE.
           IF WK-BRNO = 9999
              MOVE "STATE TOTALS:" TO D-BRNAME
           ELSE
              MOVE WK-BRNO TO D-BRNO BR-NO
              PERFORM READ-BR-FILE
              IF IO-GOOD
                 MOVE BR-NAME TO D-BRNAME
              ELSE
                 MOVE "INVALID BRANCH" TO D-BRNAME.
           MOVE 1 TO SUB.
       MOVE-AGE-DETAIL.
           MOVE WK-QTY(SUB) TO D-NUMBER(SUB).

           IF WK-BRNO = 9999
              ADD WK-QTY(SUB) TO AGE-TOT(2, SUB).

           ADD 1 TO SUB.

           IF SUB < 14
              GO TO MOVE-AGE-DETAIL.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF WK-BRNO = 9999
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           GO TO NEXT-WK.
       END-DETAIL-LN-PROCESS.
           EXIT.

      ******************************************
       PRINT-GRAND-TOTALS SECTION.
      ******************************************
           MOVE "GRAND TOTALS" TO D-BRNAME.
           MOVE 1 TO SUB.
       MOVE-GRAND-TOTALS.
           MOVE AGE-TOT(2, SUB) TO D-NUMBER(SUB).
           ADD 1 TO SUB.
           IF SUB < 14 GO TO MOVE-GRAND-TOTALS.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************
       HEAD-PAGE SECTION.
      ******************************************

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
       END-HEAD-PAGE.
           EXIT.

      ******************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO BWAGE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWAGE-SYSDATE.
           DISPLAY BWAGE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BWAGE-FORM-FIELDS TO WR-BUF.
           MOVE BWAGE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE BWAGE-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWAGE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BWAGE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       MISC-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       ZERO-WORK-TABLE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
               MOVE 0 TO WK-QTY(SUB)
           END-PERFORM.
       ZERO-WS-TOTALS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
              MOVE 0 TO AGE-TOT(1, SUB)
           END-PERFORM.

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************
       END-PROGRAM SECTION.
      ******************************************
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
              DESTROY BWAGE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
