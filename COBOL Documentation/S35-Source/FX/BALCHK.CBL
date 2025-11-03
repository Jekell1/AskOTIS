       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BALCHK.
       DATE-WRITTEN. 1/27/84
      *****************************************************
      *                      (FX)
      *    DESCRIPTION:  CHECK ENDING BALANCE IN LOAN
      *                    TO THE LAST PAYMENT RECORD
      * REV:
      *  101794 BAH REMOVED ALL OCCURANCES OF LP2
      *  MJD 103095 COPIED AND CONVERETED FROM A50.
      *             ADDED GROUP LOGIC.
      *   CS 151120 ADDED MOVE OF LP-OWNBR    TO LP-BRNO, BRNO IS IN KEY.
      *   CS 151204 REMOVED LP-BRNO FROM KEY
      *  BAH 181203 GLOBAL LNFILE
      *  BAH 181219 GLOBAL LPFILE
      *  BAH 190918 REMOVED ACCESS-CALL ON LNFILE, LPFILE
      *  BAH 2022.1004 HARD CODED STARTS
      *****************************************************
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
      /
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./FX/BALCHK.CBL S35 09/20/22-13:05:45 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *----------------------------------------------------------------

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  BEG-BRANCH           PIC 9(4).
       01  END-BRANCH           PIC 9(4).
       01  GROUP-FLAG           PIC X.
       01  ENT-GROUP            PIC X(4).

       01  ERRCD           PIC X.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(6)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD1A.
           03  FILLER           PIC X(10)    VALUE "BRANCH :".
           03  FILLER           PIC X(6)     VALUE " ".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X(5)     VALUE " ".
           03  H-BRDESC         PIC X(40).

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(6)     VALUE " ".
           03  H-TITLE            PIC X(32)    VALUE
           " - - LOAN BALANCE CHECK LIST- -".

       01  HEAD3.
           03  FILLER           PIC X(25)    VALUE
           "BRANCH #/NAME".
           03  FILLER           PIC X(34)    VALUE
           "  ACCOUNT     NAME                ".
           03  FILLER           PIC X(22)    VALUE
           "LOAN LAST PAYMENT     ".
           03  FILLER           PIC X(28)    VALUE
           " LOAN BAL        PAYMENT BAL".


       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
       01  D-LINE REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X.
           03  D-BRNAME         PIC X(20).
           03  FILLER           PIC X.
           03  D-NUMBER         PIC Z(6).
           03  FILLER           PIC X(5).
           03  D-NAME           PIC X(20).
           03  FILLER           PIC X(3).
           03  D-LN-LAST-DATE   PIC 99/99/99.
           03  FILLER       PIC XX.
           03  D-LN-LAST-AMT    PIC Z(7).99.
           03  D-MESS.
               05  D-LOAN-BAL   PIC Z(7).99.
               05  FILLER       PIC XX.
               05  D-LP-BAL     PIC Z(7).99.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  BALCHK-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/BALCHK_DEF.CPY".
       COPY "LIBFX/BALCHK_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBFX/BALCHK_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***************************************************
       MAIN-MODULE SECTION.
      ***************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BALCHK" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BALCHK-WINDOW-HANDLE.

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
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE.

           MOVE " " TO MESS.

           STRING "PROCESSING BRANCH NO: " BR-NO
                                DELIMITED BY SIZE INTO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.


           PERFORM START-LN1-FILE.

       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-POCD-NONLOAN
              GO TO NEXT-LN.

           IF LN-CONVERCD NOT = " "
              GO TO NEXT-LN.

           PERFORM DETAIL-LINE-PROCESS.
           GO TO NEXT-LN.
       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LP1-FILE.

           COMPUTE LN-COUNT = EXT-CONAME-RPT-LINES + 1.

           GO TO NEXT-GROUP-LOC.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND " TO MESS
              PERFORM SEND-MESS.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.
       GETPR.
           MOVE BALCHK-QUEUE-POS TO Q-POS.
           MOVE BALCHK-COPIES-POS TO C-POS.
           MOVE BALCHK-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00" GO TO END-INIT.

           PERFORM ENTER-PARAMETERS.
           IF ERRCD NOT = " "
              GO TO GETPR.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.
           MOVE " " TO ERRCD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           GO TO EXIT-INITIALIZE.
       END-INIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARM.
           MOVE " " TO ERRCD.

      * DEFAULT TO USER BRANCH
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 0 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM-ENDIT.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ENDIT.

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

           IF VDUSTATUS = "10" GO TO EXIT-PARM-ENDIT.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ENDIT.
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

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
      *     PERFORM BR-RANGE-SECURITY.
      *     IF BR-SECURITY-OKAY = "N"
      *        GO TO BR-01.

       OK-YN.
           MOVE "E NA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM-ENDIT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ENDIT.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS NOT = "00" GO TO OK-YN.
           IF VDUBUF = "N" GO TO EXIT-PARM-ENDIT.
           IF VDUBUF NOT = "Y" GO TO OK-YN.

           GO TO EXIT-PARM.
       EXIT-PARM-ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-PARM.
           EXIT.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO

           PERFORM START-LP1-FILE-NOT-GREATER.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR  (LN-ACCTNO NOT = LP-ACCTNO)
              MOVE LN-ACCTNO TO D-NUMBER
              MOVE "PAYMENT RECORD MISSING" TO D-MESS
              MOVE BR-NO TO D-BRNO
              MOVE BR-NAME TO D-BRNAME
              PERFORM WRITE-DETAIL-LINE
              MOVE 2 TO LN-FEED
           ELSE
              IF LN-CURBAL NOT = LP-CURBAL
                 PERFORM GET-BW
                 MOVE BW-LNAME TO D-NAME
                 MOVE LN-ACCTNO TO D-NUMBER
                 MOVE LN-CURBAL TO D-LOAN-BAL
                 MOVE LP-CURBAL TO D-LP-BAL
                 MOVE LN-DATE-PAID-LAST TO D-LN-LAST-DATE
                 MOVE LN-LASTPYAMT TO D-LN-LAST-AMT
                 MOVE BR-NO TO D-BRNO
                 MOVE BR-NAME TO D-BRNAME
                 PERFORM WRITE-DETAIL-LINE
                 MOVE 2 TO LN-FEED.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ****************************
       HEAD-PAGE SECTION.
      ****************************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE BR-NO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.
           MOVE HEAD1A TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO BALCHK-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BALCHK-SYSDATE.
           DISPLAY BALCHK-SCREEN UPON BALCHK-WINDOW-HANDLE.
           MOVE BALCHK-FORM-FIELDS TO WR-BUF.
           MOVE BALCHK-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE BALCHK-HELP--FILE TO HELP-LINK-FILE.
           MOVE BALCHK-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/BALCHK_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       MISC SECTION.
      ****************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       GET-BW.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "NOT FOUND" TO BW-LNAME.

      ****************************************************
       IO-ROUTINE SECTION.
      ****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************************
       END-PROGRAM SECTION.
      ****************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BALCHK-SCREEN.
           DESTROY BALCHK-WINDOW-HANDLE.
           EXIT PROGRAM.
