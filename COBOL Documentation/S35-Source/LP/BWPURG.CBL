       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWPURG.
       DATE-WRITTEN.    081884.
      **********************************************************
      *                      (LP)
      * DESCRIPTION:  BORROWER MASTER FILE PURGE
      *                  ONLY PURGES IN YOUR BRANCH
      *          
      *
      * REV:
      *  JFF 100295 ADDED BORROWER SCAN
      *  MJD EXPANDED BW-LOANS TO PIC 99
      *  GLF 111098 MADE MEMO DATE 2000 COMPATIBLE
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BAH 061116 FORGOT TO DELETE BX TRAILER RECORD, LENDMARK PR# 2044
      *  BAH 110914 REPLACED RRFORM WITH SCREEN
      *  BAH 170511 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181001 GLOBAL BWFILE,BXFILE, BMFILE
      *  BAH 181126 GLOBAL LHFILE
      *  BAH 181204 GLOBAL LNFILE
      *  BAH 20230714 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      **********************************************************
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
           "@(#)LP/BWPURG.CBL J35 12/21/23-08:50:50 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LPWSLH.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       01  PR-IFN               PIC X(4)    VALUE "0APR".

       01  HOLD-BW1-KEY         PIC 9(13)    VALUE 0.
       01  FILLER REDEFINES HOLD-BW1-KEY.
           03  HOLD-BW-OWNBR    PIC 9(4).
           03  HOLD-BW-SSNO     PIC 9(9).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 0.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  NAME-WORKER          PIC X(70).
       01  FIRST-NAME           PIC X(35).
       01  LAST-NAME            PIC X(35).
       01  ZIP                  PIC X(10).
       01  HOLD-BM-DATE         PIC 9(8) COMP-3.

       01  BORROWER-FG          PIC X(3)    VALUE "   ".
           88  BEG-BORROWER                 VALUE "BEG".
           88  END-BORROWER                 VALUE "END".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  ACTION          PIC X.
           03  BEG-BW          PIC 9(9).
           03  END-BW          PIC 9(9).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  PRINT-MEMOS     PIC X.

       01  ACCEPT-BUF          PIC X    VALUE " ".

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
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           "- - - BORROWER FILE PURGE REPORT - - -  ".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "S.S. NO.".
           03  FILLER      PIC X(4)     VALUE " ".
           03  FILLER      PIC X(22)    VALUE "NAME / ADDRESS / PHONE".
           03  FILLER      PIC X(13)    VALUE " ".
           03  FILLER      PIC X(10)    VALUE "REF DATE  ".
           03  FILLER      PIC X(5)     VALUE "LOANS".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(6)     VALUE "OWN/RT".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(5)     VALUE "SOLIC".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(6)     VALUE "COUNTY".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(6)     VALUE "CENSUS".
           03  FILLER      PIC X(4)     VALUE " ".
           03  FILLER      PIC X(8)     VALUE "EMPLOYER".
           03  FILLER      PIC X(20)    VALUE " ".
           03  FILLER      PIC X(7)     VALUE "NET PAY".

       01  HEAD4.
           03  FILLER      PIC X(46)    VALUE " ".
           03  FILLER      PIC X(9)     VALUE "BIRTH/AGE".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(5)     VALUE "SLSMN".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(4)     VALUE "TIME".
           03  FILLER      PIC X(4)     VALUE " ".
           03  FILLER      PIC X(4)     VALUE "SMSA".
           03  FILLER      PIC XXX      VALUE " ".
           03  FILLER      PIC X(6)     VALUE "BRANCH".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(6)     VALUE "YR EMP".
           03  FILLER      PIC X(4)     VALUE " ".
           03  FILLER      PIC X(8)     VALUE "POSITION".
           03  FILLER      PIC X(20)    VALUE " ".
           03  FILLER      PIC X(7)     VALUE "OTH PAY".

       01  HEAD5.
           03  FILLER      PIC X(49)    VALUE " ".
           03  FILLER      PIC X(6)     VALUE "DEPEND".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(5)     VALUE "REPCD".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(6)     VALUE "CREDIT".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(5)     VALUE "DWELL".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(6)     VALUE "CONFID".
           03  FILLER      PIC X(12)    VALUE " ".
           03  FILLER      PIC X(5)     VALUE "PHONE".
           03  FILLER      PIC X(23)    VALUE " ".
           03  FILLER      PIC X(7)     VALUE "OTH SRC".

       01  HEAD6.
           03  FILLER      PIC X(49)    VALUE " ".
           03  FILLER      PIC X(6)     VALUE "ORIGIN".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(5)     VALUE "PRLNS".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(6)     VALUE "      ".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(5)     VALUE "     ".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(6)     VALUE "      ".
           03  FILLER      PIC X(12)    VALUE " ".
           03  FILLER      PIC X(10)    VALUE "WORK HOURS".
           03  FILLER      PIC X(18)    VALUE " ".
           03  FILLER      PIC X(7)     VALUE "       ".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-SSNO      PIC 999/99/9999.
           03  FILLER      PIC X.
           03  D-NAME      PIC X(34).
           03  FILLER      PIC X.
           03  D-DATE      PIC 99/99/99.
           03  FILLER      PIC X(5).
           03  D-LOANS     PIC Z9.
           03  FILLER      PIC X(3).
           03  D-RENTCD    PIC XXXX.
           03  FILLER      PIC X(5).
           03  D-SOLICIT   PIC X.
           03  FILLER      PIC X(6).
           03  D-COUNTY    PIC XXX.
           03  FILLER      PIC XXX.
           03  D-CENSUSCD  PIC X(10).
           03  D-EMPLOYER  PIC X(24).
           03  FILLER      PIC X.
           03  D-EMPPAY    PIC ZZZ,ZZZ.ZZ.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(57).
           03  D-ORGSLSMN  PIC ZZZZ9.
           03  FILLER      PIC X(3).
           03  D-TIMEADDR  PIC XXXX.
           03  FILLER      PIC XXX.
           03  D-STRTGCD   PIC X(6).
           03  FILLER      PIC XXX.
           03  D-BRANCH    PIC ZZZ9.
           03  FILLER      PIC X(6).
           03  D-EMPYEARS  PIC Z9.
           03  FILLER      PIC X(6).
           03  D-EMPLPOS   PIC X(24).
           03  FILLER      PIC X.
           03  D-EMPPAYOT  PIC ZZZ,ZZZ.ZZ.

       01  D-LINE3 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(53).
           03  D-DEPEND    PIC Z9.
           03  FILLER      PIC X(3).
           03  D-REPCODE   PIC XXXX.
           03  FILLER      PIC X(3).
           03  D-CREDITCD  PIC XXXX.
           03  FILLER      PIC X(4).
           03  D-DWELLCD   PIC XX.
           03  FILLER      PIC X(7).
           03  D-CONFIDEN  PIC X.
           03  FILLER      PIC X(14).
           03  D-EMPPHONE  PIC 999/999/9999.
           03  FILLER      PIC XX.
           03  D-EXTTAG    PIC XXXX.
           03  FILLER      PIC X.
           03  D-EMPEXT    PIC ZZZZ.
           03  FILLER      PIC X(4).
           03  D-EMPPAYSR  PIC X(8).

       01  D-LINE4 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(51).
           03  D-ORIGIN    PIC XXXX.
           03  FILLER      PIC X(5).
           03  D-PRLOANS   PIC Z9.
           03  FILLER      PIC X(35).
           03  D-WORKHRS   PIC X(9).

       01  D-LINE5 REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(12).
           03  D-PHONE         PIC 999/999/9999.
           03  D-PHONEX        REDEFINES D-PHONE.
               05  D-SSNO2     PIC 999/99/9999  BLANK ZERO.
               05  FILLER      PIC X.
           03  FILLER          PIC X(23).
           03  D-INT           PIC ZZZZZZ9.99-    BLANK ZERO.
           03  D-INTX          REDEFINES D-INT.
               05  D-PAYOFF    PIC 99/99/99 BLANK ZERO.
               05  FILLER      PIC XXX.
           03  D-REFX          PIC X(9).
           03  FILLER          PIC X(10).
           03  D-INT2          PIC ZZZZZZ9.99-    BLANK ZERO.
           03  D-REFX2         PIC X(9).

       01  D-LINE6 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(47).
           03  D-BM-DATE   PIC 99/99/99.
           03  FILLER      PIC XX.
           03  D-BM-SEQ    PIC 99.
           03  FILLER      PIC XX.
           03  D-BM-MEMO   PIC X(60).
           03  FILLER      PIC X(11).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(50).
           03  RANGE-SELECTION  PIC X(72).
           03  RANGE-DATE REDEFINES RANGE-SELECTION PIC 9999/99/99.

       COPY "LIBWI/BWSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01  BWPURG-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BWPURG_DEF.CPY".
       COPY "LIBLP/BWPURG_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/BWPURG_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LH1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWPURG" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BWPURG-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

      D    STOP MESS.
           MOVE BW-PATH-OWNBR TO BW-OWNBR
                                 QBW1-WBEG-OWNBR
                                 QBW1-WEND-OWNBR.
           MOVE BEG-BW        TO BW-SSNO
                                 QBW1-WBEG-SSNO.
           MOVE END-BW        TO QBW1-WEND-SSNO.

           PERFORM START-BW1-FILE.
       NEXT-BW.
           PERFORM READ-BW1-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
             GO TO END-ROUTINE.

           IF BW-SSNO > END-BW
              GO TO END-ROUTINE.

           IF BW-SSNO < BEG-BW
      *       UNLOCK BW-FILE
              GO TO NEXT-BW.

           IF BW-LOANS NOT = 0 OR
                BW-LIASMLN NOT = 0 OR
                  BW-LIAOTH NOT = 0
      *       UNLOCK BW-FILE
              GO TO NEXT-BW.

      * IF BW-LASTPODATE IS 0, IT WAS GETTING SKIPPED DUE TO < 19500101
           IF (BEG-DATE NOT = EXT-JULIAN-BEG) AND
               (END-DATE NOT = EXT-JULIAN-END)
             IF (BW-LASTPODATE < BEG-DATE) OR (BW-LASTPODATE > END-DATE)
      *         UNLOCK BW-FILE
                GO TO NEXT-BW.

           PERFORM CHECK-LOAN-ACTIVITY.
           IF IO-BAD
      *       UNLOCK BW-FILE
              GO TO NEXT-BW.

      * SAVE THIS KEY
           MOVE BW1-KEY TO HOLD-BW1-KEY. 

      * IF REPORT OR BOTH
           IF ACTION NOT = "P"
              PERFORM DETAIL-LINE-PROCESS.
      * IF PURGE OR BOTH
           IF ACTION NOT = "R"
              PERFORM PURGE-BORROWER-PROCESS.

           MOVE "N" TO FIRST-TIME.
           GO TO NEXT-BW.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.
       SETRP.
           MOVE BWPURG-QUEUE-POS TO Q-POS.
           MOVE BWPURG-COPIES-POS TO C-POS.
           MOVE BWPURG-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.
       RRACT.
           MOVE "E  A010ACT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RRACT.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "R" GO TO BW-01.
           IF ACTION = "P" GO TO BW-01.
           IF ACTION = "B" GO TO BW-01.
           GO TO RRACT.

       BW-01.
           MOVE "ENNN090BW1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-BW.
           IF VDUSTATUS = "1U" GO TO RRACT.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO BORROWER-FG
              PERFORM BORROWER-SCAN
              IF NOT IO-GOOD
                 GO TO BW-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BW-01.
           MOVE VDUNUM0 TO BEG-BW.

       BW-02.
           MOVE "ENNN090BW2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO BW-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1<"
              MOVE "END" TO BORROWER-FG
              PERFORM BORROWER-SCAN
              IF NOT IO-GOOD
                 GO TO BW-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BW-02.
           MOVE VDUNUM0 TO END-BW.

           IF BEG-BW > END-BW
              MOVE "INVALID BORROWER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BW-01.

           GO TO DT-01.
       ALL-BW.
           MOVE "S  A090BW1." TO VDU.
           MOVE "000000000" TO VDUBUF.
           MOVE 0 TO BEG-BW.
           PERFORM SCREENIO.
           MOVE "S  A090BW2." TO VDU.
           MOVE 999999999 TO END-BW VDUBUF.
           PERFORM SCREENIO.
       DT-01.
           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO BW-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       DT-02.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           IF END-DATE = 0
              GO TO DT-02.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           GO TO PR-PM.

       ALL-DT.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       PR-PM.
           MOVE "E  A010PM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PR-PM.
           MOVE VDUBUF TO PRINT-MEMOS.
           IF PRINT-MEMOS NOT = "Y" AND NOT = "N"
              GO TO PR-PM.
       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO PR-PM.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO RRACT.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

           ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.


       SET-HEAD.
           IF ACTION = "R"
              MOVE "REPORT IN PROGRESS....................." TO MESS.
           IF ACTION = "P"
              MOVE "PURGE IN PROGRESS......................" TO MESS.
           IF ACTION = "B"
              MOVE "PURGE AND REPORT IN PROGRESS..........." TO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-BM1-FILE.
           PERFORM OPEN-BX1-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      **************************************
       CHECK-LOAN-ACTIVITY SECTION.
      **************************************
           PERFORM OPEN-LN4-FILE.

           MOVE LN-PATH-OWNBR    TO LN-OWNBR
                                    QLN4-WBEG-OWNBR
                                    QLN4-WEND-OWNBR.
           MOVE BW-SSNO          TO LN-SSNO(1)
                                    QLN4-WBEG-SSNO1
                                    QLN4-WEND-SSNO1.

           PERFORM START-LN4-FILE.
       NEXT-LN4-CHECK.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                      OR (LN-SSNO(1) NOT = BW-SSNO)
              GO TO CHECK-LN5-ACTIVITY.

           MOVE 9 TO IO-FG.
           GO TO EXIT-CHECK-LOAN-ACTIVITY.
       CHECK-LN5-ACTIVITY.
           PERFORM CLOSE-LN4-FILE.

           PERFORM OPEN-LN5-FILE.

           MOVE LN-PATH-OWNBR    TO LN-OWNBR
                                    QLN5-WBEG-OWNBR
                                    QLN5-WEND-OWNBR.
           MOVE BW-SSNO          TO LN-SSNO(2)
                                    QLN5-WBEG-SSNO2
                                    QLN5-WEND-SSNO2.
           PERFORM START-LN5-FILE.
       NEXT-LN5-CHECK.
           PERFORM READ-LN5-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                      OR (LN-SSNO(2) NOT = BW-SSNO)
      *       GO TO CHECK-LN6-ACTIVITY.
              GO TO CHECK-LH1-ACTIVITY.

           MOVE 9 TO IO-FG.
           GO TO EXIT-CHECK-LOAN-ACTIVITY.

      *CHECK-LN6-ACTIVITY.
      *    PERFORM CLOSE-LN5-FILE.
      *    PERFORM OPEN-LN6-FILE.

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN6-WBEG-OWNBR
      *                             QLN6-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(3)
      *                             QLN6-WBEG-SSNO3
      *                             QLN6-WEND-SSNO3.

      *    PERFORM START-LN6-FILE.
      *NEXT-LN6-CHECK.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(3) NOT = BW-SSNO)
      *       GO TO CHECK-LN7-ACTIVITY.

      *    MOVE 9 TO IO-FG.
      *    GO TO EXIT-CHECK-LOAN-ACTIVITY.
      *CHECK-LN7-ACTIVITY.
      *    PERFORM CLOSE-LN6-FILE.
      *    PERFORM OPEN-LN7-FILE.

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN7-WBEG-OWNBR
      *                             QLN7-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(4)
      *                             QLN7-WBEG-SSNO4
      *                             QLN7-WEND-SSNO4.

      *    PERFORM START-LN7-FILE.
      *NEXT-LN7-CHECK.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(4) NOT = BW-SSNO)
      *       GO TO CHECK-LH1-ACTIVITY.

      *    MOVE 9 TO IO-FG.
      *    GO TO EXIT-CHECK-LOAN-ACTIVITY.

       CHECK-LH1-ACTIVITY.
      *    PERFORM CLOSE-LN7-FILE.
           PERFORM CLOSE-LN5-FILE.

           PERFORM OPEN-LH1-FILE.
           MOVE LH-PATH-OWNBR TO LH-BRNO
                                 QLH1-WBEG-BRNO
                                 QLH1-WEND-BRNO.
           MOVE BW-SSNO       TO LH-SSNO
                                 QLH1-WBEG-SSNO
                                 QLH1-WEND-SSNO.
           MOVE 0             TO LH-ACCTNO
                                 QLH1-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLH1-WEND-ACCTNO.

           PERFORM START-LH1-FILE.
       NEXT-LH1-CHECK.
           PERFORM READ-LH1-FILE-NEXT.
           IF IO-BAD OR (LH-BRNO NOT = LH-PATH-OWNBR)
              MOVE 0 TO IO-FG
              GO TO EXIT-CHECK-LOAN-ACTIVITY.

           IF LH-SSNO NOT = BW-SSNO
              GO TO EXIT-CHECK-LOAN-ACTIVITY.

           MOVE 9 TO IO-FG.
       EXIT-CHECK-LOAN-ACTIVITY.
           PERFORM CLOSE-LH1-FILE.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************
           MOVE BW-SSNO     TO D-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY "-".
           MOVE BW-LNAME    TO LAST-NAME.
           MOVE BW-FNAME    TO FIRST-NAME.
           PERFORM SET-NAME.
           MOVE BW-REFDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE BW-LOANS    TO D-LOANS.
           MOVE BW-RENTCD   TO D-RENTCD.
           MOVE BW-SOLICIT  TO D-SOLICIT.
           MOVE BW-COUNTY   TO D-COUNTY.
           MOVE BW-CENSUSCD TO D-CENSUSCD.
           MOVE BW-EMPLOYER TO D-EMPLOYER.
           MOVE BW-EMPPAY   TO D-EMPPAY.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BW-ADR1     TO D-NAME.
           MOVE BW-BDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE BW-ORGSLSMN TO D-ORGSLSMN.
           MOVE BW-TIMEADDR TO D-TIMEADDR.
           MOVE BW-STRTGCD  TO D-STRTGCD.
           MOVE BW-ORGBR    TO D-BRANCH.
           MOVE BW-EMPYEARS TO D-EMPYEARS.
           MOVE BW-EMPLPOS  TO D-EMPLPOS.
           MOVE BW-EMPPAYOT TO D-EMPPAYOT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BW-ADR2         TO D-NAME.
           MOVE BW-DEPEND       TO D-DEPEND.
           MOVE BW-REPCODE      TO D-REPCODE.
           MOVE BW-CREDITCD     TO D-CREDITCD.
           MOVE BW-DWELLCD      TO D-DWELLCD.
           MOVE BW-CONFIDENTIAL TO D-CONFIDEN.
           IF BW-EMPPHONE = 0
               GO TO PRINT-SRC.
           MOVE BW-EMPPHONE     TO D-EMPPHONE.
           INSPECT D-EMPPHONE REPLACING ALL "/" BY "-".
           IF BW-EMPEXT > 0
               MOVE "EXT."      TO D-EXTTAG
               MOVE BW-EMPEXT   TO D-EMPEXT.
       PRINT-SRC.
           MOVE BW-EMPPAYSR TO D-EMPPAYSR.
           IF BW-ADR2 = " "
               GO TO PRINT-STATE.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-STATE.
           MOVE BW-CITY       TO LAST-NAME.
           MOVE BW-STATE      TO FIRST-NAME.
           PERFORM SET-NAME.
           MOVE BW-ZIP TO ZIP.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY "??".
           INSPECT NAME-WORKER REPLACING FIRST "          "
               BY ZIP.
           INSPECT NAME-WORKER REPLACING FIRST "??" BY "  ".
           MOVE NAME-WORKER   TO D-NAME.
           IF BW-ADR2 NOT = " "
              MOVE BW-ORIGIN  TO D-ORIGIN
              MOVE BW-PRLOANS TO D-PRLOANS
              MOVE BW-WORKHRS TO D-WORKHRS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BW-PHONE      TO D-PHONE.
           INSPECT D-PHONE REPLACING ALL "/" BY "-".
           IF BW-ADR2 = " "
              MOVE BW-ORIGIN  TO D-ORIGIN
              MOVE BW-PRLOANS TO D-PRLOANS
              MOVE BW-WORKHRS TO D-WORKHRS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BW-SPLNAME TO LAST-NAME.
           MOVE BW-SPFNAME TO FIRST-NAME.
           PERFORM SET-NAME.
           IF (BW-LASTPODATE NOT = 0)
              MOVE BW-LASTPODATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-PAYOFF
              MOVE "<= PAYOFF" TO D-REFX.
           IF DETAIL-LINE NOT = " "
              PERFORM WRITE-DETAIL-LINE.

           MOVE BW-SPSSNO TO D-SSNO2.
           INSPECT D-SSNO2 REPLACING ALL "/" BY "-".
           IF BW-INTYTD NOT = 0
              MOVE BW-INTYTD TO D-INT
              MOVE "<= INTYTD" TO D-REFX.
           IF BW-INTLYR NOT = 0
              MOVE BW-INTLYR TO D-INT2
              MOVE "<= INTLYR" TO D-REFX2.
           IF DETAIL-LINE NOT = " "
              PERFORM WRITE-DETAIL-LINE.

           IF PRINT-MEMOS = "N"
              GO TO END-DETAIL-LN-PROCESS.

           MOVE BM-PATH-OWNBR TO BM-BRNO
                                 QBM1-WBEG-BRNO
                                 QBM1-WEND-BRNO.
           MOVE BW-SSNO       TO BM-SSNO
                                 QBM1-WBEG-SSNO
                                 QBM1-WEND-SSNO.
           MOVE 0             TO BM-DATE HOLD-BM-DATE
                                 QBM1-WBEG-DATE-REV
                                 BM-SEQ 
                                 QBM1-WBEG-SEQ.
           MOVE ALL "9"       TO QBM1-WEND-DATE-REV
                                 QBM1-WEND-SEQ.
           PERFORM START-BM1-FILE.

       NEXT-BM1-PRINT.
           PERFORM READ-BM1-FILE-NEXT.
           IF IO-BAD OR (BM-BRNO NOT = BM-PATH-OWNBR)
              GO TO END-DETAIL-LN-PROCESS.

           IF (BM-SSNO NOT = BW-SSNO)
              GO TO END-DETAIL-LN-PROCESS.

           IF BM-DATE NOT = HOLD-BM-DATE
              MOVE BM-DATE TO HOLD-BM-DATE
              SUBTRACT BM-DATE FROM 99999999 GIVING DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-BM-DATE.

           MOVE BM-SEQ TO D-BM-SEQ.
           MOVE BM-MEMO TO D-BM-MEMO.

           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-BM1-PRINT.

       END-DETAIL-LN-PROCESS.

      **************************************
       SET-NAME SECTION.
      **************************************
           MOVE SPACES TO NAME-WORKER.
           MOVE LAST-NAME TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST
                 "                                   " BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           IF NAME-WORKER = ","
              MOVE " " TO NAME-WORKER.
           MOVE NAME-WORKER TO D-NAME.

      **************************************
       PURGE-BORROWER-PROCESS SECTION.
      **************************************

      *    MOVE HOLD-BW-SSNO  TO BW-SSNO. 
      *    PERFORM READ-BW1-FILE.
      *    IF IO-BAD
      *       GO TO EXIT-PURGE-BW-PROCESS.

           PERFORM OPEN-BM1-FILE.

           MOVE BM-PATH-OWNBR TO BM-BRNO
                                 QBM1-WBEG-BRNO
                                 QBM1-WEND-BRNO.
           MOVE BW-SSNO       TO BM-SSNO
                                 QBM1-WBEG-SSNO
                                 QBM1-WEND-SSNO.
           MOVE 0             TO BM-DATE 
                                 QBM1-WBEG-DATE-REV
                                 BM-SEQ 
                                 QBM1-WBEG-SEQ.
           MOVE ALL "9"       TO QBM1-WEND-DATE-REV
                                 QBM1-WEND-SEQ.
           PERFORM START-BM1-FILE.

       NEXT-BM1-PURGE.
           PERFORM READ-BM1-FILE-NEXT.
           IF IO-BAD OR (BM-BRNO NOT = BW-OWNBR)
                      OR (BM-SSNO NOT = BW-SSNO)
              GO TO DELETE-BW-RECORD.

           PERFORM DELETE-BM1-FILE.

           GO TO NEXT-BM1-PURGE.

       DELETE-BW-RECORD.

      * CHECK FOR BX BORROWER TRAILER
           MOVE HOLD-BW-SSNO  TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-BX1-FILE.

           PERFORM DELETE-BW1-FILE.

       EXIT-PURGE-BW-PROCESS.
           EXIT.

      *********************************
       WRITE-DETAIL-LINE SECTION.
      *********************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *********************************
       HEAD-PAGE SECTION.
      *********************************
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

           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD5 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE HEAD6 TO PR-REC.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 8 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *********************************
       PRINT-RANGE-LINES SECTION.
      *********************************
           MOVE 4 TO LN-FEED.
           MOVE "REPORT ACTION" TO RANGE-LINE.
           MOVE ACTION TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEG BORROWER" TO RANGE-LINE.
           MOVE BEG-BW TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END BORROWER" TO RANGE-LINE.
           MOVE END-BW TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEG LAST P/O DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LAST P/O DATE" TO RANGE-LINE.
           MOVE END-DATE TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "PRINT MEMOS ?" TO RANGE-LINE.
           MOVE PRINT-MEMOS TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ***************************
       BORROWER-SCAN SECTION.
      ***************************
           MOVE 0   TO BWSCAN-NO.
           MOVE "S" TO BWSCAN-SCAN-TYPE.
           MOVE "WI/BWSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWSCAN-STATUS NOT = "00" OR BWSCAN-NO = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-BORROWER-SCAN.

           IF ( BORROWER-FG = "BEG" )
              MOVE "SNNN090BW1." TO VDU
           ELSE
              MOVE "SNNN090BW2." TO VDU.
           MOVE BWSCAN-NO TO BW-SSNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE BW-SSNO TO VDUNUM0.

           MOVE 0 TO IO-FG.

       EXIT-BORROWER-SCAN.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO BWPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWPURG-SYSDATE.
           DISPLAY BWPURG-SCREEN UPON BWPURG-WINDOW-HANDLE.
           MOVE BWPURG-FORM-FIELDS TO WR-BUF.
           MOVE BWPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE BWPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWPURG-HELP--DIR  TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BWPURG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      ******************************************
       IO-ROUTINE SECTION.
      ******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPLH1RN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************
       END-PROGRAM SECTION.
      ******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/EMPUMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWPURG-SCREEN.
           DESTROY BWPURG-WINDOW-HANDLE.
           EXIT PROGRAM.
