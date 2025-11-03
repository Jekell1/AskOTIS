       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LHPURG.
       DATE-WRITTEN.    081884.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  LOAN HISTORY FILE PURGE
      *
      * CURRENT BRANCH ONLY, DOES NOT CROSS DATA PATHS
      *
      *=================================================================
      * REV:
      * JFF 101295 ADDED CLASS SCAN AND BORROWER SCAN
      * BAH 110914 REPLACED RRFORM WITH SCREEN
      *
      * KEC 2017.0424 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED BEG-AC & END-AC TO BE PIC 9(8) BUT THE
      *          SCREEN STILL NEEDS TO BE CHANGED.
      * BAH 170605 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181127 GLOBAL LHFILE
      * BAH 181210 GLOBAL LNFILE
      * BAH 20230713 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LHPURG.CBL J35 12/21/23-08:52:00 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       01  PR-IFN               PIC X(4)    VALUE "0APR".

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

       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).
       01  HOLD-SSNO            PIC 9(10) COMP-3 VALUE 0.
       01  PURGE-COUNT          PIC 9(6)       VALUE 0.
       01  CLASS-FG             PIC X(3)    VALUE "   ".
           88  CLASS-BEG                    VALUE "BEG".
           88  CLASS-END                    VALUE "END".
       01  BORROWER-FG          PIC X(4)    VALUE "    ".
           88  BORROWER-BEG                 VALUE "BBEG".
           88  BORROWER-END                 VALUE "BEND".

       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/BWSCANW.CPY".
       COPY "LIBLP/LHSTATW.CPY".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ACTION          PIC X.
           03  BEG-BW          PIC 9(9).
           03  END-BW          PIC 9(9).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  BEG-CL          PIC 9(3).
           03  END-CL          PIC 9(3).
           03  BEG-AC          PIC 9(8).
           03  END-AC          PIC 9(8).
           03  PURGE-ACTIVE    PIC X.

       01  ERRCD               PIC X.
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
           "- - - LOAN HISTORY PURGE REPORT - - -   ".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD3.
           03  FILLER      PIC X(101)   VALUE SPACES.
           03  FILLER      PIC X(31)    VALUE
           "--- PAYOFF ---   CONTRACTUAL MK".

       01  HEAD4.
           03  FILLER      PIC X(53)    VALUE
           "SOC-SEC-NO  BORROWER                ACCOUNT NO.".
           03  FILLER      PIC X(50)    VALUE
           "LOANDATE TRM CC  DEALER   LOANAMT PLDATE JDDATE CD".
           03  FILLER      PIC X(29)    VALUE
           "  TRM   DATE   PROFILE     CD".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-SSNO           PIC 999/99/9999.
           03  FILLER           PIC X.
           03  D-BWNAME         PIC X(23).
           03  FILLER           PIC X(1).
           03  D-LOAN-NUM       PIC X(16).
           03  FILLER           PIC X.
           03  D-LOANDATE       PIC 99/99/99 BLANK ZERO.
           03  FILLER           PIC X.
           03  D-TERM           PIC ZZ9.
           03  FILLER           PIC X.
           03  D-COLLCD         PIC XX.
           03  FILLER           PIC X.
           03  D-DLNO           PIC Z(4).99 BLANK ZERO.
           03  FILLER           PIC X.
           03  D-LNAMT          PIC ZZZZZ9.99 BLANK ZERO.
           03  FILLER           PIC X.
           03  D-PLDATE         PIC 9(6) BLANK ZERO.
           03  FILLER           PIC X.
           03  D-JDDATE         PIC 9(6) BLANK ZERO.
           03  FILLER           PIC X.
           03  D-POCD           PIC XXX.
           03  FILLER           PIC X.
           03  D-POTERMS        PIC ZZ9.
           03  FILLER           PIC X.
           03  D-PAIDOFF        PIC 99/99/99 BLANK ZERO.
           03  FILLER           PIC X.
           03  D-PYPROFILE      PIC X(12).
           03  FILLER           PIC X.
           03  D-MAKERCD        PIC X.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(50).
           03  RANGE-SELECTION  PIC X(72).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  LHPURG-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LHPURG_DEF.CPY".
       COPY "LIBLP/LHPURG_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/LHPURG_SCN.CPY".

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
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LHPURG" TO FORM-PROG VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LHPURG-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-LH.
           PERFORM READ-LH1-FILE-NEXT.
           IF (IO-FG = 9) OR (LH-BRNO NOT = LH-PATH-OWNBR)
              GO TO END-ROUTINE.

           IF LH-SSNO > END-BW
              GO TO END-ROUTINE.

           IF LH-SSNO < BEG-BW
              GO TO NEXT-LH.

           IF LH-CLASS < BEG-CL OR LH-CLASS > END-CL
              GO TO NEXT-LH.
           IF LH-ACCTNO < BEG-AC OR LH-ACCTNO > END-AC
              GO TO NEXT-LH.

           IF LH-POFFDATE < BEG-DATE OR LH-POFFDATE > END-DATE
               GO TO NEXT-LH.

           IF PURGE-ACTIVE = "N"
              PERFORM CHECK-LOAN-ACTIVITY
              IF IO-FG = 9
                 GO TO NEXT-LH.

           IF ACTION NOT = "P"
              PERFORM DETAIL-LINE-PROCESS.

           IF ACTION NOT = "R"
              PERFORM DELETE-LH1-FILE.

           MOVE "N" TO FIRST-TIME.

           GO TO NEXT-LH.
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
       SETRP.
           MOVE LHPURG-QUEUE-POS TO Q-POS.
           MOVE LHPURG-COPIES-POS TO C-POS.
           MOVE LHPURG-PRU-POS TO P-POS.
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
              MOVE "BBEG" TO BORROWER-FG
              PERFORM BORROWER-SCAN
              IF IO-FG NOT = 0
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
              MOVE "BEND" TO BORROWER-FG
              PERFORM BORROWER-SCAN
              IF IO-FG NOT = 0
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

           GO TO CL-01.
       ALL-DT.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO CL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CL.

       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CL.

           GO TO AC-01.
       ALL-CL.
           MOVE "S  A020CL1." TO VDU.
           MOVE 0 TO BEG-CL VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CL2." TO VDU.
           MOVE 99 TO END-CL VDUBUF.
           PERFORM SCREENIO.
       AC-01.
           MOVE "ENNN060AC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-AC.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AC-01.
           MOVE VDUNUM0 TO BEG-AC.
       AC-02.
           MOVE "ENNN060AC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO AC-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AC-02.
           MOVE VDUNUM0 TO END-AC.
           IF BEG-AC > END-AC
              GO TO AC-01.
           GO TO PR-PA.
       ALL-AC.
           MOVE "S  A060AC1." TO VDU.
           MOVE 0 TO BEG-AC VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A060AC2." TO VDU.
           MOVE 999999 TO END-AC VDUBUF.
           PERFORM SCREENIO.
       PR-PA.
           MOVE "E  A010PA." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO AC-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PR-PA.
           MOVE VDUBUF TO PURGE-ACTIVE.
           IF PURGE-ACTIVE = "Y" GO TO OK-YN.
           IF PURGE-ACTIVE NOT = "N" GO TO PR-PA.
       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO PR-PA.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO RRACT.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

           ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.

           MOVE " " TO ERRCD.

           IF ERRCD = "1" GO TO RRACT.
           IF ERRCD = "E" GO TO ENDIT.

       SET-HEAD.
           IF ACTION = "R"
              MOVE "REPORT IN PROGRESS.................." TO WR-BUF.
           IF ACTION = "P"
              MOVE "PURGE IN PROGRESS..................." TO WR-BUF.
           IF ACTION = "B"
              MOVE "PURGE AND REPORT IN PROGRESS........" TO WR-BUF.

           MOVE MESS-LIST TO WR-LIST
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN4-FILE.
           PERFORM OPEN-LH1-FILE.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-BR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE LH-PATH-OWNBR TO LH-BRNO
                                 QLH1-WBEG-BRNO
                                 QLH1-WEND-BRNO.
           MOVE BEG-BW        TO LH-SSNO
                                 QLH1-WBEG-SSNO.
           MOVE END-BW        TO QLH1-WEND-SSNO.
           MOVE 0             TO LH-ACCTNO
                                 QLH1-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLH1-WEND-ACCTNO.

           PERFORM START-LH1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       CHECK-LOAN-ACTIVITY SECTION.
      ****************************
       CHECK-LN4-ACTIVITY.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR.
           MOVE LH-SSNO       TO LN-SSNO(1) 
                                 QLN4-WBEG-SSNO1
                                 QLN4-WEND-SSNO1.

           PERFORM START-LN4-FILE.
       NEXT-LN4-CHECK.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO CHECK-LN5-ACTIVITY.

           IF LN-SSNO(1) NOT = LH-SSNO
              GO TO CHECK-LN5-ACTIVITY.

           PERFORM CHECK-BALS.
           IF IO-FG = 9
              GO TO EXIT-CHECK-LOAN-ACTIVITY.

           GO TO NEXT-LN4-CHECK.
       CHECK-LN5-ACTIVITY.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN5-WBEG-OWNBR
                                 QLN5-WEND-OWNBR.
           MOVE LH-SSNO       TO LN-SSNO(2) 
                                 QLN5-WBEG-SSNO2
                                 QLN5-WEND-SSNO2.

           PERFORM START-LN5-FILE.
       NEXT-LN5-CHECK.
           PERFORM READ-LN5-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *       GO TO CHECK-LN6-ACTIVITY.
              MOVE 0 TO IO-FG
              GO TO EXIT-CHECK-LOAN-ACTIVITY.

           IF LN-SSNO(2) NOT = LH-SSNO
              GO TO EXIT-CHECK-LOAN-ACTIVITY.
      *       GO TO CHECK-LN6-ACTIVITY.

           PERFORM CHECK-BALS.
           IF IO-FG = 9
              GO TO EXIT-CHECK-LOAN-ACTIVITY.

           GO TO NEXT-LN5-CHECK.
      *CHECK-LN6-ACTIVITY.
      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN6-WBEG-OWNBR
      *                          QLN6-WEND-OWNBR.
      *    MOVE LH-SSNO       TO LN-SSNO(3) 
      *                          QLN6-WBEG-SSNO3
      *                          QLN6-WEND-SSNO3.

      *    PERFORM START-LN6-FILE.
      *NEXT-LN6-CHECK.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *       GO TO CHECK-LN7-ACTIVITY.

      *    IF LN-SSNO(3) NOT = LH-SSNO
      *       GO TO CHECK-LN7-ACTIVITY.

      *    PERFORM CHECK-BALS.
      *    IF IO-FG = 9
      *       GO TO EXIT-CHECK-LOAN-ACTIVITY.

      *    GO TO NEXT-LN6-CHECK.
      *CHECK-LN7-ACTIVITY.
      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN7-WBEG-OWNBR
      *                          QLN7-WEND-OWNBR.
      *    MOVE LH-SSNO       TO LN-SSNO(4) 
      *                          QLN7-WBEG-SSNO4
      *                          QLN7-WEND-SSNO4.

      *    PERFORM START-LN7-FILE.
      *NEXT-LN7-CHECK.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *       MOVE 0 TO IO-FG
      *       GO TO EXIT-CHECK-LOAN-ACTIVITY.

      *    IF LN-SSNO(4) NOT = LH-SSNO
      *       GO TO EXIT-CHECK-LOAN-ACTIVITY.

      *    PERFORM CHECK-BALS.
      *    IF IO-FG = 9
      *       GO TO EXIT-CHECK-LOAN-ACTIVITY.

      *    GO TO NEXT-LN7-CHECK.

       EXIT-CHECK-LOAN-ACTIVITY.
           EXIT.

      ****************************
       CHECK-BALS SECTION.
      ****************************
           MOVE 9 TO IO-FG.
           IF LN-CURBAL NOT = 0
              GO TO EXIT-CHECK-BALS.
           IF LN-INTBAL NOT = 0
              GO TO EXIT-CHECK-BALS.
           IF LN-LCBAL NOT = 0
              GO TO EXIT-CHECK-BALS.
           IF LN-OTHBAL NOT = 0
              GO TO EXIT-CHECK-BALS.
           MOVE 0 TO IO-FG.
       EXIT-CHECK-BALS.
           EXIT.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************
       DETAIL-LN-PROCESS.
           MOVE 1 TO LN-FEED.
           IF LH-SSNO = HOLD-SSNO
              GO TO MOVE-ACCOUNT.
           MOVE LH-SSNO TO D-SSNO BW-SSNO HOLD-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY "-".
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE "BORROWER" TO BW-LNAME
              MOVE "?" TO BW-FNAME.
           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", +".
           INSPECT NAME-WORKER REPLACING FIRST "+               "
              BY FIRST-NAME.
           MOVE NAME-WORKER TO D-BWNAME.
           MOVE 2 TO LN-FEED.
       MOVE-ACCOUNT.
           PERFORM LHSTAT-DISPLAY.
           MOVE LHS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LH-MAKERCD TO D-MAKERCD.
           MOVE LH-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.
           MOVE LH-ORGTERM TO D-TERM.
           MOVE LH-COLLCD TO D-COLLCD.
           MOVE LH-DLNO TO D-DLNO.
           MOVE LH-LNAMT TO D-LNAMT.
           MOVE LH-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PAIDOFF.
           MOVE LH-POCD TO D-POCD.
           MOVE LH-POTERMS TO D-POTERMS.
           MOVE LH-JDDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-JDDATE.
           MOVE LH-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PLDATE.
           MOVE LH-PYPROFILE TO D-PYPROFILE.
           INSPECT D-PYPROFILE REPLACING ALL "9" BY "-".
           PERFORM WRITE-DETAIL-LINE.
           ADD 1 TO PURGE-COUNT.


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

      *******************************
       HEAD-PAGE SECTION.
      *******************************
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

           MOVE 4 TO LN-FEED.
           MOVE 2 TO LN-COUNT.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
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
           MOVE BEG-DATE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LAST P/O DATE" TO RANGE-LINE.
           MOVE END-DATE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-CL TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-CL TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEG LOAN ACCOUNT" TO RANGE-LINE.
           MOVE BEG-AC TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LOAN ACCOUNT" TO RANGE-LINE.
           MOVE END-AC TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "PURGE ACTIVE ?" TO RANGE-LINE.
           MOVE PURGE-ACTIVE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ITEMS REPORTED" TO RANGE-LINE.
           MOVE PURGE-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      **********************************
       CLASS-SCAN SECTION.
      **********************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
               MOVE "SN N020CL1." TO VDU
           ELSE
               MOVE "SN N020CL2." TO VDU.

           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.

           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      *******************************************
       BORROWER-SCAN SECTION.
      *******************************************
           MOVE 0   TO BWSCAN-NO.
           MOVE "S" TO BWSCAN-SCAN-TYPE.
           MOVE "WI/BWSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWSCAN-STATUS NOT = "00" OR BWSCAN-NO = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-BORROWER-SCAN.
           IF ( BORROWER-FG = "BBEG" )
               MOVE "SNNN090BW1." TO VDU
           ELSE
               MOVE "SNNN090BW2." TO VDU.
           MOVE BWSCAN-NO TO BW-SSNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE BW-SSNO TO VDUNUM0.

           MOVE 0 TO IO-FG.

       EXIT-BORROWER-SCAN.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LHSTAT.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO LHPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LHPURG-SYSDATE.
           DISPLAY LHPURG-SCREEN UPON LHPURG-WINDOW-HANDLE.
           MOVE LHPURG-FORM-FIELDS TO WR-BUF.
           MOVE LHPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE LHPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE LHPURG-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LHPURG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************
       MISC-ROUTINES SECTION.
      *******************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      *******************************
       IO-ROUTINE SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLH1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************
       END-PROGRAM SECTION.
      *******************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/EMPUMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LHPURG-SCREEN.
           DESTROY LHPURG-WINDOW-HANDLE.
           EXIT PROGRAM.
