       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWCRE3.
       DATE-WRITTEN.
      **************************************************************************
      *   BORROWER CREATE   F5-CREATE BORROWER FROM LOAN INPUT
      *                          PAGE 2 (LIABILITIES)
      *                        (WINDOW)
      * REV:
      *  JTG 082197 CORRECT F1-CLR (RESET TO ORIGINAL RECORD) LOGIC
      *  JTG 082197 ADDED BW-LAST-FORM-NO BW-LAST-FORM-DATE
      *                   BW-PRIOR-RN-FOR10-DATE BW-LAST-RN-FOR10-DATE
      *  MJD 112497 EXPANDED BW-REC TO 480 BYTES
      *  MJD 120497 EXPANDED BW-LOANS TO PIC 99
      *  MJD 021898 EXPANDED BW-REC TO 546 BYTES
      *  JTG 093098 ALLOWED BW-LIASMLN & BW-LIAOTH TO GO NEGATIVE; REGENCY REVOL
      *  BAH 030402 ADDED CREDIT SCORES AND BNKRPT SCORE, LENDMARK PR# 2044
      *             INTO A NEW BORROWER TRAILER (BXFILE)
      *  BAH 030626 CREATE A CHANGES REC AND BORROWER MEMO WHEN FIELDS ARE
      *             CHANGED, LENDMARK
      *  BAH 120830 ADDED BW-DEALTRACK-CHG-DATE, TRACKS CHANGES MADE TO LNAME,
      *             FNAME,ADR1,ADR2,CITY,ST,ZIP,PHONE,EMPPHONE REGACC PR# 319
      *  BAH 130123 A30 COMPLETED
      *  BAH 160217 SET BM-BRNO PD0003
      *  BAH 160520 REMOVED BXFILE
      *  BAH 170407 ACTIVATE 8 DIGIT DATES, PD0006
      *  AMM 220105 ADDED BW-OPT-IN-EMAIL/TEXT FIELDS
      *  BAH 2412111 ADDED BW-UMC-EXPIRE-DATE AND BW-UMC-PR-EXPIRE-DATE FOR
      *              DISPLAY ONLY #1687 S35Q-146
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/BWCRE3.CBL S35 01/05/22-14:35:59 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       01  ERRCD               PIC X     VALUE SPACES.

       01  SAVE-BM1-KEY.
           03  SAVE-BM-BRNO    PIC 9(4).
           03  SAVE-BM-SSNO    PIC 9(9).
           03  SAVE-BM-DATE    PIC 9(8).
           03  SAVE-BM-SEQ     PIC 9(3).

       01  MEMO-AREA.
           03  FILLER          PIC X(5)  VALUE "OLD: ".
           03  MEMO-DESCRIPT   PIC X(20).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-ORG-CHAR   PIC X(24).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-USERID     PIC X(10).
       01  LEGEND          PIC X(35).
       01  LEGEND-01.
           03  FILLER      PIC X(21)  VALUE
           "F1, F7-EXIT, ENTER #:".
       01  LEGEND-02.
           03  FILLER      PIC X(8)  VALUE
           " F7-EXIT".

       01  BWCRE3-LINES          PIC 99  VALUE 22.
       01  BWCRE3-COLUMNS        PIC 99  VALUE 78.
       01  BWCRE3-BEGIN-Y        PIC 99  VALUE 2.
       01  BWCRE3-BEGIN-X        PIC 99  VALUE 2.

       01  WS-DEALTRACK-CHG-DATE       PIC 9(8).
       01  SUB             PIC 999    COMP  VALUE 0.
       01  ONE             PIC S9     COMP.
       01  ELE             PIC 9(2)   COMP.
       01  NEW-FLAG        PIC 9.
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

       01  IO-TYPE         PIC X(3).

      *************************************
      *   SCREEN DISPLAY BUFFER
      *************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC Z9.
           03  F02         PIC Z9.
           03  F03         PIC Z,ZZZ,ZZZ.99-.
           03  F04         PIC Z,ZZZ,ZZZ.99-.
           03  F05         PIC ZZ,ZZZ.99-.
           03  F06         PIC ZZ,ZZZ.99-.
           03  F07         PIC Z,ZZZ,ZZZ.99-.
           03  F08         PIC Z,ZZZ,ZZZ.99-.
           03  F09         PIC 99/99/99.
           03  F10         PIC 99/99/99.
           03  F11         PIC 99/99/99.
           03  F12         PIC ZZ9.
           03  F13         PIC 99/99/99.
           03  F-CRSCORE   PIC 9(4) OCCURS 4.
           03  F-BNKSCORE  PIC 9(4) OCCURS 4.
           03  F22         PIC 99/99/99.
           03  F23         PIC 99/99/99.
           03  F24         PIC 99/99/99.
           03  F25         PIC X.
           03  F26         PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(44) VALUE
           "010 020 030 040 050 060 070 080 090 100 110 ".
           03  FILLER      PIC X(44) VALUE
           "120 130 140 150 160 170 180 190 200 210 220 ".
           03  FILLER      PIC X(16) VALUE
           "230 240 250 260.".

       01  MAX             PIC 99   VALUE 26.

       01  SPEC-TABLE.
      *01. NUMBER OF PRIOR LOANS
           03  FILLER      PIC X(8) VALUE "ENNN020N".
      *02. NUMBER OF CURRENT LOANS
           03  FILLER      PIC X(8) VALUE "ENNN020N".
      *03. Y-T-D INTEREST
           03  FILLER      PIC X(8) VALUE "E NS072N".
      *04. L-Y-R INTEREST
           03  FILLER      PIC X(8) VALUE "E NS072N".
      *05. REAL ESTATE INTEREST YTD
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *06. REAL ESTATE INTEREST LYR
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *07. SMALL LOAN LIABILITY
           03  FILLER      PIC X(8) VALUE "E NS072N".
      *08. OTHER LOAN LIABILITY
           03  FILLER      PIC X(8) VALUE "E NS072N".
      *09. LAST PAYOFF DATE
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *10. PRIOR RENEWAL FOR 10%
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *11. LAST RENEWAL FOR 10%
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *12. LAST FORM NO. SENT
           03  FILLER      PIC X(8) VALUE "ENNN030N".
      *13. LAST FORM SENT DATE
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *14. CREDIT SCORE Q1
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *15. CREDIT SCORE Q2
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *16. CREDIT SCORE Q3
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *17. CREDIT SCORE Q4
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *18. BNKRPT SCORE Q1
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *19. BNKRPT SCORE Q2
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *20. BNKRPT SCORE Q3
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *21. BNKRPT SCORE Q4
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *22. DEALER TRACK CHANGE DATE
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *23. UMC EXPIRATION
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *24. UMC PRIOR EXPIRATION
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *25. EMAIL OPT IN
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *26. TEXT OPT IN
           03  FILLER      PIC X(8) VALUE "E  A010 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 26 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  LEGEND-WALK.
           03  FILLER                   PIC X(10)  VALUE "F1, ^-UP, ".
           03  FILLER                   PIC X      VALUE X'76'.
           03  FILLER                   PIC X(24)  VALUE
           "-DOWN, ENTER FIELD INFO:".


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BWCRE3_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/BWCRE3_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  BWCRE3-WINDOW-HANDLE       PIC X(10).
       01  CHAN                       PIC X(4).

       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "ORIG-BW".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LP01BW.CPY".
       01  ALLOW-CHANGES       PIC X.

       SCREEN SECTION.
       COPY "LIBWI/BWCRE3_SCN.CPY".

       PROCEDURE DIVISION USING FORM-PATHNAME
                  BW-REC
                  ALLOW-CHANGES 
                  EXIT-PATHNAME. 

      ****************************
      *      MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "BWCRE3" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT  SCREEN LINE BWCRE3-BEGIN-Y * VDU-WIN-L-OFFSET
               AT  SCREEN COL  BWCRE3-BEGIN-X * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE    BWCRE3-COLUMNS + VDU-WIN-W-MOD
               LINES   BWCRE3-LINES + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS BWCRE3-WINDOW-HANDLE.

           MOVE " " TO VDU-WIMENU-SELECTION.

      * STORE ORIGINAL BW-REC FROM PAGE 1:
           MOVE BW-REC TO ORIG-BW-REC.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE " " TO BR-CHANGE-MEMO.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

      * RESTORE ORIGINAL BW-REC FROM PAGE 1:
           IF IO-TYPE = "CLR"
              MOVE ORIG-BW-REC TO BW-REC.

           GO TO MAIN-MODULE.


      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE " " TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
      * ALLOW MODIFY OF BRANCH
           IF ALLOW-CHANGES = "Y"
              MOVE LEGEND-01 TO LEGEND
           ELSE
              MOVE LEGEND-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              PERFORM CREATE-CHANGE-REC
              MOVE "END" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

      * NO CHANGES TO UMC FIELDS
           IF ELE = 23 OR 24
              GO TO MODIFY-ROUTINE.

           IF ALLOW-CHANGES = "N"
              PERFORM ALARM
              MOVE "PLEASE NOTE..." TO ERRMSG-HEAD
              MOVE "CHANGES ARE NOT ALLOWED FROM THIS SCREEN!!"
                                            TO ERRMSG-MSG1
              MOVE SPACES TO ERRMSG-MSG2 ERRMSG-MSG3
              PERFORM CALL-ERRMSG
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = CHAN
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 1
              MOVE LEGEND-WALK TO LEGEND
           ELSE
              MOVE "F1-CANCEL, ENTER FIELD INFO:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25
                 F-26
                 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUNUM0 TO BW-PRLOANS.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM0 TO BW-LOANS.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM2 TO BW-INTYTD.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO BW-INTLYR.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO BW-RE-INTYTD.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM2 TO BW-RE-INTLYR.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM2 TO BW-LIASMLN.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO BW-LIAOTH.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDU-DATE-YYYYMMDD TO BW-LASTPODATE.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDU-DATE-YYYYMMDD TO BW-PRIOR-RN-FOR10-DATE.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDU-DATE-YYYYMMDD TO BW-LAST-RN-FOR10-DATE.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM0 TO BW-LAST-FORM-NO.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDU-DATE-YYYYMMDD TO BW-LAST-FORM-DATE.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUNUM0 TO BW-CRED-SCORE(1).
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM0 TO BW-CRED-SCORE(2).
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUNUM0 TO BW-CRED-SCORE(3).
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUNUM0 TO BW-CRED-SCORE(4).
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUNUM0 TO BW-BNKRPT-SCORE(1).
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM0 TO BW-BNKRPT-SCORE(2).
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUNUM0 TO BW-BNKRPT-SCORE(3).
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUNUM0 TO BW-BNKRPT-SCORE(4).
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDU-DATE-YYYYMMDD TO BW-DEALTRACK-CHG-DATE.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDU-DATE-YYYYMMDD TO BW-UMC-EXPIRE-DATE.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDU-DATE-YYYYMMDD TO BW-UMC-PR-EXPIRE-DATE.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUBUF TO BW-OPT-IN-EMAIL.
           IF NOT (BW-OPT-IN-EMAIL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE VDUBUF TO BW-OPT-IN-TEXT.
           IF NOT (BW-OPT-IN-TEXT = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       EXIT-ENTRY-MODULE.
           EXIT.

      *****************************
       CREATE-CHANGE-REC SECTION.
      *****************************
           PERFORM GETDAT.

           PERFORM OPEN-CH-FILE.

           PERFORM OPEN-BM1-FILE.

           IF ORIG-BW-CRED-SCORE(1) NOT = BW-CRED-SCORE(1)
              PERFORM CREATE-SETUP-REC
              MOVE "BW CRSCORE(1)"       TO CH-DESCRIPT
              MOVE ORIG-BW-CRED-SCORE(1) TO CH-ORG-NUM
              MOVE BW-CRED-SCORE(1)      TO CH-NEW-NUM
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BW-CRED-SCORE(2) NOT = BW-CRED-SCORE(2)
              PERFORM CREATE-SETUP-REC
              MOVE "BW CRSCORE(2)"       TO CH-DESCRIPT
              MOVE ORIG-BW-CRED-SCORE(2) TO CH-ORG-NUM
              MOVE BW-CRED-SCORE(2)       TO CH-NEW-NUM
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BW-CRED-SCORE(3) NOT = BW-CRED-SCORE(3)
              PERFORM CREATE-SETUP-REC
              MOVE "BW CRSCORE(3)"       TO CH-DESCRIPT
              MOVE ORIG-BW-CRED-SCORE(3) TO CH-ORG-NUM
              MOVE BW-CRED-SCORE(3)      TO CH-NEW-NUM
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BW-CRED-SCORE(4) NOT = BW-CRED-SCORE(4)
              PERFORM CREATE-SETUP-REC
              MOVE "BW CRSCORE(4)"       TO CH-DESCRIPT
              MOVE ORIG-BW-CRED-SCORE(4) TO CH-ORG-NUM
              MOVE BW-CRED-SCORE(4)      TO CH-NEW-NUM
              PERFORM UPDATE-CH-BM-FILE.

           IF ORIG-BW-BNKRPT-SCORE(1) NOT = BW-BNKRPT-SCORE(1)
              PERFORM CREATE-SETUP-REC
              MOVE "BW BNKRPT(1)"          TO CH-DESCRIPT
              MOVE ORIG-BW-BNKRPT-SCORE(1) TO CH-ORG-NUM
              MOVE BW-BNKRPT-SCORE(1)      TO CH-NEW-NUM
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BW-BNKRPT-SCORE(2) NOT = BW-BNKRPT-SCORE(2)
              PERFORM CREATE-SETUP-REC
              MOVE "BW BNKRPT(2)"          TO CH-DESCRIPT
              MOVE ORIG-BW-BNKRPT-SCORE(2) TO CH-ORG-NUM
              MOVE BW-BNKRPT-SCORE(2)      TO CH-NEW-NUM
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BW-BNKRPT-SCORE(3) NOT = BW-BNKRPT-SCORE(3)
              PERFORM CREATE-SETUP-REC
              MOVE "BW BNKRPT(3)"          TO CH-DESCRIPT
              MOVE ORIG-BW-BNKRPT-SCORE(3) TO CH-ORG-NUM
              MOVE BW-BNKRPT-SCORE(3)      TO CH-NEW-NUM
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BW-BNKRPT-SCORE(4) NOT = BW-BNKRPT-SCORE(4)
              PERFORM CREATE-SETUP-REC
              MOVE "BW BNKRPT(4)"          TO CH-DESCRIPT
              MOVE ORIG-BW-BNKRPT-SCORE(4) TO CH-ORG-NUM
              MOVE BW-BNKRPT-SCORE(4)      TO CH-NEW-NUM
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BW-OPT-IN-EMAIL NOT = BW-OPT-IN-EMAIL
              PERFORM CREATE-SETUP-REC
              MOVE "BW OPT-IN EMAIL"       TO CH-DESCRIPT
              MOVE ORIG-BW-OPT-IN-EMAIL    TO CH-ORG-CHAR
              MOVE BW-OPT-IN-EMAIL         TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BW-OPT-IN-TEXT NOT = BW-OPT-IN-TEXT
              PERFORM CREATE-SETUP-REC
              MOVE "BW OPT-IN TEXT"       TO CH-DESCRIPT
              MOVE ORIG-BW-OPT-IN-TEXT    TO CH-ORG-CHAR
              MOVE BW-OPT-IN-TEXT         TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.

           PERFORM CLOSE-CH-FILE.

           GO TO EXIT-CREATE-CHANGE-REC.

       CREATE-SETUP-REC.

           PERFORM CLEAR-CH-FILE.

           MOVE BW-OWNBR           TO CH-BRNO.
           MOVE SYS-DATE           TO CH-DATE.
           MOVE "M"                TO CH-CODE.
           MOVE EXT-CONAME-USER-ID TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS           TO CH-TIME.
           MOVE 1                  TO CH-SEQ.

           MOVE BW-SSNO            TO CH-SSNO.
       EXIT-CREATE-CHANGE-REC.
           EXIT.

      ****************************************
       UPDATE-CH-BM-FILE SECTION.
      ****************************************
       UPDATE-CH-AGAIN.
           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF CH-SEQ < 999
                 ADD 1 TO CH-SEQ
                 GO TO UPDATE-CH-AGAIN.

           IF BR-CHANGE-MEMO NOT = "Y"
              GO TO UPDATE-CH-BM-FILE-EXIT.

           MOVE BM-PATH-OWNBR TO SAVE-BM-BRNO.
           MOVE CH-SSNO       TO SAVE-BM-SSNO.
           COMPUTE SAVE-BM-DATE = 99999999 - SYS-DATE.
           MOVE 1             TO SAVE-BM-SEQ.
       UPDATE-CH-BM-FILE-AGAIN.
           MOVE SPACES TO BM-REC.

           MOVE SAVE-BM1-KEY TO BM1-KEY.
           MOVE CH-USERID TO MEMO-USERID.
           MOVE CH-DESCRIPT TO MEMO-DESCRIPT.
           MOVE CH-ORG-CHAR TO MEMO-ORG-CHAR.
           MOVE MEMO-AREA TO BM-MEMO.

           MOVE "9" TO BM-MEMO-TYPE.
           PERFORM WRITE-BM1-FILE.
           IF IO-BAD
              IF SAVE-BM-SEQ < 999
                 ADD 1 TO SAVE-BM-SEQ
                 GO TO UPDATE-CH-BM-FILE-AGAIN.

       UPDATE-CH-BM-FILE-EXIT.
           EXIT.

      ***************************************
       DISPLAY-FIELDS SECTION.
      ***************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE BW-PRLOANS TO F01.
           MOVE BW-LOANS TO F02.
           MOVE BW-INTYTD TO F03.
           MOVE BW-INTLYR TO F04.
           MOVE BW-RE-INTYTD TO F05.
           MOVE BW-RE-INTLYR TO F06.
           MOVE BW-LIASMLN TO F07.
           MOVE BW-LIAOTH TO F08.
           IF BW-LASTPODATE NOT = 0
              MOVE BW-LASTPODATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO F09.

           IF BW-PRIOR-RN-FOR10-DATE NOT = 0
              MOVE BW-PRIOR-RN-FOR10-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO F10.
           IF BW-LAST-RN-FOR10-DATE NOT = 0
              MOVE BW-LAST-RN-FOR10-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO F11.

           MOVE BW-LAST-FORM-NO TO F12.
           IF BW-LAST-FORM-DATE NOT = 0
              MOVE BW-LAST-FORM-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO F13.

           MOVE BW-DEALTRACK-CHG-DATE TO WS-DEALTRACK-CHG-DATE.
           IF WS-DEALTRACK-CHG-DATE NOT = 0 AND NOT = 02020202 AND
                                                 NOT = 20202020
              MOVE BW-DEALTRACK-CHG-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO F22.

           MOVE BW-UMC-EXPIRE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F23.
           MOVE BW-UMC-PR-EXPIRE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F24.
            
           MOVE BW-OPT-IN-EMAIL TO F25.
           MOVE BW-OPT-IN-TEXT  TO F26.

           MOVE 1 TO SUB.
       DISPLAY-NEXT-SCORE.
           MOVE BW-CRED-SCORE(SUB) TO F-CRSCORE(SUB).
           MOVE BW-BNKRPT-SCORE(SUB) TO F-BNKSCORE(SUB).
           IF SUB < 4
              ADD 1 TO SUB
              GO TO DISPLAY-NEXT-SCORE.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ******************************************************
       FORM-RESET SECTION.
      ******************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWCRE3-SCREEN.
           DISPLAY BWCRE3-SCREEN UPON BWCRE3-WINDOW-HANDLE.
           MOVE BWCRE3-FORM-FIELDS TO WR-BUF.
           MOVE BWCRE3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************
           MOVE BWCRE3-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWCRE3-HELP--DIR TO HELP-LINK-DIR.

      ******************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BWCRE3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETDAT.CPY".
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

      ***************************
       IO-ROUTINE SECTION.
      ***************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPCHIN.CPY".

      ***************************
       END-ROUTINE SECTION.
      ***************************
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWCRE3-SCREEN.
           DESTROY BWCRE3-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
