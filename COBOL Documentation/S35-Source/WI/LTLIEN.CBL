       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LTLIEN.
       DATE-WRITTEN.    052803.
      **************************************************************************
      *   LIENHOLDER INFO   CALLED FROM F9-COLLATERAL (LTSCAN)
      *                          PAGE 2
      *                        (WINDOW)
      * REV:
      *  030528 BAH NEW, LIENHOLDER INFO FOR REGACC PR# 20
      *  030625 BAH ADDED LIENHOLDER COMMENTS, REGACC PR# 32
      *  BAH 030711 GUNK REMAINED IN THE CH-NEW-CHAR CAUSE IT DID NOT
      *             GET CLEARED OUT BEFORE MOVING IN CH-NEW-NUM'S..
      *             CAUSED AUDIT-CHANGES REGISTER TO NOT LINE UP CORRECTLY
      *  BAH 040521 ADDED ADDITIONAL LIEN FIELDS FOR REGACC PR# 60
      *             LT-L-TITLE-RCV-DATE, LT-L-TITLE-LIEN-DATE,
      *             LT-L-TITLE-NO, LT-L-LIEN-LETTER-SENT
      *  BAH 040803 ADDED LIBWI/TITLEW, REGACC PR# 58
      *  BAH 120905 ADDED LT-L-LIEN-TITLE-STATE, REGACC PR# 319
      *  BAH 120924 ADDED LT-L-TITLE-STATUS-CODE, REGACC PR# 321
      *  BAH 140505 RENAMED WI/TITLE TO WI/TITLEW, RESERVED WORD VERYANT
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      *  BAH 170420 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181130 GLOBAL LMFILE
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
           "@(#)WI/LTLIEN.CBL S35 02/22/24-12:53:12 >".
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01LTL.CPY".
       COPY "LIBLP/LP01LTL_SQL.CPY".
       COPY "LIBLP/LPWSLTL.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ERRCD                       PIC X VALUE " ".
       01  SAVE-LM1-KEY                VALUE ZEROES.
           03  SAVE-LM-BRNO            PIC 9(04).
           03  SAVE-LM-ACCTNO          PIC 9(08).
           03  SAVE-LM-DATE            PIC 9(08).
           03  SAVE-LM-SEQ             PIC 9(03).

       01  MEMO-AREA.
           03  FILLER          PIC X(5)  VALUE "OLD: ".
           03  MEMO-DESCRIPT   PIC X(20).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-ORG-CHAR   PIC X(24).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-USERID     PIC X(10).

       01  LEGEND          PIC X(35).
       01  LEGEND-01.
           03  FILLER      PIC X(33)  VALUE
           "F1, F7-EXIT WITH UPDATE, ENTER #:".
       01  LEGEND-02.
           03  FILLER      PIC X(8)  VALUE
           " F7-EXIT".

       01  HOLD-SYS-DATE   PIC 9(8).

       01  SUB             PIC 999    COMP  VALUE 0.
       01  ONE             PIC S9     COMP.
       01  ELE             PIC 9(2)   COMP.
       01  NEW-FLAG        PIC 9.

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM1       REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.

      * ORIGINAL RECORD
       COPY "LIBLP/LP01LTL.CPY" REPLACING LEADING "LTL" BY "LTL-OR".
      * CHANGED RECORD
       COPY "LIBLP/LP01LTL.CPY" REPLACING LEADING "LTL" BY "LTL-CH".
    

       01  IO-TYPE         PIC X(3).
       01  FIRST-TIME      PIC 9     VALUE 0.

      *************************************
      *   SCREEN DISPLAY BUFFER
      *************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  DISPLAY-BUFX OCCURS 3 TIMES.
               05  D-TYPE          PIC 9.
               05  D-NAME          PIC X(30).
               05  D-ADR           PIC X(24).
               05  D-CITY          PIC X(15).
               05  D-STATE         PIC X(2).
               05  D-ZIP           PIC X(10).
               05  D-DATE          PIC 99/99/99.
           03  D-COMMENT1          PIC X(60).
           03  D-COMMENT2          PIC X(60).
           03  D-TITLE-RCV-DATE    PIC 99/99/99.
           03  D-TITLE-LIEN-DATE   PIC 99/99/99.
           03  D-TITLE-NO          PIC X(20).
           03  D-LIEN-LETTER-SENT  PIC X.
           03  D-LIEN-TITLE-STATE  PIC XX.
           03  D-TITLE-STATUS-CODE PIC XX.


       01  DISPLAY-LIST.
           03  FILLER      PIC X(56) VALUE
           "010 020 030 040 041 042 050 060 070 080 090 091 092 100 ".
           03  FILLER      PIC X(56) VALUE
           "110 120 130 140 141 142 150 160 170 180 190 200 210 220 ".
           03  FILLER      PIC X(4) VALUE
           "230.".

       01  MAX             PIC 99   VALUE 23.

       01  SPEC-TABLE.
      *01. TYPE 1
           03  FILLER      PIC X(8) VALUE "ENNN010 ".
      *02. NAME 1
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      *03. ADR 1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *04. CITY 1
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      *05. DATE 1
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *06. TYPE 2
           03  FILLER      PIC X(8) VALUE "ENNN010 ".
      *07. NAME 2
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      *08. ADR 2
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *09. CITY 2
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      *10. DATE 2
           03  FILLER      PIC X(8) VALUE "ERNM080 ".
      *11. TYPE 3
           03  FILLER      PIC X(8) VALUE "ENNN010 ".
      *12. NAME 3
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      *13. ADR 3
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *14. CITY 3
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      *15. DATE 3
           03  FILLER      PIC X(8) VALUE "ERNM080 ".
      *16. COMMENT1
           03  FILLER      PIC X(8) VALUE "E  A600 ".
      *17. COMMENT2
           03  FILLER      PIC X(8) VALUE "E  A600 ".
      *18. DATE TITLE RECEIVED
           03  FILLER      PIC X(8) VALUE "ERNM080 ".
      *19. TITLE LIEN DATE
           03  FILLER      PIC X(8) VALUE "ERNM080 ".
      *20. TITLE NO.
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      *21. LIEN LETTER SENT
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *22. TITLE STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      *23. TITLE STATUS CODE
           03  FILLER      PIC X(8) VALUE "E  A020N".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 23 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.


       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TITLEW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  MODIFY-SEQ              PIC 99 VALUE 01.

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LTLIEN_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/LTLIEN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  LN-OWNBR          PIC 9(4) COMP-3.
       01  LN-ACCTNO         PIC 9(8).

       SCREEN SECTION.
       COPY "LIBWI/LTLIEN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME LN-OWNBR LN-ACCTNO EXIT-PATHNAME.

      *******************************
       MAIN-PROGRAM SECTION.
      *******************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE 22 TO WINDOW-LINES.
           MOVE 78 TO WINDOW-COLUMNS.
           MOVE 2 TO WINDOW-BEGIN-Y.
           MOVE 2 TO WINDOW-BEGIN-X.
           MOVE "WI/LTLIEN" TO WINDOW-FORM-NAM.
           MOVE "LTLIEN" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY LTLIEN-SCREEN.

           MOVE LTLIEN-FORM-FIELDS TO WR-BUF.
           MOVE LTLIEN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE " " TO BR-CHANGE-MEMO
              MOVE "N" TO BR-COLL-ID-SCAN.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

       MAIN-MODULE.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
           ELSE
              PERFORM FORM-RESET.

           MOVE SPACES TO IO-TYPE.
           PERFORM DISPLAY-FIELDS.

           MOVE 0 TO NEW-FLAG.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.


      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
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
           MOVE LEGEND-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              PERFORM UPDATE-LT
              PERFORM CREATE-CHANGE-REC
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           IF FORM-KEY1 = " "
              IF FORM-P1 = "P"
                 IF FORM-SEL1 = 3
                    MOVE "END" TO IO-TYPE
                    PERFORM UPDATE-LT
                    PERFORM CREATE-CHANGE-REC
                    MOVE LN-OWNBR  TO TITLE-LN-OWNBR
                    MOVE LN-ACCTNO TO TITLE-LN-ACCTNO
                    MOVE "N" TO TITLE-FROM-LONPFA
                    MOVE "WI/TITLEW" TO FORM-NAM
                    CALL FORM-PROGX USING FORM-PATH TITLE-PROCESSING-BUF
                    CANCEL FORM-PROGX
                    GO TO MODIFY-ROUTINE
                 ELSE
                    GO TO MODIFY-ROUTINE.
           IF ACTION = SPACES
              GO TO MODIFY-ROUTINE.

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
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.

           IF ELE = 18 OR 19 OR 20 OR 21
              MOVE MODIFY-SEQ TO PASSWDW-SEQ
              MOVE "TO ALLOW MODIFY:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

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
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-04 A-05 A-06 A-07 A-08
                 A-09 A-10 A-11 A-12 A-13 A-14 A-15 A-16
                 A-17 A-18 A-19 A-20 A-21 A-22 A-23
                  DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************

       A-01.
           MOVE VDUNUM0 TO LTL-LIEN-TYPE(1).
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO LTL-LIEN-NAME(1).
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO LTL-LIEN-ADR(1).
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO LTL-LIEN-CITY(1).
       A-041.
           MOVE "E  A020041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-041.
           MOVE VDUBUF TO LTL-LIEN-STATE(1).

       A-042.
           MOVE "E  A100042." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-041.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-042.
           MOVE VDUBUF TO LTL-LIEN-ZIP(1).

           GO TO CREATE-ROUTINE.

       A-05.
           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE LTL-LIEN-DATE(1).
           IF VDUNUM0 NOT = 0
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 MOVE "INVALID DATE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       A-06.
           MOVE VDUNUM0 TO LTL-LIEN-TYPE(2).
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUBUF TO LTL-LIEN-NAME(2).
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUBUF TO LTL-LIEN-ADR(2).
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUBUF TO LTL-LIEN-CITY(2).
       A-091.
           MOVE "E  A020091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-091.
           MOVE VDUBUF TO LTL-LIEN-STATE(2).

       A-092.
           MOVE "E  A100092." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-091.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-092.
           MOVE VDUBUF TO LTL-LIEN-ZIP(2).

           GO TO CREATE-ROUTINE.

       A-10.
           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE LTL-LIEN-DATE(2).
           IF VDUNUM0 NOT = 0
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 MOVE "INVALID DATE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.


       A-11.
           MOVE VDUNUM0 TO LTL-LIEN-TYPE(3).
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUBUF TO LTL-LIEN-NAME(3).
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUBUF TO LTL-LIEN-ADR(3).
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUBUF TO LTL-LIEN-CITY(3).
       A-141.
           MOVE "E  A020141." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-141.
           MOVE VDUBUF TO LTL-LIEN-STATE(3).

       A-142.
           MOVE "E  A100142." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-141.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-142.
           MOVE VDUBUF TO LTL-LIEN-ZIP(3).

           GO TO CREATE-ROUTINE.

       A-15.
           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE LTL-LIEN-DATE(3).
           IF VDUNUM0 NOT = 0
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 MOVE "INVALID DATE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       A-16.
           MOVE VDUBUF TO LTL-LIEN-COMMENT1.
           GO TO CREATE-ROUTINE.

       A-17.
           MOVE VDUBUF TO LTL-LIEN-COMMENT2.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE LTL-TITLE-RCV-DATE.
           IF VDUNUM0 NOT = 0
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 MOVE "INVALID DATE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE LTL-TITLE-LIEN-DATE.
           IF VDUNUM0 NOT = 0
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 MOVE "INVALID DATE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUBUF TO LTL-TITLE-NO.
           GO TO CREATE-ROUTINE.
       A-21.
           IF VDUBUF NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LTL-LIEN-LETTER-SENT.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUBUF TO LTL-LIEN-TITLE-STATE.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUBUF TO LTL-TITLE-STATUS-CODE.
           GO TO CREATE-ROUTINE.


       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************
       UPDATE-LT SECTION.
      ************************************
           PERFORM OPEN-LTL1-FILE.

      * IF ORIGINAL IS NOT = WHAT YOU JUST BUILT
           IF LTL-OR-REC NOT = LTL-REC
              PERFORM WRITE-LTL1-FILE
              IF IO-FG NOT = 0
                 PERFORM REWRITE-LTL1-FILE.

           PERFORM CLOSE-LTL1-FILE.

       EXIT-UPDATE-LT.
           EXIT.

      **************************
       CREATE-CHANGE-REC SECTION.
      **************************
           PERFORM OPEN-CH-FILE.

           PERFORM OPEN-LM1-FILE.

           IF LTL-OR-LIEN-TYPE(1) NOT = LTL-LIEN-TYPE(1)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN TYPE 1"       TO CH-DESCRIPT
              MOVE SPACES TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE LTL-OR-LIEN-TYPE(1) TO CH-ORG-NUM
              MOVE LTL-LIEN-TYPE(1)    TO CH-NEW-NUM
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-NAME(1) NOT = LTL-LIEN-NAME(1)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN NAME 1"       TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-NAME(1) TO CH-ORG-CHAR
              MOVE LTL-LIEN-NAME(1)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-ADR(1) NOT = LTL-LIEN-ADR(1)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN ADR 1" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-ADR(1) TO CH-ORG-CHAR
              MOVE LTL-LIEN-ADR(1)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-CITY(1) NOT = LTL-LIEN-CITY(1)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN CITY 1" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-CITY(1) TO CH-ORG-CHAR
              MOVE LTL-LIEN-CITY(1) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-STATE(1) NOT = LTL-LIEN-STATE(1)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN STATE 1" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-STATE(1) TO CH-ORG-CHAR
              MOVE LTL-LIEN-STATE(1) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-ZIP(1) NOT = LTL-LIEN-ZIP(1)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN ZIP 1" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-ZIP(1) TO CH-ORG-CHAR
              MOVE LTL-LIEN-ZIP(1) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-DATE(1) NOT = LTL-LIEN-DATE(1)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN DATE 1" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-DATE(1) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY         TO CH-ORG-NUM
              MOVE LTL-LIEN-DATE(1) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY         TO CH-NEW-NUM
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-TITLE-RCV-DATE NOT = LTL-TITLE-RCV-DATE
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE RCV DATE" TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-RCV-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY         TO CH-ORG-NUM
              MOVE LTL-TITLE-RCV-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY         TO CH-NEW-NUM
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-TITLE-LIEN-DATE NOT = LTL-TITLE-LIEN-DATE
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE LIEN DATE" TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-LIEN-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY         TO CH-ORG-NUM
              MOVE LTL-TITLE-LIEN-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY         TO CH-NEW-NUM
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-TITLE-NO NOT = LTL-TITLE-NO
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE NO." TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-NO TO CH-ORG-CHAR
              MOVE LTL-TITLE-NO TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-LETTER-SENT NOT = LTL-LIEN-LETTER-SENT
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN LETTER SENT" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-LETTER-SENT TO CH-ORG-CHAR
              MOVE LTL-LIEN-LETTER-SENT TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTL-OR-LIEN-TYPE(2) NOT = LTL-LIEN-TYPE(2)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN TYPE 2"    TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-TYPE(2) TO CH-ORG-NUM
              MOVE LTL-LIEN-TYPE(2)    TO CH-NEW-NUM
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-NAME(2) NOT = LTL-LIEN-NAME(2)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN NAME 2"    TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-NAME(2) TO CH-ORG-CHAR
              MOVE LTL-LIEN-NAME(2)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-ADR(2) NOT = LTL-LIEN-ADR(2)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN ADR 2" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-ADR(2) TO CH-ORG-CHAR
              MOVE LTL-LIEN-ADR(2)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-CITY(2) NOT = LTL-LIEN-CITY(2)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN CITY 2" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-CITY(2) TO CH-ORG-CHAR
              MOVE LTL-LIEN-CITY(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-STATE(2) NOT = LTL-LIEN-STATE(2)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN STATE 2" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-STATE(2) TO CH-ORG-CHAR
              MOVE LTL-LIEN-STATE(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-ZIP(2) NOT = LTL-LIEN-ZIP(2)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN ZIP 2" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-ZIP(2) TO CH-ORG-CHAR
              MOVE LTL-LIEN-ZIP(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-DATE(2) NOT = LTL-LIEN-DATE(2)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN DATE 2" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-DATE(2) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-ORG-NUM
              MOVE LTL-LIEN-DATE(2) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-NEW-NUM
              PERFORM UPDATE-CH-LM-FILE.


           IF LTL-OR-LIEN-TYPE(3) NOT = LTL-LIEN-TYPE(3)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN TYPE 3"    TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-TYPE(3) TO CH-ORG-NUM
              MOVE LTL-LIEN-TYPE(3)    TO CH-NEW-NUM
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-NAME(3) NOT = LTL-LIEN-NAME(3)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN NAME 3"    TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-NAME(3) TO CH-ORG-CHAR
              MOVE LTL-LIEN-NAME(3)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-ADR(3) NOT = LTL-LIEN-ADR(3)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN ADR 3" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-ADR(3) TO CH-ORG-CHAR
              MOVE LTL-LIEN-ADR(3)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-CITY(3) NOT = LTL-LIEN-CITY(3)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN CITY 3" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-CITY(3) TO CH-ORG-CHAR
              MOVE LTL-LIEN-CITY(3) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-STATE(3) NOT = LTL-LIEN-STATE(3)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN STATE 3" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-STATE(3) TO CH-ORG-CHAR
              MOVE LTL-LIEN-STATE(3) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-ZIP(3) NOT = LTL-LIEN-ZIP(3)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN ZIP 3" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-ZIP(3) TO CH-ORG-CHAR
              MOVE LTL-LIEN-ZIP(3) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-DATE(3) NOT = LTL-LIEN-DATE(3)
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN DATE 3" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-DATE(3) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-ORG-NUM
              MOVE LTL-LIEN-DATE(3) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-NEW-NUM
              PERFORM UPDATE-CH-LM-FILE.

           IF LTL-OR-LIEN-COMMENT1 NOT = LTL-LIEN-COMMENT1
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN COMMENT1 1" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-COMMENT1 TO CH-ORG-CHAR
              MOVE LTL-LIEN-COMMENT1    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-COMMENT2 NOT = LTL-LIEN-COMMENT2
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN COMMENT2 2" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-COMMENT2 TO CH-ORG-CHAR
              MOVE LTL-LIEN-COMMENT2 TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-LIEN-TITLE-STATE NOT = LTL-LIEN-TITLE-STATE
              PERFORM CREATE-SETUP-REC
              MOVE "LIEN TITLE STATE" TO CH-DESCRIPT
              MOVE LTL-OR-LIEN-TITLE-STATE TO CH-ORG-CHAR
              MOVE LTL-LIEN-TITLE-STATE TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-TITLE-STATUS-CODE NOT = LTL-TITLE-STATUS-CODE
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE STATUS CD " TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-STATUS-CODE TO CH-ORG-CHAR
              MOVE LTL-TITLE-STATUS-CODE TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           PERFORM CLOSE-CH-FILE.
           GO TO EXIT-CREATE-CHANGE-REC.
       CREATE-SETUP-REC.
           PERFORM CLEAR-CH-FILE.
           MOVE EXT-CONAME-USER-LOC TO CH-BRNO.
           MOVE HOLD-SYS-DATE       TO CH-DATE.
           MOVE "T"                 TO CH-CODE.
           MOVE EXT-CONAME-USER-ID  TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS            TO CH-TIME.
           MOVE 1                   TO CH-SEQ.

           MOVE LN-ACCTNO TO CH-ACCTNO.

       EXIT-CREATE-CHANGE-REC.
           EXIT.

      ************************************
       UPDATE-CH-LM-FILE SECTION.
      ************************************
       UPDATE-CH-AGAIN.
           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF CH-SEQ < 999
                 ADD 1 TO CH-SEQ
                 GO TO UPDATE-CH-AGAIN.

           IF BR-CHANGE-MEMO NOT = "Y"
              GO TO UPDATE-CH-LM-FILE-EXIT.

           MOVE LM-PATH-OWNBR TO SAVE-LM-BRNO.
           MOVE LN-ACCTNO     TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - HOLD-SYS-DATE.
           MOVE 1             TO SAVE-LM-SEQ.
       UPDATE-CH-LM-FILE-AGAIN.
           MOVE SPACES TO LM-REC.

           MOVE SAVE-LM1-KEY TO LM1-KEY.
           MOVE CH-USERID    TO MEMO-USERID.
           MOVE CH-DESCRIPT  TO MEMO-DESCRIPT.
           MOVE CH-ORG-CHAR  TO MEMO-ORG-CHAR.
           MOVE MEMO-AREA    TO LM-MEMO.
           MOVE "9"          TO LM-MEMO-TYPE.

           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              IF SAVE-LM-SEQ < 999
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO UPDATE-CH-LM-FILE-AGAIN.

       UPDATE-CH-LM-FILE-EXIT.
           EXIT.

      ************************************
       DISPLAY-FIELDS SECTION.
      ************************************

           MOVE SPACES TO DISPLAY-BUF
                          LTL-OR-REC
                          LTL-CH-REC.

           PERFORM OPEN-LTL1-FILE.

           MOVE LN-OWNBR  TO LTL-BRNO.
           MOVE LN-ACCTNO TO LTL-ACCTNO.
           MOVE 1         TO LTL-SEQNO.
           PERFORM READ-LTL1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-LTL-FILE
              MOVE LN-OWNBR  TO LTL-BRNO
              MOVE LN-ACCTNO TO LTL-ACCTNO
              MOVE 1         TO LTL-SEQNO.

           MOVE LTL-REC TO LTL-OR-REC LTL-CH-REC.

           MOVE 1 TO SUB.
           PERFORM FILL-DISPLAY-TABLE.

           MOVE LTL-TITLE-RCV-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO D-TITLE-RCV-DATE.

           MOVE LTL-TITLE-LIEN-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO D-TITLE-LIEN-DATE.

           MOVE LTL-TITLE-NO         TO D-TITLE-NO.
           MOVE LTL-LIEN-LETTER-SENT TO D-LIEN-LETTER-SENT.

           MOVE 2 TO SUB.
           PERFORM FILL-DISPLAY-TABLE.

           MOVE 3 TO SUB.
           PERFORM FILL-DISPLAY-TABLE.

           MOVE LTL-LIEN-COMMENT1 TO D-COMMENT1.
           MOVE LTL-LIEN-COMMENT2 TO D-COMMENT2.
           MOVE LTL-LIEN-TITLE-STATE TO D-LIEN-TITLE-STATE.
           MOVE LTL-TITLE-STATUS-CODE TO D-TITLE-STATUS-CODE.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CLOSE-LTL1-FILE.

      ************************
       VERIFY-PASSWORD SECTION.
      ************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LTLIEN" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPLTLCL.CPY".

      ***************************************
       FORM-RESET SECTION.
      ***************************************
           DISPLAY LTLIEN-SCREEN.
           MOVE LTLIEN-FORM-FIELDS TO WR-BUF.
           MOVE LTLIEN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************
       SETUP-LINK-INFO SECTION.
      **************************************
           MOVE LTLIEN-HELP--FILE TO HELP-LINK-FILE.
           MOVE LTLIEN-HELP--DIR TO HELP-LINK-DIR.

      **************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LTLIEN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************
       MISC-PARAGRAPHS SECTION.
      ********************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       FILL-DISPLAY-TABLE.
           MOVE LTL-LIEN-TYPE(SUB)  TO D-TYPE(SUB).
           MOVE LTL-LIEN-NAME(SUB)  TO D-NAME(SUB).
           MOVE LTL-LIEN-ADR(SUB)   TO D-ADR(SUB).
           MOVE LTL-LIEN-CITY(SUB)  TO D-CITY(SUB).
           MOVE LTL-LIEN-STATE(SUB) TO D-STATE(SUB).
           MOVE LTL-LIEN-ZIP(SUB)   TO D-ZIP(SUB).
           MOVE LTL-LIEN-DATE(SUB)  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-DATE(SUB).
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

      ***************************
       IO-ROUTINE SECTION.
      ***************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTL1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLTLGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLTL1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ***************************
       END-ROUTINE SECTION.
      ***************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LTLIEN-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
