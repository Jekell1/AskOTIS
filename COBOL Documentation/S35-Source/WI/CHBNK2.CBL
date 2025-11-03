       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHBNK2.
       DATE-WRITTEN.    04/15/2016.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   CHANGE BANKRRUPT STATUS
      *                 CALLED FROM F9 INTERRUPT AND LONPF2
      *                       PAGE 2 - REAFFIRMATION AND DISCHARGE
      *
      * REV:
      *  20160415 BAH NEW FOR WORLD PR#1150
      *  20160706 BAH TEST FOR SYSDATE-YYMMDD ENV VARIABLE 1165
      *  20170104 BAH DO NOT SET THRU ALL FIELDS ON NEW RECORD, WORLD #1216
      *               SET NEW-FLAG TO 2 INSTEAD OF 1 IN NEW-RECORD-SETUP
      *  BAH 170407 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 20190711 CHANGING AN EXISTING DISCHARGE DATE WAS MOVING VDUNUM0
      *               TO LTB-DISCHARGE-DATE INSTEAD OF VDU-DATE-YYYYMMDD
      *               CAUSING DATE TO BE WRONG. QA0043
      *************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      *******************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/CHBNK2.CBL S34 05/25/21-10:35:45 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  CHBNK2-LINES              PIC 99  VALUE 22.
       01  CHBNK2-COLUMNS            PIC 99  VALUE 78.
       01  CHBNK2-BEGIN-Y            PIC 99  VALUE 2.
       01  CHBNK2-BEGIN-X            PIC 99  VALUE 2.

       01  ERRCD           PIC X      VALUE SPACES.
       01  LEGEND-01.
           03  FILLER       PIC X(50)  VALUE
           "F1-RESET, F2-MEMO, F7-UPDATE, ENTER FIELD #:".

       01  LEGEND          PIC X(70).
       01  CHAN            PIC X(4).

       01  HOLD-SYS-DATE   PIC 9(8).
       01  BRANCH          PIC 9(4)   VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 99.
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM1       REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.
       01  D-DATE1           PIC X(6) VALUE " ".
       01  D-DATE            REDEFINES D-DATE1.
           03  D-YY          PIC 99.
           03  D-MM          PIC 99.
           03  D-DD          PIC 99.

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.


      * ORIGINAL RECORD
           COPY "LIBLP/LP01LTB.CPY" REPLACING LEADING "LTB" BY "LTB-OR".
      * CHANGED RECORD
           COPY "LIBLP/LP01LTB.CPY" REPLACING LEADING "LTB" BY "LTB-CH".

      *************************************
      *   SCREEN DISPLAY BUFFER
      *************************************
       01  DISPLAY-BUF     VALUE " ".
      * EFFECTIVE DATE
           03  F01         PIC 99/99/99.
           03  F02         PIC 99/99/99.
           03  F03         PIC Z(6).99.
           03  F04         PIC Z(6).99.
      * NUMBER OF PAYMENTS
           03  F05         PIC Z(3).
           03  F06         PIC Z(3).999.
           03  F07         PIC 99/99/99.
      * PARTIAL/FULL REAFFIRM
           03  F08         PIC X.

      * -- DISCHARGE --
           03  F09         PIC 99/99/99.
           03  F10         PIC 99/99/99.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(24) VALUE
           "010 020 030 040 050 060 ".
           03  FILLER      PIC X(24) VALUE
           "070 080 090 100.".

       01  MAX             PIC 99   VALUE 10.

      **********************************
      *   ELEMENT SPECIFICATIONS
      **********************************
       01  SPEC-TABLE.
      * 1. EFFECTIVE DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 2. SYSTEM DATE
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 3. BALANCE
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 4. PAYMENT AMOUNT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 5. NUMBER OF PAYMENTS
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      * 6. INTEREST RATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 7. RESCINDED DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 8. PARTIAL/FULL REAFFIRM
           03  FILLER      PIC X(8) VALUE "E  A010 ".

      * -- DISCHARGE --
      * 09. EFFECTIVE DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 10. SYSTEM DATE
           03  FILLER      PIC X(8) VALUE "ERNM060N".


       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 10 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      ************(WINDOW INFORMATION):
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/PAYOFXW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  MODIFY-SEQ            PIC 99 VALUE 01.

       01  CHBNK2-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CHBNK2_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/CHBNK2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CHBNK2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************
      *   P R O G R A M   C O N T R O L  *
      ************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHBNK2" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BRANCH.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE CHBNK2-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  CHBNK2-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    CHBNK2-COLUMNS + VDU-WIN-W-MOD
                      LINES   CHBNK2-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS CHBNK2-WINDOW-HANDLE.

           MOVE "SYSDATE_YYMMDD" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO D-DATE1
              MOVE D-DATE TO DATE-YYMMDD
              PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
           ELSE
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

           MOVE "S  A240TITLE:FR." TO VDU.
           MOVE " BANKRUPTCY INFORMATION" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM GET-GPENV.

           PERFORM READ-LOAN-FILE.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.


       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM GET-LT-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE
              GO TO END-ROUTINE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      *************************
       GET-LT-MODULE SECTION.
      *************************

      * ORIGINAL
           MOVE SPACES TO LTB-OR-REC.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

           MOVE EXT-WI-CL-OWNBR  TO LTB-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LTB-ACCTNO.
           MOVE 1                TO LTB-SEQNO.
           PERFORM OPEN-LTB1-FILE.
           PERFORM READ-LTB1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           IF IO-FG = 9
              PERFORM NEW-RECORD-SETUP
              GO TO END-GET-LT-MODULE.

      * DISPLAY THE BANKRUPT WHO FILED FLAG
           MOVE "S  A010WHO." TO VDU.
           MOVE LTB-WHO-FILED TO VDUBUF.
           PERFORM SCREENIO.

           MOVE 0 TO NEW-FLAG.
           MOVE LTB-REC TO LTB-OR-REC.

           MOVE LTB-REAFF-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO F01.
           MOVE LTB-REAFF-SYSDATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO F02.
           MOVE LTB-REAFF-BALANCE     TO F03.
           MOVE LTB-REAFF-PAYAMT      TO F04.
           MOVE LTB-REAFF-NO-PAYS     TO F05.
           MOVE LTB-INTRATE           TO F06.
           MOVE LTB-RESCIND-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO F07.
           MOVE LTB-PARTIAL-FULL      TO F08.
           MOVE LTB-DISCHARGE-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO F09.
           MOVE LTB-DISCHARGE-SYSDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO F10.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       END-GET-LT-MODULE.
           EXIT.

      *******************************
       NEW-RECORD-SETUP SECTION.
      *******************************

      *    MOVE 1 TO NEW-FLAG.
      * WORLD DOES NOT WANT TO WALK THRU ALL FIELDS ON A NEW RECORD
           MOVE 2 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LTB-FILE.

           MOVE EXT-WI-CL-OWNBR  TO LTB-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LTB-ACCTNO.
           MOVE 1                TO LTB-SEQNO.
           MOVE 0 TO ELE.

       NEW-RECORD-SETUP-EXIT.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
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
           IF VDUSTATUS = "12"
              MOVE "L"              TO MEMOSC-ONLY
              MOVE "B"              TO MEMOSC-MEMO-TYPE
              MOVE HOLD-SYS-DATE    TO EXT-WI-CL-TRANS-DATE
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "1>"
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.

      *    IF EXT-WI-CL-ARCHIVE-FG = "Y"
      *       GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           INSPECT ACTION REPLACING ALL " " BY ZEROS.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

      * DONT ALLOW CHANGES IF ACCOUNT IS NOT CURRENTLY IN BANKRUPT STATUS
           PERFORM READ-LOAN-FILE.
           IF IO-FG NOT = 0 OR LN-BNKRPTDATE = 0
              IF GP-BK-PRIOR-TO-POSTING NOT = "Y"
                 MOVE "ACCOUNT NOT IN BANKRUPT STATUS, CAN'T MODIFY"
                   TO MESS
                 PERFORM SEND-MESS
                 GO TO MODIFY-ROUTINE.

      * TEST FOR PASSWORD TO BE ABLE TO MODIFY ANY FIELD...
           MOVE MODIFY-SEQ TO PASSWDW-SEQ.
           MOVE "TO ALLOW MODIFY:" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              GO TO MODIFY-ROUTINE.

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

           MOVE " F1-CANCEL, ENTER FIELD INFORMATION:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE SPEC (ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F7.
           IF GP-BK-PRIOR-TO-POSTING = "Y"
              IF VDUSTATUS = "1>"
                 MOVE "END" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.
       TEST-F2.
           IF VDUSTATUS NOT = "12"
              GO TO TEST-F4.

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
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      **********************************
      *      SET RECORD ELEMENTS
      **********************************
       F-01.
      * IF CUSTOMER CLEARS THE REAFF DATE, THEN CLEAR THE SYSDATE
      * IF THEY CHANGE AN EXISTING REAFF DATE, LEAVE SYSDATE ALONE

      * TEST ORIGINAL REAFF DATE
           IF LTB-REAFF-DATE = 0
              MOVE VDU-DATE-YYYYMMDD TO LTB-REAFF-DATE
              IF VDUNUM0 NOT = 0
                 MOVE "S  A080020." TO VDU
                 MOVE HOLD-SYS-DATE TO LTB-REAFF-SYSDATE DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO VDU-DATE-EDIT
                 PERFORM SCREENIO
              ELSE
                 NEXT SENTENCE
      * IF ORIGINAL WAS NOT 0, AND THEY NOW ZEROED IT
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO LTB-REAFF-DATE
              IF VDUNUM0 = 0
                 MOVE "S  A080020." TO VDU
                 MOVE 0 TO VDU-DATE-EDIT
                           LTB-REAFF-SYSDATE
                 PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       F-02.
           IF VDUNUM0 = 0
              MOVE "S  A080030." TO VDU
              MOVE HOLD-SYS-DATE TO LTB-REAFF-SYSDATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO LTB-REAFF-SYSDATE.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM2 TO LTB-REAFF-BALANCE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO LTB-REAFF-PAYAMT.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM0 TO LTB-REAFF-NO-PAYS.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM3 TO LTB-INTRATE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDU-DATE-YYYYMMDD TO LTB-RESCIND-DATE.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO LTB-PARTIAL-FULL.
           IF LTB-PARTIAL-FULL NOT = "P" AND NOT = "F" AND NOT = " "
               MOVE "ENTER 'P' OR 'F' ONLY" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-09.
      * IF CUSTOMER CLEARS THE DISCHARGE DATE, THEN CLEAR THE SYSDATE
      * IF THEY CHANGE AN EXISTING DISCHARGE DATE, LEAVE SYSDATE ALONE

      * TEST ORIGINAL DISCHARGE DATE
           IF LTB-DISCHARGE-DATE = 0
              MOVE VDU-DATE-YYYYMMDD TO LTB-DISCHARGE-DATE
              IF VDUNUM0 NOT = 0
                 MOVE "S  A080100." TO VDU
                 MOVE HOLD-SYS-DATE TO LTB-DISCHARGE-SYSDATE
                                       DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO VDU-DATE-EDIT
                 PERFORM SCREENIO
              ELSE
                 NEXT SENTENCE
      * IF ORIGINAL WAS NOT 0, AND THEY NOW ZEROED IT
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO LTB-DISCHARGE-DATE
              IF VDUNUM0 = 0
                 MOVE "S  A080100." TO VDU
                 MOVE 0 TO VDU-DATE-EDIT
                           LTB-DISCHARGE-SYSDATE
                 PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

       F-10.
           IF VDUNUM0 = 0
              MOVE "S  A080100." TO VDU
              MOVE HOLD-SYS-DATE TO LTB-DISCHARGE-SYSDATE
                                    DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO LTB-DISCHARGE-SYSDATE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************
       WRITE-MODULE SECTION.
      ************************************

           PERFORM OPEN-LTB1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE LTB-REC TO LTB-CH-REC.

           IF LTB-CH-REC = LTB-OR-REC
              GO TO END-WRITE-MODULE.

       REWRITE-RECORD.
           PERFORM READ-LTB1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LTB-REC NOT = LTB-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE LTB-CH-REC TO LTB-REC.
           PERFORM REWRITE-LTB1-FILE.

           PERFORM CREATE-CHANGE-REC.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LTB1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           EXIT.


      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "CHBNK2" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************************
       CREATE-CHANGE-REC SECTION.
      ***************************************************
           PERFORM OPEN-CH-FILE.

      * CH-DESCRIPT MUST BE LIMITED TO 20 SO THE MEMO WILL FIT!

           MOVE LN-ACCTNO  TO CH-ACCTNO.

           IF (LTB-OR-REAFF-DATE NOT = LTB-REAFF-DATE)
              PERFORM CREATE-SETUP-REC
              MOVE "REAFF EFFDATE"      TO CH-DESCRIPT
              MOVE LTB-OR-REAFF-DATE    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY          TO CH-ORG-DATE
              MOVE LTB-REAFF-DATE       TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY          TO CH-NEW-DATE
              PERFORM UPDATE-CH-LM-FILE.
           IF (LTB-OR-REAFF-SYSDATE NOT = LTB-REAFF-SYSDATE)
              PERFORM CREATE-SETUP-REC
              MOVE "REAFF SYSDATE"       TO CH-DESCRIPT
              MOVE LTB-OR-REAFF-SYSDATE  TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY           TO CH-ORG-DATE
              MOVE LTB-REAFF-SYSDATE     TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY           TO CH-NEW-DATE
              PERFORM UPDATE-CH-LM-FILE.
           IF (LTB-OR-REAFF-BALANCE NOT = LTB-REAFF-BALANCE)
              PERFORM CREATE-SETUP-REC
              MOVE "REAFF BALANCE"       TO CH-DESCRIPT
              MOVE LTB-OR-REAFF-BALANCE    TO CH-ORG-6DEC2
              MOVE LTB-REAFF-BALANCE       TO CH-NEW-6DEC2
              PERFORM UPDATE-CH-LM-FILE.
           IF (LTB-OR-REAFF-PAYAMT NOT = LTB-REAFF-PAYAMT)
              PERFORM CREATE-SETUP-REC
              MOVE "REAFF PAYAMT"       TO CH-DESCRIPT
              MOVE LTB-OR-REAFF-PAYAMT    TO CH-ORG-6DEC2
              MOVE LTB-REAFF-PAYAMT       TO CH-NEW-6DEC2
              PERFORM UPDATE-CH-LM-FILE.
           IF (LTB-OR-REAFF-NO-PAYS NOT = LTB-REAFF-NO-PAYS)
              PERFORM CREATE-SETUP-REC
              MOVE "REAFF NUMBER PAYS"   TO CH-DESCRIPT
              MOVE LTB-OR-REAFF-NO-PAYS    TO CH-ORG-NODEC
              MOVE LTB-REAFF-NO-PAYS       TO CH-NEW-NODEC
              PERFORM UPDATE-CH-LM-FILE.
           IF (LTB-OR-INTRATE NOT = LTB-INTRATE)
              PERFORM CREATE-SETUP-REC
              MOVE "REAFF INTRATE"       TO CH-DESCRIPT
              MOVE LTB-OR-INTRATE    TO CH-ORG-RATE
              MOVE LTB-INTRATE       TO CH-NEW-RATE
              PERFORM UPDATE-CH-LM-FILE.
           IF (LTB-OR-RESCIND-DATE NOT = LTB-RESCIND-DATE)
              PERFORM CREATE-SETUP-REC
              MOVE "REAFF RESCIND DATE"       TO CH-DESCRIPT
              MOVE LTB-OR-RESCIND-DATE    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY            TO CH-ORG-DATE
              MOVE LTB-RESCIND-DATE       TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY            TO CH-NEW-DATE
              PERFORM UPDATE-CH-LM-FILE.
           IF (LTB-OR-PARTIAL-FULL NOT = LTB-PARTIAL-FULL)
              PERFORM CREATE-SETUP-REC
              MOVE "REAFF REAFF TYPE" TO CH-DESCRIPT
              MOVE LTB-OR-PARTIAL-FULL    TO CH-ORG-CHAR
              MOVE LTB-PARTIAL-FULL       TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF (LTB-OR-DISCHARGE-DATE NOT = LTB-DISCHARGE-DATE)
              PERFORM CREATE-SETUP-REC
              MOVE "DISCHARGE EFFDATE"   TO CH-DESCRIPT
              MOVE LTB-OR-DISCHARGE-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY           TO CH-ORG-DATE
              MOVE LTB-DISCHARGE-DATE    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY           TO CH-NEW-DATE
              PERFORM UPDATE-CH-LM-FILE.
           IF (LTB-OR-DISCHARGE-SYSDATE NOT = LTB-DISCHARGE-SYSDATE)
              PERFORM CREATE-SETUP-REC
              MOVE "DISCHARGE SYSDATE"      TO CH-DESCRIPT
              MOVE LTB-OR-DISCHARGE-SYSDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY              TO CH-ORG-DATE
              MOVE LTB-DISCHARGE-SYSDATE    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY              TO CH-NEW-DATE
              PERFORM UPDATE-CH-LM-FILE.

           PERFORM CLOSE-CH-FILE.
           GO TO EXIT-CREATE-CHANGE-REC.

       CREATE-SETUP-REC.
           PERFORM CLEAR-CH-FILE.
           MOVE BRANCH             TO CH-BRNO.
           MOVE HOLD-SYS-DATE      TO CH-DATE.
           MOVE "B"                TO CH-CODE.
           MOVE EXT-CONAME-USER-ID TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS           TO CH-TIME.
           MOVE 1                  TO CH-SEQ.
           MOVE LN-ACCTNO  TO CH-ACCTNO.
           MOVE SPACES     TO CH-ORG-CHAR CH-NEW-CHAR.
       EXIT-CREATE-CHANGE-REC.
           EXIT.

      **********************************************
       UPDATE-CH-LM-FILE SECTION.
      **********************************************
       UPDATE-CH-AGAIN.
           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF CH-SEQ < 999
                 ADD 1 TO CH-SEQ
                 GO TO UPDATE-CH-AGAIN.

           IF BR-CHANGE-MEMO = "Y"
              MOVE "CL/CHMEMO" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CH-REC
              CANCEL FORM-PROGX.

       EXIT-UPDATE-CH-LM-FILE.
           EXIT.

       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPLTBCL.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON CHBNK2-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY CHBNK2-SCREEN UPON CHBNK2-WINDOW-HANDLE.
           MOVE CHBNK2-FORM-FIELDS TO WR-BUF.
           MOVE CHBNK2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE CHBNK2-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHBNK2-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CHBNK2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************
       MISC-PARAGRAPHS SECTION.
      ********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       READ-LOAN-FILE.
           PERFORM OPEN-LN1-FILE.
           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE
           " THIS RECORD WAS ALREADY CREATED BY ANOTHER USER,"
           TO MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE LEGEND TO VDUBUF.
           PERFORM SCREENIO.

      **************************
       IO-ROUTINE SECTION.
      **************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBLP/LPLTB1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHBNK2-SCREEN.
           DESTROY CHBNK2-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
