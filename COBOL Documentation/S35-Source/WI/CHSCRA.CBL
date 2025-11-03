       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHSCRA.
       DATE-WRITTEN. 03/28/2011.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   SERVICE MEMBER CIVIL RELIEF ACT
      *                 CALLED FROM F9 INTERRUPT
      *
      *         WRITTEN AND PAID FOR BY REGACC
      *
      *  REV:
      *   BAH 110328 REGACC PR# 289
      *   BAH 160209 SPLIT OUT LSFILE, PD0003
      *   BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      *   BAH 170412 ACTIVATE 8 DIGIT DATES, PD0006
      *   BAH 181130 GLOBAL LMFILE
      *   BAH 190513 NEW CHFILE
      *   BAH 2024.0417 REMOVED CHSCRAW, CALL PROGRAM INSTEAD OF WINDOW
      *                 NOTHING TO PASS
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/CHSCRA.CBL S35 02/22/24-09:56:51 >".

      ******************************************************************
      *
      *     P R O G R A M    W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01LSR.CPY".
       COPY "LIBLP/LP01LSR_SQL.CPY".
       COPY "LIBLP/LPWSLSR.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
      
       01  CHSCRA-LINES              PIC 99  VALUE 22.
       01  CHSCRA-COLUMNS            PIC 99  VALUE 78.
       01  CHSCRA-BEGIN-Y            PIC 99  VALUE 2.
       01  CHSCRA-BEGIN-X            PIC 99  VALUE 2.

       01  ERRCD           PIC X       VALUE SPACES.
       01  LEGEND          PIC X(70).
       01  LEGEND-01.
           03  FILLER       PIC X(37)  VALUE
           "F1-RESET, F7-UPDATE, ENTER FIELD #:".

       01  SAVE-LM1-KEY                 VALUE ZEROES.
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
      *-----------------------------------------------------------------
       01  CHAN              PIC X(4)  VALUE "CHAN".
       01  ONE               PIC S9.
       01  ELE               PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).

       01  NEW-FLAG          PIC 9.

       01  HOLD-LSR1-KEY.
           03  HOLD-BRNO     PIC 9(4).
           03  HOLD-ACCTNO   PIC 9(8).
           03  HOLD-SEQNO    PIC 99.

       01  HOLD-SYS-DATE   PIC 9(8).

      * ORIGINAL RECORD
           COPY "LIBLP/LP01LSR.CPY" REPLACING LEADING "LSR" BY "LSR-OR".
      * CHANGED RECORD
           COPY "LIBLP/LP01LSR.CPY" REPLACING LEADING "LSR" BY "LSR-CH".

       01  DISPLAY-BUF1      VALUE SPACES.
           03  D-NUMBER          PIC Z(6).
           03  D-NAME            PIC X(30).

       01  DISPLAY-LIST1.
           03  FILLER      PIC X(12) VALUE
           "ACCTNO NAME.".

       01  DISPLAY-BUF      VALUE SPACES.
      * ACTVE
           03  D-01              PIC X.
      * SCRA START DATE
           03  D-02              PIC 99/99/99.
      * SCRA END DATE
           03  D-03              PIC 99/99/99.
      * BORROWER
           03  D-04              PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(16) VALUE
           "010 020 030 040.".

       78  MAX            VALUE 4.
      ******************************************************************
      *      ELEMENT SPECIFICATIONS
      ******************************************************************
       01  SPEC-TABLE.
      * 1 ACTIVE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 2 SCRA START DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 3 SCRA END DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 4 BORROWER
           03  FILLER      PIC X(8) VALUE "E  A010 ".

       01  SPEC-TAB      REDEFINES SPEC-TABLE.
           03  SPEC-REC  OCCURS MAX TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.


      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/PASSWDW.CPY".
       01  CREATE-SEQ              PIC 99 VALUE 01.
       01  MODIFY-SEQ              PIC 99 VALUE 02.
       01  DELETE-SEQ              PIC 99 VALUE 03.


       01  CHSCRA-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CHSCRA_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/CHSCRA_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CHSCRA_SCN.CPY".

      ************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************
      *   P R O G R A M   C O N T R O L  *
      ************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHSCRA" TO VDU-FORM-NAME.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE CHSCRA-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  CHSCRA-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    CHSCRA-COLUMNS + VDU-WIN-W-MOD
                      LINES   CHSCRA-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS CHSCRA-WINDOW-HANDLE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-LS-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE
              GO TO END-ROUTINE.

           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE
             GO TO END-ROUTINE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      **************************
       REC-LS-MODULE SECTION.
      **************************
           MOVE SPACES TO IO-TYPE LSR-OR-REC.
           MOVE 0 TO NEW-FLAG.

           MOVE SPACES TO DISPLAY-BUF1.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           MOVE LN-ACCTNO TO D-NUMBER.

           PERFORM OPEN-BW1-FILE.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           MOVE SPACES TO D-NAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                                         INTO D-NAME.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE EXT-WI-CL-OWNBR  TO LSR-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LSR-ACCTNO.
           MOVE 1                TO LSR-SEQNO.
           MOVE LSR1-KEY TO HOLD-LSR1-KEY.

           PERFORM OPEN-LSR1-FILE.
           PERFORM READ-LSR1-FILE.
           PERFORM CLOSE-LSR1-FILE.
           IF IO-FG = 0
              MOVE LSR-REC TO LSR-OR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              GO TO END-REC-LS-MODULE.

           PERFORM NEW-RECORD-SETUP.

           MOVE CREATE-SEQ TO PASSWDW-SEQ.
           MOVE "TO CREATE NEW RECORD:" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LS-MODULE.

       END-REC-LS-MODULE.
           EXIT.

      ******************************************************************
      *    ENTRY-MODULE
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
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
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

      *----------------------------------------------------------------
      * RECORD CAN ONLY BE DELETED WHILE AT PAGE 1.
      *----------------------------------------------------------------
           IF ACTION = "DELETE"
              MOVE DELETE-SEQ TO PASSWDW-SEQ
              MOVE "TO ALLOW DELETE:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "DEL" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      * TEST FOR PASSWORD TO BE ABLE TO MODIFY ANY FIELD...
           MOVE MODIFY-SEQ TO PASSWDW-SEQ.
           MOVE "TO ALLOW MODIFY:" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              GO TO MODIFY-ROUTINE.

       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.

           MOVE " F1-CANCEL, ENTER FIELD INFORMATION:" TO LEGEND.
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
       TEST-F6.
       TEST-F7.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *          SET RECORD ELEMENTS
      *****************************************************************
       F-01.
           MOVE VDUBUF TO LSR-SCRA-ACTIVE.
           IF LSR-SCRA-ACTIVE NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDU-DATE-YYYYMMDD TO LSR-SCRA-START-DATE.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDU-DATE-YYYYMMDD TO LSR-SCRA-END-DATE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO LSR-SCRA-BORROWER.
           IF LSR-SCRA-BORROWER NOT = "M" AND NOT = "C" AND NOT = "B"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ****************************
       WRITE-MODULE SECTION.
      ****************************
           PERFORM OPEN-LSR1-FILE.

           MOVE LSR-REC TO LSR-CH-REC.

           IF LSR-OR-REC = LSR-CH-REC
              GO TO END-WRITE-MODULE.

           IF LSR-OR-REC = SPACES
              GO TO WRITE-RECORD.

       REWRITE-RECORD.
           PERFORM READ-LSR1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LSR-REC NOT = LSR-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE LSR-CH-REC TO LSR-REC.

           PERFORM REWRITE-LSR1-FILE.

           PERFORM CREATE-CHANGE-RECORD.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LSR1-FILE.

           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.


      ****************************
       DELETE-MODULE SECTION.
      ****************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LSR1-FILE.
           PERFORM READ-LSR1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF LSR-REC NOT = LSR-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LSR1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-LSR1-FILE.

      ****************************
       NEW-RECORD-SETUP SECTION.
      ****************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LSR-FILE.
           MOVE HOLD-LSR1-KEY TO LSR1-KEY.

       NEW-RECORD-EXIT.
           EXIT.

      ***************************
       DISPLAY-FIELDS SECTION.
      ***************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE LSR-SCRA-ACTIVE   TO D-01.
           MOVE LSR-SCRA-START-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-02.
           MOVE LSR-SCRA-END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-03.
           MOVE LSR-SCRA-BORROWER TO D-04.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ***************************************************
       CREATE-CHANGE-RECORD SECTION.
      ***************************************************

           PERFORM OPEN-CH-FILE.
           PERFORM OPEN-LM1-FILE.

           IF  (LSR-OR-SCRA-ACTIVE NOT = LSR-SCRA-ACTIVE)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "SCRA ACTIVE" TO CH-DESCRIPT
               MOVE LSR-OR-SCRA-ACTIVE TO CH-ORG-CHAR
               MOVE LSR-SCRA-ACTIVE TO CH-NEW-CHAR
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSR-OR-SCRA-START-DATE NOT = LSR-SCRA-START-DATE)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "SCRA START DATE" TO CH-DESCRIPT
               MOVE LSR-OR-SCRA-START-DATE TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY       TO CH-ORG-DATE
               MOVE LSR-SCRA-START-DATE TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY       TO CH-NEW-DATE
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSR-OR-SCRA-END-DATE NOT = LSR-SCRA-END-DATE)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "SCRA END DATE" TO CH-DESCRIPT
               MOVE LSR-OR-SCRA-END-DATE TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY     TO CH-ORG-DATE
               MOVE LSR-SCRA-END-DATE TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY     TO CH-NEW-DATE
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSR-OR-SCRA-BORROWER NOT = LSR-SCRA-BORROWER)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "SCRA BORROWER" TO CH-DESCRIPT
               MOVE LSR-OR-SCRA-BORROWER TO CH-ORG-CHAR
               MOVE LSR-SCRA-BORROWER TO CH-NEW-CHAR
               PERFORM UPDATE-CH-LM-FILE.

           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-LM1-FILE.

      ************************************
       UPDATE-CH-LM-FILE SECTION.
      ************************************
       UPDATE-CH-AGAIN.
           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF CH-SEQ < 999
                 ADD 1 TO CH-SEQ
                 GO TO UPDATE-CH-AGAIN.

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

       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPLSRCL.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "CHSCRA" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON CHSCRA-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY CHSCRA-SCREEN UPON CHSCRA-WINDOW-HANDLE.
           MOVE CHSCRA-FORM-FIELDS TO WR-BUF.
           MOVE CHSCRA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE CHSCRA-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHSCRA-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CHSCRA_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************
       MISC-PARAGRAPHS SECTION.
      ****************************
       COPY "LIBGB/ACCESS.CPY".
       CREATE-SETUP-CH-REC.
           PERFORM CLEAR-CH-FILE.
           MOVE EXT-CONAME-USER-LOC TO CH-BRNO.
           MOVE HOLD-SYS-DATE       TO CH-DATE.
           MOVE "Z"                 TO CH-CODE.
           MOVE EXT-CONAME-USER-ID  TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS            TO CH-TIME.
           MOVE 1                   TO CH-SEQ.

           MOVE LN-ACCTNO TO CH-ACCTNO.
      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE LEGEND TO VDUBUF.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LSR1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLSRGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLSR1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHSCRA-SCREEN.
           DESTROY CHSCRA-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
