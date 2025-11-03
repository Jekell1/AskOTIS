       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PTHMOD.
       DATE-WRITTEN.   09/14/05.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   PAID THRU (DEFERMENT) MODIFICATION
      *                     APPROVAL WINDOW
      *                 CALLED FROM F9 INTERRUPT IF GP-DEFERMENT-APPROVAL
      *
      *         WRITTEN AND PAID FOR BY LENDMARK
      *
      *  REV:
      * BAH 050914 LENDMARK PR# 2568
      * BAH 160128 REMOVED LTFILE, PD0003
      * BAH 160209 SPLIT OUT LSFILE, PD0003
      * KEC 2016.0216 FIXED KEYS & RECS FOR FD-REC.
      *               CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * BAH 2016.0218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      * KEC 2016.0328 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * BAH 2017.0421 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2019.0128 SPLIT CDFILE
      * BAH 2022.0803 HARD CODED START
      * BAH 2024.0417 REMOVED PTHMODW, CALL INSTEAD OF WINDOW, NOTHING TO PASS
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
           "@(#)WI/PTHMOD.CBL S35 02/23/24-11:40:20 >".
      ******************************************************************
      *
      *     P R O G R A M    W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01LSP.CPY".
       COPY "LIBLP/LP01LSP_SQL.CPY".
       COPY "LIBLP/LPWSLSP.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
      *-----------------------------------------------------------------

       01  PTHMOD-LINES              PIC 99  VALUE 22.
       01  PTHMOD-COLUMNS            PIC 99  VALUE 78.
       01  PTHMOD-BEGIN-Y            PIC 99  VALUE 2.
       01  PTHMOD-BEGIN-X            PIC 99  VALUE 2.

       01  ERRCD           PIC X VALUE " ".
       01  LEGEND          PIC X(70).
       01  LEGEND-01.
           03  FILLER       PIC X(37)  VALUE
           "F1-RESET, F7-UPDATE, ENTER FIELD #:".

       01  TEST-TRCD           PIC XX    VALUE "  ".
           88  DF-POSTING             VALUE "DF" "D2" "D3" "D4"
                                            "D5" "D6" "D7" "D8"
                                            "D9".
      *-----------------------------------------------------------------
       01  BRANCH            PIC 9(4)  VALUE 0.
       01  CHAN              PIC X(4)  VALUE "CHAN".
       01  ONE               PIC S9.
       01  ELE               PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).

       01  NEW-FLAG          PIC 9.

       01  HOLD-LSP1-KEY     PIC 9(14).

       01  HOLD-SYS-DATE          PIC 9(8).
       01  HOLD-SYS-DATE-MMDDYY   PIC 9(6).

       01  HOLD-DF-MEMO.
           03  FILLER             PIC X(10).
           03  HOLD-DF-TEXT       PIC X(50).
       01  HOLD-DF-MEMO1 REDEFINES HOLD-DF-MEMO.
           03  DF-MEMO1           PIC X(30).
           03  DF-MEMO2           PIC X(30).
       01  HOLD-DF-MEMO2 REDEFINES HOLD-DF-MEMO.
           03  DF-MEMO3           PIC X(42).
           03  DF-MEMO4           PIC X(18).
       01  HOLD-MEMO-AMOUNT       PIC Z(5)99.99.
       01  HOLD-MEMO-DATE         PIC 99/99/99.

      * ORIGINAL RECORD
           COPY "LIBLP/LP01LSP.CPY" REPLACING
                  LEADING "LSP" BY "LSP-OR".
      * CHANGED RECORD
           COPY "LIBLP/LP01LSP.CPY" REPLACING
                  LEADING "LSP" BY "LSP-CH".


      *-----------------------------------------------------------------


       01  DISPLAY-BUF1      VALUE SPACES.
           03  D-NUMBER          PIC Z(6).
           03  D-NAME            PIC X(30).
           03  D-BALANCE         PIC ZZZZZ9.99.
           03  D-LAST-DATE       PIC 99/99/99 BLANK ZERO.
           03  D-LAST-REASON     PIC XX.

       01  DISPLAY-LIST1.
           03  FILLER      PIC X(42) VALUE
           "ACCTNO NAME BALANCE DATE-LAST LAST-REASON.".

       01  DISPLAY-BUF2      VALUE SPACES.
           03  D-DATE       PIC 99/99/99 BLANK ZERO.
           03  D-USERID     PIC X(10).


       01  DISPLAY-BUF      VALUE SPACES.
      * REASON CODE
           03  D-01              PIC XX.
      * # OF MONTHS TO DEFER
           03  D-02              PIC Z9.
      * REASON FOR MODIFICATION
           03  D-03              PIC X(50).
      * IS 1ST MORTGAGE PAST DUE
           03  D-04              PIC X.
      * 1ST MORTGAGE BAL
           03  D-05              PIC ZZZZZ9.99.
      * ATTEMPTS TO REFINANCE
           03  D-06              PIC X(50).
           03  D-BR-DATE         PIC 99/99/99 BLANK ZERO.
           03  D-BR-LOGIN        PIC X(10).
      * BRANCH MANAGER COMMENTS
           03  D-07              PIC X(50).
      * AVP APPROVE
           03  D-08              PIC X.
           03  D-AVP-DATE         PIC 99/99/99 BLANK ZERO.
           03  D-AVP-LOGIN        PIC X(10).
      * AVP COMMENTS
           03  D-081             PIC X(50).
      * VP APPROVE
           03  D-09              PIC X.
           03  D-VP-DATE         PIC 99/99/99 BLANK ZERO.
           03  D-VP-LOGIN        PIC X(10).
      * VP COMMENTS
           03  D-091              PIC X(50).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(49) VALUE
           "010 020 030 040 050 060 BR-DATE BR-LOGIN 070 080 ".
           03  FILLER      PIC X(48) VALUE
           "AVP-DATE AVP-LOGIN 081 090 VP-DATE VP-LOGIN 091.".

      ******************************************************************
      *      ELEMENT SPECIFICATIONS
      ******************************************************************
       01  SPEC-TABLE.
      * 1 REASON CODE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 2 # OF MONTHS TO DEFER
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 3 REASON FOR MODIFICATION
           03  FILLER      PIC X(8) VALUE "E  A500 ".
      * 4 1ST MORTGAGE PAST DUE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 5 1ST MORTGAGE BALANCE
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 6 ATTEMPTS TO REFINANCE
           03  FILLER      PIC X(8) VALUE "E  A500 ".
      * 7  BRANCH MANAGER COMMENTS
           03  FILLER      PIC X(8) VALUE "E  A500 ".
      * 8  AVP APPROVE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 9 VP APPROVE
           03  FILLER      PIC X(8) VALUE "E  A010 ".

       01  SPEC-TAB      REDEFINES SPEC-TABLE.
           03  SPEC-REC  OCCURS 9 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       01  MAX           PIC 99   VALUE 9.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/PASSWDW.CPY".
       01  BR-APPROVAL-SEQ         PIC 99 VALUE 01.
       01  AVP-APPROVAL-SEQ        PIC 99 VALUE 02.
       01  VP-APPROVAL-SEQ         PIC 99 VALUE 03.

       01  PTHMOD-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/PTHMOD_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/PTHMOD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/PTHMOD_SCN.CPY".


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
           MOVE "PTHMOD" TO VDU-FORM-NAME.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT  SCREEN LINE PTHMOD-BEGIN-X * VDU-WIN-L-OFFSET
               AT  SCREEN COL  PTHMOD-BEGIN-Y * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE    PTHMOD-COLUMNS + VDU-WIN-W-MOD
               LINES   PTHMOD-LINES + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS PTHMOD-WINDOW-HANDLE.

           MOVE "S  A400TITLE:FR." TO VDU.
           MOVE "PAID THRU DATE MODIFICATION"  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HOLD-SYS-DATE-MMDDYY.

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

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      **************************
       REC-LS-MODULE SECTION.
      **************************
           MOVE SPACES TO IO-TYPE LSP-OR-REC.
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

      * AMOUNT OWED AT DATE OF REPO
           MOVE LN-CURBAL TO D-BALANCE.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.
           PERFORM START-LP1-FILE-NOT-GREATER.
       NEXT-LP.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0 OR LP-BRNO NOT = LN-OWNBR OR
                                LP-ACCTNO NOT = LN-ACCTNO
              NEXT SENTENCE
           ELSE
              MOVE LP-TRCD TO TEST-TRCD
              IF DF-POSTING AND LP-REV NOT = "Y"
                 MOVE LP-REFNO TO D-LAST-REASON
                 MOVE LP-TRDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO D-LAST-DATE
              ELSE
                 GO TO NEXT-LP.

           PERFORM OPEN-LSP1-FILE.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE EXT-WI-CL-OWNBR  TO LSP-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LSP-ACCTNO.
           MOVE 1                TO LSP-SEQNO.
           MOVE LSP1-KEY TO HOLD-LSP1-KEY.

           PERFORM READ-LSP1-FILE.
           PERFORM CLOSE-LSP1-FILE.
           IF IO-FG = 0
              MOVE LSP-REC TO LSP-OR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              GO TO EXIT-REC-LS-MODULE.

           PERFORM NEW-RECORD-SETUP.

       EXIT-REC-LS-MODULE.
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
      * ONLY ALLOW NEW RECORD ENTRY UP TO FIELD 6.
      * THE REST GET FILLED IN ANOTHER TIME BY MODIFYING....
           IF ELE NOT > 6
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

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.
           IF ELE = 7
              MOVE BR-APPROVAL-SEQ TO PASSWDW-SEQ
              MOVE "FOR BRANCH APPROVAL:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.
           IF ELE = 8
              MOVE AVP-APPROVAL-SEQ TO PASSWDW-SEQ
              MOVE "FOR AVP APPROVAL:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.
           IF ELE = 9
              MOVE VP-APPROVAL-SEQ TO PASSWDW-SEQ
              MOVE "FOR VP APPROVAL:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

           IF ELE = 1 OR 2 OR 3
              IF LSP-AVP-APPROVE = "Y" OR LSP-VP-APPROVE = "Y"
                 MOVE "YOU CAN NOT CHANGE THESE FIELDS AFTER APPROVAL"
                       TO MESS
                 PERFORM SEND-MESS
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
           IF VDUSTATUS = "1<"
              IF (ELE = 1 )
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    MOVE "S  A020   ." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE 0    TO VDUCOL
                    MOVE CDSCAN-CODE TO VDUBUF
                    PERFORM SCREENIO
                    MOVE CDSCAN-CODE TO VDUBUF
                    MOVE 1 TO ONE
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE.
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
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *          SET RECORD ELEMENTS
      *****************************************************************
       F-01.
           MOVE VDUBUF TO LSP-REASON-CODE.
           PERFORM DISPLAY-REASON-DESC.
           IF IO-FG NOT = 0
              MOVE "INVALID REASON CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM0 TO LSP-PERIODS-TO-DEFER.
           IF VDUNUM0 > 9
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO LSP-REASON-FOR-DEFER.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO LSP-MORTGAGE-PAST-DUE.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO LSP-MORTGAGE-BALANCE.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO LSP-ATTEMPT-REFINANCE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO LSP-BR-MANAGER-COMMENTS.
           MOVE HOLD-SYS-DATE TO LSP-BR-MANAGER-DATE.
           MOVE HOLD-SYS-DATE-MMDDYY TO D-DATE.
           MOVE EXT-CONAME-USER-ID TO LSP-BR-MANAGER-USERID D-USERID.
           MOVE "BR-DATE BR-LOGIN." TO WR-LIST.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           PERFORM CALL-WRFORM.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO LSP-AVP-APPROVE.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           MOVE HOLD-SYS-DATE TO LSP-AVP-DATE.
           MOVE HOLD-SYS-DATE-MMDDYY TO D-DATE.
           MOVE EXT-CONAME-USER-ID TO LSP-AVP-USERID D-USERID.
           MOVE "AVP-DATE AVP-LOGIN." TO WR-LIST.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           PERFORM CALL-WRFORM.

       F-081.
           MOVE "E  A500081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-081.
           MOVE VDUBUF TO LSP-AVP-COMMENTS.
           IF LSP-AVP-COMMENTS = SPACES
              IF LSP-AVP-APPROVE = "N"
                 MOVE "MUST ENTER COMMENT" TO MESS
                 PERFORM SEND-MESS
                 GO TO F-081.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUBUF TO LSP-VP-APPROVE.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           MOVE HOLD-SYS-DATE TO LSP-VP-DATE.
           MOVE HOLD-SYS-DATE-MMDDYY TO D-DATE.
           MOVE EXT-CONAME-USER-ID TO LSP-VP-USERID D-USERID.
           MOVE "VP-DATE VP-LOGIN." TO WR-LIST.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           PERFORM CALL-WRFORM.
       F-091.
           MOVE "E  A500091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-091.
           MOVE VDUBUF TO LSP-VP-COMMENTS.
           IF LSP-VP-COMMENTS = SPACES
              IF LSP-VP-APPROVE = "N"
                 MOVE "MUST ENTER COMMENT" TO MESS
                 PERFORM SEND-MESS
                 GO TO F-091.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ****************************
       WRITE-MODULE SECTION.
      ****************************
           PERFORM OPEN-LSP1-FILE.

           MOVE LSP-REC TO LSP-CH-REC.

           IF LSP-OR-REC = LSP-CH-REC
              GO TO EXIT-WRITE-MODULE.

           IF LSP-OR-REC = SPACES
              GO TO WRITE-RECORD.

       REWRITE-RECORD.
           PERFORM READ-LSP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO EXIT-WRITE-MODULE.

           IF LSP-REC NOT = LSP-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO EXIT-WRITE-MODULE.

           MOVE LSP-CH-REC TO LSP-REC.

           PERFORM REWRITE-LSP1-FILE.

      * VP IS ALWAYS FINAL SAY
           IF LSP-VP-APPROVE = "N" AND LSP-VP-COMMENTS NOT = SPACES
              PERFORM DELETE-LS-CREATE-MEMO.

      * IF NOT REAL ESTATE AND THE VP DECLINED IT, REMOVE THE TRAILER AND
      * CREATE MEMOS ABOVE ^
           IF (LN-SMALLOAN = "N" AND LN-SALEFIN = "N")
              NEXT SENTENCE
           ELSE
      * IF NOT REAL ESTATE AND ASSISTANT VP DECLINED IT, REMOVE THE TRAILER
      * AND CREATE MEMOS
              IF LSP-AVP-APPROVE = "N" AND
                      LSP-AVP-COMMENTS NOT  = SPACES
                 IF LSP-VP-APPROVE NOT = "Y"
                    PERFORM DELETE-LS-CREATE-MEMO.


           GO TO EXIT-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LSP1-FILE.

           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.


       EXIT-WRITE-MODULE.

      ****************************
       NEW-RECORD-SETUP SECTION.
      ****************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LSP-FILE.
           MOVE HOLD-LSP1-KEY TO LSP1-KEY.

           MOVE HOLD-SYS-DATE TO LSP-INFO-DATE.
           MOVE HOLD-SYS-DATE-MMDDYY TO D-DATE.
           MOVE EXT-CONAME-USER-ID TO D-USERID LSP-INFO-USERID.
           MOVE "INFO-DATE INFO-USERID." TO WR-LIST.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           PERFORM CALL-WRFORM.
       NEW-RECORD-EXIT.
           EXIT.

      ***************************
       DISPLAY-FIELDS SECTION.
      ***************************
           MOVE LSP-INFO-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE LSP-INFO-USERID TO D-USERID.
           MOVE "INFO-DATE INFO-USERID." TO WR-LIST.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE SPACES TO DISPLAY-BUF.

           MOVE LSP-REASON-CODE         TO D-01.
           PERFORM DISPLAY-REASON-DESC.
           MOVE LSP-PERIODS-TO-DEFER    TO D-02.
           MOVE LSP-REASON-FOR-DEFER    TO D-03.
           MOVE LSP-MORTGAGE-PAST-DUE   TO D-04.
           MOVE LSP-MORTGAGE-BALANCE    TO D-05.
           MOVE LSP-ATTEMPT-REFINANCE   TO D-06.
           MOVE LSP-BR-MANAGER-COMMENTS TO D-07.
           MOVE LSP-BR-MANAGER-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO D-BR-DATE.
           MOVE LSP-BR-MANAGER-USERID   TO D-BR-LOGIN.

           MOVE LSP-AVP-APPROVE  TO D-08.
           MOVE LSP-AVP-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-AVP-DATE.
           MOVE LSP-AVP-USERID   TO D-AVP-LOGIN.
           MOVE LSP-AVP-COMMENTS TO D-081.

           MOVE LSP-VP-APPROVE  TO D-09.
           MOVE LSP-VP-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO D-VP-DATE.
           MOVE LSP-VP-USERID   TO D-VP-LOGIN.
           MOVE LSP-VP-COMMENTS TO D-091.


            MOVE DISPLAY-BUF TO WR-BUF.
            MOVE DISPLAY-LIST TO WR-LIST.
            PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.


      ***********************************
       DELETE-LS-CREATE-MEMO SECTION.
      ***********************************

           PERFORM OPEN-LM1-FILE.
           MOVE 0 TO LM-SEQ.
           PERFORM OPEN-LSP1-FILE.

           MOVE LN-OWNBR  TO LSP-BRNO.
           MOVE LN-ACCTNO TO LSP-ACCTNO.
           MOVE 1         TO LSP-SEQNO.
           PERFORM READ-LSP1-FILE.
           IF (IO-BAD)
              GO TO EXIT-UPDATE-DEFERMENT-APPROVAL.

           MOVE SPACES TO HOLD-DF-MEMO.
           STRING "REASON CODE: ",LSP-REASON-CODE DELIMITED
               BY "   " INTO DF-MEMO1.
           STRING "# OF MONTHS: ",LSP-PERIODS-TO-DEFER
               DELIMITED BY "   " INTO DF-MEMO2.

           PERFORM UPDATE-DF-MEMO.

           IF LSP-REASON-FOR-DEFER NOT = SPACES
              MOVE "REASON:" TO HOLD-DF-MEMO
              MOVE LSP-REASON-FOR-DEFER TO HOLD-DF-TEXT
              PERFORM UPDATE-DF-MEMO.

           STRING "MORTGAGE PAST DUE: ",LSP-MORTGAGE-PAST-DUE
                DELIMITED BY "   " INTO DF-MEMO1.
           MOVE LSP-MORTGAGE-BALANCE TO HOLD-MEMO-AMOUNT.
           STRING "MORTGAGE BALANCE: ",HOLD-MEMO-AMOUNT
               DELIMITED BY "      " INTO DF-MEMO2.
           PERFORM UPDATE-DF-MEMO.

           IF LSP-ATTEMPT-REFINANCE NOT = SPACES
              MOVE "ATTEMPT:" TO HOLD-DF-MEMO
              MOVE LSP-ATTEMPT-REFINANCE TO HOLD-DF-TEXT
              PERFORM UPDATE-DF-MEMO.

           STRING "WHO ENTERED INFO: ",LSP-INFO-USERID
                DELIMITED BY "   " INTO DF-MEMO1.
           MOVE LSP-INFO-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HOLD-MEMO-DATE.
           STRING "DATE ENTERED INFO: ",HOLD-MEMO-DATE
               DELIMITED BY "  " INTO DF-MEMO2.
           PERFORM UPDATE-DF-MEMO.

           IF LSP-BR-MANAGER-COMMENTS NOT = SPACES
              MOVE "MGR COMM:" TO HOLD-DF-MEMO
              MOVE LSP-BR-MANAGER-COMMENTS TO HOLD-DF-TEXT
              PERFORM UPDATE-DF-MEMO.

           STRING "USERID MANAGER: ",LSP-BR-MANAGER-USERID
                DELIMITED BY "   " INTO DF-MEMO1.
           MOVE LSP-BR-MANAGER-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HOLD-MEMO-DATE.
           STRING "DATE MANAGER: ",HOLD-MEMO-DATE
               DELIMITED BY "  " INTO DF-MEMO2.
           PERFORM UPDATE-DF-MEMO.

           IF LSP-AVP-COMMENTS NOT = SPACES
              MOVE "AVP COMM:" TO HOLD-DF-MEMO
              MOVE LSP-AVP-COMMENTS TO HOLD-DF-TEXT
              PERFORM UPDATE-DF-MEMO.

           STRING "AVP APPROVED: ",LSP-AVP-APPROVE,
                  " USERID AVP: ",LSP-AVP-USERID
                         DELIMITED BY "   " INTO DF-MEMO3.
           MOVE LSP-AVP-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HOLD-MEMO-DATE.
           STRING "DATE AVP: ",HOLD-MEMO-DATE DELIMITED BY
                                        "  " INTO DF-MEMO4.
           PERFORM UPDATE-DF-MEMO.

           IF LSP-VP-COMMENTS NOT = SPACES
              MOVE "VP COMM:" TO HOLD-DF-MEMO
              MOVE LSP-VP-COMMENTS TO HOLD-DF-TEXT
              PERFORM UPDATE-DF-MEMO.

           STRING "VP APPROVED: ",LSP-VP-APPROVE,
                  " USERID VP: ",LSP-VP-USERID
                         DELIMITED BY "   " INTO DF-MEMO3.
           MOVE LSP-VP-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HOLD-MEMO-DATE.
           STRING "DATE VP: ",HOLD-MEMO-DATE DELIMITED BY
                                       "  " INTO DF-MEMO4.
           PERFORM UPDATE-DF-MEMO.

      * REMOVE TRAILER RECORD AFTER CREATING ALL MEMOS

           PERFORM DELETE-LSP1-FILE.

       EXIT-UPDATE-DEFERMENT-APPROVAL.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LSP1-FILE.

      **************************
       UPDATE-DF-MEMO SECTION.
      **************************
           MOVE LN-OWNBR  TO LM-BRNO.
           MOVE LN-ACCTNO TO LM-ACCTNO.
           SUBTRACT HOLD-SYS-DATE FROM 999999999 GIVING LM-DATE.

           MOVE HOLD-DF-MEMO TO LM-MEMO.
           ADD 1 TO LM-SEQ.
       CREATE-DF-MEMO-AGAIN.
           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              IF LM-SEQ NOT = 999
                 ADD 1 TO LM-SEQ
                 GO TO CREATE-DF-MEMO-AGAIN.

           MOVE SPACES TO HOLD-DF-MEMO.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPLSPCL.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************
       CODE-SCAN SECTION.
      *******************
           MOVE "DF"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "PTHMOD" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON PTHMOD-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY PTHMOD-SCREEN UPON PTHMOD-WINDOW-HANDLE.
           MOVE PTHMOD-FORM-FIELDS TO WR-BUF.
           MOVE PTHMOD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE PTHMOD-HELP--FILE TO HELP-LINK-FILE.
           MOVE PTHMOD-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/PTHMOD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************
       MISC-PARAGRAPHS SECTION.
      ****************************
       COPY "LIBGB/ACCESS.CPY".
       DISPLAY-REASON-DESC.
           MOVE "DF"            TO CD-TYPE.
           MOVE LSP-REASON-CODE TO CD-CODE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           MOVE "S  A200011." TO VDU.
           IF IO-FG = 0
              MOVE CD-DESC TO VDUBUF
           ELSE
              MOVE "*** INVALID ***" TO VDUBUF.
           PERFORM SCREENIO.
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
           PERFORM CLOSE-LSP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLSPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLSP1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PTHMOD-SCREEN.
           DESTROY PTHMOD-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
