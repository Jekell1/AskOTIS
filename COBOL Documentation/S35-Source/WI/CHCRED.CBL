       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHCRED.
       DATE-WRITTEN.    110788.
      ******************************************************************
      *
      *                 (WI)
      *  DESCRIPTION:   CHANGE HIGH CREDIT  * WINDOW VERSION  *
      *
      *                 CALLED FROM CL/CLMENU F9
      *
      * REV:
      * 091020 BAH WORLD PR# 659
      * 110713 BAH IF NOT AVAILABLE, YOU WERE CAUGHT IN A LOOP, FIXED FOR
      *            FRANEK, PL# 742
      * KEC 2017.0530 ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 2019.0513 NEW CHFILE
      * BAH 2024.0131 CALL CRNOR2 AND WRITE NOTICE RECORD
      * BAH 2024.0417 REMOVED CHCREDW, CALL NOT WINDOW, NOTHING TO PASS
      ******************************************************************
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
           "@(#)WI/CHCRED.CBL S35 04/17/24-12:57:52 >".

       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  CHCRED-LINES            PIC 9(02)     VALUE 07.
       01  CHCRED-COLUMNS          PIC 9(02)     VALUE 75.
       01  CHCRED-BEGIN-Y          PIC 9(02)     VALUE 4.
       01  CHCRED-BEGIN-X          PIC 9(02)     VALUE 07.

       01  ONE               PIC S9.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  NEW-FLAG          PIC 9       VALUE 1.
       01  IO-TYPE           PIC X(3).
       01  ELE               PIC 9(2).
       01  ERRCD             PIC X.
       01  CHAN              PIC X(4).
       01  BRANCH            PIC 9(4).
       01  ORIGINAL-LN-REC   PIC X(LN-SIZE).
       01  CHANGED-LN-REC    PIC X(LN-SIZE).

       01  LEGEND                      PIC X(43)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  HOLD-CREDITLIM    PIC 9(6)  COMP-3 VALUE 0.
       01  HOLD-CH-SEQ       PIC 9(3).

      **********************************
      *    ELEMENT SPECIFICATIONS
      **********************************
       01  MAX               PIC 99   VALUE 1.
       01  SPEC-TABLE.
      *01. NEW HIGH CREDIT AMOUNT
           03  FILLER        PIC X(8) VALUE "EN N060 ".
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 1 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.


       01  DISPLAY-BUF  VALUE " ".
           03  D-HICR        PIC Z(5)9.

       01  DISPLAY-LIST.
           03  FILLER        PIC X(6) VALUE "OHICR.".


       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01  CHCRED-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CHCRED_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/CHCRED_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CHCRED_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ********************************
      *    MAIN PROGRAM MODULE
      ********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE "CHCRED" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE SPACES        TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BRANCH.

           MOVE SPACES TO ERRCD.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE CHCRED-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  CHCRED-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    CHCRED-COLUMNS + VDU-WIN-W-MOD
                      LINES   CHCRED-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS CHCRED-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF BR-AVAIL-CREDIT NOT = "Y" AND NOT = "S"
              MOVE "HIGH CREDIT OPTION NOT AVAILABLE" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           GO TO END-ROUTINE.


      ***********************************
       ENTER-KEYS SECTION.
      ***********************************

           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-BAD
              MOVE "END" TO IO-TYPE
              MOVE "INVALID ACCOUNT NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-EXIT.

           IF ( LN-POCD-VOID )
              MOVE "END" TO IO-TYPE
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-EXIT.

           IF ( LN-POCD-INRESCISSION )
              MOVE "END" TO IO-TYPE
              MOVE "ACCOUNT IS IN RESCISSION" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-EXIT.

           IF ( LN-POCD-RESCIND )
              MOVE "END" TO IO-TYPE
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-EXIT.

           MOVE LN-REC TO ORIGINAL-LN-REC.

           MOVE SPACES TO MESS.
           PERFORM DISPLAY-FIELDS.

           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE "CHANGES WILL NOT BE ALLOWED" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO CHFG(1).

       ENTER-KEYS-EXIT.
           EXIT.

      *******************************************
       VERIFY-PASSWORD SECTION.
      *******************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "CHCRED" TO PASSWDW-PGM.
           MOVE 0 TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO CHANGE HIGH CREDIT...." TO PASSWDW-PROMPT2
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


      *************************************************
      *    ENTRY-MODULE
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *************************************************
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
           MOVE "ENTER FIELD #, F1-CANCEL, F7-UPDATE/EXIT" TO LEGEND.
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

           MOVE SPEC (ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
       TEST-UP-KEY.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.
       TEST-DOWN-KEY.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01.

       F-01.
           MOVE VDUNUM0 TO LN-CREDITLIM.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ****************************
       WRITE-MODULE SECTION.
      ****************************
           PERFORM OPEN-LN1-FILE.

           MOVE LN-REC TO CHANGED-LN-REC.
           IF ORIGINAL-LN-REC = CHANGED-LN-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LN1-FILE.
           IF NOT IO-GOOD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LN-REC NOT = ORIGINAL-LN-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-LN-REC TO LN-REC.
           PERFORM REWRITE-LN1-FILE.

           PERFORM UPDATE-NOTICE-FILE.

      * CREATE A CHANGE-RECORD
           PERFORM CREATE-CHANGE-REC.
           IF BR-CHANGE-MEMO = "Y"
              MOVE "CL/CHMEMO" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CH-REC
              CANCEL FORM-PROGX.

       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           ACCEPT CRNOR2-SYSDATE FROM CENTURY-DATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

           PERFORM CLOSE-NO1-FILE.
       EXIT-UPDATE-NOTICE-FILE.
           EXIT.

      ******************************
       DISPLAY-FIELDS SECTION.
      ******************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE LN-CREDITLIM TO D-HICR HOLD-CREDITLIM.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************
       CREATE-CHANGE-REC SECTION.
      ******************************************
           PERFORM GETDAT.

           PERFORM OPEN-CH-FILE.

           MOVE 1                  TO HOLD-CH-SEQ.

       CREATE-CHANGE-REC-AGAIN.
           PERFORM CLEAR-CH-FILE.

           MOVE BRANCH             TO CH-BRNO.
           MOVE SYS-DATE           TO CH-DATE.
           MOVE "H"                TO CH-CODE.
           MOVE EXT-CONAME-USER-ID TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS           TO CH-TIME.
           MOVE HOLD-CH-SEQ        TO CH-SEQ.

           MOVE "HIGH CREDIT LIMIT CHANGE" TO CH-DESCRIPT.
           MOVE LN-ACCTNO                  TO CH-ACCTNO.

           MOVE HOLD-CREDITLIM TO CH-ORG-NODEC.
           MOVE LN-CREDITLIM   TO CH-NEW-NODEC.
           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF HOLD-CH-SEQ < 999
                 ADD 1 TO HOLD-CH-SEQ
                 GO TO CREATE-CHANGE-REC-AGAIN.

           PERFORM CLOSE-CH-FILE.
       EXIT-CREATE-CHANGE-REC.
           EXIT.

       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON CHCRED-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY CHCRED-SCREEN UPON CHCRED-WINDOW-HANDLE.
           MOVE CHCRED-FORM-FIELDS TO WR-BUF.
           MOVE CHCRED-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE CHCRED-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHCRED-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CHCRED_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************
       MISC-PARAGRAPHS SECTION.
      ********************************
       COPY "LIBGB/GETDAT.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-NO1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPCHIN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHCRED-SCREEN.
           DESTROY CHCRED-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
