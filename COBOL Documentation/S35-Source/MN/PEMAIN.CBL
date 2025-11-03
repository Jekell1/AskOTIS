       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PEMAIN.
       DATE-WRITTEN.    10-19-07.
      ******************************************************************
      *
      *                 (MN)
      *
      *  DESCRIPTION:   PASSWORD EXCEPTION FILE MAINTENANCE
      *                 REGACC PR# 188
      *
      *=================================================================
      * REV:
      * 130802 BAH WAS NOT ALLOWING ENTRY OF A SEQUENCE 0, PCCREDIT PL#804
      *
      * KEC 2015.1201 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED TO NOT USE PE-DIR ANYMORE.
      *         INCREASED PE-SEQ TO BE 3 BYTES.
      *  BAH 20220218 CALL NEW AUDIT/CHANGE PROGRAM, AUDITW, #1542
      *               REMOVED CHANGES LOG ON NEW AND DELETED RECORDS
      * BAH 2023.1006 CHANGED SCREEN FIELD USERID TO MUST Y TO ALLOW
      *               LOWERCASE
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/PEMAIN.CBL S34 06/06/21-16:33:10 >".

      ******************************************************************
      *
      *    F I L E - I F N
      *
      ******************************************************************
       COPY "LIBGB/GB01PW.CPY".
       COPY "LIBGB/GB01PW_SQL.CPY".
       COPY "LIBGB/GBWSPW.CPY".
       COPY "LIBGB/GB01PE.CPY".
       COPY "LIBGB/GB01PE_SQL.CPY".
       COPY "LIBGB/GBWSPE.CPY".
      ******************************************************************
      *
      *    P R O G R A M - W O R K E R S
      *
      ******************************************************************
       01  ERRCD           PIC X        VALUE SPACES.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).


       01  IO-TYPE         PIC X(3)     VALUE SPACES.
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9        VALUE 0.

       01  HOLD-PE1-KEY.
           03  HOLD-PW-KEY.
               05  HOLD-PE-PGM     PIC X(10).
               05  HOLD-PE-SEQ     PIC 9(03).
           03  HOLD-PE-LOGIN       PIC X(8).

       01  WS-HOLD-PE1-KEY.
           03  WS-HOLD-PE-PGM     PIC X(10).
           03  WS-HOLD-PE-SEQ     PIC 9(03).
           03  WS-HOLD-PE-LOGIN   PIC X(8).

       01  ORIGINAL-PE-REC        PIC X(PE-SIZE)   VALUE SPACES.
       01  CHANGED-PE-REC         PIC X(PE-SIZE)   VALUE SPACES.

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(4) VALUE
           "010.".
           03  FILLER      PIC X(8) VALUE
           "090 100.".

      ******************************************************************
      *    E L E M E N T - S P E C I F I C A T I O N S
      ******************************************************************
       01  MAX             PIC 99   VALUE 0.

       01  SPEC-TABLE.
      * 1. DUMMY
           03  FILLER      PIC X(8) VALUE "E  A010 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 1 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      ****************************************************************
      *
      *     K E Y   B U F F E R
      *
      ****************************************************************
       01  KEY-BUF.
           03  D-PGM       PIC X(10).
           03  D-SEQ       PIC ZZ9.
           03  D-LOGIN     PIC X(8).

       01  KEY-LIST.
           03  FILLER      PIC X(18)   VALUE
               "DIR PGM SEQ LOGIN.".

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/AUDITWW.CPY".
       COPY "LIBWI/PWSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  PEMAIN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PEMAIN_DEF.CPY".
       COPY "LIBMN/PEMAIN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/PEMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "PEMAIN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS PEMAIN-WINDOW-HANDLE.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-PE-MODULE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-PE-MODULE SECTION.
      ******************************************************************
       REC-PE-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

       REC-PE-PGM.
           MOVE "ENTER PROGRAM, F5-SCAN PASSWORDS, F6-SCAN EXCEPTIONS"
              TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A100PGM." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-PE-MODULE.

           IF VDUSTATUS = "1:"
              MOVE SPACES TO PWSCANW-PGM
              MOVE "WI/PWSCAN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH PWSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF PWSCAN-STATUS NOT = "00"
                 GO TO REC-PE-PGM
              ELSE
                 MOVE PWSCAN-PW1-KEY TO PE-PW-KEY HOLD-PW-KEY
                 MOVE SPACES         TO HOLD-PE-LOGIN
                 PERFORM DISPLAY-KEY
                 GO TO READ-PWFILE.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-PE-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-PE-MODULE-NO.

           IF VDUBUF = SPACES
              GO TO REC-PE-PGM.

           MOVE VDUBUF TO PE-PGM HOLD-PE-PGM.

       REC-PE-SEQ.
           MOVE "ENTER OPTION, F6-SCAN EXCEPTIONS" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N030SEQ." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" OR "1U"
              GO TO REC-PE-PGM.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-PE-MODULE.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-PE-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-PE-SEQ.

           MOVE VDUNUM0 TO PE-SEQ HOLD-PE-SEQ.

       READ-PWFILE.
           MOVE PE-PW-KEY TO PW1-KEY.
           PERFORM OPEN-PW1-FILE.
           PERFORM READ-PW1-FILE.
           PERFORM CLOSE-PW1-FILE.
           IF IO-GOOD
              MOVE "S  A000PWDESC." TO VDU
              MOVE PW-DESC TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "YOU MUST ENTER A VALID PASSWORD" TO MESS
              PERFORM SEND-MESS
              GO TO REC-PE-MODULE-NO.

       REC-PE-LOGIN.
           MOVE "ENTER LOGIN EXCEPTION, F6-SCAN EXCEPTIONS"
              TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A080LOGIN." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-PE-MODULE.
           IF F6-KEY
              PERFORM ENTER-SCANKEY
              GO TO END-REC-PE-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-PE-MODULE-NO.
           IF VDUBUF = SPACES
              GO TO REC-PE-LOGIN.
           MOVE VDUBUF TO PE-LOGIN.
           CALL "C$TOLOWER" USING PE-LOGIN, VALUE 8.
           MOVE PE-LOGIN TO HOLD-PE-LOGIN.

           MOVE PE1-KEY TO HOLD-PE1-KEY.
           PERFORM OPEN-PE1-FILE.
           PERFORM READ-PE1-FILE.
           PERFORM CLOSE-PE1-FILE.
           IF IO-FG = 0
              MOVE PE-REC TO ORIGINAL-PE-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.
       END-REC-PE-MODULE.
           EXIT.

      *****************************************************
       ENTER-SCANKEY SECTION.
      ********************************************************
           MOVE 0 TO FIRST-SCAN.
           MOVE " ENTER DIR/PGM OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           MOVE SPACES TO PE-PGM PE-LOGIN.
           MOVE 0 TO PE-SEQ.

           PERFORM OPEN-PE1-FILE.
           PERFORM START-PE1-FILE.

       ENTER-KEY.
           MOVE "E  A210SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.

           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE PE1-KEY TO HOLD-PE1-KEY
                 MOVE PE-REC TO ORIGINAL-PE-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUBUF TO WS-HOLD-PE1-KEY.
           MOVE WS-HOLD-PE-PGM TO HOLD-PE-PGM PE-PGM.
           MOVE WS-HOLD-PE-SEQ TO HOLD-PE-SEQ PE-SEQ.
           MOVE WS-HOLD-PE-LOGIN TO HOLD-PE-LOGIN PE-LOGIN.
           PERFORM START-PE1-FILE.

       READ-FILES-NEXT.
           PERFORM READ-PE1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK PE-FILE.

           MOVE "S  A100PGM." TO VDU.
           MOVE PE-PGM TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN N030SEQ." TO VDU.
           MOVE PE-SEQ TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A080LOGIN." TO VDU.
           MOVE PE-LOGIN TO VDUBUF.
           PERFORM SCREENIO.

           IF IO-FG NOT = 0
              MOVE "PASSWORD RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
           ELSE
              PERFORM DISPLAY-FIELDS
              MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-KEY.

       CLOSE-SCANKEY.
           PERFORM CLOSE-PE1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

       END-SCANKEY.
           EXIT.

      ******************************************************************
      *
      *  E N T R Y - M O D U L E
      *
      *
      *  NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *             1 - INITIAL NEW RECORD ENTRY
      *             2 - MODIFY NEW RECORD
      *
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
           MOVE "PRESS <ENTER> TO ACCEPT, DELETE" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ACTION = "DELETE"
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

       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
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

           GO TO END-ENTRY-MODULE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-PE1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE PE-REC TO CHANGED-PE-REC.

           IF ORIGINAL-PE-REC = CHANGED-PE-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-PE1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF PE-REC NOT = ORIGINAL-PE-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-PE-REC TO PE-REC.
           PERFORM REWRITE-PE1-FILE.
           IF IO-FG = 0
              MOVE "C"            TO AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-PE1-FILE.
           IF IO-FG = 0
              MOVE PE-REC TO ORIGINAL-PE-REC
              MOVE "A"    TO AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW
           ELSE
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-PE1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-PE1-FILE.
           PERFORM READ-PE1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF ( PE-REC NOT = ORIGINAL-PE-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-PE1-FILE.
           IF IO-FG = 0
              MOVE "D"            TO AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW.

       END-DELETE-MODULE.
           PERFORM CLOSE-PE1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.

           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES TO PE-REC.

           MOVE 0 TO PE-SEQ PE-LTOUCH-DATE.

           MOVE HOLD-PE1-KEY TO PE1-KEY.
           MOVE 0 TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************************
       DISPLAY-KEY SECTION.
      ********************************************************
           MOVE HOLD-PE-PGM      TO D-PGM.
           MOVE HOLD-PE-SEQ      TO D-SEQ.
           MOVE HOLD-PE-LOGIN    TO D-LOGIN.

           MOVE KEY-BUF TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************

           DISPLAY SPACES UPON PEMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PEMAIN-SCREEN UPON PEMAIN-WINDOW-HANDLE.
           MOVE PEMAIN-FORM-FIELDS TO WR-BUF.
           MOVE PEMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE PEMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE PEMAIN-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBMN/PEMAIN_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       UPDATE-AUDITW.
           MOVE FORM-PATHNAME   TO FORM-PATH.
           MOVE FORM-PROGX      TO AUDITW-PROGRAM.
           MOVE "AUDITW"        TO FORM-NAM.
           MOVE ORIGINAL-PE-REC TO AUDITW-ORIG-REC.
           MOVE PE-REC          TO AUDITW-CHG-REC.
           MOVE "PEFILE"        TO AUDITW-FILE-NAME.
           CALL FORM-PATH USING FORM-PATH AUDITW-WORKERS.
           CANCEL FORM-PATH.
       SEND-LEGEND.
           MOVE "S  A600MSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
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
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PW1-FILE.
           PERFORM CLOSE-PE1-FILE.
       COPY "LIBGB/GBPWGS_SQL.CPY".
       COPY "LIBGB/GBPEGS_SQL.CPY".
       COPY "LIBGB/GBPE1IN.CPY".
       COPY "LIBGB/GBPW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PEMAIN-SCREEN.
           DESTROY PEMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
