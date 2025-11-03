       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FLMAIN.
      ********************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   FLAG  FILE MAINTENANCE
      * REV:
      ********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSFL.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDFL.CPY".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/FLMAIN.CBL S34 06/03/21-20:52:55 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
      *********************************************************
      *        LIBGB/GBWSFL: FLFILE PATH WORKERS
      *********************************************************
      * /USR/AXX/FILE.DOC/FLAGS
       01  FL-PATH.
            03  FILLER                PIC X VALUE "/".
            03  FL-PATH-REL           PIC X(7).
            03  FILLER                PIC X(25) VALUE
                                  "/FILE.DOC/FLAGS          ".


       01  ERRCD           PIC X        VALUE " ".

       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9       VALUE 0.

       01  HOLD-KEY.
           03  HOLD-FL-FIELD-NAME  PIC X(30).

       01  ORIGINAL-REC    PIC X(FL-SIZE).
       01  CHANGED-REC     PIC X(FL-SIZE).
      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(35).
           03  F02         PIC X(6).
           03  F03         PIC 99.
           03  F04         PIC 99.
           03  F05         PIC X(20).
           03  F06         PIC X(20).
           03  F07         PIC X(20).
           03  F08         PIC X(20).
           03  F09         PIC X(20).
           03  F10         PIC X(20).
           03  F11         PIC X(20).
           03  F12         PIC X(20).
           03  F13         PIC X(20).
           03  F14         PIC X(20).
       01  DISPLAY-LIST.
           03  FILLER      PIC X(28) VALUE
           "010 020 030 040 050 060 070 ".
           03  FILLER      PIC X(28) VALUE
           "080 090 100 110 120 130 140.".
      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 14.
       01  SPEC-TABLE.
      *1.  FIELD DESCRIPTION
           03  FILLER      PIC X(8) VALUE "E  A350 ".
      *2.  PROGRAM
           03  FILLER      PIC X(8) VALUE "E  A060 ".
      *3.  PAGE
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *3.  FIELD-NO
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *4.  KEYWORD
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      *5.  KEYWORD
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      *6.  KEYWORD
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      *7.  KEYWORD
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      *8.  KEYWORD
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      *9.  KEYWORD
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      *10. KEYWORD
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      *12. KEYWORD
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      *13. KEYWORD
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      *14. KEYWORD
           03  FILLER      PIC X(8) VALUE "E  A200 ".
       01  SPEC-TABLE2    REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 14 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      *
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  FLMAIN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/FLMAIN_DEF.CPY".
       COPY "LIBMN/FLMAIN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/FLMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ****************************
       MAIN-PROGRAM SECTION.
      ****************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "FLMAIN" TO VDU-FORM-NAME.

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO FL-PATH-REL.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FLMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-FL-MODULE.
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


      ************************************
       REC-FL-MODULE SECTION.
      ************************************
       REC-FL-FIELD-NAME.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "ENTER FIELD NAME" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A300FLDNAME." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-FL-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-FL-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-FL-FIELD-NAME.

           MOVE VDUBUF TO FL-FIELD-NAME HOLD-FL-FIELD-NAME.
           IF FL-FIELD-NAME = SPACES
              GO TO REC-FL-FIELD-NAME.

       READ-FLFILE.
           PERFORM OPEN-FL1-FILE.
           PERFORM READ-FL1-FILE.
           CLOSE FL-FILE.
           IF IO-FG = 0
              MOVE FL-REC TO ORIGINAL-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.
       END-REC-FL-MODULE.
           EXIT.

      ************************************************
       ENTER-SCANKEY SECTION.
      ************************************************
           MOVE "ENTER ACCOUNT #, OR F6 TO SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE 0 TO FIRST-SCAN.

           PERFORM OPEN-FL1-FILE.
           PERFORM START-FL1-FILE.
       ENTER-KEY.
           MOVE "E RA000SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FLFILE-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE FL1-KEY TO HOLD-KEY
                 MOVE FL-REC TO ORIGINAL-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           MOVE VDUBUF TO FL1-KEY.
           PERFORM START-FL1-FILE.

       READ-FLFILE-NEXT.
           PERFORM READ-FL1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
      *    UNLOCK FL-FILE.

           MOVE "S  A300FLDNAME." TO VDU.
           MOVE FL-FIELD-NAME TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           PERFORM CLOSE-FL1-FILE.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       END-SCANKEY.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
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
           IF CHFG(ELE) = "N"
              GO TO CREATE-ROUTINE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
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
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
                 F-08 F-09 F-10 F-11 F-12 F-13 F-14
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO FL-DESC.
           GO TO CREATE-ROUTINE.

       F-02.
           MOVE VDUBUF TO FL-PROG.
           GO TO CREATE-ROUTINE.

       F-03.
           MOVE VDUNUM0 TO FL-PAGE.
           GO TO CREATE-ROUTINE.

       F-04.
           MOVE VDUNUM0 TO FL-FIELD-NO.
           GO TO CREATE-ROUTINE.

       F-05.
           MOVE VDUBUF TO FL-KEYWORD(1).
           GO TO CREATE-ROUTINE.

       F-06.
           MOVE VDUBUF TO FL-KEYWORD(2).
           GO TO CREATE-ROUTINE.

       F-07.
           MOVE VDUBUF TO FL-KEYWORD(3).
           GO TO CREATE-ROUTINE.

       F-08.
           MOVE VDUBUF TO FL-KEYWORD(4).
           GO TO CREATE-ROUTINE.

       F-09.
           MOVE VDUBUF TO FL-KEYWORD(5).
           GO TO CREATE-ROUTINE.

       F-10.
           MOVE VDUBUF TO FL-KEYWORD(6).
           GO TO CREATE-ROUTINE.

       F-11.
           MOVE VDUBUF TO FL-KEYWORD(7).
           GO TO CREATE-ROUTINE.

       F-12.
           MOVE VDUBUF TO FL-KEYWORD(8).
           GO TO CREATE-ROUTINE.

       F-13.
           MOVE VDUBUF TO FL-KEYWORD(9).
           GO TO CREATE-ROUTINE.

       F-14.
           MOVE VDUBUF TO FL-KEYWORD(10).
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      **********************************
       WRITE-MODULE SECTION.
      **********************************
           PERFORM OPEN-FL1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE FL-REC TO CHANGED-REC.
           IF ORIGINAL-REC = CHANGED-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-FL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF FL-REC NOT = ORIGINAL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-REC TO FL-REC.
           PERFORM REWRITE-FL1-FILE.

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-FL1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           CLOSE FL-FILE.

      **********************************
       DELETE-MODULE SECTION.
      **********************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM OPEN-FL1-FILE.
           PERFORM READ-FL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF FL-REC NOT = ORIGINAL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-FL1-FILE.
       END-DELETE-MODULE.
           CLOSE FL-FILE.

      **********************************
       NEW-RECORD-SETUP SECTION.
      **********************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           MOVE SPACES TO FL-REC.
           MOVE HOLD-KEY TO FL1-KEY.

           MOVE 0 TO ELE.

      **********************************
       DISPLAY-FIELDS SECTION.
      **********************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE FL-DESC        TO F01.
           MOVE FL-PROG        TO F02.
           MOVE FL-PAGE        TO F03.
           MOVE FL-FIELD-NO    TO F04.
           MOVE FL-KEYWORD(1)  TO F05.
           MOVE FL-KEYWORD(2)  TO F06.
           MOVE FL-KEYWORD(3)  TO F07.
           MOVE FL-KEYWORD(4)  TO F08.
           MOVE FL-KEYWORD(5)  TO F09.
           MOVE FL-KEYWORD(6)  TO F10.
           MOVE FL-KEYWORD(7)  TO F11.
           MOVE FL-KEYWORD(8)  TO F12.
           MOVE FL-KEYWORD(9)  TO F13.
           MOVE FL-KEYWORD(10) TO F14.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO FLMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FLMAIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FLMAIN-SCREEN UPON FLMAIN-WINDOW-HANDLE.
           MOVE FLMAIN-FORM-FIELDS TO WR-BUF.
           MOVE FLMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE FLMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE FLMAIN-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/FLMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************
       MISC-PARAGRAPHS SECTION.
      **********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
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
       SEND-LEGEND.
           PERFORM CALL-TMMSG.

      *******************************
       IO-ROUTINE SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
       COPY "LIBGB/GBFL1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *******************************
       END-ROUTINE SECTION.
      *******************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.       
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FLMAIN-SCREEN.             
           DESTROY FLMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
