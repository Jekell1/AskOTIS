       IDENTIFICATION DIVISION.
       PROGRAM-ID.  RXMAIN.
      ******************************************************************
      *
      *  DESCRIPTION:   RESCISSION INDEX FILE MAINTENANCE
      *
      *=================================================================
      * REV:
      * AMW 2008.0204 CREATED NEW FILE MAINTENANCE
      * KEC 2017.0313 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED RX-BRNO.
      *  CS 170505 ACTIVATE 8 DIGIT DATES
      * AMM 190412 LOCAL TO GLOBAL CHANGES
      * JKC 200612 FIXED USE OF RX-INTDATE TO USE YYYYMMDD FORMAT; WAS
      *            STILL USING YYYMMDD FORMAT.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/RXMAIN.CBL S34 06/06/21-16:49:15 >".
       COPY "LIBLP/LP01RX.CPY".
       COPY "LIBLP/LP01RX_SQL.CPY".
       COPY "LIBLP/LPWSRX.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".

       01  ERRCD           PIC X VALUE " ".
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9      VALUE 0.

       01  HOLD-RX1-KEY           VALUE SPACES.
           03  HOLD-RX-BRNO       PIC 9(4).
           03  HOLD-RX-ACCTNO     PIC 9(8).

       01  ORIGINAL-RX-REC    PIC X(RX-SIZE).
       01  CHANGED-RX-REC     PIC X(RX-SIZE).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(16).
           03  F02         PIC X(16).
           03  F03         PIC 99/99/99.
           03  F04         PIC ZZZZZZ.99.
           03  F05         PIC ZZZZZZ.99.
           03  F06         PIC ZZZZ.99.
           03  F07         PIC 99/99/99.
           03  F08         PIC X(01).
       01  DISPLAY-LIST.
           03  FILLER      PIC X(32)  VALUE
           "010 020 030 040 050 060 070 080.".

      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       78  MAX                      VALUE 8.
       01  SPEC-TABLE.
      * 1. DESCRIPTION
           03  FILLER      PIC X(8) VALUE "E  A160 ".
           03  FILLER      PIC X(8) VALUE "E  A160 ".
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS MAX TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.
      *
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/RXMAIN_DEF.CPY".
       COPY "LIBMN/RXMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  RXMAIN-WINDOW-HANDLE PIC X(10).

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/RXMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************
       MAIN-PROGRAM SECTION.
      **************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "RXMAIN" TO VDU-FORM-NAME.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS RXMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM FORM-RESET.

           PERFORM RX-KEY-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ***************************
       RX-KEY-MODULE SECTION.
      ***************************
       RX-KEY-MODULE-NO.
           MOVE 0 TO RX-ACCTNO FIRST-SCAN.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "ENNN080ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-RX-KEY-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-RX-KEY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO RX-KEY-MODULE-NO.
           IF VDUNUM0 = 0
              GO TO RX-KEY-MODULE-NO.

           MOVE VDUNUM0 TO RX-ACCTNO HOLD-RX-ACCTNO.
       READ-RECISION-FILE.
           PERFORM OPEN-RX1-FILE.
           PERFORM READ-RX1-FILE.
           PERFORM CLOSE-RX1-FILE.
           IF IO-FG = 0
              MOVE RX-REC TO ORIGINAL-RX-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.
       END-RX-KEY-MODULE.
           EXIT.

      ************************************************
       ENTER-SCANKEY SECTION.
      ************************************************
           MOVE "ENTER ACCOUNT #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-RX1-FILE.
           MOVE RX-PATH-OWNBR TO RX-BRNO

           PERFORM START-RX1-FILE.
       ENTER-KEY.
           MOVE "EN N080SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE RX1-KEY TO HOLD-RX1-KEY
                 MOVE RX-REC TO ORIGINAL-RX-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0 TO RX-ACCTNO.

           PERFORM START-RX1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-RX1-FILE-NEXT.
           IF IO-FG = 9 OR (RX-BRNO NOT = RX-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
      *    UNLOCK RX-FILE.

           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "SN N080ACCTNO." TO VDU.
           MOVE RX-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-RX1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      ******************************************************************
       DEALER-SCAN SECTION.
      ******************************************************************

           MOVE ZEROES     TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( DLSCAN-STATUS NOT = "00" ) OR ( DLSCAN-NUM = " " )
              MOVE 9 TO IO-FG
              GO TO EXIT-DEALER-SCAN.

           MOVE "SN N042060." TO VDU.
           MOVE DLSCAN-DLNO   TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE ZEROES      TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
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
              GO TO CREATE-ROUTINE.
       TEST-F6.
           IF ( VDUSTATUS NOT = "1<" )
              GO TO TEST-F1.
           IF ( ELE = 6 )
              PERFORM DEALER-SCAN
              IF ( IO-FG = 0 )
                 MOVE 1 TO ONE
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08
                 DEPENDING ON ELE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           IF ( VDUBUF = SPACES )
              GO TO ENTER-ELE.
           MOVE VDUBUF TO RX-LNAME.
           GO TO CREATE-ROUTINE.

       F-02.
           IF ( VDUBUF = SPACES )
              GO TO ENTER-ELE.
           MOVE VDUBUF TO RX-FNAME.
           GO TO CREATE-ROUTINE.

       F-03.
           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-ELE.
           MOVE VDU-DATE-YYYYMMDD  TO RX-LOANDATE.
           GO TO CREATE-ROUTINE.

       F-04.
           IF ( VDUNUM2 = ZEROES )
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO RX-LOANAMT.
           GO TO CREATE-ROUTINE.

       F-05.
           IF ( VDUNUM2 = ZEROES )
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO RX-PROCEEDS.
           GO TO CREATE-ROUTINE.

       F-06.
           MOVE VDUNUM2 TO DL-DLNO RX-DLNO.
           PERFORM GET-DL1.
           IF ( IO-FG = 9 )
              MOVE " INVALID DEALER NUMBER, F6-SCAN" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-07.
           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-ELE.
           MOVE VDU-DATE-YYYYMMDD TO RX-INTDATE.
           GO TO CREATE-ROUTINE.

       F-08.
           IF NOT ( VDUBUF = "I" OR "R" )
              MOVE "INVALID CODE, ENTER 'I' OR 'R'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO RX-STATUS.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *****************************
       WRITE-MODULE SECTION.
      *****************************
           PERFORM OPEN-RX1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE RX-REC TO CHANGED-RX-REC.
           IF ORIGINAL-RX-REC = CHANGED-RX-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-RX1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF RX-REC NOT = ORIGINAL-RX-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-RX-REC TO RX-REC.
           PERFORM REWRITE-RX1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-RX1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-RX1-FILE.

      *****************************
       DELETE-MODULE SECTION.
      *****************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-RX1-FILE.
           PERFORM READ-RX1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF RX-REC NOT = ORIGINAL-RX-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-RX1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-RX1-FILE.

      *****************************
       NEW-RECORD-SETUP SECTION.
      *****************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES TO RX-REC.
           MOVE HOLD-RX1-KEY TO RX1-KEY.
           MOVE 0 TO RX-DLNO RX-INTDATE RX-LTOUCH-DATE RX-LOANDATE
                     RX-LOANAMT RX-PROCEEDS.

           MOVE 0 TO ELE.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE RX-LNAME    TO F01.
           MOVE RX-FNAME    TO F02.
           MOVE RX-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F03.
           MOVE RX-LOANAMT  TO F04.
           MOVE RX-PROCEEDS TO F05.
           MOVE RX-DLNO     TO F06.
           MOVE RX-INTDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F07.
           MOVE RX-STATUS   TO F08.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO RXMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RXMAIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY RXMAIN-SCREEN UPON RXMAIN-WINDOW-HANDLE.
           MOVE RXMAIN-FORM-FIELDS TO WR-BUF.
           MOVE RXMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/RXMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE RXMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE RXMAIN-HELP--DIR TO HELP-LINK-DIR.

      **************************
       MISC-PARAGRAPHS SECTION.
      **************************
       COPY "LIBGB/ACCESS.CPY".
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
       GET-DL1.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.

      ***************************
       IO-ROUTINES SECTION.
      ***************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RX1-FILE.
           PERFORM CLOSE-DL1-FILE.
       COPY "LIBLP/LPRXGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPRX1IN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ***************************
       END-ROUTINE SECTION.
      ***************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY RXMAIN-SCREEN.       
           DESTROY RXMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
