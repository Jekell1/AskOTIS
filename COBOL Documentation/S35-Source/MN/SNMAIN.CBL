       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SNMAIN.
       DATE-WRITTEN.    101410.
      ******************************************************************
      *
      *  DESCRIPTION:   SECURITY PROGRAM NAME MAINTENANCE
      *
      * REV:
      * BLM 101202 IF PURCHASABLE FLAG IS CHANGED, UPDATE SEFILE WITH
      *            NEW VALUE OF PURCHASABLE & SET SE-EXP-DATE TO 0.
      *            ALSO, IF CREATING NEW SN REC, CREATE SE REC.
      * KEC 2015.1113 REMOVED USE OF SE-DIR & SN-DIR
      * BLM 150119 FIXED GARBLED SCREEN, WAS MOVING SN-REC TO WK-REC &
      *            BACK & WK-REC STILL HAD DIR. CHANGED OPTION TO SORT
      *            BY DIR TO SORT BY NAME.  STILL HARD TO FIND STUFF.
      *  CS 190410 ADDED FUTURE FIELDS TO WK-SN-REC WHICH IS USED TO MOVED 
      *            TO INDIVIDUAL SCREEN FIELDS
      *  BAH 20210506 REMOVED FUTURE FIELDS
      *  BAH 20220218 CALL NEW AUDIT/CHANGE PROGRAM, AUDITW, #1542
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSWKR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-SN-REC.
               05  WK-SN-SEQ           PIC 9(8).
               05  WK-SN-FORM          PIC X(6).
               05  WK-SN-LTOUCH-DATE   PIC 9(8) COMP-3.
               05  WK-SN-DESC          PIC X(30).
               05  WK-SN-PURCHASABLE   PIC X.
           03  WK-STATUS               PIC X.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/SNMAIN S34 07/23/21-10:54:46 >".
       COPY "LIBGB/GB01SN.CPY".
       COPY "LIBGB/GB01SN_SQL.CPY".
       COPY "LIBGB/GBWSSN.CPY".
       COPY "LIBGB/GB01SE.CPY".
       COPY "LIBGB/GB01SE_SQL.CPY".
       COPY "LIBGB/GBWSSE.CPY".

       COPY "LIBGB/GB01SE.CPY" REPLACING LEADING "SE" BY "SEOR".
       COPY "LIBGB/GB01SN.CPY" REPLACING LEADING "SN" BY "SNOR".

       01  WK-IFN          PIC X(2) VALUE "WK".
       01  WK-KEY          PIC 9(6).

       01  ERRCD           PIC X    VALUE " ".
       01  ONE             PIC S9.
       01  ELE             PIC 9(6).
       01  ACTION          PIC X(6).
       01  NEW-FLAG        PIC 9.
       01  UPDATE-FLAG     PIC 9  VALUE 0.

       01  SUB             PIC 9(6) COMP-3   VALUE 0.
       01  LIST-ORDER      PIC X    VALUE "S".
       01  LEGEND-01       PIC X(38) VALUE
           "^-UP, V-DN, F6-ADD, AUDIT LINE#       ".
       01  LEGEND-01D      PIC X(38) VALUE
           "^-UP, V-DN, F6-ADD, AUDIT/DELETE LINE#".

      ************************************
      *     WORK LINE DISPLAY BUFFER     *
      ************************************
       01  ENTRY-LINE             PIC 9(4) VALUE 0.
       01  B-LN                   PIC 9(4) VALUE 0.
       01  B-TOP-LN               PIC 9(4) VALUE 0.
       01  B-SCREEN-LN            PIC 9(4) VALUE 0.
       01  LSUB                   PIC 9(6) COMP-3.
       01  LSUB2                  PIC 9(6) COMP-3.
       01  S-LNS                  PIC 9(6) COMP-3 VALUE 15.
       01  S-LNS-PLUS1            PIC 9(6) COMP-3 VALUE 16.
       01  S-LNS-LESS1            PIC 9(6) COMP-3 VALUE 14.
       01  S-LNSX2-LESS1          PIC 9(6) COMP-3 VALUE 29.
       01  S-LNS-FREE-WK-RECS     PIC 99 VALUE 0.
       01  LN-WK                  PIC 9(6) COMP-3.
       01  LINE-BUF.
           03  D-LN               OCCURS 15 TIMES.
               05  D-LN-NO        PIC 9(4).
               05  D-LN-BODY      PIC X(74).
               05  D-LN-DEL       PIC X.
               05  FILLER         PIC X.
       01  LINE-LISTS.
           03  FILLER             PIC X(40) VALUE
           "01L 02L 03L 04L 05L 06L 07L 08L 09L 10L ".
           03  FILLER             PIC X(20) VALUE
           "11L 12L 13L 14L 15L.".
       01  LINE-LIST REDEFINES LINE-LISTS
                                  PIC X(60).

      **************************************
      *        ENTRY BAR LAYOUT            *
      **************************************
       01  LN-BUF                 VALUE SPACE.
           03  L-010              PIC 9(4).
           03  FILLER             PIC X(2).
           03  L-020              PIC X(2).
           03  FILLER             PIC X(4).
           03  L-030              PIC X(6).
           03  FILLER             PIC X(5).
           03  L-040              PIC Z(8).
           03  FILLER             PIC X(4).
           03  L-050              PIC X(30).
           03  FILLER             PIC X(6).
           03  L-060              PIC X.
           03  FILLER             PIC X(2).
           03  L-DEL              PIC X.
           03  FILLER             PIC X.
       01  REV-MESS               PIC X(27) VALUE
           "YN 060 050 040 030 020 010.".

      *************************************
      *    ENTRY LINE AUTO SETUP BUFFER   *
      *************************************
       01  EL-BUF                 VALUE SPACE.
           03  EL-YN              PIC X.
           03  EL-060             PIC X.
           03  EL-050             PIC X(30).
           03  EL-040             PIC Z(8).
           03  EL-030             PIC X(6).
           03  EL-020             PIC X(2).
           03  EL-010             PIC 9(4).

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   VALUE 6.
       01  SPEC-TABLE.
           03  FILLER      PIC X(8) VALUE "EN N040N".
           03  FILLER      PIC X(8) VALUE "E  A020N".
           03  FILLER      PIC X(8) VALUE "E  A100 ".
           03  FILLER      PIC X(8) VALUE "ENNN080 ".
           03  FILLER      PIC X(8) VALUE "E  A300 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 6 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBCL/AUDITWW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  SNMAIN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SNMAIN_DEF.CPY".
       COPY "LIBMN/SNMAIN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/SNMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SN2-FILE.
           PERFORM CLOSE-SN1-FILE.
           PERFORM CLOSE-SE1-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "SNMAIN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SNMAIN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      D    STOP MESS.
       ENTER-LOAD-ORDER.

           MOVE "ORDER: S-SEQUENCE, N-PROGRAM NAME" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A010ORDER." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LOAD-ORDER.
           IF VDUBUF NOT = "S" AND VDUBUF NOT = "N"   
              MOVE "ENTER 'S' (BY SEQ) OR 'N' (BY NAME)" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LOAD-ORDER.

           MOVE VDUBUF TO LIST-ORDER.

           PERFORM LOAD-WORK-FILE.
  
      * ENTER A TICKET # FOR EACH NEW LOAD
      * DO NOT ENTER TICKET # UNTIL F7 UPDATE FOR ALL CHANGES AT ONCE
           MOVE "Y"            TO AUDITW-ENTER-TICKET.

       ENTER-RECORDS.
           MOVE "N" TO CHFG(1).
           PERFORM ENTRY-MODULE.

       UPDATE-YN.
           MOVE "F1-CANCEL, U-UPDATE, A-AUDIT/ADD:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA000SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM CLEAR-ALL-LINES
              GO TO ENTER-LOAD-ORDER.
           IF VDUSTATUS NOT = "00"
              GO TO UPDATE-YN.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "     A"
              MOVE "SEL." TO MESS
              PERFORM RESET-MESS
              GO TO ENTER-RECORDS.
           IF ACTION NOT = "     U"
              GO TO UPDATE-YN.

           PERFORM RECORD-UPDATE.

           PERFORM CLEAR-ALL-LINES.

           GO TO ENTER-LOAD-ORDER.

      **************************************
      *   E L E M E N T   E N T R Y        *
      *   NEW-FLAG: 0 - MOD EXISTING REC   *
      *             1 - INITIAL ENTRY      *
      *             2 - MOD NEW REC        *
      **************************************
       ENTRY-MODULE SECTION.
       ENTER-NEXT-LINE.
           ADD 1 TO B-LN.
           MOVE B-LN TO LN-WK.
           MOVE "S  A020010." TO VDU.
           MOVE B-LN TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-RECORD.

       BEGIN-ENTRY.
           IF CHFG(1) = " "
              PERFORM RESET-ENTRY-LINE
              IF EXIT-PATH = "GB/COMENU"
                 MOVE LEGEND-01D TO MESS
              ELSE
                 MOVE LEGEND-01 TO MESS
              END-IF
           ELSE
              MOVE "F6-AUDIT, F7-DONE" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 1 TO ONE NEW-FLAG.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO ACCEPT-LINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE = 6
              IF EXIT-PATH NOT = "GB/COMENU"
                 GO TO CREATE-ROUTINE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       ACCEPT-LINE.
           IF CHFG(1) = "N"
              MOVE "^-BACK, A-ACCEPT" TO MESS
           ELSE
              IF EXIT-PATH = "GB/COMENU"
                 MOVE "F1-NEW LINE, ^-BACK, D-DELETE, A-ACCEPT"
                    TO MESS
              ELSE
                 MOVE "F1-NEW LINE, ^-BACK, A-ACCEPT" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010YN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO BEGIN-ENTRY.
           IF VDUSTATUS = "1U"
              MOVE 1 TO NEW-FLAG
              MOVE -1 TO ONE
              ADD 1 MAX GIVING ELE
              GO TO CREATE-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ACCEPT-LINE.
           IF CHFG(1) = "N"
              IF VDUBUF = "A"
                 PERFORM WRITE-LINE-SCREEN
                 ADD S-LNS-FREE-WK-RECS B-LN GIVING WK-KEY
                 PERFORM WRITE-LINE-DISK
                 PERFORM RESET-ENTRY-LINE
                 GO TO ENTER-NEXT-LINE
              ELSE
                 GO TO ACCEPT-LINE.
           IF VDUBUF = "A"
              PERFORM UPDATE-AUDIT
              GO TO BEGIN-ENTRY.
           IF VDUBUF = "D"
              IF EXIT-PATH = "GB/COMENU"
                 PERFORM DELETE-AUDIT
                 GO TO BEGIN-ENTRY.
           GO TO ACCEPT-LINE.
       ENTER-ELE.
           IF ELE = 4
              MOVE "F3 - CLEAR SEQUENCE " TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 5
              MOVE SPACES TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 2
              IF CHFG(1) = " "
                 PERFORM RESET-SEL.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F1.
           IF VDUSTATUS = "10"
              GO TO BEGIN-ENTRY.
       TEST-F3.
           IF VDUSTATUS NOT = "16"
              GO TO TEST-F4.
           IF ELE = 4
              MOVE 0 TO VDUNUM0 WK-SN-SEQ
              GO TO GOTO-FIELD.
       TEST-F4.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-F5.
           IF ELE = 1
              PERFORM SCROLL-DOWN
              GO TO ENTER-ELE.
           IF ELE = 2
              GO TO BEGIN-ENTRY.
           IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS NOT = "1D"
              GO TO TEST-F6.
           IF ELE = 1
              PERFORM SCROLL-UP
              GO TO ENTER-ELE.
           GO TO GOTO-FIELD.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-F7.
           MOVE 0 TO UPDATE-FLAG.
           IF CHFG(1) = " "
              IF ELE = 1
                 PERFORM ADJUST-LINES-TO-END
                 MOVE "N" TO CHFG(1)
                 GO TO ENTER-NEXT-LINE.
           IF CHFG(1) = "N"
              MOVE " " TO CHFG(1)
              SUBTRACT 1 FROM B-LN
              GO TO BEGIN-ENTRY.
       TEST-F7.
           IF VDUSTATUS = "1>"
              IF CHFG(1) NOT = " "
                 SUBTRACT 1 FROM B-LN
                 PERFORM RESET-ENTRY-LINE
                 GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
       GOTO-FIELD.
           MOVE 1 TO ONE.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06
              DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      ************************************
      *     S E T   E L E M E N T S      *
      ************************************
       F-01.
           MOVE VDUNUM0 TO ENTRY-LINE.
           IF ENTRY-LINE = 0 OR ENTRY-LINE > B-LN
              GO TO ENTER-ELE.
           IF B-LN > S-LNS-LESS1
              ADD B-TOP-LN S-LNS-LESS1 GIVING LN-WK
              IF ENTRY-LINE < B-TOP-LN OR ENTRY-LINE > LN-WK MOVE
              "PLEASE REVIEW LINE PRIOR TO ATTEMPTING THIS FUNCTION !"
              TO MESS PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE ENTRY-LINE TO LN-WK.
           PERFORM SETUP-OLD-LINE.
           GO TO CREATE-ROUTINE.

       F-02.

           GO TO CREATE-ROUTINE.

       F-03.
           IF UPDATE-FLAG = 1 AND VDUBUF NOT EQUAL WK-SN-FORM
              MOVE "CANNOT CHANGE - MUST DELETE & ADD"
                   TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO WK-SN-FORM.

           IF WK-SN-FORM = " "
              GO TO ENTER-ELE.

           MOVE WK-SN-FORM TO SN-FORM.
           PERFORM READ-SN1-FILE.
           IF IO-FG = 0
              MOVE "SN N080040." TO VDU
              MOVE SN-SEQ TO VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A300050." TO VDU
              MOVE SN-DESC TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A010060." TO VDU
              MOVE SN-PURCHASABLE TO VDUBUF
              PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-04.
           IF VDUNUM0 = 0
              MOVE "SN N080040." TO VDU
              MOVE WK-SN-SEQ TO VDUNUM0
              PERFORM SCREENIO.
           MOVE VDUNUM0 TO WK-SN-SEQ.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO WK-SN-DESC.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO WK-SN-PURCHASABLE.
           IF WK-SN-PURCHASABLE NOT = "Y"
              IF WK-SN-PURCHASABLE NOT = "N"
                 MOVE "PLEASE ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      *****************************************
       WRITE-LINE-SCREEN SECTION.
      *****************************************
           PERFORM SETUP-LINE.

           ADD 1 TO B-SCREEN-LN.
           IF B-SCREEN-LN > S-LNS
              MOVE 1 TO LSUB
              PERFORM ADJUST-LINES S-LNS-LESS1 TIMES
              MOVE LN-BUF TO D-LN(S-LNS)
              PERFORM DISPLAY-ALL-LINES
            ELSE
              MOVE "S  A00000L." TO VDU
              MOVE B-SCREEN-LN TO VDUROW
              MOVE LN-BUF TO VDUBUF D-LN(B-SCREEN-LN)
              PERFORM SCREENIO.

           GO TO END-WRITE-LINE-SCREEN.
       ADJUST-LINES.
           COMPUTE LSUB2 = LSUB + 1
           MOVE D-LN(LSUB2) TO D-LN(LSUB).
           ADD 1 TO LSUB.
       END-WRITE-LINE-SCREEN.
           MOVE D-LN-NO(1) TO B-TOP-LN.

      *************************************
       WRITE-LINE-DISK SECTION.
      *************************************
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

       END-WRITE-LINE-DISK.
           PERFORM CLOSE-WK-FILE.

      *********************************
       UPDATE-AUDIT SECTION.
      *********************************
           ADD ENTRY-LINE S-LNS-FREE-WK-RECS GIVING WK-KEY.
           MOVE " " TO WK-STATUS.

           PERFORM SETUP-LINE.

           COMPUTE ENTRY-LINE = ENTRY-LINE - B-TOP-LN + 1.
           MOVE "S  A00000L." TO VDU.
           MOVE ENTRY-LINE TO VDUROW.
           MOVE LN-BUF TO VDUBUF D-LN(ENTRY-LINE).
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG.

           PERFORM OPEN-WK-FILE.
           MOVE " " TO WK-STATUS.
           PERFORM REWRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-WK-FILE.

      *********************************
       DELETE-AUDIT SECTION.
      *********************************
           ADD S-LNS-FREE-WK-RECS ENTRY-LINE GIVING WK-KEY.

           MOVE "D" TO WK-STATUS.

           PERFORM SETUP-LINE.

           COMPUTE ENTRY-LINE = ENTRY-LINE - B-TOP-LN + 1.
           MOVE "D" TO D-LN-DEL(ENTRY-LINE).
           MOVE "S  A00000L." TO VDU.
           MOVE ENTRY-LINE TO VDUROW.
           MOVE LN-BUF TO VDUBUF D-LN(ENTRY-LINE).
           PERFORM SCREENIO.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           MOVE "D" TO WK-STATUS.
           PERFORM REWRITE-WK-FILE.
           PERFORM CLOSE-WK-FILE.

      ***********************************
       SETUP-OLD-LINE SECTION.
      ***********************************
           ADD S-LNS-FREE-WK-RECS ENTRY-LINE GIVING WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM CLOSE-WK-FILE.
           MOVE SPACE TO EL-BUF.
           MOVE ENTRY-LINE TO EL-010.
           MOVE SPACES TO EL-020.          
           MOVE WK-SN-FORM TO EL-030.
           MOVE WK-SN-SEQ TO EL-040.
           MOVE WK-SN-DESC TO EL-050.
           MOVE WK-SN-PURCHASABLE TO EL-060.
           MOVE "A" TO EL-YN.
           PERFORM SEND-ENTRY-LINE.
           MOVE 1 TO UPDATE-FLAG.

      *********************************
       SCROLL-DOWN SECTION.
      *********************************

      * INSERT THIS CHECK WHEN THIS ROUTINE IS NOT USED IN LOAD-WORK-FILE
      * IF B-TOP-LN NOT > 1
      *    GO TO END-SCROLL-DOWN.

           IF B-TOP-LN > S-LNS
              SUBTRACT S-LNS-PLUS1 FROM B-TOP-LN GIVING LN-WK
            ELSE
              MOVE 0 TO LN-WK.
           MOVE 1 TO B-SCREEN-LN.
           MOVE SPACES TO LINE-BUF.
           MOVE 0 TO IO-FG.

           PERFORM OPEN-WK-FILE.
           PERFORM SETUP-LINE-BUF UNTIL B-SCREEN-LN > S-LNS.
           PERFORM CLOSE-WK-FILE.

           PERFORM DISPLAY-ALL-LINES.

           MOVE D-LN-NO(1) TO B-TOP-LN.
       END-SCROLL-DOWN.
           EXIT.

      *********************************
       SCROLL-UP SECTION.
      *********************************
           IF B-LN < S-LNS-PLUS1
              GO TO END-SCROLL-UP.

           ADD B-TOP-LN S-LNSX2-LESS1 GIVING LN-WK.
           IF LN-WK > B-LN
              MOVE B-LN TO LN-WK.
           SUBTRACT S-LNS FROM LN-WK.
           MOVE 1 TO B-SCREEN-LN.
           MOVE SPACES TO LINE-BUF.
           MOVE 0 TO IO-FG.

           PERFORM OPEN-WK-FILE.
           PERFORM SETUP-LINE-BUF UNTIL B-SCREEN-LN > S-LNS.
           PERFORM CLOSE-WK-FILE.

           PERFORM DISPLAY-ALL-LINES.

           MOVE D-LN-NO(1) TO B-TOP-LN.
       END-SCROLL-UP.
           EXIT.

      *************************************
       SETUP-LINE-BUF SECTION.
      *************************************
           ADD 1 S-LNS-FREE-WK-RECS LN-WK GIVING WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE S-LNS-PLUS1 TO B-SCREEN-LN
              GO TO END-SETUP-LINE-BUF.
           ADD 1 TO LN-WK.

           PERFORM SETUP-LINE.

           MOVE LN-BUF TO D-LN(B-SCREEN-LN).
           ADD 1 TO B-SCREEN-LN.
       END-SETUP-LINE-BUF.
           EXIT.

      *********************************
       SETUP-LINE SECTION.
      *********************************
           MOVE SPACES            TO LN-BUF.
           MOVE LN-WK             TO L-010.
           IF WK-STATUS = "D"
              MOVE "D"            TO L-DEL.
           MOVE SPACES            TO L-020. 
           MOVE WK-SN-FORM        TO L-030.
           MOVE WK-SN-SEQ         TO L-040.
           MOVE WK-SN-DESC        TO L-050.
           MOVE WK-SN-PURCHASABLE TO L-060.

      *******************************
       CLEAR-RECORD SECTION.
      *******************************
           MOVE SPACES TO WK-REC.
           MOVE 0      TO WK-SN-SEQ.

      *********************************
       LOAD-WORK-FILE SECTION.
      *********************************
           MOVE "LOADING WORK FILE...................." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           IF LIST-ORDER NOT = "N"
              PERFORM OPEN-SN2-FILE
              MOVE 0      TO SN-SEQ
              MOVE SPACES TO SN-FORM
              PERFORM START-SN2-FILE
           ELSE
              PERFORM OPEN-SN1-FILE
              MOVE SPACES TO SN-FORM
              PERFORM START-SN1-FILE.

           MOVE 0 TO B-LN B-TOP-LN B-SCREEN-LN.
           ADD 1 S-LNS-FREE-WK-RECS GIVING SUB.
       NEXT-WORK-LOAD.
           IF LIST-ORDER = "N"
              PERFORM READ-SN1-FILE-NEXT
           ELSE
              PERFORM READ-SN2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-LOAD-WORK-FILE.

           MOVE SUB TO WK-KEY.
           MOVE SN-REC TO WK-REC.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO SUB.
           ADD 1 TO B-LN.
           GO TO NEXT-WORK-LOAD.
       END-LOAD-WORK-FILE.
           PERFORM CLOSE-WK-FILE.
           IF B-LN > S-LNS
              PERFORM ADJUST-LINES-TO-END
           ELSE
              IF B-LN > 0
                 PERFORM SCROLL-DOWN
                 COMPUTE B-SCREEN-LN = 1 + B-LN - B-TOP-LN.
       EXIT-LOAD-WORK-FILE.
           EXIT.

      *********************************
       RECORD-UPDATE SECTION.
      *********************************
           MOVE "UPDATE IN PROGRESS..................." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-WK-FILE.
           MOVE S-LNS-FREE-WK-RECS TO SUB.
       NEXT-RECORD-UPDATE.
           ADD 1 TO SUB.
           MOVE SUB TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO END-RECORD-UPDATE.

           MOVE WK-SN-FORM TO SN-FORM.
           PERFORM READ-SN1-FILE.

           IF WK-STATUS = "D"
              GO TO UPD-DELETE.

           IF SN-REC = WK-SN-REC
              GO TO NEXT-RECORD-UPDATE.

           IF IO-BAD
              GO TO CREATE-NEW-SN.

           IF WK-SN-PURCHASABLE NOT = SN-PURCHASABLE
              MOVE WK-SN-FORM TO SE-FORM
              PERFORM OPEN-SE1-FILE
              PERFORM READ-SE1-FILE
              IF IO-GOOD
                 MOVE SE-REC            TO SEOR-REC
                 MOVE WK-SN-PURCHASABLE TO SE-PURCHASABLE
                 MOVE 0                 TO SE-EXP-DATE
                 PERFORM REWRITE-SE1-FILE
                 MOVE "C"            TO AUDITW-CHANGE-TYPE
                 PERFORM UPDATE-AUDITW-SEFILE
                 MOVE "N"            TO AUDITW-ENTER-TICKET.
      * ONLY WANT TO ENTER TICKET # ONCE IF MULTIPLE CHANGES

           MOVE SN-REC     TO SNOR-REC.
           MOVE WK-SN-SEQ  TO SN-SEQ.
           MOVE WK-SN-DESC TO SN-DESC.
           MOVE WK-SN-PURCHASABLE TO SN-PURCHASABLE.
           PERFORM REWRITE-SN1-FILE.
           MOVE "C"            TO AUDITW-CHANGE-TYPE.
           PERFORM UPDATE-AUDITW-SNFILE.
           MOVE "N"            TO AUDITW-ENTER-TICKET.
      * ONLY WANT TO ENTER TICKET # ONCE IF MULTIPLE CHANGES

           GO TO NEXT-RECORD-UPDATE.

       CREATE-NEW-SN.
           MOVE SN-REC    TO SNOR-REC.
           MOVE WK-SN-REC TO SN-REC.
           PERFORM WRITE-SN1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR
           ELSE
              MOVE "A"    TO AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW-SNFILE
              MOVE "N"    TO AUDITW-ENTER-TICKET.
          
           MOVE WK-SN-FORM TO SE-FORM.
           PERFORM OPEN-SE1-FILE.
           PERFORM READ-SE1-FILE
           IF IO-BAD
              MOVE SPACES         TO SE-REC
              MOVE SN-FORM        TO SE-FORM
              MOVE "YNNNNNNNNNNNNNNNNNNNNNNNNN"
                                  TO SE-PROFILE
              MOVE SN-PURCHASABLE TO SE-PURCHASABLE
              MOVE 0              TO SE-EXP-DATE
              PERFORM WRITE-SE1-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 MOVE SE-REC TO SEOR-REC
                 MOVE "A"    TO AUDITW-CHANGE-TYPE
                 PERFORM UPDATE-AUDITW-SEFILE
                 MOVE "N"    TO AUDITW-ENTER-TICKET.

           PERFORM CLOSE-SE1-FILE.

           GO TO NEXT-RECORD-UPDATE.
       UPD-DELETE.
           IF EXIT-PATH NOT = "GB/COMENU"
              GO TO NEXT-RECORD-UPDATE.
           IF IO-FG = 9
              GO TO NEXT-RECORD-UPDATE.
           MOVE SN-REC TO SNOR-REC.
           PERFORM DELETE-SN1-FILE.
           MOVE "D"    TO AUDITW-CHANGE-TYPE.
           PERFORM UPDATE-AUDITW-SNFILE.
           MOVE "N"    TO AUDITW-ENTER-TICKET.

           GO TO NEXT-RECORD-UPDATE.
       END-RECORD-UPDATE.
           PERFORM CLOSE-WK-FILE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************

           DISPLAY SPACES UPON SNMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SNMAIN-SCREEN UPON SNMAIN-WINDOW-HANDLE.
           MOVE SNMAIN-FORM-FIELDS TO WR-BUF.
           MOVE SNMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE SNMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE SNMAIN-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBMN/SNMAIN_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       DISPLAY-ALL-LINES.
           MOVE LINE-BUF TO WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAR-ALL-LINES.
           MOVE " " TO LINE-BUF.
           PERFORM DISPLAY-ALL-LINES.
       RESET-LIST.
           MOVE SPACES TO WR-BUF.
           MOVE MESS TO WR-LIST.
           PERFORM CALL-WRFORM.
       RESET-ENTRY-LINE.
           MOVE REV-MESS TO MESS.
           PERFORM RESET-LIST.
       RESET-SEL.
           MOVE "SEL MSEL." TO MESS.
           PERFORM RESET-LIST.
       SEND-ENTRY-LINE.
           MOVE EL-BUF TO WR-BUF.
           MOVE REV-MESS TO WR-LIST.
           PERFORM CALL-WRFORM.
       ADJUST-LINES-TO-END.
           COMPUTE LN-WK = B-LN - B-TOP-LN.
           IF LN-WK NOT < S-LNS
              SUBTRACT S-LNS-LESS1 FROM B-LN GIVING B-TOP-LN
              PERFORM SCROLL-UP.
       UPDATE-AUDITW-SEFILE.
           MOVE FORM-PATHNAME  TO FORM-PATH.
           MOVE FORM-PROGX     TO AUDITW-PROGRAM.
           MOVE "AUDITW"       TO FORM-NAM.
           MOVE SEOR-REC       TO AUDITW-ORIG-REC.
           MOVE SE-REC         TO AUDITW-CHG-REC.
           MOVE "SEFILE"       TO AUDITW-FILE-NAME.
           CALL FORM-PATH USING FORM-PATH AUDITW-WORKERS.
           CANCEL FORM-PATH.
       UPDATE-AUDITW-SNFILE.
           MOVE FORM-PATHNAME  TO FORM-PATH.
           MOVE FORM-PROGX     TO AUDITW-PROGRAM.
           MOVE "AUDITW"       TO FORM-NAM.
           MOVE SNOR-REC       TO AUDITW-ORIG-REC.
           MOVE SN-REC         TO AUDITW-CHG-REC.
           MOVE "SNFILE"       TO AUDITW-FILE-NAME.
           CALL FORM-PATH USING FORM-PATH AUDITW-WORKERS.
           CANCEL FORM-PATH.



      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBSNGS_SQL.CPY".
       COPY "LIBGB/GBSEGS_SQL.CPY".
       COPY "LIBGB/GBSN1IN.CPY".
       COPY "LIBGB/GBSN2RN.CPY".
       COPY "LIBGB/GBSE1IN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY SNMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
