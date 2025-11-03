       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PWMAIN.
       DATE-WRITTEN.    03/91.
      ******************************************************************
      *                 (GB)
      *  DESCRIPTION:   USER SECURITY PASSWORD MAINTENANCE
      *
      * REV:
      *  JTG 012394 OPENED UP SEQ == PROGRAM OPTION
      *  JTG 060995 ADDED PW-USER-PROFILE
      *  BAH 030698 COULDNT DELETE WITH JUST "D", PL# 171
      *  JTG 040525 ALLOWED (PAGE UP & PAGE DOWN) KEYS
      *  BLF 071019 ADDED ENTRY OF PW-EXCEPTION-FG & PW-ASK-FG, REMOVED
      *             FIELD 051 (PASSWORD) SINCE IT'S ENTERED IN A WINDOW,
      *             REGACC PR# 188
      *  BLM 101207 ADDED EXIT-PATH LOGIC SO PROGRAM WILL EXIT TO
      *             OLD SECURITY MENU (USMENU) OR NEW SECURITY MENU
      *             SEMENU, PR# NEWSECURITY
      *  BLM 130528 FORCE PASSWORD (PA-PP) TO UPPER CASE BEFORE
      *             CALLING ENCRYPTION ROUTINE
      *  BLM 140925 FIXED BUG, WHEN YOU ENTERED Y FOR REQUIRED, IF YOU
      *             ENTERED INCORRECT PASSWORD FOR OLD PASSWORD, IT LET
      *             YOU CONTINUE!
      * KEC 2015.1130 FIXED TO NOT USE PW-DIR ANYMORE.
      *               INCREASED PW-SEQ TO BE 3 BYTES.
      * BAH 20220218 CALL NEW AUDIT/CHANGE PROGRAM, AUDITW, #1542
      * BAH 2023.1006 CHANGED SCREEN FIELD PROFILE TO MUST Y TO ALLOW
      *               LOWERCASE
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


      ******************************************************************
      *
      *    W K - R E C
      *
      *=================================================================
      *    WK-REC IS IDENTICAL TO PW-REC.
      *    WK-XX-STATUS IS NOT A PART OF THE PW-REC.
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-PW1-KEY.
               05  WK-PW-PGM                   PIC X(10).
               05  WK-PW-SEQ                   PIC 9(03).
           03  WK-PW-LTOUCH-DATE               PIC 9(08)       COMP-3.
           03  WK-PW-PASSYN                    PIC X.
           03  WK-PW-PASSWORD                  PIC X(14).
           03  WK-PW-DESC                      PIC X(30).
           03  WK-PW-USER-PROFILE              PIC X.
           03  WK-PW-ASK-FG                    PIC X.
           03  WK-PW-EXCEPTION-FG              PIC X.
           03  WK-XX-STATUS                    PIC X(3).
 
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/PWMAIN S34 08/31/21-13:31:57 >".

       COPY "LIBGB/GB01PW.CPY".
       COPY "LIBGB/GB01PW_SQL.CPY".
       COPY "LIBGB/GBWSPW.CPY".
       01  WK-IFN          PIC X(2) VALUE "WK".
       01  WK-KEY          PIC 9(6).

       COPY "LIBGB/GB01PW.CPY" REPLACING LEADING "PW" BY "PWOR".

       01  PWMAIN-WINDOW-HANDLE  PIC X(10).
       01  ERRCD           PIC X VALUE " ".
       01  ONE             PIC S9.
       01  ELE             PIC 9(6).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  NEW-FLAG        PIC 9.
       01  UPDATE-FLAG     PIC 9  VALUE 0.

       01  SUB             PIC 9(6) COMP   VALUE 0.
       01  HOLD-PASSWORD   PIC X(8) VALUE SPACES.
       01  FIRST-PASSWORD  PIC X(8) VALUE SPACES.

      ************************************
      *     WORK LINE DISPLAY BUFFER     *
      ************************************
       01  ENTRY-LINE             PIC 9(4) VALUE 0.
       01  B-LN                   PIC 9(4) VALUE 0.
       01  B-TOP-LN               PIC 9(4) VALUE 0.
       01  B-SCREEN-LN            PIC 9(4) VALUE 0.
       01  LSUB                   PIC 9(6) COMP.
       01  LSUB2                  PIC 9(6) COMP.
       01  S-LNS                  PIC 9(6) COMP   VALUE 15.
       01  S-LNS-PLUS1            PIC 9(6) COMP   VALUE 16.
       01  S-LNS-LESS1            PIC 9(6) COMP   VALUE 14.
       01  S-LNSX2-LESS1          PIC 9(6) COMP   VALUE 29.
       01  S-LNS-FREE-WK-RECS     PIC 99          VALUE 0.
       01  LN-WK                  PIC 9(6) COMP.
       01  LINE-BUF.
           03  D-LN               OCCURS 15 TIMES.
               05  D-LN-NO        PIC 9(4).
               05  D-LN-BODY      PIC X(74).
               05  D-LN-DEL       PIC X.
               05  FILLER         PIC X.
       01  LINE-LIST.
           03  FILLER             PIC X(40) VALUE
           "01L 02L 03L 04L 05L 06L 07L 08L 09L 10L ".
           03  FILLER             PIC X(20) VALUE
           "11L 12L 13L 14L 15L.".

      **************************************
      *        ENTRY BAR LAYOUT
      **************************************
       01  LN-BUF                 VALUE SPACE.
           03  L-010              PIC 9(4).
           03  FILLER             PIC X(3).
           03  L-020              PIC X(2).
           03  FILLER             PIC X(3).
           03  L-030              PIC X(10).
           03  FILLER             PIC X.
           03  L-031              PIC ZZ9.
           03  FILLER             PIC X(2).
           03  L-040              PIC X(30).
           03  FILLER             PIC X(3).
           03  L-050              PIC X(1).
           03  FILLER             PIC X(3).
           03  L-060              PIC X.
           03  FILLER             PIC X(4).
           03  L-070              PIC X.
           03  FILLER             PIC X(3).
           03  L-080              PIC X.
           03  FILLER             PIC X(3).
           03  L-DEL              PIC X.
       01  REV-MESS               PIC X(39) VALUE
           "YN 080 070 060 050 040 031 030 020 010.".

      *************************************
      *    ENTRY LINE AUTO SETUP BUFFER   *
      *************************************
       01  EL-BUF                 VALUE SPACE.
           03  EL-YN              PIC X.
           03  EL-080             PIC X.
           03  EL-070             PIC X.
           03  EL-060             PIC X.
           03  EL-050             PIC X(1).
           03  EL-040             PIC X(30).
           03  EL-031             PIC ZZ9.
           03  EL-030             PIC X(10).
           03  EL-020             PIC X(2).
           03  EL-010             PIC 9(4).

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   VALUE 8.
       01  SPEC-TABLE.
           03  FILLER      PIC X(8) VALUE "ENNN040N".
           03  FILLER      PIC X(8) VALUE "E  A020N".
           03  FILLER      PIC X(8) VALUE "E  A100 ".
           03  FILLER      PIC X(8) VALUE "E  A300 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 8 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  PASSWORD-KEY        PIC X(14).
       01  PASSWORD-PROMPT1    PIC X(31)  VALUE " ".
       01  PASSWORD-PROMPT2    PIC X(31)  VALUE " ".

       01  LEGEND-01.
           03  FILLER                 PIC X(6)   VALUE "^-UP, ".
           03  FILLER                 PIC X      VALUE X'76'.
           03  FILLER                 PIC X(43)   VALUE
           "-DN, F6-ADD, ENTER LINE # TO CHANGE/DELETE:".

       COPY "LIBCL/AUDITWW.CPY".
       COPY "LIBGB/PASSARGS.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/PASSWDW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PWMAIN_DEF.CPY".
       COPY "LIBWI/PASSWD_DEF.CPY".
       COPY "LIBMN/PWMAIN_WKS.CPY".
       COPY "LIBWI/PASSWD_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/PWMAIN_SCN.CPY".
       COPY "LIBWI/PASSWD_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-PW1-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
       MAIN-PROGRAM SECTION.
      ***********************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "PWMAIN" TO VDU-FORM-NAME.
           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS PWMAIN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       BEGIN-IT.
           PERFORM LOAD-WORK-FILE.

       ENTER-RECORDS.
           MOVE "N" TO CHFG(1).
           PERFORM ENTRY-MODULE.
       UPDATE-YN.
           MOVE "F7-EXIT, F1-CANCEL, C-CHANGE, U-UPDATE:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA000SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              PERFORM CLEAR-ALL-LINES
              GO TO END-PROGRAM.
           IF VDUSTATUS = "10"
              PERFORM CLEAR-ALL-LINES
              GO TO BEGIN-IT.
           IF VDUSTATUS NOT = "00"
              GO TO UPDATE-YN.

           MOVE VDUBUF TO ACTION.
           IF ACTION = "     C"
              MOVE "SEL." TO MESS
              PERFORM RESET-MESS
              GO TO ENTER-RECORDS.
           IF ACTION NOT = "     U"
              GO TO UPDATE-YN.

           PERFORM RECORD-PW-CHANGES.

           PERFORM CLEAR-ALL-LINES.

           GO TO END-PROGRAM.

      **************************************
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
              MOVE LEGEND-01 TO MESS
           ELSE
              MOVE "F7-DONE, F6-AUDIT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE 1 TO ONE NEW-FLAG.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO ACCEPT-LINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       ACCEPT-LINE.
           IF CHFG(1) = "N"
              MOVE "F1-RESET, ^-BACK, A-ACCEPT:" TO MESS
           ELSE
              MOVE "F1-RESET, ^-BACK, D-DELETE, A-ACCEPT:"
                                                      TO MESS.
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
           IF NOT (VDUSTATUS = "00")
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
              PERFORM UPDATE-CHANGE
              GO TO BEGIN-ENTRY.
           IF CHFG(1) NOT = "N"
              IF VDUBUF = "D"
                 PERFORM DELETE-CHANGE
                 GO TO BEGIN-ENTRY
              ELSE
                 GO TO ACCEPT-LINE.
           GO TO ACCEPT-LINE.
       ENTER-ELE.
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
       TEST-UP.
           IF NOT (VDUSTATUS = "1U" OR "1P")
              GO TO TEST-DOWN.
           IF ELE = 1
              PERFORM SCROLL-DOWN
              GO TO ENTER-ELE.
           IF ELE = 2
              GO TO BEGIN-ENTRY.
           IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-DOWN.
           IF NOT (VDUSTATUS = "1D" OR "1N")
              GO TO TEST-F6.
           IF ELE = 1
              PERFORM SCROLL-PW
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
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08
              DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      ************************************
      *     S E T   E L E M E N T S      *
      ************************************
       F-01.
           MOVE VDUNUM0 TO ENTRY-LINE.
           IF ENTRY-LINE = 0
                 OR ENTRY-LINE > B-LN
              GO TO ENTER-ELE.
           IF B-LN > S-LNS-LESS1
              ADD B-TOP-LN S-LNS-LESS1 GIVING LN-WK
              IF ENTRY-LINE < B-TOP-LN
                     OR ENTRY-LINE > LN-WK
                 MOVE "PLEASE REVIEW LINE PRIOR TO ATTEMPTING THIS!"
                                                TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           MOVE ENTRY-LINE TO LN-WK.
           PERFORM SETUP-OLD-LINE.
           IF WK-PW-PASSYN = "N" OR "0" OR "1" OR "2" OR "3" OR "4"
                               OR "5" OR "6" OR "7" OR "8" OR "9"
              MOVE "N" TO CHFG(6)
                          CHFG(7)
                          CHFG(8)
           ELSE
              MOVE " " TO CHFG(6)
                          CHFG(7)
                          CHFG(8).
           GO TO CREATE-ROUTINE.
       F-02.
           GO TO CREATE-ROUTINE.
       F-03.
           IF VDUBUF = " "
              MOVE "S  A000030." TO VDU
              MOVE WK-PW-PGM TO VDUBUF
              PERFORM SCREENIO.
           IF UPDATE-FLAG = 1
              IF VDUBUF NOT = WK-PW-PGM
                 MOVE "CAN'T CHANGE - MUST DELETE & RE-ADD!"
                                                      TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE VDUBUF TO WK-PW-PGM.

       F-031.
           MOVE "ENNN030031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-031.
           MOVE VDUNUM0 TO WK-PW-SEQ.
           GO TO CREATE-ROUTINE.
       F-04.
           IF VDUBUF = " "
              MOVE "S  A000040." TO VDU
              MOVE WK-PW-DESC TO VDUBUF
              PERFORM SCREENIO.
           MOVE VDUBUF TO WK-PW-DESC.
           GO TO CREATE-ROUTINE.
       F-05.
           IF WK-PW-PGM = "COMMON"
              IF NOT (VDUBUF = "Y" OR "N" OR "#")
                 MOVE "PLEASE ENTER Y OR N !!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 NEXT SENTENCE
           ELSE
              IF NOT (VDUBUF = "Y" OR "N" OR "0" OR "1" OR "2"
                             OR "3" OR "4" OR "5" OR "6" OR "7"
                              OR "8" OR "9" OR "#")
                 MOVE "PLEASE ENTER Y OR N OR 0 THRU 9!!!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           IF VDUBUF = "#"
              MOVE "S  A010050." TO VDU
              MOVE WK-PW-PASSYN TO VDUBUF
              PERFORM SCREENIO
              IF UPDATE-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO ENTER-ELE.

           MOVE VDUBUF TO WK-PW-PASSYN.
           IF WK-PW-PASSYN = "N" OR "0" OR "1" OR "2" OR "3" OR "4"
                               OR "5" OR "6" OR "7" OR "8" OR "9"
              IF WK-PW-PASSWORD = " "
                 GO TO CREATE-ROUTINE.
           PERFORM ENTER-NEW-PASSWORD.
           IF STAT NOT = "00"
              GO TO ENTER-ELE.
           IF WK-PW-PASSYN = "N" OR "0" OR "1" OR "2" OR "3" OR "4"
                               OR "5" OR "6" OR "7" OR "8" OR "9"
              MOVE "S  A010060." TO VDU
              MOVE " " TO VDUBUF
                          WK-PW-USER-PROFILE
                          WK-PW-EXCEPTION-FG
                          WK-PW-ASK-FG
              PERFORM SCREENIO
              MOVE "N" TO CHFG(6)
                          CHFG(7)
                          CHFG(8)
           ELSE
              MOVE " " TO CHFG(6)
                          CHFG(7)
                          CHFG(8).
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO WK-PW-USER-PROFILE.
           IF WK-PW-USER-PROFILE = " "
              GO TO CREATE-ROUTINE.
           IF WK-PW-USER-PROFILE < X"60"
                    OR WK-PW-USER-PROFILE > X"7A"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-07.
           IF NOT (VDUBUF = "Y" OR "N")
              MOVE "PLEASE ENTER Y OR N !" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO WK-PW-EXCEPTION-FG.
           GO TO CREATE-ROUTINE.
       F-08.
           IF NOT (VDUBUF = "Y" OR "N" OR "Q")
              MOVE "PLEASE ENTER Y OR N OR Q!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO WK-PW-ASK-FG.
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
       UPDATE-CHANGE SECTION.
      *********************************
           ADD ENTRY-LINE S-LNS-FREE-WK-RECS GIVING WK-KEY.
           MOVE " " TO WK-XX-STATUS.
           PERFORM SETUP-LINE.
           COMPUTE ENTRY-LINE = ENTRY-LINE - B-TOP-LN + 1.
           MOVE "S  A00000L." TO VDU.
           MOVE ENTRY-LINE TO VDUROW.
           MOVE LN-BUF TO VDUBUF D-LN(ENTRY-LINE).
           PERFORM SCREENIO.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           MOVE " " TO WK-XX-STATUS.
           PERFORM REWRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-WK-FILE.

      *********************************
       DELETE-CHANGE SECTION.
      *********************************
           ADD S-LNS-FREE-WK-RECS ENTRY-LINE GIVING WK-KEY.
           MOVE "D" TO WK-XX-STATUS.
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
           MOVE "D" TO WK-XX-STATUS.
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

           MOVE SPACES             TO EL-BUF.
           MOVE ENTRY-LINE         TO EL-010.
           MOVE SPACES             TO EL-020.    
           MOVE WK-PW-PGM          TO EL-030.
           MOVE WK-PW-SEQ          TO EL-031.
           MOVE WK-PW-DESC         TO EL-040.
           MOVE WK-PW-PASSYN       TO EL-050.
           MOVE WK-PW-USER-PROFILE TO EL-060.
           MOVE WK-PW-EXCEPTION-FG TO EL-070.
           MOVE WK-PW-ASK-FG       TO EL-080.
           MOVE "A"                TO EL-YN.
           PERFORM SEND-ENTRY-LINE.

           MOVE 1 TO UPDATE-FLAG.

      *********************************
       SCROLL-DOWN SECTION.
      *********************************

      * INSERT THIS CHECK WHEN THIS ROUTINE IS NOT USED IN LOAD-WORK-FILE
      *    IF B-TOP-LN NOT > 1
      *       GO TO END-SCROLL-DOWN.

           IF B-TOP-LN > S-LNS
              SUBTRACT S-LNS-PLUS1 FROM B-TOP-LN GIVING LN-WK
            ELSE
              MOVE 0 TO LN-WK.
           MOVE 1 TO B-SCREEN-LN.
           MOVE " " TO LINE-BUF.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           PERFORM SETUP-LINE-BUF UNTIL B-SCREEN-LN > S-LNS.
           PERFORM CLOSE-WK-FILE.
           PERFORM DISPLAY-ALL-LINES.
           MOVE D-LN-NO(1) TO B-TOP-LN.

       END-SCROLL-DOWN.
           EXIT.

      *********************************
       SCROLL-PW SECTION.
      *********************************
           IF B-LN < S-LNS-PLUS1
              GO TO END-SCROLL-PW.

           ADD B-TOP-LN S-LNSX2-LESS1 GIVING LN-WK.
           IF LN-WK > B-LN
              MOVE B-LN TO LN-WK.
           SUBTRACT S-LNS FROM LN-WK.
           MOVE 1 TO B-SCREEN-LN.
           MOVE " " TO LINE-BUF.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           PERFORM SETUP-LINE-BUF UNTIL B-SCREEN-LN > S-LNS.
           PERFORM CLOSE-WK-FILE.
           PERFORM DISPLAY-ALL-LINES.
           MOVE D-LN-NO(1) TO B-TOP-LN.

       END-SCROLL-PW.
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
           MOVE SPACES             TO LN-BUF.
           MOVE LN-WK              TO L-010.
           IF WK-XX-STATUS = "D"
              MOVE "D"             TO L-DEL.
           MOVE SPACES             TO L-020.
           MOVE WK-PW-PGM          TO L-030.
           MOVE WK-PW-SEQ          TO L-031.
           MOVE WK-PW-DESC         TO L-040.
           MOVE WK-PW-PASSYN       TO L-050.
           MOVE WK-PW-USER-PROFILE TO L-060.
           MOVE WK-PW-EXCEPTION-FG TO L-070.
           MOVE WK-PW-ASK-FG       TO L-080.

      *******************************
       CLEAR-RECORD SECTION.
      *******************************
           MOVE SPACES TO WK-REC.
           MOVE 0      TO WK-PW-SEQ.

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

           PERFORM OPEN-PW1-FILE.
           MOVE SPACES TO PW-PGM.
           MOVE 0      TO PW-SEQ.
           PERFORM START-PW1-FILE.

           MOVE 0 TO B-LN B-TOP-LN B-SCREEN-LN.
           ADD 1 S-LNS-FREE-WK-RECS GIVING SUB.
       NEXT-WORK-LOAD.
           PERFORM READ-PW1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-LOAD-WORK-FILE.

           MOVE SUB TO WK-KEY.
           MOVE PW-REC TO WK-REC.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           ADD 1 TO SUB
                    B-LN.
           GO TO NEXT-WORK-LOAD.
       END-LOAD-WORK-FILE.
           PERFORM CLOSE-PW1-FILE.
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
       RECORD-PW-CHANGES SECTION.
      *********************************
           MOVE "UPDATE IN PROGRESS..................." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-WK-FILE.
           PERFORM OPEN-PW1-FILE.

           MOVE S-LNS-FREE-WK-RECS TO SUB.
       RECORD-PW-CHANGES-NEXT.
           ADD 1 TO SUB.
           MOVE SUB TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO END-RECORD-PW-CHANGES.

           MOVE WK-PW-PGM TO PW-PGM.
           MOVE WK-PW-SEQ TO PW-SEQ.
           PERFORM READ-PW1-FILE.
           IF WK-XX-STATUS = "D"
              GO TO RECORD-PW-CHANGES-DELETE.
           IF IO-FG = 0
              MOVE PW-REC             TO PWOR-REC
              MOVE WK-PW-DESC         TO PW-DESC
              MOVE WK-PW-PASSWORD     TO PW-PASSWORD
              MOVE WK-PW-PASSYN       TO PW-PASSYN
              MOVE WK-PW-USER-PROFILE TO PW-USER-PROFILE
              MOVE WK-PW-EXCEPTION-FG TO PW-EXCEPTION-FG
              MOVE WK-PW-ASK-FG       TO PW-ASK-FG
              PERFORM REWRITE-PW1-FILE
              MOVE "C"                TO AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW
              MOVE "N"                TO AUDITW-ENTER-TICKET
      * ONLY WANT TO ENTER TICKET # ONCE IF MULITPLE CHANGES
              GO TO RECORD-PW-CHANGES-NEXT.

           MOVE WK-REC TO PW-REC.
           PERFORM WRITE-PW1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR
           ELSE
              MOVE PW-REC TO PWOR-REC
              MOVE "A"    TO AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW
              MOVE "N"    TO AUDITW-ENTER-TICKET.

           GO TO RECORD-PW-CHANGES-NEXT.

       RECORD-PW-CHANGES-DELETE.
           IF IO-FG = 9
              GO TO RECORD-PW-CHANGES-NEXT.
           MOVE PW-REC TO PWOR-REC.
           PERFORM DELETE-PW1-FILE.
           MOVE "D"    TO AUDITW-CHANGE-TYPE.
           PERFORM UPDATE-AUDITW.
           MOVE "N"    TO AUDITW-ENTER-TICKET.

           GO TO RECORD-PW-CHANGES-NEXT.

       END-RECORD-PW-CHANGES.
           PERFORM CLOSE-PW1-FILE.
           PERFORM CLOSE-WK-FILE.

      **********************************************
       ENTER-NEW-PASSWORD SECTION.
      **********************************************
           IF WK-PW-PASSYN = "Y"
              IF WK-PW-PASSWORD = " "
                 GO TO PASS-CHANGE.
           IF WK-PW-PASSYN = "N" OR "0" OR "1" OR "2" OR "3"
                               OR "4" OR "5" OR "6" OR "7"
                                OR "8" OR "9"
              IF WK-PW-PASSWORD = " "
                 MOVE "00" TO STAT
                 GO TO EXIT-ENTER-PASS.

           MOVE "PLEASE ENTER OLD PASSWORD FOR" TO PASSWORD-PROMPT1.
           MOVE WK-PW-PGM TO PASSWORD-PROMPT2.
           PERFORM ENTER-PASSWORD.
           MOVE SPACES TO PASSARGS.
           MOVE HOLD-PASSWORD TO PA-PP.
           CALL "C$TOUPPER" USING PA-PP, VALUE 10.
           MOVE WK-PW-PASSWORD TO PA-ENCRYPTED.
           PERFORM PASSWORD-CALL.
           IF PA-STAT NOT = "00"
              MOVE "09" TO STAT
              MOVE "PASSWORD DOES NOT MATCH EXISTING PASSWORD!"
                TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-ENTER-PASS.
           IF WK-PW-PASSYN = "N" OR "0" OR "1" OR "2" OR "3"
                               OR "4" OR "5" OR "6" OR "7"
                                OR "8" OR "9"
              MOVE " " TO WK-PW-PASSWORD
              MOVE " PASSWORD REMOVED" TO MESS
              PERFORM SEND-MESS
              MOVE "00" TO STAT
              GO TO EXIT-ENTER-PASS.

       PASS-CHANGE.
           MOVE "     PLEASE ENTER THE NEW" TO PASSWORD-PROMPT1.
           MOVE "     PASSWORD............" TO PASSWORD-PROMPT2.
           MOVE " " TO PASSWORD-KEY.
           PERFORM ENTER-PASSWORD.
           MOVE HOLD-PASSWORD TO FIRST-PASSWORD.

           MOVE "     TO BE SURE, RE-ENTER" TO PASSWORD-PROMPT1.
           MOVE "     THE NEW PASSWORD...." TO PASSWORD-PROMPT2.
           PERFORM ENTER-PASSWORD.

           IF HOLD-PASSWORD NOT = FIRST-PASSWORD
              MOVE "PASSWORDS DIDN'T MATCH--TRY AGAIN" TO MESS
              PERFORM SEND-MESS
              GO TO PASS-CHANGE.

           MOVE SPACES TO PASSARGS.
           MOVE HOLD-PASSWORD TO PA-PP.
           CALL "C$TOUPPER" USING PA-PP, VALUE 10.
           PERFORM PASSWORD-CALL.
           IF PA-STAT = "00"
              MOVE PA-ENCRYPTED TO WK-PW-PASSWORD
              MOVE "PASSWORD ACCEPTED" TO MESS
              PERFORM SEND-MESS.
              MOVE "00" TO STAT.

       EXIT-ENTER-PASS.
           EXIT.

      ***********************************************************
       ENTER-PASSWORD SECTION.
      ***********************************************************
           MOVE 4 TO WINDOW-LINES.
           MOVE 35 TO WINDOW-COLUMNS.
           MOVE 10 TO WINDOW-BEGIN-Y.
           MOVE 26 TO WINDOW-BEGIN-X.
           MOVE "PASSWD" TO VDU-FORM-NAME.
           MOVE "WI/PASSWD" TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY PASSWD-SCREEN.

           MOVE PASSWD-FORM-FIELDS TO WR-BUF.
      **** MOVE PASSWD-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "S  A000PWPROMPT1." TO VDU.
           MOVE PASSWORD-PROMPT1 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000PWPROMPT2." TO VDU.
           MOVE PASSWORD-PROMPT2 TO VDUBUF.
           PERFORM SCREENIO.

       PASSWORD-ENTRY.
           MOVE "F7-EXIT, ENTER PASSWD" TO MESS.
      *    PERFORM SEND-WSEL-LEGEND.

           MOVE "E  A080PW." TO VDU.
           PERFORM SCREENIO.
           IF VDUBUF = SPACES
              MOVE "(ENTER) IS NOT A SECURE PASSWORD" TO MESS
              PERFORM SEND-MESS
              GO TO PASSWORD-ENTRY.
           IF VDUSTATUS NOT = "00" GO TO PASSWORD-ENTRY.

           MOVE VDUBUF TO HOLD-PASSWORD.

       EXIT-PASSWORD-ENTRY.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PASSWD-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "PWMAIN" TO VDU-FORM-NAME.

      ********************************************************
       PASSWORD-CALL SECTION.
      ********************************************************
           MOVE "GB/ENCRYP"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSARGS.
           CANCEL FORM-PROGX.

      ********************************************************
       FORM-RESET SECTION.
      ********************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PWMAIN-SCREEN UPON PWMAIN-WINDOW-HANDLE.
           MOVE PWMAIN-FORM-FIELDS TO WR-BUF.
           MOVE PWMAIN-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           MOVE PWMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE PWMAIN-HELP--DIR  TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PWMAIN_EVA.CPY".
              COPY "LIBWI/PASSWD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/DATER.CPY".

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       UPDATE-AUDITW.
           MOVE FORM-PATHNAME  TO FORM-PATH.
           MOVE FORM-PROGX     TO AUDITW-PROGRAM.
           MOVE "AUDITW"       TO FORM-NAM.
           MOVE PWOR-REC       TO AUDITW-ORIG-REC.
           MOVE PW-REC         TO AUDITW-CHG-REC.
           MOVE "PWFILE"       TO AUDITW-FILE-NAME.
           CALL FORM-PATH USING FORM-PATH AUDITW-WORKERS.
           CANCEL FORM-PATH.

       SEND-LEGEND.
           MOVE "S  A600MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       DISPLAY-ALL-LINES.
           MOVE LINE-LIST TO WR-LIST.
           MOVE LINE-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.
       CLEAR-ALL-LINES.
           MOVE " " TO LINE-BUF.
           PERFORM DISPLAY-ALL-LINES.
       RESET-LIST.
           PERFORM FORM-RESET.
       RESET-ENTRY-LINE.
           MOVE REV-MESS TO WR-LIST.
           MOVE SPACES TO LN-BUF.
           MOVE LN-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.
       RESET-SEL.
           MOVE "SEL MSEL." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
       SEND-ENTRY-LINE.
           MOVE REV-MESS TO WR-LIST.
           MOVE EL-BUF   TO WR-BUF.
           PERFORM CALL-WRFORM.
       ADJUST-LINES-TO-END.
           COMPUTE LN-WK = B-LN - B-TOP-LN.
           IF LN-WK NOT < S-LNS
              SUBTRACT S-LNS-LESS1 FROM B-LN GIVING B-TOP-LN
              PERFORM SCROLL-PW.


      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBPWGS_SQL.CPY".
       COPY "LIBGB/GBPW1IN.CPY".
       COPY "LIBGB/GBWKIN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-PROG.
           PERFORM CLOSE-FILES.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           MOVE EXIT-PATHNAME TO FORM-PATH.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PWMAIN-SCREEN.
           DESTROY PWMAIN-WINDOW-HANDLE.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
           EXIT PROGRAM.
