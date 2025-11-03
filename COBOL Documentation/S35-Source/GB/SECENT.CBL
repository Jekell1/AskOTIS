       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SECENT.
       DATE-WRITTEN.    111510.
      ******************************************************************
      *
      *                 (MN)
      *  DESCRIPTION:   USER SECURITY PROFILE MAINTENANCE
      *
      *=================================================================
      * REV:
      * KEC 2015.1112 REMOVED USE OF SN-DIR & SE-DIR
      * BAH 2022.0218 CALL NEW AUDIT/CHANGE PROGRAM, AUDITW, #1542
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
           03  WK-FORM              PIC X(6).
           03  WK-EXP-DATE-MMDDYY   PIC 9(6).
           03  WK-PURCHASABLE       PIC X.
           03  WK-PROF              PIC X.
           03  WK-STATUS            PIC X.

      ************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/SECENT.CBL S34 05/24/21-11:49:53 >".
      ************************************************
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/SECENT.CBL A30 11/13/15-10:46:52 >".
       COPY "LIBGB/GB01SE.CPY".
       COPY "LIBGB/GB01SE_SQL.CPY".
       COPY "LIBGB/GBWSSE.CPY".
       COPY "LIBGB/GB01SN.CPY".
       COPY "LIBGB/GB01SN_SQL.CPY".
       COPY "LIBGB/GBWSSN.CPY".

       COPY "LIBGB/GB01SE.CPY" REPLACING LEADING "SE" BY "SEOR".

       01  WK-IFN          PIC X(4) VALUE "0CWK".
       01  WK-KEY          PIC 9(6).

       01  ERRCD           PIC X    VALUE " ".

       01  ONE             PIC S9.
       01  ELE             PIC 9(6).
       01  ACTION          PIC X(6).
       01  NEW-FLAG        PIC 9.
       01  UPDATE-FLAG     PIC 9  VALUE 0.

       01  SUB             PIC 9(6) COMP-3   VALUE 0.
       01  SECURITY-PROF   PIC X    VALUE " ".

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
      *        ENTRY BAR LAYOUP            *
      **************************************
       01  LN-BUF                 VALUE SPACE.
           03  L-010              PIC 9(4).
           03  FILLER             PIC X(3).
           03  L-020              PIC X(2).
           03  FILLER             PIC X(3).
           03  L-030              PIC X(6).
           03  FILLER             PIC X(4).
           03  L-040              PIC X(30).
           03  FILLER             PIC X(4).
           03  L-050              PIC X.
           03  FILLER             PIC X(4).
           03  L-060              PIC 99/99/99 BLANK ZERO.
           03  FILLER             PIC X(4).
           03  L-070              PIC X.
           03  FILLER             PIC X(4).
           03  L-DEL              PIC X.
           03  FILLER             PIC X.

       01  REV-LIST               PIC X(31) VALUE
           "YN 070 060 050 040 030 020 010.".

      *************************************
      *    ENTRY LINE AUPO SETUP BUFFER   *
      *************************************
       01  EL-BUF                 VALUE SPACE.
           03  EL-YN              PIC X.
           03  EL-070             PIC X.
           03  EL-060             PIC 99/99/99.
           03  EL-050             PIC X.
           03  EL-040             PIC X(30).
           03  EL-030             PIC X(6).
           03  EL-020             PIC X(2).
           03  EL-010             PIC 9(4).

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   VALUE 7.
       01  SPEC-TABLE.
           03  FILLER      PIC X(8) VALUE "EN N040N".
           03  FILLER      PIC X(8) VALUE "E  A020N". 
           03  FILLER      PIC X(8) VALUE "E  A060 ".
           03  FILLER      PIC X(8) VALUE "E  A300N".
           03  FILLER      PIC X(8) VALUE "E  A010N".
           03  FILLER      PIC X(8) VALUE "ERNM060N".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 7 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBCL/AUDITWW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  SECENT-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/SECENT_DEF.CPY".
       COPY "LIBGB/SECENT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGB/SECENT_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SN1-FILE.
           PERFORM CLOSE-SE1-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SECENT" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SECENT-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM OPEN-SN1-FILE.

       ENTER-SECURITY-PROF.
           MOVE "ENTER PROFILE (B-Z) :" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PROF." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SECURITY-PROF.
           IF VDUBUF < "B" OR VDUBUF > "Z"
              GO TO ENTER-SECURITY-PROF.
           MOVE VDUBUF TO SECURITY-PROF.

           PERFORM SET-PROFILE-SUB.

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
              GO TO ENTER-SECURITY-PROF.
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

           GO TO ENTER-SECURITY-PROF.

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

           MOVE SPACES TO WK-REC.

       BEGIN-ENTRY.
           IF CHFG(1) = " "
              PERFORM RESET-ENTRY-LINE
              MOVE "^-UP, V-DN, F6-ADD, AUDIT LINE#" TO MESS
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
           IF (ELE > 3 AND ELE < 7)
              GO TO CREATE-ROUTINE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       ACCEPT-LINE.
           IF CHFG(1) = "N"
              MOVE "^-BACK, A-ACCEPT" TO MESS
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
           GO TO ACCEPT-LINE.

       ENTER-ELE.

      * SKIP OVER ASKING DIRECTORY - NO LONGER USED IN SE & SN
      
           IF ( ELE = 2 )                            
              PERFORM RESET-SEL                      
              MOVE SPEC(ELE)         TO VDU          
              MOVE "S"               TO VDUACTION    
              MOVE ELE               TO VDUROW       
              MOVE 0                 TO VDUCOL       
              MOVE "."               TO VDUDECIMAL   
              MOVE SPACES            TO VDUBUF       
              PERFORM SCREENIO                       
              GO TO TEST-F1.                         

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
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
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
      * SKIP OVER ASKING DIRECTORY - NO LONGER USED IN SE & SN
           GO TO CREATE-ROUTINE.
       F-03.
           IF UPDATE-FLAG = 1 AND VDUBUF NOT = WK-FORM
             MOVE "CANNOT CHANGE"
               TO MESS
             PERFORM SEND-MESS
             GO TO ENTER-ELE.
           MOVE VDUBUF TO WK-FORM.
           IF WK-FORM = " "
              GO TO ENTER-ELE.
      * DONT VALIDATE ON A 'DELETE'
           IF CHFG(1) NOT = " "
              PERFORM GET-SN1
              IF IO-FG = 9
                 MOVE "INVALID TRANSACTION" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           MOVE "S  A300040." TO VDU.
           MOVE SN-DESC TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-04.
           GO TO CREATE-ROUTINE.
       F-05.
           GO TO CREATE-ROUTINE.
       F-06.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO WK-PROF.
           IF WK-PROF NOT = "Y"
              IF WK-PROF NOT = "N"
                 MOVE "PLEASE ENTER 'Y' OR 'N'!" TO MESS
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
           MOVE WK-FORM            TO EL-030.
           PERFORM GET-SN1.
           MOVE SN-DESC            TO EL-040.
           MOVE WK-PURCHASABLE     TO EL-050.
           MOVE WK-EXP-DATE-MMDDYY TO EL-060.
           MOVE WK-PROF            TO EL-070.
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

           MOVE 1      TO B-SCREEN-LN.
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

           MOVE SPACES TO LN-BUF.

           MOVE LN-WK              TO L-010.
           IF WK-STATUS = "D"
              MOVE "D"             TO L-DEL.
           MOVE SPACES             TO L-020.
           MOVE WK-FORM            TO L-030.
           PERFORM GET-SN1.
           MOVE SN-DESC            TO L-040.
           MOVE WK-PURCHASABLE     TO L-050.
           MOVE WK-EXP-DATE-MMDDYY TO L-060.
           MOVE WK-PROF            TO L-070.


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

           MOVE SPACES TO SE1-KEY.

           PERFORM OPEN-SE1-FILE.
           PERFORM START-SE1-FILE.

           MOVE 0 TO B-LN B-TOP-LN B-SCREEN-LN.
           ADD 1 S-LNS-FREE-WK-RECS GIVING SUB.
       NEXT-WORK-LOAD.
           PERFORM READ-SE1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-LOAD-WORK-FILE.

           MOVE SUB TO WK-KEY.
           MOVE SE-FORM        TO WK-FORM.
           MOVE SE-PROF(SESUB) TO WK-PROF.
           MOVE SE-PURCHASABLE TO WK-PURCHASABLE.
           MOVE SE-EXP-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO WK-EXP-DATE-MMDDYY.

           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO SUB.
           ADD 1 TO B-LN.

           GO TO NEXT-WORK-LOAD.
       END-LOAD-WORK-FILE.
           PERFORM CLOSE-SE1-FILE.
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
           PERFORM OPEN-SE1-FILE.

           MOVE S-LNS-FREE-WK-RECS TO SUB.
       NEXT-RECORD-UPDATE.
           ADD 1 TO SUB.
           MOVE SUB TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO END-RECORD-UPDATE.

           MOVE WK-FORM TO SE-FORM.
           PERFORM READ-SE1-FILE.
           IF IO-GOOD
              MOVE SE-REC  TO SEOR-REC
              MOVE WK-PROF TO SE-PROF(SESUB)
              PERFORM REWRITE-SE1-FILE
              IF SE-REC NOT = SEOR-REC
                 MOVE "C" TO AUDITW-CHANGE-TYPE
                 PERFORM UPDATE-AUDITW
      * ONLY ENTER TICKET ONCE FOR CHANGES
                 MOVE "N" TO AUDITW-ENTER-TICKET
                 GO TO NEXT-RECORD-UPDATE
              ELSE
                 GO TO NEXT-RECORD-UPDATE.

           MOVE WK-FORM TO SN-FORM.
           PERFORM READ-SN1-FILE.
           IF IO-BAD
              MOVE "N" TO SN-PURCHASABLE.

           MOVE SPACES         TO SE-REC.
           MOVE WK-FORM        TO SE-FORM.
           MOVE "YNNNNNNNNNNNNNNNNNNNNNNNNN"
                               TO SE-PROFILE.
           MOVE WK-PROF        TO SE-PROF(SESUB).
           MOVE SN-PURCHASABLE TO SE-PURCHASABLE.
           MOVE 0              TO SE-EXP-DATE.
           PERFORM WRITE-SE1-FILE.
           IF IO-BAD
              GO TO IO-ERROR
           ELSE
              MOVE SE-REC TO SEOR-REC
              MOVE "A"    TO AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW
      * ONLY ENTER TICKET ONCE FOR CHANGES
              MOVE "N" TO AUDITW-ENTER-TICKET.


           GO TO NEXT-RECORD-UPDATE.
       END-RECORD-UPDATE.
           PERFORM CLOSE-SE1-FILE.
           PERFORM CLOSE-WK-FILE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           DISPLAY SECENT-SCREEN UPON SECENT-WINDOW-HANDLE.
           MOVE SECENT-FORM-FIELDS TO WR-BUF.
           MOVE SECENT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SECENT-HELP--FILE TO HELP-LINK-FILE.
           MOVE SECENT-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/SECENT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       UPDATE-AUDITW.
           MOVE FORM-PATHNAME  TO FORM-PATH.
           MOVE FORM-PROGX     TO AUDITW-PROGRAM.
           MOVE "AUDITW"       TO FORM-NAM.
           MOVE SEOR-REC       TO AUDITW-ORIG-REC.
           MOVE SE-REC         TO AUDITW-CHG-REC.
           MOVE "SEFILE"       TO AUDITW-FILE-NAME.
           CALL FORM-PATH USING FORM-PATH AUDITW-WORKERS.
           CANCEL FORM-PATH.
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
           MOVE MESS   TO WR-LIST.
           PERFORM CALL-WRFORM.
       RESET-ENTRY-LINE.
           MOVE REV-LIST TO MESS.
           PERFORM RESET-LIST.
       RESET-SEL.
           MOVE "SEL MSEL." TO MESS.
           PERFORM RESET-LIST.
       SEND-ENTRY-LINE.
           MOVE EL-BUF TO WR-BUF.
           MOVE REV-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       ADJUST-LINES-TO-END.
           COMPUTE LN-WK = B-LN - B-TOP-LN.
           IF LN-WK NOT < S-LNS
              SUBTRACT S-LNS-LESS1 FROM B-LN GIVING B-TOP-LN
              PERFORM SCROLL-UP.
       GET-SN1.
           MOVE WK-FORM TO SN-FORM.
           PERFORM READ-SN1-FILE.
       SET-PROFILE-SUB.
           EVALUATE SECURITY-PROF
              WHEN 'A' MOVE 1 TO SESUB
              WHEN 'B' MOVE 2 TO SESUB
              WHEN 'C' MOVE 3 TO SESUB
              WHEN 'D' MOVE 4 TO SESUB
              WHEN 'E' MOVE 5 TO SESUB
              WHEN 'F' MOVE 6 TO SESUB
              WHEN 'G' MOVE 7 TO SESUB
              WHEN 'H' MOVE 8 TO SESUB
              WHEN 'I' MOVE 9 TO SESUB
              WHEN 'J' MOVE 10 TO SESUB
              WHEN 'K' MOVE 11 TO SESUB
              WHEN 'L' MOVE 12 TO SESUB
              WHEN 'M' MOVE 13 TO SESUB
              WHEN 'N' MOVE 14 TO SESUB
              WHEN 'O' MOVE 15 TO SESUB
              WHEN 'P' MOVE 16 TO SESUB
              WHEN 'Q' MOVE 17 TO SESUB
              WHEN 'R' MOVE 18 TO SESUB
              WHEN 'S' MOVE 19 TO SESUB
              WHEN 'T' MOVE 20 TO SESUB
              WHEN 'U' MOVE 21 TO SESUB
              WHEN 'V' MOVE 22 TO SESUB
              WHEN 'W' MOVE 23 TO SESUB
              WHEN 'X' MOVE 24 TO SESUB
              WHEN 'Y' MOVE 25 TO SESUB
              WHEN 'Z' MOVE 26 TO SESUB.
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBSNGS_SQL.CPY".
       COPY "LIBGB/GBSEGS_SQL.CPY".
       COPY "LIBGB/GBSE1IN.CPY".
       COPY "LIBGB/GBSN1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           MOVE "GB/SEMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SECENT-SCREEN.
           DESTROY SECENT-WINDOW-HANDLE.
           EXIT PROGRAM.
