       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       MEMOSP.
       AUTHOR.              MJD.
       DATE-WRITTEN. 2000/09/27.
      ******************************************************************
      *          STATE PARAMETER MEMO INQUIRY / INPUT
      *
      * REV:
      * KEC 2015.1111 INCREASED SM-SEQ FROM 2 TO 3.
      * KEC 2015.1204 REMOVED USE OF EXT-WI-CL-SP-KEY; USING MEMOSP-SP1-KEY
      *               CHANGED USE OF SM-MEMO-????-KEY TO BE ????-SM1-KEY
      * BAH 170420 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2024.0418 CHANGED TO CALL INSTEAD OF WINDOW
      *
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/MEMOSP S35 01/17/23-12:25:29 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01SM.CPY".
       COPY "LIBLP/LP01SM_SQL.CPY".
       COPY "LIBLP/LPWSSM.CPY".

       01  MEMOSP-LINES                PIC 99  VALUE 13.
       01  MEMOSP-COLUMNS              PIC 99  VALUE 80.
       01  MEMOSP-BEGIN-Y              PIC 99  VALUE 1.
       01  MEMOSP-BEGIN-X              PIC 99  VALUE 5.

       01  ERRCD                 PIC X VALUE " ".
       01  MEMO-ENTERED-FG       PIC X      VALUE "N".
       01  LEGEND                PIC X(78).
       01  LEGEND-LIST           PIC X(7)    VALUE "MSEL:D.".

      *=================================================================
      * KEC 2015.1204 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED USE OF EXT-WI-CL-SP-KEY; USING MEMOSP-SP1-KEY
      *          CHANGED USE OF SM-MEMO-????-KEY TO BE ????-SM1-KEY
      *-----------------------------------------------------------------
       01  HOLD-SM1-KEY.
           03  HOLD-SM-SP1-KEY                 PIC X(09).
           03  HOLD-SM-DATE                    PIC 9(08).
           03  HOLD-SM-SEQ                     PIC 9(03).
       01  LAST-SM1-KEY.
           03  LAST-SM-SP1-KEY                 PIC X(09).
           03  LAST-SM-DATE                    PIC 9(08).
           03  LAST-SM-SEQ                     PIC 9(03).
       01  LINE1-SM1-KEY.
           03  LINE1-SM-SP1-KEY                PIC X(09).
           03  LINE1-SM-DATE                   PIC 9(08).
           03  LINE1-SM-SEQ                    PIC 9(03).

      *=================================================================
       01  SAVE-BEG-SM-SEQ         PIC 9(3)    VALUE 0.
       01  SAVE-END-SM-SEQ         PIC 9(3)    VALUE 0.

       01  SM-MSG1.
           03  FILLER            PIC X(12) VALUE
           "LOAN MEMOS: ".
           03  FILLER            PIC X(32) VALUE
           "F7-EXIT, ^/V-SCROLL, ENTER DATE:".

       01  SM-MSG2.
           03  FILLER            PIC X(12) VALUE
           " SPR MEMOS: ".
           03  FILLER            PIC X(41) VALUE
           "^-BACK, <ENTER>-NEXT SEQNO:".

       01  SM-MSG3.
           03  FILLER            PIC X(21) VALUE
           " SPR MEMOS: F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/V-SCROLL, ENTER MEMO".

       01  SM-MSG3-TOGGLE.
           03  FILLER            PIC X(21) VALUE
           " SPR MEMOS: F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/V-SCROLL, ENTER MEMO".

       01  SM-MSG3-AUDIT.
           03  FILLER            PIC X(21) VALUE
           " SPR MEMOS: F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/V-SCROLL, 'DELETE'  ".

       01  SM-MSG3-AUDIT-TOGGLE.
           03  FILLER            PIC X(21) VALUE
           " SPR MEMOS: F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/V-SCROLL, 'DELETE'  ".

       COPY "LIBSP/SPMEMO_CMD.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBSP/SPMEMOW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  DELAUD-DEL-OLD-SEQ       VALUE 1.
       78  DELAUD-DEL-PR-SEQ        VALUE 2.
      * TFC PR# 83 WIBMEMO
       78  DELAUD-ADJ-SYS-SEQ       VALUE 3.

       01  MEMOSP-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/MEMOSP_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/MEMOSP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/MEMOSPW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/MEMOSP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME MEMOSP-BUF EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MEMOSP" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO MEMO-HOLD-SYS.

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT  SCREEN LINE MEMOSP-BEGIN-X * VDU-WIN-L-OFFSET
               AT  SCREEN COL  MEMOSP-BEGIN-Y * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
            COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE    MEMOSP-COLUMNS + VDU-WIN-W-MOD
               LINES   MEMOSP-LINES + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS MEMOSP-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       MAIN-MODULE.
           MOVE 0 TO IO-FG.

           PERFORM SM-MEMO-RESET-LINE1-KEY.
           PERFORM SM-MEMO-SCAN.
           PERFORM SM-MEMO-MODULE.
           GO TO END-ROUTINE.

      ********************************************************
       SM-MEMO-MODULE SECTION.
      ********************************************************
       SM-MEMO-RESET.

           MOVE MEMO-ENTRY-LIST TO MESS.
           PERFORM RESET-MESS.
           MOVE "N" TO MEMO-AUDIT-FLAG.

           GO TO SM-MEMO-SEND-DATE.

       SM-MEMO-ENTER-DATE.
           MOVE SM-MSG1 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE MEMOSP-SP1-KEY TO SM-SP1-KEY.
           MOVE "ER M060MEMODATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "MEMODATE." TO MESS
              PERFORM RESET-MESS
              GO TO SM-MEMO-MODULE-EXIT.

      * SCROLL BACKWARD FROM CURRENT FILE POINTER
           IF VDUSTATUS = "1U"
              PERFORM SM-MEMO-SCROLL-UP
              GO TO SM-MEMO-ENTER-DATE.

      * SCROLL FORWARD FROM CURRENT FILE POINTER
           IF VDUSTATUS = "1D"
              PERFORM SM-MEMO-SCROLL-DOWN
              GO TO SM-MEMO-ENTER-DATE.

      * NO PRIORITY MEMOS

           IF VDUSTATUS NOT = "00"
              GO TO SM-MEMO-ENTER-DATE.

           MOVE VDU-DATE-YYYYMMDD TO DATE-YYYYMMDD.

           IF DATE-YYYYMMDD = 0
              MOVE "S  A080MEMODATE." TO VDU
              MOVE MEMO-HOLD-SYS TO SM-DATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT 
              PERFORM SCREENIO
              MOVE "Y" TO MEMO-IS-TODAY
           ELSE
              IF DATE-YYYYMMDD NOT = 99999999
                 IF MEMO-HOLD-SYS < DATE-YYYYMMDD
                    GO TO SM-MEMO-ENTER-DATE
                 ELSE
                    IF MEMO-HOLD-SYS = DATE-YYYYMMDD
                       MOVE "Y" TO MEMO-IS-TODAY
                    ELSE
                       IF MEMO-PASSWORD-00-OK = "N"
                          MOVE "AUDMEM" TO PASSWDW-PGM
                          MOVE "TO ADJUST OLD MEMO ENTRIES:"
                                           TO PASSWDW-PROMPT2
                          MOVE 0 TO PASSWDW-SEQ
                          PERFORM VERIFY-MEMO-PASSWORD
                          IF PASSWDW-STAT = "OK"
                             MOVE "Y" TO MEMO-PASSWORD-00-OK
                          ELSE
                             GO TO SM-MEMO-RESET.

       SM-MEMO-SEND-DATE.
           MOVE MEMOSP-SP1-KEY TO SM-SP1-KEY.
           IF MEMO-AUDIT-FLAG NOT = "Y"
              MOVE "S  A080MEMODATE." TO VDU
              MOVE MEMO-HOLD-SYS TO SM-DATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO.

           COMPUTE SM-DATE = 99999999 - DATE-YYYYMMDD.

           IF MEMO-AUDIT-FLAG NOT = "Y"
              MOVE 0 TO SM-SEQ
              GO TO SM-MEMO-NEXT-SEQ.

       SM-MEMO-ENTER-SEQ.
           MOVE SM-MSG2 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "EN N030MEMOSEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR = "1U"
              GO TO SM-MEMO-RESET.
           IF VDUSTATUS NOT = "00"
              GO TO SM-MEMO-ENTER-SEQ.
           MOVE VDUNUM0 TO SM-SEQ.

       SM-MEMO-NEXT-SEQ.
           IF MEMO-AUDIT-FLAG NOT = "Y"
              IF SM-SEQ = 999
                 MOVE "MEMO LIMIT FOR TODAY HAS BEEN EXCEEDED!"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO SM-MEMO-ENTER-DESC
              ELSE
                 ADD 1 TO SM-SEQ.

           PERFORM OPEN-SM1-FILE.
           PERFORM READ-SM1-FILE.
           MOVE IO-FG TO MEMO-IO-FG.
           PERFORM CLOSE-SM1-FILE.

           IF MEMO-AUDIT-FLAG NOT = "Y"
              IF IO-FG NOT = 0
                 GO TO SM-MEMO-SEND-SEQ
              ELSE
                 GO TO SM-MEMO-NEXT-SEQ.

           IF IO-FG NOT = 0
              GO TO SM-MEMO-RESET.

           MOVE "S  A600MEMODESC." TO VDU.
           MOVE SM-MEMO TO VDUBUF.
           PERFORM SCREENIO.
           MOVE SM1-KEY TO HOLD-SM1-KEY.
           GO TO SM-MEMO-ENTER-DESC.

       SM-MEMO-SEND-SEQ.
           MOVE "SN N030MEMOSEQ." TO VDU.
           MOVE SM-SEQ TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE SM1-KEY TO HOLD-SM1-KEY.

       SM-MEMO-ENTER-DESC.

           IF MEMO-AUDIT-FLAG = "Y"
              MOVE SM-MSG3-AUDIT-TOGGLE TO LEGEND
           ELSE
              MOVE SM-MSG3-TOGGLE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A600MEMODESC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO SM-MEMO-ENTER-DESC.

      * NO PRIORITY MEMOS

           IF VDUSTATUS = "1<"
              GO TO SM-MEMO-MODULE-EXIT.
           IF VDUSTATUS = "1>"
              MOVE "MEMODATE." TO MESS
              PERFORM RESET-MESS
              GO TO SM-MEMO-MODULE-EXIT.
           IF VDUSTATUS = "18"
              MOVE "Y" TO MEMO-AUDIT-FLAG
              GO TO SM-MEMO-ENTER-DATE.

      * SCROLL BACKWARD FROM CURRENT FILE POINTER
           IF VDUSTATUS = "1U"
              PERFORM SM-MEMO-SCROLL-UP
              GO TO SM-MEMO-ENTER-DESC.

      * SCROLL FORWARD FROM CURRENT FILE POINTER
           IF VDUSTATUS = "1D"
              PERFORM SM-MEMO-SCROLL-DOWN
              GO TO SM-MEMO-ENTER-DESC.

           IF VDUSTATUS NOT = "00"
              GO TO SM-MEMO-ENTER-DESC.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO SM-MEMO-ENTER-DESC.

           IF VDUBUF = SPACES
              GO TO SM-MEMO-ENTER-DESC.

           IF SM-SEQ = 999
              GO TO SM-MEMO-ENTER-DESC.

           IF SM-DATE NOT = 0
              GO TO DELETE-MEMO
           ELSE
              GO TO DELETE-PRIORITY.

       DELETE-MEMO.
           IF VDUBUF = "DELETE"
              IF MEMO-AUDIT-FLAG = "Y"
                 PERFORM OPEN-SM1-FILE
                 MOVE HOLD-SM1-KEY TO SM1-KEY
                 PERFORM READ-SM1-FILE
                 IF IO-FG = 0
                    IF MEMO-IS-TODAY NOT = "Y"
                       IF DELE-VERIFY-PASSWORD = "N"
                          MOVE "DELAUD" TO PASSWDW-PGM
                          MOVE "TO DELETE OLD MEMO ENTRIES:"
                                              TO PASSWDW-PROMPT2
                          MOVE 1 TO PASSWDW-SEQ
                          PERFORM VERIFY-MEMO-PASSWORD
                          IF PASSWDW-STAT = "OK"
                             MOVE "Y" TO DELE-VERIFY-PASSWORD
                          ELSE
                             MOVE "S  A600MEMODESC." TO VDU
                             MOVE SM-MEMO TO VDUBUF
                             PERFORM SCREENIO
                             GO TO SM-MEMO-ENTER-DESC
                          END-IF
                       END-IF
                    END-IF
                    PERFORM DELETE-SM1-FILE
                    PERFORM CLOSE-SM1-FILE
                    MOVE 0 TO SM-SEQ
                    PERFORM SM-MEMO-SCAN
                    GO TO SM-MEMO-RESET
                 ELSE
                    PERFORM CLOSE-SM1-FILE
                    GO TO SM-MEMO-ENTER-DESC
              ELSE
                 GO TO SM-MEMO-ENTER-DESC.

       DELETE-PRIORITY.
           IF VDUBUF = "DELETE"
              IF MEMO-AUDIT-FLAG = "Y"
                 PERFORM OPEN-SM1-FILE
                 MOVE HOLD-SM1-KEY TO SM1-KEY
                 PERFORM READ-SM1-FILE
                 IF IO-FG = 0
                    IF DELE-VERIFY-PASSWORD = "N"
                       MOVE "DELAUD" TO PASSWDW-PGM
                       MOVE "TO DELETE PRIORITY ENTRIES:"
                                           TO PASSWDW-PROMPT2
                       MOVE 2 TO PASSWDW-SEQ
                       PERFORM VERIFY-MEMO-PASSWORD
                       IF PASSWDW-STAT = "OK"
                          MOVE "Y" TO DELE-VERIFY-PASSWORD
                       ELSE
                          MOVE "S  A600MEMODESC." TO VDU
                          MOVE SM-MEMO TO VDUBUF
                          PERFORM SCREENIO
                          GO TO SM-MEMO-ENTER-DESC
                       END-IF
                    END-IF
                    PERFORM DELETE-SM1-FILE
                    PERFORM CLOSE-SM1-FILE
                    MOVE 0 TO SM-SEQ
                    PERFORM SM-MEMO-SCAN
                    GO TO SM-MEMO-RESET
                 ELSE
                    PERFORM CLOSE-SM1-FILE
                    GO TO SM-MEMO-ENTER-DESC
              ELSE
                 GO TO SM-MEMO-ENTER-DESC.

       SM-MEMO-UPDATE.
           PERFORM OPEN-SM1-FILE.
           MOVE VDUBUF TO SM-MEMO.
           MOVE SPACES TO SM-MEMO-TYPE.
           MOVE HOLD-SM1-KEY TO SM1-KEY.
           IF MEMO-IO-FG = 0
              PERFORM READ-SM1-FILE
              IF IO-FG = 0
                 MOVE VDUBUF TO SM-MEMO
                 PERFORM REWRITE-SM1-FILE
                 PERFORM CLOSE-SM1-FILE
                 GO TO CONTINUE-SM-MEMO-UPDATE.
           PERFORM WRITE-SM1-FILE.
           IF IO-FG = 9
              IF E-CODE NOT = "22"
                 GO TO IO-ERROR
              ELSE
                 ADD 1 TO HOLD-SM-SEQ
                 IF HOLD-SM-SEQ < 999
                    GO TO SM-MEMO-UPDATE
                 ELSE
                    MOVE 999 TO SM-SEQ
                    PERFORM CLOSE-SM1-FILE
                    GO TO SM-MEMO-NEXT-SEQ.
           PERFORM CLOSE-SM1-FILE.

       CONTINUE-SM-MEMO-UPDATE.
      * SAVE SEQ OF FIRST MEMO IN SESSION FOR CJ UPDATE
           IF SM-DATE = 0 OR MEMO-AUDIT-FLAG = "Y"
              GO TO RESTART-SM-SCAN.
           IF SAVE-BEG-SM-SEQ = 0
              MOVE SM-SEQ TO SAVE-BEG-SM-SEQ
                             SAVE-END-SM-SEQ.
           IF SM-SEQ > SAVE-BEG-SM-SEQ
              MOVE SM-SEQ TO SAVE-END-SM-SEQ.
           MOVE "Y" TO MEMO-ENTERED-FG.

       RESTART-SM-SCAN.
      *    START SCAN FROM FIRST SEQ# FOR DATE AND SP-KEY#;
      *    IF MEMO CONTAINS MORE THAN MEMO-MAX LINES, SCAN.
           MOVE 0 TO SM-SEQ.
           PERFORM SM-MEMO-SCAN.
           GO TO SM-MEMO-RESET.

       SM-MEMO-RESET-LINE1-KEY.
           MOVE MEMOSP-SP1-KEY TO SM-SP1-KEY.
           MOVE 0 TO SM-DATE SM-SEQ.
           MOVE SM1-KEY TO LINE1-SM1-KEY.

       SM-MEMO-MODULE-EXIT.
           PERFORM SEND-LEGEND.

      ******************************************************************
       SM-MEMO-SCAN SECTION.
      ******************************************************************
      D    STOP MESS.
           PERFORM SEND-HEAD-3.
           MOVE SPACES TO MEMO-BUF.
           MOVE 0 TO MEMO-LINE.

           PERFORM OPEN-SM1-FILE.

           PERFORM START-SM1-FILE-SCAN.

       SM-MEMO-SCAN-NEXT-SEQ.
           PERFORM READ-SM1-FILE-NEXT.
      *    UNLOCK SM-FILE.
           IF IO-FG = 9 OR SM-SP1-KEY NOT = MEMOSP-SP1-KEY
              PERFORM SM-MEMO-BUMP-DOWN
              GO TO SM-MEMO-SCAN-EXIT.

           IF MEMO-LINE NOT < MEMO-MAX
              GO TO SM-MEMO-SCAN-EXIT.

           ADD 1 TO MEMO-LINE.

           IF SM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - SM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO MEMO-DATE(MEMO-LINE).

           MOVE SM-SEQ TO MEMO-SEQ(MEMO-LINE).
           MOVE SM-MEMO TO MEMO-MEMO(MEMO-LINE).

           IF MEMO-LINE = 1
              MOVE SM1-KEY TO LINE1-SM1-KEY.

           MOVE SM1-KEY TO LAST-SM1-KEY.

           GO TO SM-MEMO-SCAN-NEXT-SEQ.

       SM-MEMO-SCAN-EXIT.
           MOVE MEMO-BUF TO WR-BUF.
           MOVE MEMO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM CLOSE-SM1-FILE.

      ******************************************************************
       SM-MEMO-BUMP-DOWN SECTION.
      ******************************************************************
           IF MEMO-LINE > 7
              GO TO SM-EXIT-BUMP-DOWN.

           MOVE LINE1-SM1-KEY TO SM1-KEY.
           PERFORM START-SM1-FILE-LESS-THAN.
       SM-BUMP-DOWN-READ-PREV.
           PERFORM READ-SM1-FILE-PREVIOUS.
      *    UNLOCK SM-FILE.
           IF IO-FG NOT = 0 OR SM-SP1-KEY NOT = MEMOSP-SP1-KEY
              GO TO SM-BUMP-DOWN-SAVE-LAST-KEY.

           MOVE MEMO-TAB(7) TO MEMO-TAB(8).
           MOVE MEMO-TAB(6) TO MEMO-TAB(7).
           MOVE MEMO-TAB(5) TO MEMO-TAB(6).
           MOVE MEMO-TAB(4) TO MEMO-TAB(5).
           MOVE MEMO-TAB(3) TO MEMO-TAB(4).
           MOVE MEMO-TAB(2) TO MEMO-TAB(3).
           MOVE MEMO-TAB(1) TO MEMO-TAB(2).
           IF SM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - SM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO MEMO-DATE(1).
           MOVE SM-SEQ TO MEMO-SEQ(1).
           MOVE SM-MEMO TO MEMO-MEMO(1).
           MOVE SM1-KEY TO LINE1-SM1-KEY.
           ADD 1 TO MEMO-LINE.
           IF MEMO-LINE < 8
              GO TO SM-BUMP-DOWN-READ-PREV.
       SM-BUMP-DOWN-SAVE-LAST-KEY.
           IF MEMO-LINE > 0
              IF MEMO-TAB(MEMO-LINE) NOT = SPACES
                 MOVE MEMOSP-SP1-KEY TO LAST-SM-SP1-KEY
                 MOVE MEMO-SEQ(MEMO-LINE) TO LAST-SM-SEQ
                 IF MEMO-DATEX(MEMO-LINE) = "PRIORITY"
                    MOVE 0 TO LAST-SM-DATE
                 ELSE
                    MOVE MEMO-DATE(MEMO-LINE) TO DATE-MMDDYY
                    PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                    COMPUTE LAST-SM-DATE = 99999999
                                              - DATE-YYYYMMDD.
       SM-EXIT-BUMP-DOWN.
           EXIT.

      *********************************************
       SM-MEMO-SCROLL-DOWN SECTION.
      *********************************************
      D    STOP MESS.
           PERFORM OPEN-SM1-FILE.

           MOVE LAST-SM1-KEY TO SM1-KEY.
           ADD 1 TO SM-SEQ.

           PERFORM START-SM1-FILE-SCAN.
           PERFORM READ-SM1-FILE-NEXT.
      *    UNLOCK SM-FILE.
           IF IO-FG = 0
              IF SM-SP1-KEY = MEMOSP-SP1-KEY
                 MOVE LAST-SM1-KEY TO SM1-KEY
                 ADD 1 TO SM-SEQ
                 PERFORM SM-MEMO-SCAN
              ELSE
                 PERFORM CLOSE-SM1-FILE.

      *********************************************
       SM-MEMO-SCROLL-UP SECTION.
      *********************************************
      D    STOP MESS.
           PERFORM OPEN-SM1-FILE.
           MOVE LINE1-SM1-KEY TO SM1-KEY.
           PERFORM START-SM1-FILE-LESS-THAN.
           PERFORM READ-SM1-FILE-PREVIOUS.
      *    UNLOCK SM-FILE.
           IF IO-FG NOT = 0 OR SM-SP1-KEY NOT = MEMOSP-SP1-KEY
              GO TO SM-SCROLL-UP-EXIT.

           IF SM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - SM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO WS-MEMO-DATE-MDY.                             

           MOVE SM-SEQ         TO WS-SEQ.
           
           IF ( SM-SP1-KEY = MEMOSP-SP1-KEY ) AND
              ( WS-SEQ     = MEMO-SEQ(1) )    AND
              ( SM-MEMO    = MEMO-MEMO(1) )   
              GO TO SM-SCROLL-UP-EXIT.                                                                  
           MOVE 0 TO MEMO-LINE.
           MOVE LINE1-SM1-KEY TO SM1-KEY.
           PERFORM START-SM1-FILE-LESS-THAN.
       SM-SCROLL-UP-READ-PREV.
           PERFORM READ-SM1-FILE-PREVIOUS.
      *    UNLOCK SM-FILE.
           IF IO-FG NOT = 0 OR SM-SP1-KEY NOT = MEMOSP-SP1-KEY
              GO TO SM-SCROLL-UP-SAVE-LAST-KEY.

           MOVE MEMO-TAB(7) TO MEMO-TAB(8).
           MOVE MEMO-TAB(6) TO MEMO-TAB(7).
           MOVE MEMO-TAB(5) TO MEMO-TAB(6).
           MOVE MEMO-TAB(4) TO MEMO-TAB(5).
           MOVE MEMO-TAB(3) TO MEMO-TAB(4).
           MOVE MEMO-TAB(2) TO MEMO-TAB(3).
           MOVE MEMO-TAB(1) TO MEMO-TAB(2).
           IF SM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - SM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO MEMO-DATE(1).
           MOVE SM-SEQ TO MEMO-SEQ(1).
           MOVE SM-MEMO TO MEMO-MEMO(1).
           MOVE SM1-KEY TO LINE1-SM1-KEY.
           ADD 1 TO MEMO-LINE.
           IF MEMO-LINE < 8
              GO TO SM-SCROLL-UP-READ-PREV.
       SM-SCROLL-UP-SAVE-LAST-KEY.
           IF MEMO-LINE > 0
              IF MEMO-TAB(8) NOT = SPACES
                 MOVE MEMOSP-SP1-KEY TO LAST-SM-SP1-KEY
                 MOVE MEMO-SEQ(8) TO LAST-SM-SEQ
                 IF MEMO-DATEX(8) = "PRIORITY"
                    MOVE 0 TO LAST-SM-DATE
                 ELSE
                    MOVE MEMO-DATE(8) TO DATE-MMDDYY
                    PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                    COMPUTE LAST-SM-DATE = 99999999
                                                   - DATE-YYYYMMDD.
           MOVE MEMO-BUF TO WR-BUF.
           MOVE MEMO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SM-SCROLL-UP-EXIT.
           PERFORM CLOSE-SM1-FILE.

      ***************************************************
       VERIFY-MEMO-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.
       EXIT-VERIFY-MEMO-PASSWORD.
           EXIT.

      ***************************************************
       SEND-HEAD-3 SECTION.
      ***************************************************
           MOVE "S  A220TITLE." TO VDU.
           MOVE " STATE PARAMETER MEMO" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A660HEAD:S." TO VDU.
           MOVE HEAD-3 TO VDUBUF.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON MEMOSP-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY MEMOSP-SCREEN UPON MEMOSP-WINDOW-HANDLE.
           MOVE MEMOSP-FORM-FIELDS TO WR-BUF.
           MOVE MEMOSP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE MEMOSP-HELP--FILE TO HELP-LINK-FILE.
           MOVE MEMOSP-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/MEMOSP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SM1-FILE.
       COPY "LIBLP/LPSMGS_SQL.CPY".
       COPY "LIBLP/LPSM1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY MEMOSP-SCREEN.
           DESTROY MEMOSP-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
