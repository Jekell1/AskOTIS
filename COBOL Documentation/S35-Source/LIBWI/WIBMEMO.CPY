      * COPYMEMBER: LIBWI/WIBMEMO
      ************************************************************************
      *    BM   M E M O
      * ONLY IN SP/SPINQ1.CX WI/MEMOSC.C  (8/1/00)
      * REV:
      *  071900 BAH PASSWORD PROTECT DELETING SYSTEM GENERATED
      *             MEMOS, TFC PR# 83
      *  091100 BAH I DUPLICATED SEQUENCES, CAUSING LOAN PRIORITY
      *             MEMOS TO NOT 'DELETE', SO I MOVED MY ABOVE
      *             CHANGE TO SEQ 3
      *  101900 BLV REMOVED EXTRA END-IF IN DELETE-BM-PRIORITY
      *             WHICH STOPPED YOU FROM CREATING BW MEMOS
      *  MJD 001019 CHANGED TO ACCEPT PAGE-UP AND PAGE-DOWN (AS PER JTG)
      *  BLV 020214 CHANGED PERFORMS OF VERIFY-PASSWORD TO
      *             VERIFY-MEMO-PASSWORD TO AVOID CONFUSIONS &
      *             CONFLICTS WITH VERIFY-PASSWORD ROUTINE IN
      *             SPINQ1 WHICH COPIES THIS ROUTINE; PROBLEM WAS THIS
      *             WHEN THIS ROUTINE PERFORMED VERIFY-PASSWORD IN SPINQ1,
      *             THE PASSWDW-PGM GET SET TO SPINQ1 INSTEAD OF AUDMEM
      *             OR DELAUD
      *  BAH 040825 ADDED AUDMEM SEQ 2 FOR EDITING TODAYS MEMO AND ADDED
      *             DELAUD SEQ 4 FOR DELETING TODAYS MEMO, REGACC PR# 75
      *  CS  041011 ADD VALIDATE PRIORITY1 IF GP-I-AM-MODERN PR#2388
      *  BAH 070313 CHANGED NAME OF MEMO-VERIFY-PASSWORD TO MEMO-PASSWORD-00-OK
      *             AND ADDED NEW ONES FOR 01-OK AND 02-OK, CAUSING TROUBLE
      *              REGENCY PL# 582
      *  BAH 110208 REWRITING AN EXISTING MEMO WAS NOT WORKING, CHANGE
      *             WAS NOT BEING SAVED, MOVE VDUBUF WAS REMOVED FROM ABOVE
      *             CHANGE, PUT IT BACK. UNITED PL# 724
      *  BLM 130530 REPLACED RESET-MESS WITH CALL-WRFORM
      *  BAH 150624 PASSWORD CHANGING SPANISH NAME MEMOS WHICH ARE
      *             PRIORITY MEMO SEQ 1, 2, AND 3  WORLD PR# 1092
      *  BAH 160824 REMOVED BM-PHONE CHECK, MOVED PHONES AND CONTACT INFO
      *             TO BW-REC
      *  BAH 170420 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 180925 ADDED BM-BRNO TO KEY
      *  BAH 20190624 PRIORITY MEMO EDIT, PRESS F3, F4-DATE, F3 AND IT WAS
      *               DISPLAYING DATE, NOT "PRIORITY". NEEDED TO MOVE
      *               99999999 TO VDU-DATE-YYYYMMDD BECAUSE VDUNUM0 WAS ONLY
      *               999999. QA0038
      * BAH 20220803 HARD CODED START
      ***********************************************************************

      * PASSWORDS PASSWDW
      *  AUDMEM  SEQ 0   NO EDIT MEMO > TODAY
      *  AUDMEM  SEQ 1   TO EDIT PRIORITY MEMOS
      *  AUDMEM  SEQ 2   TO EDIT MEMO MADE TODAY
      *  AUDMEM  SEQ 3   TO EDIT PRIORITY SPANISH NAMES

      *********************************************
       BM-MEMO-MODULE SECTION.
      *********************************************
       BM-MEMO-RESET.
           MOVE MEMO-ENTRY-LIST TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "N" TO MEMO-AUDIT-FLAG.
           GO TO BM-MEMO-SEND-DATE.
       BM-MEMO-ENTER-DATE.
           MOVE BM-MSG1 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE EXT-WI-CL-OWNBR                   TO BM-BRNO.
           MOVE EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO) TO BM-SSNO.

           MOVE "ER M060MEMODATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
      *       PERFORM RESET-MESS
              MOVE "MEMODATE." TO WR-LIST
              MOVE SPACES TO WR-BUF
              PERFORM CALL-WRFORM
              GO TO BM-MEMO-MODULE-EXIT.
           IF UP-KEY OR PAGE-UP-KEY
      * SCROLL BACKWARD FROM CURRENT FILE POINTER
              PERFORM BM-MEMO-SCROLL-UP
              GO TO BM-MEMO-ENTER-DATE.
           IF DOWN-KEY OR PAGE-DOWN-KEY
      * SCROLL FORWARD FROM CURRENT FILE POINTER
              PERFORM BM-MEMO-SCROLL-DOWN
              GO TO BM-MEMO-ENTER-DATE.
           IF VDUSTATUS = "16"
              MOVE "S  A080MEMODATE." TO VDU
              MOVE "PRIORITY" TO VDUBUF
              PERFORM SCREENIO
              MOVE 999999   TO VDUNUM0
              MOVE 99999999 TO VDU-DATE-YYYYMMDD
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO BM-MEMO-ENTER-DATE.

           MOVE VDU-DATE-YYYYMMDD TO DATE-YYYYMMDD.
           MOVE "N" TO MEMO-IS-TODAY.
           
      * 2/11/98-ADDED FOR WORLD FOR GIFTNET MEMOS:
      *         WI/INVENT SENDS 999999 AS SYSTEM DATE TO DEFAULT
      *         SINCE DELIVERY INSTRUCTIONS IN BORROWER PRIORITY
           IF DATE-YYYYMMDD = 0
              IF MEMO-HOLD-SYS = 99999999
                 MOVE 99999999 TO DATE-YYYYMMDD BM-DATE
                 MOVE "S  A080MEMODATE." TO VDU
                 MOVE "PRIORITY" TO VDUBUF
                 PERFORM SCREENIO.

           IF DATE-YYYYMMDD = 0
              MOVE "S  A080MEMODATE." TO VDU
              MOVE MEMO-HOLD-SYS TO BM-DATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO
              MOVE "Y" TO MEMO-IS-TODAY
           ELSE
              IF DATE-YYYYMMDD NOT = 99999999
                 IF MEMO-HOLD-SYS < DATE-YYYYMMDD
                    GO TO BM-MEMO-ENTER-DATE
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
                             GO TO BM-MEMO-RESET.

           IF MEMO-IS-TODAY = "Y"
              IF MEMO-PASSWORD-02-OK = "N"
                MOVE "AUDMEM" TO PASSWDW-PGM
                MOVE "TO ADJUST TODAYS ENTRIES:" TO PASSWDW-PROMPT2
                MOVE 2 TO PASSWDW-SEQ
                PERFORM VERIFY-MEMO-PASSWORD
                IF PASSWDW-STAT = "OK"
                   MOVE "Y" TO MEMO-PASSWORD-02-OK
                ELSE
                   GO TO BM-MEMO-RESET.

           IF DATE-MMDDYY = 99999999
             IF MEMO-PASSWORD-01-OK = "N"
                MOVE "AUDMEM" TO PASSWDW-PGM
                MOVE "TO ADJUST PRIORITY ENTRIES:"
                      TO PASSWDW-PROMPT2
                MOVE 1 TO PASSWDW-SEQ
                PERFORM VERIFY-MEMO-PASSWORD
                IF PASSWDW-STAT = "OK"
                   MOVE "Y" TO MEMO-PASSWORD-01-OK
                ELSE
                   GO TO BM-MEMO-RESET.

       BM-MEMO-SEND-DATE.
           MOVE EXT-WI-CL-OWNBR                   TO BM-BRNO.
           MOVE EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO) TO BM-SSNO.
           IF MEMO-AUDIT-FLAG NOT = "Y"
              IF MEMO-HOLD-SYS = 999999
                 MOVE 99999999   TO DATE-YYYYMMDD BM-DATE
                 MOVE "S  A080MEMODATE." TO VDU
                 MOVE "PRIORITY" TO VDUBUF
                 PERFORM SCREENIO
              ELSE
                 MOVE "S  A080MEMODATE." TO VDU
                 MOVE MEMO-HOLD-SYS TO BM-DATE DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO VDU-DATE-EDIT
                 PERFORM SCREENIO.

           IF DATE-MMDDYY = 99999999
              MOVE 0 TO BM-DATE
           ELSE
              COMPUTE BM-DATE = 99999999 - DATE-YYYYMMDD.

           IF MEMO-AUDIT-FLAG NOT = "Y"
              MOVE 0 TO BM-SEQ
              GO TO BM-MEMO-NEXT-SEQ.

       BM-MEMO-ENTER-SEQ.
           MOVE BM-MSG2 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "EN N020MEMOSEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR = "1U"
              GO TO BM-MEMO-RESET.
           IF VDUSTATUS NOT = "00"
              GO TO BM-MEMO-ENTER-SEQ.
           MOVE VDUNUM0 TO BM-SEQ.

       BM-MEMO-NEXT-SEQ.
           IF MEMO-AUDIT-FLAG NOT = "Y"
              IF BM-SEQ = 999
                 MOVE "MEMO LIMIT FOR TODAY HAS BEEN EXCEEDED!"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO BM-MEMO-ENTER-DESC
              ELSE
                 ADD 1 TO BM-SEQ.

           PERFORM OPEN-BM1-FILE.
           PERFORM READ-BM1-FILE.
           MOVE IO-FG TO MEMO-IO-FG.
           PERFORM CLOSE-BM1-FILE.

           IF MEMO-AUDIT-FLAG NOT = "Y"
              IF IO-FG NOT = 0
                 GO TO BM-MEMO-SEND-SEQ
              ELSE
                 GO TO BM-MEMO-NEXT-SEQ.

           IF IO-FG NOT = 0
              GO TO BM-MEMO-RESET.

           IF BM-MEMO-TYPE NOT = "  "
      * TFC PR# 83
              MOVE "DELAUD" TO PASSWDW-PGM
              MOVE "TO AUDIT SYSTEM MEMOS:" TO PASSWDW-PROMPT2
              MOVE 3 TO PASSWDW-SEQ
              PERFORM VERIFY-MEMO-PASSWORD
              IF PASSWDW-STAT = "OK"
                 NEXT SENTENCE
              ELSE
                 MOVE "SYSTEM GENERATED MEMO LINE, CAN'T CHANGE"
                                             TO MESS
                 PERFORM SEND-MESS
                 GO TO BM-MEMO-RESET.

      * MEXICO SPANISH NAMES ARE STORED IN FIRST 3 PRIORITY MEMOS
      * CREATED IN WI/MXCREA, DO NOT ALLOW CHANGES TO THOSE.
           IF DATE-MMDDYY = 99999999
              IF EXT-CONAME-USER-LOC > 2999 AND < 4000
                 IF BM-SEQ = 1 OR 2 OR 3
                    MOVE "AUDMEM" TO PASSWDW-PGM
                    MOVE "TO ADJUST NAMES ENTRIES:" TO PASSWDW-PROMPT2
                    MOVE 3 TO PASSWDW-SEQ
                    PERFORM VERIFY-MEMO-PASSWORD
                    IF PASSWDW-STAT NOT = "OK"
                       GO TO BM-MEMO-RESET.

           MOVE "S  A600MEMODESC." TO VDU.
           MOVE BM-MEMO TO VDUBUF.
           PERFORM SCREENIO.
           MOVE BM1-KEY TO BM-MEMO-HOLD-KEY.
           GO TO BM-MEMO-ENTER-DESC.

       BM-MEMO-SEND-SEQ.
           MOVE "SN N020MEMOSEQ." TO VDU.
           MOVE BM-SEQ TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE BM1-KEY TO BM-MEMO-HOLD-KEY.

       BM-MEMO-ENTER-DESC.

           IF MEMO-AUDIT-FLAG = "Y"
              IF MEMOSC-ONLY = " " OR "T"
                 MOVE BM-MSG3-AUDIT-TOGGLE TO LEGEND
              ELSE
                 MOVE BM-MSG3-AUDIT TO LEGEND
           ELSE
              IF MEMOSC-ONLY = " " OR "T"
                 IF GP-DL-MEMO-TOGGLE  = "Y"
                    MOVE BM-MSG3-DL-TOGGLE TO LEGEND
                 ELSE
                    MOVE BM-MSG3-TOGGLE TO LEGEND
                 END-IF
              ELSE
                 MOVE BM-MSG3 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A600MEMODESC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO BM-MEMO-ENTER-DESC.
           IF VDUSTATUS = "16"
              MOVE "S  A080MEMODATE." TO VDU
              MOVE "PRIORITY" TO VDUBUF
              PERFORM SCREENIO
              MOVE EXT-WI-CL-OWNBR                   TO BM-BRNO
              MOVE EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO) TO BM-SSNO
              MOVE 0 TO BM-DATE BM-SEQ VDUNUM0
              GO TO BM-MEMO-NEXT-SEQ.
           IF VDUSTATUS = "1<"
              IF MEMOSC-ONLY NOT = "B"
                 GO TO BM-MEMO-MODULE-EXIT.
           IF VDUSTATUS = "1>"
      *       PERFORM RESET-MESS
              MOVE "MEMODATE." TO WR-LIST
              MOVE SPACES TO WR-BUF
              PERFORM CALL-WRFORM
              GO TO BM-MEMO-MODULE-EXIT.
           IF VDUSTATUS = "18"
              MOVE "Y" TO MEMO-AUDIT-FLAG
              GO TO BM-MEMO-ENTER-DATE.
           IF UP-KEY OR PAGE-UP-KEY
      * SCROLL BACKWARD FROM CURRENT FILE POINTER
              PERFORM BM-MEMO-SCROLL-UP
              GO TO BM-MEMO-ENTER-DESC.
           IF DOWN-KEY OR PAGE-DOWN-KEY
      * SCROLL FORWARD FROM CURRENT FILE POINTER
              PERFORM BM-MEMO-SCROLL-DOWN
              GO TO BM-MEMO-ENTER-DESC.
           IF VDUSTATUS NOT = "00"
              GO TO BM-MEMO-ENTER-DESC.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO BM-MEMO-ENTER-DESC.

           IF VDUBUF = SPACES
              GO TO BM-MEMO-ENTER-DESC.

           IF BM-SEQ = 999
              GO TO BM-MEMO-ENTER-DESC.

           IF BM-DATE NOT = 0
              GO TO DELETE-BM-MEMO
           ELSE
              GO TO DELETE-BM-PRIORITY.

       DELETE-BM-MEMO.
           IF VDUBUF = "DELETE"
              IF MEMO-AUDIT-FLAG = "Y"
                 PERFORM OPEN-BM1-FILE
                 MOVE BM-MEMO-HOLD-KEY TO BM1-KEY
                 PERFORM READ-BM1-FILE
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
                             MOVE BM-MEMO TO VDUBUF
                             PERFORM SCREENIO
                             GO TO BM-MEMO-ENTER-DESC
                          END-IF
                       END-IF
                    ELSE
      * MEMO IS FROM TODAY
                       IF DELE-VERIFY-PASSWORD = "N"
                          MOVE "DELAUD" TO PASSWDW-PGM
                          MOVE "TO DELETE TODAYS ENTRIES:"
                                              TO PASSWDW-PROMPT2
                          MOVE 4 TO PASSWDW-SEQ
                          PERFORM VERIFY-MEMO-PASSWORD
                          IF PASSWDW-STAT = "OK"
                             MOVE "Y" TO DELE-VERIFY-PASSWORD
                          ELSE
                             MOVE "S  A600MEMODESC." TO VDU
                             MOVE BM-MEMO TO VDUBUF
                             PERFORM SCREENIO
                             GO TO BM-MEMO-ENTER-DESC
                          END-IF
                       END-IF
                    END-IF
                    PERFORM DELETE-BM1-FILE
                    PERFORM CLOSE-BM1-FILE
                    MOVE 0 TO BM-SEQ
                    PERFORM BM-MEMO-SCAN
                    GO TO BM-MEMO-RESET
                 ELSE
                    PERFORM CLOSE-BM1-FILE
                    GO TO BM-MEMO-ENTER-DESC
              ELSE
                 GO TO BM-MEMO-ENTER-DESC.

       DELETE-BM-PRIORITY.
           IF VDUBUF = "DELETE"
              IF MEMO-AUDIT-FLAG = "Y"
                 PERFORM OPEN-BM1-FILE
                 MOVE BM-MEMO-HOLD-KEY TO BM1-KEY
                 PERFORM READ-BM1-FILE
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
                          MOVE BM-MEMO TO VDUBUF
                          PERFORM SCREENIO
                          GO TO BM-MEMO-ENTER-DESC
                       END-IF
                    END-IF
                    PERFORM DELETE-BM1-FILE
                    PERFORM CLOSE-BM1-FILE
                    MOVE 0 TO BM-SEQ
                    PERFORM BM-MEMO-SCAN
                    GO TO BM-MEMO-RESET
                 ELSE
                    PERFORM CLOSE-BM1-FILE
                    GO TO BM-MEMO-ENTER-DESC
              ELSE
                 GO TO BM-MEMO-ENTER-DESC.

       BM-MEMO-UPDATE.
           PERFORM OPEN-BM1-FILE.
           PERFORM DETERMINE-BM-VDUBUF.
           MOVE HOLD-MEMO-TYPE TO BM-MEMO-TYPE.
           MOVE BM-MEMO-HOLD-KEY TO BM1-KEY.
           IF MEMO-IO-FG = 0
              PERFORM READ-BM1-FILE
              IF IO-FG = 0
                 PERFORM DETERMINE-BM-VDUBUF
                 PERFORM REWRITE-BM1-FILE
                 PERFORM CLOSE-BM1-FILE
                 GO TO CONTINUE-BM-MEMO-UPDATE.

           PERFORM WRITE-BM1-FILE.
           IF IO-FG = 9
              IF E-CODE NOT = "22"
                 GO TO IO-ERROR
              ELSE
                 ADD 1 TO BM-MEMO-HOLD-SEQ
                 IF BM-MEMO-HOLD-SEQ < 999
                    GO TO BM-MEMO-UPDATE
                 ELSE
                    MOVE 999 TO BM-SEQ
                    PERFORM CLOSE-BM1-FILE
                    GO TO BM-MEMO-NEXT-SEQ.
           PERFORM CLOSE-BM1-FILE.
           MOVE "Y" TO MEMO-ENTERED-FG.

       CONTINUE-BM-MEMO-UPDATE.
      *    START SCAN FROM FIRST SEQ# FOR DATE AND SSNO#;
      *    IF MEMO CONTAINS MORE THAN MEMO-MAX LINES, SCAN.
           MOVE 0 TO BM-SEQ.
           PERFORM BM-MEMO-SCAN.
           GO TO BM-MEMO-RESET.

       BM-MEMO-RESET-LINE1-KEY.
           MOVE EXT-WI-CL-OWNBR                   TO BM-BRNO.
           MOVE EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO) TO BM-SSNO.
           MOVE 0                                 TO BM-DATE BM-SEQ.
           MOVE BM1-KEY TO BM-MEMO-LINE1-KEY.

       BM-MEMO-MODULE-EXIT.
           PERFORM SEND-LEGEND.

      **************************
       DETERMINE-BM-VDUBUF SECTION.
      **************************
           MOVE VDUBUF TO BM-MEMO.

      ******************************************************************
      *
      *    BM   M E M O   S C A N
      *    SHOW MEMO-MAX MEMOS
      *
      ******************************************************************
       BM-MEMO-SCAN SECTION.
      D    STOP MESS.
           MOVE "    BORROWER MEMO" TO MEMO-WHICH.
           PERFORM SEND-HEAD-3.
           MOVE SPACES TO MEMO-BUF.
           MOVE 0 TO MEMO-LINE.

           PERFORM OPEN-BM1-FILE.
           PERFORM START-BM1-FILE-SCAN.

      *----------------------------------------------------------------
       BM-MEMO-SCAN-NEXT-SEQ.
           PERFORM READ-BM1-FILE-NEXT.
      *    UNLOCK BM-FILE.

           IF (IO-FG = 9) OR (BM-BRNO NOT = EXT-WI-CL-OWNBR) OR
                   (BM-SSNO NOT = EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO))
              PERFORM BM-MEMO-BUMP-DOWN
              GO TO BM-MEMO-SCAN-EXIT.

           IF HOLD-MEMO-TYPE NOT = "  "
                   AND BM-MEMO-TYPE NOT = HOLD-MEMO-TYPE
              GO TO BM-MEMO-SCAN-NEXT-SEQ.

           IF MEMO-LINE NOT < MEMO-MAX
              GO TO BM-MEMO-SCAN-EXIT.

           ADD 1 TO MEMO-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - BM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO MEMO-DATE(MEMO-LINE)
           ELSE
              MOVE "PRIORITY" TO MEMO-TAB(MEMO-LINE).

           MOVE BM-SEQ TO MEMO-SEQ(MEMO-LINE).
           MOVE BM-MEMO TO MEMO-MEMO(MEMO-LINE).

           IF MEMO-LINE = 1
              MOVE BM1-KEY TO BM-MEMO-LINE1-KEY.

           MOVE BM1-KEY TO BM-MEMO-LAST-LINE-KEY.

           GO TO BM-MEMO-SCAN-NEXT-SEQ.

      *----------------------------------------------------------------
       BM-MEMO-SCAN-EXIT.
           MOVE MEMO-BUF TO WR-BUF.
           MOVE MEMO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM CLOSE-BM1-FILE.

      ******************************************************************
      *  L M   M E M O   S P A C E
      ******************************************************************
       BM-MEMO-BUMP-DOWN SECTION.
      D    STOP MESS.
       BM-BUMP-DOWN-START.
           IF MEMO-LINE > 7
              GO TO BM-EXIT-BUMP-DOWN.

           MOVE BM-MEMO-LINE1-KEY TO BM1-KEY.
           PERFORM START-BM1-FILE-LESS-THAN.

       BM-BUMP-DOWN-READ-PREV.
           PERFORM READ-BM1-FILE-PREVIOUS.
      *    UNLOCK BM-FILE.
           IF (IO-FG NOT = 0) OR (BM-BRNO NOT = EXT-WI-CL-OWNBR) OR
                  (BM-SSNO NOT = EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO))
              GO TO BM-BUMP-DOWN-SAVE-LAST-KEY.
           IF HOLD-MEMO-TYPE NOT = "  "
                   AND BM-MEMO-TYPE NOT = HOLD-MEMO-TYPE
              GO TO BM-BUMP-DOWN-READ-PREV.

           MOVE MEMO-TAB(7) TO MEMO-TAB(8).
           MOVE MEMO-TAB(6) TO MEMO-TAB(7).
           MOVE MEMO-TAB(5) TO MEMO-TAB(6).
           MOVE MEMO-TAB(4) TO MEMO-TAB(5).
           MOVE MEMO-TAB(3) TO MEMO-TAB(4).
           MOVE MEMO-TAB(2) TO MEMO-TAB(3).
           MOVE MEMO-TAB(1) TO MEMO-TAB(2).
           IF BM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - BM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO MEMO-DATE(1)
           ELSE
              MOVE "PRIORITY" TO MEMO-TAB(1).
           MOVE BM-SEQ TO MEMO-SEQ(1).
           MOVE BM-MEMO TO MEMO-MEMO(1).
           MOVE BM1-KEY TO BM-MEMO-LINE1-KEY.
           ADD 1 TO MEMO-LINE.
           IF MEMO-LINE < 8
              GO TO BM-BUMP-DOWN-READ-PREV.
       BM-BUMP-DOWN-SAVE-LAST-KEY.
           IF MEMO-LINE > 0
              IF MEMO-TAB(MEMO-LINE) NOT = SPACES
                 MOVE EXT-WI-CL-OWNBR     TO BM-MEMO-LAST-LINE-BRNO
                 MOVE EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO)
                                          TO BM-MEMO-LAST-LINE-SSNO
                 MOVE MEMO-SEQ(MEMO-LINE) TO BM-MEMO-LAST-LINE-SEQ
                 IF MEMO-DATEX(MEMO-LINE) = "PRIORITY"
                    MOVE 0 TO BM-MEMO-LAST-LINE-DATE
                 ELSE
                    MOVE MEMO-DATE(MEMO-LINE) TO DATE-MMDDYY
                    PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                    COMPUTE BM-MEMO-LAST-LINE-DATE = 99999999 -
                                                 DATE-YYYYMMDD.
       BM-EXIT-BUMP-DOWN.
           EXIT.

      *********************************************
      *   SCROLL DOWN ROUTINE
      *********************************************
       BM-MEMO-SCROLL-DOWN SECTION.
           PERFORM OPEN-BM1-FILE.
           MOVE BM-MEMO-LAST-LINE-KEY TO BM1-KEY.
           ADD 1 TO BM-SEQ.

           PERFORM START-BM1-FILE-SCAN.
           PERFORM READ-BM1-FILE-NEXT.
      *    UNLOCK BM-FILE.
           IF IO-FG = 0
              IF (BM-BRNO = EXT-WI-CL-OWNBR) AND
                   (BM-SSNO = EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO))
                 MOVE BM-MEMO-LAST-LINE-KEY TO BM1-KEY
                 ADD 1 TO BM-SEQ
                 PERFORM BM-MEMO-SCAN
              ELSE
                 PERFORM CLOSE-BM1-FILE
           ELSE
              PERFORM CLOSE-BM1-FILE.

      *********************************************
      *   SCROLL UP ROUTINE
      *********************************************
       BM-MEMO-SCROLL-UP SECTION.
           PERFORM OPEN-BM1-FILE.

           MOVE BM-MEMO-LINE1-KEY TO BM1-KEY.
           PERFORM START-BM1-FILE-LESS-THAN.
           PERFORM READ-BM1-FILE-PREVIOUS.
      *    UNLOCK BM-FILE.
           IF (IO-FG NOT = 0) OR (BM-BRNO NOT = EXT-WI-CL-OWNBR) OR
                 (BM-SSNO NOT = EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO))
              GO TO BM-SCROLL-UP-EXIT.

           IF BM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - BM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO WS-MEMO-DATE-MDY
           ELSE
              MOVE "PRIORITY"  TO WS-MEMO-DATEX.

           MOVE BM-SEQ         TO WS-SEQ.

           IF ( BM-BRNO      = EXT-WI-CL-OWNBR )   AND
              ( BM-SSNO      = EXT-WI-CL-SSNO-NO ) AND
              ( WS-SEQ       = MEMO-SEQ(1) )       AND
              ( WS-MEMO-DATE = MEMO-DATEX(1) )     AND
              ( LM-MEMO      = MEMO-MEMO(1) )
              GO TO BM-SCROLL-UP-EXIT.

           MOVE 0 TO MEMO-LINE.
           MOVE BM-MEMO-LINE1-KEY TO BM1-KEY.
           PERFORM START-BM1-FILE-LESS-THAN.

       BM-SCROLL-UP-READ-PREV.
           PERFORM READ-BM1-FILE-PREVIOUS.
      *    UNLOCK BM-FILE.
           IF (IO-FG NOT = 0) OR (BM-BRNO NOT = EXT-WI-CL-OWNBR) OR
                 (BM-SSNO NOT = EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO))
              GO TO BM-SCROLL-UP-SAVE-LAST-KEY.

           IF HOLD-MEMO-TYPE NOT = "  "
                   AND BM-MEMO-TYPE NOT = HOLD-MEMO-TYPE
              GO TO BM-SCROLL-UP-READ-PREV.

           MOVE MEMO-TAB(7) TO MEMO-TAB(8).
           MOVE MEMO-TAB(6) TO MEMO-TAB(7).
           MOVE MEMO-TAB(5) TO MEMO-TAB(6).
           MOVE MEMO-TAB(4) TO MEMO-TAB(5).
           MOVE MEMO-TAB(3) TO MEMO-TAB(4).
           MOVE MEMO-TAB(2) TO MEMO-TAB(3).
           MOVE MEMO-TAB(1) TO MEMO-TAB(2).
           IF BM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - BM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO MEMO-DATE(1)
           ELSE
              MOVE "PRIORITY" TO MEMO-TAB(1).
           MOVE BM-SEQ  TO MEMO-SEQ(1).
           MOVE BM-MEMO TO MEMO-MEMO(1).
           MOVE BM1-KEY TO BM-MEMO-LINE1-KEY.
           ADD 1 TO MEMO-LINE.
           IF MEMO-LINE < 8
              GO TO BM-SCROLL-UP-READ-PREV.
       BM-SCROLL-UP-SAVE-LAST-KEY.
           IF MEMO-LINE > 0
              IF MEMO-TAB(8) NOT = SPACES
                 MOVE EXT-WI-CL-OWNBR TO BM-MEMO-LAST-LINE-BRNO
                 MOVE EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO)
                                      TO BM-MEMO-LAST-LINE-SSNO
                 MOVE MEMO-SEQ(8)     TO BM-MEMO-LAST-LINE-SEQ
                 IF MEMO-DATEX(8) = "PRIORITY"
                    MOVE 0 TO BM-MEMO-LAST-LINE-DATE
                 ELSE
                    MOVE MEMO-DATE(8) TO DATE-MMDDYY
                    PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                    COMPUTE BM-MEMO-LAST-LINE-DATE = 99999999
                                              - DATE-YYYYMMDD.
           MOVE MEMO-BUF TO WR-BUF.
           MOVE MEMO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       BM-SCROLL-UP-EXIT.
           PERFORM CLOSE-BM1-FILE.
