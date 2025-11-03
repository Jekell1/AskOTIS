      * COPYMEMBER: LIBWI/WIDMEMO
      *************************************************************************
      *    DM   M E M O
      *    GLF 110498 MADE DATE 2000 COMPATIBLE
      *    GLF 990413 FIX NOT BEING ABLE TO AUDIT PRIORITY
      *               MEMOS (Y2K PROBLEM)
      *  MJD 001019 CHANGED TO ACCEPT PAGE-UP AND PAGE-DOWN (AS PER JTG)
      *  BLV 020214 CHANGED PERFORMS OF VERIFY-PASSWORD TO
      *             VERIFY-MEMO-PASSWORD TO AVOID CONFUSIONS &
      *             CONFLICTS WITH VERIFY-PASSWORD ROUTINE IN
      *             SPINQ1 WHICH COPIES THIS ROUTINE; PROBLEM WAS THIS
      *             WHEN THIS ROUTINE PERFORMED VERIFY-PASSWORD IN SPINQ1,
      *             THE PASSWDW-PGM GET SET TO SPINQ1 INSTEAD OF AUDMEM
      *             OR DELAUD
      *  BAH 110208 REWRITING AN EXISTING MEMO WAS NOT WORKING, CHANGE
      *             WAS NOT BEING SAVED, MOVE VDUBUF WAS REMOVED FROM ABOVE
      *             CHANGE, PUT IT BACK. UNITED PL# 724
      *  BLM 130530 REPLACED RESET-MESS WITH CALL-WRFORM
      *  BAH 160217 SET DM-BRNO PD0003
      *  BAH 170420 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181015 ADDED DM-BRNO TO DM1-KEY, GLOBAL DMFILE
      *  BAH 20190624 PRIORITY MEMO EDIT, PRESS F3, F4-DATE, F3 AND IT WAS
      *               DISPLAYING DATE, NOT "PRIORITY". NEEDED TO MOVE
      *               99999999 TO VDU-DATE-YYYYMMDD BECAUSE VDUNUM0 WAS ONLY
      *               999999. QA0038
      ************************************************************************
       DM-MEMO-MODULE SECTION.
       DM-MEMO-RESET.
      *    PERFORM RESET-MESS.
           MOVE MEMO-ENTRY-LIST TO WR-LIST.
           MOVE SPACES          TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "N" TO MEMO-AUDIT-FLAG.

           GO TO DM-MEMO-SEND-DATE.
       DM-MEMO-ENTER-DATE.
           MOVE DM-MSG1 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE EXT-WI-CL-OWNBR TO DM-BRNO.
           MOVE EXT-WI-CL-DLNO  TO DM-DL-DLNO.
           MOVE "ER M060MEMODATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
      *       PERFORM RESET-MESS
              MOVE "MEMODATE." TO WR-LIST
              MOVE SPACES TO WR-BUF
              PERFORM CALL-WRFORM
              GO TO DM-MEMO-MODULE-EXIT.
           IF UP-KEY OR PAGE-UP-KEY
      * SCROLL BACKWARD FROM CURRENT FILE POINTER
              PERFORM DM-MEMO-SCROLL-UP
              GO TO DM-MEMO-ENTER-DATE.
           IF DOWN-KEY OR PAGE-DOWN-KEY
      * SCROLL FORWARD FROM CURRENT FILE POINTER
              PERFORM DM-MEMO-SCROLL-DOWN
              GO TO DM-MEMO-ENTER-DATE.
           IF VDUSTATUS = "16"
              MOVE "S  A080MEMODATE." TO VDU
              MOVE "PRIORITY" TO VDUBUF
              PERFORM SCREENIO
              MOVE 999999   TO VDUNUM0
              MOVE 99999999 TO VDU-DATE-YYYYMMDD
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO DM-MEMO-ENTER-DATE.

           MOVE VDU-DATE-YYYYMMDD TO DATE-YYYYMMDD.
           
           IF DATE-YYYYMMDD = 0
              MOVE "S  A080MEMODATE." TO VDU
              MOVE MEMO-HOLD-SYS TO DM-DATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT 
              PERFORM SCREENIO
              MOVE "Y" TO MEMO-IS-TODAY
           ELSE
              IF DATE-MMDDYY NOT = 99999999
                 IF MEMO-HOLD-SYS < DATE-YYYYMMDD
                    GO TO DM-MEMO-ENTER-DATE
                 ELSE
                    IF MEMO-HOLD-SYS = DATE-YYYYMMDD
                       MOVE "Y" TO MEMO-IS-TODAY
                    ELSE
                       IF MEMO-VERIFY-PASSWORD = "N"
                          MOVE "AUDMEM" TO PASSWDW-PGM
                          MOVE "TO ADJUST OLD MEMO ENTRIES:"
                                           TO PASSWDW-PROMPT2
                          MOVE 0 TO PASSWDW-SEQ
                          PERFORM VERIFY-MEMO-PASSWORD
                          IF PASSWDW-STAT = "OK"
                             MOVE "Y" TO MEMO-VERIFY-PASSWORD
                          ELSE
                             GO TO DM-MEMO-RESET.

       DM-MEMO-SEND-DATE.
           MOVE EXT-WI-CL-OWNBR TO DM-BRNO.
           MOVE EXT-WI-CL-DLNO  TO DM-DL-DLNO.

           IF MEMO-AUDIT-FLAG NOT = "Y"
              MOVE "S  A080MEMODATE." TO VDU
              MOVE MEMO-HOLD-SYS TO DM-DATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT 
              PERFORM SCREENIO.

           IF DATE-MMDDYY = 99999999
              MOVE 0 TO DM-DATE
           ELSE
              COMPUTE DM-DATE = 99999999 - DATE-YYYYMMDD.

           IF MEMO-AUDIT-FLAG NOT = "Y"
              MOVE 0 TO DM-SEQ
              GO TO DM-MEMO-NEXT-SEQ.

       DM-MEMO-ENTER-SEQ.
           MOVE DM-MSG2 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "EN N020MEMOSEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR = "1U"
              GO TO DM-MEMO-RESET.
           IF VDUSTATUS NOT = "00"
              GO TO DM-MEMO-ENTER-SEQ.
           MOVE VDUNUM0 TO DM-SEQ.

       DM-MEMO-NEXT-SEQ.
           IF MEMO-AUDIT-FLAG NOT = "Y"
              IF DM-SEQ = 999
                 MOVE "MEMO LIMIT FOR TODAY HAS BEEN EXCEEDED!"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO DM-MEMO-ENTER-DESC
              ELSE
                 ADD 1 TO DM-SEQ.

           PERFORM OPEN-DM1-FILE.
           PERFORM READ-DM1-FILE.
           MOVE IO-FG TO MEMO-IO-FG.
           PERFORM CLOSE-DM1-FILE.

           IF MEMO-AUDIT-FLAG NOT = "Y"
              IF IO-FG NOT = 0
                 GO TO DM-MEMO-SEND-SEQ
              ELSE
                 GO TO DM-MEMO-NEXT-SEQ.

           IF IO-FG NOT = 0
              GO TO DM-MEMO-RESET.

           IF DM-MEMO-TYPE NOT = "  "
              MOVE
               "SYSTEM GENERATED MEMO LINE, CAN'T CHANGE"
                                             TO MESS
              PERFORM SEND-MESS
              GO TO DM-MEMO-RESET.

           MOVE "S  A600MEMODESC." TO VDU.
           MOVE DM-MEMO TO VDUBUF.
           PERFORM SCREENIO.
           MOVE DM1-KEY TO DM-MEMO-HOLD-KEY.
           GO TO DM-MEMO-ENTER-DESC.

       DM-MEMO-SEND-SEQ.
           MOVE "SN N020MEMOSEQ." TO VDU.
           MOVE DM-SEQ TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE DM1-KEY TO DM-MEMO-HOLD-KEY.

       DM-MEMO-ENTER-DESC.

           IF MEMO-AUDIT-FLAG = "Y"
              IF MEMOSC-ONLY = " " OR "T"
                 MOVE DM-MSG3-AUDIT-TOGGLE TO LEGEND
              ELSE
                 MOVE DM-MSG3-AUDIT TO LEGEND
           ELSE
              IF MEMOSC-ONLY = " " OR "T"
                 MOVE DM-MSG3-TOGGLE TO LEGEND
              ELSE
                 MOVE DM-MSG3 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A600MEMODESC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO DM-MEMO-ENTER-DESC.
           IF VDUSTATUS = "16"
              MOVE "S  A080MEMODATE." TO VDU
              MOVE "PRIORITY" TO VDUBUF
              PERFORM SCREENIO
              MOVE EXT-WI-CL-OWNBR TO DM-BRNO
              MOVE EXT-WI-CL-DLNO  TO DM-DL-DLNO
              MOVE 0             TO DM-DATE DM-SEQ VDUNUM0
              GO TO DM-MEMO-NEXT-SEQ.
           IF VDUSTATUS = "1<"
              IF MEMOSC-ONLY NOT = "D"
                 GO TO DM-MEMO-MODULE-EXIT.
           IF VDUSTATUS = "1>"
      *       PERFORM RESET-MESS
              MOVE "MEMODATE." TO WR-LIST
              MOVE SPACES TO WR-BUF
              PERFORM CALL-WRFORM
              GO TO DM-MEMO-MODULE-EXIT.
           IF VDUSTATUS = "18"
              MOVE "Y" TO MEMO-AUDIT-FLAG
              GO TO DM-MEMO-ENTER-DATE.
           IF UP-KEY OR PAGE-UP-KEY
      * SCROLL BACKWARD FROM CURRENT FILE POINTER
              PERFORM DM-MEMO-SCROLL-UP
              GO TO DM-MEMO-ENTER-DESC.
           IF DOWN-KEY OR PAGE-DOWN-KEY
      * SCROLL FORWARD FROM CURRENT FILE POINTER
              PERFORM DM-MEMO-SCROLL-DOWN
              GO TO DM-MEMO-ENTER-DESC.
           IF VDUSTATUS NOT = "00"
              GO TO DM-MEMO-ENTER-DESC.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO DM-MEMO-ENTER-DESC.

           IF VDUBUF = SPACES
              GO TO DM-MEMO-ENTER-DESC.

           IF DM-SEQ = 999
              GO TO DM-MEMO-ENTER-DESC.

           IF VDUBUF = "DELETE"
              IF MEMO-AUDIT-FLAG = "Y"
                 PERFORM OPEN-DM1-FILE
                 MOVE DM-MEMO-HOLD-KEY TO DM1-KEY
                 PERFORM READ-DM1-FILE
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
                             MOVE DM-MEMO TO VDUBUF
                             PERFORM SCREENIO
                             GO TO DM-MEMO-ENTER-DESC
                          END-IF
                       END-IF
                    END-IF
                    PERFORM DELETE-DM1-FILE
                    PERFORM CLOSE-DM1-FILE
                    MOVE 0 TO DM-SEQ
                    PERFORM DM-MEMO-SCAN
                    GO TO DM-MEMO-RESET
                 ELSE
                    PERFORM CLOSE-DM1-FILE
                    GO TO DM-MEMO-ENTER-DESC
              ELSE
                 GO TO DM-MEMO-ENTER-DESC.
       DM-MEMO-UPDATE.
           PERFORM OPEN-DM1-FILE.
           PERFORM DETERMINE-DM-VDUBUF.
           MOVE HOLD-MEMO-TYPE TO DM-MEMO-TYPE.
           MOVE DM-MEMO-HOLD-KEY TO DM1-KEY.
           IF MEMO-IO-FG = 0
              PERFORM READ-DM1-FILE
              IF IO-FG = 0
                 PERFORM DETERMINE-DM-VDUBUF
                 PERFORM REWRITE-DM1-FILE
                 PERFORM CLOSE-DM1-FILE
                 GO TO CONTINUE-DM-MEMO-UPDATE.
           MOVE EXT-WI-CL-OWNBR TO DM-BRNO.
           PERFORM WRITE-DM1-FILE.
           IF IO-FG = 9
              IF E-CODE NOT = "22"
                 GO TO IO-ERROR
              ELSE
                 ADD 1 TO DM-MEMO-HOLD-SEQ
                 IF DM-MEMO-HOLD-SEQ < 999
                    GO TO DM-MEMO-UPDATE
                 ELSE
                    MOVE 999 TO DM-SEQ
                    PERFORM CLOSE-DM1-FILE
                    GO TO DM-MEMO-NEXT-SEQ.
           PERFORM CLOSE-DM1-FILE.
           MOVE "Y" TO MEMO-ENTERED-FG.

       CONTINUE-DM-MEMO-UPDATE.
      *    START SCAN FROM FIRST SEQ# FOR DATE AND DLNO#;
      *    IF MEMO CONTAINS MORE THAN MEMO-MAX LINES, SCAN.

           MOVE 0 TO DM-SEQ.

           PERFORM DM-MEMO-SCAN.

           GO TO DM-MEMO-RESET.

       DM-MEMO-RESET-LINE1-KEY.
           MOVE EXT-WI-CL-OWNBR TO DM-BRNO.
           MOVE EXT-WI-CL-DLNO  TO DM-DL-DLNO.
           MOVE 0               TO DM-DATE DM-SEQ.
           MOVE DM1-KEY TO DM-MEMO-LINE1-KEY.

       DM-MEMO-MODULE-EXIT.
           PERFORM SEND-LEGEND.

      *******************************
       DETERMINE-DM-VDUBUF SECTION.
      *******************************
           MOVE VDUBUF TO DM-MEMO.

      ******************************************************************
      *
      *    DM   M E M O   S C A N
      *    SHOW MEMO-MAX MEMOS
      *
      ******************************************************************
       DM-MEMO-SCAN SECTION.
      D    STOP MESS.
           MOVE "     DEALER MEMO " TO MEMO-WHICH.
           PERFORM SEND-HEAD-3.
           MOVE SPACES TO MEMO-BUF.
           MOVE 0 TO MEMO-LINE.

           PERFORM OPEN-DM1-FILE.

           PERFORM START-DM1-FILE-SCAN.

      *----------------------------------------------------------------
       DM-MEMO-SCAN-NEXT-SEQ.
           PERFORM READ-DM1-FILE-NEXT.
      *    UNLOCK DM-FILE.

           IF (IO-FG = 9) OR (DM-DL-DLNO NOT = EXT-WI-CL-DLNO) OR
                   (DM-BRNO NOT = EXT-WI-CL-OWNBR)
              PERFORM DM-MEMO-BUMP-DOWN
              GO TO DM-MEMO-SCAN-EXIT.

           IF HOLD-MEMO-TYPE NOT = "  "
                   AND DM-MEMO-TYPE NOT = HOLD-MEMO-TYPE
              GO TO DM-MEMO-SCAN-NEXT-SEQ.

           IF MEMO-LINE NOT < MEMO-MAX
              GO TO DM-MEMO-SCAN-EXIT.

           ADD 1 TO MEMO-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF DM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - DM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO MEMO-DATE(MEMO-LINE)
           ELSE
              MOVE "PRIORITY" TO MEMO-TAB(MEMO-LINE).

           MOVE DM-SEQ TO MEMO-SEQ(MEMO-LINE).
           MOVE DM-MEMO TO MEMO-MEMO(MEMO-LINE).

           IF MEMO-LINE = 1
              MOVE DM1-KEY TO DM-MEMO-LINE1-KEY.

           MOVE DM1-KEY TO DM-MEMO-LAST-LINE-KEY.

           GO TO DM-MEMO-SCAN-NEXT-SEQ.

      *----------------------------------------------------------------
       DM-MEMO-SCAN-EXIT.
           MOVE MEMO-BUF TO WR-BUF.
           MOVE MEMO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CLOSE-DM1-FILE.

      ******************************************************************
      *  L M   M E M O   S P A C E
      ******************************************************************
       DM-MEMO-BUMP-DOWN SECTION.
       DM-BUMP-DOWN-START.
           IF MEMO-LINE > 7
              GO TO DM-EXIT-BUMP-DOWN.

           MOVE DM-MEMO-LINE1-KEY TO DM1-KEY.

           PERFORM START-DM1-FILE-LESS-THAN.
       DM-BUMP-DOWN-READ-PREV.
           PERFORM READ-DM1-FILE-PREVIOUS.
      *    UNLOCK DM-FILE.
           IF (IO-FG NOT = 0) OR (DM-DL-DLNO NOT = EXT-WI-CL-DLNO) OR
                   (DM-BRNO NOT = EXT-WI-CL-OWNBR)
              GO TO DM-BUMP-DOWN-SAVE-LAST-KEY.
           IF HOLD-MEMO-TYPE NOT = "  "
                   AND DM-MEMO-TYPE NOT = HOLD-MEMO-TYPE
              GO TO DM-BUMP-DOWN-READ-PREV.

           MOVE MEMO-TAB(7) TO MEMO-TAB(8).
           MOVE MEMO-TAB(6) TO MEMO-TAB(7).
           MOVE MEMO-TAB(5) TO MEMO-TAB(6).
           MOVE MEMO-TAB(4) TO MEMO-TAB(5).
           MOVE MEMO-TAB(3) TO MEMO-TAB(4).
           MOVE MEMO-TAB(2) TO MEMO-TAB(3).
           MOVE MEMO-TAB(1) TO MEMO-TAB(2).
           IF DM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - DM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO MEMO-DATE(1)
           ELSE
              MOVE "PRIORITY" TO MEMO-TAB(1).
           MOVE DM-SEQ TO MEMO-SEQ(1).
           MOVE DM-MEMO TO MEMO-MEMO(1).
           MOVE DM1-KEY TO DM-MEMO-LINE1-KEY.
           ADD 1 TO MEMO-LINE.
           IF MEMO-LINE < 8
              GO TO DM-BUMP-DOWN-READ-PREV.
       DM-BUMP-DOWN-SAVE-LAST-KEY.
           IF MEMO-LINE > 0
              IF MEMO-TAB(MEMO-LINE) NOT = SPACES
                 MOVE EXT-WI-CL-OWNBR TO DM-MEMO-LAST-LINE-BRNO
                 MOVE EXT-WI-CL-DLNO  TO DM-MEMO-LAST-LINE-DLNO
                 MOVE MEMO-SEQ(MEMO-LINE) TO DM-MEMO-LAST-LINE-SEQ
                 IF MEMO-DATEX(MEMO-LINE) = "PRIORITY"
                    MOVE 0 TO DM-MEMO-LAST-LINE-DATE
                 ELSE
                    MOVE MEMO-DATE(MEMO-LINE) TO DATE-MMDDYY
                    PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                    COMPUTE DM-MEMO-LAST-LINE-DATE = 99999999
                                                   - DATE-YYYYMMDD.
       DM-EXIT-BUMP-DOWN.
           EXIT.

      *********************************************
      *   SCROLL DOWN ROUTINE
      *********************************************
       DM-MEMO-SCROLL-DOWN SECTION.
      D    STOP MESS.
           PERFORM OPEN-DM1-FILE.
           MOVE DM-MEMO-LAST-LINE-KEY TO DM1-KEY.
           ADD 1 TO DM-SEQ.

           PERFORM START-DM1-FILE-SCAN.
           PERFORM READ-DM1-FILE-NEXT.
      *    UNLOCK DM-FILE.
           IF IO-FG = 0
              IF (DM-DL-DLNO = EXT-WI-CL-DLNO) AND
                                         (DM-BRNO = EXT-WI-CL-OWNBR)
                 MOVE DM-MEMO-LAST-LINE-KEY TO DM1-KEY
                 ADD 1 TO DM-SEQ
                 PERFORM DM-MEMO-SCAN
              ELSE
                 PERFORM CLOSE-DM1-FILE
           ELSE
              PERFORM CLOSE-DM1-FILE.

      *********************************************
      *   SCROLL UP ROUTINE
      *********************************************
       DM-MEMO-SCROLL-UP SECTION.
      D    STOP MESS.
           PERFORM OPEN-DM1-FILE.
           MOVE DM-MEMO-LINE1-KEY TO DM1-KEY.

           PERFORM START-DM1-FILE-LESS-THAN.
           PERFORM READ-DM1-FILE-PREVIOUS.
      *    UNLOCK DM-FILE.
           IF (IO-FG NOT = 0) OR (DM-DL-DLNO NOT = EXT-WI-CL-DLNO) OR
                   (DM-BRNO NOT = EXT-WI-CL-OWNBR)
              GO TO DM-SCROLL-UP-EXIT.

           IF DM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - DM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO WS-MEMO-DATE-MDY
           ELSE
              MOVE "PRIORITY"  TO WS-MEMO-DATEX.

           MOVE DM-SEQ         TO WS-SEQ.

           IF ( DM-BRNO      = EXT-WI-CL-OWNBR )  AND
              ( DM-DL-DLNO   = EXT-WI-CL-DLNO )   AND
              ( WS-SEQ       = MEMO-SEQ(1) )      AND
              ( WS-MEMO-DATE = MEMO-DATEX(1) )    AND
              ( DM-MEMO      = MEMO-MEMO(1) )
              GO TO DM-SCROLL-UP-EXIT.

           MOVE 0 TO MEMO-LINE.
           MOVE DM-MEMO-LINE1-KEY TO DM1-KEY.

           PERFORM START-DM1-FILE-LESS-THAN.
       DM-SCROLL-UP-READ-PREV.
           PERFORM READ-DM1-FILE-PREVIOUS.
      *    UNLOCK DM-FILE.
           IF (IO-FG NOT = 0) OR (DM-DL-DLNO NOT = EXT-WI-CL-DLNO) OR
                   (DM-BRNO NOT = EXT-WI-CL-OWNBR)
              GO TO DM-SCROLL-UP-SAVE-LAST-KEY.

           IF HOLD-MEMO-TYPE NOT = "  "
                   AND DM-MEMO-TYPE NOT = HOLD-MEMO-TYPE
              GO TO DM-SCROLL-UP-READ-PREV.

           MOVE MEMO-TAB(7) TO MEMO-TAB(8).
           MOVE MEMO-TAB(6) TO MEMO-TAB(7).
           MOVE MEMO-TAB(5) TO MEMO-TAB(6).
           MOVE MEMO-TAB(4) TO MEMO-TAB(5).
           MOVE MEMO-TAB(3) TO MEMO-TAB(4).
           MOVE MEMO-TAB(2) TO MEMO-TAB(3).
           MOVE MEMO-TAB(1) TO MEMO-TAB(2).
           IF DM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - DM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO MEMO-DATE(1)
           ELSE
              MOVE "PRIORITY" TO MEMO-TAB(1).
           MOVE DM-SEQ  TO MEMO-SEQ(1).
           MOVE DM-MEMO TO MEMO-MEMO(1).
           MOVE DM1-KEY TO DM-MEMO-LINE1-KEY.
           ADD 1 TO MEMO-LINE.
           IF MEMO-LINE < 8
              GO TO DM-SCROLL-UP-READ-PREV.
       DM-SCROLL-UP-SAVE-LAST-KEY.
           IF MEMO-LINE > 0
              IF MEMO-TAB(8) NOT = SPACES
                 MOVE EXT-WI-CL-OWNBR  TO DM-MEMO-LAST-LINE-BRNO
                 MOVE EXT-WI-CL-DLNO   TO DM-MEMO-LAST-LINE-DLNO
                 MOVE MEMO-SEQ(8) TO DM-MEMO-LAST-LINE-SEQ
                 IF MEMO-DATEX(8) = "PRIORITY"
                    MOVE 0 TO DM-MEMO-LAST-LINE-DATE
                 ELSE
                    MOVE MEMO-DATE(8) TO DATE-MMDDYY
                    PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                    COMPUTE DM-MEMO-LAST-LINE-DATE = 99999999
                                                   - DATE-YYYYMMDD.
           MOVE MEMO-BUF TO WR-BUF.
           MOVE MEMO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DM-SCROLL-UP-EXIT.
           PERFORM CLOSE-DM1-FILE.
