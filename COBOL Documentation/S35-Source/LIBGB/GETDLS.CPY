      * COPYMEMBER: LIBGB/GETDLS
      **************************************************************************
      * TEST FOR DAYLIGHT SAVINGS TIME.
      *
      * COPY  : LIBGB/GETDLSW
      *
      * INPUT  DLS-DATE - CURRENT DATE (YYYYMMDD) TO DETERMINE IF DLS IN EFFECT.
      *
      * OUTPUT: DLS-FG   "Y" IF DLS IN EFFECT
      *                  "N" IF DLS NOT IN EFFECT
      *    
      * UNDER LEGISLATION ENACTED IN 1986, DAYLIGHT SAVING TIME
      * IN THE USA:
      *             * BEGINS AT 2:00AM ON THE FIRST SUNDAY IN APRIL
      *             * ENDS   AT 2:00AM ON THE LAST SUNDAY IN OCTOBER
      *  FOR PURPOSES OF COLLECTIONS I WILL ASSUME THAT ALL CALLES ARE PLACED
      *  DURING WORK HOURS AND THAT THE 2:00AM CHANGE OVER WILL NOT COME INTO
      *  PLAY
      *
      *  REV:
      *  BLV 000911 CHANGED TRANS-DATE TO DLS-DATE
      *  BLV 030403 FIX WHAT LOOKED LIKE A MAJOR OOPS HERE, WAS CHECKING
      *             FOR DLS-MM OF 4 WHEN IT WAS TRYING TO DO THE OCTOBER
      *             CHECKS, FIXED TO CHECK FOR 10!
      *  BLM 130319 FIX ROUTINES TO CHECK FOR CURRENT DLS LAWS:
      *             DAYLIGHT SAVINGS STARTS 2AM 2ND SUNDAY IN MARCH
      *             DAYLIGHT SAVINGS ENDS   2AM 1ST SUNDAY IN NOVEMBER
      **************************************************************************
       GET-DLS SECTION.
           MOVE "N" TO DLS-FG.
      *    -------------------------------------------------------------
      *    IF MONTH = JAN, FEB OR DEC THEN NOT ON DLS.
      *    -------------------------------------------------------------
           IF DLS-MM < 3 OR > 11
              GO TO GET-DLS-EXIT.

      *    -------------------------------------------------------------
      *    IF MONTH = APRIL, MAY, JUN, JUL, AUG, SEP, NOV DLS IN EFFECT.
      *    -------------------------------------------------------------
           IF DLS-MM > 3 AND < 11
              MOVE "Y" TO DLS-FG
              GO TO GET-DLS-EXIT.

      *    -------------------------------------------------------------
      *    IF MONTH = MARCH
      *      IF DAY > 14 THEN DLS IS IN EFFECT.
      *    ELSE
      *      IF DAY IS 2ND SUNDAY OR LATER THEN DLS IS IN EFFECT
      *    -------------------------------------------------------------
           IF DLS-MM = 3
              IF DLS-DD > 14
                 MOVE "Y" TO DLS-FG
                 GO TO GET-DLS-EXIT
              ELSE
                 MOVE DLS-DATE TO NUM-DATE
                 MOVE 01       TO NUM-DA
                 PERFORM WEEKDAY
                 EVALUATE TRUE
                    WHEN DAY-IS-MON  IF DLS-DD > 13 MOVE "Y" TO DLS-FG
                                     END-IF
                    WHEN DAY-IS-TUE  IF DLS-DD > 12 MOVE "Y" TO DLS-FG
                                     END-IF
                    WHEN DAY-IS-WED  IF DLS-DD > 11 MOVE "Y" TO DLS-FG
                                     END-IF
                    WHEN DAY-IS-THU  IF DLS-DD > 10 MOVE "Y" TO DLS-FG
                                     END-IF
                    WHEN DAY-IS-FRI  IF DLS-DD > 9 MOVE "Y" TO DLS-FG
                                     END-IF
                    WHEN DAY-IS-SAT  IF DLS-DD > 8 MOVE "Y" TO DLS-FG
                                     END-IF
                    WHEN DAY-IS-SUN  IF DLS-DD > 7 MOVE "Y" TO DLS-FG
                                     END-IF
                    WHEN OTHER                     MOVE "N" TO DLS-FG
                 END-EVALUATE
              END-IF
           ELSE
      *    -------------------------------------------------------------
      *    IF MONTH = NOVEMBER
      *      IF DAY FALLS BEFORE FIRST SUNDAY THEN DLS IS IN EFFECT
      *    -------------------------------------------------------------
           IF DLS-MM = 11
              MOVE DLS-DATE TO NUM-DATE
              MOVE 01       TO NUM-DA
              PERFORM WEEKDAY
              EVALUATE TRUE
                 WHEN DAY-IS-MON IF DLS-DD < 7 MOVE "Y" TO DLS-FG
                                 END-IF
                 WHEN DAY-IS-TUE IF DLS-DD < 6 MOVE "Y" TO DLS-FG
                                 END-IF
                 WHEN DAY-IS-WED IF DLS-DD < 5 MOVE "Y" TO DLS-FG
                                 END-IF
                 WHEN DAY-IS-THU IF DLS-DD < 4 MOVE "Y" TO DLS-FG
                                 END-IF
                 WHEN DAY-IS-FRI IF DLS-DD < 3 MOVE "Y" TO DLS-FG
                                 END-IF
                 WHEN DAY-IS-SAT IF DLS-DD < 2 MOVE "Y" TO DLS-FG
                                 END-IF
                 WHEN OTHER                     MOVE "N" TO DLS-FG
              END-EVALUATE.
  
       GET-DLS-EXIT.
              EXIT.
