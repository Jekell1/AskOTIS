      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\TIMEIO.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/TIMEIO
      ******************************************************************
      *              (TIMEIO)
      *   GET/FORMAT CLOCK TIME
      *=================================================================
      * REV:
      * JTG 020594 ADDED TIME-MILITARY-FG
      *
      * JKC 2020-1026 ADDED TIMER-START AND TIMER-STOP SECTIONS.
      *          USED FOR GB/TIMER.CBL.
      *
      ******************************************************************
       GET-TIME SECTION.
           ACCEPT TIME-NOW FROM TIME.
           IF TIME-MILITARY-FG = "Y"
              MOVE " " TO T-AMPM
           ELSE
              IF H-AND-M NOT GREATER THAN 1159
                 MOVE "AM" TO T-AMPM
              ELSE
                 MOVE "PM" TO T-AMPM.
           IF TIME-MILITARY-FG = " "
              IF H-AND-M IS NOT LESS THAN 1300
                 SUBTRACT 1200 FROM H-AND-M.
           IF TIME-MILITARY-FG = " "
              IF CUR-HH = 0
                 MOVE 12 TO CUR-HH.
           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":" TO T-COL.
           MOVE " " TO T-SPACE.

       GET-TIME-EXIT.
           MOVE " " TO TIME-MILITARY-FG.

      ******************************************************************
      *
      *   T I M E R - S T A R T 
      *
      *=================================================================
      * IN  : NOTHING
      * OUT : TIMER-LINK-BUF
      * DESC: USED TO CLEAR ALL TIME FIELDS THEN SET BEG FIELDS.
      ******************************************************************
