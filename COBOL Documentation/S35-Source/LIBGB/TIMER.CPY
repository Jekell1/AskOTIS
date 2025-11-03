      * COPYMEMBER: LIBGB/TIMER
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/TIMER.CPY
      *
      * DESCRIPTION   : TIMER ROUTINES
      *                 USED FOR PROGRAMS THAT DO NOT HAVE:
      *                 - LIBGB/EOINIT WHICH SETS TIMER TYPE TO
      *                   BEING A REPORT, AS WELL AS STARTS THE
      *                   TIMER SO THAT IT DISPLAYS BOX WINDOW RESULTS
      *                 - LIBGB/GETPR WHICH VIA PRU-OUT WILL STOP
      *                   THE TIMER
      *
      * NEEDED        : LIBGB/PRU_LNK.CPY  (FOR P-TIMER WORKERS)
      *
      *=================================================================
      * LINKAGE USES TIMER-TYPE WHICH IT SETTINGS DOES:
      *
      * "E" = EXCEL   | DISPLAY RESULTS WITH MS EXCEL FORMAT
      *               | AND RETURN RESULTS.
      *
      * "R" = REPORT  | DISPLAY RESULTS WITHOUT MS EXCEL FORMAT
      *               | AND RETURN RESULTS.
      *
      * "U" = UTILITY | RETURNS RESULTS; NO DISPLAY
      *
      *=================================================================
      * REV:
      * JKC 2023-1207 ***NEW***  USED FOR GB/TIMER.CBL
      *
      ******************************************************************
      *
      *    T I M E R - F O R C E - S T A R T
      *
      *=================================================================
      * IN  : EXT-ROUTE-BUF
      * OUT : P-TIMER-DAYSEC-BEG
      *       P-TIMER-TYPE-SET
      * DESC: FORCE THE TIMER TO START; SETTING THE TYPE TO BE A 
      *       (R)EPORT SO THAT IT DISPLAYS BOX WINDOW OF RESULTS
      ******************************************************************
       TIMER-FORCE-START SECTION.

           IF ( EXT-ROUTE-BUF NOT = "00" )
      * UTILITY OPTION
              MOVE "U"    TO P-TIMER-TYPE
              MOVE ZEROES TO P-TIMER-DAYSEC-BEG
              GO TO TIMER-FORCE-START-EXIT.

      * REPORT OPTION
           MOVE "R"    TO P-TIMER-TYPE. 
           MOVE ZEROES TO P-TIMER-DAYSEC-BEG.

           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

      *=================================================================
       TIMER-FORCE-START-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T I M E R - F O R C E - S T O P
      *
      *=================================================================
      * IN  : EXT-ROUTE-BUF
      *       P-TIMER-DAYSEC-BEG
      *       P-TIMER-TYPE
      * OUT : NOTHING
      * DESC: FORCE THE TIMER TO STOP; SETTING THE TYPE TO BE A 
      *       (R)EPORT SO THAT IT DISPLAYS BOX WINDOW OF RESULTS
      ******************************************************************
       TIMER-FORCE-STOP SECTION.

           IF ( EXT-ROUTE-BUF NOT = "00" ) OR
              ( NOT P-TIMER-TYPE-REPORT  )
              GO TO TIMER-FORCE-STOP-EXIT.

           MOVE "GB/TIMER" TO FORM-NAM
           CALL FORM-PROGX USING P-TIMER-LINK-BUF
           CANCEL FORM-PROGX.

      *=================================================================
       TIMER-FORCE-STOP-EXIT.
           EXIT.

