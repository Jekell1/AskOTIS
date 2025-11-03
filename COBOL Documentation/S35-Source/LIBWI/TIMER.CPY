      * COPYMEMBER: LIBWI/TIMER
      ******************************************************************
      *
      * IDENTIFICATION:  LIBWI/TIMER
      *
      * DESCRIPTION   :  TIMER FOR RUNNING CONVERSIONS/FIXES/REPORTS
      *
      * NEED INFO.    :  LIBGB/TIMEIO
      *                  LIBGB/TIMEWK
      *                  LIBGB/GBPRINT
      *                  LIBWI/WINDOWS
      *                  LIBWI/WINDOWSW
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 2000.0223 ***NEW***
      *
      ******************************************************************
      *
      *    T I M E R - R O U T I N E
      *
      ******************************************************************
       TIMER-ROUTINE SECTION.

           PERFORM TIMER-INITIALIZATION.
           PERFORM TIMER-SETTING.

           IF ( TIMER-ACCEPT-YN NOT = "Y" )
              GO TO TIMER-ROUTINE-END.

           PERFORM TIMER-DRIVER.

       TIMER-ROUTINE-END.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TIMER-SCREEN.
           PERFORM CLOSE-WINDOW.

       TIMER-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T I M E R - I N I T I A L I Z A T I O N
      *
      ******************************************************************
       TIMER-INITIALIZATION SECTION.

           MOVE 06         TO WINDOW-LINES.
           MOVE 33         TO WINDOW-COLUMNS.
           MOVE 17         TO WINDOW-BEGIN-Y.
           MOVE 24         TO WINDOW-BEGIN-X.
           MOVE "TIMER"    TO VDU-FORM-NAME.
           MOVE "WI/TIMER" TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY TIMER-SCREEN.

           MOVE TIMER-FORM-FIELDS TO WR-BUF.
           MOVE TIMER-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "ACCEPT (Y/N):_" TO TIMER-ACCEPT-BUF.
           PERFORM TIMER-SEND-ACCEPT.
           PERFORM TIMER-GET-CURRENT-TIME.
           PERFORM TIMER-SEND-DISPLAY-TIME-XX.

       TIMER-INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T I M E R - S E T T I N G
      *
      ******************************************************************
       TIMER-SETTING SECTION.
       TIMER-SETTING-BEGIN.

           MOVE "N" TO TIMER-ACCEPT-YN.

      *-----------------------------------------------------------------
       TIMER-SETTING-START-TIME-HH.

           MOVE "ENNN020START-HH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO TIMER-SETTING-BEGIN.
           IF (        F7-KEY ) GO TO TIMER-SETTING-END.
           IF ( NOT ENTER-KEY ) GO TO TIMER-SETTING-START-TIME-HH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO TIMER-START-TIME-HH     TIMER-VERIFY-TIME-HH.

           IF ( NOT TIMER-VERIFY-TIME-HH-VALID )
              MOVE "INVALID: ENTER HOURS '01' THRU '12'" TO MESS
              PERFORM SEND-MESS
              GO TO TIMER-SETTING-START-TIME-HH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ SENDING  }
           MOVE "S  A020START-HH."  TO VDU.
           MOVE TIMER-START-TIME-HH TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TIMER-SETTING-START-TIME-MM.

           MOVE "ENNN020START-MM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO TIMER-SETTING-START-TIME-HH.
           IF (        F1-KEY ) GO TO TIMER-SETTING-BEGIN.
           IF (        F7-KEY ) GO TO TIMER-SETTING-END.
           IF ( NOT ENTER-KEY ) GO TO TIMER-SETTING-START-TIME-MM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO TIMER-START-TIME-MM     TIMER-VERIFY-TIME-MM.

           IF ( NOT TIMER-VERIFY-TIME-MM-VALID )
              MOVE "INVALID: ENTER MINUTES '00' THRU '59'" TO MESS
              PERFORM SEND-MESS
              GO TO TIMER-SETTING-START-TIME-MM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ SENDING  }
           MOVE "S  A020START-MM."  TO VDU.
           MOVE TIMER-START-TIME-MM TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TIMER-SETTING-START-TIME-SS.

           MOVE ZEROES TO TIMER-START-TIME-SS      TIMER-VERIFY-TIME-SS.

      *-----------------------------------------------------------------
       TIMER-SETTING-START-TIME-XX.

           MOVE "E  A020START-SS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO TIMER-SETTING-START-TIME-MM.
           IF (        F1-KEY ) GO TO TIMER-SETTING-BEGIN.
           IF (        F7-KEY ) GO TO TIMER-SETTING-END.
           IF ( NOT ENTER-KEY ) GO TO TIMER-SETTING-START-TIME-XX.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO TIMER-START-TIME-XX      TIMER-VERIFY-TIME-XX.

           IF ( NOT TIMER-VERIFY-TIME-XX-VALID )
              MOVE "INVALID: ENTER 'AM' OR 'PM' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO TIMER-SETTING-START-TIME-XX.

      *-----------------------------------------------------------------
       TIMER-SETTING-ACCEPT-YN.

           MOVE "E  A010TIMER_ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO TIMER-SETTING-START-TIME-XX.
           IF (        F1-KEY ) GO TO TIMER-SETTING-BEGIN.
           IF (        F7-KEY ) GO TO TIMER-SETTING-END.
           IF ( NOT ENTER-KEY ) GO TO TIMER-SETTING-ACCEPT-YN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO TIMER-ACCEPT-YN.

           IF ( TIMER-ACCEPT-YN = "Y" ) GO TO TIMER-SETTING-EXIT.
           IF ( TIMER-ACCEPT-YN = "N" ) GO TO TIMER-SETTING-EXIT.

           GO TO TIMER-SETTING-ACCEPT-YN.

       TIMER-SETTING-END.

           IF ( F7-KEY ) MOVE "X" TO TIMER-ACCEPT-YN.

       TIMER-SETTING-EXIT.
           EXIT.

      ******************************************************************
       TIMER-DRIVER SECTION.
      ******************************************************************

           PERFORM TIMER-SEND-ACCEPT.

       TIMER-DRIVER-LOOP.

           PERFORM TIMER-GET-CURRENT-TIME.
           PERFORM TIMER-SEND-DISPLAY-TIME.

           IF ( TIMER-CURRENT-TIME-HH = TIMER-START-TIME-HH ) AND
              ( TIMER-CURRENT-TIME-MM = TIMER-START-TIME-MM ) AND
              ( TIMER-CURRENT-TIME-XX = TIMER-START-TIME-XX )
              GO TO TIMER-DRIVER-EXIT.

           GO TO TIMER-DRIVER-LOOP.

       TIMER-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       TIMER-MISCELLANEOUS SECTION.
      ******************************************************************
       TIMER-GET-CURRENT-TIME.

           PERFORM GET-TIME.
           MOVE CUR-HH TO TIMER-CURRENT-TIME-HH.
           MOVE CUR-MM TO TIMER-CURRENT-TIME-MM.
           MOVE CUR-SS TO TIMER-CURRENT-TIME-SS.
           MOVE T-AMPM TO TIMER-CURRENT-TIME-XX.

       TIMER-SEND-ACCEPT.

           MOVE TIMER-ACCEPT-BUF TO WR-BUF.
           MOVE TIMER-ACCEPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES TO TIMER-ACCEPT-BUF.

       TIMER-SEND-DISPLAY-TIME.

           MOVE SPACES                TO TIMER-DISPLAY-TIME-BUF.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ CURRENT TIME }
           MOVE SPACES                TO TIMER-FORMAT-TIME.
           MOVE TIMER-CURRENT-TIME-HH TO TIMER-FORMAT-TIME-HH.
           MOVE ":"                   TO TIMER-FORMAT-TIME-COLUMN.
           MOVE TIMER-CURRENT-TIME-MM TO TIMER-FORMAT-TIME-MM.
           MOVE "."                   TO TIMER-FORMAT-TIME-PERIOD.
           MOVE TIMER-CURRENT-TIME-SS TO TIMER-FORMAT-TIME-SS.
           MOVE SPACES                TO TIMER-FORMAT-TIME-SPACE.
           MOVE TIMER-CURRENT-TIME-XX TO TIMER-FORMAT-TIME-XX.

           MOVE TIMER-FORMAT-TIME     TO TIMER-DISPLAY-CURRENT-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ START TIME }
           MOVE TIMER-START-TIME-HH   TO TIMER-FORMAT-TIME-HH.
           MOVE ":"                   TO TIMER-FORMAT-TIME-COLUMN.
           MOVE TIMER-START-TIME-MM   TO TIMER-FORMAT-TIME-MM.
           MOVE "."                   TO TIMER-FORMAT-TIME-PERIOD.
           MOVE TIMER-START-TIME-SS   TO TIMER-FORMAT-TIME-SS.
           MOVE SPACES                TO TIMER-FORMAT-TIME-SPACE.
           MOVE TIMER-START-TIME-XX   TO TIMER-FORMAT-TIME-XX.

           MOVE TIMER-FORMAT-TIME     TO TIMER-DISPLAY-START-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TIMER-DISPLAY-TIME-BUF TO WR-BUF.
           MOVE TIMER-DISPLAY-TIME-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       TIMER-SEND-DISPLAY-TIME-XX.

           MOVE SPACES                TO TIMER-DISPLAY-TIME-BUF.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ CURRENT TIME }
           MOVE SPACES                TO TIMER-FORMAT-TIME.
           MOVE TIMER-CURRENT-TIME-HH TO TIMER-FORMAT-TIME-HH.
           MOVE ":"                   TO TIMER-FORMAT-TIME-COLUMN.
           MOVE TIMER-CURRENT-TIME-MM TO TIMER-FORMAT-TIME-MM.
           MOVE SPACES                TO TIMER-FORMAT-TIME-PERIOD.
           MOVE TIMER-CURRENT-TIME-XX TO TIMER-FORMAT-TIME-SSX.
      *KC->MOVE SPACES                TO TIMER-FORMAT-TIME-SPACE.
      *KC->MOVE TIMER-CURRENT-TIME-XX TO TIMER-FORMAT-TIME-XX.

           MOVE TIMER-FORMAT-TIME     TO TIMER-DISPLAY-CURRENT-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ START TIME }
           MOVE TIMER-CURRENT-TIME-HH TO TIMER-FORMAT-TIME-HH.
           MOVE ":"                   TO TIMER-FORMAT-TIME-COLUMN.
           MOVE TIMER-CURRENT-TIME-MM TO TIMER-FORMAT-TIME-MM.
           MOVE SPACES                TO TIMER-FORMAT-TIME-PERIOD.
           MOVE TIMER-CURRENT-TIME-XX TO TIMER-FORMAT-TIME-SSX.
      *KC->MOVE SPACES                TO TIMER-FORMAT-TIME-SPACE.
      *KC->MOVE TIMER-CURRENT-TIME-XX TO TIMER-FORMAT-TIME-XX.

           MOVE TIMER-FORMAT-TIME     TO TIMER-DISPLAY-START-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TIMER-DISPLAY-TIME-BUF TO WR-BUF.
           MOVE TIMER-DISPLAY-TIME-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

