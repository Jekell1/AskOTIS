      * COPYMEMBER: LIBGB/PROGID
      ******************************************************************
      *
      *   IDENTIFICATION: LIBGB/PROGID
      *
      *   DESCRIPTION   : PROGRESS INDICATOR
      *
      *-----------------------------------------------------------------
      *   IN:  PROGID-BRANCH
      *        PROGID-LINE-LIST
      *        PROGID-PRU-LP
      *        PROGID-ROUTE-BUF
      *        PROGID-SLEEP-COUNTER-MAX
      *        PROGID-TEXT
      *        PROGID-TYPE
      *        PROGID-TIME
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 121798 ***NEW***
      * KEC 010799 ADDED 'PROGID-TIME' LOGIC.
      *
      ******************************************************************
      *
      *    P R O G I D - D I S P L A Y
      *
      ******************************************************************
       PROGID-DISPLAY SECTION.
      *-----------------------------------------------------------------
       PROGID-DISPLAY-BEGIN.

           IF ( PROGID-SLEEP-COUNTER < PROGID-SLEEP-COUNTER-MAX )
              ADD 1 TO PROGID-SLEEP-COUNTER
              GO TO PROGID-DISPLAY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES TO PROGID-SLEEP-COUNTER.

      *-----------------------------------------------------------------
      *    SETTING UP THE 'PROGRESS INDICTATOR' LINE DISPLAY
      *-----------------------------------------------------------------
       PROGID-DISPLAY-SETUP.

           MOVE "(" TO PROGID-PARENTHESIS-LEFT.
           MOVE ")" TO PROGID-PARENTHESIS-RIGHT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PROGID-INDICATOR = "-" ) MOVE "\" TO PROGID-INDICATOR
           ELSE
           IF ( PROGID-INDICATOR = "\" ) MOVE "|" TO PROGID-INDICATOR
           ELSE
           IF ( PROGID-INDICATOR = "|" ) MOVE "/" TO PROGID-INDICATOR
           ELSE
           IF ( PROGID-INDICATOR = "/" ) MOVE "-" TO PROGID-INDICATOR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PROGID-TYPE-BRANCH )
              MOVE "BRANCH: "              TO PROGID-BRANCH-PROMPT
              MOVE PROGID-BRANCH           TO PROGID-BRANCH-NUMBER
              MOVE " - "                   TO PROGID-BRANCH-HYPHEN
              MOVE PROGID-TEXT             TO PROGID-BRANCH-TEXT
           ELSE
              MOVE PROGID-TEXT             TO PROGID-LINE-TEXT.

           IF ( PROGID-TIME NOT = SPACES )
              MOVE SPACES      TO PROGID-TIME-FILLER
              MOVE PROGID-TIME TO PROGID-TIME-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SENDING THE 'PROGRESS INDICTATOR' LINE DISPLAY TO THE SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PROGID-ROUTE-MENU-DRIVEN     ) AND
              ( PROGID-QUEUE-PRINTER-OR-VIEW )
              MOVE PROGID-LINE TO WR-BUF
              MOVE PROGID-LINE-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      *=================================================================
       PROGID-DISPLAY-EXIT.
           EXIT.
