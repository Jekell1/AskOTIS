      * A30 CONVERSION COMPLETE
      ******************************************************************
      *
      * DESCRIPTION: DISPLAY A MESSAGE AND BEEP AND SAYS "PRESS ENTER:"
      *              SEND MESSAGE
      *
      *=================================================================
      * REV:
      * BAH 140508 CHANGED TO DISPLAY MESASGE BOX FOR SEND-MESS INSTEAD
      *            OF CALLING GB/SENDME, VERYANT
      * JKC 210106 ADDED SEND-MESS-BOX-LINES FOR USE IN DISPLAYING
      *            MULTIPLE LINES (4 LINES; HEAD, MSG1, MSG2, MSG3).
      * BAH 2024.0308 REMOVED SEND-MESS-LINES
      *
      * JKC 2024-0501 ADDED USE OF EXT-API-SCREEN-DISABLE AND
      *         EXT-API-MESS FOR SUPPRESS SCREEN INTERACTIONS FOR
      *         USE IN API PROGRAMS WITHOUT SCREENS.
      *
      ******************************************************************
       SEND-MESS.
           IF ( EXT-ROUTE-BUF = "70" )
              MOVE MESS TO EXT-EOBUF-MESS
           ELSE
           IF ( EXT-API-SCREEN-DISABLE = "Y" )
              MOVE MESS TO EXT-API-MESS
           ELSE
              DISPLAY MESSAGE BOX MESS.

      *SEND-MESS-LINES.
      *    IF ( EXT-ROUTE-BUF = "70" )
      *       MOVE SENDMESS-HEAD TO EXT-EOBUF-MESS
      *    ELSE
      *    IF ( SENDMESS-MSG3 NOT = SPACES )
      *       DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
      *                           SENDMESS-MSG1, X"0A"
      *                           SENDMESS-MSG2, X"0A"
      *                           SENDMESS-MSG3, X"0A"

      *    ELSE
      *    IF ( SENDMESS-MSG2 NOT = SPACES )
      *       DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
      *                           SENDMESS-MSG1, X"0A"
      *                           SENDMESS-MSG2, X"0A"
      *    ELSE
      *    IF ( SENDMESS-MSG1 NOT = SPACES )
      *       DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
      *                           SENDMESS-MSG1, X"0A"
      *    ELSE
      *       DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A".

