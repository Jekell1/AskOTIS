      * COPYMEMBER: LIBUP/UP3234.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBUP/UP3234.CPY
      *
      * DESCRIPTION   : ROUTINES FOR UPGRADE A30 TO A32
      *
      *                 UP3234-CREATE-DETAIL-LINE
      *                 UP3234-CREATE-BRANCH-HEAD
      *                 UP3234-CREATE-MACHINE-HEAD
      *                 UP3234-CREATE-FILE-ERROR
      *                 UP3234-CREATE-FILE-TOTALS
      *                 UP3234-LOAD-FILE-PATH
      *                 UP3234-VPI-DISPLAY
      *                 UP3234-VPI-START
      *
      *=================================================================
      * REV:
      * KEC 2018-0918 ***NEW*** LOCAL-TO-GLOBAL L2G
      *
      ******************************************************************
      *
      *    U P 3 2 3 4 - C R E A T E - D E T A I L - L I N E
      *
      *=================================================================
      *    IN  : UP3234-DETAIL-LINE
      *          UP3234-LN-FEED
      *          UP3234-REPORT-KEY
      *
      *    OUT : UP3234-DETAIL-LINE
      *          UP3234-LN-FEED
      *          UP3234-REPORT-KEY
      *
      *    DESC: WRITE RECORD TO REPORT-FILE USING DETAIL-LINE.
      ******************************************************************
       UP3234-CREATE-DETAIL-LINE SECTION.

            IF ( UP3234-REPORT-KEY = 999999999 )
               GO TO UP3234-CREATE-DETAIL-EXIT.

      *- - - - - - - - - - - - - - - - - - - -{ CREATE 'REPORT' RECORD }
            ADD  01                        TO UP3234-REPORT-KEY.
            MOVE UP3234-DETAIL-LINE        TO REPORT-DETAIL-LINE.
            MOVE UP3234-LN-FEED            TO REPORT-LN-FEED.

            PERFORM WRITE-REPORT-FILE.

            IF ( IO-BAD )
               GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ RESET FIELDS }
            MOVE SPACES                    TO UP3234-DETAIL-LINE.
            MOVE 01                        TO UP3234-LN-FEED.

      *=================================================================
       UP3234-CREATE-DETAIL-EXIT.
            EXIT.

      ******************************************************************
      *
      *    U P 3 2 3 4 - C R E A T E - B R A N C H - H E A D
      *
      *=================================================================
      *    IN  : BR-REC
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE HEADER LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP3234-CREATE-BRANCH-HEAD SECTION.

           MOVE "BRANCH: "                 TO UP3234-D1-BRANCH-PROMPT.
           MOVE BR-NO                      TO UP3234-D1-BRANCH.
           MOVE BR-NAME                    TO UP3234-D1-BRANCH-NAME.
           MOVE 02                         TO UP3234-LN-FEED.
           PERFORM UP3234-CREATE-DETAIL-LINE.

      *=================================================================
       UP3234-CREATE-B-HEAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 2 3 4 - C R E A T E - M A C H I N E - H E A D
      *
      *=================================================================
      *    IN  : UP3234-MACHINE
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE MACHINE LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP3234-CREATE-MACHINE-HEAD SECTION.

           MOVE "MACHINE: "                TO UP3234-D2-MACHINE-PROMPT.
      *KC  MOVE UP3234-MACHINE             TO UP3234-D2-MACHINE.
           MOVE CURRENT-MACHINE            TO UP3234-D2-MACHINE.
           MOVE "RISC MACHINE"             TO UP3234-D2-MACHINE-NAME.
           MOVE 02                         TO UP3234-LN-FEED.
           PERFORM UP3234-CREATE-DETAIL-LINE.

      *=================================================================
       UP3234-CREATE-M-HEAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 2 3 4 - C R E A T E - F I L E - E R R O R
      *
      *=================================================================
      *    IN  : UP3234-CURRECT-FILE
      *          UP3234-FILE-ORDER    [SOURCE/TARGET]
      *          STAT
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE ERROR LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP3234-CREATE-FILE-ERROR SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE "FILE: "                   TO UP3234-F1-FILE-PROMPT.
           MOVE UP3234-CURRENT-FILE        TO UP3234-F1-FILE.

           IF ( UP3234-FILE-ORDER-SOURCE )
              STRING "ERROR - SOURCE FILE: ",STAT
                  DELIMITED BY "  "      INTO UP3234-F1-FILE-MESSAGE
           ELSE
           IF ( UP3234-FILE-ORDER-TARGET )
              STRING "ERROR - TARGET FILE: ",STAT
                  DELIMITED BY "  "      INTO UP3234-F1-FILE-MESSAGE
           ELSE
              STRING "ERROR - MISC.. FILE: ",STAT
                  DELIMITED BY "  "      INTO UP3234-F1-FILE-MESSAGE.

           MOVE 01                         TO UP3234-LN-FEED.
           PERFORM UP3234-CREATE-DETAIL-LINE.

      *=================================================================
       UP3234-CREATE-FILE-ER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 2 3 4 - C R E A T E - F I L E - U P D A T E
      *
      *=================================================================
      *    IN  : UP3234-CURRENT-FILE
      *          UP3234-UPDATE-KEY
      *          UP3234-UPDATE-STAT
      *          UP3234-UPDATE-TYPE    [ERROR/SKIP]
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE LINE OF SKIP/ERROR RECORD FOR THE REPORT-FILE.
      ******************************************************************
       UP3234-CREATE-FILE-UPDATE SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE "FILE: "                   TO UP3234-F1-FILE-PROMPT.
           MOVE UP3234-CURRENT-FILE        TO UP3234-F1-FILE.

           IF ( UP3234-UPDATE-TYPE-ERROR )
              MOVE SPACES                  TO UP3234-F1-FILE-MESSAGE
              STRING "ERROR - KEY: ",
                     UP3234-UPDATE-KEY, " ["
                     UP3234-UPDATE-STAT, "]"
                     DELIMITED BY "  "   INTO UP3234-F1-FILE-MESSAGE
           ELSE
              MOVE SPACES                  TO UP3234-F1-FILE-MESSAGE
              STRING "SKIP - KEY: ",
                     UP3234-UPDATE-KEY
                     DELIMITED BY "  "   INTO UP3234-F1-FILE-MESSAGE.

           MOVE 01                         TO UP3234-LN-FEED.
           PERFORM UP3234-CREATE-DETAIL-LINE.

      *=================================================================
       UP3234-CREATE-FILE-UP-END.

           MOVE SPACES                     TO UP3234-UPDATE-KEY
                                              UP3234-UPDATE-TYPE
                                              UP3234-UPDATE-STAT.

      *-----------------------------------------------------------------
       UP3234-CREATE-FILE-UP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 2 3 4 - C R E A T E - F I L E - T O T A L S
      *
      *=================================================================
      *    IN  : UP3234-CURRECT-FILE
      *          UP3234-RECORD-COUNTERS
      *          STAT
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE TOTAL LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP3234-CREATE-FILE-TOTALS SECTION.

           MOVE "FILE: "                    TO UP3234-F1-FILE-PROMPT.
           MOVE UP3234-CURRENT-FILE         TO UP3234-F1-FILE.

           MOVE "READ: "                    TO
                UP3234-F1-RECORD-READ-PROMPT.
           MOVE UP3234-RECORD-COUNTER-READ  TO UP3234-F1-RECORD-READ.

           MOVE "WRITE: "                   TO
                UP3234-F1-RECORD-WRITE-PROMPT.
           MOVE UP3234-RECORD-COUNTER-WRITE TO UP3234-F1-RECORD-WRITE.

           MOVE "SKIP: "                    TO
                UP3234-F1-RECORD-SKIP-PROMPT.
           MOVE UP3234-RECORD-COUNTER-SKIP  TO UP3234-F1-RECORD-SKIP.

           MOVE 01                          TO UP3234-LN-FEED.
           PERFORM UP3234-CREATE-DETAIL-LINE.

      *=================================================================
       UP3234-CREATE-FILE-T-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 2 3 4 - L O A D - F I L E - P A T H
      *
      *=================================================================
      *    IN  : BR-REC
      *          UP3234-FILE-INPUT  [EXP.  XX/XXXXXX]
      *          UP3234-FILE-ORDER  [SOURCE/TARGET]
      *          UP3234-FILE-TYPE   [GLOBAL/LOCAL]
      *          UP3234-SOURCE-PATH
      *          UP3234-TARGET-PATH
      *
      *    OUT : UP3234-FILE-PATH
      *          - GLOBAL: /XXX/XXXX/R#/XX/XXXXXX
      *          - LOCAL : /XXX/XXXX/R#/XX####/XX/XXXXXX
      *
      *    DESC: LOAD FILE PATH BASED ON SOURCE/TARGET & GLOBAL/LOCAL.
      ******************************************************************
       UP3234-LOAD-FILE-PATH SECTION.

           MOVE SPACES                     TO UP3234-FILE-PATH.

           IF ( UP3234-FILE-INPUT           = SPACES ) OR
              ( UP3234-FILE-INPUT-DIR       = SPACES ) OR
              ( UP3234-FILE-INPUT-SLASH NOT = "/"    ) OR
              ( UP3234-FILE-INPUT-NAME      = SPACES )
              GO TO UP3234-LOAD-FILE-PATH-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP3234-FILE-ORDER-SOURCE ) AND
              ( UP3234-FILE-TYPE-GLOBAL  )
      *KC     MOVE UP3234-SOURCE-PATH  TO UP3234-FILE-PATH
              MOVE SOURCE-PATH         TO UP3234-FILE-PATH
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-3
      *KC     MOVE UP3234-MACHINE      TO UP3234-FILE-PATH-MACHINE
              MOVE CURRENT-MACHINE     TO UP3234-FILE-PATH-MACHINE
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-4
              MOVE UP3234-FILE-INPUT   TO UP3234-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP3234-FILE-ORDER-SOURCE ) AND
              ( UP3234-FILE-TYPE-LOCAL   )
      *KC     MOVE UP3234-SOURCE-PATH  TO UP3234-FILE-PATH
              MOVE SOURCE-PATH         TO UP3234-FILE-PATH
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-3
      *KC     MOVE UP3234-MACHINE      TO UP3234-FILE-PATH-MACHINE
              MOVE CURRENT-MACHINE     TO UP3234-FILE-PATH-MACHINE
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO UP3234-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO UP3234-FILE-PATH-LOCAL-DATA-SL
              MOVE UP3234-FILE-INPUT   TO UP3234-FILE-PATH-LOCAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP3234-FILE-ORDER-TARGET ) AND
              ( UP3234-FILE-TYPE-GLOBAL  )
      *KC     MOVE UP3234-TARGET-PATH  TO UP3234-FILE-PATH
              MOVE TARGET-PATH         TO UP3234-FILE-PATH
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-3
      *KC     MOVE UP3234-MACHINE      TO UP3234-FILE-PATH-MACHINE
              MOVE CURRENT-MACHINE     TO UP3234-FILE-PATH-MACHINE
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-4
              MOVE UP3234-FILE-INPUT   TO UP3234-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP3234-FILE-ORDER-TARGET ) AND
              ( UP3234-FILE-TYPE-LOCAL   )
      *KC     MOVE UP3234-TARGET-PATH  TO UP3234-FILE-PATH
              MOVE TARGET-PATH         TO UP3234-FILE-PATH
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-3
      *KC     MOVE UP3234-MACHINE      TO UP3234-FILE-PATH-MACHINE
              MOVE CURRENT-MACHINE     TO UP3234-FILE-PATH-MACHINE
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO UP3234-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO UP3234-FILE-PATH-LOCAL-DATA-SL
              MOVE UP3234-FILE-INPUT   TO UP3234-FILE-PATH-LOCAL-PATH.

      *=================================================================
       UP3234-LOAD-FILE-PATH-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 2 3 4 - V P I - D I S P L A Y 
      *
      *=================================================================
      *   IN:  UP3234-VPI-BRNO
      *        UP3234-VPI-FILE-PREFIX
      *        UP3234-VPI-FILE-TYPE
      *        UP3234-VPI-PRU-LP
      *        UP3234-VPI-ROUTE-BUF
      *        UP3234-VPI-SLEEP-COUNTER
      *        UP3234-VPI-SLEEP-COUNTER-MAX
      *        UP3234-VPI-TICKER
      *        UP3234-VPI-TIME-EDIT
      *
      *  OUT:  UP3234-VPI-SLEEP-COUNTER
      *        UP3234-VPI-TICKER
      *
      * DESC:  UP3234 VIDEO PROGRESS INDICATOR (VPI)
      *        DISPLAYS TO THE SCREEN THE BRANCH NUMBER & FILE THE
      *        PROGRAM IS CURRENTLY CONVERTING.
      *
      ******************************************************************
       UP3234-VPI-DISPLAY SECTION.

           IF ( UP3234-VPI-SLEEP-COUNTER 
              < UP3234-VPI-SLEEP-COUNTER-MAX )
              ADD 1 TO UP3234-VPI-SLEEP-COUNTER
              GO TO UP3234-VPI-DISPLAY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BEGIN TO CREATE UP3234-VPI-DISPLAY-BUF
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES                     TO UP3234-VPI-SLEEP-COUNTER.
           MOVE SPACES                     TO UP3234-VPI-DISPLAY-BUF.

           MOVE "("                        TO UP3234-VPI-DISPLAY-LEFT.
           MOVE ")"                        TO UP3234-VPI-DISPLAY-RIGHT.

           IF ( UP3234-VPI-TICKER = "-" )
              MOVE "\"                     TO UP3234-VPI-DISPLAY-TICKER
                                              UP3234-VPI-TICKER
           ELSE
           IF ( UP3234-VPI-TICKER = "\" )
              MOVE "|"                     TO UP3234-VPI-DISPLAY-TICKER
                                              UP3234-VPI-TICKER
           ELSE
           IF ( UP3234-VPI-TICKER = "|" )
              MOVE "/"                     TO UP3234-VPI-DISPLAY-TICKER
                                              UP3234-VPI-TICKER
           ELSE
              MOVE "-"                     TO UP3234-VPI-DISPLAY-TICKER
                                              UP3234-VPI-TICKER.

           IF ( UP3234-VPI-FILE-TYPE-LOCAL )
              MOVE UP3234-VPI-BRNO         TO UP3234-VPI-DISPLAY-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET THE FILE NAME IN THE DISPLAY
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "***FILE NOT FOUND***"     TO UP3234-VPI-DISPLAY-FILE

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ GLOBAL }
           IF ( UP3234-VPI-FILE-EO )
              MOVE "BRANCH REPORT PROCESSING (EO)"
                TO UP3234-VPI-DISPLAY-FILE
           ELSE
           IF ( UP3234-VPI-FILE-BY )
              MOVE "FORM DESCRIPTOR (FD)"
                TO UP3234-VPI-DISPLAY-FILE
           ELSE
           IF ( UP3234-VPI-FILE-FDB )
              MOVE "FORM DATABASE (FDB)"
                TO UP3234-VPI-DISPLAY-FILE
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ MISC }
           IF ( UP3234-VPI-FILE-WK1 )
              MOVE "WORK FILE 1 (WK1)"
                TO UP3234-VPI-DISPLAY-FILE
           ELSE
           IF ( UP3234-VPI-FILE-WK2 )
              MOVE "WORK FILE 2 (WK2)"
                TO UP3234-VPI-DISPLAY-FILE
           ELSE
           IF ( UP3234-VPI-FILE-REPORT )
              MOVE "UP3234 REPORT (REPORT)"
                TO UP3234-VPI-DISPLAY-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE UP3234-VPI-TIME-EDIT       TO UP3234-VPI-DISPLAY-TIME.
           CALL "C$JUSTIFY" USING UP3234-VPI-DISPLAY-TEXT "L".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SENDING THE 'PROGRESS INDICTATOR' LINE DISPLAY TO THE SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP3234-VPI-ROUTE-MENU-DRIVEN     ) AND
              ( UP3234-VPI-QUEUE-PRINTER-OR-VIEW )
              MOVE UP3234-VPI-DISPLAY-BUF  TO WR-BUF
              MOVE UP3234-VPI-DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      *=================================================================
       UP3234-VPI-DISPLAY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 2 3 4 - V P I - S T A R T
      *
      *=================================================================
      *   IN:  UP3234-CURRENT-FILE
      *        UP3234-FILE-TYPE
      *        UP3234-PRU-LP
      *
      *  OUT:  UP3234-VPI WORKERS
      *
      * DESC:  SETTING THE VIDEO PROGRESS INDICATOR (VPI) WORKERS FOR
      *        THE START OF FILE UPGRADE.
      *
      ******************************************************************
       UP3234-VPI-START SECTION.

           IF ( UP3234-FILE-TYPE-LOCAL )
              MOVE BR-NO                   TO UP3234-VPI-BRNO
           ELSE
              MOVE ZEROES                  TO UP3234-VPI-BRNO.

           MOVE UP3234-CURRENT-FILE        TO UP3234-VPI-FILE-PREFIX.
           MOVE UP3234-FILE-TYPE           TO UP3234-VPI-FILE-TYPE.
      *KC  MOVE UP3234-PRU-LP              TO UP3234-VPI-PRU-LP.
           MOVE PRU-LP                     TO UP3234-VPI-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO UP3234-VPI-ROUTE-BUF.
           MOVE 9999                       TO UP3234-VPI-SLEEP-COUNTER.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO UP3234-VPI-TIME-EDIT.

      *=================================================================
       UP3234-VPI-START-EXIT.
           EXIT.

