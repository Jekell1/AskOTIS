      * COPYMEMBER: LIBUP/UP1534.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBUP/UP1534.CPY
      *
      * DESCRIPTION   : ROUTINES FOR UPGRADE A15 TO A32
      *
      *                 UP1534-CREATE-DETAIL-LINE
      *                 UP1534-CREATE-BRANCH-HEAD
      *                 UP1534-CREATE-MACHINE-HEAD
      *                 UP1534-CREATE-FILE-ERROR
      *                 UP1534-CREATE-FILE-TOTALS
      *                 UP1534-LOAD-FILE-PATH
      *                 UP1534-VPI-DISPLAY
      *                 UP1534-VPI-START
      *
      *=================================================================
      * REV:
      * KEC 2018-0918 ***NEW*** LOCAL-TO-GLOBAL L2G
      * KEC 2019-1010 ADDED UP1534-MV15-OLD-TO-NEW AND
      *               UP1534-MV15-NEW-TO-OLD SECTIONS.
      * KEC 2019-1014 ADDED DISPLAY OF SOURCE-MACHINE IN
      *               UP1534-CREATE-FILE-ERROR SECTION.
      * KEC 2019-1114 ADDED UP1534-LOAD-TIME-ELAPSED SECTION.
      *               ADDED TIMES TO UP1534-CREATE-FILE-TOTALS SECTION.
      * KEC 2020-0309 CHANGED UP1534-REPORT-KEY FROM 9(9) TO 9(11).
      * KEC 2024-0223 ADDED TRACE LOGIC TO:
      *               + UP1534-CREATE-BRANCH-HEAD
      *               + UP1534-CREATE-FILE-ERROR
      *               + UP1534-CREATE-FILE-UPDATE
      *               + UP1534-CREATE-FILE-TOTALS
      *
      ******************************************************************
      *
      *    U P 1 5 3 4 - C R E A T E - D E T A I L - L I N E
      *
      *=================================================================
      *    IN  : UP1534-DETAIL-LINE
      *          UP1534-LN-FEED
      *          UP1534-REPORT-KEY
      *
      *    OUT : UP1534-DETAIL-LINE
      *          UP1534-LN-FEED
      *          UP1534-REPORT-KEY
      *
      *    DESC: WRITE RECORD TO REPORT-FILE USING DETAIL-LINE.
      ******************************************************************
       UP1534-CREATE-DETAIL-LINE SECTION.

            IF ( UP1534-REPORT-KEY = 99999999999 )
               GO TO UP1534-CREATE-DETAIL-EXIT.

      *- - - - - - - - - - - - - - - - - - - -{ CREATE 'REPORT' RECORD }
            ADD  01                        TO UP1534-REPORT-KEY.
            MOVE UP1534-DETAIL-LINE        TO REPORT-DETAIL-LINE.
            MOVE UP1534-LN-FEED            TO REPORT-LN-FEED.

            PERFORM WRITE-REPORT-FILE.

            IF ( IO-BAD )
               GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ RESET FIELDS }
            MOVE SPACES                    TO UP1534-DETAIL-LINE.
            MOVE 01                        TO UP1534-LN-FEED.

      *=================================================================
       UP1534-CREATE-DETAIL-EXIT.
            EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - C R E A T E - B R A N C H - H E A D
      *
      *=================================================================
      *    IN  : BR-REC
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE HEADER LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP1534-CREATE-BRANCH-HEAD SECTION.

           MOVE "BRANCH: "                 TO UP1534-D1-BRANCH-PROMPT.
           MOVE BR-NO                      TO UP1534-D1-BRANCH.
           MOVE BR-NAME                    TO UP1534-D1-BRANCH-NAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE UP1534-DETAIL-LINE         TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 02                         TO UP1534-LN-FEED.
           PERFORM UP1534-CREATE-DETAIL-LINE.

      *=================================================================
       UP1534-CREATE-B-HEAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - C R E A T E - M A C H I N E - H E A D
      *
      *=================================================================
      *    IN  : UP1534-MACHINE
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE MACHINE LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP1534-CREATE-MACHINE-HEAD SECTION.

           MOVE "MACHINE: "                TO UP1534-D2-MACHINE-PROMPT.
      *KC  MOVE UP1534-MACHINE             TO UP1534-D2-MACHINE.
           MOVE CURRENT-MACHINE            TO UP1534-D2-MACHINE.
           MOVE "RISC MACHINE"             TO UP1534-D2-MACHINE-NAME.
           MOVE 02                         TO UP1534-LN-FEED.
           PERFORM UP1534-CREATE-DETAIL-LINE.

      *=================================================================
       UP1534-CREATE-M-HEAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - C R E A T E - F I L E - E R R O R
      *
      *=================================================================
      *    IN  : UP1534-CURRECT-FILE
      *          UP1534-FILE-ORDER    [SOURCE/TARGET]
      *          SOURCE-MACHINE       [R1/R2]
      *          STAT
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE ERROR LINE FOR THE REPORT-FILE.
      *          ALSO SEND ERROR TO THE TRACE LOG.
      ******************************************************************
       UP1534-CREATE-FILE-ERROR SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE "FILE: "                   TO UP1534-F1-FILE-PROMPT.
           MOVE UP1534-CURRENT-FILE        TO UP1534-F1-FILE.

           IF ( UP1534-FILE-ORDER-SOURCE )
              STRING "ERROR - SOURCE FILE: ",STAT, " - ", SOURCE-MACHINE
                  DELIMITED BY "  "      INTO UP1534-F1-FILE-MESSAGE
           ELSE
           IF ( UP1534-FILE-ORDER-TARGET )
              STRING "ERROR - TARGET FILE: ",STAT, " - ", SOURCE-MACHINE
                  DELIMITED BY "  "      INTO UP1534-F1-FILE-MESSAGE
           ELSE
              STRING "ERROR - MISC.. FILE: ",STAT, " - ", SOURCE-MACHINE
                  DELIMITED BY "  "      INTO UP1534-F1-FILE-MESSAGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE UP1534-F1-FILE-MESSAGE TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01                         TO UP1534-LN-FEED.
           PERFORM UP1534-CREATE-DETAIL-LINE.

      *=================================================================
       UP1534-CREATE-FILE-ER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - C R E A T E - F I L E - U P D A T E
      *
      *=================================================================
      *    IN  : UP1534-CURRENT-FILE
      *          UP1534-UPDATE-KEY
      *          UP1534-UPDATE-STAT
      *          UP1534-UPDATE-TYPE    [ERROR/SKIP]
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE LINE OF SKIP/ERROR RECORD FOR THE REPORT-FILE.
      *          ALSO SEND ERROR TO THE TRACE LOG.
      ******************************************************************
       UP1534-CREATE-FILE-UPDATE SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE "FILE: "                   TO UP1534-F1-FILE-PROMPT.
           MOVE UP1534-CURRENT-FILE        TO UP1534-F1-FILE.

           IF ( UP1534-UPDATE-TYPE-ERROR )
              MOVE SPACES                  TO UP1534-F1-FILE-MESSAGE
              STRING "ERROR - KEY: ",
                     UP1534-UPDATE-KEY, " ["
                     UP1534-UPDATE-STAT, "]"
                     DELIMITED BY "  "   INTO UP1534-F1-FILE-MESSAGE
           ELSE
              MOVE SPACES                  TO UP1534-F1-FILE-MESSAGE
              STRING "SKIP - KEY: ",
                     UP1534-UPDATE-KEY
                     DELIMITED BY "  "   INTO UP1534-F1-FILE-MESSAGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE UP1534-F1-FILE-MESSAGE TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01                         TO UP1534-LN-FEED.
           PERFORM UP1534-CREATE-DETAIL-LINE.

      *=================================================================
       UP1534-CREATE-FILE-UP-END.

           MOVE SPACES                     TO UP1534-UPDATE-KEY
                                              UP1534-UPDATE-TYPE
                                              UP1534-UPDATE-STAT.

      *-----------------------------------------------------------------
       UP1534-CREATE-FILE-UP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - C R E A T E - F I L E - T O T A L S
      *
      *=================================================================
      *    IN  : UP1534-CURRECT-FILE
      *          UP1534-RECORD-COUNTERS
      *          STAT
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE TOTAL LINE FOR THE REPORT-FILE.
      *          ALSO UPDATING THE TRACE LOG
      ******************************************************************
       UP1534-CREATE-FILE-TOTALS SECTION.

           MOVE "FILE: "                    TO UP1534-F1-FILE-PROMPT.
           MOVE UP1534-CURRENT-FILE         TO UP1534-F1-FILE.

           MOVE "READ: "                    TO
                UP1534-F1-RECORD-READ-PROMPT.
           MOVE UP1534-RECORD-COUNTER-READ  TO UP1534-F1-RECORD-READ.

           MOVE "WRITE: "                   TO
                UP1534-F1-RECORD-WRITE-PROMPT.
           MOVE UP1534-RECORD-COUNTER-WRITE TO UP1534-F1-RECORD-WRITE.

           MOVE "SKIP: "                    TO
                UP1534-F1-RECORD-SKIP-PROMPT.
           MOVE UP1534-RECORD-COUNTER-SKIP  TO UP1534-F1-RECORD-SKIP.

           MOVE "BEG: "                  TO UP1534-F1-BEG-PROMPT.
           MOVE UP1534-FILE-BEG-TIME     TO UP1534-F1-BEG-TIME.
           INSPECT UP1534-F1-BEG-TIME REPLACING ALL "/" BY ":".

           MOVE "END: "                  TO UP1534-F1-END-PROMPT.
           MOVE "Y"                      TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE SPACES                   TO TIME-MILITARY-FG.
           MOVE HH-MM-SS                 TO UP1534-F1-END-TIME.
           INSPECT UP1534-F1-END-TIME REPLACING ALL "/" BY ":".

           MOVE "TOTAL: "                TO UP1534-F1-TOT-PROMPT.
           MOVE UP1534-FILE-BEG-TIME     TO UP1534-TIME-BEG.
           MOVE HH-MM-SS                 TO UP1534-TIME-END.
           PERFORM UP1534-LOAD-TIME-ELAPSED.
           MOVE UP1534-TIME-ELAPSED      TO UP1534-F1-TOT-TIME.
           INSPECT UP1534-F1-TOT-TIME REPLACING ALL "/" BY ":".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    USE POPULATED UP1534-DETAIL-LINE TO UPDATE THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           STRING "     R: ", UP1534-F1-RECORD-READ
                      " W: ", UP1534-F1-RECORD-WRITE
                      " S: ", UP1534-F1-RECORD-SKIP
                  DELIMITED BY SIZE INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           MOVE UP1534-F1-TIMES TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01                       TO UP1534-LN-FEED.
           PERFORM UP1534-CREATE-DETAIL-LINE.

      *=================================================================
       UP1534-CREATE-FILE-T-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - L O A D - F I L E - B E G - T I M E
      *
      *=================================================================
      *    IN  : NOTHING
      *    OUT : UP1534-FILE-BEG-TIME
      *    DESC: LOAD THE BEGINNING TIME OF THE UPGRADE OF FILE
      ******************************************************************
       UP1534-LOAD-FILE-BEG-TIME SECTION.

           MOVE "Y"                        TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE SPACES                     TO TIME-MILITARY-FG.
           MOVE HH-MM-SS                   TO UP1534-FILE-BEG-TIME.

      *=================================================================
       UP1534-LOAD-FILE-B-T-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - L O A D - F I L E - P A T H
      *
      *=================================================================
      *    IN  : BR-REC
      *          UP1534-FILE-INPUT  [EXP.  XX/XXXXXX]
      *          UP1534-FILE-ORDER  [SOURCE/TARGET]
      *          UP1534-FILE-TYPE   [GLOBAL/LOCAL]
      *          UP1534-SOURCE-PATH
      *          UP1534-TARGET-PATH
      *
      *    OUT : UP1534-FILE-PATH
      *          - GLOBAL: /XXX/XXXX/R#/XX/XXXXXX
      *          - LOCAL : /XXX/XXXX/R#/XX####/XX/XXXXXX
      *
      *    DESC: LOAD FILE PATH BASED ON SOURCE/TARGET & GLOBAL/LOCAL.
      ******************************************************************
       UP1534-LOAD-FILE-PATH SECTION.

           MOVE SPACES                     TO UP1534-FILE-PATH.

           IF ( UP1534-FILE-INPUT           = SPACES ) OR
              ( UP1534-FILE-INPUT-DIR       = SPACES ) OR
              ( UP1534-FILE-INPUT-SLASH NOT = "/"    ) OR
              ( UP1534-FILE-INPUT-NAME      = SPACES )
              GO TO UP1534-LOAD-FILE-PATH-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-FILE-ORDER-SOURCE ) AND
              ( UP1534-FILE-TYPE-GLOBAL  )
              MOVE SOURCE-PATH         TO UP1534-FILE-PATH
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-3
              MOVE SOURCE-MACHINE      TO UP1534-FILE-PATH-MACHINE
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-4
              MOVE UP1534-FILE-INPUT   TO UP1534-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-FILE-ORDER-SOURCE ) AND
              ( UP1534-FILE-TYPE-LOCAL   )
              MOVE SOURCE-PATH         TO UP1534-FILE-PATH
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-3
              MOVE SOURCE-MACHINE      TO UP1534-FILE-PATH-MACHINE
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO UP1534-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO UP1534-FILE-PATH-LOCAL-DATA-SL
              MOVE UP1534-FILE-INPUT   TO UP1534-FILE-PATH-LOCAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-FILE-ORDER-TARGET ) AND
              ( UP1534-FILE-TYPE-GLOBAL  )
            MOVE TARGET-PATH         TO UP1534-FILE-PATH
            MOVE "/"                 TO UP1534-FILE-PATH-SLASH-3
            MOVE TARGET-MACHINE      TO UP1534-FILE-PATH-MACHINE
            MOVE "/"                 TO UP1534-FILE-PATH-SLASH-4
            MOVE UP1534-FILE-INPUT   TO UP1534-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-FILE-ORDER-TARGET ) AND
              ( UP1534-FILE-TYPE-LOCAL   )
              MOVE TARGET-PATH         TO UP1534-FILE-PATH
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-3
              MOVE TARGET-MACHINE      TO UP1534-FILE-PATH-MACHINE
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO UP1534-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO UP1534-FILE-PATH-LOCAL-DATA-SL
              MOVE UP1534-FILE-INPUT   TO UP1534-FILE-PATH-LOCAL-PATH.

       UP1534-LOAD-FILE-PATH-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - L O A D - T I M E - E L A P S E D
      *
      *=================================================================
      *    IN  : UP1534-TIME-BEG
      *          UP1534-TIME-END
      *
      *    OUT : UP1534-TIME-ELAPSED
      *
      *    DESC: USED TO GET THE ELAPSED TIME BETWEEN BEGIN AND END
      *
      *    NOTE: IF THE TIME CROSSED OVER MIDNIGHT, NEED TO ADD 24 HOURS
      *          TO THE END TIME SO TO CALCULATE THE ELAPSED TIME.
      *
      *=================================================================
      *    CALCULATION IS BASED ON CONVERTING THE TIME INTO SECONDS:
      *      1 HOUR   = 3600 SECONDS
      *      1 MINUTE = 60 SECONDS
      *      1 SECOND = 1 SECOND
      *
      *      02:34:56  = 9296
      *      02 * 3600 = 7200
      *      34 * 60   = 2040
      *      56 * 1    = 56
      *
      *      9296 = 02:34:56
      *      02 = 9296 / 3600
      *      34 = (9296 - (3600 * (9296/3600))) / 60
      *      56 = (9296 - (3600 * (9296/3600)) -
      *           ( 60 * ((9296 - (3600 * (9296/3600)))
      *
      *      9296 = 02:34:56
      *      02 = 9296 / 3600
      *      34 = (9296 - (3600 * [2]) / 60
      *      56 = (9296 - (3600 * [2]) - (60 * [34])
      *            9296 - (  7200    ) - (  2040   )
      *
      ******************************************************************
       UP1534-LOAD-TIME-ELAPSED SECTION.

           IF ( UP1534-TIME-BEG-HH > UP1534-TIME-END-HH )
              ADD 24 TO UP1534-TIME-END-HH.

           COMPUTE UP1534-TIME-SECONDS = (( UP1534-TIME-END-HH * 3600 )
                                       +  ( UP1534-TIME-END-MM * 60   )
                                       +    UP1534-TIME-END-SS         )
                                       - (( UP1534-TIME-BEG-HH * 3600 )
                                       +  ( UP1534-TIME-BEG-MM * 60   )
                                       +    UP1534-TIME-BEG-SS        ).

           COMPUTE UP1534-TIME-ELAPSED-HH = 
                   UP1534-TIME-SECONDS / 3600.

           COMPUTE UP1534-TIME-ELAPSED-MM =
                   ( UP1534-TIME-SECONDS - 
                    (3600 * UP1534-TIME-ELAPSED-HH))
                   / 60.

           COMPUTE UP1534-TIME-ELAPSED-SS =
                   UP1534-TIME-SECONDS -
                   (3600 * UP1534-TIME-ELAPSED-HH) -
                   (60 * UP1534-TIME-ELAPSED-MM).

      *=================================================================
       UP1534-LOAD-TIME-ELAP-EXIT.
           EXIT.

      ******************************************************************
      * DUE TO ACU4GL, THE A15 FILE NEEDS TO BE RENAMED (VIA MOVE; MV)
      * SO IT IDENTIFIES THE CORRECT FILE.
      * FOR EXAMPLE, USING THE STFILE, THE A15 FILE OF
      * /US1/DA15/R1/LP/STFILE IS SEEN AS THE A34 STFILE DATABASE TABLE.
      * WHEN TRYING TO OPEN THE A15 FILE IT WILL RESULT IN AN ACU ERROR
      * OF "9D,,2".  FOR THE UPGRADE PROGRAMS TO WORK, NEED TO:
      * 
      *  1. MOVE /US1/DA15/R1/LP/STFILE TO /US1/DA15/R1/LP/OSTFILE
      *  2. OPEN, READ-NEXT, CLOSE ON THE OSTFILE.
      *  3. MOVE /US1/DA15/R1/LP/OSTFILE TO /US1/DA15/R1/LP/STFILE
      * 
      * NEED TO MOVE THE FILE BACK TO ITS ORIGINAL NAME IN CASE THE
      * UPGRADE IS BEING EXECUTED MULTIPLE TIMES (QA TESTING) OR FOR
      * SIDE-BY-SIDE TESTING BETWEEN A15 & A34.
      *
      *=================================================================
      *  BASESH - /US1/DA15/SH/YYFILE
      *  GLOBAL - /US1/DA15/R1/XX/YYFILE
      *  LOCAL  - /US1/DA15/R1/SS####/XX/YYFILE
      *           ....|....1....|....2....|....3
      *
      ******************************************************************
      *
      *    U P 1 5 3 4 - M V 1 5 - O L D - T O - N E W
      *
      *=================================================================
      * IN  : UP1534-MV15-OLD-PATH
      *       UP1534-FILE-TYPE  [B, G, L]
      * OUT : UP1534-MV15-NEW-PATH
      * DESC: EXECUTE SYSTEM MOVE COMMAND TO MOVE OLD FILE TO NEW FILE.
      ******************************************************************
       UP1534-MV15-OLD-TO-NEW SECTION.

           MOVE UP1534-MV15-OLD-PATH TO UP1534-MV15-NEW-PATH.

           IF ( UP1534-FILE-TYPE-BASESH )
              MOVE "O" TO UP1534-MV15-NEW-BASESH-O 
              MOVE UP1534-MV15-OLD-BASESH-FILE TO
                   UP1534-MV15-NEW-BASESH-FILE
           ELSE
           IF ( UP1534-FILE-TYPE-GLOBAL )
              MOVE "O" TO UP1534-MV15-NEW-GLOBAL-O 
              MOVE UP1534-MV15-OLD-GLOBAL-FILE TO
                   UP1534-MV15-NEW-GLOBAL-FILE
           ELSE
              MOVE "O" TO UP1534-MV15-NEW-LOCAL-O 
              MOVE UP1534-MV15-OLD-LOCAL-FILE TO
                   UP1534-MV15-NEW-LOCAL-FILE.

           MOVE MV-CMD               TO SYSTEM-BUF.
           MOVE UP1534-MV15-OLD-PATH TO MV-FILE1.
           MOVE UP1534-MV15-NEW-PATH TO MV-FILE2.

           PERFORM SYSTEM-CALL.

      *=================================================================
       UP1534-MV15-OLD-TO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - M V 1 5 - N E W - T O - O L D
      *
      *=================================================================
      * IN  : UP1534-MV15-NEW-PATH
      *       UP1534-FILE-TYPE  [B, G, L]
      * OUT : UP1534-MV15-OLD-PATH
      * DESC: EXECUTE SYSTEM MOVE COMMAND TO MOVE NEW FILE TO OLD FILE.
      ******************************************************************
       UP1534-MV15-NEW-TO-OLD SECTION.

           MOVE UP1534-MV15-NEW-PATH TO UP1534-MV15-OLD-PATH.

           IF ( UP1534-FILE-TYPE-BASESH )
              MOVE UP1534-MV15-NEW-BASESH-FILE TO
                   UP1534-MV15-OLD-BASESH-FILE
           ELSE
           IF ( UP1534-FILE-TYPE-GLOBAL )
              MOVE UP1534-MV15-NEW-GLOBAL-FILE TO
                   UP1534-MV15-OLD-GLOBAL-FILE
           ELSE
              MOVE UP1534-MV15-NEW-LOCAL-FILE TO
                   UP1534-MV15-OLD-LOCAL-FILE.

           MOVE MV-CMD               TO SYSTEM-BUF.
           MOVE UP1534-MV15-NEW-PATH TO MV-FILE1.
           MOVE UP1534-MV15-OLD-PATH TO MV-FILE2.

           PERFORM SYSTEM-CALL.

      *=================================================================
       UP1534-MV15-NEW-TO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - V P I - D I S P L A Y 
      *
      *=================================================================
      *   IN:  UP1534-VPI-BRNO
      *        UP1534-VPI-FILE-PREFIX
      *        UP1534-VPI-FILE-TYPE
      *        UP1534-VPI-PRU-LP
      *        UP1534-VPI-ROUTE-BUF
      *        UP1534-VPI-SLEEP-COUNTER
      *        UP1534-VPI-SLEEP-COUNTER-MAX
      *        UP1534-VPI-TICKER
      *        UP1534-VPI-TIME-EDIT
      *
      *  OUT:  UP1534-VPI-SLEEP-COUNTER
      *        UP1534-VPI-TICKER
      *
      * DESC:  UP1534 VIDEO PROGRESS INDICATOR (VPI)
      *        DISPLAYS TO THE SCREEN THE BRANCH NUMBER & FILE THE
      *        PROGRAM IS CURRENTLY CONVERTING.
      *
      ******************************************************************
       UP1534-VPI-DISPLAY SECTION.

           IF ( UP1534-VPI-SLEEP-COUNTER 
              < UP1534-VPI-SLEEP-COUNTER-MAX )
              ADD 1 TO UP1534-VPI-SLEEP-COUNTER
              GO TO UP1534-VPI-DISPLAY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BEGIN TO CREATE UP1534-VPI-DISPLAY-BUF
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES                     TO UP1534-VPI-SLEEP-COUNTER.
           MOVE SPACES                     TO UP1534-VPI-DISPLAY-BUF.

           MOVE "("                        TO UP1534-VPI-DISPLAY-LEFT.
           MOVE ")"                        TO UP1534-VPI-DISPLAY-RIGHT.

           IF ( UP1534-VPI-TICKER = "-" )
              MOVE "\"                     TO UP1534-VPI-DISPLAY-TICKER
                                              UP1534-VPI-TICKER
           ELSE
           IF ( UP1534-VPI-TICKER = "\" )
              MOVE "|"                     TO UP1534-VPI-DISPLAY-TICKER
                                              UP1534-VPI-TICKER
           ELSE
           IF ( UP1534-VPI-TICKER = "|" )
              MOVE "/"                     TO UP1534-VPI-DISPLAY-TICKER
                                              UP1534-VPI-TICKER
           ELSE
              MOVE "-"                     TO UP1534-VPI-DISPLAY-TICKER
                                              UP1534-VPI-TICKER.

           IF ( UP1534-VPI-FILE-TYPE-LOCAL )
              MOVE UP1534-VPI-BRNO         TO UP1534-VPI-DISPLAY-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET THE FILE NAME IN THE DISPLAY
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "***FILE NOT FOUND***"     TO UP1534-VPI-DISPLAY-FILE

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ GLOBAL }
           IF ( UP1534-VPI-FILE-AL )
              MOVE "G/L ALLOCATION (AL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-BR )
              MOVE "BRANCH MASTER (BR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-BY )
              MOVE "BRANCH STATISTICS (BY)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CD )
              MOVE "CODE DESCRIPTION (CD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CL )
              MOVE "CAN LOAN (CL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-EA )
              MOVE "EXTRACTION (EA)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-EP )
              MOVE "ELECTRONIC PAYMENT (EP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-EW )
              MOVE "ELECTRONIC PAY WORK (EW)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-FD )
              MOVE "FORMS DATABASE (FD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-FS )
              MOVE "FINANCIAL STATEMENT (FS)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GI )
              MOVE "G/L INTERFACE (GI)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GL )
              MOVE "G/L MASTER (GL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GT )
              MOVE "G/L TRANSACTIONS (GT)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GQ )
              MOVE "GLOBAL QUEUE (GQ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-II )
              MOVE "IRS IDENTIFICATION (II)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-IR )
              MOVE "INSURANCE RATE TABLE (IR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-JD )
              MOVE "G/L JOURNAL DETAIL (JD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-JH )
              MOVE "G/L JOURNAL HISTORY (JH)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LC )
              MOVE "LOAN CLASS (LC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-PE )
              MOVE "PASSWORD EXCEPTIONS (PE)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-RB )
              MOVE "REBATE/CANCEL (RB)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-RC )
              MOVE "DAILY STATUS (RC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SE )
              MOVE "USER SECURITY (SE)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SN )
              MOVE "SECURITY NAMES (SN)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SM )
              MOVE "STATE PARAM MEMOS (SM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SP )
              MOVE "STATE PARAMETERS (SP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SS )
              MOVE "CREDIT SCORE STATS (SS)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-ST )
              MOVE "STATE FILE (ST)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-TRWINFO )
              MOVE "TRW INFO FILE (TRWINFO)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-TZ )
              MOVE "TIME ZONE (TZ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-WR )
              MOVE "WORLD RATING SHEET (WR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ LOCAL  }
           IF ( UP1534-VPI-FILE-BM )
              MOVE "BORROWER MEMO (BM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-BW )
              MOVE "BORROWER MASTER (BW)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CA )
              MOVE "CASH DRAWER (CA)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CI )
              MOVE "CUSTOMER INFORMATION (CI)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CJ )
              MOVE "COLLECTOR JOURNAL (CJ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CK )
              MOVE "LOAN CHECK MASTER (CK)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CP )
              MOVE "COLLECTOR POOL (CP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CQ )
              MOVE "COLLECTOR QUEUE (CQ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-DL )
              MOVE "DEALER MASTER (DL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-DM )
              MOVE "DEALER MEMO (DM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-DS )
              MOVE "DEALER STATISTICS (DS)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-DT )
              MOVE "DEALER TRANSACTION (DT)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-ES )
              MOVE "LOAN ESCROW (ES)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GB )
              MOVE "GLOBAL (GB)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-IC )
              MOVE "INSURANCE COMPANY (IC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-IR )
              MOVE "INSURANCE RATE (IR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-IX )
              MOVE "ALTERNATE INDEX (IX)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LE )
              MOVE "LOAN ENDORSER (LE)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LH )
              MOVE "LOAN HISTORY (LH)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LM )
              MOVE "LOAN MEMO (LM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LN )
              MOVE "LOAN MASTER (LN)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LP )
              MOVE "LOAN PAYMENT (LP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LS )
              MOVE "LOAN STATUS TRAILER (LS)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSD )
              MOVE "LOAN STATUS - DEFICIENCY (LSD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSF )
              MOVE "LOAN STATUS - FOLLOW UP (LSF)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSG )
              MOVE "LOAN STATUS - GPS STARTER (LSG)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSJ )
              MOVE "LOAN STATUS - JUDGEMENT (LSJ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSL )
              MOVE "LOAN STATUS - LEGEL (LSL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSM )
              MOVE "LOAN STATUS - MISC (LSM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSP )
              MOVE "LOAN STATUS - PAID THRU (LSP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSR )
              MOVE "LOAN STATUS - RESCHEDULE (LSR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSX )
              MOVE "LOAN STATUS - REPO (LSX)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LT )
              MOVE "LOAN TRAILER (LT)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTA )
              MOVE "LOAN TRAILER - AUTO INS (LTA)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTB )
              MOVE "LOAN TRAILER - BANKRUPTCY (LTB)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTC )
              MOVE "LOAN TRAILER - COLLATERAL (LTC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTD )
              MOVE "LOAN TRAILER - DEBIT CARD (LTD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTF )
              MOVE "LOAN TRAILER - FEE CODE (LTF)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTI )
              MOVE "LOAN TRAILER - INSURANCE (LTI)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTL )
              MOVE "LOAN TRAILER - LIENHOLDER (LTL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTP )
              MOVE "LOAN TRAILER - PYMT SCHEDULE (LTP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTR )
              MOVE "LOAN TRAILER - REVOLVING (LTR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LX )
              MOVE "LOAN PAYMENT ADDON (LX)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-ML )
              MOVE "MAIL LIST (ML)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-MP )
              MOVE "LOAN MEMO PAYMENT (MP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-PA )
              MOVE "PAYEE (PA)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-PC )
              MOVE "LOAN SCRATCH COLLATERAL (PC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-PD )
              MOVE "LOAN SCRATCH PAD (PD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-TY )
              MOVE "TODAY'S TOTAL STATISTICS (TY)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ MISC }
           IF ( UP1534-VPI-FILE-WK1 )
              MOVE "WORK FILE 1 (WK1)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-WK2 )
              MOVE "WORK FILE 2 (WK2)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-REPORT )
              MOVE "UP1534 REPORT (REPORT)"
                TO UP1534-VPI-DISPLAY-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE UP1534-VPI-TIME-EDIT       TO UP1534-VPI-DISPLAY-TIME.
           CALL "C$JUSTIFY" USING UP1534-VPI-DISPLAY-TEXT "L".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SENDING THE 'PROGRESS INDICTATOR' LINE DISPLAY TO THE SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-VPI-ROUTE-MENU-DRIVEN     ) AND
              ( UP1534-VPI-QUEUE-PRINTER-OR-VIEW )
              MOVE UP1534-VPI-DISPLAY-BUF  TO WR-BUF
              MOVE UP1534-VPI-DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      *=================================================================
       UP1534-VPI-DISPLAY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - V P I - S T A R T
      *
      *=================================================================
      *   IN:  UP1534-CURRENT-FILE
      *        UP1534-FILE-TYPE
      *        UP1534-PRU-LP
      *
      *  OUT:  UP1534-VPI WORKERS
      *
      * DESC:  SETTING THE VIDEO PROGRESS INDICATOR (VPI) WORKERS FOR
      *        THE START OF FILE UPGRADE.
      *
      ******************************************************************
       UP1534-VPI-START SECTION.

           IF ( UP1534-FILE-TYPE-LOCAL )
              MOVE BR-NO                   TO UP1534-VPI-BRNO
           ELSE
              MOVE ZEROES                  TO UP1534-VPI-BRNO.

           MOVE UP1534-CURRENT-FILE        TO UP1534-VPI-FILE-PREFIX.
           MOVE UP1534-FILE-TYPE           TO UP1534-VPI-FILE-TYPE.
      *KC  MOVE UP1534-PRU-LP              TO UP1534-VPI-PRU-LP.
           MOVE PRU-LP                     TO UP1534-VPI-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO UP1534-VPI-ROUTE-BUF.
           MOVE 9999                       TO UP1534-VPI-SLEEP-COUNTER.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO UP1534-VPI-TIME-EDIT.

      *=================================================================
       UP1534-VPI-START-EXIT.
           EXIT.

