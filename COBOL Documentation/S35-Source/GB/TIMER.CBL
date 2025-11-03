       IDENTIFICATION DIVISION.
       PROGRAM-ID.   TIMER.
       AUTHOR.       JKC.
       DATE-WRITTEN. 10-23-2020.
      ******************************************************************
      *
      * IDENTIFICATION: GB/TIMER.CBL
      *
      * DESCRIPTION   : TIMER UTILITY - START, FINISH, ELAPSED, EXCEL
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
      * TIME FORMAT OF HH:MM:SS CAN BE CONVERTED TO MS EXCEL TIME
      * FORMAT WHICH IS BASED ON A CALCULATION OF 86,400 SECONDS IN
      * A DAY.  THE CONVERSION CAN BE REVERSED BACK TO HH:MM:SS FORMAT.
      *
      * BY USING A BEGINNING AND ENDING TIMES IN MS EXCEL FORMAT YOU CAN
      * CALCUATE THE ELAPSED TIME.
      *
      *=================================================================
      * REV:
      * JKC 2020-1023 ***NEW***
      * JKC 2020-1103 ADDED CHANGE TO 'TIMER-GET-DAYSEC.' TO ADD 12
      *          HOURS IF AFTER 12 PM; HELPS GET CORRECT ELAPSED TIME
      *          IF BEG TIME IS 'AM' AND END TIME IS 'PM'.
      * SMR 2024-0401 ADDED TIMER-BEG-DATE TO BE USED FOR CALCULATING
      *          THE ELAPSED TIME IF THE END TIME CROSS OVER TO A NEW
      *          DAY.
      *
      * JKC 2024-0501 ADDED USE OF EXT-API-SCREEN-DISABLE AND
      *         EXT-API-MESS FOR SUPPRESS SCREEN INTERACTIONS FOR
      *         USE IN API PROGRAMS WITHOUT SCREENS.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)GB/TIMER.CBL S35 05/01/24-21:27:53 >".

      ******************************************************************
      *
      *   M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ELE                         PIC 9(02) VALUE ZEROES.

       01  TIMER-CALC-ALL              PIC 9(08)     VALUE ZEROES.
       01  TIMER-CALC-PART             PIC 9(02)     VALUE ZEROES.
       01  TIMER-DAYSEC                PIC 99V9(7)   VALUE ZEROES.
       01  TIMER-END-DATE              PIC 9(08)     VALUE ZEROES.
      *01  TIMER-DAYSEC-BEG            PIC 99V9(7)   VALUE ZEROES.
      *01  TIMER-DAYSEC-END            PIC 99V9(7)   VALUE ZEROES.
      *01  TIMER-DAYSEC-ELAPSED        PIC 99V9(7)   VALUE ZEROES.

      *-----------------------------------------------------------------
      * START   : 01:00:00 AM
      * FINISH  : 12:59:59 PM
      * ELAPSTED: 00:11:22:33 (DD:HH:MM:SS)
      * MS EXCEL: 12.3456789
      *-----------------------------------------------------------------
       01  TIMER-DISPLAY-BUF.
           04  TIMER-DISPLAY-MESS      PIC X(35) OCCURS 4 TIMES.

       01  TIMER-MESS.
           03  TIMER-MESS-TEXT         PIC X(10).
           03  TIMER-MESS-TIME         PIC X(11).
           03  TIMER-MESS-EXCEL        REDEFINES TIMER-MESS-TIME
                                       PIC 99.9(7).
           03  TIMER-MESS-DESC         PIC X(14).

      *-----------------------------------------------------------------
      *    TIMER-HHMMSS IS MOSTLY IN A HH:MM:SS FORMAT USED AS
      *    INPUT VARIABLE.  BUT IN SOME CASES IT ALSO COULD BE IN A
      *    DD:HH:MM:SS FORMAT.  IF CALCULATING AN ELAPSED TIME THEN
      *    THE RESULTS COULD COME BACK BEYOND A DAY.  THE PROGRAM
      *    THAT USES THIS LOGIC WILL NEED TO ADJUST FOR THAT BUT IN A
      *    MOVE STATEMENT WILL BE RIGHT JUSTIFIED THUS CUTTING OFF THE
      *    LEFT IF NOT NEEDED.
      *-----------------------------------------------------------------
       01  TIMER-HHMMSS                PIC 9(8)      VALUE ZEROES.
       01  FILLER            REDEFINES TIMER-HHMMSS.
           03  TIMER-HHMMSS-DD         PIC 9(2).     *> DAYS
           03  TIMER-HHMMSS-HH         PIC 9(2).     *> HOURS
           03  TIMER-HHMMSS-MM         PIC 9(2).     *> MINUTES
           03  TIMER-HHMMSS-SS         PIC 9(2).     *> SECONDS

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".

      ******************************************************************
      *
      *    L I N K A G E - S E C T I O N
      *
      ******************************************************************
       LINKAGE SECTION.
       COPY "LIBGB/TIMER_LNK.CPY" REPLACING LEADING "TIMER-" BY
                                                    "P-TIMER-".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING P-TIMER-LINK-BUF.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP P-TIMER-LINK-BUF.

           MOVE P-TIMER-LINK-BUF TO TIMER-LINK-BUF.

           IF ( TIMER-DAYSEC-BEG NOT NUMERIC )
              MOVE ZEROES TO TIMER-DAYSEC-BEG.

           IF NOT ( TIMER-TYPE-VALID )
              MOVE "U" TO TIMER-TYPE.     *> SET TO UTILITY; NO DISPLAY

           IF ( TIMER-DAYSEC-BEG NOT = ZEROES )
              GO TO MAIN-PROGAM-TIMER-STOP.

      *-----------------------------------------------------------------
      *    PERFORM TIMER-START; SET BEG TIME FIELDS
      *-----------------------------------------------------------------
       MAIN-PROGAM-TIMER-START.

           PERFORM GET-TIME.
           MOVE HH-MM-SS                   TO TIMER-HHMMSS.
           PERFORM TIMER-GET-DAYSEC.
           MOVE TIMER-DAYSEC               TO TIMER-DAYSEC-BEG.

           MOVE ZEROES                     TO TIMER-DAYSEC-END.
           MOVE ZEROES                     TO TIMER-DAYSEC-ELAPSED.

           MOVE HH-MM-SS                   TO TIMER-BEG-HHMMSS.
           MOVE T-AMPM                     TO TIMER-BEG-AMPM.
           INSPECT TIMER-BEG-EDIT REPLACING ALL "/" BY ":".

           MOVE SPACES                     TO TIMER-END-EDIT
           MOVE ZEROES                     TO TIMER-ELAPSED-EDIT

           ACCEPT TIMER-BEG-DATE FROM CENTURY-DATE.

           GO TO MAIN-PROGAM-EXIT.

      *=================================================================
      *    PERFORM TIMER-STOP; GET END & ELAPSED; DISPLAY IF SELECTED
      *=================================================================
       MAIN-PROGAM-TIMER-STOP.

           PERFORM GET-TIME.
           MOVE HH-MM-SS                   TO TIMER-HHMMSS.
           PERFORM TIMER-GET-DAYSEC.
           MOVE TIMER-DAYSEC               TO TIMER-DAYSEC-END.

           MOVE HH-MM-SS                   TO TIMER-END-HHMMSS.
           MOVE T-AMPM                     TO TIMER-END-AMPM.
           INSPECT TIMER-END-HHMMSS REPLACING ALL "/" BY ":".

           ACCEPT SYS-DATE FROM CENTURY-DATE.
           MOVE TIMER-BEG-DATE             TO NUM-DATE.
           PERFORM TIMALL.

           PERFORM TIMER-CALC-ELAPSED.
           MOVE TIMER-DAYSEC               TO TIMER-DAYSEC-ELAPSED.
           MOVE TIMER-HHMMSS               TO TIMER-ELAPSED-EDIT.
           INSPECT TIMER-ELAPSED-EDIT REPLACING ALL "/" BY ":".

           IF ( EXT-ROUTE-BUF NOT = "00" ) OR
              ( TIMER-TYPE-UTILITY       )
              GO TO MAIN-PROGAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY RESULTS IN BOX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES                     TO ELE.
           MOVE SPACES                     TO TIMER-DISPLAY-BUF.

           ADD 01 TO ELE.
           MOVE SPACES                     TO TIMER-MESS
           MOVE "START   :"                TO TIMER-MESS-TEXT.
           MOVE TIMER-BEG-EDIT             TO TIMER-MESS-TIME.
           MOVE TIMER-BEG-DATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.
           STRING " | ",DATE-MMSDDSYY INTO TIMER-MESS-DESC.
           MOVE TIMER-MESS                 TO TIMER-DISPLAY-MESS(ELE).

           ADD 01 TO ELE.
           MOVE SPACES                     TO TIMER-MESS
           MOVE "FINISH  :"                TO TIMER-MESS-TEXT.
           MOVE TIMER-END-EDIT             TO TIMER-MESS-TIME.
           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.
           STRING " | ",DATE-MMSDDSYY INTO TIMER-MESS-DESC.
           MOVE TIMER-MESS                 TO TIMER-DISPLAY-MESS(ELE).

           ADD 01 TO ELE.
           MOVE SPACES                     TO TIMER-MESS
           MOVE "ELAPSED :"                TO TIMER-MESS-TEXT.
           MOVE TIMER-ELAPSED-EDIT         TO TIMER-MESS-TIME.
           MOVE TIMER-MESS                 TO TIMER-DISPLAY-MESS(ELE).

           IF ( TIMER-TYPE-EXCEL )
              ADD 01 TO ELE
              MOVE SPACES                  TO TIMER-MESS
              MOVE "MS EXCEL:"             TO TIMER-MESS-TEXT
              MOVE TIMER-DAYSEC-ELAPSED    TO TIMER-MESS-EXCEL
              MOVE " (DD:HH:MM:SS)"        TO TIMER-MESS-DESC
              MOVE TIMER-MESS              TO TIMER-DISPLAY-MESS(ELE).

           IF ( EXT-API-SCREEN-DISABLE = "Y" )
              GO TO MAIN-PROGAM-EXIT.    *> SKIP DISPLAY OF MESSAGE BOX

           IF ( TIMER-TYPE-EXCEL )
              DISPLAY MESSAGE BOX TIMER-DISPLAY-MESS(1), X"0A"
                                  TIMER-DISPLAY-MESS(2), X"0A"
                                  TIMER-DISPLAY-MESS(3), X"0A"
                                  TIMER-DISPLAY-MESS(4), X"0A"
           ELSE
              DISPLAY MESSAGE BOX TIMER-DISPLAY-MESS(1), X"0A"
                                  TIMER-DISPLAY-MESS(2), X"0A"
                                  TIMER-DISPLAY-MESS(3), X"0A".

      *=================================================================
       MAIN-PROGAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *   T I M E R - C A L C - E L A P S E D
      *
      ******************************************************************
      * IN  : TIMER-DAYSEC-BEG [DAYS IN SECONDS; MS EXCEL]
      *       TIMER-DAYSEC-END [DAYS IN SECONDS; MS EXCEL]
      * OUT : TIMER-DAYSEC     [DAYS IN SECONDS; MS EXCEL]
      *       TIMER-HHMMSS     [DD:HH:MM:SS]
      * DESC: CALCUATE THE TIME ELAPSED USING MS EXCEL DATE FORMAT.
      ******************************************************************
       TIMER-CALC-ELAPSED SECTION.

           IF (ELAPSED-DAYS = 0)
               COMPUTE TIMER-DAYSEC =
                      (TIMER-DAYSEC-END - TIMER-DAYSEC-BEG)
           ELSE
               COMPUTE TIMER-DAYSEC =
                      (1 - TIMER-DAYSEC-BEG) +
                       TIMER-DAYSEC-END          +
                      (1 * (ELAPSED-DAYS - 1)).

           PERFORM TIMER-GET-HHMMSS.

      *=================================================================
       TIMER-CALC-ELAPSED-EXIT.
           EXIT.

      ******************************************************************
      *
      *   T I M E R - G E T - D A Y S E C 
      *
      ******************************************************************
      * IN  : TIMER-HHMMSS [HH:MM:SS]
      *       T-AMPM       [AM, PM OR SPACES (MILITARY)]
      * OUT : TIMER-DAYSEC [DAYS IN SECONDS; MS EXCEL]
      * DESC: CONVERT INPUT TIME IN STANDARD FORMAT (HH:MM:SS) TO
      *       MS EXCEL DATE FORMAT. IT NEEDS TO BE CONVERTED TO
      *       SECONDS THEN DIVIDED BY 86400 (TOTAL OF SECOND FOR
      *       24 HOURS).  FOR EXAMPLE, 12:34:56 IS 45296 SECONDS WHICH
      *       DIVIDED BY 86400 SECONDS IS 0.5242592 WHICH WITHIN
      *       MS EXCEL YOU CAN FORMAT THE WHOLE COLUMN TO A
      *       CUSTOM FORMAT OF HH:MM:SS.
      ******************************************************************
       TIMER-GET-DAYSEC SECTION.

           IF ( T-AMPM              = "PM" ) AND
              ( TIMER-HHMMSS-HH NOT = 12   )
              ADD 12 TO TIMER-HHMMSS-HH.

           COMPUTE TIMER-DAYSEC = (( TIMER-HHMMSS-HH * 3600 )
                                +  ( TIMER-HHMMSS-MM * 60   )
                                +    TIMER-HHMMSS-SS         )
                                / 86400.

      *=================================================================
       TIMER-CALC-DAYSEC-EXIT.
           EXIT.

      ******************************************************************
      *
      *   T I M E R - G E T - H H M M S S
      *
      ******************************************************************
      * IN  : TIMER-DAYSEC [DAYS IN SECONDS; MS EXCEL]
      * OUT : TIMER-HHMMSS [HH:MM:SS]
      * DESC: CONVERT INPUT TIME IN MS EXCEL DATE FORMAT TO STANDARD
      *       FORMAT (HH:MM:SS).
      ******************************************************************
       TIMER-GET-HHMMSS SECTION.

           COMPUTE TIMER-CALC-ALL ROUNDED = TIMER-DAYSEC * 86400.

      *- -{ CALCULATE DAYS: 86400 SECONDS PER DAY }- - - - - - - - - - -
           COMPUTE TIMER-CALC-PART = TIMER-CALC-ALL / 86400.
           MOVE TIMER-CALC-PART TO TIMER-HHMMSS-DD.
           COMPUTE TIMER-CALC-ALL  = TIMER-CALC-ALL
                                   - ( TIMER-CALC-PART * 86400 ).

      *- -{ CALCULATE HOURS: 3600 SECONDS PER HOUR }- - - - - - - - - -
           COMPUTE TIMER-CALC-PART = TIMER-CALC-ALL / 3600.
           MOVE TIMER-CALC-PART TO TIMER-HHMMSS-HH.
           COMPUTE TIMER-CALC-ALL  = TIMER-CALC-ALL
                                   - ( TIMER-CALC-PART * 3600 ).

      *- -{ CALCULATE MINUTES: 60 SECONDS PER MINUTE }- - - - - - - - -
           COMPUTE TIMER-CALC-PART = TIMER-CALC-ALL / 60.
           MOVE TIMER-CALC-PART TO TIMER-HHMMSS-MM.
           COMPUTE TIMER-CALC-ALL  = TIMER-CALC-ALL
                                   - ( TIMER-CALC-PART * 60 ).

      *- -{ CALCULATE SECONDS }- - - - - - - - - - - - - - - - - - - - -
           MOVE TIMER-CALC-ALL TO TIMER-HHMMSS-SS.

      *=================================================================
       TIMER-GET-HHMMSS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
      *COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
      *
      *    G E T - T I M E
      *
      *=================================================================
      * NOTE: THIS AN EXACT COPY OF GET-TIME WITHIN LIBGB/TIMEIO.CPY;
      *       NEEDS TO BE HARDCODED SINCE FORM-PATH IS NOT DEFINED
      *       WITHIN THIS PROGRAM.
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

      *=================================================================
       GET-TIME-EXIT.
           MOVE " " TO TIME-MILITARY-FG.

      ******************************************************************
      *
      *    E X I T - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.
           EXIT PROGRAM.
