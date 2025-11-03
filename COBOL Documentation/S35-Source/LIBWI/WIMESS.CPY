      * COPYMEMBER: LIBWI/WIMESS
      ******************************************************************
      * DESC :  WINDOW MESSAGE, DISPLAYS BOX WITH UP TO 9 LINES OF
      *         60 CHARACTERS OF MESSAGE & WAITS FOR USER TO PRESS ENTER
      * USAGE:  MOVE MESSAGES TO WIMESS1, WIMESS2, ETC, UP TO WIMESS9
      * COPY :  LIBWI/WIMESSW
      * REV:
      *
      * JKC 2024-0501 ADDED USE OF EXT-API-SCREEN-DISABLE AND
      *         EXT-API-MESS FOR SUPPRESS SCREEN INTERACTIONS FOR
      *         USE IN API PROGRAMS WITHOUT SCREENS.
      *
      ******************************************************************
      *    IN  :  WIMESS1, WIMESS2, UP TO WIMESS9
      *           WIMESS-LINES (FROM 1 TO 9, DEFAULTS TO 9)
      *           WIMESS-JUSTIFY1 - WIMESS-JUSTIFY9, CAN LOAD WITH
      *              "L" OR "R" TO JUSTIFY, DEFAULT CENTERS MESS LINE
      ******************************************************************
       SEND-WIMESS SECTION.

           PERFORM VARYING WIMESS-SUB FROM 1 BY 1
                     UNTIL WIMESS-SUB > WIMESS-LINES
              IF WIMESS-JUSTIFY-CENTER(WIMESS-SUB)
                 CALL "C$JUSTIFY" USING WIMESS-MSG(WIMESS-SUB) "C"
              END-IF
              IF WIMESS-JUSTIFY-LEFT(WIMESS-SUB)
                 CALL "C$JUSTIFY" USING WIMESS-MSG(WIMESS-SUB) "L"
              END-IF
              IF WIMESS-JUSTIFY-RIGHT(WIMESS-SUB)
                 CALL "C$JUSTIFY" USING WIMESS-MSG(WIMESS-SUB) "R"
              END-IF
           END-PERFORM.

           PERFORM VARYING WIMESS-SUB FROM 1 BY 1
                     UNTIL WIMESS-SUB > WIMESS-LINES
              INSPECT WIMESS-DISPLAY-BUF REPLACING FIRST
              "
      -       "    " BY WIMESS(WIMESS-SUB)
           END-PERFORM.

           IF ( EXT-API-SCREEN-DISABLE NOT = "Y" )
              DISPLAY MESSAGE BOX, WIMESS-DISPLAY-BUF.

       SEND-WIMESS-EXIT.
           EXIT.
