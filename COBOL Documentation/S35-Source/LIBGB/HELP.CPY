      **************************************************************************
      * COPYBOOK: LIBGB/HELP.CPY
      * DESCRIPTION: MAIN HELP LOGIC COPIED INTO GB/HELP AND
      *                   GB/HELP2 (GENERAL HELP)
      *
      * REV:
      * MJD 141026 ADDED LOGIC TO DISPLAY THE PROGRAM NAME (HELP-FILE)
      *            IN THE CENTER OF THE LEGEND WHEN DISPLAYING THE HELP SCREEN.
      * BLM 141119 ADDED SEND-MESS WITH PROGRAM NAME IF HELP FILE NOT FOUND
      *            & MESSAGE WITH FIELD NAME IF NOT FOUND
      * MJD 170228 EXPANDED HELP TABLE TO 1000 LINES.   SOME SPR ENTRIESI
      *            WERE CAUSIONG AN INDEX OUT OF BOUNDS ERROR AS THE HELP
      *            ENTRIES WERE LARGER THEN THE ALLOTED SPACE OF 457 LINES.
      * MJD 170301 CHANGED TEST FOR "/" TO SEARCH ENTIRE LINE  INSTEAD
      *            OF JUST FIORST COLUMN.  ADDED TEST OF HELP-SEQ TO
      *             PREVENT INDEX OUT OF BOUNDS.
      * BAH 2022.0518 CHANGED HELP-LINES-PER-PAGE FROM 99 TO 9(6). 
      *               SPMANC HELP, FIELD 010, WAS ONLY DISPLAY 5 LINES, THE
      *               TILDA LOGIC ISNT REALLY WORKING. LOOKS LIKE ITS READING
      *               THE ENTIRE HELP FOR EACH FIELD.
      **************************************************************************
       77  KEYWORD-STATUS               PIC XX.
       77  NUMBER-READS-NEEDED          PIC 99.

       01  HELP--FILE                   PIC X(10).
       01  HELP--DIR                    PIC X(02).

       01  ERRCD                        PIC X.
       77  D-PROGRAM                    PIC X(6).
      *-------------------------------------------------------------------------
      * HELP WINDOW WORKERS
      *-------------------------------------------------------------------------
       01  HELP-WINDOW-WORKERS.
           03  HELP-POP-UP-WINDOW            PIC X(10).
           03  HELP-WINDOW-COL               PIC 99.
           03  HELP-WINDOW-LINES             PIC 99.
           03  HELP-LINE-WIDTH               PIC 99.
           03  HELP-LINES-PER-PAGE           PIC 9(6).
           03  HELP-HOLD-COLUMNS             PIC 99.
           03  HELP-HOLD-LINES               PIC 99.
           03  HELP-FIELD-INPUT              PIC X(80).
           03  HELP-COND                     PIC X(80).
           03  HELP-COND-FIELD               PIC X(80).
           03  HELP-COND-OPERATOR            PIC X(02).
           03  HELP-COND-VALUE               PIC X(80).
           03  HELP-COND-TEST                PIC X(80).
           03  HELP-FIELD                    PIC X(80).
           03  HELP-COND-FLAG                PIC X(01).
           03  HELP-JUNK                     PIC X(80).
           03  HELP-SEQ                      PIC 9(06).
           78  HELP-SEQ-MAX                  VALUE 1000.
           03  HELP-FIELD-FOUND              PIC X(01).
           03  HELP-FG-COLOR                 PIC 9(02).
           03  HELP-BG-COLOR                 PIC 9(02).
           03  HELP-GENERAL-HELP             PIC X(01) VALUE "N".
           03  HELP-WINDOW-TITLE             PIC X(40).

      *    03  HELP-TABLE                    PIC X(31990).
      *    03  HELP-TABLE-1 REDEFINES HELP-TABLE OCCURS 457
      *                                      PIC X(70).
           03  HELP-TABLE                    PIC X(70000).
           03  HELP-TABLE-1 REDEFINES HELP-TABLE OCCURS HELP-SEQ-MAX
                                             PIC X(70).
           03  HELP-SUB                      PIC 9(02).
           03  HELP-PTR                      PIC 9(06).
           03  HELP-LIST-SIZE                PIC 9(06).
           03  HELP-ELEMENT                  PIC X(14).
           03  HELP-LIST-EMPTY               PIC X(01) VALUE "N".
               88  HELP-NO-MORE-ELEMENTS        VALUE "Y".
       78  HELP-QUOTE                       VALUE X'22'.
       01  HELP-LEGEND-PNAME                PIC X(11) VALUE SPACES.
       01  HELP-LEGEND-PNAME-START          PIC 9(02).
       01  HELP-LEGEND1                     PIC X(76) VALUE
           "ENTER = EXIT        F8 = GENERAL HELP".
       01  HELP-LEG1-WK REDEFINES HELP-LEGEND1.
           03  FILLER                       PIC X(20).
           03  HELP-LEGEND1-RT              PIC X(60).
       01  HELP-RT-LEN                      PIC 9(3).
       01  HELP-LEGEND2                     PIC X(76) VALUE
           "ENTER = EXIT".

       01  CONDITION-POS                    PIC 9(04).
       01  CONDITION-SIZE                   PIC 9(02).
       01  COND-LIST-SIZE                   PIC 9(06).
       01  CONDITION-INPUT-BUF.
           03  CONDITION-INPUT-FIELD        PIC X(15).
           03  CONDITION-INPUT-POS          PIC 9(04).
           03  CONDITION-INPUT-SIZE         PIC 9(02).

       COPY "LIBGB/ACUCOBOL_DEF.CPY".
       COPY "LIBGB/ACUGUI_DEF.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       SCREEN SECTION.
       COPY "LIBGB/HELP_SCN.CPY".

       PROCEDURE DIVISION USING FORM-PATHNAME HELP-LINK-BUF
                 EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON HE-FILE.
       COPY "LIBGB/DECLARE.CPY".
           CLOSE HE-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ****************************************************
       MAIN-LOGIC SECTION.
      ****************************************************
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM SQL-CONNECT.

           MOVE HELP-LINK-FILE  TO HELP--FILE.
           MOVE HELP-LINK-DIR   TO HELP--DIR.
           MOVE HELP-LINK-FIELD TO HELP-FIELD.
           IF HELP-FIELD = "GENERALHELP"
              MOVE "Y" TO HELP-GENERAL-HELP.

      *    CREATE THE "(XX/YYYYYY)" TO DISPLAY IN HELP LEGEND
      *    BASED ON THE HELP DIR AND FILE PASSED INTO THE PROGRAM.
           MOVE SPACES TO HELP-LEGEND-PNAME.
           STRING "(" HELP-LINK-DIR "/" HELP-LINK-FILE ")"
              DELIMITED BY " " INTO HELP-LEGEND-PNAME.

           PERFORM CALL-HELP.
           GO TO END-ROUTINE.


      *****************************************************************
       CALL-HELP SECTION.
      *****************************************************************
           MOVE SPACES TO HELP-TABLE.
      *    MOVE 0 TO EVENT-TYPE.
           ACCEPT HE-REL FROM ENVIRONMENT "REL".
      *    MOVE HELP--DIR  TO HE-DIR.
      *    MOVE "/"        TO HE-SLASH.
      *    MOVE HELP--FILE TO HE-PROG.
           MOVE SPACES TO HE-PROG.
           STRING HELP--FILE ".TXT" DELIMITED BY " " INTO HE-PROG.
           MOVE HE-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "NO HELP FILE FOR &&&&&&&&&&&&&&&&&&&&" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&&&&&&&&&&&&&&&&&"
                                          BY HELP-LINK-FILE
              PERFORM SEND-MESS
              GO TO CALL-HELP-EXIT.
           PERFORM OPEN-HE-FILE.
           PERFORM HELP-FIND-FIELD.
           IF HELP-FIELD-FOUND = "Y"
              PERFORM HELP-DISPLAY-WINDOW.

       CALL-HELP-EXIT.
           EXIT.

      *****************************************************************
       HELP-FIND-FIELD SECTION.
      *****************************************************************

      * READ HELP TEXT FILE UNTIL CORRECT ENTRY FIELD IS FOUND
           MOVE "N" TO HELP-FIELD-FOUND.
       HELP-SCAN-NEXT-LINE.
           MOVE SPACES TO HE-LINE.
           PERFORM READ-HE-FILE.
           IF IO-BAD
              MOVE HELP-LINK-FILE TO D-PROGRAM
              STRING HELP-LINK-DIR, "/",
                     D-PROGRAM, ".TXT",
                     " HAS NO HELP FOR FIELD: "
                     HELP-LINK-FIELD
                 DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO HELP-FIND-FIELD-EXIT.

           MOVE 0 TO HELP-SUB
           INSPECT HE-LINE TALLYING HELP-SUB FOR ALL "\"
           IF HELP-SUB = 0
              GO TO HELP-SCAN-NEXT-LINE.

           MOVE 1 TO HELP-PTR.
           SET HELP-LIST-SIZE TO SIZE OF MESS
           MOVE "N" TO HELP-LIST-EMPTY.
           PERFORM HELP-PROCESS-ELEMENTS UNTIL HELP-NO-MORE-ELEMENTS.

           IF HELP-FIELD-FOUND = "N"
              GO TO HELP-SCAN-NEXT-LINE.

           MOVE 0 TO HELP-LINE-WIDTH HELP-LINES-PER-PAGE.
           MOVE 1 TO HELP-SEQ HELP-LINES-PER-PAGE.

      * READ BODY OF HELP ENTRY UNTIL EOF OR BEG OF NEXT ENTRY

       HELP-NEXT-LINE.
           MOVE SPACES TO HE-LINE.
           PERFORM READ-HE-FILE.
           MOVE 0 TO HELP-SUB.
           INSPECT HE-LINE TALLYING HELP-SUB FOR ALL "\".
      *    IF IO-BAD OR HE-REC(1:1) = "\"
           IF IO-BAD OR HELP-SUB NOT = 0
              GO TO HELP-FIND-FIELD-EXIT.

      *    ---------------------------------------------------------------------
      *    CALCULATE LONGEST LINE TO DETERMINE WINDOW WIDTH
      *    ---------------------------------------------------------------------
           MOVE 0 TO HELP-SUB.
           INSPECT HE-LINE TALLYING HELP-SUB FOR TRAILING " ".
           SUBTRACT HELP-SUB FROM 80 GIVING HELP-HOLD-COLUMNS
           IF HELP-HOLD-COLUMNS > HELP-LINE-WIDTH
              MOVE HELP-HOLD-COLUMNS TO HELP-LINE-WIDTH.

      *    ---------------------------------------------------------------------
      *    CALCULATE THE MOST # OF LINES PER PAGE TO DETERMINE WINDOW HEIGHT
      *    ---------------------------------------------------------------------
           IF HE-REC(1:1) = "~"
              MOVE SPACES TO HE-LINE.

      *    ---------------------------------------------------------------------
      *    WRITE HELP LINE TO TABLE FILE FOR QUICK ACCESS
      *    ---------------------------------------------------------------------
           MOVE HE-REC TO HELP-TABLE-1(HELP-SEQ).

           ADD 1 TO HELP-SEQ HELP-LINES-PER-PAGE.

           IF HELP-SEQ > HELP-SEQ-MAX
              MOVE HELP-LINK-FILE TO D-PROGRAM
              STRING HELP-LINK-DIR, "/",
                     D-PROGRAM, ".TXT",
                     " HELP FOR ", HELP-LINK-FIELD,
                     " EXCEEDS MAXIMUM LINES"
                 DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO HELP-FIND-FIELD-EXIT.

           GO TO HELP-NEXT-LINE.
       HELP-FIND-FIELD-EXIT.
           EXIT.


      **************************************************************************
      * EVALUATE AND TEST CONDITIONAL FIELD
      *
      * INPUT: HELP-COND
      *        EXAMPLES:     (100 EQ 2)   : NUMERIC EQUAL
      *                      (100 NE 2)   : NUMERIC NOT EQUAL
      *                      (100 = '2')  : ALPHANUMERIC EQUAL
      *                      (100 ~ '2')  : ALPHANUMERIC NOT EQUAL
      *
      * OUT  : HELP-COND-FLAG = "Y" : CONDITION PASSED
      *                         "N" : CONDITION FAILED
      **************************************************************************
       HELP-EVALUATE-COND SECTION.
           MOVE "N" TO HELP-COND-FLAG.
           UNSTRING HELP-COND DELIMITED BY " "
                    INTO HELP-COND-FIELD
                         HELP-COND-OPERATOR
                         HELP-COND-VALUE.

           PERFORM COND-GET-INFORMATION.
           IF CONDITION-POS = 0 OR CONDITION-SIZE = 0
              GO TO HELP-EVALUATE-COND-EXIT.

           MOVE SPACES TO HELP-COND-TEST.
           ACCEPT HELP-COND-TEST
                  FROM SCREEN
                  AT CONDITION-POS
                  SIZE CONDITION-SIZE.

           INSPECT HELP-COND-VALUE REPLACING ALL "'" BY " ".
           INSPECT HELP-COND-VALUE REPLACING ALL HELP-QUOTE BY " ".
           CALL "C$JUSTIFY" USING HELP-COND-VALUE "L".
           CALL "C$JUSTIFY" USING HELP-COND-TEST  "L".
           EVALUATE HELP-COND-OPERATOR
              WHEN "EQ" IF HELP-COND-TEST = HELP-COND-VALUE
                           MOVE "Y" TO HELP-COND-FLAG
                        END-IF
              WHEN "NE" IF HELP-COND-TEST NOT = HELP-COND-VALUE
                           MOVE "Y" TO HELP-COND-FLAG
                        END-IF
              WHEN "="  IF HELP-COND-TEST = HELP-COND-VALUE
                           MOVE "Y" TO HELP-COND-FLAG
                        END-IF
              WHEN "~"  IF HELP-COND-TEST NOT = HELP-COND-VALUE
                           MOVE "Y" TO HELP-COND-FLAG
                        END-IF
           END-EVALUATE.
       HELP-EVALUATE-COND-EXIT.
           EXIT.

      ****************************************************
       HELP-DISPLAY-WINDOW SECTION.
      ****************************************************
           ADD 2 TO HELP-LINE-WIDTH GIVING HELP-WINDOW-COL.
           IF HELP-WINDOW-COL < 40
              MOVE 43 TO HELP-WINDOW-COL.
           IF HELP-WINDOW-COL > 78
              MOVE 78 TO HELP-WINDOW-COL.
           IF HELP-LINES-PER-PAGE > 20
               MOVE 20 TO HELP-WINDOW-LINES
           ELSE
              IF HELP-LINES-PER-PAGE < 5
                  MOVE 5 TO HELP-WINDOW-LINES
              ELSE
                  MOVE HELP-LINES-PER-PAGE TO HELP-WINDOW-LINES.

      *  CALCULATE THE STARTING POINT FOR THE PROGRAM NAME WITHIN THE LEGEND.
      *  FIND CENTER (WIDTH / 2) AND SUBTRACT 6 (1/2 FULL LENGTH OF 11). 
      *  THIS SHOULD CENTER IT ON THE LEGEND.

           COMPUTE HELP-LEGEND-PNAME-START =
                            (HELP-WINDOW-COL / 2) - 6
           END-COMPUTE.

      *    -------------------------------------------------------------
      *    OPEN WINDOW OF CORRECT WIDTH
      *    -------------------------------------------------------------
           IF HELP-GENERAL-HELP = "Y"
              MOVE "GENERAL HELP" TO HELP-WINDOW-TITLE
              CALL "C$JUSTIFY" USING HELP-LEGEND2 "L"
              CALL "C$JUSTIFY" USING HELP-LEGEND2(1:HELP-WINDOW-COL) "R"
              MOVE HELP-LEGEND-PNAME TO
                      HELP-LEGEND2(HELP-LEGEND-PNAME-START:11)
              MOVE 7 TO HELP-FG-COLOR
              MOVE 4 TO HELP-BG-COLOR
              DISPLAY FLOATING GRAPHICAL WINDOW
                      LINES HELP-WINDOW-LINES
                      SIZE HELP-WINDOW-COL
                      HIGHLIGHT
                      TOP TITLE IS HELP-WINDOW-TITLE
                      BOXED
                      WITH NO WRAP
                      FOREGROUND-COLOR IS HELP-FG-COLOR
                      BACKGROUND-COLOR IS HELP-BG-COLOR
                      POP-UP AREA IS HELP-POP-UP-WINDOW
              END-DISPLAY
           ELSE
              MOVE "HELP" TO HELP-WINDOW-TITLE
              CALL "C$JUSTIFY" USING HELP-LEGEND1-RT "L"
              SUBTRACT 20 FROM HELP-WINDOW-COL GIVING HELP-RT-LEN
              CALL "C$JUSTIFY" USING HELP-LEGEND1-RT(1:HELP-RT-LEN) "R"
              MOVE HELP-LEGEND-PNAME TO
                      HELP-LEGEND1(HELP-LEGEND-PNAME-START:11)
              MOVE 7 TO HELP-FG-COLOR
              MOVE 1 TO HELP-BG-COLOR
              DISPLAY FLOATING GRAPHICAL WINDOW
                      LINES HELP-WINDOW-LINES
                      SIZE HELP-WINDOW-COL
                      HIGHLIGHT
                         TOP TITLE IS HELP-WINDOW-TITLE
                      BOXED
                      REVERSE
                      WITH NO WRAP
                      FOREGROUND-COLOR IS HELP-FG-COLOR
                      BACKGROUND-COLOR IS HELP-BG-COLOR
                      POP-UP AREA IS HELP-POP-UP-WINDOW.

           IF HELP-GENERAL-HELP = "Y"
              DISPLAY GENERALHELP-LEGEND-LINE
              DISPLAY GENERAL-HELP-SCREEN-1
           ELSE
              DISPLAY HELP-LEGEND-LINE
              DISPLAY HELP-SCREEN-1.

      *-------------------------------------------------------------------------
      * ACTIVATE HELP CONTROLS
      *-------------------------------------------------------------------------
       HELP-SCAN-ACCEPT.
           IF HELP-GENERAL-HELP = "Y"
              ACCEPT GENERAL-HELP-SCREEN-1
                ON EXCEPTION
                   MOVE KEY-STATUS TO VDU-KEY-DEST
                   PERFORM VDU-SET-STATUS
                   IF STAT = "1>"
                      GO TO HELP-DISPLAY-WINDOW-EXIT
                   END-IF
                   GO TO HELP-SCAN-ACCEPT
              END-ACCEPT
           ELSE
              ACCEPT HELP-SCREEN-1
                ON EXCEPTION
                   MOVE KEY-STATUS TO VDU-KEY-DEST
                   PERFORM VDU-SET-STATUS
                   IF STAT = "1>"
                      GO TO HELP-DISPLAY-WINDOW-EXIT
                   END-IF
                   IF STAT = "1 " AND HELP-GENERAL-HELP NOT = "Y"
                      MOVE "GENERALHELP" TO HELP-LINK-FIELD
                      MOVE "GB/HELP2" TO FORM-NAM
                      CALL FORM-PROGX USING FORM-PATHNAME
                                           HELP-LINK-BUF
                      CANCEL FORM-PROGX
                   END-IF
                   GO TO HELP-SCAN-ACCEPT
              END-ACCEPT.

      *    MOVE "00" TO SCAN-STATUS.

       HELP-DISPLAY-WINDOW-EXIT.
           IF HELP-GENERAL-HELP = "Y"
              DESTROY GENERALHELP-LEGEND-LINE
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GENERAL-HELP-SCREEN-1
           ELSE
              DESTROY HELP-LEGEND-LINE
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY HELP-SCREEN-1.
           CLOSE WINDOW HELP-POP-UP-WINDOW.
           DESTROY HELP-POP-UP-WINDOW.

      ****************************************************
       HELP-PROCESS-ELEMENTS SECTION.
      ****************************************************
           UNSTRING HE-LINE
                 DELIMITED BY ALL "\"
                 INTO MESS
                 POINTER HELP-PTR
                 ON OVERFLOW
                    IF HELP-PTR > HELP-LIST-SIZE
                       MOVE "Y" TO HELP-LIST-EMPTY
                    END-IF
              PERFORM HELP-TEST-FIELD
              MOVE SPACES TO MESS
           END-UNSTRING.

      ****************************************************
       HELP-TEST-FIELD SECTION.
      ****************************************************
           MOVE SPACES TO HELP-FIELD-INPUT HELP-JUNK HELP-COND.
           UNSTRING MESS DELIMITED BY "(" INTO HELP-FIELD-INPUT
                                               HELP-JUNK.
           IF HELP-JUNK NOT = SPACES
              UNSTRING HELP-JUNK
                       DELIMITED BY ")"
                       INTO HELP-COND
              END-UNSTRING.

      *    ------------------------------------------------------------------
      *    IF NOT THE RIGHT FIELD THEN CONTINUE
      *    ------------------------------------------------------------------
           IF HELP-FIELD-INPUT NOT = HELP-FIELD
              GO TO HELP-TEST-FIELD-EXIT.

      *    ------------------------------------------------------------------
      *    IF THERE IS A CONDITION THEN TEST IT.
      *    ------------------------------------------------------------------
           IF HELP-GENERAL-HELP = "Y"
              MOVE SPACES TO HELP-COND
              MOVE "Y" TO HELP-FIELD-FOUND
                          HELP-LIST-EMPTY
           ELSE
              IF HELP-COND = SPACES
                 MOVE "Y" TO HELP-FIELD-FOUND
                             HELP-LIST-EMPTY
              ELSE
                 PERFORM HELP-EVALUATE-COND
                 IF HELP-COND-FLAG = "Y"
                    MOVE "Y" TO HELP-FIELD-FOUND
                                HELP-LIST-EMPTY.

       HELP-TEST-FIELD-EXIT.
           EXIT.

      ******************************************************************
      * GET SIZE AND POSITION FOR CONDITIONAL FIELD BASED ON
      * BUFFER PASSED FROM CALLING PROGRAM.
      ******************************************************************
       COND-GET-INFORMATION SECTION.
      *    MOVE 0 TO CONDITION-POS CONDITION-SIZE.
      *    CALL "C$TOUPPER" USING HELP-LINK-INFO, VALUE HELP-INFO-SIZE.
      *    MOVE 1 TO HELP-PTR.
      *    SET COND-LIST-SIZE TO SIZE OF HELP-LINK-INFO.
      *    MOVE "N" TO HELP-LIST-EMPTY.
      *    PERFORM COND-PROCESS-FIELDS UNTIL HELP-NO-MORE-ELEMENTS.

       COND-PROCESS-FIELDS SECTION.
      *    UNSTRING HELP-LINK-INFO
      *          DELIMITED BY ALL "\"
      *          INTO CONDITION-INPUT-BUF
      *          POINTER HELP-PTR
      *          ON OVERFLOW
      *             IF HELP-PTR > HELP-LIST-SIZE
      *                MOVE "Y" TO HELP-LIST-EMPTY
      *             END-IF
      *    PERFORM COND-SETUP-INFO
      *    MOVE SPACES TO CONDITION-INPUT-BUF
      *    END-UNSTRING.

      ****************************************************
       COND-SETUP-INFO SECTION.
      ****************************************************
           IF CONDITION-INPUT-FIELD = HELP-COND-FIELD
              MOVE CONDITION-INPUT-POS TO CONDITION-POS
              MOVE CONDITION-INPUT-SIZE TO CONDITION-SIZE
              MOVE "Y" TO HELP-LIST-EMPTY.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************************
       COPY "LIBGB/ACCESS.CPY".
       VDU-CONVERT-FIELD.
       SETUP-LINK-INFO.
       FORM-RESET.

      ****************************************************
       IO-ROUTINES SECTION.
      ****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBHER.CPY".
       COPY "LIBGB/FERRORS.CPY".
           IF HELP-GENERAL-HELP = "Y"
              DESTROY GENERALHELP-LEGEND-LINE
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GENERAL-HELP-SCREEN-1
           ELSE
              DESTROY HELP-LEGEND-LINE
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY HELP-SCREEN-1.
           CLOSE WINDOW HELP-POP-UP-WINDOW.
           DESTROY HELP-POP-UP-WINDOW.

      ****************************************************
       END-ROUTINE SECTION.
      ****************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           EXIT PROGRAM.

