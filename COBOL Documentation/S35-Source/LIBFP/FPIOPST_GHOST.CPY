      * COPYMEMBER: LIBFP/FPIOPST
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/FPIOPST
      *
      * DESCRIPTION   :  'IO' FOR PST-FILE
      *
      * *****NOTE*****:  NEED LIBGB/GETPR
      *                       LIBGB/MKTEMP(W)
      *                       LIBGB/RM.CMD
      *                       LIBGB/RMFILE
      *                       LIBGB/SYSTEM(W)
      *
      *=================================================================
      * REV:
      * KEC 2007.1217 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ***NEW***
      *
      * KEC 2009.0407 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ADDED 'PST-WS-RM-COMMAND-SWITCH' FOR HELP
      *               TESTING.  THE SWITCH WILL NOT REMOVE THE
      *               TEMPORARY PRINT/PST FILES AND LEAVE THEM IN TMP.
      *
      * KEC 2020-0828 {PR#00000} WORLD <A15> [JAY CISZEWSKI]
      *               PER APPROVAL FROM KEVIN MULLEN, COMMENTED OUT
      *               USE OF PST-WS-RM-COMMAND-SWITCH; ADDED USE OF
      *               PST-WS-OUTPUT-SWITCH WITH OPTIONS OF N, P & S.
      *
      * KEC 2020-1014 {PR#00000} WORLD <A15> [JAY CISZEWSKI]
      *               ADDED CREATE-DEV-PST-FILE WHICH WILL CONVERTED
      *               A MERGED PST FILES (PRT) TO A PDF FILE USING
      *               GHOSTSCRIPT (PS2PDF).
      *
      * AMM 2021.0219 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ADDED PST-SEQ-FILE
      *
      * KEC 2021-0303 {PR#00000} WORLD <A32> [JAY CISZEWSKI]
      *               ADDED THE USE OF "C$SYSTEM" IN PRINT-PST-FILE
      *               PARAGRAPH TO CAT THE OUTPUT TO THE PRINTER.
      *
      ******************************************************************
      *    IN  : PST-PATH-WORKERS
      *    OUT : NOTHING
      *    DESC: USED TO OPEN PST-FILE.
      *          WILL CREATE PST-PATH-WORKERS.
      *          WILL CREATE PST-FILE IF NEW.
      *=================================================================
       OPEN-PST-FILE.

           MOVE PST-PATH                   TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              PERFORM OPEN-PST-FILE-OUTPUT
              PERFORM CLOSE-PST-FILE.

           PERFORM OPEN-PST-FILE-EXTEND.

      *-----------------------------------------------------------------
       OPEN-PST-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE PST-PATH                   TO E-FILE.
           OPEN OUTPUT PST-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-PST-FILE-OUTPUT.

           IF ( IO-ALREADY-OPEN )
              CLOSE PST-FILE
              GO TO OPEN-PST-FILE-OUTPUT.

      *-----------------------------------------------------------------
       OPEN-PST-FILE-EXTEND.

           PERFORM OPEN-IT.
           MOVE PST-PATH                   TO E-FILE.
           OPEN EXTEND PST-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-PST-FILE-EXTEND.

           IF ( IO-ALREADY-OPEN )
              CLOSE PST-FILE
              GO TO OPEN-PST-FILE-EXTEND.

      *=================================================================
       CLOSE-PST-FILE.

           PERFORM CLOSE-IT.
           CLOSE PST-FILE.

      *=================================================================
      *    IN  : PST-REC
      *=================================================================
       WRITE-PST-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MERGE A 'LINEFEED' CHARACTER AT THE END OF THE PST-REC
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE PST-REC                    TO PST-MERGE-SOURCE-1.
      *    MOVE PST-CMD-LINEFEED           TO PST-MERGE-SOURCE-2.
      *    PERFORM PST-MERGE-ROUTINE.
      *    MOVE PST-MERGE-TARGET           TO PST-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET PST-REC-BYTE-SIZE
      *    *NOTE* NEED TO ADD 1 TO THE END IN ORDER TO GET THE
      *           LINEFEED CHARACTER TO REMAIN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAT---GOOD                TO PST-WS-STAT.
           MOVE PST-REC-BYTE-MAX           TO PST-WS-MAX.

           PERFORM VARYING PST-WS-ELE FROM PST-WS-MAX BY -1
                   UNTIL   PST-WS-STAT-BAD

               IF ( PST-WS-ELE = ZEROES )
                  MOVE 01                  TO PST-WS-ELE
                  MOVE STAT---BAD          TO PST-WS-STAT
               ELSE
               IF ( PST-REC-BYTE(PST-WS-ELE) NOT = SPACES )
                  ADD  01                  TO PST-WS-ELE
                  MOVE STAT---BAD          TO PST-WS-STAT
               END-IF
               END-IF

           END-PERFORM.

           MOVE PST-WS-ELE                 TO PST-REC-BYTE-SIZE.

           IF ( PST-REC-BYTE-SIZE > PST-REC-BYTE-MAX )
              MOVE PST-REC-BYTE-MAX        TO PST-REC-BYTE-SIZE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *KC  MOVE PST-REC                    TO PST-REC-DEBUG-VIEW.
      *KC  STOP PST-REC-DEBUG-VIEW.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

           PERFORM WRITE-IT.
           MOVE PST-PATH                   TO E-FILE.
           WRITE PST-REC.

           IF ( IO-LOCKED )
              GO TO WRITE-PST-FILE.

      *=================================================================
      *    IN  : PST-PATH-WORKERS
      *    OUT : PST-PATH-WORKERS
      *    DESC: USED TO EXECUTE THE PRINT FILE (PST-PATH).
      *          REMOVE THE TEMP FILE (PST-PATH).
      *          RESETS THE PST-PATH-WORKERS.
      *=================================================================
       PRINT-PST-FILE.

           PERFORM MERGE-PST-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SWITCH OPTIONS:
      *    N = NORMAL (PRINT AND REMOVE FILES)
      *    D = DEVELOPMENT (CREATE PDF AND REMOVE FILES; NO PRINT)
      *    P = PRINT & SAVE FILES
      *    S = SAVE ONLY (NOTHING GOES TO PRINTER; SAVES FILES)
      *    G = GHOSTSCRIPT (SSH TO ANOTHER SERVER; REMOVE FILES)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PST_OUTPUT" TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-BAD )
              MOVE "N" TO PST-WS-OUTPUT-SWITCH
           ELSE
              MOVE GETENV-BUF TO PST-WS-OUTPUT-SWITCH.

      D    STOP PST-WS-OUTPUT-SWITCH.
           IF NOT ( PST-WS-OUTPUT-SWITCH-VALID )
              MOVE "N" TO PST-WS-OUTPUT-SWITCH.

           IF ( PST-WS-OUTPUT-SWITCH-DEV )
              PERFORM CREATE-DEV-PST-FILE
              PERFORM REMOVE-PST-FILE
              EXIT PARAGRAPH.

           IF ( PST-WS-OUTPUT-SWITCH-GHOST )
              PERFORM GHOSTSCRIPT-SSH-FILE
              PERFORM REMOVE-PST-FILE
              EXIT PARAGRAPH.

           IF ( PST-WS-OUTPUT-SWITCH-SAVE )
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PRU-PREP
      *
      *    THIS IS PERFORMED INSIDE OF OPEN-PR-FILE.
      *    THISE CHANGES TERMINAL SETUP IF TERMINAL PRINT OR AUXILIARY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM PRU-PREP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THIS EXECUTES THE 'CAT' COMMAND TO PRINT OR LOAD TO THE
      *    PRINTER MEMORY THE PST FILE FOR THE GIVEN PAGE NUMBER.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE PST-PATH-PRINT             TO PST-WS-CAT-BUF.
           MOVE PST-WS-CAT-COMMAND         TO SYSTEM-BUF.
           CALL "C$SYSTEM" USING SYSTEM-BUF 2 GIVING STAT-99.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PRU-RESET.
      *
      *    THIS IS PERFORMED AT END-PROG.
      *    THIS RESETS TERMINAL SETUP IF TERMINAL PRINT OR AUXILIARY.
      *    BUT IT DOES NOT REMOVE LOCK.
      *    HAD TO ADD THE 'IF' TO CATCH SECURITY VIOLATIONS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM PRU-RESET.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    REMOVE ALL WORK FILES (PST-PATHS).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM REMOVE-PST-FILE.

      *=================================================================
      *    IN  : PST-PATH-WORKERS
      *          FORM-PATHNAME
      *    OUT : PST-PATH-WORKERS
      *    DESC: USED TO CREATE A UNIQUE FILE PATHS.
      *=================================================================
       LOAD-PST-FILE-PATH.

           IF ( PST-PATH = SPACES )
              MOVE MKTEMP-DEFAULT          TO MKTEMP-BUF
              PERFORM MKTEMP-CALL.

           MOVE FORM-PATHNAME              TO FORM-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE PST-PATH-DIRECTORY-DEFAULT TO PST-PATH-DIRECTORY.
           MOVE FORM-PROGX                 TO PST-PATH-FORM-PROGX.
           MOVE MKTEMP-NAME                TO PST-PATH-MKTEMP.
           ADD  01                         TO PST-PATH-PGNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ PST-PATH }
           MOVE SPACES                     TO PST-PATH.

           STRING PST-PATH-DIRECTORY,
                  PST-PATH-FORM-PROGX, "-",
                  PST-PATH-MKTEMP,     "-",
                  PST-PATH-PGNO
                  DELIMITED BY "  "      INTO PST-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ PST-PATH-ALL }
           MOVE SPACES                     TO PST-PATH-ALL.

           STRING PST-PATH-DIRECTORY,
                  PST-PATH-FORM-PROGX, "-",
                  PST-PATH-MKTEMP,     "*"
                  DELIMITED BY "  "      INTO PST-PATH-ALL.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ PST-PATH-PRINT }
           MOVE "PRT"                      TO PST-PATH-FORM-PROGX-PRE.
           MOVE SPACES                     TO PST-PATH-PRINT.

           STRING PST-PATH-DIRECTORY,
                  PST-PATH-FORM-PROGX, "-",
                  PST-PATH-MKTEMP
                  DELIMITED BY "  "      INTO PST-PATH-PRINT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    RESET PST-PATH-FORM-PROGX BACK
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PROGX                 TO PST-PATH-FORM-PROGX.

      *=================================================================
      *    IN  : PST-PATH-PRINT
      *    OUT : PST-PATH-PRINT
      *    DESC: USED TO CREATE PDF FILE FOR DEVELOPMENT USING
      *          GHOSTSCRIPT (PS2PDF).
      *          EXECUTE PST_GHOSTSCRIPT.SH PASSING INPUT VARIABLES.
      *=================================================================
       CREATE-DEV-PST-FILE.

           ACCEPT PST-TIME-NOW FROM TIME.

           MOVE SPACES                     TO PST-PATH-DEV.

           STRING PST-PATH-DIRECTORY,       "DEV"
                  PST-PATH-FORM-PROGX-POST, "-",
                  EXT-CONAME-SYSDATE-YY,
                  EXT-CONAME-SYSDATE-MM,
                  EXT-CONAME-SYSDATE-DD,    "-",
                  PST-TIME-NOW-HH,
                  PST-TIME-NOW-MM,
                  PST-TIME-NOW-SS,
                  PST-PATH-PDF-EXT
                  DELIMITED BY "  "      INTO PST-PATH-DEV.

           MOVE PST-PATH-PRINT             TO PST-WS-PS2PDF-FILE-01.
           MOVE PST-PATH-DEV               TO PST-WS-PS2PDF-FILE-02.
           MOVE PST-WS-PS2PDF-COMMAND      TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      *=================================================================
      *    IN  : PST-PATH-FORM-PROGX
      *          PST-WS-SSH-ACCTNO
      *          PST-WS-SSH-BRNO
      *          PST-WS-SSH-SSNO
      *    OUT : PST-WS-SSH-PRT-FILE (CANNOT BE SET TILL THIS PARAGRAPH)
      *    DESC: USED TO SEND (SSH) PRINT FILE (PRT) TO ANOTHER SERVER
      *          THEN USE GHOSTSCRIPT (PS2PDF) TO CREATE PDF FILE.
      *          EXECUTE PST_GHOSTSCRIPT.SH PASSING INPUT VARIABLES.
      *=================================================================
       GHOSTSCRIPT-SSH-FILE.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      D    STOP PST-WS-SSH-SCRIPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET PST-WS-SSH-PRT-FILE WITH 'PRT' FILE;
      *    ALTER PST-PATH-FORM-PROGX, STRING IT INTO PST-WS-SSH-PRT-FILE
      *    THEN RESET IT WITH FORM-PROGX.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRT"                      TO PST-PATH-FORM-PROGX-PRE.
           MOVE SPACES                     TO PST-WS-SSH-PRT-FILE.
           STRING PST-PATH-FORM-PROGX, "-",
                  PST-PATH-MKTEMP
                  DELIMITED BY "  "      INTO PST-WS-SSH-PRT-FILE.
           MOVE FORM-PROGX                 TO PST-PATH-FORM-PROGX.

           MOVE PST-WS-SSH-SCRIPT          TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           IF ( STAT-BAD )
      *%%%%WHAT TO DO HERE??? SEND-MESS OR POP-UP WINDOW???
              MOVE SPACES TO MESS.

      *=================================================================
      *    IN  : PST-PATH-WORKERS
      *    OUT : PST-PATH-WORKERS
      *    DESC: USED TO MERGE (CAT) PST-PATHS INTO ONE FILE.
      *=================================================================
       MERGE-PST-FILE.

           MOVE PST-PATH-ALL               TO PST-WS-CAT-FILE-01.
           MOVE " >> "                     TO PST-WS-CAT-ROUTER.
           MOVE PST-PATH-PRINT             TO PST-WS-CAT-FILE-02.
           MOVE PST-WS-CAT-COMMAND         TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      *=================================================================
      *    IN  : PST-PATH-WORKERS
      *    OUT : PST-PATH-WORKERS
      *    DESC: USED TO REMOVE (RM) ALL PST-PATHS.
      *=================================================================
       REMOVE-PST-FILE.

      *OLD IF ( PST-WS-RM-COMMAND-SWITCH-ON )
           IF ( PST-WS-OUTPUT-SWITCH-NORMAL )
              MOVE PST-PATH-ALL            TO PST-WS-RM-FILE
              MOVE PST-WS-RM-COMMAND       TO SYSTEM-BUF
              PERFORM SYSTEM-CALL
              MOVE PST-PATH-PRINT          TO PST-WS-RM-FILE
              MOVE PST-WS-RM-COMMAND       TO SYSTEM-BUF
              PERFORM SYSTEM-CALL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    RESET PST-PATH FIELDS SO TO SET NEW PST-PATH FOR NEW ACCTNO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO PST-PATH
                                              PST-PATH-ALL
                                              PST-PATH-PRINT
                                              PST-PATH-DIRECTORY
                                              PST-PATH-FORM-PROGX
                                              PST-PATH-MKTEMP.
           MOVE ZEROES                     TO PST-PATH-PGNO.

      *=================================================================
      *    DESC: USED TO OPEN PST-SEQ-FILE.
      *=================================================================
       OPEN-PST-SEQ-FILE.

           PERFORM OPEN-IT.
           MOVE PST-SEQ-PATH                TO E-FILE.
           OPEN INPUT PST-SEQ-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-PST-SEQ-FILE.

           IF ( IO-ALREADY-OPEN )
              CLOSE PST-SEQ-FILE
              GO TO OPEN-PST-SEQ-FILE.

      *-----------------------------------------------------------------
       READ-PST-SEQ-FILE.

           PERFORM READ-IT.
           MOVE PST-SEQ-PATH                TO E-FILE.
           MOVE SPACES                      TO E-KEYX.
           READ PST-SEQ-FILE.
 
           IF ( IO-LOCKED )
              GO TO READ-PST-SEQ-FILE.

      *-----------------------------------------------------------------
       CLOSE-PST-SEQ-FILE.

           PERFORM CLOSE-IT.
           CLOSE PST-SEQ-FILE.

      *=================================================================
