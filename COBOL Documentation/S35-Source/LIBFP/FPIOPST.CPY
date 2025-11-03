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
      * BAH 2023-1110 ADDED SEVERAL ITEMS FOR F3-ALL DOCUMENT PRINT TO FILE
      *               WITH MANIFEST #1630
      * BAH 2023-1205 MADE MANIFEST FILE VARYING RECORDS #1630A
      * BAH 2025-0305 MISSED THE REMOVE OF PST FILES IN USR/TMP FOR
      *               PST-WS-OUTPUT-SWITCH-ALL S35Q-171
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
      *    A = PRINT 'ALL' AND DO LIKE 'S'
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PST_OUTPUT" TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-BAD )
              MOVE "N" TO PST-WS-OUTPUT-SWITCH
           ELSE
              MOVE GETENV-BUF TO PST-WS-OUTPUT-SWITCH.

           IF NOT ( PST-WS-OUTPUT-SWITCH-VALID )
              MOVE "N" TO PST-WS-OUTPUT-SWITCH.

           IF ( PST-WS-OUTPUT-SWITCH-DEV )
              PERFORM CREATE-DEV-PST-FILE
              PERFORM REMOVE-PST-FILE
              EXIT PARAGRAPH.

      * IF PRINTING F3-ALL, MUST RENAME WITH BRNO/ACCTNO IN NAME
      * FTP/OUT/DOCS/PRTXXX-BBBBNNNNNNNN-XX
           IF ( PST-WS-OUTPUT-SWITCH-ALL )
              PERFORM MOVE-AND-REMOVE-PST-FILES
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
      *    IN  : PST-PATH-WORKERS
      *    OUT : PST-PATH-WORKERS
      *    DESC: USED TO CREATE PDF FILE FOR DEVELOPMENT USING
      *          GHOSTSCRIPT (PS2PDF).
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

           IF ( PST-WS-OUTPUT-SWITCH-NORMAL ) OR
              ( PST-WS-OUTPUT-SWITCH-DEV    ) OR
              ( PST-WS-OUTPUT-SWITCH-ALL    )
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

      *****************************************************************
      * THIS IS FOR THE F3-ALL OPTION IN DOCUMENT PRINT (SCREEN 7)
      * MOVE THE PST FILE TO /USR/EXTRACT/DOCS AND THEN REMOVE ALL THE
      * TEMP FILES AND WRITE THE NAME TO THE MANIFEST FILE
      *****************************************************************
       MOVE-AND-REMOVE-PST-FILES.
      * /USR/EXTRACT/DOCS/XXX-BBBBNNNNNNNN-FFF-XX
           MOVE "FIL"                    TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF               TO PST-DATA-PATH.

           MOVE PST-FIL-BASE             TO PST-PATH-MAN-FIL
                                            PST-PATH-PRINT-ALL-FIL.
           MOVE PST-PATH-FORM-PROGX-POST TO PST-PATH-PRINT-ALL-PROGRAM.
           MOVE LN-OWNBR                 TO PST-PATH-PRINT-ALL-BRNO.
           MOVE LN-ACCTNO                TO PST-PATH-PRINT-ALL-ACCTNO
           MOVE "-"                      TO PST-PATH-PRINT-ALL-DASH1
                                            PST-PATH-PRINT-ALL-DASH2
                                            PST-PATH-PRINT-ALL-DASH3
           MOVE FD-FMNO                  TO PST-PATH-PRINT-ALL-FMNO
           MOVE FD-TYPE-OF-DOC           TO PST-PATH-PRINT-ALL-TYPE
           MOVE PST-PATH-PRINT           TO PST-WS-MV-FILE-1
           MOVE PST-PATH-PRINT-ALL       TO PST-WS-MV-FILE-2
           MOVE PST-WS-MV-COMMAND        TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           PERFORM REMOVE-PST-FILE.

           MOVE LN-OWNBR                 TO PST-PATH-MAN-BRNO.
           MOVE LN-ACCTNO                TO PST-PATH-MAN-ACCTNO.
           PERFORM OPEN-MANIFEST-FILE.
           MOVE PST-PATH-PRINT-ALL        TO MANIFEST-PROGRAM.
           MOVE X"0D"                     TO MANIFEST-PROGRAM-CR.
           MOVE MANIFEST-PROGRAM-REC      TO MANIFEST-REC.
           PERFORM WRITE-MANIFEST-FILE.
           PERFORM CLOSE-MANIFEST-FILE.

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

      *-----------------------------------------------------------------
       OPEN-MANIFEST-FILE.

           PERFORM OPEN-IT.
           MOVE PST-PATH-MANIFEST TO E-FILE.
           OPEN EXTEND MANIFEST-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-MANIFEST-FILE.

           IF ( IO-ALREADY-OPEN )
              CLOSE MANIFEST-FILE
              GO TO OPEN-MANIFEST-FILE.

      *-----------------------------------------------------------------
       OPEN-MANIFEST-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE PST-PATH-MANIFEST TO E-FILE.
           OPEN OUTPUT MANIFEST-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-MANIFEST-FILE-OUTPUT.

           IF ( IO-ALREADY-OPEN )
              CLOSE MANIFEST-FILE
              GO TO OPEN-MANIFEST-FILE-OUTPUT.

      *-----------------------------------------------------------------
       WRITE-MANIFEST-FILE.
           PERFORM WRITE-IT.
           MOVE PST-PATH-MANIFEST TO E-FILE.
           
           WRITE MANIFEST-REC.
           IF IO-FG = 8
              GO TO WRITE-MANIFEST-FILE.

       CLOSE-MANIFEST-FILE.

           PERFORM CLOSE-IT.
           CLOSE MANIFEST-FILE.

