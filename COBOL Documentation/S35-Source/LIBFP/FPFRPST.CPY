      * COPYMEMBER: LIBFP/FPFRPST
      ******************************************************************
      ******************************************************************
      ******************************************************************
      *  NOTE: EDITS HERE NEED TO BE DONE IN LIBFP/FPFRPST_35PPM
      *  NOTE: THIS USES THE NEW 'HP UNIVERSAL PS' PRINT DRIVER
      *  NOTE: ENABLES PST-WS-FORM-BEGPAGE FROM USE
      *  NOTE: ENABLES PST-WS-FORM-ENDPAGE FROM USE
      *  NOTE: DISABLES PST-WS-FORM-ENDPAGE-35PPM IN USE
      ******************************************************************
      ******************************************************************
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/FPFRPST
      *
      * DESCRIPTION   :  FILE ROUTINES FOR PST-FILE.
      *                  USED FOR PST (LASER) FORM PRINTING.
      *
      *                  ESCAPE CODES FOR MODES OF: PST AND HP-GL/2.
      *
      * *** NOTE ***  :  YOU NEED TO HAVE PST-FILE TO YOUR PROGRAM
      *
      *                  LIBFP/FPFSPST
      *                  LIBFP/FPFDPST
      *                  LIBFP/FPWSPST
      *                  LIBFP/FPFRPST
      *                  LIBFP/FPIOPST
      *
      *=================================================================
      *
      *  SECTION NAME
      *  --------------------------
      *
      *  PST-FR-ERROR
      *  PST-FR-FORM-FINISH
      *  PST-FR-FORM-INITIALIZATION
      *  PST-FR-FORM-START
      *  PST-FR-FORM-TAG
      *  PST-FR-POSITION
      *  PST-FR-POSITION-RESET
      *  PST-FR-FONT
      *  PST-FR-FONT-DEFAULT
      *  PST-FR-FONT-RESET
      *  PST-FR-LINE-SPACING
      *  PST-MERGE-ROUTINE
      *  WRITE-DETAIL-LINE
      *
      *=================================================================
      * REV:
      * KEC 2008-0110 {PR#00000} PARADATA <A15> [JIM ROSENAUER]
      *               ***NEW***  COPIED FROM LIBLP/LASER.
      *               COPIED TO CREATE A NEW WAY FOR LASER FORMS.
      *               CREATED PST-FILE TO SPEED UP PRINTING.
      *
      * KEC 2014-0603 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               FIXED FOR 'PST-FR-FORM-FINISH-EXIT.' NOT BE
      *               BE DUPLICATED FOR DUPLEX SECTION.
      *
      * KEC 2014-0721 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               HAD TO MAKE A CHANGE IN THE USE OF
      *               PST-PAGE-Y-CURRENT IN 'IF' CONDITION.
      *               MUST USE PST-PAGE-Y-CALC INSTEAD OF
      *               PST-PAGE-Y-CURRENT IN 'IF' CONDTION.
      *
      * KEC 2016-0519 {PR#05076} PARADATA <A15> [JUDY REIFSTECK]
      *               ADDED TO PST-FR-FORM-TAG SECTION THE USE OF 
      *                PST-WS-FORM-PROGRAM IN THE FORM TAG LINE.
      *               SIDE NOTE: LOGIC FROM PR#5076 WAS NOT ADDED TO
      *                LPFRPST.  PST LASER FORMS WAS NEW AT THE TIME OF
      *                THIS PR# WHICH IS WHY IT WAS SKIPPED.  THIS WILL
      *                BE REQUIRED IN THE FUTURE.  SO ADDED TO
      *                PST-FR-FORM-TAG SECTION THE USE OF
      *                PST-WS-FORM-PROGRAM IN THE FORM TAG LINE.
      *
      * KEC 2020-0722 {PR#00000} WORLD <A15> [JAY CISZEWSKI]
      *               DUE TO '35PPM PS' PRINT DRIVER NOT WORKING
      *               WITH LATEST WINDOWS 10 UPDATE, NEEDED TO MAKE
      *               CHANGES TO USE 'HP UNIVERSAL PRINTING PS'
      *               PRINT DRIVER WITH NEW /USR/A##/PST FILES:
      *                  ADDED USE OF PST-WS-FORM-BEGPAGE IN
      *                  PST-FR-FORM-START SECTION.
      *               DUE TO PREVIOUS REQUESTS TO HAVE ENTIRE SIDE
      *               TAG PRINT (TOP WAS GETTING CUT OFF) I MOVED
      *               THE TAG POSITION TO THE RIGHT (OR DOWN) BY ONE
      *               POINT:
      *                  WAS - STRING "12 596 ", PST-CMD-MOVETO
      *                  NOW - STRING "12 595 ", PST-CMD-MOVETO
      *
      * KEC 2020-1029 {PR#00000} WORLD <A15> [JAY CISZEWSKI]
      *               COMMENTED OUT 'PST-FR-FORM-FINISH-DUPLEX SECTION.'
      *               WORLD DOES NOT USE DUPLEX AND IT WAS BEING SLIPPED
      *               INTO LIVE PRODUCTION BY COPY EXISTING PROGRAMS
      *               FROM PFS CUSTOMERS AS A STARTER.
      * AMM 2021-0309 ADDED SWITCH FOR PST PRINT DRIVER TO USE FOR 
      *               PRINTING LASER FORMS 
      * JKC 2021-0914 CHANGED THE 'PST-FR-FORM-START SECTION'
      *               TO USE PST-WS-FORM-PROGRAM INSTEAD OF FORM-PROGX;
      *               THERE WAS AN ERROR WHEN CALLING TO CRNOR2 WHICH
      *               CHANGES FORM-PROGX THEN PST-FR-FORM-START STILL
      *               USED IT WHICH HAD "CRNOR2" INSTEAD OF "PST???".
      * BAH 2024-0521 TAG MUST MATCH A15. CHANGED LN-ACCTNO TO USE
      *               PST-WS-TAG-NUMBER AND THE DATE WAS WRONG ALSO S35Q-32
      * JKC 2025-0206 REMOVED PST-WS-SSH-SCRIPT WORKERS SINCE NOT USED.
      *
      ******************************************************************
      *
      *    P S T - F R - E R R O R
      *
      *=================================================================
      *    IN  :  PST-WS-FORM-PROGRAM
      *           PST-WS-STAT   (STAT-BAD OR STAT-GOOD)
      *
      *    OUT :  NOTHING
      *
      *    DESC:  STAT ERROR OCCURRED.  DISPLAYING ERROR.
      ******************************************************************
       PST-FR-ERROR SECTION.

           MOVE SPACES TO MESS.
           STRING "ERROR: 'PST' FILE NOT FOUND {",
                  PST-WS-STAT,
                  "}"
                  DELIMITED BY "  " INTO MESS.
           PERFORM SEND-MESS.

           MOVE SPACES TO MESS.
           STRING "       'PST' FILE: ",
                  PST-WS-FORM-PROGRAM
                  DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-MESS.

       PST-FR-ERROR-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F R - F O R M - F I N I S H
      *
      *=================================================================
      *    IN  :  NOTHING
      *    OUT :  LOAD THE 'TAG' IF 'ON'.
      *    DESC:  FORCING THE PAGE TO EJECT FROM THE PRINTER.
      ******************************************************************
       PST-FR-FORM-FINISH SECTION.

           PERFORM PST-FR-FORM-TAG.

      * SEND ('CAT' COMMAND) THE FORM-ENDPAGE TO PST-FILE.
           IF ( PST-WS-DRIVER-SWITCH-HP )
              MOVE PST-WS-FORM-ENDPAGE       TO PST-WS-CAT-FILE-01
           ELSE
              MOVE PST-WS-FORM-ENDPAGE-35PPM TO PST-WS-CAT-FILE-01.
           MOVE " >> "                       TO PST-WS-CAT-ROUTER.
           MOVE PST-PATH                     TO PST-WS-CAT-FILE-02.

           MOVE PST-WS-CAT-COMMAND           TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           MOVE SPACES                       TO PST-WS-CAT-BUF.

       PST-FR-FORM-FINISH-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F R - F O R M - F I N I S H - D U P L E X
      *
      *=================================================================
      *    IN  :  NOTHING
      *    OUT :  LOAD THE 'TAG' IF 'ON'.
      *    DESC:  FORCING THE PAGE TO EJECT FROM THE PRINTER (DUPLEX).
      ******************************************************************
      *PST-FR-FORM-FINISH-DUPLEX SECTION.

      *    PERFORM PST-FR-FORM-TAG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SEND ('CAT' COMMAND) THE FORM-ENDPAGE TO PST-FILE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE PST-WS-FORM-ENDPAGEDUPLEX TO PST-WS-CAT-FILE-01.
      *    MOVE " >> "                    TO PST-WS-CAT-ROUTER.
      *    MOVE PST-PATH                  TO PST-WS-CAT-FILE-02.

      *    MOVE PST-WS-CAT-COMMAND        TO SYSTEM-BUF.
      *    PERFORM SYSTEM-CALL.

      *    MOVE SPACES                    TO PST-WS-CAT-BUF.

      *=================================================================
      *PST-FR-FORM-FINISH-DX-EXIT.
      *    EXIT.

      ******************************************************************
      *
      *    P S T - F R - F O R M - I N I T I A L I Z A T I O N
      *
      *=================================================================
      *    IN  :  PST-WS-FORM-PAGE-MAX *REQUIRED*
      *           PST-WS-FORM-PROGRAM  *REQUIRED*
      *
      *    OUT :  PST-WS-STAT   (STAT-BAD OR STAT-GOOD)
      *
      *    DESC:  CHECKING TO SEE IF THE PST FILE(S) EXISTS
      ******************************************************************
       PST-FR-FORM-INITIALIZATION SECTION.

           MOVE STAT---GOOD                TO PST-WS-STAT.

           MOVE "REL"                      TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF ( STAT-BAD )
              MOVE STAT                    TO PST-WS-STAT
              GO TO PST-FR-FORM-INITIAL-END.

           MOVE GETENV-BUF                 TO PST-WS-FORM-RELEASE
                                              PST-WS-FORM-BP-RELEASE
                                              PST-WS-FORM-EP-RELEASE
                                              PST-WS-FORM-EP-RELEA35.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE FORM-PROGX                 TO PST-WS-FORM-PROGRAM.

           PERFORM VARYING PST-WS-FORM-PAGE-NO FROM 1 BY 1
                   UNTIL   PST-WS-FORM-PAGE-NO
                         > PST-WS-FORM-PAGE-MAX

                   MOVE PST-WS-FORM-FILE   TO ACCESS-BUF
                   PERFORM ACCESS-CALL
                   IF ( STAT-BAD )
                      MOVE STAT           TO PST-WS-STAT
                   END-IF

           END-PERFORM.

       PST-FR-FORM-INITIAL-END.

           IF ( PST-WS-STAT-BAD )
              PERFORM PST-FR-ERROR.

       PST-FR-FORM-INITIAL-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F R - F O R M - S T A R T
      *
      *=================================================================
      *    IN  :  PST-WS-FORM-PAGE-NO   *REQUIRED*
      *           PST-WS-FORM-PROGRAM
      *
      *    OUT :  PST-WS-STAT   (STAT-BAD OR STAT-GOOD)
      *
      *    DESC:  VERIFY FORM-LAYOUT (PST???.??) EXISTS;
      *           CREATE (LOAD) NEW FILE PATH (PST-PATH);
      *           SEND ('CAT' COMMAND) THE FORM-LAYOUT (PST???.??);
      *           OPEN THE PST-FILE;
      *           SEND ('CAT' COMMAND) THE BEG PAGE (PST-BEGPAGE-PST)
      *           RESET THE FONT TYPE/SIZE.
      *
      ******************************************************************
       PST-FR-FORM-START SECTION.

           MOVE STAT---GOOD                TO PST-START-STAT.

      * VERIFICATION THAT A VALID PAGE NUMBER WAS SET.

           IF ( PST-WS-FORM-PAGE-NO = ZEROES               ) OR
              ( PST-WS-FORM-PAGE-NO > PST-WS-FORM-PAGE-MAX )
              MOVE STAT---BAD              TO PST-WS-STAT
              GO TO PST-FR-FORM-START-EXIT.

      * SET PATH FOR SWITCH VERIFICATION 

           MOVE PST-WS-FORM-RELEASE        TO PST-SEQ-PATH-REL.
           MOVE PST-WS-FORM-PROGRAM        TO PST-SEQ-FORM-PROGX.
           MOVE PST-WS-FORM-PAGE-NO        TO PST-SEQ-PATH-PGNO.

           PERFORM OPEN-PST-SEQ-FILE.
           PERFORM READ-PST-SEQ-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE STAT---BAD              TO PST-WS-STAT
              GO TO PST-FR-FORM-START-EXIT.
              
           PERFORM CLOSE-PST-SEQ-FILE.

           MOVE PST-SEQ-REC                TO PST-WS-SEQ-REC.

      * CREATE (LOAD) NEW FILE PATH (PST-PATH)

           PERFORM LOAD-PST-FILE-PATH.

      * SEND ('CAT' COMMAND) THE FORM-LAYOUT (PST???.??) TO PST-FILE.

           MOVE PST-WS-FORM-FILE         TO PST-WS-CAT-FILE-01.
           MOVE " >> "                   TO PST-WS-CAT-ROUTER.
           MOVE PST-PATH                 TO PST-WS-CAT-FILE-02.

           MOVE PST-WS-CAT-COMMAND       TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           MOVE SPACES                   TO PST-WS-CAT-BUF.

           PERFORM OPEN-PST-FILE.

      * SEND ('CAT' COMMAND) THE FORM-BEGPAGE TO PST-FILE.
      * HP UNIVERSAL DRIVER ONLY; 35PPM SKIPS THIS PART.

           IF ( PST-WS-DRIVER-SWITCH-HP )
              MOVE PST-WS-FORM-BEGPAGE   TO PST-WS-CAT-FILE-01
              MOVE " >> "                TO PST-WS-CAT-ROUTER
              MOVE PST-PATH              TO PST-WS-CAT-FILE-02
              MOVE PST-WS-CAT-COMMAND    TO SYSTEM-BUF
              PERFORM SYSTEM-CALL
              MOVE SPACES                TO PST-WS-CAT-BUF.

           PERFORM PST-FR-FONT-RESET.

       PST-FR-FORM-START-END.

           MOVE PST-START-STAT             TO PST-WS-STAT.

       PST-FR-FORM-START-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F R - F O R M - T A G
      *
      *=================================================================
      *    IN  :  PST-WS-TAG-MODE      {ON/OFF}
      *           PST-WS-FORM-PROGRAM  *REQUIRED*
      *           BR-NO                *REQUIRED*
      *           FM-FMNO              *REQUIRED*
      *           LN-ACCTNO            *REQUIRED*
      *           LN-LOANDATE          *REQUIRED*
      *           PST-WS-TAG-PAGE      *REQUIRED*
      *
      *    OUT :  PST-WS-TAG-PAGE
      *
      *    DESC:  THIS INCREMENTS THE PAGE COUNTER (PST-WS-TAG-PAGE),
      *           THEN CREATES A TAG LINE (PST-WS-TAG-TEXT) THAT WILL BE
      *           USED TO IDENTIFY THE PAGE PRINTED.
      ******************************************************************
       PST-FR-FORM-TAG SECTION.

           IF ( PST-WS-TAG-MODE-OFF )
              GO TO PST-FR-FORM-TAG-EXIT.

      * PST-WS-TAG-TEXT 
           ADD  01                         TO PST-WS-TAG-PAGE.
           MOVE SPACES                     TO PST-WS-TAG-TEXT.

      * MUST REMAIN A 6 DIGIT ACCOUNTNO
           MOVE LN-ACCTNO                  TO PST-WS-TAG-NUMBER.

           IF ( PST-WS-TAG-TYPE-OT )
              MOVE PST-WS-TAG-DATE         TO DATE-YYYYMMDD
              STRING BR-NO,               "-",
                     PST-WS-TAG-NUMBER,   "-",
                     DATE-YYYYMMDD,       "-",
                     FM-FMNO,             "-",
                     PST-WS-FORM-PROGRAM, "-",
                     PST-WS-TAG-PAGE
                     DELIMITED BY SIZE      INTO PST-WS-TAG-TEXT
           ELSE
              MOVE LN-LOANDATE             TO DATE-YYYYMMDD
              STRING BR-NO,               "-",
                     PST-WS-TAG-NUMBER,   "-",
                     DATE-YYYYMMDD,       "-",
                     FM-FMNO,             "-",
                     PST-WS-FORM-PROGRAM, "-",
                     PST-WS-TAG-PAGE
                     DELIMITED BY SIZE      INTO PST-WS-TAG-TEXT.

           CALL "C$JUSTIFY" USING PST-WS-TAG-TEXT PST-WS-TAG-JUSTIFY.

      * 90 ROTATE 

           MOVE SPACES                     TO PST-REC.
           STRING "90 ", PST-CMD-ROTATE
                  DELIMITED BY SIZE      INTO PST-REC.
           PERFORM WRITE-PST-FILE.

      * 0 -612 TRANSLATE 
           MOVE SPACES                     TO PST-REC.
           STRING "0 -612 ", PST-CMD-TRANSLATE
                  DELIMITED BY SIZE      INTO PST-REC.
           PERFORM WRITE-PST-FILE.

      * 12 595 MOVETO
           MOVE SPACES                     TO PST-REC.
           STRING "12 595 ", PST-CMD-MOVETO
                  DELIMITED BY SIZE      INTO PST-REC.
           PERFORM WRITE-PST-FILE.

      * /COURIER 08 SELECTFONT 
           MOVE SPACES                     TO PST-REC.
           STRING "/COURIER 08 ", PST-CMD-SELECTFONT
                  DELIMITED BY SIZE      INTO PST-REC.
           PERFORM WRITE-PST-FILE.

      * SHOW 
           MOVE SPACES                     TO PST-REC.
           STRING "(", PST-WS-TAG-TEXT, PST-CMD-SHOW
                  DELIMITED BY SIZE      INTO PST-REC.
           PERFORM WRITE-PST-FILE.

      * -90 ROTATE 
           MOVE SPACES                     TO PST-REC.
           STRING "-90 ", PST-CMD-ROTATE
                  DELIMITED BY SIZE      INTO PST-REC.
           PERFORM WRITE-PST-FILE.

      * -612 0 TRANSLATE 
           MOVE SPACES                     TO PST-REC.
           STRING "-612 0 ", PST-CMD-TRANSLATE
                  DELIMITED BY SIZE      INTO PST-REC.
           PERFORM WRITE-PST-FILE.

       PST-FR-FORM-TAG-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F R - P O S I T I O N
      *
      *=================================================================
      *    IN  : PST-PAGE-X
      *          PST-PAGE-X-CURRENT
      *          PST-PAGE-Y
      *          PST-PAGE-Y-CURRENT
      *
      *    OUT : PST-PAGE-X
      *          PST-PAGE-X-CURRENT
      *          PST-PAGE-Y
      *          PST-PAGE-Y-CURRENT
      *
      *    DESC: USED TO PLACE THE CURSOR AT THE POINT TO START PRINTING
      *
      *    NOTE: THE X/Y 0/0 POSITION IS IN THE LOWER-LEFT CORNER.
      *          SO TO START IN THE UPPER-LEFT CORNER IT WILL HAVE A
      *          HIGHER Y NUMBER (792 LETTER/1008 LEGAL).
      *
      ******************************************************************
       PST-FR-POSITION SECTION.

           MOVE PST-PAGE-X                 TO PST-PAGE-X-CURRENT.
           MOVE PST-PAGE-Y                 TO PST-PAGE-Y-CURRENT.
           MOVE SPACES                     TO PST-REC.

           STRING PST-PAGE-X-CURRENT, " ",
                  PST-PAGE-Y-CURRENT, " ",
                  PST-CMD-MOVETO
                  DELIMITED BY SIZE      INTO PST-REC.

           PERFORM WRITE-PST-FILE.

           MOVE ZEROES                     TO PST-PAGE-X
                                              PST-PAGE-Y.

       PST-FR-POSITION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F R - P O S I T I O N - R E S E T
      *
      *=================================================================
      *    IN  :  NOTHING
      *
      *    OUT :  PST-PAGE-X-CURRENT
      *           PST-PAGE-Y-CURRENT
      *
      *    DESC: USED TO RESET THE CURSOR AT THE TOP RIGHT CORNER.
      ******************************************************************
       PST-FR-POSITION-RESET SECTION.

           IF ( PST-PAGE-SIZE-LEGAL )
              MOVE PST-PAGE-X-START-LEGAL  TO PST-PAGE-X-CURRENT
              MOVE PST-PAGE-Y-START-LEGAL  TO PST-PAGE-Y-CURRENT
           ELSE
              MOVE PST-PAGE-X-START-LETTER TO PST-PAGE-X-CURRENT
              MOVE PST-PAGE-Y-START-LETTER TO PST-PAGE-Y-CURRENT.

           MOVE SPACES                     TO PST-REC.

           STRING PST-PAGE-X-CURRENT, " ",
                  PST-PAGE-Y-CURRENT, " ",
                  PST-CMD-MOVETO
                  DELIMITED BY SIZE      INTO PST-REC.

           PERFORM WRITE-PST-FILE.

       PST-FR-POSITION-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F R - F O N T
      *
      *=================================================================
      *    IN  :  PST-WS-FONT-SIZE   *REQUIRED*
      *           PST-WS-FONT-TYPE   *REQUIRED*
      *
      *    OUT :  PST-WS-FONT-SIZE
      *           PST-WS-FONT-TYPE
      *
      *    DESC:  THIS WILL SET THE FONT PROPERTIES ACCORDING TO THE
      *           SETTINGS GIVEN.
      *
      ******************************************************************
       PST-FR-FONT SECTION.

           MOVE SPACES                     TO PST-WS-FONT-CODE.

           STRING PST-WS-FONT-TYPE-ELE(PST-WS-FONT-TYPE), " ",
                  PST-WS-FONT-SIZE, " ",
                  PST-CMD-SELECTFONT
                  DELIMITED BY "  "      INTO PST-WS-FONT-CODE.

           MOVE PST-WS-FONT-CODE TO PST-REC.
           PERFORM WRITE-PST-FILE.

           MOVE 12.00                      TO PST-WS-FONT-SIZE.
           MOVE PST-WS-FONT-COURIER        TO PST-WS-FONT-TYPE.

       PST-FR-FONT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F R - F O N T - D E F A U L T
      *
      *=================================================================
      *    IN  :  NOTHING
      *
      *    OUT :  PST-PAGE-Y-SPACING
      *
      *    DESC:  SET THE FONT & SPACING TO THE DEFAULT (COURIER 12CPI).
      *
      ******************************************************************
       PST-FR-FONT-DEFAULT SECTION.

           MOVE SPACES                     TO PST-WS-FONT-CODE.

      * FONT 
           MOVE SPACES                     TO PST-WS-FONT-CODE.
           STRING "/COURIER 12.00 ",
                  PST-CMD-SELECTFONT,
                  DELIMITED BY "  "      INTO PST-WS-FONT-CODE.
           MOVE PST-WS-FONT-CODE TO PST-REC.
           PERFORM WRITE-PST-FILE.

      * LINE SPACING 
           MOVE 06.00                      TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

       PST-FR-FONT-DEFAULT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F R - F O N T - R E S E T
      *
      *=================================================================
      *    IN  :  NOTING
      *
      *    OUT :  PST-PAGE-Y-SPACING
      *
      *    DESC:  RESET THE FONT PROPERTIES TO STANDARD/DEFAULT SETTINGS
      *           AND SET CURSOR POSITION TO TOP OF PAGE.
      *
      ******************************************************************
       PST-FR-FONT-RESET SECTION.

           PERFORM PST-FR-POSITION-RESET.

      * FONT 
           MOVE SPACES                     TO PST-WS-FONT-CODE.
           STRING "/COURIER 12.00 ",
                  PST-CMD-SELECTFONT,
                  DELIMITED BY "  "      INTO PST-WS-FONT-CODE.
           MOVE PST-WS-FONT-CODE TO PST-REC.
           PERFORM WRITE-PST-FILE.

      * LINE SPACING 
           MOVE 06.00                      TO PST-WS-LINE-SPACING-SIZE.
           PERFORM PST-FR-LINE-SPACING.

       PST-FR-FONT-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F R - L I N E - S P A C I N G
      *
      *=================================================================
      *    IN  :  PST-WS-LINE-SPACING-SIZE
      *
      *    OUT :  PST-PAGE-Y-SPACING
      *
      *    DESC:  THIS WILL SET THE FONT PROPERTIES ACCORDING TO THE
      *           SETTINGS GIVEN.
      *
      *    NOTE:  THE "SIZE" IS HOW MANY LINES PER INCH.
      *           THE STANDARD "SIZE" IS 6 LINES PER INCH.
      *
      ******************************************************************
       PST-FR-LINE-SPACING SECTION.

           IF ( PST-WS-LINE-SPACING-SIZE = ZEROES )
              MOVE 06.00                   TO PST-WS-LINE-SPACING-SIZE.

      * PST-PAGE-Y-SPACING 
           COMPUTE PST-PAGE-Y-SPACING ROUNDED =
                   PST-PAGE-DPI / PST-WS-LINE-SPACING-SIZE.

       PST-FR-LINE-SPACING-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - M E R G E - R O U T I N E
      *
      *=================================================================
      *    IN  : PST-MERGE-SOURCE-1
      *          PST-MERGE-SOURCE-2
      *
      *    OUT : PST-MERGE-TARGET
      *
      *    DESC: MERGE TWO SOURCE FIELDS INTO ONE TARGET FIELD.
      ******************************************************************
       PST-MERGE-ROUTINE SECTION.

           MOVE SPACES                     TO PST-MERGE-TARGET.

      * FIND THE END OF PST-MERGE-SOURCE-1

       PST-MERGE-SEARCH.

           MOVE STAT---GOOD                TO PST-MERGE-STAT.

           PERFORM VARYING PST-MERGE-ELE FROM PST-MERGE---MAX BY -1
                   UNTIL   PST-MERGE-STAT-BAD

               IF ( PST-MERGE-ELE = ZEROES )
                  MOVE PST-MERGE-ELE   TO PST-MERGE-SUB-1
                  MOVE STAT---BAD      TO PST-MERGE-STAT
               ELSE
               IF ( PST-MERGE-SOURCE-1-ELE(PST-MERGE-ELE) NOT = SPACES )
                  MOVE PST-MERGE-ELE   TO PST-MERGE-SUB-1
                  MOVE STAT---BAD      TO PST-MERGE-STAT
               END-IF
               END-IF

           END-PERFORM.

           ADD 01 TO PST-MERGE-SUB-1.

           IF ( PST-MERGE-SUB-1 > PST-MERGE---MAX )
              MOVE PST-MERGE-SOURCE-1      TO PST-MERGE-TARGET
              GO TO PST-MERGE-ROUTINE-END.

      * START PST-MERGE-SOURCE-2 INTO THE END OF PST-MERGE-SOURCE-1

           MOVE PST-MERGE-SOURCE-1         TO PST-MERGE-TARGET.
           MOVE ZEROES                     TO PST-MERGE-SUB-2.

           PERFORM VARYING PST-MERGE-ELE FROM PST-MERGE-SUB-1 BY 1
                   UNTIL   PST-MERGE-ELE > PST-MERGE---MAX

                   ADD 01 TO PST-MERGE-SUB-2
                   MOVE PST-MERGE-SOURCE-2-ELE(PST-MERGE-SUB-2)
                     TO PST-MERGE-TARGET-ELE  (PST-MERGE-ELE)

           END-PERFORM.

       PST-MERGE-ROUTINE-END.

           MOVE SPACES                     TO PST-MERGE-SOURCE-1
                                              PST-MERGE-SOURCE-2.

       PST-MERGE-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      *=================================================================
      *    IN  :  DETAIL-LINE
      *           PLW-FEED (LN-FEED)
      *           PST-PAGE-X-CURRENT
      *           PST-PAGE-Y-CURRENT
      *
      *    OUT :  DETAIL-LINE
      *           PLW-FEED (LN-FEED)
      *           PST-PAGE-X-CURRENT
      *           PST-PAGE-Y-CURRENT
      *
      *    DESC:  WRITES THE DETAIL-LINE TO THE PST-FILE (PST-REC).
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           MOVE PST-PAGE-Y-CURRENT         TO PST-PAGE-Y-CALC.

           COMPUTE PST-PAGE-Y-CALC ROUNDED =
                   PST-PAGE-Y-CALC - ( PLW-FEED * PST-PAGE-Y-SPACING).

           MOVE PST-PAGE-Y-CALC            TO PST-PAGE-Y-CURRENT.

           IF ( PST-PAGE-Y-CALC    < ZEROES )
              MOVE ZEROES                  TO PST-PAGE-Y-CURRENT.

           MOVE PST-PAGE-X-CURRENT         TO PST-PAGE-X.
           MOVE PST-PAGE-Y-CURRENT         TO PST-PAGE-Y.
           PERFORM PST-FR-POSITION.

       WRITE-DETAIL-LINE-SHOW.

           MOVE SPACES                     TO PST-REC.

           STRING "(", DETAIL-LINE
                  DELIMITED BY SIZE      INTO PST-REC.

           MOVE PST-REC                    TO PST-MERGE-SOURCE-1.
           MOVE PST-CMD-SHOW               TO PST-MERGE-SOURCE-2.
           PERFORM PST-MERGE-ROUTINE.
           MOVE PST-MERGE-TARGET           TO PST-REC.

           PERFORM WRITE-PST-FILE.

           MOVE SPACES                     TO DETAIL-LINE.
           MOVE 01                         TO PLW-FEED.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.
