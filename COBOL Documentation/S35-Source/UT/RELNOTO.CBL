      * A30 CONVERSION STARTED
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELNOT.
      ******************************************************************
      *
      * * * TO SEE ALL NOTES, INCLUDING THE "P" PROGRAMMER NOTES, YOU
      *   * MUST SET HELPSH (ALL CAPS) = VI (SMALL LETTERS)
      *     HELPSH=VI;EXPORT HELPSH
      *
      *=================================================================
      * REV:
      * MJD 141008 CHANGED PATH TO ADD .TXT TO FILE NAME
      *            (I.E. RELNOT.TXT)
      * BAH 170421 ACTIVATE 8 DIGIT DATES, PD0006
      * JKC 210702 ADDED SQL-DISCONNECT BEFORE 'STOP RUN'; WE FOUND THAT
      *            THE USE OF SQL-DISCONNECT WHEN EXITING THE
      *            APPLICATION IS ONLY NEEDED; DOING A SQL-DISCONNECT
      *            WHEN EXITING A PROGRAM WILL CAUSE ALL CURSORS TO BE
      *            CLOSED FROM THE CALLING PROGRAM WHICH EFFECTS IT 
      *            WHEN CONTINUING WITH A READ-NEXT.
      *
      * JKC 220623 HAD TO ADD `EXEC SQL INCLUDE` (SQLCA & SQLDA) FOR
      *            THE USE OF LIBGB/DISCONNECT_SQL.CPY TO COMPILE
      *            WITHOUT WARNINGS WHICH DOES NOT SHOW ANY WARNING
      *            MESSAGE BUT SHOWS 1,000+ DUPLICATION OF THIS LINE:
      *               EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
      *
      ******************************************************************
       INSTALLATION.                    COMMENT-ENTRY.
       DATE-WRITTEN.                    1998/02/XX - 99:99:99.
       DATE-COMPILED.                   1998/XX/XX - 00:00:00.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                 COMPUTER-NAME.
       OBJECT-COMPUTER.                 COMPUTER-NAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RN-FILE ASSIGN TO RN-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  FILE STATUS FILE-STAT.
           SELECT WK-FILE ASSIGN TO WK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  FILE STATUS FILE-STAT.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  RN-FILE
           LABEL RECORDS ARE STANDARD.
      ***********************************
      *             (UT)
      *    RN FILE DEFINITION
      *           (80 BYTES)
      ***********************************
       01  RN-REC.
           03  RN-LINE            PIC X(80).
           03  RN-LINEX  REDEFINES RN-LINE.
               05  RN-CHAR        OCCURS 80
                                  PIC X(01).
      *    -------------------------------------------------------------
           03  FILLER REDEFINES RN-LINE.
               05  RN-SLASH1      PIC X(01).
               05  RN-DATE        PIC 9(08).
               05  RN-DATEX REDEFINES RN-DATE.
                   07  RN-YYYY    PIC 9(04).
                   07  RN-MM      PIC 9(02).
                   07  RN-DD      PIC 9(02).
               05  RN-SLASH2      PIC X(01).
               05  RN-TYPE        PIC X(01).
                   88  RN-IS-GENERAL         VALUE " " "G".
                   88  RN-IS-PROGRAMMING     VALUE "P".
               05  RN-SLASH3      PIC X(01).
               05  RN-HEADER      PIC X(63).
               05  RN-REMAINING   PIC X(05).


       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK1-KEY.
               05  WK-DATE             PIC 9(08).
               05  WK-SEQNO            PIC 9(08).
           03  WK-TYPE                 PIC X(01).
           03  WK-HEADER               PIC X(67).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/RELNOT S35 01/10/24-05:31:38 >".

       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLDA END-EXEC.

      *********************************************************
      *        LIBLP/LPWSRN: RN FILE PATH
      *********************************************************
       01  WK-PATH                PIC X(35).
       01  RN-PATH.
           03  FILLER             PIC X(01) VALUE "/".
           03  RN-REL             PIC X(07).
           03  FILLER             PIC X(19) VALUE "/HELP/RELNOT.TXT".

       78  LIST-BOX-LINES               VALUE 19.
       01  RELNOT-POP-UP-WINDOW          PIC X(10).
       01  RELNOT-WINDOW-TITLE           PIC X(30).
       01  SENDMESS-POP-UP-WINDOW        PIC X(10).

       COPY "LIBGB/ACUCOBOL_DEF.CPY".
       COPY "LIBGB/ACUGUI_DEF.CPY".

       77  NUMBER-READS-NEEDED          PIC 99.

      * STATE-FLAG TRACKS WHERE YOU ARE IN THE FILE.
      * READING-FORWARDS INDICATES THAT THE LAST READ WAS
      * OF THE LAST RECORD SHOWN IN THE BOX.
      * READING-BACKWARDS INDICATES THAT THE LAST READ
      * WAS OF THE FIRST RECORD IN THE BOX (VIA READ
      * PREVIOUS).  AT-START AND AT-END HANDLE SPECIAL
      * CASES WHEN YOU REACH EITHER END OF THE FILE.

       77  STATE-FLAG                   PIC X.
           88  READING-FORWARDS         VALUE "F".
           88  READING-BACKWARDS        VALUE "B".
           88  AT-START                 VALUE "S".
           88  AT-END                   VALUE "E".

       77  LIST-DATA                    PIC X(78).

      *    VI +/\"\FIELD"  /$REL/HELP/
       01  HELP-CALL-VI.
           03  FILLER                PIC X(6)  VALUE
               X"7669202B2F22".
           03  HELP-CALL-VI-FIELD    PIC X(77) VALUE " ".
           03  HELP-CALL-VI-FILE.
               05  FILLER            PIC X(11) VALUE
                   X"2F2452454C2F48454C502F".
               05  HELP-NAM-VI       PIC X(9)  VALUE " ".

      *    /$REL/SH/HELP.SH
       01  HELP-CALL-SH.
           03  FILLER                PIC X(17) VALUE
               X"2F2452454C2F53482F68656C702E736820".
           03  HELP-NAM-SH              PIC X(9)  VALUE " ".


       01  HEADER-COUNT                 PIC 9(08) COMP.

       01  NOTE-TYPE                    PIC X(01).
           88 NOTE-TYPE-PROGRAMMER      VALUE "P" X'70'.
       01  SCAN-LINE.
           03  FILLER                   PIC X(01).
           03  SCAN-DATE                PIC 99/99/9999.
           03  FILLER                   PIC X(01).
           03  SCAN-TYPE                PIC X(01).
           03  FILLER                   PIC X(01).
           03  SCAN-HEADER              PIC X(64).

      *77  KEY-STATUS
      *    IS SPECIAL-NAMES CRT STATUS  PIC 9(4).

      * NOTE: ALTHOUGH THE PROGRAM NEVER DIRECTLY
      * REFERENCES THE SCREEN CONTROL DATA ITEM, YOU HAVE
      * TO DECLARE IT ANYWAY.  THAT IS BECAUSE "NOTIFY"
      * STYLE EVENTS WILL SET IT TO THE PROPER VALUE WHEN
      * ENTERING AN EXCEPTION PROCEDURE.  THE VALUE
      * GENERATED CAUSES THE CONTROLLING ACCEPT STATEMENT
      * TO CONTINUE PROCESSING AFTER THE EXCEPTION
      * PROCEDURE FINISHES.  IF YOU OMIT SCREEN-CONTROL,
      * THEN THE RESULT IS THAT YOU EXIT THE ACCEPT AFTER
      * THE EXCEPTION PROCEDURE COMPLETES.

       01  SCREEN-CONTROL
           IS SPECIAL-NAMES SCREEN CONTROL.
           03  ACCEPT-CONTROL            PIC 9.
           03  CONTROL-VALUE             PIC 999.

       01  EVENT-STATUS
           IS SPECIAL-NAMES EVENT STATUS.
           03  EVENT-TYPE                PIC X(4) COMP-X.
           03  EVENT-WINDOW-HANDLE       HANDLE OF WINDOW.
           03  EVENT-CONTROL-HANDLE      HANDLE.
           03  EVENT-CONTROL-ID          PIC X(2) COMP-X.
           03  EVENT-DATA-1              SIGNED-SHORT.
           03  EVENT-DATA-2              SIGNED-LONG.
      *    03  EVENT-ACTION              PIC X COMP-X.
       01  RELNOT-BUF.
           03  LINK-FILE       PIC X(20).
           03  LINK-HEADER     PIC X(80).

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBWI/RELNOT_CMD.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       01  SCREEN-1.
           03  LABEL LINE 01 COL 04 X'44617465'  ,LOW.
           03  LABEL LINE 01 COL 15 X'4368616E6765'  ,LOW.
           03  LABEL LINE 02 COL 02 "----------"  ,LOW.
           03  LABEL LINE 02 COL 15
               "------------------------------"  ,LOW.
           03  LABEL LINE 02 COL 44
               "---------------------------------"  ,LOW.
           03  LIST-1, LIST-BOX USING LIST-DATA,
               LINE 03, COLUMN 01, SIZE 78,
      *        LINES LIST-BOX-LINES + VDU-WIN-H-MOD
               LINES LIST-BOX-LINES 
               PAGED, NO-BOX, NO-SEARCH, COLOR IS 8
               EXCEPTION PROCEDURE IS LIST-1-HANDLER.

       01 SENDMESS-WINDOW.
          03  USING MESS
              LINE 001
              COL 001
              BOLD
              CHARACTER
              COLOR IS 8
              PROMPT " "
              NO ECHO.
          03  LINE 001
              COL 062
              "PRESS <ENTER>:"
              BOLD
              COLOR IS 8.
       01 SENDMESS-ACCEPT.
          03  USING STAT
              LINE 001
              COL 076
              BOLD
              CHARACTER
              COLOR IS 8
              PROMPT " ".


       PROCEDURE DIVISION USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON RN-FILE WK-FILE.
       CHECK-ERROR-TYPE.
           MOVE 0 TO IO-FG
           IF FILE-STAT NOT = "00"
              MOVE 9 TO IO-FG
              GO TO END-DECLARE.
       CLOSE-FILES.
           CLOSE RN-FILE WK-FILE.
       DECLARE-STOP.
           PERFORM SQL-DISCONNECT.  *> NEED TO BE LAST BEFORE `STOP RUN`
           STOP RUN.
       END-DECLARE.
           EXIT.
       END DECLARATIVES.

      ******************************************************************
       MAIN-LOGIC SECTION.
      ******************************************************************
           MOVE FORM-PATHNAME TO FORM-PATH.

      D    STOP MESS.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY "LOADING HEADERS....." AT 002002.

           MOVE 0 TO EVENT-TYPE.
           SET READING-FORWARDS TO TRUE.

           ACCEPT RN-REL FROM ENVIRONMENT "REL".
           MOVE RN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO END-ROUTINE.

      * SETUP WORK FILE PATH AND OPEN/CLEAR

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

      * GET TYPE OF SCAN FROM ENVIRONMENT.

           MOVE SPACES TO NOTE-TYPE MESS.
           ACCEPT MESS FROM ENVIRONMENT "HELPSH".
           IF MESS = X'7669'
              MOVE "P" TO NOTE-TYPE
           ELSE
              MOVE SPACES TO NOTE-TYPE.

      * SETUP WORK RECORD

           PERFORM GET-RELNOT-HEADERS.
           IF HEADER-COUNT = 0
              GO TO END-ROUTINE.

      * OPEN WINDOW

           IF NOTE-TYPE-PROGRAMMER
              MOVE PROGRAMMER-TITLE TO RELNOT-WINDOW-TITLE
           ELSE
              MOVE GENERAL-TITLE TO RELNOT-WINDOW-TITLE.

           DISPLAY "                    " AT 002002.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE 2
                   AT COL  2
                   SIZE 78 + VDU-WIN-W-MOD
                   LINES 22 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   TOP CENTERED TITLE IS RELNOT-WINDOW-TITLE
                   POP-UP AREA IS RELNOT-POP-UP-WINDOW.

           DISPLAY SCREEN-1.

      * LOAD THE FIRST PAGE OF LIST BOX ITEMS

           MODIFY LIST-1, MASS-UPDATE = 1
           PERFORM LIST-BOX-LINES TIMES
              PERFORM READ-WK1-FILE-NEXT
              IF IO-BAD OR IO-AT-END
                  SET AT-END TO TRUE
                  EXIT PERFORM
              END-IF
              SUBTRACT WK-DATE FROM 99999999 GIVING DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY
              MOVE DATE-MMDDYYYY TO SCAN-DATE
              IF WK-TYPE = "P"
                 MOVE WK-TYPE TO SCAN-TYPE
              ELSE
                 MOVE SPACE TO SCAN-TYPE
              END-IF
              MOVE WK-HEADER TO SCAN-HEADER
              MODIFY LIST-1,
                     ITEM-TO-ADD = SCAN-LINE
           END-PERFORM

           MODIFY LIST-1, MASS-UPDATE = 0.

      * ACTIVATE THE LIST BOX

       SCAN-ACCEPT.

           ACCEPT SCREEN-1
             ON EXCEPTION
                MOVE KEY-STATUS TO VDU-KEY-DEST
                PERFORM VDU-SET-STATUS
                IF STAT = "1>"
                   GO TO END-ROUTINE
                END-IF
                GO TO SCAN-ACCEPT
           END-ACCEPT.

           MOVE LIST-DATA TO SCAN-LINE.
           MOVE SCAN-HEADER TO LINK-HEADER.
           IF LINK-HEADER = SPACES
              GO TO SCAN-ACCEPT.

           MOVE "GB/RELNOT" TO LINK-FILE.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WI/RELNO2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME RELNOT-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           GO TO SCAN-ACCEPT.

           GO TO END-ROUTINE.

      ******************************************************************
      * LIST-1-HANDLER HANDLES ALL EXCEPTIONS GENERATED
      * BY THE LIST BOX.  THE ONLY EXCEPTIONS YOU CARE
      * ABOUT ARE THOSE THAT REQUIRE A RESPONSE IN
      * ORDER TO MANAGE THE LIST BOX PROPERLY.
      ******************************************************************
       LIST-1-HANDLER SECTION.
           IF KEY-STATUS = W-EVENT
              EVALUATE EVENT-TYPE
                WHEN NTF-PL-NEXT
                  PERFORM GET-NEXT-ITEM

                WHEN NTF-PL-PREV
                  PERFORM GET-PREV-ITEM

                WHEN NTF-PL-NEXTPAGE
                  MODIFY LIST-1, MASS-UPDATE = 1
                  PERFORM GET-NEXT-ITEM LIST-BOX-LINES TIMES
                  MODIFY LIST-1, MASS-UPDATE = 0

                WHEN NTF-PL-PREVPAGE
                  MODIFY LIST-1, MASS-UPDATE = 1
                  PERFORM GET-PREV-ITEM LIST-BOX-LINES TIMES
                  MODIFY LIST-1, MASS-UPDATE = 0

                WHEN NTF-PL-FIRST
                  MOVE LOW-VALUES TO WK-DATE
                  PERFORM START-WK1-FILE-NOT-LESS-THAN
                  IF IO-INVALID-KEY
                     EXIT PARAGRAPH
                  END-IF
                  SET READING-FORWARDS TO TRUE
                  MODIFY LIST-1, MASS-UPDATE = 1
                  MODIFY LIST-1, RESET-LIST = 1
                  PERFORM GET-NEXT-ITEM LIST-BOX-LINES TIMES
                  MODIFY LIST-1, MASS-UPDATE = 0

                WHEN NTF-PL-LAST
                  MOVE HIGH-VALUES TO WK-DATE
                  PERFORM START-WK1-FILE-NOT-GREATER
                  IF IO-INVALID-KEY
                     EXIT PARAGRAPH
                  END-IF
                  SET READING-BACKWARDS TO TRUE
                  MODIFY LIST-1, MASS-UPDATE = 1
                  MODIFY LIST-1, RESET-LIST = 1
                  PERFORM GET-PREV-ITEM LIST-BOX-LINES TIMES
                  MODIFY LIST-1, MASS-UPDATE = 0

      ******************************************************************
      * IN THE SEARCH LOGIC, IF YOU GET TOO CLOSE TO THE
      * END OF THE FILE, YOU SIMPLY ACT AS IF YOU WANTED TO
      * FIND THE LAST FULL PAGE.  DO THIS BY SETTING THE
      * EVENT TYPE TO NTF-PL-LAST AND RE-EVALUATING.
      ******************************************************************

                WHEN NTF-PL-SEARCH
                  INQUIRE LIST-1,
                          SEARCH-TEXT IN WK-DATE
                  PERFORM START-WK1-FILE-NOT-LESS-THAN
                  IF IO-INVALID-KEY
                     MOVE NTF-PL-LAST TO EVENT-TYPE
                     GO TO LIST-1-HANDLER
                  END-IF
                  SET READING-FORWARDS TO TRUE
                  MODIFY LIST-1, MASS-UPDATE = 1
                  PERFORM GET-NEXT-ITEM LIST-BOX-LINES TIMES
                  IF AT-END
                    MOVE NTF-PL-LAST TO EVENT-TYPE
                    GO TO LIST-1-HANDLER
                  END-IF
                  MODIFY LIST-1, MASS-UPDATE = 0
              END-EVALUATE
            END-IF.

      ******************************************************************
      * GET-NEXT-ITEM HANDLES ALL CASES WHERE YOU READ
      * FORWARDS THROUGH THE FILE.  IT ADJUSTS FOR THE
      * FOUR POSSIBLE STATES YOU COULD BE IN AND THEN
      * RETRIEVES THE NEXT RECORD.  THIS RECORD IS ADDED
      * TO THE END OF THE LIST BOX.  IN SOME CASES, YOU
      * HAVE TO TRAVERSE OVER THE PAGE OF RECORDS
      * CURRENTLY DISPLAYED (BECAUSE YOU ARE SWITCHING
      * DIRECTION).
      ******************************************************************
       GET-NEXT-ITEM SECTION.
           EVALUATE TRUE
             WHEN AT-START
                MOVE LOW-VALUES TO WK-DATE
                PERFORM START-WK1-FILE-NOT-LESS-THAN
                IF IO-INVALID-KEY
                   EXIT PARAGRAPH
                END-IF
                ADD 1 TO LIST-BOX-LINES
                    GIVING NUMBER-READS-NEEDED
             WHEN AT-END
                EXIT PARAGRAPH
             WHEN READING-BACKWARDS
                MOVE LIST-BOX-LINES TO NUMBER-READS-NEEDED
             WHEN READING-FORWARDS
                MOVE 1 TO NUMBER-READS-NEEDED
             END-EVALUATE

             PERFORM NUMBER-READS-NEEDED TIMES
                PERFORM READ-WK1-FILE-NEXT
                IF IO-BAD OR IO-AT-END
                   SET AT-END TO TRUE
                   EXIT PARAGRAPH
                END-IF
             END-PERFORM

             SUBTRACT WK-DATE FROM 99999999 GIVING DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY
             MOVE DATE-MMDDYYYY TO SCAN-DATE
              IF WK-TYPE = "P"
                 MOVE WK-TYPE TO SCAN-TYPE
              ELSE
                 MOVE SPACE TO SCAN-TYPE
              END-IF
             MOVE WK-HEADER TO SCAN-HEADER
             MODIFY LIST-1,
                    ITEM-TO-ADD = SCAN-LINE

             SET READING-FORWARDS TO TRUE.

      ******************************************************************
      * GET-PREV-ITEM IS THE CONVERSE OF GET-NEXT-ITEM.
      * IT RETRIEVES THE PREVIOUS RECORD IN THE LIST AND
      * ADDS IT TO THE TOP OF THE LIST BOX.  THE CODE IS
      * STRUCTURED IDENTICALLY TO GET-NEXT-ITEM.
      ******************************************************************
       GET-PREV-ITEM SECTION.
               EVALUATE TRUE
                 WHEN AT-END
                    MOVE HIGH-VALUES TO WK-DATE
                    PERFORM START-WK1-FILE-NOT-GREATER
                    IF IO-INVALID-KEY
                       EXIT PARAGRAPH
                    END-IF
                   ADD 1 TO LIST-BOX-LINES
                       GIVING NUMBER-READS-NEEDED
                 WHEN AT-START
                    EXIT PARAGRAPH
                 WHEN READING-FORWARDS
                    MOVE LIST-BOX-LINES TO NUMBER-READS-NEEDED
                 WHEN READING-BACKWARDS
                    MOVE 1 TO NUMBER-READS-NEEDED
               END-EVALUATE

            PERFORM NUMBER-READS-NEEDED TIMES
               PERFORM READ-WK1-FILE-PREVIOUS
               IF IO-BAD OR IO-AT-END
                  SET AT-START TO TRUE
                  EXIT PARAGRAPH
               END-IF
            END-PERFORM

             SUBTRACT WK-DATE FROM 99999999 GIVING DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY
             MOVE DATE-MMDDYYYY TO SCAN-DATE
              IF WK-TYPE = "P"
                 MOVE WK-TYPE TO SCAN-TYPE
              ELSE
                 MOVE SPACE TO SCAN-TYPE
              END-IF
             MOVE WK-HEADER TO SCAN-HEADER
            MODIFY LIST-1
               INSERTION-INDEX = 1
               ITEM-TO-ADD = SCAN-LINE

            SET READING-BACKWARDS TO TRUE.

      ******************************************************************
      * GET HEADER PORTION OF RELNOT FILE
      *
      *  HEADER FORMAT:
      * \20000101\P\ {UP TO ?? CHARACTERS FOR HEADER.\
      ******************************************************************
       GET-RELNOT-HEADERS SECTION.
           MOVE 0 TO HEADER-COUNT.
           PERFORM OPEN-RN-FILE.
       NEXT-RN.
           MOVE SPACES TO RN-REC.
           PERFORM READ-RN-FILE.
           IF IO-BAD
              GO TO GET-RELNOT-HEADERS-EXIT.
           IF RN-SLASH1 NOT = "\" OR
                 RN-SLASH2 NOT = "\" OR
                    RN-SLASH3 NOT = "\"
              GO TO NEXT-RN.
           IF RN-DATE NOT NUMERIC
              GO TO NEXT-RN.

           IF NOT NOTE-TYPE-PROGRAMMER
              IF RN-TYPE = "P" OR X'70'
                 GO TO NEXT-RN.

           SUBTRACT RN-DATE FROM 99999999 GIVING WK-DATE.
           INSPECT RN-HEADER REPLACING ALL "\" BY " ".
           MOVE RN-HEADER TO WK-HEADER.
           MOVE RN-TYPE TO WK-TYPE.
           MOVE HEADER-COUNT TO WK-SEQNO.
           PERFORM WRITE-WK1-FILE.
           IF IO-FG NOT = 0
              GO TO GET-RELNOT-HEADERS-EXIT.

           ADD 1 TO HEADER-COUNT.
           GO TO NEXT-RN.
       GET-RELNOT-HEADERS-EXIT.
           PERFORM CLOSE-RN-FILE.


       COPY "LIBGB/DATER.CPY".

      ******************************************************************
      * CONVERT ACUCOBOL FUNCTION KEYS TO CURRENT PARADATA CODES
      ******************************************************************
       VDU-SET-STATUS SECTION.
ENTER         MOVE "00" TO STAT.
           EVALUATE VDU-KEY-DEST
ENTER         WHEN 13 MOVE "00" TO STAT
TAB   *------>WHEN  9 MOVE "??" TO STAT
F1            WHEN  1 MOVE "10" TO STAT
F2            WHEN  2 MOVE "12" TO STAT
F3            WHEN  3 MOVE "16" TO STAT
F4            WHEN  4 MOVE "18" TO STAT
F5            WHEN  5 MOVE "1:" TO STAT
F6            WHEN  6 MOVE "1<" TO STAT
F7            WHEN  7 MOVE "1>" TO STAT
F8            WHEN  8 MOVE "1 " TO STAT
F9            WHEN  9 MOVE "1(" TO STAT
F10           WHEN 10 MOVE "1)" TO STAT
F11           WHEN 11 MOVE "1!" TO STAT
F12           WHEN 12 MOVE "1#" TO STAT
F13   *------>WHEN 13 MOVE "??" TO STAT
^L            WHEN 14 MOVE "^L" TO STAT
^N            WHEN 15 MOVE "^N" TO STAT
^B            WHEN 16 MOVE "^B" TO STAT
F17   *------>WHEN 17 MOVE "??" TO STAT
F18   *       WHEN 18 MOVE "??" TO STAT
F19   *       WHEN 19 MOVE "??" TO STAT
F20   *       WHEN 20 MOVE "??" TO STAT
DO CO *       WHEN 40 MOVE "??" TO STAT
HELP  *------>WHEN 90 MOVE "??" TO STAT
UP A          WHEN 52 MOVE "1U" TO STAT
DN A          WHEN 53 MOVE "1D" TO STAT
PG U          WHEN 67 MOVE "1P" TO STAT
PF D          WHEN 68 MOVE "1N" TO STAT
           END-EVALUATE.
           INITIALIZE VDU-KEY-DEST.

      **************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************

      **************************************
       FORM-RESET SECTION.
      **************************************

      **************************************
       SEND-MESS SECTION.
      **************************************
           MOVE "00" TO STAT.
           DISPLAY FLOATING GRAPHICAL WINDOW
              AT 022002
              LINES 01 + VDU-WIN-H-MOD
              SIZE 78 + VDU-WIN-W-MOD
              TOP CENTERED TITLE IS "MESSAGE"
              POP-UP AREA IS SENDMESS-POP-UP-WINDOW.

           DISPLAY SENDMESS-WINDOW.
           ACCEPT STAT
              AT 001076
              BOLD
              COLOR IS 8
              PROMPT " "
              NO ECHO.

      * CLOSE WINDOW AND EXIT PROGRAM

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SENDMESS-POP-UP-WINDOW.

           IF STAT = "~~"
              GO TO END-ROUTINE.
           MOVE "00" TO STAT.


      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/DISCONNECT_SQL.CPY".
       OPEN-RN-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE RN-PATH TO E-FILE.
           OPEN INPUT RN-FILE.
           IF IO-FG = 8
              GO TO OPEN-RN-FILE.
           IF IO-FG = 7
              CLOSE RN-FILE
              GO TO OPEN-RN-FILE.
       READ-RN-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE RN-PATH TO E-FILE.
           READ RN-FILE
                AT END MOVE 9 TO IO-STATUS
           END-READ.
           IF IO-FG = 8
              GO TO READ-RN-FILE.
       CLOSE-RN-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE RN-FILE.

       OPEN-WK1-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK1-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK1-FILE.
       OPEN-WK1-FILE-OUTPUT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK1-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK1-FILE.
       READ-WK1-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE.
           IF IO-FG = 8
              GO TO READ-WK1-FILE.
       READ-WK1-FILE-NEXT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE NEXT
              AT END    MOVE 9 TO IO-STATUS
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK1-FILE-NEXT.
       READ-WK1-FILE-PREVIOUS.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE PREVIOUS
              AT END    MOVE 9 TO IO-STATUS
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK1-FILE-PREVIOUS.
       START-WK1-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK1-KEY
              INVALID KEY
                 MOVE 9 TO IO-STATUS
           END-START.
       START-WK1-FILE-GREATER.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           START WK-FILE KEY > WK1-KEY
              INVALID KEY
                 MOVE 9 TO IO-STATUS
           END-START.
           UNLOCK WK-FILE.
       START-WK1-FILE-NOT-GREATER.
      *--->PERFORM START-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           START WK-FILE KEY NOT > WK1-KEY
              INVALID KEY
                 MOVE 9 TO IO-STATUS
           END-START.
           UNLOCK WK-FILE.
       START-WK1-FILE-NOT-LESS-THAN.
      *--->PERFORM START-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK1-KEY
              INVALID KEY
                 MOVE 9 TO IO-STATUS
           END-START.
           UNLOCK WK-FILE.
       WRITE-WK1-FILE.
      *--->PERFORM WRITE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           WRITE WK-REC.
           IF IO-FG = 8
             GO TO WRITE-WK1-FILE.
           UNLOCK WK-FILE.
       REWRITE-WK1-FILE.
      *--->PERFORM REWRITE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
             GO TO REWRITE-WK1-FILE.
           UNLOCK WK-FILE.
       DELETE-WK1-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           DELETE WK-FILE.
           IF IO-FG = 8
             GO TO DELETE-WK1-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK1-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE WK-FILE.

      *******************************************
       FILE-IO-ERROR SECTION.
      *******************************************
       IO-ERROR.

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE "GB/GBMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ALL.
           CLOSE WINDOW RELNOT-POP-UP-WINDOW.
           EXIT PROGRAM.
