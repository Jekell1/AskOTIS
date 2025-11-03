       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      EXTVAR.
      ******************************************************************
      *
      * DISPLAYS ALL EXTERNAL VARIABLES IN CONAME_EXT
      *
      * REV:
      * MJD 2015-0126 NEW PROFGRAM.
      *
      * KEC 2016-0517 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED USE OF APPNO TO APP-NUMBER FOR MA1-KEY USAGE.
      *
      * JKC 2020-0612 COMMENTED OUT THE FOLLOWING SINCE THEY ARE NOT
      *         NOT USED ANYMORE:
      *           EXT-CONAME-AUTO-INS-EARN
      *           EXT-CONAME-BRGIFTNET
      *           EXT-CONAME-EXTRACT-AUTO-UPD
      *           EXT-LAMENU-APP-NUMBER
      *           EXT-LAMENU-CALLING-STAT
      *           EXT-TERM-COLUMNS
      *           EXT-TERM-LINES
      *           EXT-TERM-NAME
      *           EXT-TERM-PHYS-SCRN-HEIGHT
      *           EXT-TERM-PHYS-SCRN-WIDTH
      *           EXT-TERM-USABLE-SCRN-HEIGHT
      *           EXT-TERM-USABLE-SCRN-WIDTH
      *           EXT-TERM-ATTR-132-MODE
      *           EXT-TERM-ATTR-BLINK
      *           EXT-TERM-ATTR-COLOR
      *           EXT-TERM-ATTR-DUAL-INTENSITY
      *           EXT-TERM-ATTR-GUI
      *           EXT-TERM-ATTR-LINE-DRAW
      *           EXT-TERM-ATTR-PIGGY
      *           EXT-TERM-ATTR-REVERSE
      *           EXT-TERM-ATTR-UNDERLINE
      *           EXT-TERM-ATTR-VIS-ATTRIB
      *
      ******************************************************************
       INSTALLATION.                    COMMENT-ENTRY.
       DATE-WRITTEN.                    2015/01/26 - 15:00:00.
       DATE-COMPILED.                   2015/01/26 - 15:00:00.
       AUTHOR.                          MIKE DONIGAN - MJD

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                 COMPUTER-NAME.
       OBJECT-COMPUTER.                 COMPUTER-NAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/EXTVAR.CBL A32 06/12/20-12:28:14 >".

      *********************************************************
      *        LIBLP/LPWSRN: RN FILE PATH
      *********************************************************

      *FOR C$CALLERR ROUTINE TO GET WHY THE CALL STATEMENT FAILED.
       77  ERR-CODE                      PIC X(2).
       77  ERR-MESSAGE                   PIC X(80).
      *FOR C$RERR ROUTINE TO GET THE EXTENDEED FILE STATUS INFORMATION.
       01  EXTEND-STAT.
           03 PRIMARY-ERROR              PIC X(2).
           03 SECONDARY-ERROR            PIC X(10).
       01  TEXT-MESSAGE                  PIC X(40).
      *FOR C$RERRNAME ROUTINE TO GET THE LAST FILE USED IN AN I/O STATEMENT.
       77  LAST-FILENAME                 PIC X(40).
       01  WS-CONT                       PIC X(01).

       78  LIST-BOX-LINES               VALUE 19.
       01  EXTVAR-POP-UP-WINDOW          PIC X(10).
       01  SENDMESS-POP-UP-WINDOW        PIC X(10).
       01  VI-POP-UP-WINDOW              PIC X(10).

      * NOT NEEDED.
      * COPY "LIBGB/ACUCOBOL_DEF.CPY".
      * COPY "LIBGB/ACUGUI_DEF.CPY".

      *INTERNAL STORAGE FOR EXTERNAL VARIABLES
       01  EV-TABLE.
           03  EV-LINE         OCCURS 200.
               05  EV-NAME        PIC X(30).
               05  EV-VALUE       PIC X(46).
               05 FILLER REDEFINES EV-VALUE.
                   07  FILLER     PIC X(36).
                   07  EV-INT PIC 9(10).

       01  EV-TAB-PTR         PIC 99.
       01  EV-TAB-MAX         PIC 99.

       77  KEYWORD-STATUS               PIC XX.
       77  NUMBER-READS-NEEDED          PIC 99.

       77  LIST-DATA                    PIC X(76).
       01  ADD-ITEM.
           03  FILLER                   PIC X(01).
           03  ADD-NAME                 PIC X(30).
           03  FILLER                   PIC X(01).
           03  ADD-VALUE                PIC X(44).

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

       01  SCAN-LINE.
           03  FILLER                   PIC X(01).
           03  SCAN-NAME                PIC X(30).
           03  FILLER                   PIC X(01).
           03  SCAN-VALUE               PIC X(44).

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

      *01  EVENT-STATUS
      *    IS SPECIAL-NAMES EVENT STATUS.
      *    03  EVENT-TYPE                PIC X(4) COMP-X.
      *    03  EVENT-WINDOW-HANDLE       HANDLE OF WINDOW.
      *    03  EVENT-CONTROL-HANDLE      HANDLE.
      *    03  EVENT-CONTROL-ID          PIC X(2) COMP-X.
      *    03  EVENT-DATA-1              SIGNED-SHORT.
      *    03  EVENT-DATA-2              SIGNED-LONG.
      **    03  EVENT-ACTION              PIC X COMP-X.
       01  EXTVAR-BUF.
           03  LINK-FILE       PIC X(20).
           03  LINK-HEADER     PIC X(80).

       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       01  SCREEN-1.
           03  LABEL LINE 01 COL 02 "VARIABLE NAME"  ,LOW.
           03  LABEL LINE 01 COL 33 "VALUE"  ,LOW.
           03  LABEL LINE 02 COL 02
               "------------------------------"  ,LOW.
           03  LABEL LINE 02 COL 33
               "---------------------------------------------"  ,LOW.
           03  LIST-1, LIST-BOX
               LINE 03, COLUMN 01, SIZE 76,
               LINES LIST-BOX-LINES 
               VALUE SCAN-LINE, ITEM-TO-ADD = ADD-ITEM
               NO-BOX, NO-SEARCH.

       01 SENDMESS-WINDOW.
          03  USING MESS
              LINE 001
              COL 001
              BOLD
              CHARACTER
              PROMPT " "
              NO ECHO.
          03  LINE 001
              COL 062
              "PRESS <ENTER>:"
              BOLD.
       01 SENDMESS-ACCEPT.
          03  USING STAT
              LINE 001
              COL 076
              BOLD
              CHARACTER
              PROMPT " ".


      ******************************************************************
      ******************************************************************

       PROCEDURE DIVISION USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
       MAIN-LOGIC SECTION.
      ******************************************************************
           MOVE FORM-PATHNAME TO FORM-PATH.
      D    STOP MESS.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY "LOADING HEADERS....." AT 002002.
      *    MOVE 0 TO EVENT-TYPE.

      *    -------------------------------------------------------------
      *    SETUP WORK RECORD
      *    -------------------------------------------------------------
           PERFORM GET-EXTVAR-VALUES.
           IF EV-TAB-PTR = 0
              GO TO END-ROUTINE.

      *    -------------------------------------------------------------
      *    OPEN WINDOW
      *    -------------------------------------------------------------
           DISPLAY "                    " AT 002002.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE 2 * VDU-WIN-L-OFFSET
                   AT COL  2 * VDU-WIN-C-OFFSET
                   SIZE 78 + VDU-WIN-W-MOD
                   LINES 22 + VDU-WIN-H-MOD
                   CELL SIZE IS LABEL FONT
                   COLOR IS VDU-WCOLOR-FWIN
                   CONTROL VALUE IS VDU-WCTRL-FWIN
                   BLANK SCREEN
                   TOP CENTERED TITLE IS "EXTERNAL VARIABLES"
                   POP-UP AREA IS EXTVAR-POP-UP-WINDOW.

           DISPLAY SCREEN-1.

      *    -------------------------------------------------------------
      *    LOAD THE FIRST PAGE OF LIST BOX ITEMS
      *    -------------------------------------------------------------
           PERFORM VARYING EV-TAB-PTR FROM 1 BY 1
                           UNTIL EV-TAB-PTR > EV-TAB-MAX
              MOVE EV-NAME(EV-TAB-PTR) TO ADD-NAME
              MOVE EV-VALUE(EV-TAB-PTR) TO ADD-VALUE
              DISPLAY LIST-1
           END-PERFORM.
           MOVE SPACES TO ADD-ITEM SCAN-LINE.

      *    -------------------------------------------------------------
      *    ACTIVATE THE LIST BOX
      *    -------------------------------------------------------------
       SCAN-ACCEPT.
           ACCEPT SCREEN-1
             ON EXCEPTION
                MOVE KEY-STATUS TO VDU-KEY-DEST
                PERFORM VDU-SET-STATUS
                IF STAT = "1>"
                   GO TO END-ROUTINE
                END-IF
      *         IF STAT = "1!"
      *            PERFORM CALL-VI-EDITOR
      *         END-IF
                GO TO SCAN-ACCEPT
           END-ACCEPT.

      *    MOVE LIST-DATA TO SCAN-LINE.
      *    MOVE SCAN-HEADER TO LINK-HEADER.
      *    IF LINK-HEADER = SPACES
      *       GO TO SCAN-ACCEPT.
      *    MOVE "GB/RELNOT" TO LINK-FILE.
      *    MOVE FORM-PATHNAME TO FORM-PATH.
      *    MOVE "WI/RELNO2" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATHNAME RELNOT-BUF EXIT-PATHNAME.
      *    CANCEL FORM-PROGX.
      *    GO TO SCAN-ACCEPT.

           GO TO END-ROUTINE.


      ******************************************************************
      * LOAD INTERNAL TABLE OF EXT VALUES AND DESCRIPTIONS
      ******************************************************************
       GET-EXTVAR-VALUES SECTION.
           MOVE SPACES TO EV-TABLE.

           MOVE 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "BICREA-CALLED-FROM"         TO EV-NAME(EV-TAB-PTR).
           STRING '"'  BICREA-CALLED-FROM '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "EXT-CONAME-BALLOON-LOANS-FG" TO EV-NAME(EV-TAB-PTR).
           STRING '"'  EXT-CONAME-BALLOON-LOANS-FG '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "EXT-CONAME-BRNO-BUF"        TO EV-NAME(EV-TAB-PTR).
           STRING '"'  EXT-CONAME-BRNO-BUF '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "EXT-CONAME-CAN-LOANS-FG"    TO EV-NAME(EV-TAB-PTR).
           STRING '"'  EXT-CONAME-CAN-LOANS-FG '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "EXT-CONAME-CONAME"          TO EV-NAME(EV-TAB-PTR).
           STRING '"'  EXT-CONAME-CONAME '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

      ** I DID NOT DISPLAY DAILY PASSWORD FOR SECURITY REASONS.
      ** IF NEEDED, ACCESS VIA INTERRUPT
      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-CONAME-DAILY-PASSWORD"  TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-CONAME-DAILY-PASSWORD '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-CONAME-EXTRACT-AUTO-UPD" TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-CONAME-EXTRACT-AUTO-UPD '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "EXT-CONAME-NEW-COLL-INQUIRY" TO EV-NAME(EV-TAB-PTR).
           STRING '"'  EXT-CONAME-NEW-COLL-INQUIRY '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "EXT-CONAME-RPT-LINES"       TO EV-NAME(EV-TAB-PTR).
           STRING '"'  EXT-CONAME-RPT-LINES '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "EXT-CONAME-SYSDATE"         TO EV-NAME(EV-TAB-PTR).
           STRING '"'  EXT-CONAME-SYSDATE '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "EXT-CONAME-USER-ID"         TO EV-NAME(EV-TAB-PTR).
           STRING '"'  EXT-CONAME-USER-ID '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "EXT-F12-OPEN"               TO EV-NAME(EV-TAB-PTR).
           STRING '"'  EXT-F12-OPEN '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "EXT-FILPATH"                TO EV-NAME(EV-TAB-PTR).
           STRING '"'  EXT-FILPATH '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "VDU-LABEL-OFFSET"           TO EV-NAME(EV-TAB-PTR).
           STRING '"'  VDU-LABEL-OFFSET '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-LAMENU-APP-NUMBER"      TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-LAMENU-APP-NUMBER '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-LAMENU-CALLING-STAT"    TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-LAMENU-CALLING-STAT '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "EXT-MENU-OPTION-FG"         TO EV-NAME(EV-TAB-PTR).
           STRING '"'  EXT-MENU-OPTION-FG '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

           ADD 1 TO EV-TAB-PTR.
           MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
           MOVE  "EXT-ROUTE-BUF"              TO EV-NAME(EV-TAB-PTR).
           STRING '"'  EXT-ROUTE-BUF '"' DELIMITED BY SIZE
                  INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-LINES"             TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-LINES '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-NAME"              TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-NAME '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-PHYS-SCRN-HEIGHT"  TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-PHYS-SCRN-HEIGHT '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-PHYS-SCRN-WIDTH"   TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-PHYS-SCRN-WIDTH '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-USABLE-SCRN-HEIGHT" TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-USABLE-SCRN-HEIGHT '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-USABLE-SCRN-WIDTH" TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-USABLE-SCRN-WIDTH '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-ATTR-132-MODE"     TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-ATTR-132-MODE '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-ATTR-BLINK"        TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-ATTR-BLINK '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-ATTR-COLOR"        TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-ATTR-COLOR '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-ATTR-DUAL-INTENSITY" TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-ATTR-DUAL-INTENSITY '"'
      *           DELIMITED BY SIZE INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-ATTR-GUI"          TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-ATTR-GUI  '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-ATTR-LINE-DRAW"    TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-ATTR-LINE-DRAW '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-ATTR-PIGGY"        TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-ATTR-PIGGY '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-ATTR-REVERSE"      TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-ATTR-REVERSE '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-ATTR-UNDERLINE"    TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-ATTR-UNDERLINE '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).

      *    ADD 1 TO EV-TAB-PTR.
      *    MOVE SPACES TO EV-VALUE(EV-TAB-PTR).
      *    MOVE  "EXT-TERM-ATTR-VIS-ATTRIB"   TO EV-NAME(EV-TAB-PTR).
      *    STRING '"'  EXT-TERM-ATTR-VIS-ATTRIB '"' DELIMITED BY SIZE
      *           INTO EV-VALUE(EV-TAB-PTR).



           MOVE EV-TAB-PTR TO EV-TAB-MAX.

      *COPY "LIBGB/SCREEN.CPY".
      ******************************************************************
      * CONVERT ACUCOBOL FUNCTION KEYS TO CURRENT PARADATA CODES
      ******************************************************************
       VDU-SET-STATUS SECTION.
      * ENTER
              MOVE "00" TO STAT.
           EVALUATE VDU-KEY-DEST
      * ENTER
              WHEN 13 MOVE "00" TO STAT
      * TAB
      *------>WHEN  9 MOVE "??" TO STAT
      * F1
              WHEN  1 MOVE "10" TO STAT
      * F2
              WHEN  2 MOVE "12" TO STAT
      * F3
              WHEN  3 MOVE "16" TO STAT
      * F4
              WHEN  4 MOVE "18" TO STAT
      * F5
              WHEN  5 MOVE "1:" TO STAT
      * F6
              WHEN  6 MOVE "1<" TO STAT
      * F7
              WHEN  7 MOVE "1>" TO STAT
      * F8
              WHEN  8 MOVE "1 " TO STAT
      * F9
              WHEN  9 MOVE "1(" TO STAT
      * F10
              WHEN 10 MOVE "1)" TO STAT
      * F11
              WHEN 11 MOVE "1!" TO STAT
      * F12
              WHEN 12 MOVE "1#" TO STAT
              WHEN 14 MOVE "^L" TO STAT
              WHEN 15 MOVE "^N" TO STAT
              WHEN 16 MOVE "^B" TO STAT
      * HELP
      *------>WHEN 90 MOVE "??" TO STAT
      * UP ARROW
              WHEN 52 MOVE "1U" TO STAT
      * DOWN ARROW
              WHEN 53 MOVE "1D" TO STAT
      * PAGE UP
              WHEN 67 MOVE "1P" TO STAT
      * PAGE DOWN
              WHEN 68 MOVE "1N" TO STAT
           END-EVALUATE.
           INITIALIZE VDU-KEY-DEST.

       VDU-CONVERT-FIELD SECTION.
       FORM-RESET SECTION.

      *SEND-MESS SECTION.
      *    MOVE "00" TO STAT.
      *    DISPLAY FLOATING GRAPHICAL WINDOW
      *       AT 022002
      *       LINES 01 + VDU-WIN-H-MOD
      *       SIZE 78 + VDU-WIN-W-MOD
      *             HIGHLIGHT
      *       TOP CENTERED TITLE IS "MESSAGE"
      *             BOXED
      *             BACKGROUND-COLOR IS 1
      *       FOREGROUND-COLOR IS 14
      *             WITH NO SCROLL
      *       POP-UP AREA IS SENDMESS-POP-UP-WINDOW.

      *    DISPLAY SENDMESS-WINDOW.
      *    ACCEPT STAT
      *       AT 001076
      *       BOLD
      *       PROMPT " "
      *       NO ECHO.

      *    -------------------------------------------------------------
      *    CLOSE WINDOW AND EXIT PROGRAM
      *    -------------------------------------------------------------
      *    DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
      *    DESTROY SENDMESS-POP-UP-WINDOW.

      *    IF STAT = "~~"
      *       GO TO END-ROUTINE.
      *    MOVE "00" TO STAT.


       END-ROUTINE SECTION.
       EXIT-PROG.
           MOVE "GB/GBMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ALL.
           CLOSE WINDOW EXTVAR-POP-UP-WINDOW.
           EXIT PROGRAM.
