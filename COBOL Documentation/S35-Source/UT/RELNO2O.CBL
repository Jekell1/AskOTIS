      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                          RELNO2.
       AUTHOR.                                 MJD.
       DATE-WRITTEN.                    2000/07/14.

      ******************************************************************
      *                       RELNO2 WINDOW
      *
      * CALLED FROM WI/RELNOT.C
      *
      *  DISPLAYS /HELP/GB/RELNOT FILE FOR USE IN RELEASE NOTES.
      *-------------------------------------------------------------------------
      * RELEASE NOTES FILE TOR THE LOAN MANAGER
      *
      * FORMAT:   "#" IN COL 1 DENOTES COMMENT
      *           "\" IN COLUMN 1 DENOTES START OF HEW HEADING.
      * HEADING FORMAT:
      *
      *  \{DATE}\{TYPE}\{TEXT}\
      *
      * {DATE} = DATE OF NOTE IN YYYYMMDD FORMAT.
      * {TYPE} = NOTE TYPE.  "P" PROGRAMMER NOTES; " " GENERAL NOTES.
      * {TEXT} = HEADER, DECRIPTION OF CHANGE, ETC.
      *          NOTE:  ** HEADER CANNOT EXCEED 63 CHARACTERS ***
      *
      * NOTE" ENDING "\" MUST OCCUR PRIOR TO COLUMN 77 (14 TRU 76)
      *
      * BODY FORMAT:
      *        BODY OF TEXT CAN CONSIST OF ANY AND ALL USABLE CHARACTERS AS LONG
      *        AS "#" AND "\" DO NOT APPEAR IN THE FIRST COLUMN.
      *        THE LENGTH OF A EACH LINE IS LIMITED TO 76 CHARACTERS. ANYTHING
      *        LONGER WILL BE TRUNCATED.
      *
      * REV:
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
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                 COMPUTER-NAME.
       OBJECT-COMPUTER.                 COMPUTER-NAME.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *-----------------------------------------------------------------
      * FILE SELECT WORKER FOR /USR/???/HELP/GB/RELNOT
      *-----------------------------------------------------------------
           SELECT RN-FILE ASSIGN TO RN-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  FILE STATUS FILE-STAT.
       DATA DIVISION.
       FILE SECTION.
      *-----------------------------------------------------------------
      * FILE DEFINITION WORKER FOR /USR/???/HELP/GB/RELNOT
      *-----------------------------------------------------------------
       FD  RN-FILE
           LABEL RECORDS ARE STANDARD.
       01  RN-REC.
           03  RN-LINE            PIC X(80).
           03  RN-LINEX  REDEFINES RN-LINE.
               05  RN-CHAR        OCCURS 80
                                  PIC X(01).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/RELNO2 S35 01/10/24-05:31:43 >".

       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLDA END-EXEC.

      *-----------------------------------------------------------------
      * WORKING STORAGE WORKER FOR /USR/???/HELP/GB/RELNOT
      *-----------------------------------------------------------------
       01  RN-PATH.
           03  FILLER             PIC X(01) VALUE "/".
           03  RN-REL             PIC X(07).
           03  FILLER             PIC X(06) VALUE "/HELP/".
           03  RN-FILENAM         PIC X(10).
           03  RN-FILEX  REDEFINES RN-FILENAM.
               05  RN-DIR         PIC X(02).
               05  RN-SLASH       PIC X(01).
               05  RN-PROG        PIC X(06).

       01  RNO2--FILE                   PIC X(10).

      *-----------------------------------------------------------------
      * RNO2 WINDOW WORKERS
      *-----------------------------------------------------------------
       01  RNO2-WINDOW-WORKERS.
           03  RELNO2-POP-UP-WINDOW            PIC X(10).
           03  RNO2-RNO2-LINE-WIDTH               PIC 99.
           03  RNO2-LINES-PER-PAGE           PIC 99.
           03  RNO2-HOLD-COLUMNS             PIC 99.
           03  RNO2-FIELD-INPUT              PIC X(80).
           03  RNO2-FIELD                    PIC X(80).
           03  RNO2-SEQ                      PIC 9(06).
           03  RNO2-FIELD-FOUND              PIC X(01).
           03  RNO2-FG-COLOR                 PIC 9(02).
           03  RNO2-BG-COLOR                 PIC 9(02).
           03  RNO2-WINDOW-TITLE             PIC X(40).

           03  RNO2-TABLE                    PIC X(30400).
           03  RNO2-TABLE-1 REDEFINES RNO2-TABLE OCCURS 400
                                             PIC X(76).
           03  RNO2-SUB                      PIC 9(02).
           03  RNO2-PTR                      PIC 9(06).
           03  RNO2-LIST-SIZE                PIC 9(06).
           03  RNO2-ELEMENT                  PIC X(14).
           03  RNO2-LIST-EMPTY               PIC X(01) VALUE "N".
               88  RNO2-NO-MORE-ELEMENTS        VALUE "Y".
       78  RNO2-QUOTE                       VALUE X'22'.
       01  RNO2-LEGEND1.
           03  FILLER                       PIC X(66) VALUE SPACES.
           03  FILLER                       PIC X(12) VALUE
               X'3C46373E203D204578697420'.
       01  RNO2-LEGEND2.
           03  FILLER                       PIC X(78) VALUE ALL "-".


       COPY "LIBGB/ACUCOBOL_DEF.CPY".
       COPY "LIBGB/ACUGUI_DEF.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  RELNOT-BUF.
           03  RELNOT-FILE       PIC X(20).
           03  RELNOT-HEADER      PIC X(80).

      *-----------------------------------------------------------------
      * SCREEN SECTION
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  RNO2-LEGEND-LINE.
           03  USING RNO2-LEGEND1
               COL 1
               LINE 1.
       01  RNO2-LEGEND-LINE2.
           03  USING RNO2-LEGEND2
               COL 1
               LINE 2.
       01  RNO2-SCREEN-1.
           03  ENTRY-FIELD, LINE 3, COL 1, VALUE MULTIPLE RNO2-TABLE-1,
               LINES 20, SIZE 78 VSCROLL-BAR, READ-ONLY,
               NO-BOX, NO-AUTOSEL, COLOR IS 8.

      ******************************************************************
       PROCEDURE DIVISION USING FORM-PATHNAME RELNOT-BUF EXIT-PATHNAME.
      ******************************************************************
      * D E C L A R A T I V E S.
      ******************************************************************
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON RN-FILE.
       CHECK-ERROR-TYPE.
           MOVE 0 TO IO-FG
           IF FILE-STAT NOT = "00"
              MOVE 9 TO IO-FG
              GO TO END-DECLARE.
       CLOSE-FILES.
           CLOSE RN-FILE.
       DECLARE-STOP.
           PERFORM SQL-DISCONNECT.  *> NEED TO BE LAST BEFORE `STOP RUN`
           STOP RUN.
       END-DECLARE.
           EXIT.
       END DECLARATIVES.

      ******************************************************************
       MAIN-LOGIC SECTION.
      ******************************************************************
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE RELNOT-FILE TO RNO2--FILE.
           MOVE RELNOT-HEADER TO RNO2-FIELD.

           PERFORM CALL-RNO2.
           GO TO END-ROUTINE.


      *****************************************************************
       CALL-RNO2 SECTION.
      *****************************************************************
           MOVE SPACES TO RNO2-TABLE.
      *    MOVE 0 TO EVENT-TYPE.
           ACCEPT RN-REL FROM ENVIRONMENT "REL".
           MOVE RNO2--FILE TO RN-FILENAM.
           MOVE RN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO EXIT-CALL-RNO2.
           PERFORM OPEN-RN-FILE.
           PERFORM RNO2-FIND-FIELD.
           IF RNO2-FIELD-FOUND = "Y"
              PERFORM RNO2-DISPLAY-WINDOW.

       EXIT-CALL-RNO2.
           EXIT.

      ****************************************
       RNO2-FIND-FIELD SECTION.
      ****************************************

      * READ RNO2 TEXT FILE UNTIL CORRECT ENTRY FIELD IS FOUND

           MOVE "N" TO RNO2-FIELD-FOUND.
       RNO2-SCAN-NEXT-LINE.
           MOVE SPACES TO RN-LINE.
           PERFORM READ-RN-FILE.
           IF IO-BAD
              GO TO RNO2-FIND-FIELD-EXIT.

           MOVE 0 TO RNO2-SUB
           INSPECT RN-LINE TALLYING RNO2-SUB FOR ALL "\"
           IF RNO2-SUB = 0
              GO TO RNO2-SCAN-NEXT-LINE.

           MOVE 1 TO RNO2-PTR.
           SET RNO2-LIST-SIZE TO SIZE OF MESS
           MOVE "N" TO RNO2-LIST-EMPTY.
           PERFORM RNO2-PROCESS-ELEMENTS UNTIL RNO2-NO-MORE-ELEMENTS.

           IF RNO2-FIELD-FOUND = "N"
              GO TO RNO2-SCAN-NEXT-LINE.

           MOVE 0 TO RNO2-RNO2-LINE-WIDTH RNO2-LINES-PER-PAGE.
           MOVE 1 TO RNO2-SEQ RNO2-LINES-PER-PAGE.

      * READ BODY OF RNO2 ENTRY UNTIL EOF OR BEG OF NEXT ENTRY

       RNO2-NEXT-LINE.
           MOVE SPACES TO RN-LINE.
           PERFORM READ-RN-FILE.
           IF IO-BAD OR RN-REC(1:1) = "\"
              GO TO RNO2-FIND-FIELD-EXIT.

           IF RN-REC(1:1) = "#"
              GO TO RNO2-NEXT-LINE.

      * CALCULATE LONGEST LINE TO DETERMINE WINDOW WIDTH

           MOVE 0 TO RNO2-SUB.
           INSPECT RN-LINE TALLYING RNO2-SUB FOR TRAILING " ".
           SUBTRACT RNO2-SUB FROM 80 GIVING RNO2-HOLD-COLUMNS
           IF RNO2-HOLD-COLUMNS > RNO2-RNO2-LINE-WIDTH
              MOVE RNO2-HOLD-COLUMNS TO RNO2-RNO2-LINE-WIDTH.

      * WRITE RNO2 LINE TO TABLE FILE FOR QUICK ACCESS

           MOVE RN-REC TO RNO2-TABLE-1(RNO2-SEQ).

           ADD 1 TO RNO2-SEQ RNO2-LINES-PER-PAGE.
           GO TO RNO2-NEXT-LINE.
       RNO2-FIND-FIELD-EXIT.
           EXIT.


      ******************************************************************
       RNO2-DISPLAY-WINDOW SECTION.
      ******************************************************************
           DISPLAY FLOATING GRAPHICAL WINDOW
                   LINES 22 + VDU-WIN-H-MOD
                   SIZE 78 + VDU-WIN-W-MOD
                   TOP CENTERED TITLE IS RELNOT-HEADER
                   FOREGROUND-COLOR IS 14
                   POP-UP AREA IS RELNO2-POP-UP-WINDOW.

           DISPLAY RNO2-LEGEND-LINE.
           DISPLAY RNO2-LEGEND-LINE2.
           DISPLAY RNO2-SCREEN-1.

      * ACTIVATE RNO2 CONTROLS

       RNO2-SCAN-ACCEPT.
           ACCEPT RNO2-SCREEN-1
             ON EXCEPTION
                MOVE KEY-STATUS TO VDU-KEY-DEST
                PERFORM VDU-SET-STATUS
                IF STAT = "1>"
                   GO TO RNO2-DISPLAY-WINDOW-EXIT
                END-IF
                GO TO RNO2-SCAN-ACCEPT
           END-ACCEPT.
           GO TO RNO2-SCAN-ACCEPT.

       RNO2-DISPLAY-WINDOW-EXIT.
           DESTROY RNO2-LEGEND-LINE.
           DESTROY RNO2-LEGEND-LINE2.
           DESTROY RNO2-SCREEN-1.
           CLOSE WINDOW RELNO2-POP-UP-WINDOW.
           DESTROY RELNO2-POP-UP-WINDOW.

      ******************************************************************
       RNO2-PROCESS-ELEMENTS SECTION.
      ******************************************************************
           UNSTRING RN-LINE
                 DELIMITED BY ALL "\"
                 INTO RNO2-FIELD-INPUT
                 POINTER RNO2-PTR
                 ON OVERFLOW
                    IF RNO2-PTR > RNO2-LIST-SIZE
                       MOVE "Y" TO RNO2-LIST-EMPTY
                    END-IF
              PERFORM RNO2-TEST-FIELD
              MOVE SPACES TO MESS
           END-UNSTRING.

      ******************************************************************
       RNO2-TEST-FIELD SECTION.
      ******************************************************************

      * IF NOT THE RIGHT FIELD THEN CONTINUE

           IF RNO2-FIELD-INPUT NOT = RNO2-FIELD
              GO TO RNO2-TEST-FIELD-EXIT.

           MOVE "Y" TO RNO2-FIELD-FOUND
                       RNO2-LIST-EMPTY.

       RNO2-TEST-FIELD-EXIT.
           EXIT.

      ******************************************************************
       VDU-SET-STATUS SECTION.
      ******************************************************************

      * CONVERT ACUCOBOL FUNCTION KEYS TO CURRENT PARADATA CODES

ENTER         MOVE "00" TO STAT.

           EVALUATE VDU-KEY-DEST
ENTER         WHEN 13 MOVE "00" TO STAT
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
^L            WHEN 14 MOVE "^L" TO STAT
^N            WHEN 15 MOVE "^N" TO STAT
^B            WHEN 16 MOVE "^B" TO STAT
UP A          WHEN 52 MOVE "1U" TO STAT
DN A          WHEN 53 MOVE "1D" TO STAT
PG U          WHEN 67 MOVE "1P" TO STAT
PF D          WHEN 68 MOVE "1N" TO STAT
           END-EVALUATE.
           INITIALIZE VDU-KEY-DEST.

      ***************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************

      ***************************************
       SETUP-RELNOT-INFO SECTION.
      ***************************************

      ***************************************
       FORM-RESET SECTION.
      ***************************************

      ***************************************
       MISC-PARAGRAPHS SECTION.
      ***************************************
       COPY "LIBGB/ACCESS.CPY".

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
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
           READ RN-FILE.
           IF IO-FG = 8
              GO TO READ-RN-FILE.
       CLOSE-RN-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE RN-FILE.

      ***************************************
       FILE-IO-ERROR SECTION.
      ***************************************
       IO-ERROR.
           DESTROY RNO2-LEGEND-LINE.
           DESTROY RNO2-LEGEND-LINE2.
           DESTROY RNO2-SCREEN-1.
           CLOSE WINDOW RELNO2-POP-UP-WINDOW.
           DESTROY RELNO2-POP-UP-WINDOW.

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           EXIT PROGRAM.

