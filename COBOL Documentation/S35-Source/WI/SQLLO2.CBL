       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLLO2.
       AUTHOR.        RMZ.
       DATE-WRITTEN.  10-13-2024.
      ******************************************************************
      *
      * IDENTIFICATION:  WI/SQLLO2.CBL (#S35Q-94 & #S35Q-104)
      *
      * DESCRIPTION: THIS PROGRAM READS A BUFFER PASSED FROM PROGRAM 
      *              'WI/SQLLOG.CBL' OF A SELECTED SQLLOG FILE, AND SHOWS     
      *              SELECTED INFORMATION ON TWO DIFFERENT SCREENS.  
      *              IF A SPECIFIC LOG FILE IS SELECTED, THE PROCESS   
      *              WILL BE PASSED TO PROGRAM 'WI/SQLLO3.CBL', WHERE 
      *              ALL OF THE SQLLOG'S DETAIL WILL BE SHOWN. 
      *
      *-----------------------------------------------------------------
      * REV:
      * XXX YYMMDD -    
      *
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                 COMPUTER-NAME.
       OBJECT-COMPUTER.                 COMPUTER-NAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "LIBGB/GBFSSQLLOG.CPY".

           SELECT WK-FILE ASSIGN TO WK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDSQLLOG.CPY".
   
      *    -------------------------------------------------------------
 
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY                      PIC 9(09).
           03  WK-LINE                     PIC X(276).
           03  WK-LINEX  REDEFINES WK-LINE.
               05  WK-DATE-YYYY-MM-DD      PIC 9999/99/99.
               05  FILLER                  PIC X(1).
               05  WK-TIME                 PIC 99/99/99.
               05  FILLER                  PIC X(1).
               05  WK-PROGRAM-NAME         PIC X(40).
               05  FILLER                  PIC X(1).
               05  WK-PROGRAM-RANGES       PIC X(18).
               05  FILLER                  PIC X(1).
               05  WK-STATUS-CODE          PIC X(4).
               05  FILLER                  PIC X(1).
               05  WK-DESCRIPTION          PIC X(150).
               05  FILLER                  PIC X(1).
               05  WK-DB-NAME              PIC X(40).


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/SQLLO2 S35 10/18/24-08:15:41 >".

      * COPY "LIBGB/GB01SQLLOG.CPY".
       COPY "LIBGB/GBWSSQLLOG.CPY".

       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLDA END-EXEC.
	   
      *********************************************************
      *        WK FILE PATH
      *********************************************************
       01  WK-PATH                     PIC X(40).

       78  LIST-BOX-LINES              VALUE 19.
       01  SQLLO2-POP-UP-WINDOW        PIC X(10).
       01  SQLLO2-WINDOW-TITLE         PIC X(30).
       01  SENDMESS-POP-UP-WINDOW      PIC X(10).
       01  HEX-TAB                     PIC X(01)     VALUE X"09".

       COPY "LIBGB/ACUCOBOL_DEF.CPY".
       COPY "LIBGB/ACUGUI_DEF.CPY".

       77  NUMBER-READS-NEEDED         PIC 99.

      * STATE-FLAG TRACKS WHERE YOU ARE IN THE FILE.
      * READING-FORWARDS INDICATES THAT THE LAST READ WAS
      * OF THE LAST RECORD SHOWN IN THE BOX.
      * READING-BACKWARDS INDICATES THAT THE LAST READ
      * WAS OF THE FIRST RECORD IN THE BOX (VIA READ
      * PREVIOUS).  AT-START AND AT-END HANDLE SPECIAL
      * CASES WHEN YOU REACH EITHER END OF THE FILE.

       77  STATE-FLAG                  PIC X.
           88  READING-FORWARDS        VALUE "F".
           88  READING-BACKWARDS       VALUE "B".
           88  AT-START                VALUE "S".
           88  AT-END                  VALUE "E".

       77  LIST-DATA                   PIC X(78).

      *    VI +/\"\FIELD"  /$REL/HELP/
       01  HELP-CALL-VI.
           03  FILLER                  PIC X(6)  VALUE
               X"7669202B2F22".        
           03  HELP-CALL-VI-FIELD      PIC X(77) VALUE " ".
           03  HELP-CALL-VI-FILE.      
               05  FILLER              PIC X(11) VALUE
                   X"2F2452454C2F48454C502F".
               05  HELP-NAM-VI         PIC X(9)  VALUE " ".

      *    /$REL/SH/HELP.SH
       01  HELP-CALL-SH.
           03  FILLER                  PIC X(17) VALUE
               X"2F2452454C2F53482F68656C702E736820".
           03  HELP-NAM-SH             PIC X(9)  VALUE " ".
  
       01  HEADER-COUNT                PIC 9(09).
       01  CURRENT-STAT                PIC X(02).
   
       01  NOTE-TYPE                   PIC X(01).
           88 NOTE-TYPE-PROGRAMMER     VALUE "P" X'70'.

       01  SCAN-LINE.
           03  SCAN-HEADER                       PIC X(78). 
           03  SCAN-HEADERX REDEFINES SCAN-HEADER.
               05  FILLER                        PIC X(01).
               05  SCAN-DATE-YYYY-MM-DD          PIC X(10).
               05  FILLER                        PIC X(01).   
               05  SCAN-TIME                     PIC X(8).
               05  FILLER                        PIC X(01). 
               05  SCAN-STATUS-CODE              PIC X(4).
               05  FILLER                        PIC X(1).
               05  SCAN-PROGRAM-NAME             PIC X(33).
               05  FILLER                        PIC X(1).
               05  SCAN-PROGRAM-RANGES           PIC X(17).



       01  SCAN2-LINE.
           03  SCAN2-HEADER                      PIC X(78). 
           03  SCAN2-HEADERX REDEFINES SCAN2-HEADER.
               05  FILLER                        PIC X(01).
               05  SCAN2-DATE-YYYY-MM-DD         PIC X(10).
               05  FILLER                        PIC X(01).   
               05  SCAN2-TIME                    PIC X(08).
               05  FILLER                        PIC X(01).
               05  SCAN2-STATUS-CODE             PIC X(04).
               05  FILLER                        PIC X(01). 
               05  SCAN2-DESCRIPTION             PIC X(51).
               05  FILLER                        PIC X(01).
 
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

       01  COLUMN-TITLE                  PIC X(25) VALUE
           "SQLLOG FILE ENTRIES      ".
       01  COLUMN-HEADINGS               PIC X(19) VALUE
           "   DIRECTORIES:    ".
       01  COLUMN-HEADINGS2.
           03  FILLER                    PIC X(13) VALUE
           "-------------".
           03  FILLER                    PIC X(30) VALUE
           "------------------------------".
           03  FILLER                    PIC X(33) VALUE
           "---------------------------------".


       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETFMW.CPY".

       01  SQLLOG-BUF.
           03  LINK-FILE       PIC X(20).
           03  LINK-HEADER     PIC X(40).  
           03  LINK-LINE       PIC X(500).   

       SCREEN SECTION.
       01  SCREEN-1.
           03  LABEL LINE 01 COL 68 'F5=DISPLAY'     ,LOW.
           03  LABEL LINE 02 COL 02 'DATE      '     ,UNDERLINE.
           03  LABEL LINE 02 COL 13 'TIME    '       ,UNDERLINE.
           03  LABEL LINE 02 COL 22 'STAT'           ,UNDERLINE.
           03  LABEL LINE 02 COL 27 'PROGRAM NAME'   ,UNDERLINE.
           03  LABEL LINE 02 COL 39 
               '                     '               ,UNDERLINE.
           03  LABEL LINE 02 COL 61 'RANGES         ',UNDERLINE.
           03  LABEL LINE 02 COL 71 
               '       '                             ,UNDERLINE.
 

 
           03  LIST-1, LIST-BOX USING LIST-DATA,
               LINE 04, COLUMN 01, SIZE 78,
      *        LINES LIST-BOX-LINES + VDU-WIN-H-MOD
               LINES LIST-BOX-LINES 
               PAGED, NO-BOX, NO-SEARCH, COLOR IS 8
               EXCEPTION PROCEDURE IS LIST-1-HANDLER.

       01  SCREEN-2.
           03  LABEL LINE 01 COL 68 'F5=DISPLAY'      ,LOW.
           03  LABEL LINE 02 COL 02 'DATE      '      ,UNDERLINE.
           03  LABEL LINE 02 COL 13 'TIME    '        ,UNDERLINE.
           03  LABEL LINE 02 COL 22 'STAT'            ,UNDERLINE.
           03  LABEL LINE 02 COL 27 'DESCRIPTION    ' ,UNDERLINE.
           03  LABEL LINE 02 COL 41 
               '                                   '  ,UNDERLINE.
           03  LABEL LINE 02 COL 73 '     '           ,UNDERLINE.

 
           03  LIST-2, LIST-BOX USING LIST-DATA,
               LINE 04, COLUMN 01, SIZE 78,
      *        LINES LIST-BOX-LINES + VDU-WIN-H-MOD
               LINES LIST-BOX-LINES 
               PAGED, NO-BOX, NO-SEARCH, COLOR IS 8
               EXCEPTION PROCEDURE IS LIST-2-HANDLER.

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


       PROCEDURE DIVISION USING FORM-PATHNAME SQLLOG-BUF EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON SQLLOG-FILE WK-FILE.
       CHECK-ERROR-TYPE.
           MOVE 0 TO IO-FG
           IF FILE-STAT NOT = "00"
              MOVE 9 TO IO-FG
              GO TO END-DECLARE.
       CLOSE-FILES.
           CLOSE SQLLOG-FILE WK-FILE.
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
           MOVE LINK-HEADER   TO SQLLOG-PATH.

      D    STOP MESS.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           MOVE 0 TO EVENT-TYPE.
           SET READING-FORWARDS TO TRUE.
           MOVE '  '            TO CURRENT-STAT.

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

           MOVE ZEROES TO WK-KEY.
           PERFORM COUNT-SQLLOG-RECORDS.
           IF HEADER-COUNT = 0
              GO TO END-ROUTINE.

      ******************************************************************
      * SCREEN-1 - SHOW SQLLOG INFORMATION.
      ******************************************************************
       SCREEN-1-MAIN-PROCESS SECTION. 

           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

      * DESTROY CURRENT SCREEN
 
           IF STAT NOT = "  "
              PERFORM DESTROY-SCREEN. 

      * OPEN WINDOW

           MOVE COLUMN-TITLE TO SQLLO2-WINDOW-TITLE.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 78 + VDU-WIN-W-MOD
                   LINES 22 + VDU-WIN-H-MOD
      *            BLANK SCREEN
                   TOP CENTERED TITLE IS SQLLO2-WINDOW-TITLE
                   POP-UP AREA IS SQLLO2-POP-UP-WINDOW. 
        
           DISPLAY SCREEN-1. 

      * LOAD THE FIRST PAGE OF LIST BOX ITEMS

           MODIFY LIST-1, MASS-UPDATE = 1
           PERFORM LIST-BOX-LINES TIMES
              PERFORM READ-WK1-FILE-NEXT
              IF IO-BAD OR IO-AT-END
                  SET AT-END TO TRUE
                  EXIT PERFORM
              END-IF
              MOVE WK-DATE-YYYY-MM-DD   TO SCAN-DATE-YYYY-MM-DD           
                                           SCAN2-DATE-YYYY-MM-DD
              MOVE WK-TIME              TO SCAN-TIME
                                           SCAN2-TIME
              MOVE WK-PROGRAM-NAME      TO SCAN-PROGRAM-NAME
              MOVE WK-PROGRAM-RANGES    TO SCAN-PROGRAM-RANGES
              MOVE WK-STATUS-CODE       TO SCAN-STATUS-CODE
                                           SCAN2-STATUS-CODE
              MOVE WK-DESCRIPTION       TO SCAN2-DESCRIPTION
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
                MOVE STAT TO CURRENT-STAT
                IF STAT = "1>"
                   GO TO END-ROUTINE
                END-IF
                IF STAT = "1:"
                   GO TO SCREEN-2-MAIN-PROCESS 
                END-IF
                GO TO SCAN-ACCEPT
           END-ACCEPT.

           MOVE LIST-DATA TO SCAN-LINE.
           MOVE SCAN-HEADER TO LINK-HEADER.
           IF LINK-HEADER = SPACES
              GO TO SCAN-ACCEPT.

           MOVE ZEROES TO WK-KEY.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.


       READ-WK1-NEXT-LOOP. 
           PERFORM READ-WK1-FILE-NEXT
           IF IO-BAD OR IO-AT-END
              GO TO END-ROUTINE.

           IF SCAN-DATE-YYYY-MM-DD    = WK-DATE-YYYY-MM-DD      AND 
              SCAN-TIME               = WK-TIME                 AND
              SCAN-PROGRAM-NAME       = WK-PROGRAM-NAME(1:33)   AND 
              SCAN-PROGRAM-RANGES     = WK-PROGRAM-RANGES(1:17) AND
              SCAN-STATUS-CODE        = WK-STATUS-CODE      
              MOVE WK-LINE           TO LINK-LINE
           ELSE
              GO TO READ-WK1-NEXT-LOOP. 

           MOVE "WI/SQLLO2"   TO LINK-FILE.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WI/SQLLO3"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME SQLLOG-BUF EXIT-PATHNAME.
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
                  MOVE LOW-VALUES TO WK-KEY        
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
                  MOVE HIGH-VALUES TO WK-KEY       
                  PERFORM START-WK1-FILE-NOT-GREATER
                  IF IO-INVALID-KEY
                     EXIT PARAGRAPH
                  END-IF
                  SET READING-BACKWARDS TO TRUE
                  MODIFY LIST-1, MASS-UPDATE = 1
                  MODIFY LIST-1, RESET-LIST  = 1
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
                          SEARCH-TEXT IN WK-TIME      
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
                MOVE LOW-VALUES TO WK-KEY      
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

             MOVE WK-DATE-YYYY-MM-DD   TO SCAN-DATE-YYYY-MM-DD           
                                          SCAN2-DATE-YYYY-MM-DD.
             MOVE WK-TIME              TO SCAN-TIME
                                          SCAN2-TIME.
             MOVE WK-PROGRAM-NAME      TO SCAN-PROGRAM-NAME.
             MOVE WK-PROGRAM-RANGES    TO SCAN-PROGRAM-RANGES.
             MOVE WK-STATUS-CODE       TO SCAN-STATUS-CODE
                                          SCAN2-STATUS-CODE.
             MOVE WK-DESCRIPTION       TO SCAN2-DESCRIPTION.

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
                    MOVE HIGH-VALUES TO WK-KEY       
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

            MOVE WK-DATE-YYYY-MM-DD   TO SCAN-DATE-YYYY-MM-DD           
                                         SCAN2-DATE-YYYY-MM-DD.
            MOVE WK-TIME              TO SCAN-TIME
                                         SCAN2-TIME.
            MOVE WK-PROGRAM-NAME      TO SCAN-PROGRAM-NAME.
            MOVE WK-PROGRAM-RANGES    TO SCAN-PROGRAM-RANGES.
            MOVE WK-STATUS-CODE       TO SCAN-STATUS-CODE
                                         SCAN2-STATUS-CODE.
            MOVE WK-DESCRIPTION       TO SCAN2-DESCRIPTION.

            MODIFY LIST-1
               INSERTION-INDEX = 1
               ITEM-TO-ADD = SCAN-LINE
            SET READING-BACKWARDS TO TRUE.


      ******************************************************************
      * SCREEN-2 - SHOW DATE/TIME/DESCRIPTION IFORMATION.
      ******************************************************************
       SCREEN-2-MAIN-PROCESS SECTION. 

           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

      * DESTROY CURRENT SCREEN
 
           PERFORM DESTROY-SCREEN. 

      * OPEN WINDOW

           MOVE COLUMN-TITLE TO SQLLO2-WINDOW-TITLE.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 78 + VDU-WIN-W-MOD
                   LINES 22 + VDU-WIN-H-MOD
      *            BLANK SCREEN
                   TOP CENTERED TITLE IS SQLLO2-WINDOW-TITLE
                   POP-UP AREA IS SQLLO2-POP-UP-WINDOW. 
        
           DISPLAY SCREEN-2. 

      * LOAD THE FIRST PAGE OF LIST BOX ITEMS

           MODIFY LIST-2, MASS-UPDATE = 1
           PERFORM LIST-BOX-LINES TIMES
              PERFORM READ-WK1-FILE-NEXT
              IF IO-BAD OR IO-AT-END
                  SET AT-END TO TRUE
                  EXIT PERFORM
              END-IF
              MOVE WK-DATE-YYYY-MM-DD   TO SCAN-DATE-YYYY-MM-DD           
                                           SCAN2-DATE-YYYY-MM-DD
              MOVE WK-TIME              TO SCAN-TIME
                                           SCAN2-TIME
              MOVE WK-PROGRAM-NAME      TO SCAN-PROGRAM-NAME
              MOVE WK-PROGRAM-RANGES    TO SCAN-PROGRAM-RANGES
              MOVE WK-STATUS-CODE       TO SCAN-STATUS-CODE
                                           SCAN2-STATUS-CODE
              MOVE WK-DESCRIPTION       TO SCAN2-DESCRIPTION
              MODIFY LIST-2,
                     ITEM-TO-ADD = SCAN2-LINE
           END-PERFORM

           MODIFY LIST-2, MASS-UPDATE = 0.

      * ACTIVATE THE LIST BOX

       SCAN2-ACCEPT.

           ACCEPT SCREEN-2
             ON EXCEPTION
                MOVE KEY-STATUS TO VDU-KEY-DEST
                PERFORM VDU-SET-STATUS
                MOVE STAT TO CURRENT-STAT
                IF STAT = "1>"
                   GO TO END-ROUTINE
                END-IF
                IF STAT = "1:"
                   GO TO SCREEN-1-MAIN-PROCESS 
                END-IF
                GO TO SCAN2-ACCEPT
           END-ACCEPT.

           MOVE LIST-DATA TO SCAN2-LINE.
           MOVE SCAN2-HEADER TO LINK-HEADER.
           IF LINK-HEADER = SPACES
              GO TO SCAN2-ACCEPT.

           MOVE ZEROES TO WK-KEY.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.


       READ2-WK1-NEXT-LOOP. 
           PERFORM READ-WK1-FILE-NEXT
           IF IO-BAD OR IO-AT-END
              GO TO END-ROUTINE.

           IF SCAN2-DATE-YYYY-MM-DD   = WK-DATE-YYYY-MM-DD   AND 
              SCAN2-TIME              = WK-TIME              AND
              SCAN2-DESCRIPTION       = WK-DESCRIPTION(1:51) 
              MOVE WK-LINE           TO LINK-LINE  
           ELSE
              GO TO READ2-WK1-NEXT-LOOP. 

           MOVE "WI/SQLLO2"   TO LINK-FILE.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WI/SQLLO3"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME SQLLOG-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           GO TO SCAN2-ACCEPT.

           GO TO END-ROUTINE.


      ******************************************************************
      * LIST-2-HANDLER HANDLES ALL EXCEPTIONS GENERATED
      * BY THE LIST BOX.  THE ONLY EXCEPTIONS YOU CARE
      * ABOUT ARE THOSE THAT REQUIRE A RESPONSE IN
      * ORDER TO MANAGE THE LIST BOX PROPERLY.
      ******************************************************************
       LIST-2-HANDLER SECTION.
           IF KEY-STATUS = W-EVENT
              EVALUATE EVENT-TYPE
                WHEN NTF-PL-NEXT
                  PERFORM GET2-NEXT-ITEM

                WHEN NTF-PL-PREV
                  PERFORM GET2-PREV-ITEM

                WHEN NTF-PL-NEXTPAGE
                  MODIFY LIST-2, MASS-UPDATE = 1
                  PERFORM GET2-NEXT-ITEM LIST-BOX-LINES TIMES
                  MODIFY LIST-2, MASS-UPDATE = 0

                WHEN NTF-PL-PREVPAGE
                  MODIFY LIST-2, MASS-UPDATE = 1
                  PERFORM GET2-PREV-ITEM LIST-BOX-LINES TIMES
                  MODIFY LIST-2, MASS-UPDATE = 0

                WHEN NTF-PL-FIRST
                  MOVE LOW-VALUES TO WK-KEY        
                  PERFORM START-WK1-FILE-NOT-LESS-THAN
                  IF IO-INVALID-KEY
                     EXIT PARAGRAPH
                  END-IF
                  SET READING-FORWARDS TO TRUE
                  MODIFY LIST-2, MASS-UPDATE = 1
                  MODIFY LIST-2, RESET-LIST = 1
                  PERFORM GET2-NEXT-ITEM LIST-BOX-LINES TIMES
                  MODIFY LIST-2, MASS-UPDATE = 0

                WHEN NTF-PL-LAST
                  MOVE HIGH-VALUES TO WK-KEY       
                  PERFORM START-WK1-FILE-NOT-GREATER
                  IF IO-INVALID-KEY
                     EXIT PARAGRAPH
                  END-IF
                  SET READING-BACKWARDS TO TRUE
                  MODIFY LIST-2, MASS-UPDATE = 1
                  MODIFY LIST-2, RESET-LIST  = 1
                  PERFORM GET2-PREV-ITEM LIST-BOX-LINES TIMES
                  MODIFY LIST-2, MASS-UPDATE = 0

      ******************************************************************
      * IN THE SEARCH LOGIC, IF YOU GET2 TOO CLOSE TO THE
      * END OF THE FILE, YOU SIMPLY ACT AS IF YOU WANTED TO
      * FIND THE LAST FULL PAGE.  DO THIS BY SETTING THE
      * EVENT TYPE TO NTF-PL-LAST AND RE-EVALUATING.
      ******************************************************************

                WHEN NTF-PL-SEARCH
                  INQUIRE LIST-2,
                          SEARCH-TEXT IN WK-TIME      
                  PERFORM START-WK1-FILE-NOT-LESS-THAN
                  IF IO-INVALID-KEY
                     MOVE NTF-PL-LAST TO EVENT-TYPE
                     GO TO LIST-2-HANDLER
                  END-IF
                  SET READING-FORWARDS TO TRUE
                  MODIFY LIST-2, MASS-UPDATE = 1
                  PERFORM GET2-NEXT-ITEM LIST-BOX-LINES TIMES
                  IF AT-END
                    MOVE NTF-PL-LAST TO EVENT-TYPE
                    GO TO LIST-2-HANDLER
                  END-IF
                  MODIFY LIST-2, MASS-UPDATE = 0
              END-EVALUATE
            END-IF.

      ******************************************************************
      * GET2-NEXT-ITEM HANDLES ALL CASES WHERE YOU READ
      * FORWARDS THROUGH THE FILE.  IT ADJUSTS FOR THE
      * FOUR POSSIBLE STATES YOU COULD BE IN AND THEN
      * RETRIEVES THE NEXT RECORD.  THIS RECORD IS ADDED
      * TO THE END OF THE LIST BOX.  IN SOME CASES, YOU
      * HAVE TO TRAVERSE OVER THE PAGE OF RECORDS
      * CURRENTLY DISPLAYED (BECAUSE YOU ARE SWITCHING
      * DIRECTION).
      ******************************************************************
       GET2-NEXT-ITEM SECTION.
           EVALUATE TRUE
             WHEN AT-START
                MOVE LOW-VALUES TO WK-KEY      
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

             MOVE WK-DATE-YYYY-MM-DD   TO SCAN-DATE-YYYY-MM-DD           
                                          SCAN2-DATE-YYYY-MM-DD.
             MOVE WK-TIME              TO SCAN-TIME
                                          SCAN2-TIME.
             MOVE WK-PROGRAM-NAME      TO SCAN-PROGRAM-NAME.
             MOVE WK-PROGRAM-RANGES    TO SCAN-PROGRAM-RANGES.
             MOVE WK-STATUS-CODE       TO SCAN-STATUS-CODE
                                          SCAN2-STATUS-CODE.
             MOVE WK-DESCRIPTION       TO SCAN2-DESCRIPTION.

             MODIFY LIST-2,
                    ITEM-TO-ADD = SCAN2-LINE
             SET READING-FORWARDS TO TRUE.

      ******************************************************************
      * GET2-PREV-ITEM IS THE CONVERSE OF GET2-NEXT-ITEM.
      * IT RETRIEVES THE PREVIOUS RECORD IN THE LIST AND
      * ADDS IT TO THE TOP OF THE LIST BOX.  THE CODE IS
      * STRUCTURED IDENTICALLY TO GET2-NEXT-ITEM.
      ******************************************************************
       GET2-PREV-ITEM SECTION.
               EVALUATE TRUE
                 WHEN AT-END
                    MOVE HIGH-VALUES TO WK-KEY       
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

            MOVE WK-DATE-YYYY-MM-DD   TO SCAN-DATE-YYYY-MM-DD           
                                         SCAN2-DATE-YYYY-MM-DD.
            MOVE WK-TIME              TO SCAN-TIME
                                         SCAN2-TIME.
            MOVE WK-PROGRAM-NAME      TO SCAN-PROGRAM-NAME.
            MOVE WK-PROGRAM-RANGES    TO SCAN-PROGRAM-RANGES.
            MOVE WK-STATUS-CODE       TO SCAN-STATUS-CODE
                                         SCAN2-STATUS-CODE.
            MOVE WK-DESCRIPTION       TO SCAN2-DESCRIPTION.

            MODIFY LIST-2
               INSERTION-INDEX = 1
               ITEM-TO-ADD = SCAN2-LINE
            SET READING-BACKWARDS TO TRUE.


      ******************************************************************
      *  READ SQLLO2 FILE - VERIFY ENTRIES
      ******************************************************************
       COUNT-SQLLOG-RECORDS SECTION.
           MOVE 0 TO HEADER-COUNT.
           PERFORM OPEN-SQLLOG-FILE.
      *    MOVE SPACES TO SQLLOG-REC.
      *    PERFORM READ-SQLLOG-FILE.

       NEXT-SQLLOG.
           MOVE SPACES TO SQLLOG-REC.
           PERFORM READ-SQLLOG-FILE.
           IF IO-BAD
              GO TO COUNT-SQLLOG-RECORDS-EXIT.
 
      *    INSPECT SQLLOG-REC REPLACING ALL HEX-TAB BY "".
           MOVE SQLLOG-REC TO WK-LINE.
           ADD 1           TO WK-KEY. 
           PERFORM WRITE-WK1-FILE.
           IF IO-FG NOT = 0
              GO TO COUNT-SQLLOG-RECORDS-EXIT.

           ADD 1 TO HEADER-COUNT.
           GO    TO NEXT-SQLLOG.
       COUNT-SQLLOG-RECORDS-EXIT.
           PERFORM CLOSE-SQLLOG-FILE.


       COPY "LIBGB/DATER.CPY".

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

      ******************************************************************
      * DESTROY SCREENS WHEN GOING BETWEEN PF1 & PF2
      ******************************************************************
       DESTROY-SCREEN SECTION.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SCREEN-1.
           DESTROY SCREEN-2.
           CLOSE WINDOW SQLLO2-POP-UP-WINDOW.

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
       OPEN-SQLLOG-FILE.
           MOVE 0       TO IO-FG IO-STATUS.
           MOVE SQLLOG-PATH TO E-FILE.
           OPEN INPUT SQLLOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-SQLLOG-FILE.
           IF IO-FG = 7
              CLOSE SQLLOG-FILE
              GO TO OPEN-SQLLOG-FILE.
       READ-SQLLOG-FILE.
           MOVE 0             TO IO-FG IO-STATUS.
           MOVE SQLLOG-PATH       TO E-FILE.
           READ SQLLOG-FILE
                AT END MOVE 9 TO IO-STATUS
           END-READ.
           IF IO-FG = 8
              GO TO READ-SQLLOG-FILE.
       CLOSE-SQLLOG-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE SQLLOG-FILE.

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
              GO TO OPEN-WK1-FILE-OUTPUT.
       READ-WK1-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           READ WK-FILE.
           IF IO-FG = 8
              GO TO READ-WK1-FILE.
       READ-WK1-FILE-NEXT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           READ WK-FILE NEXT
              AT END    MOVE 9 TO IO-STATUS
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK1-FILE-NEXT.
       READ-WK1-FILE-PREVIOUS.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           READ WK-FILE PREVIOUS
              AT END    MOVE 9 TO IO-STATUS
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK1-FILE-PREVIOUS.
       START-WK1-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           START WK-FILE KEY NOT < WK-KEY
              INVALID KEY
                 MOVE 9 TO IO-STATUS
           END-START.
       START-WK1-FILE-GREATER.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           START WK-FILE KEY > WK-KEY
              INVALID KEY
                 MOVE 9 TO IO-STATUS
           END-START.
           UNLOCK WK-FILE.
       START-WK1-FILE-NOT-GREATER.
      *--->PERFORM START-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           START WK-FILE KEY NOT > WK-KEY
              INVALID KEY
                 MOVE 9 TO IO-STATUS
           END-START.
           UNLOCK WK-FILE.
       START-WK1-FILE-NOT-LESS-THAN.
      *--->PERFORM START-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           START WK-FILE KEY NOT < WK-KEY
              INVALID KEY
                 MOVE 9 TO IO-STATUS
           END-START.
           UNLOCK WK-FILE.
       WRITE-WK1-FILE.
      *--->PERFORM WRITE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           WRITE WK-REC.
           IF IO-FG = 8
             GO TO WRITE-WK1-FILE.
           UNLOCK WK-FILE.
       REWRITE-WK1-FILE.
      *--->PERFORM REWRITE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
             GO TO REWRITE-WK1-FILE.
           UNLOCK WK-FILE.
       DELETE-WK1-FILE.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
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
           MOVE "WI/SQLLOG" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ALL.
           CLOSE WINDOW SQLLO2-POP-UP-WINDOW.
           EXIT PROGRAM.
