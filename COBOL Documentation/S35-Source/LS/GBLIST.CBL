       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GBLIST.
       AUTHOR.        AMM.
       DATE-WRITTEN.  050617.
      ******************************************************************
      *
      *   IDENTIFICATION:  LS/GBLIST.C
      *
      *   DESCRIPTION   :  GLOBAL FILE LISTING
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 050617 ***NEW***
      * BAH 20190625 REMOVED GB-NEXT-SF-REVOL GB-NEXT-LN-REVOL PT0153
      *
      * JKC 2024-0305 CHANGED USE OF EXECUTION-STAT TO USE ERRCD.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/GBLIST.CBL S35 03/05/24-16:31:05 >".

      ******************************************************************
      *
      *    F I L E - P A T H - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN                      PIC X(02)     VALUE "PR".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  RECS-RSU.
           03  RECS-RSU-READ           PIC 9(11)     VALUE ZEROES.
           03  RECS-RSU-USED           PIC 9(11)     VALUE ZEROES.

       01  START-TIME                  PIC X(08)     VALUE SPACES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-PRU-IN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER PRINT".
           03  FILLER PIC X(30) VALUE "ER OPTIONS                    ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-FILE-KEY-NUMBER.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER FILE ".
           03  FILLER PIC X(30) VALUE "KEY NUMBER                    ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  LEGEND-PROCESSING.
           03  FILLER PIC X(30) VALUE "PROCESSING....................".
           03  FILLER PIC X(30) VALUE "..............................".
           03  FILLER PIC X(19) VALUE "...................".

       01  FILE-KEY-NUMBER         PIC 9(02)     VALUE ZEROES.

       01  ERRCD                   PIC X(01)     VALUE SPACES.

      ******************************************************************
      *
      *    P R I N T - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-COUNT                PIC 9(03)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

      ******************************************************************
      *
      *    H E A D E R - L I N E - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.

           03  H01-DATE                PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  H01-TIME                PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(28)     VALUE SPACES.
           03  H01-NAME                PIC X(40)     VALUE SPACES.
           03  FILLER                  PIC X(37)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  H01-PAGE-NO             PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.

           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H02-USERID              PIC X(10)     VALUE SPACES.
           03  FILLER                  PIC X(33)     VALUE SPACES.
           03  FILLER                  PIC X(28)     VALUE
                            "GLOBAL FILE (KEY 01) LISTING".
           03  FILLER                  PIC X(51)    VALUE SPACES.

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                 PIC X(132)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01              REDEFINES DETAIL-LINE.
           03  D1-PROMPT               PIC X(19).
           03  FILLER                  PIC X.
           03  D1-FIELD                PIC X(30).
           03  FILLER                  REDEFINES D1-FIELD.
               05  D1-PHONE            PIC 999/999/9999.
               05  FILLER              PIC X(18).
           03  FILLER                  REDEFINES D1-FIELD.
               05  D1-CITY             PIC X(15).
               05  FILLER              PIC X(02).
               05  D1-STATE            PIC X(02).
               05  FILLER              PIC X(02).
               05  D1-ZIP              PIC X(05).
               05  FILLER              PIC X(04).
           03  FILLER                  PIC X(30).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02-COLUMN1      REDEFINES DETAIL-LINE.
           03  D2-PROMPT1              PIC X(04).
           03  D2-FIELD1               PIC ZZZ9.
           03  FILLER                  PIC X(03).
           03  D2-PROMPT2              PIC X(04).
           03  D2-FIELD2               PIC ZZZ9.
           03  FILLER                  PIC X(03).
           03  D2-PROMPT3              PIC X(04).
           03  D2-FIELD3               PIC ZZZ9.
           03  FILLER                  PIC X(03).
           03  D2-PROMPT4              PIC X(04).
           03  D2-FIELD4               PIC ZZZ9.
           03  FILLER                  PIC X(03).
      *----DETAIL-LINE-02-COLUMN2.
           03  D2-2-PROMPT             PIC X(25).
           03  FILLER                  PIC X.
           03  D2-2-FIELD              PIC X(10).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03              REDEFINES DETAIL-LINE.
           03  D3-LINES1               PIC X(08).
           03  FILLER                  PIC X.
           03  D3-PROMPT               PIC X(22).
           03  FILLER                  PIC X.
           03  D3-LINES2               PIC X(08).
           03  FILLER                  PIC X(39).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04              REDEFINES DETAIL-LINE.
           03  D4-PROMPT               PIC X(28).
           03  FILLER                  PIC X.
           03  D4-FIELD                PIC ZZZZZZZ9.
           03  FILLER                  PIC X(43).

      *=================================================================
      *
      *    R A N G E - L I N E
      *
      *=================================================================
       01  RANGE-LINE-01               REDEFINES DETAIL-LINE.
           03  R01-PROMPT              PIC X(32).
           03  R01-TEXT                PIC X(48).
           03  R01-AMT                 REDEFINES R01-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  R01-BRNO                REDEFINES R01-TEXT
                                       PIC 9(04).
           03  R01-DATE                REDEFINES R01-TEXT
                                       PIC 9999/99/99.
           03  R01-DLNO                REDEFINES R01-TEXT
                                       PIC 9(04).99.
           03  R01-NUM                 REDEFINES R01-TEXT
                                       PIC ZZZZZZZZZZ9.
           03  R01-SSNO                REDEFINES R01-TEXT
                                       PIC 999/99/9999.
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

       01  GBLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/GBLIST_DEF.CPY".
       COPY "LIBLS/GBLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/GBLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "GBLIST" TO VDU-FORM-NAME.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              MOVE "X" TO ERRCD
              GO TO END-PROGRAM.                                                

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM EXECUTION-MODULE.

       MAIN-PROGRAM-EXIT.

              GO TO END-PROGRAM.


      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
      D    STOP MESS.

           MOVE "X"                        TO ERRCD.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GBLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM ENTER-PARAMETERS.
           IF ( F7-KEY )
              GO TO INITIALIZATION-EXIT.

           MOVE LEGEND-PROCESSING          TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO START-TIME.

           MOVE ZEROES                     TO RECS-RSU.

           PERFORM OPEN-PR-FILE.

       INITIALIZATION-END.

           MOVE SPACES                     TO ERRCD.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

       ENTER-PRU-IN.
           MOVE GBLIST-QUEUE-POS TO Q-POS.
           MOVE GBLIST-COPIES-POS TO C-POS.
           MOVE GBLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           MOVE LEGEND-PRU-IN              TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM PRU-IN.

           IF ( STAT-BAD )
              MOVE VDU---F7-KEY            TO VDUSTATUS
              GO TO ENTER-PARAMETERS-EXIT.

       
       ENTER-FILE-KEY-NUMBER.

           MOVE LEGEND-FILE-KEY-NUMBER     TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "SNNN020FILE-KEY-NO."      TO VDU.
           MOVE 01                         TO VDUNUM0   FILE-KEY-NUMBER.
           PERFORM SCREENIO.

       
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN           TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010ACCEPT."           TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PRU-IN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

           IF ( VDUBUF = "Y" ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( VDUBUF = "N" ) GO TO ENTER-PRU-IN.

           GO TO ENTER-ACCEPT-YN.

       
       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       EXECUTION-MODULE SECTION.
      ******************************************************************

           MOVE GB-PATH-OWNBR   TO GB-BRNO.
           MOVE FILE-KEY-NUMBER TO GB-NO.
           PERFORM OPEN-GB-FILE.

           PERFORM READ-GB-FILE.
           IF ( IO-BAD )
              GO TO EXECUTION-MODULE-END.

           ADD 01 TO RECS-RSU-READ.
           ADD 01 TO RECS-RSU-USED.

           PERFORM PRINT-MODULE.

       EXECUTION-MODULE-END.

           PERFORM CLOSE-GB-FILE.

           IF ( RECS-RSU-USED NOT = ZEROES )
              PERFORM WRITE-RANGE-LINES.

       EXECUTION-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-MODULE SECTION.
      ******************************************************************

           MOVE 99                         TO LN-COUNT.

      * PAGE 01  
           MOVE "1. COMPANY       :" TO D1-PROMPT.
           MOVE GB-COMPANY-NAME      TO D1-FIELD.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "2. ADDRESS1      :" TO D1-PROMPT.
           MOVE GB-ADDR-1            TO D1-FIELD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "3. ADDRESS2      :" TO D1-PROMPT.
           MOVE GB-ADDR-2            TO D1-FIELD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "4. CITY/STATE/ZIP:" TO D1-PROMPT.
           MOVE GB-CITY              TO D1-CITY.
           MOVE GB-STATE             TO D1-STATE.
           MOVE GB-ZIP               TO D1-ZIP.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "5. PHONE NUMBER  :" TO D1-PROMPT.
           MOVE GB-PHONE             TO D1-PHONE.
           INSPECT D1-PHONE REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           MOVE "--------" TO D3-LINES1.
           MOVE "HOLIDAY SCHEDULE (MMDD)" TO D3-PROMPT.
           MOVE "--------" TO D3-LINES2.
           MOVE SPACES TO D2-2-PROMPT D2-2-FIELD.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 6. "         TO D2-PROMPT1.
           MOVE GB-HOLIDAY(01) TO D2-FIELD1.
           MOVE " 7. "         TO D2-PROMPT2.
           MOVE GB-HOLIDAY(02) TO D2-FIELD2.
           MOVE " 8. "         TO D2-PROMPT3.
           MOVE GB-HOLIDAY(03) TO D2-FIELD3.
           MOVE " 9. "         TO D2-PROMPT4.
           MOVE GB-HOLIDAY(04) TO D2-FIELD4.
           MOVE SPACES         TO D2-2-PROMPT D2-2-FIELD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "10. "         TO D2-PROMPT1.
           MOVE GB-HOLIDAY(05) TO D2-FIELD1.
           MOVE "11. "         TO D2-PROMPT2.
           MOVE GB-HOLIDAY(06) TO D2-FIELD2.
           MOVE "12. "         TO D2-PROMPT3.
           MOVE GB-HOLIDAY(07) TO D2-FIELD3.
           MOVE "13. "         TO D2-PROMPT4.
           MOVE GB-HOLIDAY(08) TO D2-FIELD4.
           MOVE "30. MAX REPORT LINES    :" TO D2-2-PROMPT.
           MOVE GB-RPT-LINES                TO D2-2-FIELD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "14. "         TO D2-PROMPT1.
           MOVE GB-HOLIDAY(09) TO D2-FIELD1.
           MOVE "15. " TO D2-PROMPT2.
           MOVE GB-HOLIDAY(10) TO D2-FIELD2.
           MOVE "16. " TO D2-PROMPT3.
           MOVE GB-HOLIDAY(11) TO D2-FIELD3.
           MOVE "17. " TO D2-PROMPT4.
           MOVE GB-HOLIDAY(12) TO D2-FIELD4.
           MOVE "31. CHECK PROGRAM ID    :" TO D2-2-PROMPT.
           MOVE GB-CHECK-PROG               TO D2-2-FIELD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "18. "         TO D2-PROMPT1.
           MOVE GB-HOLIDAY(13) TO D2-FIELD1.
           MOVE "19. "         TO D2-PROMPT2.
           MOVE GB-HOLIDAY(14) TO D2-FIELD2.
           MOVE "20. "         TO D2-PROMPT3.
           MOVE GB-HOLIDAY(15) TO D2-FIELD3.
           MOVE "21. "         TO D2-PROMPT4.
           MOVE GB-HOLIDAY(16) TO D2-FIELD4.
           MOVE "32. OTHR BRANCHES POST  :" TO D2-2-PROMPT.
           MOVE GB-OTHER-BRANCH-POST        TO D2-2-FIELD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "33. AUTOMATIC DEPOSIT   :" TO D2-2-PROMPT.
           MOVE GB-AUTO-DEPOSIT             TO D2-2-FIELD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "22. NEXT S.F.  LOAN    NO. :" TO D4-PROMPT.
           MOVE GB-NEXT-SF                  TO D4-FIELD.
           MOVE "34. N/A                 :" TO D2-2-PROMPT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "23. NEXT SMALL LOAN    NO. :" TO D4-PROMPT.
           MOVE GB-NEXT-SMALL               TO D4-FIELD.
           MOVE "35. N/A                 :" TO D2-2-PROMPT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "24. NEXT OTHER LOAN    NO. :" TO D4-PROMPT.
           MOVE GB-NEXT-OTHER               TO D4-FIELD.
           MOVE "36. HOME OFFICE MACHINE :" TO D2-2-PROMPT.
           MOVE GB-UUCP-HOME                TO D2-2-FIELD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "25. N/A                    :" TO D4-PROMPT.
           MOVE "37. LAST ACCRUAL UPDATE :" TO D2-2-PROMPT.
           MOVE GB-LAST-ACCRUE              TO D2-2-FIELD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "26. N/A                    :" TO D4-PROMPT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "27. N/A                    :" TO D4-PROMPT.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-MODULE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-HEADER-LINES.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD  LN-FEED                    TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.
       
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-HEADER-LINES SECTION.
      ******************************************************************

           ADD  01                         TO PAGE-NUMBER.
           PERFORM GET-TIME.

           MOVE EXT-CONAME-SYSDATE         TO H01-DATE.
           MOVE TIME-EDIT                  TO H01-TIME.
           MOVE EXT-CONAME-CONAME          TO H01-NAME.
           MOVE PAGE-NUMBER                TO H01-PAGE-NO.

           MOVE HEADER-LINE-01             TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE EXT-CONAME-USER-ID         TO H02-USERID.
           MOVE HEADER-LINE-02             TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "-"                    TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 04                         TO LN-COUNT.
           MOVE 01                         TO LN-FEED.

       WRITE-HEADER-LINES-EXIT.
           EXIT.

      ******************************************************************
       WRITE-RANGE-LINES SECTION.
      ******************************************************************

           MOVE ALL "-"                            TO DETAIL-LINE.
           MOVE 05                                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FILE KEY NUMBER               : " TO R01-PROMPT.
           MOVE FILE-KEY-NUMBER                    TO R01-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "STARTING TIME                 : " TO R01-PROMPT.
           MOVE START-TIME                         TO R01-TEXT.
           MOVE 02                                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : " TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                          TO R01-TEXT.
           MOVE 02                                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES                     TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

       WRITE-RANGE-LINES-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO GBLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GBLIST-SYSDATE.
           DISPLAY GBLIST-SCREEN UPON GBLIST-WINDOW-HANDLE.
           MOVE GBLIST-FORM-FIELDS TO WR-BUF.
           MOVE GBLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE GBLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE GBLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/GBLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".

       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       ERROR-FLOW-CONTROL.

           MOVE "ERROR: PROGRAM LOST CONTROL (GBLIST)" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGBRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( ERRCD NOT = SPACES )
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM PRU-OUT.

           IF ( RECS-RSU-USED = ZEROES )
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           MOVE EXIT-PATH                  TO FORM-NAM.
           MOVE FORM-PATH                  TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY GBLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
