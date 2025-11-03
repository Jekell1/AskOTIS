       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GPLS01.
       AUTHOR.        AMM.
       DATE-WRITTEN.  050617.
      ******************************************************************
      *
      *   IDENTIFICATION:  LS/GPLS01.C
      *
      *   DESCRIPTION   :  GLOBAL PARAMETER FILE LISTING
      *
      *=================================================================
      * REV:
      * AMM 050617 {PL#00000} GP FILE LISTING FOR KEY 01
      *            ***NEW***
      * BAH 170427 ACTIVATE 8 DIGIT DATES, PD0006
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
           "@(#)LS/GPLS01.CBL S35 03/05/24-16:33:30 >".

      ******************************************************************
      *
      *    F I L E - P A T H - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
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
           03  FILLER                  PIC X(28)     VALUE SPACES.
           03  FILLER                  PIC X(38)     VALUE
                            "GLOBAL PARAMETER FILE (KEY 01) LISTING".
           03  FILLER                  PIC X(46)    VALUE SPACES.

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                 PIC X(132)    VALUE SPACES.

      *=================================================================
      *
      *    G L O B A L - P A R A M E T E R - P R I N T (GBP)
      *
      *-----------------------------------------------------------------
      *    CPI      : 17 CPI   [PIC X(132)]
      *    FILE NAME: GLOBAL PARAMETER FILE LISTING
      *    FMNO     : 01
      *=================================================================
       01  DETAIL-LINE-01-PG-1         REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(01).
           03  D01-PG1-LAST-DAY        PIC X(27).
           03  FILLER                  PIC X(01).
           03  D01-PG1-DATE            PIC 99/99/99.
           03  FILLER                  PIC X(02).
           03  D01-PG1-YN              PIC X.
           03  FILLER                  PIC X(13).
           03  D01-PG1-COL2-PROMPT     PIC X(22).
           03  D01-PG1-COL2-FIELD      PIC Z(15).
           03  FILLER                  REDEFINES D01-PG1-COL2-FIELD.
               05  FILLER              PIC X(03).
               05  D01-PG1-NOTES       PIC 9(3).
               05  FILLER              PIC X(09).
           03  FILLER                  REDEFINES D01-PG1-COL2-FIELD.
               05  D01-PG1-MONTHS      PIC ZZ9.
               05  FILLER              PIC X(12).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02-PG-1         REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(01).
           03  D02-PG1-LINES           PIC X(36).
           03  FILLER                  PIC X(38).
           03  D02-PG1-BATCH           PIC ZZZ9.
           03  FILLER                  PIC X(11).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03-PG-1         REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(01).
           03  D03-PG1-BOTTOM1         PIC X(23).
           03  FILLER                  PIC X.
           03  D03-PG1-BOTTOM2         PIC 9999/9999999.
           03  FILLER                  PIC X(39).
           03  D03-PG1-NAME            PIC X(03).
           03  FILLER                  PIC X(10).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04-PG-1         REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(53).
           03  D04-PG1-PROMPT2         PIC X(25).
           03  FILLER                  PIC X(01).
           03  D04-PG1-NUM             PIC ZZ9.
           03  FILLER                  PIC X(08).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05-PG-1         REDEFINES DETAIL-LINE.
           03  D05-PAGE-1              PIC X(09).

      *=================================================================
      *
      *    G L O B A L - P A R A M E T E R - P R I N T (GBP)
      *
      *-----------------------------------------------------------------
      *    CPI      : 17 CPI   [PIC X(132)]
      *    FILE NAME: GLOBAL PARAMETER FILE LISTING
      *    FMNO     : 02
      *=================================================================
       01  DETAIL-LINE-01-PG-2         REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(01).
           03  D01-PG2-PROMPT1         PIC X(31).
           03  FILLER                  PIC X(01).
           03  D01-PG2-FIELD1          PIC ZZ9.999.
           03  FILLER                  REDEFINES D01-PG2-FIELD1.
               05  D01-PG2-CO-NUM      PIC Z9.
               05  FILLER              PIC X(05).
           03  FILLER                  PIC X(13).
           03  D01-PG2-PROMPT          PIC X(35).
           03  D01-PG2-FIELD2          PIC X(04).
           03  FILLER                  REDEFINES D01-PG2-FIELD2.
               05  FILLER              PIC X(01).
               05  D01-PG2-YN2         PIC X.
               05  FILLER              PIC X(02).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02-PG-2         REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(33).
           03  D02-PG2-YN1             PIC X.
           03  FILLER                  PIC X(19).
           03  D02-PG2-PROMPT3         PIC X(26).
           03  FILLER                  PIC X(01).
           03  D02-PG2-CLASS1          PIC Z9.
           03  FILLER                  PIC X(01).
           03  D02-PG2-CLASS2          PIC Z9.
           03  FILLER                  PIC X(01).
           03  D02-PG2-CLASS3          PIC Z9.
           03  FILLER                  PIC X(01).
           03  D02-PG2-CLASS4          PIC Z9.
           03  FILLER                  PIC X(01).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03-PG-2         REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(02).
           03  D03-PG2-TEXT            PIC X(35).
           03  FILLER                  PIC X(43).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03-PG-2         REDEFINES DETAIL-LINE.
           03  D03-PAGE-2              PIC X(09).

      *=================================================================
      *
      *    G L O B A L - P A R A M E T E R - P R I N T (GBP)
      *
      *-----------------------------------------------------------------
      *    CPI      : 17 CPI   [PIC X(132)]
      *    FILE NAME: GLOBAL PARAMETER FILE LISTING
      *    FMNO     : 03
      *=================================================================
       01  DETAIL-LINE-01-PG-3         REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(01).
           03  D01-PG3-PROMPT1         PIC X(31).
           03  FILLER                  PIC X(01).
           03  D01-PG3-YN1             PIC X.
           03  FILLER                  PIC X(19).
           03  D01-PG3-PRMPT2          PIC X(35).
           03  FILLER                  PIC X(01).
           03  D01-PG3-YN2             PIC X.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02-PG-3         REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(33).
           03  D02-PG3-TIME            PIC Z9-.
           03  FILLER                  PIC X(17).
           03  D02-PG3-PROMPT3         PIC X(32).
           03  FILLER                  PIC X(01).
           03  D02-PG3-FIELD           PIC 9(04).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03-PG-3         REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(33).
           03  D03-PG3-AMOUNT          PIC ZZ,ZZ9.99.
           03  FILLER                  PIC X(46).
           03  D03-PG3-FIELD2          PIC X(02).
           03  FILLER                  REDEFINES D03-PG3-FIELD2.
               05  D03-PG3-FIELD3      PIC 9(02).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04-PG-3         REDEFINES DETAIL-LINE.
           03  D04-PAGE-3              PIC X(09).

      *=================================================================
      *
      *    G L O B A L - P A R A M E T E R - P R I N T (GBP)
      *
      *-----------------------------------------------------------------
      *    CPI      : 17 CPI   [PIC X(132)]
      *    FILE NAME: GLOBAL PARAMETER FILE LISTING
      *    FMNO     : 04
      *=================================================================
       01  DETAIL-LINE-01-PG-4         REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(01).
           03  D01-PG4-PROMPT1         PIC X(31).
           03  FILLER                  PIC X(01).
           03  D01-PG4-FIELD1          PIC X.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02-PG-4         REDEFINES DETAIL-LINE.
           03  D02-PAGE-4              PIC X(09).

      *=================================================================
      *
      *    R A N G E - L I N E
      *
      *=================================================================
       01  RANGE-LINE-01               REDEFINES DETAIL-LINE.
           03  R01-PROMPT              PIC X(32).
           03  R01-TEXT                PIC X(48).
           03  R01-NUM                 REDEFINES R01-TEXT
                                       PIC ZZZZZZZZZZ9.
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
       01  GPLS01-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/GPLS01_DEF.CPY".
       COPY "LIBLS/GPLS01_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/GPLS01_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-GP-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM EXECUTION-MODULE.

       MAIN-PROGRAM-EXIT.
              GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE "X" TO ERRCD.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "GPLS01" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GPLS01-WINDOW-HANDLE.

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

           MOVE LEGEND-PRU-IN              TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE GPLS01-QUEUE-POS TO Q-POS.
           MOVE GPLS01-COPIES-POS TO C-POS.
           MOVE GPLS01-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
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

           MOVE FILE-KEY-NUMBER            TO GP-KEY.
           PERFORM OPEN-GP-FILE.

       
       EXECUTION-MODULE-NEXT.

           PERFORM READ-GP-FILE.

           IF ( IO-BAD )
              GO TO EXECUTION-MODULE-END.

       
           ADD 01 TO RECS-RSU-READ.
           ADD 01 TO RECS-RSU-USED.

           PERFORM PRINT-MODULE.

       EXECUTION-MODULE-END.

           PERFORM CLOSE-GP-FILE.

           IF ( RECS-RSU-USED NOT = ZEROES )
              PERFORM WRITE-RANGE-LINES.

       EXECUTION-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-MODULE SECTION.
      ******************************************************************

           MOVE 99                            TO LN-COUNT.

      * PAGE 1
       
           MOVE  "{PAGE 01}"                   TO D05-PAGE-1.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 1. FIRST DAY FISCAL YEAR :" TO D01-PG1-LAST-DAY.
           MOVE GP-BEG-FISCAL-DATE            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE "17. BANK ROUTING NO  :"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-BANK-ROUTING-NO            TO D01-PG1-COL2-FIELD.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "18. BANK ACCOUNT NO  :"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-BANK-ACCTNO                TO D01-PG1-COL2-FIELD.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 2. LAST DAY OF PERIOD # 1:" TO D01-PG1-LAST-DAY.
           MOVE GP-END-FISCAL-DATE(1)         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE GP-CLOSED-FISCAL-FG(1)        TO D01-PG1-YN.
           MOVE "19. FEDERAL TAX ID   :"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-FED-TAX-ID                 TO D01-PG1-COL2-FIELD.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 3. LAST DAY OF PERIOD # 2:" TO D01-PG1-LAST-DAY.
           MOVE GP-END-FISCAL-DATE(2)         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE GP-CLOSED-FISCAL-FG(2)        TO D01-PG1-YN.
           MOVE "20. CORPORATE BR/NAME:"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-CORP-BRNO                  TO D02-PG1-BATCH.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 4. LAST DAY OF PERIOD # 3:" TO D01-PG1-LAST-DAY.
           MOVE GP-END-FISCAL-DATE(3)         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE GP-CLOSED-FISCAL-FG(3)        TO D01-PG1-YN.
           MOVE GP-CORP-NAME                  TO D04-PG1-PROMPT2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 5. LAST DAY OF PERIOD # 4:" TO D01-PG1-LAST-DAY.
           MOVE GP-END-FISCAL-DATE(4)         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE GP-CLOSED-FISCAL-FG(4)        TO D01-PG1-YN.
           MOVE "21. SHORT NAME       :"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-SHORT-NAME                 TO D03-PG1-NAME.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE " 6. LAST DAY OF PERIOD # 5:" TO D01-PG1-LAST-DAY.
           MOVE GP-END-FISCAL-DATE(5)         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE GP-CLOSED-FISCAL-FG(5)        TO D01-PG1-YN.
           MOVE "22. BANK NAME        :"      TO D01-PG1-COL2-PROMPT.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 7. LAST DAY OF PERIOD # 6:" TO D01-PG1-LAST-DAY.
           MOVE GP-END-FISCAL-DATE(6)         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE GP-CLOSED-FISCAL-FG(6)        TO D01-PG1-YN.
           MOVE GP-BANK-NAME                  TO D04-PG1-PROMPT2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 8. LAST DAY OF PERIOD # 7:" TO D01-PG1-LAST-DAY.
           MOVE GP-END-FISCAL-DATE(7)         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE GP-CLOSED-FISCAL-FG(7)        TO D01-PG1-YN.
           MOVE "23. RETAIN MONTHS TR :"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-TR-RETAIN-MONTHS           TO D01-PG1-MONTHS.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 9. LAST DAY OF PERIOD # 8:" TO D01-PG1-LAST-DAY.
           MOVE GP-END-FISCAL-DATE(8)         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE GP-CLOSED-FISCAL-FG(8)        TO D01-PG1-YN.
           MOVE "24. RETAIN MONTHS EOD:"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-EOD-RETAIN-MONTHS          TO D01-PG1-MONTHS.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "10. LAST DAY OF PERIOD # 9:" TO D01-PG1-LAST-DAY.
           MOVE GP-END-FISCAL-DATE(9)         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE GP-CLOSED-FISCAL-FG(9)        TO D01-PG1-YN.
           MOVE "25. NEXT NOTE NO.    :"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-NEXT-NOTE                  TO D04-PG1-NUM.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "11. LAST DAY OF PERIOD #10:" TO D01-PG1-LAST-DAY.
           MOVE GP-END-FISCAL-DATE(10)         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE GP-CLOSED-FISCAL-FG(10)       TO D01-PG1-YN.
           MOVE "26. NEXT NOTES BATCH#:"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-NEXT-BATCH-ID              TO D01-PG1-NOTES.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "12. LAST DAY OF PERIOD #11:" TO D01-PG1-LAST-DAY.
           MOVE GP-END-FISCAL-DATE(11)         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE GP-CLOSED-FISCAL-FG(11)       TO D01-PG1-YN.
           MOVE "27. NEXT BP    BATCH#:"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-BP-NEXT-BATCH              TO D02-PG1-BATCH.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "13. LAST DAY OF PERIOD #12:" TO D01-PG1-LAST-DAY.
           MOVE GP-END-FISCAL-DATE(12)         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D01-PG1-DATE.
           MOVE GP-CLOSED-FISCAL-FG(12)       TO D01-PG1-YN.
           MOVE "28. NEXT LBOX  BATCH#:"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-LB-NEXT-BATCH              TO D02-PG1-BATCH.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE ALL "-"                       TO D02-PG1-LINES.
           MOVE "29. NEXT TRUE  BATCH#:"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-TRUE-NEXT-BATCH            TO D02-PG1-BATCH.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "BANK GL #         :"         TO D03-PG1-BOTTOM1.
           MOVE GP-BANK-GL                    TO D03-PG1-BOTTOM2
              INSPECT D03-PG1-BOTTOM2 REPLACING ALL "/" BY "-".
           MOVE "30. NEXT MGRAM BATCH#:"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-MGRAM-NEXT-BATCH           TO D02-PG1-BATCH.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "BANK DEPOSIT GL # :"         TO D03-PG1-BOTTOM1.
           MOVE GP-BANK-DEPOSIT-GL            TO D03-PG1-BOTTOM2.
              INSPECT D03-PG1-BOTTOM2 REPLACING ALL "/" BY "-".
           MOVE "31. NEXT REFCD BATCH#:"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-REFCD-NEXT-BATCH           TO D02-PG1-BATCH.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "BANK CLEARING GL #:"         TO D03-PG1-BOTTOM1.
           MOVE GP-BANK-CLEARING-GL           TO D03-PG1-BOTTOM2.
              INSPECT D03-PG1-BOTTOM2 REPLACING ALL "/" BY "-".
           MOVE "32. NEXT OTREF BATCH#:"      TO D01-PG1-COL2-PROMPT.
           MOVE GP-OTREF-NEXT-BATCH           TO D02-PG1-BATCH.
           PERFORM WRITE-DETAIL-LINE.

      * PAGE 2
       
           MOVE  "{PAGE 02}"                  TO D03-PAGE-2.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 1. AGEING BUCKET CODE        :"  TO D01-PG2-PROMPT1.
           MOVE GP-AGE-BUCKET-CD                   TO D02-PG2-YN1.
           MOVE "18. PRINT REDEEMED ON PROGRESS RPT:" TO D01-PG2-PROMPT.
           MOVE GP-PROGRP-PRINT-REDEEM             TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 2. THE ONLY SALES FIN SRCD   :"  TO D01-PG2-PROMPT1.
           IF GP-THE-ONLY-SF-SRCD NOT = " "
              MOVE GP-THE-ONLY-SF-SRCD             TO D02-PG2-YN1.
           MOVE "19. USE BORROWER HOUSING CODES    :" TO D01-PG2-PROMPT.
           MOVE GP-BW-HOUSING-CODE                 TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           IF GP-THE-ONLY-SF-SRCD NOT = " "
              MOVE "SR" TO CD1-KEY
              MOVE GP-THE-ONLY-SF-SRCD TO CD-CODE
              PERFORM GET-CD
              IF IO-BAD
                 MOVE "****** NOT ON FILE *******" TO CD-DESC
              END-IF
              MOVE CD-DESC                         TO D03-PG2-TEXT.
           MOVE "20. REQUIRE BORROWER CREDIT SCORE :" TO D01-PG2-PROMPT.
           MOVE GP-BW-CREDIT-SCORE                 TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 3. ALLOW BATCH CHECKS        :"  TO D01-PG2-PROMPT1.
           MOVE GP-ALLOW-BATCH-CHECKS              TO D02-PG2-YN1.
           MOVE "21. PROGRP DL CONTRACTUAL 150+    :" TO D01-PG2-PROMPT.
           MOVE GP-PROGRP-DL-CONT                  TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 4. NOT USED                  :"  TO D01-PG2-PROMPT1.
           MOVE "22. ALLOW RESERVE PRIN 1          :" TO D01-PG2-PROMPT.
           MOVE GP-DLRVPRIN-1-FG                   TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 5. ALLOW RESERVE CHRGS 3     :"  TO D01-PG2-PROMPT1.
           MOVE GP-DLRVCHGS-3-FG                   TO D02-PG2-YN1.
           MOVE "23. ALLOW RESERVE PRIN 2          :" TO D01-PG2-PROMPT.
           MOVE GP-DLRVPRIN-2-FG                   TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 6. CASH EARNING METHODS ONLY :"  TO D01-PG2-PROMPT1.
           MOVE GP-CASH-METHOD-ONLY                TO D02-PG2-YN1.
           MOVE "24. ALLOW RESERVE PRIN 3          :" TO D01-PG2-PROMPT.
           MOVE GP-DLRVPRIN-3-FG                   TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 7. IB ACCRUALS ARE DONE      :"  TO D01-PG2-PROMPT1.
           MOVE GP-IB-ACCRUALS-DONE                TO D02-PG2-YN1.
           MOVE "25. ADVANCE FIRST PAYMENT OPTIONS :" TO D01-PG2-PROMPT.
           MOVE GP-ADV-TO-1ST-NO-CORR              TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 8. NO LATE CHRGE IN ARREARAGE:"  TO D01-PG2-PROMPT1.
           MOVE GP-NO-LC-IN-ARREARAGE-FG           TO D02-PG2-YN1.
           MOVE "26. COMPANY OPTIONS FOR XXXX      :" TO D01-PG2-PROMPT.
           MOVE GP-COMPANY-FLAG                    TO D01-PG2-FIELD2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 9. DEFAULT O3 INS. COMPANY NO:"  TO D01-PG2-PROMPT1.
           MOVE GP-DEFAULT-O3-INS-COMPANY          TO D01-PG2-CO-NUM.
           MOVE "27. INTERBR FULL GLNO 1234-567BBBB:" TO D01-PG2-PROMPT.
           MOVE GP-INTERBR-AR-AP-FULLGL            TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "10. STOP ON PRIOR ACCOUNT NO  :"  TO D01-PG2-PROMPT1.
           MOVE GP-STOP-ON-PRIOR-FG                TO D02-PG2-YN1.
           MOVE "28. SKIP UPDATE OF BIFILE         :" TO D01-PG2-PROMPT.
           MOVE GP-SKIP-BI-UPDATE                  TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "11. DEFAULT ACCELERATION RATE :"  TO D01-PG2-PROMPT1.
           MOVE GP-DEFAULT-ACCRATE                 TO D01-PG2-FIELD1.
           MOVE "29. ASK ? FOR HARD COPY MEMOS(PP0):" TO D01-PG2-PROMPT.
           MOVE GP-ASK-HARD-COPY-MEMOS             TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "12. DL DISCOUNT PROMPT/TITLE  :"  TO D01-PG2-PROMPT1.
           MOVE GP-DLR-DISP-TYPE                   TO D02-PG2-YN1.
           MOVE "30. BRING COLLATERAL FROM RENEWAL :" TO D01-PG2-PROMPT.
           MOVE GP-RENEWAL-COLLATERAL            TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "13. AUTO DISPLAY REPO WINDOW  :"  TO D01-PG2-PROMPT1.
           MOVE GP-AUTO-REPO-WINDOW                TO D02-PG2-YN1.
           MOVE "31. PROG RPT LOAN CLASSES:"       TO D02-PG2-PROMPT3.
           MOVE GP-PROGRP-LN-CLASS(1)              TO D02-PG2-CLASS2.
           MOVE GP-PROGRP-LN-CLASS(2)              TO D02-PG2-CLASS3.
           MOVE GP-PROGRP-LN-CLASS(3)              TO D02-PG2-CLASS4.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "14. ALLOW PI ON SE WITH SE BAL:"  TO D01-PG2-PROMPT1.
           MOVE GP-ALLOW-PI-WITH-SE-BAL            TO D02-PG2-YN1.
           MOVE "32. PROG RPT S.F. CLASSES:"       TO D02-PG2-PROMPT3.
           MOVE GP-PROGRP-SF-CLASS(1)              TO D02-PG2-CLASS1.
           MOVE GP-PROGRP-SF-CLASS(2)              TO D02-PG2-CLASS2.
           MOVE GP-PROGRP-SF-CLASS(3)              TO D02-PG2-CLASS3.
           MOVE GP-PROGRP-SF-CLASS(4)              TO D02-PG2-CLASS4.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "15. ALLOW > 1 COLLECTION FORM :"  TO D01-PG2-PROMPT1.
           MOVE GP-ALLOW-MORE-COLLECT              TO D02-PG2-YN1.
           MOVE "33. CARRY OVER DLDISC ON RENEWALS :" TO D01-PG2-PROMPT.
           MOVE GP-RENEW-DISC-UNERN                TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "16. LOAN CONVERSION UPDATE FG :"  TO D01-PG2-PROMPT1.
           MOVE GP-LOAN-CONVERSION-UPD-FG          TO D02-PG2-YN1.
           MOVE "34. NOTES EOD BATCH PROCEDURE ID   " TO D01-PG2-PROMPT.
           MOVE GP-NOTES-EOD-BATCH-ID              TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "17. AUTO (OT)CHG ON FORM PRINT:"  TO D01-PG2-PROMPT1.
           MOVE GP-FORM-CHG-CD                     TO D02-PG2-YN1.
           MOVE "35. VERIFY REFNO ENTRY            :" TO D01-PG2-PROMPT.
           MOVE GP-REFNO-VERIFY                    TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "36. VERIFY ACTION CODE ENTRY      :" TO D01-PG2-PROMPT.
           MOVE GP-ACTIONCD-VERIFY                 TO D01-PG2-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
      * PAGE 3

           MOVE 99                                  TO LN-COUNT.
       
           MOVE  "{PAGE 03}"                  TO D04-PAGE-3.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 1. ACCRUE IB EARN ON REPO'S  :"   TO D01-PG3-PROMPT1.
           MOVE GP-IB-ACCRUE-EARN-ON-REPO           TO D01-PG3-YN1.
           MOVE "19. MEMO FOR RESCHEDULES          :" TO D01-PG3-PRMPT2.
           MOVE GP-RESCHEDULE-MEMO                  TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 2. INCLUDE O2 WITH DELINQ    :"   TO D01-PG3-PROMPT1.
           MOVE GP-DELINQ-WITH-O2                   TO D01-PG3-YN1.
           MOVE "20. DONT COLLECT INT. & LC ON 'SS':" TO D01-PG3-PRMPT2.
           MOVE GP-DONT-COLL-INT-ON-SS              TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 3. ALLOW REPO TRANS ON PL ACT:"   TO D01-PG3-PROMPT1.
           MOVE GP-ALLOW-REPO-ON-PL                 TO D01-PG3-YN1.
           MOVE "21. CREATE FORM REQUESTS ON 'TP'  :" TO D01-PG3-PRMPT2.
           MOVE GP-CREATE-TP-FORMS                  TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 4. BK SCREEN PRIOR TO 'BK'   :"   TO D01-PG3-PROMPT1.
           MOVE GP-BK-PRIOR-TO-POSTING              TO D01-PG3-YN1.
           MOVE "22. ALLOW ENTRY OF DLCOMM PP & 03 :" TO D01-PG3-PRMPT2.
           MOVE GP-DLCOMM-CMSI-FG                   TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 5. AMT CASH PROCEEDS TO DISB.:"   TO D01-PG3-PROMPT1.
           MOVE GP-AMT-CASH-PROCEEDS                TO D03-PG3-AMOUNT.
           MOVE "23. EARLIEST TIME TO CLOSE DAY :"  TO D02-PG3-PROMPT3.
           MOVE GP-EARLIEST-CLOSE-TIME              TO D02-PG3-FIELD.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 6. INCL OTHER WITH EARNED    :"   TO D01-PG3-PROMPT1.
           MOVE GP-INCL-OTH-IN-EARNED               TO D01-PG3-YN1.
           MOVE "24. USE GROUP APPROVAL LIMITS     :" TO D01-PG3-PRMPT2.
           MOVE GP-GROUP-APPROVAL-FG                TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 7. TOGGLE TO DEALER MEMOS Y/N:"   TO D01-PG3-PROMPT1.
           MOVE GP-DL-MEMO-TOGGLE                   TO D01-PG3-YN1.
           MOVE "25. CREATE LOAN APPROVAL MEMO     :" TO D01-PG3-PRMPT2.
           MOVE GP-APPROVAL-MEMO                    TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 8. BASE RN FOR < 103 ON LN   :"   TO D01-PG3-PROMPT1.
           MOVE GP-RN-FOR10-LN                      TO D01-PG3-YN1.
           MOVE "26. DEFAULT O3 INSURANCE OURS FLAG:" TO D01-PG3-PRMPT2.
           MOVE GP-DEFAULT-O3-INSOURS               TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 9. ENTER TERM ON O2-O5 INS.  :"   TO D01-PG3-PROMPT1.
           MOVE GP-OT-INS-TERM                      TO D01-PG3-YN1.
           MOVE "27. USE BW BAD FLAG INDICATORS    :" TO D01-PG3-PRMPT2.
           MOVE GP-USE-BW-BAD-FLAGS                 TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "10. TRACK UNEARNED IN STATS   :"   TO D01-PG3-PROMPT1.
           MOVE GP-UNEARNED-STATS                   TO D01-PG3-YN1.
           MOVE "28. NON-FILE INSURANCE FEE CODE   :" TO D01-PG3-PRMPT2.
           MOVE GP-NON-FILE-FEECODE                 TO D03-PG3-FIELD2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "11. ALLOW RECDEL MTD PAYS NEG?:"   TO D01-PG3-PROMPT1.
           MOVE GP-RECDEL-REVERSAL-FG               TO D01-PG3-YN1.
           MOVE "29. PAST DUE DAYS FOR BW CHANGES  :" TO D01-PG3-PRMPT2.
           MOVE GP-CHBORR-PAST-DUE-DAYS             TO D03-PG3-FIELD3.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "12. ALLOW PL TO ON-HAND REPO  :"   TO D01-PG3-PROMPT1.
           MOVE GP-ALLOW-PL-ON-SX                   TO D01-PG3-YN1.
           MOVE "30. MEMO ON BACKDATE PAY OR PAYOFF:" TO D01-PG3-PRMPT2.
           MOVE GP-BACKDATE-MEMO                    TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "13. CURBAL ONLY ON CPCORP     :"   TO D01-PG3-PROMPT1.
           MOVE GP-CPCORP-CURBAL                    TO D01-PG3-YN1.
           MOVE "31. USE OTHER GLNO FOR REPO UNERN :" TO D01-PG3-PRMPT2.
           MOVE GP-NEW-REPO-UNEARNED-ACCTS          TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "14. TIME ZONE FACTOR          :"   TO D01-PG3-PROMPT1.
           MOVE GP-TIME-ZONE-FACTOR                 TO D02-PG3-TIME.
           MOVE "32. FLAG AS CONFIDENTIAL ON 'BK'  :" TO D01-PG3-PRMPT2.
           MOVE GP-BK-FLAG-CONFIDENTIAL             TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "15. TIME ZONE DAYLIGHT SAVING :"   TO D01-PG3-PROMPT1.
           MOVE GP-TIME-ZONE-DLS                    TO D01-PG3-YN1.
           MOVE "33. AMORIZE AQUISITION FEE        :" TO D01-PG3-PRMPT2.
           MOVE GP-AMORTIZE-ACQUIST-FEE             TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "16. OPEN MEMO WINDOW ON 'RV'  :"   TO D01-PG3-PROMPT1.
           MOVE GP-RV-MEMO                          TO D01-PG3-YN1.
           MOVE "34. DENY FROM REQ IF NO ADDRESS   :" TO D01-PG3-PRMPT2.
           MOVE GP-DENY-FORM-NO-ADDRESS             TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "17. IGNORE LCHG ON TDUE/NXDUE :"   TO D01-PG3-PROMPT1.
           MOVE GP-IGNORE-LCHG-NXDUE                TO D01-PG3-YN1.
           MOVE "35. HIDE FIRST 5 DIGITS OF SSNO   :" TO D01-PG3-PRMPT2.
           MOVE GP-HIDE-FIRST-5-SSNO                TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "18. NO GL ON O2 WHEN P&L      :"   TO D01-PG3-PROMPT1.
           MOVE GP-NO-GL-ON-O2-WHEN-PL              TO D01-PG3-YN1.
           MOVE "36. DEFERMENT (PAID THRU) APPROVAL:" TO D01-PG3-PRMPT2.
           MOVE GP-DEFERMENT-APPROVAL               TO D01-PG3-YN2.
           PERFORM WRITE-DETAIL-LINE.

       
      * PAGE 4
       
           MOVE  "{PAGE 04}"                     TO D02-PAGE-4.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO DETAIL-LINE
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 1. INCLUDE 02 IN BALANCE?    :" TO D01-PG4-PROMPT1.
           MOVE GP-INCLUDE-O2-IN-BAL              TO D01-PG4-FIELD1.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 2. AUTO DISPLAY BNKRPT WINDOW:" TO D01-PG4-PROMPT1.
           MOVE GP-AUTO-BANKRUPT-WINDOW           TO D01-PG4-FIELD1.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 3. BORR. SCORE POINTER(TRIAD):" TO D01-PG4-PROMPT1.
           MOVE GP-BW-SCORES-QUARTER-SUB          TO D01-PG4-FIELD1.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 4. POST 'XX' CONVERSION PMTS :" TO D01-PG4-PROMPT1.
           MOVE GP-POST-XX-BF-PAYMENTS            TO D01-PG4-FIELD1.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE " 5. VALIDATE ALL ENDORSERS AGE:" TO D01-PG4-PROMPT1.
           MOVE GP-VALIDATE-ENDORSER-AGE          TO D01-PG4-FIELD1.
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

       
       WRITE-HEADER-LINES-END.

           MOVE 04                         TO LN-COUNT.
           MOVE 01                         TO LN-FEED.

      *-----------------------------------------------------------------
       WRITE-HEADER-LINES-EXIT.
           EXIT.
      ******************************************************************
      *
      *    W R I T E - R A N G E - L I N E S
      *
      ******************************************************************
       WRITE-RANGE-LINES SECTION.

           MOVE 99                                 TO LN-COUNT.
       
           MOVE "FILE KEY NUMBER               : " TO R01-PROMPT.
           MOVE FILE-KEY-NUMBER                    TO R01-NUM.
           MOVE 01                                 TO LN-FEED.
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

       
       WRITE-RANGE-LINES-END.

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
           MOVE EXT-CONAME-CONAME TO GPLS01-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GPLS01-SYSDATE.
           DISPLAY GPLS01-SCREEN UPON GPLS01-WINDOW-HANDLE.
           MOVE GPLS01-FORM-FIELDS TO WR-BUF.
           MOVE GPLS01-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE GPLS01-HELP--FILE TO HELP-LINK-FILE.
           MOVE GPLS01-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/GPLS01_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       ERROR-FLOW-CONTROL.
           MOVE "ERROR: PROGRAM LOST CONTROL (GPLS01)" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBGPGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGPRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( ERRCD NOT = SPACES )
              GO TO EXIT-PROG.

           PERFORM PRU-OUT.

           IF ( RECS-RSU-USED = ZEROES )
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           MOVE EXIT-PATH                  TO FORM-NAM.
           MOVE FORM-PATH                  TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY GPLS01-WINDOW-HANDLE.
           EXIT PROGRAM.
