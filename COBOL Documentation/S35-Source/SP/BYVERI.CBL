       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BYVERI.
       DATE-WRITTEN. 11-11-21.
      ******************************************************************
      *    DIR :  SP
      *    DESC:  BY FILE VERIFICATION REPORT
      * WRITTEN BY: AMM
      *=================================================================
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/BYVERI.CBL S35 11/22/23-12:50:46 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
      * COPY "LIBLP/LP01BY.CPY".
      * COPY "LIBLP/LP01BYA.CPY".
      * COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  RETR-FILE            PIC X.
       01  RETR-DATE-CCYYMM     PIC 9(6).
       01  RETR-BRNO            PIC 9(4).
       01  RETR-CLASS           PIC 999.
       EXEC SQL END DECLARE SECTION END-EXEC.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  TOTAL-BYA-UNMATCH    PIC 9(5)     VALUE 0.
       01  TOTAL-BYB-UNMATCH    PIC 9(5)     VALUE 0.
       01  RECORDS-FOUND-SW     PIC X        VALUE "N".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  ERRCD-LIST           PIC X(6)     VALUE "ERRCD.".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  BEG-BY1-KEY.
               05  BEG-BY-DATE-CCYYMM  PIC 9(6).
               05  BEG-BY-BRNO         PIC 9(4).
               05  BEG-BY-CLASS        PIC 9(3).
           03  END-BY1-KEY.
               05  END-BY-DATE-CCYYMM  PIC 9(6).
               05  END-BY-BRNO         PIC 9(4).
               05  END-BY-CLASS        PIC 9(3).

       01  ACCEPT-BUF          PIC X.

      ******************************************************************
      *
      *    H E A D E R - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(10) VALUE SPACES.
           03  H01-PAGE           PIC X(09) VALUE "PAGE NO: ".
           03  H01-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  FILLER             PIC X(15) VALUE SPACES.
           03  H02-TITLE          PIC X(60) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER             PIC X(08) VALUE "BRANCH :".
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H03-BRANCH         PIC 9(04) VALUE ZEROES.
           03  H03-BRANCHX        REDEFINES H03-BRANCH
                                  PIC X(04).
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H03-BRANCH-DESC    PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(76) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-04.
           03  FILLER PIC X(30) VALUE "ORGFILE   DATE     BRNO   CLAS".
           03  FILLER PIC X(30) VALUE "S  MISSING FILE               ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
      *-----------------------------------------------------------------
       01  DETAIL-LINE                   PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01 REDEFINES DETAIL-LINE.
           03  FILLER                 PIC X(03).
           03  D-BY-ORGFILE           PIC X.
           03  FILLER                 PIC X(05).
           03  D-BY-DATE-CCYYMM       PIC 9999/99.
           03  FILLER                 PIC X(03).
           03  D-BY-BRNO              PIC ZZZ9.
           03  FILLER                 PIC X(03).
           03  D-BY-CLASS             PIC ZZ9.
           03  FILLER                 PIC X(08).
           03  D-BY-MISSING           PIC X.

      *-------------------------------------------------------{ TOTALS }
       01  TOTAL-LINE-01              REDEFINES DETAIL-LINE.
           03  T01-PROMPT             PIC X(19).
           03  FILLER                 PIC X(01).
           03  T01-NUMERIC            PIC Z,ZZZ,ZZZ,ZZ9.

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01              REDEFINES DETAIL-LINE.
           03  R01-PROMPT             PIC X(22).
           03  FILLER                 PIC X(01).
           03  R01-FIELD              PIC X(50).
           03  R01-NUMERIC            REDEFINES R01-FIELD
                                      PIC ZZZZ9.
           03  R01-DATE               REDEFINES R01-FIELD
                                      PIC 99/99.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  BYVERI-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BYVERI_DEF.CPY".
       COPY "LIBSP/BYVERI_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/BYVERI_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SQL-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      *************************************
       COPY "LIBGB/CHKSEC.CPY".
       MAIN-ROUTINE.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BYVERI" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
      *            CELL WIDTH 10
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS BYVERI-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE BEG-BY-DATE-CCYYMM   TO QBY1-WBEG-DATE-CCYYMM.
           MOVE END-BY-DATE-CCYYMM   TO QBY1-WEND-DATE-CCYYMM.
           MOVE BEG-BY-BRNO          TO QBY1-WBEG-BRNO.
           MOVE END-BY-BRNO          TO QBY1-WEND-BRNO.
           MOVE BEG-BY-CLASS         TO QBY1-WBEG-CLASS.
           MOVE END-BY-CLASS         TO QBY1-WEND-CLASS.

           PERFORM START-SQL-FILE.  *>  EMBEDDED  BYA BYB
 
       NEXT-SQL-RETR.
           PERFORM READ-SQL-FILE-NEXT.   *>  EMBEDDED  BYA BYB
           IF IO-FG = 9  
               GO TO END-ROUTINE.

           MOVE "Y" TO RECORDS-FOUND-SW.

           PERFORM PRINT-UNMATCH-RECORD-ROUTINE.

           GO TO NEXT-SQL-RETR.

       END-ROUTINE.
           PERFORM PRINT-TOTAL-LINES.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      /**********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
           MOVE " " TO ERRCD.
       SETRP.
           MOVE BYVERI-QUEUE-POS TO Q-POS.
           MOVE BYVERI-COPIES-POS TO C-POS.
           MOVE BYVERI-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00" GO TO ENDIT.
           MOVE " " TO ERRCD.
      *SET BEGINNING DATE 
       DT-01.
           MOVE "ENNN040DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-01.
           MOVE VDUNUM0 TO DATE-MMYY.

           IF ( DATE-MMYY-MM < 01 ) OR
              ( DATE-MMYY-MM > 12 )
              MOVE "ERROR: ENTRY NOT VALID (MMYY)" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BEG-BY-DATE-CCYYMM.

       DT-02.
      *SET ENDING DATE 
           MOVE "ENNN040DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-02.
           MOVE VDUNUM0 TO DATE-MMYY.
           IF ( DATE-MMYY-MM < 01 ) OR
              ( DATE-MMYY-MM > 12 )
              MOVE "ERROR: ENTRY NOT VALID (MMYY)" TO MESS
              PERFORM SEND-MESS
              GO TO DT-02.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO END-BY-DATE-CCYYMM.

           IF BEG-BY-DATE-CCYYMM > END-BY-DATE-CCYYMM
               MOVE "INVALID DATE RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO DT-01.
           GO TO BRN-01.
      *F3 ALL DATES
       ALL-DT.
           MOVE 0      TO BEG-BY-DATE-CCYYMM. 
           MOVE 999999 TO END-BY-DATE-CCYYMM.

           MOVE EXT-JULIAN-BEG TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE "SNNN040DT1." TO VDU.
           MOVE DATE-MMYY TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE EXT-JULIAN-END TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE "SNNN040DT2." TO VDU.
           MOVE DATE-MMYY TO VDUNUM0.
           PERFORM SCREENIO.

      *SET BEGINNING BRNO 
       BRN-01.
           MOVE "ENNN040BRN1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-BRN.
           IF VDUSTATUS = "1U" GO TO DT-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BRN-01.
           MOVE VDUNUM0 TO BEG-BY-BRNO.
           GO TO BRN-02.
      *SET ENDING BRNO
       BRN-02.
           MOVE "ENNN040BRN2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO BRN-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BRN-02.
           MOVE VDUNUM0 TO END-BY-BRNO.
           IF BEG-BY-BRNO > END-BY-BRNO
               MOVE "INVALID BRANCH RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO BRN-01.
           GO TO CL-01.
      *F3 ALL BRNO
       ALL-BRN.
           MOVE "SNNN040BRN1." TO VDU.
           MOVE 0 TO BEG-BY-BRNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BRN2." TO VDU.
           MOVE 9999 TO END-BY-BRNO VDUNUM0.
           PERFORM SCREENIO.

      *SET BEGINNING CLASS
       CL-01.
           MOVE "ENNN030CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BRN-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-BY-CLASS.
      *SET ENDING CLASS
       CL-02.
           MOVE "ENNN030CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-BY-CLASS.
           IF BEG-BY-CLASS > END-BY-CLASS   
               MOVE "INVALID CLASS RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO CL-01.
           GO TO OK-YN.
      *F3 ALL CLASS   
       ALL-CL.
           MOVE "SNNN030CL1." TO VDU.
           MOVE 0 TO BEG-BY-CLASS   VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030CL2." TO VDU.
           MOVE 999 TO END-BY-CLASS   VDUNUM0.
           PERFORM SCREENIO.

       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N"     GO TO DT-01.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-CONAME TO H01-NAME.
           MOVE EXT-CONAME-SYSDATE TO H01-DATE.
           MOVE EXT-CONAME-USER-ID TO H02-USERID.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER-LINES.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *    W R I T E - P A G E - H E A D E R - L I N E S
      ******************************************************************
       WRITE-PAGE-HEADER-LINES SECTION.

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.
           PERFORM GET-TIME.

      *-------------------------------------------------------------{01}
       WRITE-PAGE-HEADER-CONTINUE.

           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE-NO.

           MOVE HEADER-LINE-01                   TO PR-REC.
      *KC->ADD  01                               TO LN-COUNT.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "BY FILE VERIFICATION REPORT"    TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
      *    MOVE BR-NO                         TO H03-BRANCH.
      *    MOVE BR-NAME                       TO H03-BRANCH-DESC.
      *
      *    MOVE HEADER-LINE-03                   TO PR-REC.
      *    ADD  01                               TO LN-COUNT.
      *    MOVE 01                               TO LN-FEED.
      *    PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{04}
           MOVE HEADER-LINE-04                   TO PR-REC.
           ADD  02                               TO LN-COUNT.
           MOVE 02                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{05}
           MOVE ALL "="                          TO PR-REC.
           ADD  01 TO LN-COUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *=================================================================
       WRITE-PAGE-HEADER-LN-END.

           MOVE 01 TO LN-FEED.

      *-----------------------------------------------------------------
       WRITE-PAGE-HEADER-LN-EXIT.
           EXIT.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO BYVERI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BYVERI-SYSDATE.
           DISPLAY BYVERI-SCREEN UPON BYVERI-WINDOW-HANDLE.
           MOVE BYVERI-FORM-FIELDS TO WR-BUF.
           MOVE BYVERI-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE BYVERI-HELP--FILE TO HELP-LINK-FILE.
           MOVE BYVERI-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BYVERI_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************************
       MISC SECTION.
      ***********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

       PRINT-TOTAL-LINES.
           MOVE 3 TO LN-FEED.

           MOVE "BYA REC UNMATCHED:"   TO T01-PROMPT.
           MOVE TOTAL-BYA-UNMATCH      TO T01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BYB REC UNMATCHED:"   TO T01-PROMPT.
           MOVE TOTAL-BYB-UNMATCH      TO T01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-RANGE-LINES.
           MOVE 3 TO LN-FEED.

           MOVE "BEGINNING DATE   :"   TO R01-PROMPT.
           IF BEG-BY-DATE-CCYYMM = 0
              MOVE 150 TO R01-DATE
           ELSE
              MOVE BEG-BY-DATE-CCYYMM TO R01-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE   :"   TO R01-PROMPT.
           IF END-BY-DATE-CCYYMM = 999999
              MOVE 1249 TO R01-DATE
           ELSE
              MOVE END-BY-DATE-CCYYMM TO R01-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING BRNO   :"   TO R01-PROMPT.
           MOVE BEG-BY-BRNO TO R01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BRNO   :"   TO R01-PROMPT.
           MOVE END-BY-BRNO TO R01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CLASS  :"   TO R01-PROMPT.
           MOVE BEG-BY-CLASS TO R01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CLASS  :"   TO R01-PROMPT.
           MOVE END-BY-CLASS TO R01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

        PRINT-UNMATCH-RECORD-ROUTINE.

           MOVE RETR-FILE         TO D-BY-ORGFILE.
           MOVE RETR-DATE-CCYYMM  TO D-BY-DATE-CCYYMM.
           MOVE RETR-BRNO         TO D-BY-BRNO.
           MOVE RETR-CLASS        TO D-BY-CLASS.

           IF RETR-FILE = "A" 
              MOVE "B" TO D-BY-MISSING
              ADD 1    TO TOTAL-BYA-UNMATCH
           ELSE  *> RETR-FILE = "B" 
              MOVE "A" TO D-BY-MISSING
              ADD 1    TO TOTAL-BYB-UNMATCH.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *****************************
       IO-ROUTINE SECTION.
      *****************************
       COPY "LIBGB/CONNECT_SQL.CPY".

      *-----------------------------------------------------------------
       START-SQL-FILE.   *>  EMBEDDED  BYA BYB
           PERFORM START-IT.
           MOVE "BYVERI CSR"  TO E-FILE.
           MOVE SPACES        TO E-KEYX.

           IF ( BYA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SQL-FILE.

           EXEC SQL
            DECLARE BYA1_CURSOR CURSOR FOR
             SELECT 'A'
                   ,BYAFILE.BYA_DATE_CCYYMM
                   ,BYAFILE.BYA_BRNO
                   ,BYAFILE.BYA_CLASS
           
             FROM DBO.BYAFILE
           
             LEFT OUTER JOIN DBO.BYBFILE
               ON BYBFILE.BYB_DATE_CCYYMM = BYAFILE.BYA_DATE_CCYYMM
              AND BYBFILE.BYB_BRNO        = BYAFILE.BYA_BRNO
              AND BYBFILE.BYB_CLASS       = BYAFILE.BYA_CLASS

             WHERE BYAFILE.BYA_DATE_CCYYMM BETWEEN
                 :QBY1-WBEG-DATE-CCYYMM  AND :QBY1-WEND-DATE-CCYYMM
               AND BYAFILE.BYA_BRNO BETWEEN  :QBY1-WBEG-BRNO 
                                         AND :QBY1-WEND-BRNO
               AND BYAFILE.BYA_CLASS BETWEEN :QBY1-WBEG-CLASS
                                         AND :QBY1-WEND-CLASS
               AND BYBFILE.BYB_BRNO IS NULL 

             UNION ALL

             SELECT 'B'
                   ,BYBFILE.BYB_DATE_CCYYMM
                   ,BYBFILE.BYB_BRNO
                   ,BYBFILE.BYB_CLASS

             FROM DBO.BYBFILE

             LEFT OUTER JOIN DBO.BYAFILE
               ON BYAFILE.BYA_DATE_CCYYMM = BYBFILE.BYB_DATE_CCYYMM
              AND BYAFILE.BYA_BRNO        = BYBFILE.BYB_BRNO
              AND BYAFILE.BYA_CLASS       = BYBFILE.BYB_CLASS

             WHERE BYBFILE.BYB_DATE_CCYYMM BETWEEN
                 :QBY1-WBEG-DATE-CCYYMM AND :QBY1-WEND-DATE-CCYYMM
               AND BYBFILE.BYB_BRNO BETWEEN :QBY1-WBEG-BRNO 
                                        AND :QBY1-WEND-BRNO
               AND BYBFILE.BYB_CLASS BETWEEN :QBY1-WBEG-CLASS
                                         AND :QBY1-WEND-CLASS
               AND BYAFILE.BYA_BRNO IS NULL 

             ORDER BY 1,2,3,4

           END-EXEC.

           EXEC SQL
               OPEN BYA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO BYA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SQL-FILE-NEXT.   *>  EMBEDDED  BYA BYB
           PERFORM READ-IT.
           MOVE "BYVERI CSR"       TO E-FILE.
           MOVE SPACES             TO E-KEYX.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH BYA1_CURSOR INTO 
                   :RETR-FILE
                  ,:RETR-DATE-CCYYMM
                  ,:RETR-BRNO
                  ,:RETR-CLASS
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SQL-FILE-NEXT.

      *-----------------------------------------------------------------
       CLOSE-SQL-FILE.
           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BYA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BYA1-CURSOR-STAT.

      *=================================================================
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***********************************************************
       END-PROGRAM SECTION.
      ***********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE "LP/LPL2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY BYVERI-WINDOW-HANDLE.
           EXIT PROGRAM.
