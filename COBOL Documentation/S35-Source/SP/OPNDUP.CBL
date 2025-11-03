       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       OPNDUP.
       AUTHOR.              BLM.
       DATE-WRITTEN. 2009/05/07.
      *******************************************************************
      *    WORLD: READ BI, LOOK FOR SSNO'S WITH DUPLICATE OPEN ACCTS
      *    ONLY MAKER CD "M", SKIPS P&L'S, PAIDOUTS, WCBC
      *    FOR OKLAHOMA, CHANGED TO NOT ALLOW MORE THAN 1 LOAN
      *    WORLD PR# 635
      *
      * ONLY RAN FROM A $ SIGN
      *
      * *** PROGRAM NOTES AFTER REWRITE ***
      *
      * 09-06-2024 - THERE IS ONE DIFFERENCE IN A15 CODE FROM S35. IN 
      *   A15, BI-GIFTNET IS FOUND IN 2 PLACE TO FILTER DATA. 
      *   1. BI-GIFTNET NOT IN ('Y', 'X', 'A')
      *   2. BI-GIFTNET NOT IN ('X')
      *   S35 WAS CHANGED TO ONLY HAVE 1 FILTER - 
      *   1. BI-GIFTNET NOT IN ('Y', 'X', 'A')
      *
      *   I BELEVE THERE ARE OTHER ISSUES WITH A15.
      *   1. I DON'T THINK A15 ADD'S CORRECTLY TO THE 1 LOAN COUNTER.
      *      I THINK IT MISSES THE LAST LOAN WHEN ADDING TO THE COUNTER.
      *   2. BRANCH 200, SSNO - 249842970, ACCTNO - 7997: THIS IS 
      *      EXAMPLE OF BEING ON THE A15 REPORT WHEN IT SHOULDN'T BE.
      *   3. ANY OTHER DIFFERENCE WILL NEED TO BE LOOKED AT INDIVIDUALLY 
      *      FOR CAUSE/RESOLUTION.  
      *   RMZ       
      *
      * REV:
      * 2017.1010 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * 2017.1205 BAH SKIP BI-GIFTNET "X", TAX ADVANCE LOANS, THESE ARE
      *               NOT CONSIDER DUAL LOANS, SP-NON-LOAN-FLAG "Y", 
      *               WORLD 1288
      * 2024.0815 BAH ADDED PRINT-FULL-SSNO Y/N, 'N' PRINTS LAST 4
      *               #1673 S35Q-118
      * 2024.0823 RMZ REWROTE ENTIRE PROGRAM TO USE SQL SERVER CORRECTLY.
      *               USING THE 1ST STORED PROCEDURE, ALL DETAIL DATA IS
      *               RETRIEVED AND WRITTEN TO THE REPORT AT ONE TIME.
      *               USING THE 2ND STORED PROCEDURE, THE TOTALS ARE
      *               RETRIEVED AND WRITTEN TO THE REPORT AT END-OF-JOB. 
      *               (S35Q-120)
      *
      *******************************************************************
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./SP/OPNDUP.CBL S35 09/06/24-10:41:24 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.      
        01  QWS-BEGIN-BRNO-GRID      PIC X(04).  
        01  QWS-END-BR-NO            PIC X(04).     
        01  QWS-TOTAL-1-LOAN         PIC 9(08).
        01  QWS-TOTAL-2-LOANS        PIC 9(08).
        01  QWS-TOTAL-3-LOANS        PIC 9(08).
        01  QWS-TOTAL-4-PLUS-LOANS   PIC 9(08). 
        01  QWS-TOTAL-LOANS          PIC 9(08).  
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  BI1-CONN-OPEN-SW     PIC X        VALUE "N".
       
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  PR-IFN               PIC XX   VALUE "PR".

       01  FIRST-TIME           PIC X VALUE "Y".

       01  ENT-GROUP            PIC X(4).
       01  INCLUDE-DETAIL-FG    PIC X.
       01  PRINT-FULL-SSNO      PIC X.
       01  GROUP-FLAG           PIC X.
       01  REMAINDER-WK         PIC S9(9)V99 VALUE 0.
       01  RESULT-WK            PIC S9(9)V99 VALUE 0.

       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.

       01  RECORD-COUNTER       PIC 9(7)   VALUE 0.
	   
       01  WORK-ACCTNO          PIC 9(8).
       01  FILLER               REDEFINES WORK-ACCTNO.          
           03  WORK-NUMBER      PIC 9(6).
           03  FILLER           PIC 99.

       01  SAVE-BI1-KEY.
           03  SAVE-BI-SSNO     PIC 9(9) COMP.
           03  SAVE-BI-ACCTNO   PIC 9(8) COMP.
           03  SAVE-BI-OWNBR    PIC 9(4) COMP.
       01  NEXT-BI1-KEY.
           03  NEXT-BI-SSNO     PIC 9(9) COMP.
           03  NEXT-BI-ACCTNO   PIC 9(8) COMP.
           03  NEXT-BI-OWNBR    PIC 9(4) COMP.
       01  HOLD-BRNO            PIC 9(4) VALUE 0.
       01  PL-COUNT             PIC 9(9)   VALUE 0.
       01  LOAN-COUNT           PIC S9(9)  VALUE +0.
       01  READ-COUNT           PIC S9(9)  VALUE +0.

       01  DISPLAY-LIST         PIC X(4) VALUE "MES.".
       01  DISPLAY-MESS.
           03  FILLER           PIC X(35) VALUE
               "REPORT IN PROGRESS.............".

       01  ERRCD                PIC X VALUE SPACES.

       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      *----------------------------------------------------------------
       01  HEAD1.
           03  H-DATE               PIC X(8) VALUE SPACES.
           03  FILLER               PIC XX   VALUE SPACES.
           03  H-HR                 PIC 99   VALUE 0.
           03  FILLER               PIC X    VALUE ":".
           03  H-MIN                PIC 99   VALUE 0.
           03  FILLER               PIC X(10) VALUE SPACES.
           03  H-NAME               PIC X(40).
           03  FILLER               PIC X(16).
           03  FILLER               PIC X(5)  VALUE "PAGE ".
           03  H-PAGE-NO            PIC ZZZ9.
       01  HEAD2.
           03  FILLER               PIC X(10) VALUE "USER ID:".
           03  H-USERID             PIC X(10).
           03  FILLER               PIC X(2)  VALUE SPACES.
           03  H-TITLE              PIC X(48) VALUE
               "GLOBAL BORROWER OPEN DUPLICATE LOAN REPORT FOR: ".
           03  H-GROUP              PIC X(4).

       01  HEAD3.
           03  FILLER               PIC X(54) VALUE
           "BRNO  SSNO         BORROWER NAME".
           03  FILLER               PIC X(38) VALUE
           " ACCT #    DATE    CLASS      BALANCE ".

       01  TOTAL-HEAD.
           03  FILLER               PIC X(52) VALUE
           "  1 LOAN    2 LOANS    3 LOANS   4+ LOANS      TOTAL".

       01  DETAIL-LINE              PIC X(132).
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO               PIC 9(04).
           03  FILLER               PIC X(02).
           03  D-SSNO               PIC 999/99/9999.
           03  D-SSNOX REDEFINES D-SSNO.
               05  D-SSNO5          PIC X(7).
               05  D-SSNO4          PIC 9(4).
           03  FILLER               PIC X(02).
           03  D-BWLNAME            PIC X(16).
           03  D-BWFNAME            PIC X(16).
           03  FILLER               PIC X(02).
           03  D-ACCTNO             PIC ZZZZ9999.
           03  FILLER               PIC X(02).
           03  D-ENTDATE            PIC 99/99/99.
           03  FILLER               PIC X(04).
           03  D-CLASS              PIC 99.
           03  FILLER               PIC X(04).
           03  D-CURBAL             PIC ZZZ,ZZ9.99.
       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  D-1-LOANS            PIC ZZZZ,ZZZ.
           03  FILLER               PIC X(03).
           03  D-2-LOANS            PIC ZZZZ,ZZZ.
           03  FILLER               PIC X(03).
           03  D-3-LOANS            PIC ZZZZ,ZZZ.
           03  FILLER               PIC X(03).
           03  D-4-PLUS-LOANS       PIC ZZZZ,ZZZ.
           03  FILLER               PIC X(03).
           03  D-TOTAL-LOANS        PIC ZZZZ,ZZZ.
           03  FILLER               PIC X(03).
       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).

       01  HOLD-BI-REC          PIC X(BI-SIZE). 

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".

       01  OPNDUP-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/OPNDUP_DEF.CPY".
       COPY "LIBSP/OPNDUP_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/OPNDUP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
            PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BI1-FILE1.
           PERFORM CLOSE-BI1-FILE2.
           PERFORM CLOSE-BI1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
            PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *    ***************************
      *    *      MAIN PROGRAM MODULE
      *    ***************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
       COPY "LIBGB/CHKSEC.CPY".
       MAIN-ROUTINE.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "OPNDUP" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
      *            CELL WIDTH 10
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS OPNDUP-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE 0         TO BI-SSNO BI-ACCTNO BI-OWNBR
                             QBI1-WBEG-SSNO
                             QBI1-WBEG-ACCTNO
                             QBI1-WBEG-OWNBR.
           MOVE ALL "9"   TO QBI1-WEND-SSNO
                             QBI1-WEND-ACCTNO
                             QBI1-WEND-OWNBR.

      D    STOP MESS.

      *     MOVE "REPORT IN PROGRESS................" TO WR-BUF
      *     MOVE MESS-LIST TO WR-LIST

           MOVE DISPLAY-MESS TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.          
       
           IF INCLUDE-DETAIL-FG NOT = "Y"
              GO TO END-ROUTINE.            

       DETAIL-ROUTINE.           

           PERFORM START-BI1-FILE1. *> SSP - BI, LN, BR, GR 

       NEXT-DETAIL-BI.
           PERFORM READ-BI1-FILE-NEXT1. *> EMBEDDED - BI, LN, BR, GR
           IF IO-BAD
              PERFORM CLOSE-BI1-FILE1
              GO TO END-ROUTINE.

           IF BI-SSNO     NOT = SAVE-BI-SSNO
              MOVE SPACES    TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE
              MOVE BI-SSNO   TO SAVE-BI-SSNO. 

           MOVE BI-OWNBR     TO D-BRNO.
           IF PRINT-FULL-SSNO = "Y"
              MOVE BI-SSNO   TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY "-"
           ELSE
              MOVE "XXX-XX-" TO D-SSNO5
              MOVE BI-SSNO   TO D-SSNO4.
           MOVE BI-LNAME     TO D-BWLNAME.
           MOVE BI-FNAME     TO D-BWFNAME.
           MOVE BI-ENTDATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-ENTDATE.
           MOVE BI-ACCTNO    TO D-ACCTNO.
           MOVE LN-CLASS     TO D-CLASS.
           MOVE LN-CURBAL    TO D-CURBAL.
           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-DETAIL-BI.

        END-ROUTINE.
           PERFORM TOTAL-ROUTINE.
           PERFORM PRINT-RANGES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.

       GETPR.
           MOVE OPNDUP-QUEUE-POS TO Q-POS.
           MOVE OPNDUP-COPIES-POS TO C-POS.
           MOVE OPNDUP-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.

       ENTER-GROUP.

           MOVE "E  A040GROUP." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-GROUP
              ELSE
                 MOVE "S  A000GROUP." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF VDUSTATUS = "1>" GO TO ENDIT.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-GROUP.

           MOVE VDUBUF TO WGR-WORD
                          ENT-GROUP
                          H-GROUP.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
                   AND WGR-ERR NOT = 1
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-GROUP.

           MOVE WGR-WORD TO ENT-GROUP.
           MOVE "Y" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       INCLUDE-DETAIL-YN.
           MOVE "E  A010INCDET." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO ENTER-GROUP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO INCLUDE-DETAIL-YN.
           IF VDUBUF NOT = "N"
              IF VDUBUF NOT = "Y"
                 GO TO INCLUDE-DETAIL-YN.
           MOVE VDUBUF TO INCLUDE-DETAIL-FG.

       PRINT-FULL-SSNO-YN.
           IF INCLUDE-DETAIL-FG = "N"
              MOVE "N" TO PRINT-FULL-SSNO
              GO TO OK-YN.

           MOVE "E  A010FULL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO INCLUDE-DETAIL-YN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO PRINT-FULL-SSNO-YN.
           IF VDUBUF NOT = "N"
              IF VDUBUF NOT = "Y"
                 GO TO PRINT-FULL-SSNO-YN.

           MOVE VDUBUF TO PRINT-FULL-SSNO.

      *-----------------------------------------------------------------
       OK-YN.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO INCLUDE-DETAIL-YN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO OK-YN.
           IF VDUBUF = "N" GO TO ENDIT.
           IF VDUBUF NOT = "Y" GO TO OK-YN.

           MOVE ZEROS TO RECORD-COUNTER.

           MOVE SPACES TO DETAIL-LINE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-BI1-FILE.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

           MOVE 0 TO QWS-TOTAL-1-LOAN
                     QWS-TOTAL-2-LOANS
                     QWS-TOTAL-3-LOANS
                     QWS-TOTAL-4-PLUS-LOANS
                     QWS-TOTAL-LOANS.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      **************************************
       TOTAL-ROUTINE SECTION.
      **************************************

           PERFORM START-BI1-FILE2. *> SSP - BI, LN, BR, GR 
           PERFORM READ-BI1-FILE-NEXT2. *> EMBEDDED - BI, LN, BR, GR
           IF IO-BAD
              GO TO END-ROUTINE.  
  
           MOVE TOTAL-HEAD TO DETAIL-LINE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           MOVE QWS-TOTAL-1-LOAN       TO D-1-LOANS.
           MOVE QWS-TOTAL-2-LOANS      TO D-2-LOANS.
           MOVE QWS-TOTAL-3-LOANS      TO D-3-LOANS.
           MOVE QWS-TOTAL-4-PLUS-LOANS TO D-4-PLUS-LOANS.
           MOVE QWS-TOTAL-LOANS        TO D-TOTAL-LOANS.
           PERFORM WRITE-DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CLOSE-BI1-FILE2.

      *************************
       PRINT-RANGES SECTION.
      *************************
           MOVE "GROUP ID:" TO RANGE-LINE.
           MOVE  ENT-GROUP  TO RANGE-SELECTION.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE DETAIL: "         TO RANGE-LINE.
           MOVE INCLUDE-DETAIL-FG          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT FULL SSNO: "        TO RANGE-LINE.
           MOVE PRINT-FULL-SSNO            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

      **************************************
      *     WRITE DETAIL LINE
      **************************************
       WRITE-DETAIL-LINE SECTION.
       WRITE-DET-LINE.
           ADD LN-COUNT LN-FEED GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      **************************************

      *      HEAD PAGE ROUTINE
      **************************************
       HEAD-PAGE SECTION.
       HEAD-ROUTINE.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF INCLUDE-DETAIL-FG = "Y"
              MOVE HEAD3 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE 2 TO LN-FEED
              MOVE 3 TO LN-COUNT
           ELSE
              MOVE 2 TO LN-FEED
              MOVE 1 TO LN-COUNT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO OPNDUP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO OPNDUP-SYSDATE.
           DISPLAY OPNDUP-SCREEN UPON OPNDUP-WINDOW-HANDLE.
           MOVE OPNDUP-FORM-FIELDS TO WR-BUF.
           MOVE OPNDUP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE OPNDUP-HELP--FILE TO HELP-LINK-FILE.
           MOVE OPNDUP-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/OPNDUP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".

      ********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1RN.CPY".

      * COPY "LIBLP/LPBI1RN.CPY".
      * COPYMEMBER: LIBLP/LPBI1RN
       LOAD-BI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BI-ALL-BASE
              MOVE BI-ALL-PATH             TO BI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BI-ALL-BASE
              MOVE BI-ALL-PATH             TO BI-PATH.

      *-----------------------------------------------------------------
       OPEN-BI1-FILE.
           PERFORM LOAD-BI1-FILE.
           PERFORM OPEN-IT.
           MOVE BI-PATH-SQL TO E-FILE.

           IF BI1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO BI1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BI1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.
 
      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.
   
      *-----------------------------------------------------------------
       START-BI1-FILE1.
           PERFORM START-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.

           IF ( BI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BI1-FILE1.

           MOVE ENT-GROUP      TO QWS-BEGIN-BRNO-GRID 
                                  QWS-END-BR-NO.

      *    IF ( QBI1-WEND-OWNBR    = ZEROES    ) OR
      *       ( QBI1-WEND-OWNBR  NOT NUMERIC )
      *       MOVE BI-SSNO        TO QBI1-WBEG-SSNO
      *       MOVE ALL "9"        TO QBI1-WEND-SSNO
      *       MOVE BI-ACCTNO      TO QBI1-WBEG-ACCTNO
      *       MOVE ALL "9"        TO QBI1-WEND-ACCTNO
      *       MOVE BI-OWNBR       TO QBI1-WBEG-OWNBR
      *       MOVE ALL "9"        TO QBI1-WEND-OWNBR.
      
      *  STORED PROCEDURE:
      *
      *  SELECT BIFILE.BI_OWNBR,
      *         BIFILE.BI_SSNO,
      *         BIFILE.BI_LNAME,
      *         BIFILE.BI_FNAME,
      *         BIFILE.BI_ACCTNO,
      *         CAST(BIFILE.BI_ENTDATE  AS VARCHAR(10)) AS BI_ENTDATE,
      *         BIFILE.BI_MAKERCD, 
      *         CAST(BIFILE.BI_POFFDATE AS VARCHAR(10)) AS BI_POFFDATE,
      *         BIFILE.BI_PLCD,    
      *         BIFILE.BI_GIFTNET,
      *         LNFILE.LN_OWNBR,
      *         LNFILE.LN_ACCTNO,
      *         LNFILE.LN_CLASS,
      *         LNFILE.LN_CURBAL
      *  
      *    FROM DBO.BIFILE BIFILE
      *  
      *   INNER JOIN
      *         #TEMP3_BRANCH_TBL TEMP3
      *      ON (BIFILE.BI_OWNBR  = TEMP3.MEMBER_ID) 
      *  
      *   INNER JOIN 
      *         DBO.LNFILE LNFILE
      *      ON (BIFILE.BI_OWNBR  = LNFILE.LN_OWNBR
      *     AND  BIFILE.BI_ACCTNO = LNFILE.LN_ACCTNO) 
      *  
      *   INNER JOIN (SELECT BIFILE.BI_SSNO
      *                 FROM DBO.BIFILE BIFILE,
      *                      #TEMP3_BRANCH_TBL TEMP3
      *                WHERE BIFILE.BI_MAKERCD      = 'M'
      *                  AND BIFILE.BI_POFFDATE     = '1900-01-01'
      *                  AND BIFILE.BI_PLCD         = ' ' 
      *                  AND BIFILE.BI_GIFTNET NOT IN ('Y', 'X', 'A')
      *                  AND BIFILE.BI_OWNBR        = MEMBER_ID
      *  
      *                GROUP BY BIFILE.BI_SSNO
      *               HAVING COUNT(DISTINCT BI_ACCTNO) > 1) TEMP  
      *      ON (BIFILE.BI_SSNO = TEMP.BI_SSNO)
      *   WHERE BIFILE.BI_MAKERCD      = 'M'
      *     AND BIFILE.BI_POFFDATE     = '1900-01-01'
      *     AND BIFILE.BI_PLCD         = ' ' 
      *     AND BIFILE.BI_GIFTNET NOT IN ('Y', 'X', 'A')
      *  
      *   ORDER BY BIFILE.BI_SSNO,  
      *            BIFILE.BI_ACCTNO,
      *            BIFILE.BI_OWNBR;

           EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BI1_CURSOR1 CURSOR FOR
                :BI1-RETURN-CODE = EXEC SSP_OPNDUP_BIFILE_SELECT1 
              ( :QWS-BEGIN-BRNO-GRID,
                :QWS-END-BR-NO )
           END-EXEC.

           EXEC SQL
               OPEN BI1_CURSOR1
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBI1-WBEG-SSNO
                          QBI1-WEND-SSNO
                          QBI1-WBEG-ACCTNO
                          QBI1-WEND-ACCTNO
                          QBI1-WBEG-OWNBR
                          QBI1-WEND-OWNBR.

           MOVE STAT---GOOD TO BI1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BI1-FILE2.
           PERFORM START-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.

           IF ( BI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BI1-FILE2.

           MOVE ENT-GROUP      TO QWS-BEGIN-BRNO-GRID 
                                  QWS-END-BR-NO.

      *    IF ( QBI1-WEND-OWNBR    = ZEROES    ) OR
      *       ( QBI1-WEND-OWNBR  NOT NUMERIC )
      *       MOVE BI-SSNO        TO QBI1-WBEG-SSNO
      *       MOVE ALL "9"        TO QBI1-WEND-SSNO
      *       MOVE BI-ACCTNO      TO QBI1-WBEG-ACCTNO
      *       MOVE ALL "9"        TO QBI1-WEND-ACCTNO
      *       MOVE BI-OWNBR       TO QBI1-WBEG-OWNBR
      *       MOVE ALL "9"        TO QBI1-WEND-OWNBR.

      *  STORED PROCEDURE:
      *
      *  SELECT SUM(CASE WHEN ACCTNO_COUNT = 1
      *                  THEN 1
      *                  ELSE 0
      *              END) AS QWS_TOTAL_1_LOAN,
      *         SUM(CASE WHEN ACCTNO_COUNT = 2
      *                  THEN 1
      *                  ELSE 0
      *              END) AS QWS_TOTAL_2_LOANS,			   
      *         SUM(CASE WHEN ACCTNO_COUNT = 3
      *                  THEN 1
      *                  ELSE 0
      *              END) AS QWS_TOTAL_3_LOANS,
      *         SUM(CASE WHEN ACCTNO_COUNT > 3
      *                  THEN 1
      *                  ELSE 0
      *              END) AS QWS_TOTAL_4_PLUS_LOANS,
      *         SUM(CASE WHEN ACCTNO_COUNT > 1
      *                  THEN 1
      *                  ELSE 0
      *              END) AS QWS_TOTAL_LOANS
      *     FROM   
      *         (SELECT TEMP.BI_SSNO,
      *           COUNT(TEMP.ACCTNO_COUNT) AS ACCTNO_COUNT
      *            FROM
      *               (SELECT BIFILE.BI_SSNO,
      *                       BI_ACCTNO    AS ACCTNO_COUNT
      *                  FROM DBO.BIFILE BIFILE,
      *                       #TEMP3_BRANCH_TBL TEMP3
      *                 WHERE BIFILE.BI_MAKERCD      = 'M'
      *                   AND BIFILE.BI_POFFDATE     = '1900-01-01'
      *                   AND BIFILE.BI_PLCD         = ' ' 
      *                   AND BIFILE.BI_GIFTNET NOT IN ('Y', 'X', 'A')
      *                   AND BIFILE.BI_OWNBR        = TEMP3.MEMBER_ID  
      *                 GROUP BY BIFILE.BI_SSNO,
      *                          BIFILE.BI_ACCTNO) TEMP
      *          GROUP BY TEMP.BI_SSNO) TEMP2;
      * 
      *           EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC.
      * 
      *           EXEC SQL
      *            DECLARE BI1_CURSOR2 CURSOR FOR
      *                :BI1-RETURN-CODE = EXEC SSP_OPNDUP_BIFILE_SELECT2 
      *              ( :QWS-BEGIN-BRNO-GRID,
      *                :QWS-END-BR-NO )
      *           END-EXEC.
      * 

           EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BI1_CURSOR2 CURSOR FOR
                :BI1-RETURN-CODE = EXEC SSP_OPNDUP_BIFILE_SELECT2
              ( :QWS-BEGIN-BRNO-GRID,
                :QWS-END-BR-NO )
           END-EXEC.

           EXEC SQL
               OPEN BI1_CURSOR2
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBI1-WBEG-SSNO
                          QBI1-WEND-SSNO
                          QBI1-WBEG-ACCTNO
                          QBI1-WEND-ACCTNO
                          QBI1-WBEG-OWNBR
                          QBI1-WEND-OWNBR.

           MOVE STAT---GOOD TO BI1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BI1-FILE-NEXT1.
           PERFORM READ-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.
           INITIALIZE QBI-REC.
           INITIALIZE QLN-REC.
 
           EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC.

           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH BI1_CURSOR1 
                  INTO 
                   :QBI-OWNBR,
                   :QBI-SSNO,
                   :QBI-LNAME,
                   :QBI-FNAME,
                   :QBI-ACCTNO,
                   :QBI-ENTDATE, 
                   :QBI-MAKERCD, 
                   :QBI-POFFDATE,
                   :QBI-PLCD,    
                   :QBI-GIFTNET,
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-CURBAL
             END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BI1-FILE-NEXT1.

           IF ( IO-FG = 0 )
              PERFORM GET-BI-FIELDS
              PERFORM GET-LN-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BI1-FILE-NEXT2.
           PERFORM READ-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.
 
           EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC.

           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH BI1_CURSOR2   
                  INTO 
                   :QWS-TOTAL-1-LOAN,      
                   :QWS-TOTAL-2-LOANS,     
                   :QWS-TOTAL-3-LOANS,    
                   :QWS-TOTAL-4-PLUS-LOANS,
                   :QWS-TOTAL-LOANS       
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BI1-FILE-NEXT2.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BI1-FILE1.
           PERFORM CLOSE-IT.
           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BI1_CURSOR1
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO BI1-CURSOR-STAT.
 
      *-----------------------------------------------------------------
       CLOSE-BI1-FILE2.
           PERFORM CLOSE-IT.
           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BI1_CURSOR2
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO BI1-CURSOR-STAT.
 
      *-----------------------------------------------------------------
       CLOSE-BI1-CONNECTION.

           IF BI1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BI1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BI1-CONN-OPEN-SW.

      *-----------------------------------------------------------------

       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/LPRPMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY OPNDUP-WINDOW-HANDLE.
           EXIT PROGRAM.
