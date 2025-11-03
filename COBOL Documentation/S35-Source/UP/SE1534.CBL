       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SE1534.
       DATE-WRITTEN.    11/04/2020.
      ***********************************************************************
      *                 (UP)
      *  DESCRIPTION:   REMOVE SE RECORDS WITH NO MATCHING SN RECORD
      *
      *        READS OSEFILE, IF NO MATCH IN SNFILE, DONT CONVERT IT,
      *        IF FIND A MATCH, CONVERT THE SE RECORD
      *        CREATED A GOOD, CLEAN SNFILE PRIOR TO THIS.
      *
      * 2020.11.16 BAH
      ***********************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT OSE-FILE ASSIGN TO OSE-PATH  
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OSE1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".
       FD  OSE-FILE
           LABEL RECORDS ARE STANDARD.
       01  OSE-REC.
           03  OSE1-KEY.
               05  OSE-DIR      PIC X(2).
               05  OSE-FORM     PIC X(6).
           03  OSE-EXP-DATE     PIC 9(6) COMP-6.
           03  OSE-PURCHASABLE  PIC X.
           03  OSE-PROFILE      PIC X(26).
           03  FILLER          PIC X(30).


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UP/SE1534 A34 12/03/20-09:57:14 >".

       COPY "LIBGB/GB01SE.CPY".
       COPY "LIBGB/GB01SE_SQL.CPY".
       COPY "LIBGB/GBWSSE.CPY".
       COPY "LIBGB/GB01SN.CPY".
       COPY "LIBGB/GB01SN_SQL.CPY".
       COPY "LIBGB/GBWSSN.CPY".

       01  ORIG-PATH.
           03  ORIG-PATH-BASE    PIC X(9).
           03  ORIG-PATH-R1      PIC X(4) VALUE "/R1/".
           03  FILLER           PIC X(10) VALUE "GB/SEFILE".

       01  OSE-PATH.
           03  OSE-PATH-BASE    PIC X(9).
           03  OSE-PATH-R1      PIC X(4) VALUE "/R1/".
           03  FILLER           PIC X(10) VALUE "GB/OSEFILE".

       01  SOURCE-PATH          PIC X(9).
       01  SOURCE-PATH-LIST     PIC X(7) VALUE "S-PATH.".
      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 9(3)     VALUE 99.
           03  LN-WK            PIC 9(3)     VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  WS-COBOL-PATH.
           03  FILLER          PIC X         VALUE "/".
           03  WS-RELEASE      PIC X(7)      VALUE SPACES.
           03  FILLER          PIC X(4)      VALUE "/CO/".
           03  WS-PROGRAM      PIC X(7)      VALUE SPACES.

       01  ERRCD               PIC X.

       01  REPORT-ONLY         PIC X.
       01  ACCEPT-BUF          PIC X    VALUE " ".
       01  COUNT-SKIPPED       PIC 9(6) VALUE 0.
       01  COUNT-DUPS          PIC 9(6) VALUE 0.
       01  COUNT-READ          PIC 9(6) VALUE 0.
       01  COUNT-WRITTEN       PIC 9(6) VALUE 0.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(3)    VALUE " ".
           03  H-TITLE          PIC X(52)    VALUE
           "- SE RECORDS THAT WILL NOT CONVERT".

       01  HEAD3.
           03  FILLER           PIC X(43) VALUE
           "DIR   FORM    DESC.               ".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-DIR          PIC X(6).
           03  FILLER         PIC X(2).
           03  D-FORM         PIC X(6).
           03  FILLER         PIC X(2).
           03  D-DESC         PIC X(30).
           03  FILLER         PIC X(2).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(15).
           03  RANGE-SELECTION.
               05  RANGE-NUM    PIC 9(6).

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  SE1534-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUP/SE1534_DEF.CPY".
       COPY "LIBUP/SE1534_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUP/SE1534_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               OSE-FILE PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SN1-FILE.
           PERFORM CLOSE-SE1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OSE-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **********************************************
       MAIN-MODULE SECTION.
      **********************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SE1534" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SE1534-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

      D    STOP MESS.
      * READ OLD A15 SEFILE, IF PROGRAM EXIST IN NEW SNFILE, CONVERT THE
      * OLD SE RECORD, OTHERWISE, SKIP IT

      * ORIG-PATH              R1/GB/SEFILE
      * OSE-PATH (SOURCE-PATH)/R1/GB/OSEFILE

      *  A34 DOES NOT ALLOW DUPLICATE FILE NAMES, CONFUSES THE DATABASE
      * A15 FILE NEEDS TO BE COPIED TO OSEFILE

           MOVE SOURCE-PATH TO OSE-PATH-BASE ORIG-PATH-BASE.
           CALL "C$TOLOWER" USING OSE-PATH-R1, VALUE 4.
           CALL "C$TOLOWER" USING ORIG-PATH-R1, VALUE 4.
           MOVE SPACES TO SYSTEM-BUF.
           MOVE CP-CMD TO MV-BUF.
           MOVE ORIG-PATH TO MV-FILE1.
           MOVE OSE-PATH  TO MV-FILE2.
           PERFORM SYSTEM-CALL.

           PERFORM OPEN-SN1-FILE.
           PERFORM OPEN-SE1-FILE.
           PERFORM OPEN-OSE1-FILE.

           MOVE SPACES TO OSE-DIR OSE-FORM.

           PERFORM START-OSE1-FILE.

       NEXT-SE.
           PERFORM READ-OSE1-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           ADD 1 TO COUNT-READ.

      * THERE ARE 5 SPECIFIC RECORDS WE NEED TO LOOK FOR. DUPLICATE PROGRAM 
      * NAMES, CLRCHK, ATMAIN, CKMAIN, CHCRED AND FOLLOW
      * WE NEED TO MAKE SURE THE ONE WE WANT GETS CONVERTED
      * CLRCHK EXIST IN LP AND AP.
      * ATMAIN EXIST IN LP AND AP.
      * CKMAIN EXIST IN LP AND AP.
      * CHANGE LP/CHCRED TO LP/CHCRE2  (LONPX0 NOW CALLS CHCRE2)
      * CHANGE LP/FOLLOW TO LP/FOLRPT  (LPR2MU NOW CALLS FOLRPT)
           IF (OSE-DIR = "AP") AND
              (OSE-FORM = "CLRCHK" OR "ATMAIN" OR "CKMAIN")
              ADD 1 TO COUNT-SKIPPED
              PERFORM DETAIL-LINE-PRINT
              GO TO NEXT-SE.


      * CHECK IF SN RECORD  EXISTS, IF NOT, SKIP THIS SE RECORD
           MOVE OSE-FORM TO SN-FORM.
           PERFORM READ-SN1-FILE.
           IF IO-FG NOT = 0
              ADD 1 TO COUNT-SKIPPED
              PERFORM DETAIL-LINE-PRINT
              GO TO NEXT-SE.

      * IF IT DOES EXIST, CONVERT IT.

           IF REPORT-ONLY = "Y"
              GO TO NEXT-SE.

           MOVE SPACES TO SE-REC.
           
           MOVE OSE-FORM TO SE-FORM.
           IF OSE-FORM = "CHCRED" AND OSE-DIR = "LP"
              MOVE "CHCRE2" TO SE-FORM.
           IF OSE-FORM = "FOLLOW" AND OSE-DIR = "LP"
              MOVE "FOLRPT" TO SE-FORM.
      * CHANGE LP/CHCRED TO LP/CHCRE2, DUPLICATE PROGRAM NAMES IN DIFF
      * DIRECTORIES IN A15. CLMENU CALLED WI/CHCRED AND LONPX0 CALLED
      * LP/CHCRED, IN A32/A34 WE CHANGED LP/CHCRED TO LP/CHCRE2
      * CHANGE LP/FOLLOW TO LP/FOLRPT, DUPLICATE PROGRAM NAMES IN DIFF
      * DIRECTORIES IN A15. CLMENU CALLED WI/FOLLOW AND LPR2MU CALLED
      * LP/FOLLOW, IN A32/A34 WE CHANGED LP/FOLLOW TO LP/FOLRPT

      * TOOK THIS RIGHT FROM THE UP1534 PROGRAM
      *    NOT USING PURCHASABLE OPTION, SO FORCE 99991231
      *    TO PURCHASABLE EXPIRATION DATE FOR NOW:
           IF OSE-PURCHASABLE = "Y"
              MOVE 99991231 TO SE-EXP-DATE
           ELSE
              MOVE 0 TO SE-EXP-DATE.
           MOVE OSE-PURCHASABLE TO SE-PURCHASABLE.
           MOVE OSE-PROFILE     TO SE-PROFILE.

      * WHEN WE REMOVED THE DIRECTORY, WE HAD DUP PROGRAM NAMES
           PERFORM WRITE-SE1-FILE.
           IF IO-FG NOT = 0
              MOVE "* DUPLICATE *" TO D-DESC
              PERFORM DETAIL-LINE-PRINT
              ADD 1 TO COUNT-DUPS.

           ADD 1 TO COUNT-WRITTEN.

           GO TO NEXT-SE.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              GO TO END-PROGRAM.                                   

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ************************************************
       DETAIL-LINE-PRINT SECTION.
      ************************************************
           MOVE OSE-DIR         TO D-DIR.
           MOVE OSE-FORM        TO D-FORM.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      /**********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.

           MOVE SE1534-QUEUE-POS TO Q-POS.
           MOVE SE1534-COPIES-POS TO C-POS.
           MOVE SE1534-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT-BAD
              GO TO ENDIT.

           MOVE " " TO ERRCD.

       ENTER-SOURCE-PATH.

           MOVE "E  A090S-PATH." TO VDU.
           PERFORM SCREENIO.

           IF (        F7-KEY ) GO TO ENDIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-SOURCE-PATH.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-SOURCE-PATH.

           MOVE VDUBUF TO DIR-ACCESS-BUF SOURCE-PATH.
           PERFORM DIR-ACCESS-CALL.
           IF (STAT-BAD)
              MOVE "INVALID PATH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SOURCE-PATH.


       REPORT.            
           MOVE "E  A010RPT."    TO VDU.
           PERFORM SCREENIO.
           IF F4-KEY
              GO TO ENTER-SOURCE-PATH.
           IF F7-KEY
              GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO REPORT.

           MOVE VDUBUF TO REPORT-ONLY.    

           IF NOT (REPORT-ONLY = "Y" OR "N")
              GO TO REPORT.           

       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF F7-KEY
              GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO OK-YN.

           MOVE VDUBUF TO ACCEPT-BUF.

           IF ACCEPT-BUF = "N"
              GO TO OK-YN.
           IF ACCEPT-BUF NOT = "Y"
              GO TO OK-YN.


           MOVE " " TO ERRCD.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.


      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           MOVE "N" TO FIRST-TIME.
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

           MOVE HEAD3 TO PR-REC.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-PR-REC.


       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 4 TO LN-FEED.
           MOVE "REPORT ONLY" TO RANGE-LINE.
           MOVE REPORT-ONLY  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE "READ" TO RANGE-LINE.
           MOVE COUNT-READ  TO RANGE-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "SKIPPED" TO RANGE-LINE.
           MOVE COUNT-SKIPPED  TO RANGE-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "WRITTEN" TO RANGE-LINE.
           MOVE COUNT-WRITTEN  TO RANGE-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "DUPS" TO RANGE-LINE.
           MOVE COUNT-DUPS  TO RANGE-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO SE1534-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SE1534-SYSDATE.
           DISPLAY SE1534-SCREEN UPON SE1534-WINDOW-HANDLE.
           MOVE SE1534-FORM-FIELDS TO WR-BUF.
           MOVE SE1534-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SE1534-HELP--FILE TO HELP-LINK-FILE.
           MOVE SE1534-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUP/SE1534_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       MISC SECTION.
      ****************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".

      ****************************************************
       IO-ROUTINES SECTION.
      ****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBSNGS_SQL.CPY".
       COPY "LIBGB/GBSEGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBSN1IN.CPY".
       COPY "LIBGB/GBSE1IN.CPY".
       OPEN-OSE1-FILE.
           PERFORM OPEN-IT.
           MOVE OSE-PATH TO E-FILE.
           OPEN INPUT OSE-FILE.
           IF IO-FG = 8
              GO TO OPEN-OSE1-FILE.
           IF IO-FG = 7
              CLOSE OSE-FILE
              GO TO OPEN-OSE1-FILE.
       READ-OSE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSE-PATH TO E-FILE.
           MOVE OSE1-KEY TO E-KEYX.
           READ OSE-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OSE1-FILE-NEXT.
       START-OSE1-FILE.
           PERFORM START-IT.
           MOVE OSE-PATH TO E-FILE.
           MOVE OSE1-KEY TO E-KEYX.
           START OSE-FILE KEY NOT < OSE1-KEY.
       CLOSE-OSE1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSE-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************************
       END-PROGRAM SECTION.
      ****************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

           IF FIRST-TIME = "Y"
              MOVE "NO RECORDS FOUND" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE "UP/UPMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN |MJD001
           DESTROY SE1534-SCREEN.
           DESTROY SE1534-WINDOW-HANDLE.
           EXIT PROGRAM.
