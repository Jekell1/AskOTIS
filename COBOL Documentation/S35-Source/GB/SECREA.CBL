       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SECREA.
       DATE-WRITTEN.    101202.
      ***********************************************************************
      *                 (GB)
      *  DESCRIPTION:   CREATE SE PROFILES FOR EACH SN ENTRY
      *                    * NEW SECURITY *
      * REV:            NEW FILES  [SNFILE, SEFILE]
      *
      *        READS SNFILE, FOR EACH ENTRY IN THE SN, IF THERE IS NOT AN
      *        ENTRY IN THE SEFILE, IT CREATES IT. IF THE SE HAS A DIFFERENT
      *        PURCHASEABLE STATUS, IT OVERWRITES IT TO MATCH THE SNFILE.
      *                                                  PR# NEWSECURITY
      * 101117 CS NEW
      * 150105 CS ADDED OPTION FOR REPORT ONLY, WHEN 'Y' PRODUCES REPORT OF  
      *           FORMS IN SECURITY NAME(SN) BUT NOT IN SECURITY PROFILE(SE), 
      *           WHEN 'Y' THE SE RECORD IS NOT CREATED.  ALSO ADDED AN OPTION
      *           TO SET DEFAULT ACCESS(SEFILE) FOR THE MISSING FORM
      *                                                     PR 4678 SUPPORT
      * KEC 2015.1112 REMOVED USE OF SN-DIR & SE-DIR
      * BAH 2017.0323 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2022.0218 CALL NEW AUDIT/CHANGE PROGRAM, AUDITW, #1542
      ***********************************************************************

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
           "@(#)GB/SECREA.CBL A30 11/13/15-09:15:05 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01SE.CPY".
       COPY "LIBGB/GB01SE_SQL.CPY".
       COPY "LIBGB/GBWSSE.CPY".
       COPY "LIBGB/GB01SN.CPY".
       COPY "LIBGB/GB01SN_SQL.CPY".
       COPY "LIBGB/GBWSSN.CPY".

       COPY "LIBGB/GB01SE.CPY" REPLACING LEADING "SE" BY "SEOR".

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
       01  CHANGE-TYPE          PIC X        VALUE " ".

       01  ERRCD           PIC X.

       01  REPORT-OPTION       PIC X.
       01  DEFAULT-ACCESS      PIC X    VALUE "N".
       01  ACCEPT-BUF          PIC X    VALUE " ".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE            PIC X(52)    VALUE
           "- UPDATE SECURITY PROFILES FROM SECURITY NAME FILE -".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD3.
           03  FILLER           PIC X(43) VALUE
           "      FORM    DESC.                        ".
           03  FILLER           PIC X(26) VALUE
           "PURCH.  EXPIRES   PROFILES".


       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  FILLER         PIC X(6).
           03  D-FORM         PIC X(6).
           03  FILLER         PIC X(2).
           03  D-DESC         PIC X(30).
           03  FILLER         PIC X(2).
           03  D-PURCHASABLE  PIC X.
           03  FILLER         PIC X(3).
           03  D-EXP-DATE     PIC 99/99/99.
           03  FILLER         PIC X(3).
           03  D-PROFILE      PIC X(26).
           03  FILLER         PIC X(3).
           03  D-TYPE         PIC X(19).


       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(30).
           03  RANGE-SELECTION  PIC X(102).

       COPY "LIBCL/AUDITWW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  SECREA-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/SECREA_DEF.CPY".
       COPY "LIBGB/SECREA_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGB/SECREA_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SN1-FILE.
           PERFORM CLOSE-SE1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **********************************************
       MAIN-MODULE SECTION.
      **********************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SECREA" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SECREA-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      D    STOP MESS.
           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-SN1.
           PERFORM READ-SN1-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           MOVE SN-FORM  TO SE-FORM.
           PERFORM READ-SE1-FILE.
           IF IO-BAD
              IF REPORT-OPTION = "Y"
                 MOVE "S" TO CHANGE-TYPE
                 GO TO DETAIL-LINE-PRINT
              ELSE
                 GO TO CREATE-SE.

      *SE EXISTS & PURCHASABLE OPTION MATCHES
           IF SE-PURCHASABLE = SN-PURCHASABLE
              GO TO NEXT-SN1.

           MOVE SPACES TO CHANGE-TYPE.
       UPDATE-SE.
           IF REPORT-OPTION NOT = "Y"
              MOVE SE-REC         TO SEOR-REC
              MOVE SN-PURCHASABLE TO SE-PURCHASABLE
              PERFORM REWRITE-SE1-FILE
              MOVE "C"            TO CHANGE-TYPE AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW
              MOVE "N"            TO AUDITW-ENTER-TICKET
              GO TO DETAIL-LINE-PRINT
           ELSE
              MOVE "P"            TO CHANGE-TYPE
              GO TO DETAIL-LINE-PRINT.
  
       CREATE-SE.
           MOVE SN-FORM   TO SE-FORM.
           MOVE SN-PURCHASABLE           TO SE-PURCHASABLE.
           MOVE ZEROS                    TO SE-EXP-DATE.
           IF DEFAULT-ACCESS = "N"
               MOVE "YNNNNNNNNNNNNNNNNNNNNNNNNN" TO SE-PROFILE 
           ELSE
               MOVE "YYYYYYYYYYYYYYYYYYYYYYYYYY" TO SE-PROFILE.
           MOVE "N"                      TO CHANGE-TYPE.
           PERFORM WRITE-SE1-FILE.
           IF IO-BAD
              GO TO IO-ERROR
           ELSE
              MOVE SE-REC         TO SEOR-REC
              MOVE "A"    TO AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW
              MOVE "N"    TO AUDITW-ENTER-TICKET.

       DETAIL-LINE-PRINT.
           MOVE SN-FORM     TO D-FORM.
           MOVE SN-DESC     TO D-DESC.
           MOVE SN-PURCHASABLE TO D-PURCHASABLE.
           IF REPORT-OPTION NOT = "Y"
              MOVE SE-EXP-DATE    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY    TO D-EXP-DATE 
              MOVE SE-PROFILE     TO D-PROFILE.
           IF CHANGE-TYPE = "C"
               MOVE "*PURCH. HAS CHANGED*"     TO D-TYPE
           ELSE
           IF CHANGE-TYPE = "P"
               MOVE "PURCH. WILL CHANGE WITH UPDATE" TO D-TYPE
           ELSE
           IF CHANGE-TYPE = "S"
               MOVE SPACES TO D-TYPE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-SN1.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              GO TO END-PROGRAM.                                   

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
       SETRP.
           MOVE SECREA-QUEUE-POS TO Q-POS.
           MOVE SECREA-COPIES-POS TO C-POS.
           MOVE SECREA-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT-BAD
              GO TO ENDIT.

       REPORT.            
           MOVE " " TO ERRCD.
           MOVE "E  A010RPT."    TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO REPORT.
           MOVE VDUBUF TO REPORT-OPTION.    

           IF NOT (REPORT-OPTION = "Y" OR "N")
              GO TO REPORT.           

           IF REPORT-OPTION = "Y"
              GO TO OK-YN.

       SET-DEFAULT-ACCESS. 
           MOVE "E  A010ACCESS." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO SET-DEFAULT-ACCESS.
           MOVE VDUBUF TO DEFAULT-ACCESS.
           IF NOT (VDUBUF = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO SET-DEFAULT-ACCESS.

           MOVE VDUBUF TO DEFAULT-ACCESS.
       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.

           IF ACCEPT-BUF = "N" GO TO OK-YN.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

           PERFORM OPEN-SN1-FILE.
           PERFORM OPEN-SE1-FILE.

           MOVE " "    TO SN-FORM.

           PERFORM START-SN1-FILE.

           MOVE " " TO ERRCD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
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
           MOVE REPORT-OPTION  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "DEFAULT ACCESS" TO RANGE-LINE.
           MOVE DEFAULT-ACCESS   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO SECREA-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SECREA-SYSDATE.
           DISPLAY SECREA-SCREEN UPON SECREA-WINDOW-HANDLE.
           MOVE SECREA-FORM-FIELDS TO WR-BUF.
           MOVE SECREA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SECREA-HELP--FILE TO HELP-LINK-FILE.
           MOVE SECREA-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/SECREA_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       MISC SECTION.
      ****************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       UPDATE-AUDITW.
           MOVE FORM-PATHNAME  TO FORM-PATH.
           MOVE FORM-PROGX     TO AUDITW-PROGRAM.
           MOVE "AUDITW"       TO FORM-NAM.
           MOVE SEOR-REC       TO AUDITW-ORIG-REC.
           MOVE SE-REC         TO AUDITW-CHG-REC.
           MOVE "SEFILE"       TO AUDITW-FILE-NAME.
           CALL FORM-PATH USING FORM-PATH AUDITW-WORKERS.
           CANCEL FORM-PATH.
       INVALID-CODE.
           MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS.


      ****************************************************
       IO-ROUTINES SECTION.
      ****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBSNGS_SQL.CPY".
       COPY "LIBGB/GBSEGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBSE1IN.CPY".
       COPY "LIBGB/GBSN1RN.CPY".
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
           MOVE "GB/SEMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SECREA-SCREEN.
           DESTROY SECREA-WINDOW-HANDLE.
           EXIT PROGRAM.
