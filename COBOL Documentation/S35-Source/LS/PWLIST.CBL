       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PWLIST.
       AUTHOR.             BLF.
       DATE-WRITTEN.  10-15-07.
      ******************************************************************
      *    DIR :  LP
      *    DESC:  PASSWORD FILE LIST
      *           (THIS PROGRAM DOES NOT LIST PASSWORDS)
      *
      *  REV:
      *  BLF 071026 ADDED OPTION TO PRINT PASSWORD EXCEPTIONS FROM
      *             PEFILE, REGACC PR# 188
      *   CS 110426 IF 'N' TO EXCEPTION, DIR/PGM WERE BLANK,SEQ WAS 0 BECAUSE
      *             WAS MOVING PE-DIR/PGM/SEQ TO DETAIL-LINE. IF 'Y' TO EXCEPT
      *             PRINTED PE-DIR/PGM/SEQ AND PW-DESC,ETC. THE DETAIL LINE
      *             WAS INACCURATE. THE REPORT WAS ONLY ACCURATE IF YOU SAID
      *             'Y' AND THE PEFILE HAD NO RECORDS
      * KEC 2015.1130 FIXED TO NOT USE PE-DIR & PW-DIR ANYMORE.
      * BAH 181023 GLOBAL PEFILE
      * BAH 20190930 REMOVED ACCESS-CALL PEFILE
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

      ******************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/PWLIST.CBL S35 02/23/24-10:15:58 >".
       COPY "LIBGB/GB01PW.CPY".
       COPY "LIBGB/GB01PW_SQL.CPY".
       COPY "LIBGB/GBWSPW.CPY".
       COPY "LIBGB/GB01PE.CPY".
       COPY "LIBGB/GB01PE_SQL.CPY".
       COPY "LIBGB/GBWSPE.CPY".
       01  PR-IFN               PIC X(2)    VALUE "PR".

      *----------------------------------------------------------------
      *   LINE WORKERS
      *----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 3.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      *----------------------------------------------------------------
      *   MISC WORKERS
      *----------------------------------------------------------------
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".
       01  ERRCD                PIC X.
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      *----------------------------------------------------------------
      * RANGE PARAMETERS
      *----------------------------------------------------------------
       01  BEG-PGM              PIC X(10).
       01  BEG-SEQ              PIC 99.
       01  END-SEQ              PIC 99.
       01  BEG-REQ              PIC X.
       01  PROFILE              PIC X.
       01  PRINT-EXC-FG         PIC X.

       01  ACCEPT-BUF          PIC X.

      *----------------------------------------------------------------
      *     P R I N T   B U F F E R S
      *----------------------------------------------------------------

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
           03  FILLER           PIC X(35)    VALUE " ".
           03  FILLER           PIC X(21)    VALUE
           "PASSWORD FILE LISTING".
           03  FILLER           PIC X(56)    VALUE " ".

       01  HEAD3.
           03  FILLER           PIC X(4)  VALUE SPACES. 
           03  FILLER           PIC X(10)  VALUE "PROGRAM   ".
           03  FILLER           PIC X(3).
           03  FILLER           PIC X(3)   VALUE "SEQ".
           03  FILLER           PIC X(3).
           03  FILLER           PIC X(8)   VALUE "REQUIRED".
           03  FILLER           PIC X(4).
           03  FILLER           PIC X(30)  VALUE "DESCRIPTION".
           03  FILLER           PIC X(5).
           03  FILLER           PIC X(12)  VALUE "USER PROFILE".
           03  FILLER           PIC X(3).
           03  FILLER           PIC X(12)  VALUE "ASK PASSWORD".
           03  FILLER           PIC X(3).
           03  FILLER           PIC X(14)  VALUE "EXCEPTION FLAG".
           03  FILLER           PIC X(3).
           03  FILLER           PIC X(15)  VALUE "EXCEPTION LOGIN".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(03).
           03  D-PGM            PIC X(10).
           03  FILLER           PIC X(3).
           03  D-SEQ            PIC Z9.
           03  FILLER           PIC X(5).
           03  D-REQ            PIC X.
           03  FILLER           PIC X(11).
           03  D-DESC           PIC X(30).
           03  FILLER           PIC X(5).
           03  D-PROFILE        PIC X.
           03  FILLER           PIC X(14).
           03  D-ASK-FG         PIC X.
           03  FILLER           PIC X(14).
           03  D-EXCEPTION-FG   PIC X.
           03  FILLER           PIC X(16).
           03  D-LOGIN          PIC X(8).
           03  FILLER           PIC X(7).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-DESCRIPTION        PIC X(20).
           03  RANGE-DATA-AREA          PIC X(10).
           03  RANGE-DATA-01     REDEFINES RANGE-DATA-AREA.
               05  RANGE-SEQ            PIC Z9.
               05  FILLER               PIC X(8).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  PWLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/PWLIST_DEF.CPY".
       COPY "LIBLS/PWLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/PWLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-PW1-FILE.
           PERFORM CLOSE-PE1-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************************************************
       MAIN-MODULE SECTION.
      *****************************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PWLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS PWLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE SPACES TO PW-PGM. 
           MOVE 0      TO PW-SEQ.

           PERFORM START-PW1-FILE.

       NEXT-PW.
           PERFORM READ-PW1-FILE-NEXT.
           IF IO-FG = 9
               GO TO END-ROUTINE.

           IF BEG-PGM NOT = "-- ALL -- "
              IF PW-PGM NOT = BEG-PGM
                 GO TO NEXT-PW.

           IF PW-SEQ < BEG-SEQ
              GO TO NEXT-PW.

           IF PW-SEQ > END-SEQ
              GO TO NEXT-PW.

           IF BEG-REQ NOT = "B"
              IF BEG-REQ NOT = PW-PASSYN
                 GO TO NEXT-PW.

           IF PROFILE NOT = "*"
              IF PW-USER-PROFILE NOT = PROFILE
                 GO TO NEXT-PW.

           IF PRINT-EXC-FG = "Y"
              MOVE PW1-KEY TO PE-PW-KEY
              MOVE SPACES TO PE-LOGIN
              PERFORM START-PE1-FILE
              PERFORM READ-PE1-FILE-NEXT
              IF (IO-BAD OR PE-PW-KEY NOT = PW1-KEY)
                 MOVE SPACES TO PE-LOGIN.

           PERFORM DETAIL-LINE-PROCESS.

       NEXT-PE.
           IF PRINT-EXC-FG = "Y"
              PERFORM READ-PE1-FILE-NEXT
              IF IO-GOOD
                 IF PE-PW-KEY = PW1-KEY
                    MOVE PE-LOGIN TO D-LOGIN
                    PERFORM WRITE-DETAIL-LINE
                    GO TO NEXT-PE.

           GO TO NEXT-PW.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************

           MOVE " " TO ERRCD.
       SETRP.
           MOVE PWLIST-QUEUE-POS TO Q-POS.
           MOVE PWLIST-COPIES-POS TO C-POS.
           MOVE PWLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00" GO TO ENDIT.

       
       ENTRY-F1-RESET.

           MOVE " " TO ERRCD.

       PROGRAM-01.
           MOVE "E  A100PGM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTRY-F1-RESET.
           IF VDUSTATUS = "1U" GO TO ENTRY-F1-RESET.
           IF VDUSTATUS = "16" GO TO ALL-PROGRAMS.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO PROGRAM-01.
           MOVE VDUBUF TO BEG-PGM.
           GO TO SEQ-01.

       ALL-PROGRAMS.
           MOVE "S  A100PGM." TO VDU.
           MOVE "-- ALL -- " TO VDUBUF BEG-PGM.
           PERFORM SCREENIO.
      
       SEQ-01.
           MOVE "ENNN020SEQ1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTRY-F1-RESET.
           IF VDUSTATUS = "16" GO TO ALL-SEQ.
           IF VDUSTATUS = "1U" GO TO PROGRAM-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO SEQ-01.
           MOVE VDUNUM0 TO BEG-SEQ.
       SEQ-02.
           MOVE "ENNN020SEQ2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTRY-F1-RESET.
           IF VDUSTATUS = "1U" GO TO SEQ-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO SEQ-02.
           MOVE VDUNUM0 TO END-SEQ.
           GO TO REQ-01.
       ALL-SEQ.
           MOVE "SNNN020SEQ1." TO VDU.
           MOVE "00" TO BEG-SEQ VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020SEQ2." TO VDU.
           MOVE "99" TO END-SEQ VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       REQ-01.
           MOVE "E  A010REQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTRY-F1-RESET.
           IF VDUSTATUS = "16" GO TO ALL-REQ.
           IF VDUSTATUS = "1U" GO TO SEQ-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO REQ-01.
           IF NOT (VDUBUF = "Y" OR "N" OR "B")
              GO TO REQ-01.
           MOVE VDUBUF TO BEG-REQ.
           GO TO PROFILE-01.
       ALL-REQ.
           MOVE "S  A010REQ." TO VDU.
           MOVE "*" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "B" TO BEG-REQ.
      *----------------------------------------------------------------
       PROFILE-01.
           MOVE "E  A010PRO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTRY-F1-RESET.
           IF VDUSTATUS = "16" GO TO ALL-PROFILES.
           IF VDUSTATUS = "1U" GO TO REQ-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO PROFILE-01.
           MOVE VDUBUF TO PROFILE.
           GO TO OK-YN.
        ALL-PROFILES.
           MOVE "S  A010PRO." TO VDU.
           MOVE "*" TO VDUBUF PROFILE.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       EXCEPTIONS-YN.
           MOVE "E  A010EXC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTRY-F1-RESET.
           IF VDUSTATUS = "1U" GO TO PROFILE-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO EXCEPTIONS-YN.
           IF NOT (VDUBUF = "Y" OR "N")
              GO TO EXCEPTIONS-YN.
           MOVE VDUBUF TO PRINT-EXC-FG.
      *----------------------------------------------------------------
       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO EXCEPTIONS-YN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO ENTRY-F1-RESET.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.


           PERFORM OPEN-PW1-FILE.

           IF PRINT-EXC-FG = "Y"
              PERFORM OPEN-PE1-FILE.

           MOVE SPACES TO PE-PGM PE-LOGIN.
           MOVE 0 TO PE-SEQ.

           PERFORM START-PE1-FILE.

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

      *****************************************************************
       DETAIL-LINE-PROCESS SECTION.
      *****************************************************************

           IF FIRST-TIME = "Y"
              PERFORM HEAD-PAGE
              MOVE "N" TO FIRST-TIME.

           MOVE PW-PGM          TO D-PGM.
           MOVE PW-SEQ          TO D-SEQ.

           MOVE PW-PASSYN       TO D-REQ.
           MOVE PW-DESC         TO D-DESC.
           MOVE PW-USER-PROFILE TO D-PROFILE.
           MOVE PW-ASK-FG       TO D-ASK-FG.
           MOVE PW-EXCEPTION-FG TO D-EXCEPTION-FG.
           IF PRINT-EXC-FG = "Y"
              IF PE-PW-KEY = PW1-KEY
                 MOVE PE-LOGIN        TO D-LOGIN.

           PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *****************************************************************
       HEAD-PAGE SECTION.
      *****************************************************************
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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 3 TO LN-COUNT.

      *****************************************************************
       PRINT-RANGE-LINES SECTION.
      *****************************************************************
           MOVE 2 TO LN-FEED.
      
           MOVE "PROGRAM           : " TO RANGE-DESCRIPTION.
           MOVE BEG-PGM                    TO RANGE-DATA-AREA.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGINNING SEQUENCE: " TO RANGE-DESCRIPTION.
           MOVE BEG-SEQ                TO RANGE-SEQ.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING SEQUENCE   : " TO RANGE-DESCRIPTION.
           MOVE END-SEQ                TO RANGE-SEQ.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "USER PROFILE      : " TO RANGE-DESCRIPTION.
           MOVE PROFILE                TO RANGE-DATA-AREA.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "PRINT REQUIRED    : " TO RANGE-DESCRIPTION.
           MOVE BEG-REQ                TO RANGE-DATA-AREA.
           PERFORM WRITE-DETAIL-LINE.
     
           MOVE "PRINT EXCEPTIONS  : " TO RANGE-DESCRIPTION.
           MOVE PRINT-EXC-FG           TO RANGE-DATA-AREA.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO PWLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PWLIST-SYSDATE.
           DISPLAY PWLIST-SCREEN UPON PWLIST-WINDOW-HANDLE.
           MOVE PWLIST-FORM-FIELDS TO WR-BUF.
           MOVE PWLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE PWLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE PWLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/PWLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC SECTION.
      *****************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      *****************************************************************
       IO-ROUTINE SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBPWGS_SQL.CPY".
       COPY "LIBGB/GBPEGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBPW1RN.CPY".
       COPY "LIBGB/GBPE1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *****************************************************************
       END-PROGRAM SECTION.
      *****************************************************************
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
           DESTROY PWLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
