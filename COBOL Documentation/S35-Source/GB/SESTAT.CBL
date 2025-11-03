       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SESTAT.
       DATE-WRITTEN.    150119.
      ***********************************************************************
      *                 (GB)
      *  DESCRIPTION:   USER SECURITY PROFILE LISTING
      *                    * NEW SECURITY *
      *                    [SNFILE, SEFILE]
      *
      * REV:    
      * 150119 CS  PER JR'S EMAIL, PUT THE OLD UPSTAT BACK. PR#4678  SUPPORT
      *
      * KEC 2015.1112 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED USE OF SN-DIR & SE-DIR
      *  CS 190411 COMMENT WK-FILE, DONT SEE ITS USED
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
           "@(#)GB/SESTAT.CBL S34 05/24/21-11:48:56 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01SE.CPY".
       COPY "LIBGB/GB01SE_SQL.CPY".
       COPY "LIBGB/GBWSSE.CPY".
       COPY "LIBGB/GB01SN.CPY".
       COPY "LIBGB/GB01SN_SQL.CPY".
       COPY "LIBGB/GBWSSN.CPY".

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
       01  S1                   PIC 99       VALUE 0.
       01  LETTER-PROF          PIC X        VALUE SPACES.
       01  FIRST-TIME-PROF      PIC X        VALUE "Y".
       01  PROF                 PIC X        VALUE " ".
 
       
       01  ERRCD           PIC X.
       01  SECURITY-PROF   PIC X.
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
           03  H-TITLE            PIC X(41)    VALUE
           "- - SECURITY PROFILE STATUS REPORT - -".
           03  FILLER           PIC X(45)    VALUE " ".
 
 
       01  HEAD3.
           03  FILLER           PIC X(50) VALUE
           "PROFILE                            TRANSACTION    ".
           03  FILLER           PIC X(50) VALUE
           "DESCRIPTION                                       ".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
 
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  D-S-PROFILE      PIC X(26).            
           03  D-S-PROF REDEFINES D-S-PROFILE OCCURS 26.
               05  D-S-PROFS    PIC X. 
           03  FILLER           PIC X(9).
           03  D-S-TRAN         PIC X(10).
           03  FILLER           PIC X(05).
           03  D-S-DESC         PIC X(30).
           03  FILLER           PIC X(05).


       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(30).
           03  RANGE-SELECTION  PIC X(102).
 
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  SESTAT-WINDOW-HANDLE   PIC X(10).
       
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/SESTAT_DEF.CPY".
       COPY "LIBGB/SESTAT_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
 
      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
    
       SCREEN SECTION.
       COPY "LIBGB/SESTAT_SCN.CPY".


       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SN2-FILE.
           PERFORM CLOSE-SE1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ********************************************
       MAIN-MODULE SECTION.
      ********************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SESTAT" TO VDU-FORM-NAME.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-PROGRAM.                                                
       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SESTAT-WINDOW-HANDLE.

           PERFORM FORM-RESET.


           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.
       NEXT-SN1.   
           PERFORM READ-SN2-FILE-NEXT.
           IF IO-BAD   
              GO TO END-ROUTINE.

           IF SECURITY-PROF = " "
              PERFORM DETAIL-LINE-PROCESS-S
           ELSE
              PERFORM DETAIL-LINE-PROCESS-1-P.      

           GO TO NEXT-SN1.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS.


           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.
 
      ***********************************
       INITIALIZATION SECTION.
      ***********************************
       SETRP.
           MOVE SESTAT-QUEUE-POS TO Q-POS.
           MOVE SESTAT-COPIES-POS TO C-POS.
           MOVE SESTAT-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT-BAD       
              GO TO ENDIT.
           
       ENTER-PROF.     
           MOVE "E  A010PROF." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO SETRP.
           IF F3-KEY 
              MOVE "S  A010PROF." TO VDU
              MOVE " " TO SECURITY-PROF VDUBUF
              PERFORM SCREENIO
              GO TO OK-YN.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO ENTER-PROF.
           MOVE VDUBUF TO SECURITY-PROF.
           IF SECURITY-PROF < "A" GO TO ENTER-PROF.
           IF SECURITY-PROF > "Z" GO TO ENTER-PROF.
           
       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY GO TO ENTER-PROF.     
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.

           IF ACCEPT-BUF = "N" GO TO ENTER-PROF.     
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

      *    IF SECURITY-PROF = " "
      *       MOVE MKTEMP-DEFAULT TO MKTEMP-BUF 
      *       PERFORM MKTEMP-CALL
      *       MOVE MKTEMP-BUF TO WKPATH 
      *       PERFORM OPEN-WK-FILE-OUTPUT 
      *       PERFORM CLOSE-WK-FILE 
      *       PERFORM OPEN-WK-FILE.
          
           PERFORM OPEN-SN2-FILE.
           PERFORM OPEN-SE1-FILE.

           MOVE SPACES TO SN1-KEY.
           MOVE ZEROS TO SN-SEQ.

           PERFORM START-SN2-FILE.

           MOVE " " TO ERRCD.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "LISTING IN PROGRESS...................." TO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.
 
      *******************************************
       DETAIL-LINE-PROCESS-1-P SECTION.
      *******************************************
           MOVE SN-FORM TO SE-FORM.
           PERFORM READ-SE1-FILE.
           IF IO-BAD
              GO TO DT-LINE-PROCESS-1-P-EXIT.
           IF FIRST-TIME-PROF = "Y"     
              MOVE SECURITY-PROF TO PROF   
              PERFORM FIND-PROFILE-SUB. 
           IF SE-PROF(S1) NOT  = "Y"
              GO TO DT-LINE-PROCESS-1-P-EXIT.

           MOVE PROF     TO D-S-PROFS(1). 
           MOVE SN-FORM  TO D-S-TRAN. 
           MOVE SN-DESC  TO D-S-DESC.
              
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
     
       DT-LINE-PROCESS-1-P-EXIT.
           EXIT.

      *******************************************
       DETAIL-LINE-PROCESS-S SECTION.
      *******************************************
           MOVE SN-FORM  TO D-S-TRAN.
           MOVE SN-FORM  TO SE-FORM.  
           PERFORM READ-SE1-FILE.
           IF IO-BAD   
              MOVE "*** MISSING PROFILES ***" TO D-S-DESC 
              PERFORM WRITE-DETAIL-LINE
              GO TO DETAIL-LINE-PROCESS-S-EXIT.

           MOVE SN-DESC TO D-S-DESC.
           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 26
              IF SE-PROF(S1) = "Y"
                 PERFORM SET-LETTER-PROF   
                 MOVE LETTER-PROF TO D-S-PROFS(S1)
              END-IF
           END-PERFORM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
       DETAIL-LINE-PROCESS-S-EXIT.         
           EXIT.

      *******************************************
       WRITE-DETAIL-LINE SECTION.
      *******************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
 
      *******************************************
       HEAD-PAGE SECTION.
      *******************************************
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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
 
      *******************************************
       PRINT-RANGE-LINES SECTION.
      *******************************************

           MOVE "PROFILE"       TO RANGE-LINE.
           IF SECURITY-PROF = " "
              MOVE "ALL"        TO RANGE-SELECTION
           ELSE
              MOVE SECURITY-PROF   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
 
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO SESTAT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SESTAT-SYSDATE.
           DISPLAY SESTAT-SCREEN UPON SESTAT-WINDOW-HANDLE.
           MOVE SESTAT-FORM-FIELDS TO WR-BUF.
           MOVE SESTAT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SESTAT-HELP--FILE TO HELP-LINK-FILE.
           MOVE SESTAT-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/SESTAT_EVA.CPY".
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
       FIND-PROFILE-SUB.
           IF FIRST-TIME-PROF = "Y"
              MOVE "N" TO FIRST-TIME-PROF.
           EVALUATE PROF               
              WHEN 'A' MOVE 1 TO S1
              WHEN 'B' MOVE 2 TO S1
              WHEN 'C' MOVE 3 TO S1
              WHEN 'D' MOVE 4 TO S1
              WHEN 'E' MOVE 5 TO S1
              WHEN 'F' MOVE 6 TO S1
              WHEN 'G' MOVE 7 TO S1
              WHEN 'H' MOVE 8 TO S1
              WHEN 'I' MOVE 9 TO S1
              WHEN 'J' MOVE 10 TO S1
              WHEN 'K' MOVE 11 TO S1
              WHEN 'L' MOVE 12 TO S1
              WHEN 'M' MOVE 13 TO S1
              WHEN 'N' MOVE 14 TO S1
              WHEN 'O' MOVE 15 TO S1
              WHEN 'P' MOVE 16 TO S1
              WHEN 'Q' MOVE 17 TO S1
              WHEN 'R' MOVE 18 TO S1
              WHEN 'S' MOVE 19 TO S1
              WHEN 'T' MOVE 20 TO S1
              WHEN 'U' MOVE 21 TO S1
              WHEN 'V' MOVE 22 TO S1
              WHEN 'W' MOVE 23 TO S1
              WHEN 'X' MOVE 24 TO S1
              WHEN 'Y' MOVE 25 TO S1
              WHEN 'Z' MOVE 26 TO S1
              WHEN OTHER PERFORM INVALID-CODE.
       SET-LETTER-PROF.      
           EVALUATE S1
              WHEN  1 MOVE "A" TO LETTER-PROF
              WHEN  2 MOVE "B" TO LETTER-PROF
              WHEN  3 MOVE "C" TO LETTER-PROF
              WHEN  4 MOVE "D" TO LETTER-PROF
              WHEN  5 MOVE "E" TO LETTER-PROF
              WHEN  6 MOVE "F" TO LETTER-PROF
              WHEN  7 MOVE "G" TO LETTER-PROF
              WHEN  8 MOVE "H" TO LETTER-PROF
              WHEN  9 MOVE "I" TO LETTER-PROF
              WHEN 10 MOVE "J" TO LETTER-PROF
              WHEN 11 MOVE "K" TO LETTER-PROF
              WHEN 12 MOVE "L" TO LETTER-PROF
              WHEN 13 MOVE "M" TO LETTER-PROF
              WHEN 14 MOVE "N" TO LETTER-PROF
              WHEN 15 MOVE "O" TO LETTER-PROF
              WHEN 16 MOVE "P" TO LETTER-PROF
              WHEN 17 MOVE "Q" TO LETTER-PROF
              WHEN 18 MOVE "R" TO LETTER-PROF
              WHEN 19 MOVE "S" TO LETTER-PROF
              WHEN 20 MOVE "T" TO LETTER-PROF
              WHEN 21 MOVE "U" TO LETTER-PROF
              WHEN 22 MOVE "V" TO LETTER-PROF
              WHEN 23 MOVE "W" TO LETTER-PROF
              WHEN 24 MOVE "X" TO LETTER-PROF
              WHEN 25 MOVE "Y" TO LETTER-PROF
              WHEN 26 MOVE "Z" TO LETTER-PROF
              WHEN OTHER PERFORM INVALID-CODE.
       INVALID-CODE.
           MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS.

      **********************************************
       IO-ROUTINES SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBSNGS_SQL.CPY".
       COPY "LIBGB/GBSEGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBSE1RN.CPY".
       COPY "LIBGB/GBSN2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
 
      **********************************************
       END-PROGRAM SECTION.
      **********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

       EXIT-PROG.
           MOVE "GB/SEMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SESTAT-SCREEN.
           DESTROY SESTAT-WINDOW-HANDLE.
           EXIT PROGRAM.
