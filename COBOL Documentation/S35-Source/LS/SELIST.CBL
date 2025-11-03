       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SELIST.
       DATE-WRITTEN.    111710.
      ******************************************************************
      *                 (GB)
      *  DESCRIPTION:   USER SECURITY PROFILE LISTING
      *                    * NEW SECURITY *
      * REV:            NEW FILES  [SNFILE, SEFILE]
      *
      * 101117 CS                    *NEW*
      *            OPTION 'P' WILL RESEMBLE THE OLD UPLIST, WILL ALLOW FOR 1
      *            PROFILE OR ALL PROFILES. OPTION 'S' WILL PRINT EACH DIR/FORM
      *            AND THE 26 PROFILE VALUES (Y OR N). THIS REPORT WILL IDENTIFY
      *            IF THERE ARE SN RECORDS SET UP BUT NO CORRESPONDING SE
      *            (PROFILES)
      * KEC 2015.1112 REMOVED USE OF SE-DIR & SN-DIR
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       SELECT WK-FILE ASSIGN WKPATH
           ORGANIZATION INDEXED
           ACCESS DYNAMIC
           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
           RECORD KEY WK-KEY
           FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-LETTER-PROF   PIC X.
               05  WK-FORM          PIC X(6).
           03  WK-DESC              PIC X(30).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/SELIST.CBL S34 06/10/21-13:14:41 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01SE.CPY".
       COPY "LIBGB/GB01SE_SQL.CPY".
       COPY "LIBGB/GBWSSE.CPY".
       COPY "LIBGB/GB01SN.CPY".
       COPY "LIBGB/GB01SN_SQL.CPY".
       COPY "LIBGB/GBWSSN.CPY".
       01  WKPATH                 PIC X(30).

       01  WK-IFN                 PIC X(2) VALUE "WK".

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
       01  SUB                  PIC 99       VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  HOLD-CODE            PIC X        VALUE SPACES.
       01  S1                   PIC 99       VALUE 0.
       01  LETTER-PROF          PIC X        VALUE SPACES.
       01  FIRST-TIME-PROF      PIC X        VALUE "Y".
       01  PROF                 PIC X        VALUE " ".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  OPTION          PIC X.
           03  SECURITY-PROF   PIC X.
           03  BEG-TR          PIC X(10).
           03  END-TR          PIC X(10).
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
           03  FILLER           PIC X(41)    VALUE
           "- - USER SECURITY PROFILE FILE LIST - -".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD3.
           03  FILLER           PIC X(50) VALUE
           "PROFILE              TRANSACTION    DESCRIPTION".

       01  HEAD3A.
           03  FILLER           PIC X(57)  VALUE
           "      TRANS   DESC.                       PURC.      EXP.".
           03  H3-PROFS         OCCURS 26.
               05  FILLER       PIC X.
               05  H3-PROF      PIC X.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-PROFILE        PIC X.
           03  FILLER           PIC X(12).
           03  FILLER           PIC X(8).
           03  D-TRANS          PIC X(10).
           03  FILLER           PIC X(5).
           03  D-DESC           PIC X(30).
           03  FILLER           PIC X(66).

       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(6).
           03  D-S-TRAN         PIC X(6).
           03  FILLER           PIC X(2).
           03  D-S-DESC         PIC X(30).
           03  FILLER           PIC X(2).
           03  D-S-PURCHASABLE  PIC X.
           03  FILLER           PIC X(2).
           03  D-S-EXP-DATE     PIC 99/99/99.
           03  FILLER           PIC X.
           03  D-S-PROF         OCCURS 26.
               05  D-S-PROFS    PIC X.
               05  FILLER       PIC X.
           03  FILLER           PIC X(36).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(30).
           03  RANGE-SELECTION  PIC X(102).

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  SELIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/SELIST_DEF.CPY".
       COPY "LIBLS/SELIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/SELIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SN1-FILE.
           PERFORM CLOSE-SE1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *******************************************
       MAIN-MODULE SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SELIST" TO VDU-FORM-NAME.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO EXIT-PROG.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SELIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-SN1.
           PERFORM READ-SN1-FILE-NEXT.
           IF IO-BAD
              IF OPTION ="P" AND SECURITY-PROF = " "
                 PERFORM DT-PROCESS-P-PRINT
                   GO TO END-ROUTINE
              ELSE
                   GO TO END-ROUTINE.

           IF SN-FORM < BEG-TR OR SN-FORM > END-TR
              GO TO NEXT-SN1.

           IF OPTION = "S"
              PERFORM DETAIL-LINE-PROCESS-S
           ELSE
           IF SECURITY-PROF NOT = " "
              PERFORM DETAIL-LINE-PROCESS-1-P
           ELSE
              PERFORM DETAIL-LINE-PROCESS-P.

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
           MOVE SELIST-QUEUE-POS TO Q-POS.
           MOVE SELIST-COPIES-POS TO C-POS.
           MOVE SELIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT-BAD
              GO TO ENDIT.

       ENTER-OPT.
           MOVE "E  A010OPT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO SETRP.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO ENTER-OPT.

           MOVE VDUBUF TO OPTION.
           IF NOT (OPTION = "P" OR "S")
              GO TO ENTER-OPT.

       ENTER-PROF.
           IF OPTION NOT = "P"
              GO TO ENTER-TR1.

           MOVE "E  A010PROF." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO SETRP.
           IF F3-KEY GO TO ENTER-ALL-PROF.
           IF F4-KEY GO TO ENTER-OPT.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO ENTER-PROF.
           MOVE VDUBUF TO SECURITY-PROF.
           IF SECURITY-PROF < "A" GO TO ENTER-PROF.
           IF SECURITY-PROF > "Z" GO TO ENTER-PROF.

           GO TO ENTER-TR1.
       ENTER-ALL-PROF.
           MOVE "S  A020PROF." TO VDU.
           MOVE " " TO SECURITY-PROF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-TR1.
           MOVE "E  A060TR1." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO SETRP.
           IF F3-KEY GO TO ENTER-ALL-TR.
           IF UP-KEY GO TO ENTER-PROF.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
               GO TO ENTER-TR1.
           MOVE VDUBUF TO BEG-TR.

       ENTER-TR2.
           MOVE "E  A060TR2." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO SETRP.
           IF UP-KEY GO TO ENTER-TR1.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
               GO TO ENTER-TR1.

           MOVE VDUBUF TO END-TR.

           IF END-TR = "          " GO TO ENTER-TR2.
           IF BEG-TR > END-TR
              MOVE " INVALID TRANSACTION RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TR1.
           GO TO OK-YN.
       ENTER-ALL-TR.
           MOVE "S  A060TR1." TO VDU.
           MOVE "          " TO BEG-TR VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A060TR2." TO VDU.
           MOVE "ZZZZZZ" TO END-TR VDUBUF.
           PERFORM SCREENIO.

           GO TO OK-YN.
       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY GO TO ENTER-TR2.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.

           IF ACCEPT-BUF = "N" GO TO ENTER-OPT.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

           IF SECURITY-PROF = " "
              MOVE MKTEMP-DEFAULT TO MKTEMP-BUF
              PERFORM MKTEMP-CALL
              MOVE MKTEMP-BUF TO WKPATH
              PERFORM OPEN-WK-FILE-OUTPUT
              PERFORM CLOSE-WK-FILE
              PERFORM OPEN-WK-FILE.


           PERFORM OPEN-SN1-FILE.
           PERFORM OPEN-SE1-FILE.
           MOVE BEG-TR TO SN-FORM.

           PERFORM START-SN1-FILE.
           MOVE " " TO ERRCD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.


           IF EXT-ROUTE-BUF = "00"
              MOVE "LISTING IN PROGRESS...................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ***********************************
       DETAIL-LINE-PROCESS-P SECTION.
      ***********************************

           MOVE SN-FORM   TO SE-FORM.
           PERFORM READ-SE1-FILE.
           IF IO-BAD
              GO TO DETAIL-LINE-PROCESS-P-EXIT.

           PERFORM VARYING S1 FROM 2 BY 1 UNTIL S1 > 26
              IF SE-PROF(S1) = "Y"
                  PERFORM SET-LETTER-PROF
                  MOVE LETTER-PROF      TO WK-LETTER-PROF
                  MOVE SE-FORM          TO WK-FORM
                  MOVE SN-DESC          TO WK-DESC
                  PERFORM WRITE-WK-FILE
                  IF IO-BAD
                     IF E-CODE = "22"
                        PERFORM REWRITE-WK-FILE
                     ELSE
                        GO TO IO-ERROR
                     END-IF
                  END-IF
              END-IF
           END-PERFORM.

       DETAIL-LINE-PROCESS-P-EXIT.
           EXIT.

      ***********************************
       DETAIL-LINE-PROCESS-1-P SECTION.
      ***********************************
           MOVE SN-FORM TO SE-FORM.
           PERFORM READ-SE1-FILE.
           IF IO-BAD
              GO TO DT-LINE-PROCESS-1-P-EXIT.

           IF FIRST-TIME-PROF = "Y"
              MOVE SECURITY-PROF TO PROF
              PERFORM FIND-PROFILE-SUB.

           IF SE-PROF(S1) NOT  = "Y"
              GO TO DT-LINE-PROCESS-1-P-EXIT.

           MOVE PROF TO  D-PROFILE.
           MOVE SN-FORM  TO D-TRANS.
           MOVE SN-DESC  TO D-DESC.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       DT-LINE-PROCESS-1-P-EXIT.
           EXIT.

      ***********************************
       DETAIL-LINE-PROCESS-S SECTION.
      ***********************************
           MOVE SN-FORM  TO D-S-TRAN.
           MOVE SN-FORM  TO SE-FORM.
           PERFORM READ-SE1-FILE.
           IF IO-BAD
              MOVE "*** MISSING PROFILES ***" TO D-S-DESC
              PERFORM WRITE-DETAIL-LINE
              GO TO DETAIL-LINE-PROCESS-S-EXIT.

           MOVE SN-DESC        TO D-S-DESC.
           MOVE SE-EXP-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO D-S-EXP-DATE.
           MOVE SE-PURCHASABLE TO D-S-PURCHASABLE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 26
              MOVE SE-PROF(SUB) TO D-S-PROFS(SUB)
           END-PERFORM.

           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

       DETAIL-LINE-PROCESS-S-EXIT.
           EXIT.

      ***********************************
       DT-PROCESS-P-PRINT SECTION.
      ***********************************
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
           MOVE SPACES TO WK-KEY.
           PERFORM START-WK-FILE.
       NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO DT-PROCESS-P-PRINT-EXIT.

           MOVE WK-LETTER-PROF TO D-PROFILE.
           MOVE WK-FORM TO D-TRANS.
           MOVE WK-DESC TO D-DESC.

           IF HOLD-CODE NOT = WK-LETTER-PROF
              MOVE WK-LETTER-PROF TO HOLD-CODE
              MOVE 99 TO LN-FEED.

           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-WK.

       DT-PROCESS-P-PRINT-EXIT.
           EXIT.

      ***********************************
       WRITE-DETAIL-LINE SECTION.
      ***********************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ***********************************
       HEAD-PAGE SECTION.
      ***********************************
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

           IF OPTION = "S"
              PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 26
                 PERFORM SET-LETTER-PROF
                 MOVE LETTER-PROF TO H3-PROF(S1)
              END-PERFORM
              MOVE HEAD3A TO PR-REC
           ELSE
              MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ***********************************
       PRINT-RANGE-LINES SECTION.
      ***********************************
           MOVE 2 TO LN-FEED.
           MOVE "OPTION"        TO RANGE-LINE.
           MOVE OPTION          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PROFILE"       TO RANGE-LINE.
           IF SECURITY-PROF = " "
              MOVE "ALL"        TO RANGE-SELECTION
           ELSE
              MOVE SECURITY-PROF   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING TRANSACTION" TO RANGE-LINE.
           MOVE BEG-TR                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING TRANSACTION" TO RANGE-LINE.
           MOVE END-TR TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO SELIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SELIST-SYSDATE.
           DISPLAY SELIST-SCREEN UPON SELIST-WINDOW-HANDLE.
           MOVE SELIST-FORM-FIELDS TO WR-BUF.
           MOVE SELIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SELIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE SELIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/SELIST_EVA.CPY".
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

      *********************************************
       IO-ROUTINES SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBSNGS_SQL.CPY".
       COPY "LIBGB/GBSEGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBSE1RN.CPY".
       COPY "LIBGB/GBSN1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************************
       END-PROGRAM SECTION.
      *********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           MOVE WKPATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
           MOVE "GB/SEMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SELIST-SCREEN.
           DESTROY SELIST-WINDOW-HANDLE.
           EXIT PROGRAM.
