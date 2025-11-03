       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PHONE.
      ************************************************************
      *
      *  GLOBAL BORROWER / SCAN BY PHONE NUMBER
      *
      * CREATES A DYNAMIC UNIQUE WORK FILE IN /USR/DATA/R#/WK/ WITH JUST
      *  BW RECORDS THAT MATCH THE PHONE, IN THAT ORDER
      *
      * REV:
      *  BAH 071022 REGENCY, PR# 3071
      *  BAH 090521 ADDED BW-CELL-PHONE, SUPPORT PR# 3368
      *  BAH 090924 PAGE # WAS CHANGING BUT THE BORROWERS DIDNT, THE WK-KEY
      *             WAS NOT GETTING INITIALIZED TO 0 EACH TIME IT WAS BUILT
      * BAH 20191003 REMOVED ACCESS-CALL BWFILE
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WK-FILE ASSIGN WK-PATH
                  ORGANIZATION RELATIVE
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY WK-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-OWNBR             PIC 9(4) COMP-6.
           03  WK-SSNO              PIC 9(9) COMP-6.
           03  WK-LNAME             PIC X(16).
           03  WK-FNAME             PIC X(16).
           03  WK-DESC              PIC X(10).
           03  WK-PHONE             PIC 9(10).
           03  WK-ORGST             PIC XX.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)SP/PHONE.CBL S35 02/22/24-08:14:49 >".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  WK-KEY               PIC 9(5) VALUE 0.

       01  WK-PATH.
           03  WK-PATH-BASE          PIC X(09).
           03  FILLER                PIC X(01)   VALUE "/".
           03  WK-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(28)   VALUE "/WK/XXXXXX".

       01  ONE                      PIC S9     VALUE 1.
       01  SUB                      PIC 99.

       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-S.
               05  FLDSEL-S-SSNO      PIC X(11).
               05  FILLER             PIC XX.
               05  FLDSEL-S-NAME      PIC X(20).
               05  FILLER             PIC XX.
               05  FLDSEL-S-PHONE     PIC X(12).
               05  FILLER             PIC XX.
               05  FLDSEL-S-OWNBR     PIC 9(4).
               05  FILLER             PIC X(4).
               05  FLDSEL-S-ORGST     PIC XX.
               05  FILLER             PIC X(4).
               05  FLDSEL-S-DESC      PIC X(10).

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090 ".
           03  FILLER               PIC X(15)  VALUE
           "PGNO ENDMORE:F.".

       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
               05  D-S.
                   07  D-S-KEY      PIC X(11).
                   07  FILLER       PIC XX.
                   07  D-S-NAME     PIC X(20).
                   07  FILLER       PIC XX.
                   07  D-S-PHONE    PIC 999/999/9999 BLANK ZERO.
                   07  FILLER       PIC XX.
                   07  D-S-OWNBR    PIC Z(4).
                   07  FILLER       PIC X(4).
                   07  D-S-ORGST    PIC XX.
                   07  FILLER       PIC X(4).
                   07  D-S-DESC     PIC X(10).
           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 100.
       01  PAGE-KEY.
           03  FILLER                 OCCURS 100 TIMES.
               05  PAGE-KEY-N-X.
                   07  PAGE-KEY-N          PIC X(32).
                   07  PAGE-KEY-WK-KEY     PIC 9(5).

       01  A-NAME                   PIC X(16).

       01  GROUP-FLAG           PIC X(1)     VALUE SPACES.
       01  ENT-GROUP            PIC X(4)     VALUE SPACES.

       01  ALP-BI.
           03  ALP-BI123            PIC X(3).
           03  ALP-HYPHEN1          PIC X.
           03  ALP-BI45             PIC X(2).
           03  ALP-HYPHEN2          PIC X.
           03  ALP-BI6789           PIC X(4).

       01  NUM-BI                   PIC 9(9).
       01  FILLER                   REDEFINES NUM-BI.
           03  NUM-BI123            PIC X(3).
           03  NUM-BI45             PIC X(2).
           03  NUM-BI6789           PIC X(4).

       01  TEST-PHONE          PIC 9(10).
       01  TEST-PHONEX REDEFINES TEST-PHONE.
           03  TEST-AREA       PIC 9(3).
           03  FILLER          PIC X(7).

       01  BEG-BRANCH        PIC 9999.
       01  END-BRANCH        PIC 9999.

       01  WORK-PHONE          PIC 9(10).
       01  WORK-PHONEX REDEFINES WORK-PHONE.
           03  WORK-AREA       PIC 9(3).
           03  FILLER          PIC 9(7).

       01 ERRCD             PIC X.

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  PHONE-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/PHONE_DEF.CPY".
       COPY "LIBSP/PHONE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/PHONE_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
                WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************
       MAIN-PROGRAM SECTION.
      ******************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PHONE" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS PHONE-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE EXT-FILPATH-BASE     TO WK-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE  TO WK-PATH-MACHINE.
           MOVE WK-PATH TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

       ENTER-SCANKEY.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "EN N100PHONE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY.
           IF VDUNUM0 = 0
              GO TO ENTER-SCANKEY.

           MOVE VDUNUM0 TO WORK-PHONE.

       BR-01.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-SCANKEY.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           IF ( BEG-BRANCH = ZEROES )
              GO TO BR-01.

      *-----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-SCANKEY.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              GO TO BR-02.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       ENTER-SCANKEY-CONT.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           MOVE 0 TO PAGE-KEY-WK-KEY(PAGE-COUNT).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

      * BUILD DYNAMIC WK FILE ONLY INCLUDING MATCHING LAST 4
           PERFORM BUILD-WK-FILE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           MOVE PAGE-KEY-WK-KEY(CURRENT-PAGE) TO WK-KEY.
           PERFORM START-WK-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO DISPLAY-PAGE.

           IF SUB > 9
              GO TO DISPLAY-PAGE.

           MOVE WK-SSNO TO NUM-BI.
           IF NUM-BI = 0
              GO TO BUILD-PAGENO.

           PERFORM NUMBI-TO-ALPBI.
           MOVE ALP-BI TO D-S-KEY(SUB).

           MOVE WK-LNAME TO MESS.
           MOVE WK-FNAME TO A-NAME.
           PERFORM SET-WK-LNAME.
           MOVE MESS TO D-S-NAME(SUB)
           MOVE WK-PHONE TO D-S-PHONE(SUB)
           INSPECT D-S-PHONE(SUB) REPLACING ALL "/" BY "-"
           MOVE WK-OWNBR TO D-S-OWNBR(SUB)
           MOVE WK-ORGST TO D-S-ORGST(SUB).
           MOVE WK-DESC TO D-S-DESC(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
      D    STOP MESS.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-SCANKEY.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE WK-KEY TO PAGE-KEY-WK-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************
      *    SELECT FIELD FROM PAGES
      *************************************
       SELECT-FIELD-FROM-DISPLAY.
           MOVE "F7-EXIT" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.
      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF FLDSEL-STAT = "10"
              GO TO ENTER-SCANKEY.

           IF FLDSEL-STAT NOT = "00"
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO SELECT-FIELD-FROM-DISPLAY.

      *************************************
       BUILD-WK-FILE SECTION.
      *************************************
           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM OPEN-WK-FILE-OUTPUT.
           MOVE 0 TO WK-KEY.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO EXIT-BUILD-WK-FILE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO EXIT-BUILD-WK-FILE.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO BW-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.

           PERFORM GLOBAL-FIND-BW.

       END-OF-GROUP.

           PERFORM CLOSE-BW1-FILE.

           GO TO NEXT-GROUP-LOC.

       EXIT-BUILD-WK-FILE.
           EXIT.

      ****************************
       GLOBAL-FIND-BW SECTION.
      ****************************

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.

           PERFORM START-BW1-FILE.
       GLOBAL-FIND-NEXT.
           PERFORM READ-BW1-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              GO TO EXIT-GLOBAL-FIND-BW.

           PERFORM SEND-PROGRESS-INDICATOR.

      * HOME PHONE
           MOVE BW-PHONE TO TEST-PHONE.
           IF WORK-AREA = 0
              MOVE 0 TO TEST-AREA.
           IF WORK-PHONE = TEST-PHONE
              MOVE "HOME" TO WK-DESC
              MOVE BW-PHONE TO WK-PHONE
              PERFORM SAVE-BORROWER
              GO TO GLOBAL-FIND-NEXT.

      * WORK PHONE
           MOVE BW-EMPPHONE TO TEST-PHONE.
           IF WORK-AREA = 0
              MOVE 0 TO TEST-AREA.
           IF WORK-PHONE = TEST-PHONE
              MOVE "WORK" TO WK-DESC
              MOVE BW-EMPPHONE TO WK-PHONE
              PERFORM SAVE-BORROWER
              GO TO GLOBAL-FIND-NEXT.

      * CELL PHONE
           MOVE BW-CELL-PHONE TO TEST-PHONE
           IF WORK-AREA = 0
              MOVE 0 TO TEST-AREA.
           IF WORK-PHONE = TEST-PHONE
              MOVE "CELL" TO WK-DESC
              MOVE BW-CELL-PHONE TO WK-PHONE
              PERFORM SAVE-BORROWER
              GO TO GLOBAL-FIND-NEXT.


           PERFORM CHECK-MISC-PHONE.

      * CONTACT PHONES
           PERFORM CHECK-CONTACT-PHONE.

           GO TO GLOBAL-FIND-NEXT.

       EXIT-GLOBAL-FIND-BW.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

      ******************************
       CHECK-MISC-PHONE SECTION.
      ******************************
           MOVE 1 TO SUB.
       CHECK-MISC-NEXT.
           MOVE BW-MISC-PHONE(SUB) TO TEST-PHONE.
           IF WORK-AREA = 0
              MOVE 0 TO TEST-AREA.
           IF TEST-PHONE = WORK-PHONE
              MOVE BW-MISC-PHONE-CD(SUB) TO WK-DESC
              MOVE BW-MISC-PHONE(SUB) TO WK-PHONE
              PERFORM SAVE-BORROWER
              GO TO EXIT-CHECK-MISC-PHONE.
           ADD 1 TO SUB.
           IF SUB NOT > 6
              GO TO CHECK-MISC-NEXT.
       EXIT-CHECK-MISC-PHONE.
           EXIT.

      ******************************
       CHECK-CONTACT-PHONE SECTION.
      ******************************
           MOVE 1 TO SUB.
       CHECK-CONTACT-NEXT.
           MOVE BW-CONTACT-PHONE(SUB) TO TEST-PHONE.
           IF WORK-AREA = 0
              MOVE 0 TO TEST-AREA.
           IF TEST-PHONE = WORK-PHONE
              MOVE BW-CONTACT-DESC(SUB) TO WK-DESC
              MOVE BW-CONTACT-PHONE(SUB) TO WK-PHONE
              PERFORM SAVE-BORROWER
              GO TO EXIT-CHECK-CONTACT-PHONE.
           ADD 1 TO SUB.
           IF SUB NOT > 10
              GO TO CHECK-CONTACT-NEXT.
       EXIT-CHECK-CONTACT-PHONE.
           EXIT.

      *******************************
       SAVE-BORROWER SECTION.
      *******************************
           MOVE BW-SSNO TO WK-SSNO.
           MOVE BW-OWNBR TO WK-OWNBR.
           MOVE BW-LNAME TO WK-LNAME.
           MOVE BW-FNAME TO WK-FNAME.
           MOVE BW-STATE TO WK-ORGST.
           ADD 1 TO WK-KEY.
           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY PHONE-SCREEN UPON PHONE-WINDOW-HANDLE.
           MOVE PHONE-FORM-FIELDS TO WR-BUF.
           MOVE PHONE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE PHONE-HELP--FILE TO HELP-LINK-FILE.
           MOVE PHONE-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/PHONE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************
       MISC-PARAGRAPHS SECTION.
      ***************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-PROGRESS-INDICATOR.
           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE "SEL:B."              TO PROGID-LINE-LIST.
           MOVE "00"                   TO PROGID-ROUTE-BUF.
           MOVE "S"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

           PERFORM PROGID-DISPLAY.
       SEND-MORE-INFO.
              MOVE "MORE INFORMATION IS NOT AVAILABLE" TO MESS.
              PERFORM SEND-MESS.
       SEND-PHONE.
           MOVE TEL-1 TO TEL-ED1.
           MOVE TEL-2 TO TEL-ED2.
           MOVE TEL-3 TO TEL-ED3.
       SET-WK-LNAME.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST "                "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SET-BI-FNAME.
           INSPECT MESS REPLACING FIRST "  " BY "? ".
           INSPECT MESS REPLACING FIRST "                "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SET-CITY-STATE-ZIP.
           MOVE BW-CITY TO MESS.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST "  "
               BY BW-STATE.
           INSPECT MESS REPLACING FIRST "  " BY "? ".
           INSPECT MESS REPLACING FIRST "          "
               BY BW-ZIP.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SEND-LEGEND.
           MOVE "S  A600SEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       NUMBI-TO-ALPBI.
           MOVE NUM-BI123 TO ALP-BI123.
           MOVE NUM-BI45 TO ALP-BI45.
           MOVE NUM-BI6789 TO ALP-BI6789.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       ALPBI-TO-NUMBI.
           MOVE ALP-BI123 TO NUM-BI123.
           MOVE ALP-BI45 TO NUM-BI45.
           MOVE ALP-BI6789 TO NUM-BI6789.
           IF NUM-BI NOT NUMERIC
              MOVE 0 TO NUM-BI.

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-PATH TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE-OUTPUT.
      *    UNLOCK WK-FILE.
       OPEN-WK-FILE.
           PERFORM OPEN-IT.
           MOVE WK-PATH TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE.
      *    UNLOCK WK-FILE.
       READ-WK-FILE.
           PERFORM READ-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           READ WK-FILE.
           IF IO-FG = 8
              GO TO READ-WK-FILE.
       READ-WK-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           READ WK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK-FILE-NEXT.
       READ-WK-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           READ WK-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-WK-FILE-PREVIOUS.
       START-WK-FILE-LE.
           PERFORM START-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           START WK-FILE KEY <= WK-KEY.
      *    UNLOCK WK-FILE.
       START-WK-FILE-GREATER.
           PERFORM START-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           START WK-FILE KEY > WK-KEY.
      *    UNLOCK WK-FILE.
       START-WK-FILE.
           PERFORM START-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK-KEY.
      *    UNLOCK WK-FILE.
       WRITE-WK-FILE.
           PERFORM WRITE-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           WRITE WK-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-FILE.
      *    UNLOCK WK-FILE.
       REWRITE-WK-FILE.
           PERFORM REWRITE-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
              GO TO REWRITE-WK-FILE.
      *    UNLOCK WK-FILE.
       DELETE-WK-FILE.
           PERFORM DELETE-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           DELETE WK-FILE.
           IF IO-FG = 8
              GO TO DELETE-WK-FILE.
      *    UNLOCK WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM CLOSE-FILES.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           MOVE "SP/SPMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PHONE-SCREEN.
           DESTROY PHONE-WINDOW-HANDLE.
           EXIT PROGRAM.
