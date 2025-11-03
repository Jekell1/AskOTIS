       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LAST4.
      ************************************************************
      *
      *  GLOBAL BORROWER / SCAN BY LAST 4 DIGITS OF SSNO
      *
      * CREATES A DYNAMIC UNIQUE WORK FILE IN ../../WK/ WITH JUST
      *  BI RECORDS THAT MATCH THE LAST 4 SSNO, IN THAT ORDER
      *
      * REV:
      *  BAH 040216 WORLD, PR# 339
      *  BAH 120605 WAS NOT GOING TO A PAGE 2 IF NECESSARY, JUDY
      *  20170927 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *  20211117 BAH FIXED DISPLAY OF ACCTNO
      *  20240716 MB  PERFORMANCE TUNING -   S35Q-90
      *              CREATED STORED PROCEDURE TO RETRIEVE BIFILE DATA.  
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WK-FILE ASSIGN WK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-SSNO              PIC 9(9) COMP-6.
               05  WK-ACCTNO            PIC 9(8) COMP-6.
               05  WK-OWNBR             PIC 9(4) COMP-6.
           03  WK2-KEY.
               05  WK-LNAME             PIC X(16).
               05  WK-FNAME             PIC X(16).
           03  WK-ENTDATE               PIC 9(8) COMP-6.
           03  WK-POFFDATE              PIC 9(8) COMP-6.


      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)SP/LAST4.CBL S34 06/19/21-15:25:39 >".
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-SSNO4                  PIC X(4).
       01  PASS-LASTNAME               PIC X(35).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  BI1-CONN-OPEN-SW          PIC X        VALUE "N".

       01  WK-PATH.
           03  WK-PATH-BASE          PIC X(09).
           03  FILLER                PIC X(01)   VALUE "/".
           03  WK-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(04)   VALUE "/WK/".
           03  FILLER                PIC X(06)   VALUE "XXXXXX".
           03  FILLER                PIC X(12).


       01  ONE                      PIC S9     VALUE 1.
       01  SUB                      PIC 99.

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

       01  A-NAME                   PIC X(16).

       01  BI-ALP-TABLE.
           03  BI-ALP-TBL           OCCURS 32 TIMES
                                    PIC X.

       01  ERRCD                PIC X        VALUE " ".

       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-S.
               05  FLDSEL-S-SSNO      PIC X(11).
               05  FILLER             PIC X(24).
               05  FLDSEL-S-ACCTNO    PIC 9(8).
               05  FILLER             PIC X(6).
               05  FLDSEL-S-OWNBR     PIC 9(4).
               05  FILLER             PIC X(23).
           03  FLDSEL-N               REDEFINES FLDSEL-S.
               05  FLDSEL-N-NAME      PIC X(20).
               05  FILLER             PIC X.
               05  FLDSEL-N-SSNO      PIC X(11).
               05  FILLER             PIC X(3).
               05  FLDSEL-N-ACCTNO    PIC 9(8).
               05  FILLER             PIC X(6).
               05  FLDSEL-N-OWNBR     PIC 9(4).
               05  FILLER             PIC X(23).

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
                   07  D-S-ACCTNO   PIC Z(8).
                   07  FILLER       PIC XXX.
                   07  D-S-MAKERCD  PIC X.
                   07  FILLER       PIC XX.
                   07  D-S-OWNBR    PIC Z(4).
                   07  FILLER       PIC XX.
                   07  D-S-ORGST    PIC XX.
                   07  FILLER       PIC XX.
                   07  D-S-ENTDATE  PIC 99/99/99   BLANK ZERO.
                   07  FILLER       PIC X.
                   07  D-S-POFFDATE PIC 99/99/99   BLANK ZERO.
           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 100.
       01  PAGE-KEY.
           03  FILLER                 OCCURS 100 TIMES.
               05  PAGE-KEY-N-X.
                   07  PAGE-KEY-N     PIC X(32).
                   07  PAGE-KEY-WK-KEY.
                       09  PAGE-KEY-SSNO   PIC 9(9) COMP.
                       09  PAGE-KEY-ACCTNO PIC 9(8) COMP.
                       09  PAGE-KEY-OWNBR  PIC 9(4) COMP.

       01  TEST-SSNO           PIC 9(9).
       01  TEST-SSNOX REDEFINES TEST-SSNO.
           03  FILLER          PIC X(5).
           03  TEST-LAST4      PIC 9(4).
       01  WORK-KEY-BI1-KEY.
           03  WORK-KEY-LAST4  PIC 9(4).
           03  WORK-KEY-LAST4-RDF REDEFINES WORK-KEY-LAST4
                               PIC X(4).
           03  WORK-KEY-ACCTNO PIC 9(8).
           03  WORK-KEY-OWNBR  PIC 9(4).
       01  WORK-LNAME          PIC X(15).
       01  WORK-LNAMEX REDEFINES WORK-LNAME.
           03  WORK-LNAME-BYTE PIC X OCCURS 15 TIMES.
       01  TEST-BI-LNAME.
           03  TEST-BI-BYTE    PIC X OCCURS 15 TIMES.

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".

       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".

       01  LAST4-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/LAST4_DEF.CPY".
       COPY "LIBSP/LAST4_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/LAST4_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
      *     PERFORM CLOSE-BI2-FILE.
           PERFORM CLOSE-BI1-FILE.
           CLOSE
                WK-FILE.
           PERFORM CLOSE-BI1-CONNECTION.
       COPY "LIBGB/DECLAR2.CPY".

      ********************************
       MAIN-PROGRAM SECTION.
      ********************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LAST4" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LAST4-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE EXT-FILPATH-BASE        TO WK-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE     TO WK-PATH-MACHINE.
           MOVE WK-PATH TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

       ENTER-SCANKEY.
           MOVE SPACES TO DISPLAY-BUF WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES TO WR-BUF.
           MOVE "SEL:B." TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "EN N040SSNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY.
           IF VDUNUM0 = 0
              GO TO ENTER-SCANKEY.

           MOVE VDUNUM0 TO WORK-KEY-LAST4.

       ENTER-NAME.
           MOVE "E  A150NAME." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-SCANKEY.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-NAME.
           MOVE VDUBUF TO WORK-LNAME.


      * BUILD DYNAMIC WK FILE ONLY INCLUDING MATCHING LAST 4
           PERFORM BUILD-WK-FILE.

       ENTER-SCANKEY-CONT.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           MOVE 0 TO PAGE-KEY-SSNO(PAGE-COUNT)
           MOVE 0 TO PAGE-KEY-ACCTNO(PAGE-COUNT)
           MOVE 0 TO PAGE-KEY-OWNBR(PAGE-COUNT).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.


           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.

           MOVE PAGE-KEY-WK-KEY(CURRENT-PAGE) TO WK-KEY.
           MOVE PAGE-KEY-SSNO(CURRENT-PAGE) TO WK-SSNO.
           MOVE PAGE-KEY-ACCTNO(CURRENT-PAGE) TO WK-ACCTNO.
           MOVE PAGE-KEY-OWNBR(CURRENT-PAGE) TO WK-OWNBR.
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
           MOVE ALP-BI      TO D-S-KEY(SUB).

           MOVE WK-LNAME    TO MESS.
           MOVE WK-FNAME    TO A-NAME.
           PERFORM SET-WK-LNAME.
           MOVE MESS        TO D-S-NAME(SUB)
           MOVE WK-ACCTNO   TO D-S-ACCTNO(SUB)
           MOVE WK-OWNBR    TO D-S-OWNBR(SUB)
           MOVE SPACES      TO D-S-MAKERCD(SUB)
           MOVE SPACES      TO D-S-ORGST(SUB)
           MOVE WK-POFFDATE TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY TO D-S-POFFDATE(SUB)
           MOVE WK-ENTDATE  TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY TO D-S-ENTDATE(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
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
                    MOVE WK-SSNO TO PAGE-KEY-SSNO(PAGE-COUNT)
                    MOVE WK-ACCTNO TO PAGE-KEY-ACCTNO(PAGE-COUNT)
                    MOVE WK-OWNBR TO PAGE-KEY-OWNBR(PAGE-COUNT)
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

           PERFORM OPEN-BI1-FILE.

           PERFORM SEND-PROGRESS-INDICATOR.

      *     IF WORK-LNAME = SPACES
      *        MOVE 0 TO BI-SSNO BI-ACCTNO BI-OWNBR
      *        PERFORM START-BI1-FILE
      *     ELSE
      *        MOVE SPACES     TO BI-FNAME
      *        MOVE WORK-LNAME TO BI-LNAME
      *        PERFORM START-BI2-FILE.

           MOVE WORK-KEY-LAST4 TO PASS-SSNO4.
           MOVE WORK-LNAME     TO PASS-LASTNAME.

           PERFORM START-BI1-FILE.

       BUILD-WK-NEXT.
      *     PERFORM READ-BI-NEXT.
           PERFORM READ-BI1-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO EXIT-BUILD-WK-FILE.

           PERFORM SEND-PROGRESS-INDICATOR.

      *     IF WORK-LNAME NOT = SPACES
      *        MOVE BI-LNAME TO TEST-BI-LNAME
      *        IF TEST-BI-BYTE(1) > WORK-LNAME-BYTE(1)
      *           GO TO EXIT-BUILD-WK-FILE
      *        ELSE
      *        PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > 15
      *          IF WORK-LNAME-BYTE(SUB) NOT = " "
      *            IF TEST-BI-BYTE(SUB) > WORK-LNAME-BYTE(SUB)
      *              GO TO EXIT-BUILD-WK-FILE
      *            END-IF
      *          END-IF
      *        END-PERFORM.


      *     MOVE BI-SSNO TO TEST-SSNO.
      *     IF TEST-LAST4 NOT = WORK-KEY-LAST4
      *        GO TO BUILD-WK-NEXT.

           MOVE BI-SSNO     TO WK-SSNO.
           MOVE BI-ACCTNO   TO WK-ACCTNO.
           MOVE BI-OWNBR    TO WK-OWNBR.
           MOVE BI-LNAME    TO WK-LNAME.
           MOVE BI-FNAME    TO WK-FNAME.
           MOVE BI-ENTDATE  TO WK-ENTDATE.
           MOVE BI-POFFDATE TO WK-POFFDATE.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO BUILD-WK-NEXT.

       EXIT-BUILD-WK-FILE.
           PERFORM CLOSE-BI1-FILE.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY LAST4-SCREEN UPON LAST4-WINDOW-HANDLE.
           MOVE LAST4-FORM-FIELDS TO WR-BUF.
           MOVE LAST4-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE LAST4-HELP--FILE TO HELP-LINK-FILE.
           MOVE LAST4-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/LAST4_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************
       MISC-PARAGRAPHS SECTION.
      **********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-PROGRESS-INDICATOR.
           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE "SEL:B."               TO PROGID-LINE-LIST.
           MOVE "00"                   TO PROGID-ROUTE-BUF.
           MOVE "S"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

           PERFORM PROGID-DISPLAY.

      * READ-BI-NEXT.
      *     IF WORK-LNAME = SPACES
      *        PERFORM READ-BI1-FILE-NEXT
      *     ELSE
      *        PERFORM READ-BI2-FILE-NEXT.
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

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPBIGS_SQL.CPY".
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
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.

      * COPY "LIBLP/LPBI2RN.CPY".

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
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BI1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO BI1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.
      *-----------------------------------------------------------------
       START-BI1-FILE.   *> SSP BIFILE
           PERFORM START-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.

           IF ( BI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BI1-FILE.

      *     SELECT @SQL = 'SELECT BIFILE.BI_SSNO,
      *             BIFILE.BI_ACCTNO,
      *             BIFILE.BI_OWNBR,
      *             BIFILE.BI_LNAME, 
      *             BIFILE.BI_FNAME, 
      *             CAST(BIFILE.BI_POFFDATE AS VARCHAR(10)), 
      *             CAST(BIFILE.BI_ENTDATE AS VARCHAR(10))
      *    FROM DBO.BIFILE
      *    WHERE SUBSTRING(CAST(FORMAT(BIFILE.BI_SSNO, ''D9'') AS CHAR(9)),6,4) = @SSNO4 '
      *    IF @LASTNAME != ' '
      *         SELECT @SQL += ' AND BIFILE.BI_LNAME LIKE @LASTNAME '

           EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BI1_CURSOR CURSOR FOR
               :BI1-RETURN-CODE = 
                 EXEC SSP_LAST4_BIFILE_SELECT
                  ( :PASS-SSNO4, :PASS-LASTNAME)
           END-EXEC.

           EXEC SQL
               OPEN BI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO BI1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.
           INITIALIZE QBI-REC.

           EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC.

           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BI1_CURSOR INTO 
                   :QBI-SSNO,
                   :QBI-ACCTNO,
                   :QBI-OWNBR,
                   :QBI-LNAME, 
                   :QBI-FNAME, 
                   :QBI-POFFDATE,
                   :QBI-ENTDATE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BI1-FILE-NEXT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 0 )
              PERFORM GET-BI-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BI1-FILE.
           PERFORM CLOSE-IT.
           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BI1_CURSOR
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


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **********************
       END-ROUTINE SECTION.
      **********************
           PERFORM CLOSE-FILES.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           MOVE "SP/SPMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN |MJD001
           DESTROY LAST4-SCREEN.
           DESTROY LAST4-WINDOW-HANDLE.
           EXIT PROGRAM.

