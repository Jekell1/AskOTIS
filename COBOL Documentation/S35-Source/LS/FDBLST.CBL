       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FDDBLS.
       DATE-WRITTEN.    060184.
      ******************************************************************
      *
      *                      (LS)
      *    DESCRIPTION:  DATA BASE FILE LISTING
      *
      * REV:
      * BAH 111024 REMOVED RRFORM
      *
      * KEC 2016.0119 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *         REMOVED FT-FILE; MERGED INTO FD-FILE.
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

      ***************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/FDBLST.CBL S34 06/10/21-12:21:22 >".
      ***************************
       77  SCCS-IDENTIFIER PIC X(52) VALUE
           "@(#)LS/FDDBLS A15 10/24/11-14:51:36 COMPILE-FAILED >".
       COPY "LIBLP/LP01FDB.CPY".
       COPY "LIBLP/LP01FDB_SQL.CPY".
       COPY "LIBLP/LPWSFDB.CPY".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6) COMP-3.
       01  SUB1                 PIC 9(6) COMP-3.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  FILE-CODE1           PIC 99.
       01  FILE-CODE2           PIC 99.
       01  HOLD-DATABASE        PIC X(15).
       01  SWITCH-FLAG          PIC X        VALUE "N".

       01  SORT-FDB-REC         VALUE " ".
           03   S-DATABASE      OCCURS 100.
               05   S-DNAME     PIC X(8).
               05   S-DOFFSET   PIC 9(4)  COMP.
               05   S-DDIGITS   PIC 9(2)  COMP.
               05   S-DTYPE     PIC 9(2)  COMP.
               05   S-DDECIMAL  PIC 9(2)  COMP.
               05   S-DFIELD    PIC 9(4)  COMP.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  SORTKEY         PIC 9.

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
           03  FILLER           PIC X(4)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(12)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(3)     VALUE " ".
           03  H-TITLE          PIC X(33).

       01  HEAD3.
           03  FILLER           PIC X(57)    VALUE
           "FIELD #   --NAME--   OFFSET   TYPE   DIGITS   DECIMALS".
           03  FILLER           PIC X(17)    VALUE
           "FILE ID   FILE CD".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  D-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(3).
           03  D-DFIELD         PIC Z(4).
           03  FILLER           PIC X(3).
           03  D-DNAME          PIC X(8).
           03  FILLER           PIC X(5).
           03  D-DOFFSET        PIC ZZZZ.
           03  FILLER           PIC X(5).
           03  D-DTYPE          PIC ZZ.
           03  FILLER           PIC X(7).
           03  D-DDIGITS        PIC ZZ.
           03  FILLER           PIC X(9).
           03  D-DDECIMAL       PIC ZZ.
           03  FILLER           PIC X(8).
           03  D-CALLID         PIC X(2).
           03  FILLER           PIC X(8).
           03  D-FILECD         PIC Z9.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  FDBLST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/FDBLST_DEF.CPY".
       COPY "LIBLS/FDBLST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/FDBLST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-FDB1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************
       MAIN-MODULE SECTION.
      *************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "FDBLST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FDBLST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-FDB.
           PERFORM READ-FDB1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.
           COMPUTE FILE-CODE1 = (FDB-DBNO - 1) * 2 + 1.
           COMPUTE FILE-CODE2 = (FDB-DBNO - 1) * 2 + 2.
           IF SORTKEY = 2
              PERFORM DETAIL-LINE-PROCESS2
           ELSE
              PERFORM DETAIL-LINE-PROCESS1.
           GO TO NEXT-FDB.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND " TO MESS
              PERFORM SEND-MESS.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO SORT-FDB-REC.
       SETRP.
           MOVE FDBLST-QUEUE-POS TO Q-POS.
           MOVE FDBLST-COPIES-POS TO C-POS.
           MOVE FDBLST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.
       RRSKEY.
           MOVE "EN N010SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RRSKEY.
           MOVE VDUNUM0 TO SORTKEY.

           IF SORTKEY = 1 GO TO OK-YN.
           IF SORTKEY NOT = 2 GO TO RRSKEY.

       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO RRSKEY.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO ENDIT.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-FDB1-FILE.

           MOVE 0 TO FDB-DBNO.
           PERFORM START-FDB1-FILE.

           MOVE " " TO ERRCD.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "- - - DATA BASE FILE LISTING - - " TO H-TITLE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       DETAIL-LINE-PROCESS1 SECTION.
      ****************************
           MOVE 1 TO SUB.
           COMPUTE SUB1 = (FDB-DBNO - 1) * 200 + 1.
       NEXT-FIELD1.
           IF FDB-DNAME(SUB) = " "
              GO TO CHECK-TAB-END1.
           MOVE SUB1 TO D-DFIELD.
           MOVE FDB-DNAME(SUB) TO D-DNAME.
           MOVE FDB-DOFFSET(SUB) TO D-DOFFSET.
           SUBTRACT FDB-DDECIMAL(SUB) FROM FDB-DDIGITS(SUB)
              GIVING D-DDIGITS.
           MOVE FDB-DTYPE(SUB) TO D-DTYPE.
           MOVE FDB-DDECIMAL(SUB) TO D-DDECIMAL.
           IF SUB < 101
              MOVE FILE-CODE1 TO D-FILECD
              MOVE FDB-CALLID(1) TO D-CALLID
           ELSE
              MOVE FILE-CODE2 TO D-FILECD
              MOVE FDB-CALLID(2) TO D-CALLID.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
       CHECK-TAB-END1.
           IF SUB < 200
              ADD 1 TO SUB SUB1
              GO TO NEXT-FIELD1.

      ******************************************
       DETAIL-LINE-PROCESS2 SECTION.
      ******************************************

      * NAME ORDER 

           MOVE 1 TO SUB.
           PERFORM SORT-100-FIELDS.

           MOVE 1 TO SUB.
       NEXT-FIELD-100.
           IF S-DNAME(SUB) = " "
              GO TO CHECK-1ST-END.
           PERFORM MOVE-S-FIELDS.
       CHECK-1ST-END.
           IF SUB < 100
              ADD 1 TO SUB
              GO TO NEXT-FIELD-100.

           MOVE 101 TO SUB.
           PERFORM SORT-100-FIELDS.
           MOVE 1 TO SUB.
       NEXT-FIELD-200.
           IF S-DNAME(SUB) = " "
              GO TO CHECK-2ND-END.
           PERFORM MOVE-S-FIELDS.
       CHECK-2ND-END.
           IF SUB < 100
              ADD 1 TO SUB
              GO TO NEXT-FIELD-200.

      *************************************
       SORT-100-FIELDS SECTION.
      *************************************

           MOVE 1 TO SUB1.
       NEXT-FIELD-LOAD.
           MOVE FDB-DATABASE(SUB) TO S-DATABASE(SUB1).
           MOVE SUB TO S-DFIELD(SUB1).
           IF SUB1 < 100
              ADD 1 TO SUB SUB1
              GO TO NEXT-FIELD-LOAD.
       FLD-SORT.
           MOVE 1 TO SUB.
           MOVE 2 TO SUB1.
       NEXT-FIELD-SORT.
           IF S-DNAME(SUB) > S-DNAME(SUB1)
              MOVE S-DATABASE(SUB) TO HOLD-DATABASE
              MOVE S-DATABASE(SUB1) TO S-DATABASE(SUB)
              MOVE HOLD-DATABASE TO S-DATABASE(SUB1)
              MOVE "Y" TO SWITCH-FLAG.
           IF SUB1 < 100
              ADD 1 TO SUB SUB1
              GO TO NEXT-FIELD-SORT.
           IF SWITCH-FLAG = "Y"
              MOVE "N" TO SWITCH-FLAG
              GO TO FLD-SORT.

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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO FDBLST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FDBLST-SYSDATE.
           DISPLAY FDBLST-SCREEN UPON FDBLST-WINDOW-HANDLE.
           MOVE FDBLST-FORM-FIELDS TO WR-BUF.
           MOVE FDBLST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE FDBLST-HELP--FILE TO HELP-LINK-FILE.
           MOVE FDBLST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/FDBLST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************
       MISC-PARAGRAPHS SECTION.
      ***********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       MOVE-S-FIELDS.
           MOVE S-DFIELD(SUB) TO D-DFIELD.
           MOVE S-DNAME(SUB) TO D-DNAME.
           MOVE S-DOFFSET(SUB) TO D-DOFFSET.
           SUBTRACT S-DDECIMAL(SUB) FROM S-DDIGITS(SUB)
              GIVING D-DDIGITS.
           MOVE S-DTYPE(SUB) TO D-DTYPE.
           MOVE S-DDECIMAL(SUB) TO D-DDECIMAL.
           IF S-DFIELD(SUB) < 101
              MOVE FILE-CODE1 TO D-FILECD
              MOVE FDB-CALLID(1) TO D-CALLID
           ELSE
              MOVE FILE-CODE2 TO D-FILECD
              MOVE FDB-CALLID(2) TO D-CALLID.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      ******************************
       IO-ROUTINE SECTION.
      ******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPFDBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPFDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************
       END-PROGRAM SECTION.
      ******************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/FDMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY FDBLST-WINDOW-HANDLE.
           EXIT PROGRAM.
