       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPMB.
       DATE-WRITTEN.    060184.
      ******************************************************************
      *
      *   DIR :  LP
      *   DESC:  VARIABLE FORMS TEST PRINT
      *          FORMS DOCUMENT PRINT, CALLED FROM LP/FDTEST.CBL
      *          #5. FDMENU
      *
      *=================================================================
      * REV:
      * MJD 112597 EXPANDED FD-REC TO 2828 BYTES (ADDED 30)
      * BAH 091118 ADDED FORM#: TO PRINT AT THE TOP, WORLD PR# 661
      * BAH 110914 REPLACED RRFORM WITH SCREEN
      * KEC 2016.0118 FIXED KEYS & RECS FOR FD-REC.
      *               ADDED NEW FDB-FILE; WAS IN FD-FILE.
      * KEC 2016.0211 CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *               *NOTE* STILL NEED TO FIX FORM/SCREEN INPUT.
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

      ****************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPMB.CBL A32 10/15/20-10:58:33 >".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FDB.CPY".
       COPY "LIBLP/LP01FDB_SQL.CPY".
       COPY "LIBLP/LPWSFDB.CPY".

       01  HOLD-FD1-KEY.
           03  HOLD-FD-FMNO            PIC 9(04).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  ERRCD                PIC X.
       01  D-B-SUB              PIC 9(6) COMP-3.
       01  FIELD-SUB            PIC 9(6) COMP-3.
       01  SUB                  PIC 9(6) COMP-3.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  ALL-DASH             PIC X(132)   VALUE ALL "-".

      *************************************
      *   E D I T   O P T I O N S         *
      *   USED TO SCAN AVAILABLE EDITS    *
      *************************************
       01  EDIT-ALP.
           03  FILLER      PIC X(15)  VALUE "    NO EDITING ".
           03  FILLER      PIC X(15)  VALUE "    0000009.99-".
           03  FILLER      PIC X(15)  VALUE "       9999.99-".
           03  FILLER      PIC X(15)  VALUE "      9,999.99-".
           03  FILLER      PIC X(15)  VALUE "     $9,999.99-".
           03  FILLER      PIC X(15)  VALUE "    ****999.99-".
           03  FILLER      PIC X(15)  VALUE "      99/99/99 ".
           03  FILLER      PIC X(15)  VALUE "(999) 999-9999 ".
           03  FILLER      PIC X(15)  VALUE "   999-99-9999 ".
           03  FILLER      PIC X(15)  VALUE "      SM/SD/SY ".
           03  FILLER      PIC X(15)  VALUE "    99-9999999 ".
       01  EDIT-ALPX REDEFINES EDIT-ALP.
           03  EDIT-DESC   PIC X(15)  OCCURS 11.
       01  EDIT-MAX        PIC 9(2)   VALUE 11.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************

       01  ACCEPT-BUF          PIC X     VALUE " ".

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
           03  H-TITLE            PIC X(41).

       01  HEAD3.
           03  FILLER           PIC X(44)    VALUE
           "CODE  NEXT  DESCRIPTION".
           03  FILLER           PIC X(54)    VALUE
           "PROG   1STFILE    BODY      TOP     BOTTOM   CONTIN   ".
           03  FILLER           PIC X(23)    VALUE
           "COUNTER   FMTYPE   MEMO".

       01  HEAD4.
           03  FILLER           PIC X(50)    VALUE
           "ROW    COL       KEY---------  FIELD---------   ED".
           03  FILLER           PIC X(17)     VALUE
           "IT---------------".

       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC XXX.
           03  DETAIL-LINE-FORM PIC X(129).

       01  LINE1 REDEFINES DETAIL-LINE.
           03  D-FMNO           PIC ZZZ9.
           03  FILLER           PIC X(2).
           03  D-FMLINK.
               05  D-NEXTFM     PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-DESC           PIC X(25).
           03  FILLER           PIC X(8).
           03  D-PROG           PIC XX.
           03  FILLER           PIC X(7).
           03  D-1STFILE        PIC Z9.
           03  FILLER           PIC X(7).
           03  D-LENGTH         PIC ZZ9.
           03  FILLER           PIC X(7).
           03  D-TOP            PIC Z9.
           03  FILLER           PIC X(7).
           03  D-BOTTOM         PIC Z9.
           03  FILLER           PIC X(7).
           03  D-PRCONT         PIC X.
           03  FILLER           PIC X(9).
           03  D-LNUPDFG        PIC X.
           03  FILLER           PIC X(8).
           03  D-CRNOREC        PIC X.
           03  FILLER           PIC X(8).
           03  D-LNMEMO         PIC X.
           03  FILLER           PIC X(12).

       01  LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X.
           03  D-ROW            PIC Z9.
           03  FILLER           PIC X(5).
           03  D-COL            PIC Z9.
           03  FILLER           PIC X(6).
           03  D-KEY            PIC ZZZ9.
           03  FILLER           PIC X(1).
           03  D-KEYDESC        PIC X(8).
           03  FILLER           PIC X(3).
           03  D-FIELD          PIC ZZZ9.
           03  FILLER           PIC X(1).
           03  D-FIELDDESC      PIC X(8).
           03  FILLER           PIC X(4).
           03  D-EDIT           PIC ZZ9.
           03  FILLER           PIC X.
           03  D-EDITDESC       PIC X(15).
           03  FILLER           PIC X(64).

       01  LINE3 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(85).
           03  D-TITLEX         PIC X(7).
           03  D-TITLE          PIC X(40).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPMB_DEF.CPY".
       COPY "LIBLP/LONPMB_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  LINK-BUF          VALUE " ".
           03  BEG-FM               PIC 999.
           03  END-FM               PIC 999.
           03  DESC-YN              PIC X.
           03  TEST-ONLY            PIC X.
           03  LONPMB-WINDOW-HANDLE PIC X(10).

       01  PRU-PATH            VALUE " ".
           03  PRU-CMD.
               05  PRU-LP      PIC X(3).
                   88  QUEUE-PIGGYBACK     VALUE "PRU" "DEV".
                   88  QUEUE-PRINTER       VALUE "   ".
                   88  QUEUE-TERMINAL      VALUE "VDU".
                   88  QUEUE-VIEW          VALUE "HLD".
               05  PRU-DASHD   PIC X(2).
               05  PRU-DEST    PIC X(10).
               05  FILLER      PIC X.
               05  PRU-DASHS   PIC X(2).
               05  FILLER      PIC X.
               05  PRU-DASHN   PIC X(2).
               05  PRU-COPIES  PIC 9(1).
               05  FILLER      PIC X(1).
      * USER FIELD NOT USED ON 3B2
               05  PRU-DASHA   PIC X(2).
               05  PRU-USER    PIC X(11).
           03  PRU-FILE.
               05  PRU-DIR     PIC X(17).
               05  PRU-PROG    PIC X(6).
               05  PRU-X       PIC X(6).
               05  FILLER      PIC X(25).

       SCREEN SECTION.
       COPY "LIBLP/LONPMB_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME LINK-BUF PRU-PATH EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-FDB1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***************************
       MAIN-MODULE SECTION.
      ***************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPMB" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF TEST-ONLY = "Y"
              PERFORM TEST-FORM
              GO TO END-PROGRAM.

       NEXT-FD.
           PERFORM READ-FD1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF FD-FMNO > END-FM
              GO TO END-ROUTINE.

           MOVE FD-FMNO TO HOLD-FD-FMNO.
           MOVE 99 TO LN-FEED.

           PERFORM PRINT-FORM.
           IF DESC-YN = "Y"
              PERFORM PRINT-DESCRIPTOR.

           GO TO NEXT-FD.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND " TO MESS
              PERFORM SEND-MESS.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE " " TO ERRCD.
       PRINT-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PRINT-YN.
           MOVE VDUBUF TO ACCEPT-BUF.

           IF ACCEPT-BUF = "N"
              MOVE "X" TO ERRCD
              GO TO EXIT-INITIALIZE.

           IF ACCEPT-BUF NOT = "Y"
              GO TO PRINT-YN.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE "- - - - VARIABLE FORM DESCRIPTOR - - - -" TO H-TITLE.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-FD1-FILE.
           PERFORM OPEN-FDB1-FILE.

           MOVE BEG-FM TO FD-FMNO HOLD-FD-FMNO
                          QFD1-WBEG-FMNO.
           MOVE END-FM TO QFD1-WEND-FMNO.

           PERFORM START-FD1-FILE.
           IF TEST-ONLY = "Y"
              PERFORM TEST-FORM
              GO TO EXIT-INITIALIZE.

       EXIT-INITIALIZE.
           EXIT.

      ****************************
       TEST-FORM SECTION.
      ****************************
           PERFORM READ-FD1-FILE-NEXT.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO EXIT-TEST-FORM.

           IF FD-FMNO NOT = BEG-FM
              PERFORM NOT-FOUND
              GO TO EXIT-TEST-FORM.

           MOVE 0 TO LN-FEED.
           PERFORM PRINT-FORM.

       EXIT-TEST-FORM.
           EXIT.

      ****************************
       PRINT-FORM SECTION.
      ****************************
           MOVE "N" TO FIRST-TIME.
           MOVE " " TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "FORM#:" TO D-TITLEX.
           MOVE FD-FMNO TO D-TITLE.
           MOVE DETAIL-LINE TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "TITLE:" TO D-TITLEX.
           MOVE FD-DESC TO D-TITLE.
           MOVE DETAIL-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "DATE :" TO D-TITLEX.
           MOVE EXT-CONAME-SYSDATE TO D-TITLE.
           MOVE DETAIL-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE " " TO DETAIL-LINE.

           MOVE " " TO PR-REC.
           MOVE 5 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE 1 TO SUB.
           IF FD-BODY > 30
              MOVE 30 TO FD-BODY.
       NEXT-FORM-LINE.
           MOVE FD-PROMPTS(SUB) TO DETAIL-LINE-FORM.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           IF SUB < FD-BODY
              ADD 1 TO SUB
              GO TO NEXT-FORM-LINE.
           MOVE SPACES TO DETAIL-LINE.

      ****************************
       PRINT-DESCRIPTOR SECTION.
      ****************************
           PERFORM HEAD-PAGE.
           MOVE FD-FMNO TO D-FMNO.
           IF FD-NEXTFM NUMERIC
              MOVE FD-NEXTFM TO D-NEXTFM
           ELSE
              MOVE " ? " TO D-FMLINK.
           MOVE FD-DESC TO D-DESC.
           MOVE FD-PROG TO D-PROG.
           MOVE FD-1STFILE TO D-1STFILE.
           MOVE FD-BODY TO D-LENGTH.
           MOVE FD-TOP TO D-TOP.
           MOVE FD-BOTTOM TO D-BOTTOM.
           MOVE FD-PRCONT TO D-PRCONT.
           MOVE FD-FMTYPE TO D-CRNOREC.
           MOVE FD-INCR-DELCNT TO D-LNUPDFG.
           MOVE FD-LNMEMO TO D-LNMEMO.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL-DASH TO PR-REC.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 3 TO LN-FEED.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 2 TO LN-FEED.
           MOVE 1 TO SUB.
       NEXT-FIELD-DESC.
           IF FD-FIELDNO(SUB) = 0
              GO TO TEST-FIELD-END.
           MOVE FD-FIELDROW(SUB) TO D-ROW.
           MOVE FD-FIELDCOL(SUB) TO D-COL.
           MOVE FD-FIELDNO(SUB) TO D-FIELD FIELD-SUB.
           PERFORM GET-DNAME.
           MOVE FDB-DNAME(D-B-SUB) TO D-FIELDDESC.
           MOVE FD-FIELDKEY(SUB) TO D-KEY FIELD-SUB.
           IF FIELD-SUB = 0
              MOVE "N / A" TO D-KEYDESC
           ELSE
              PERFORM GET-DNAME
              MOVE FDB-DNAME(D-B-SUB) TO D-KEYDESC.
           MOVE FD-FIELDED(SUB) TO D-EDIT FIELD-SUB.
           MOVE "     * ERROR *" TO D-EDITDESC.
           IF FIELD-SUB > 0 AND FIELD-SUB NOT > EDIT-MAX
              MOVE EDIT-DESC(FIELD-SUB) TO D-EDITDESC.
           PERFORM WRITE-DETAIL-LINE.
       TEST-FIELD-END.
           IF SUB < 50
              ADD 1 TO SUB
              GO TO NEXT-FIELD-DESC.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > 60
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
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

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO LONPMB-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LONPMB-SYSDATE.
           DISPLAY LONPMB-SCREEN UPON LONPMB-WINDOW-HANDLE.
           MOVE LONPMB-FORM-FIELDS TO WR-BUF.
           MOVE LONPMB-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE LONPMB-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPMB-HELP--DIR  TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPMB_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************
       MISC-PARAGRAPHS SECTION.
      ***********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       NOT-FOUND.
           MOVE "FORM NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       GET-DNAME.
           COMPUTE FDB-DBNO = (FIELD-SUB - 1) / 200 + 1.
           COMPUTE D-B-SUB = FIELD-SUB - (FDB-DBNO - 1) * 200.
           PERFORM READ-FDB1-FILE.
           IF IO-FG = 9
              MOVE "*ERROR*" TO FDB-DNAME(D-B-SUB).

      ******************************
       IO-ROUTINE SECTION.
      ******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPFDBGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************
       END-PROGRAM SECTION.
      ******************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-RESET.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
