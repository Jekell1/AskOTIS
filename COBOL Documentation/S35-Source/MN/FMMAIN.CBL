       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FMMAIN.
      ******************************************************************
      *
      *     FORM REQUEST FILE MAINTENANCE
      *
      *=================================================================
      * REV:
      * 102296 JTG CHANGED DISPLAY OF FIELD 'FM' TO 3 NUMERIC
      * 001127 GLB NEW FORM REQUEST SCAN WINDOW
      * 020109 GLB ALLOW F6-SCAN FROM FORM CODE TO ENABLE
      *            STARTING FM-FILE IN WI/FMSCAN.C
      * 020228 BAH UNCOMMENTED THE FLDVAL OF EXIT-PATH, WAS EXITING
      *            TO $ SIGN, CALLED FROM LPM2MU AND FDMENU ! PL# 353
      * 020516 BAH INITIALIZED FM-INITKEY TO SPACES BEFORE ENTERING
      *            ANY PART OF THE KEY, SCAN WAS NOT STARTING RIGHT !
      * KEC 2017.0210 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'FM' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * KEC 2017.0217 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED VERIFICATION OF FM-BRNO ON ENTRY.
      *  CS 170421 ACTIVATE 8 DIGIT DATES, FM-TRDATE
      * BAH 181115 GLOBAL FMFILE
      * BAH 190312 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *            REDEFINE IN THE FM-REC
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BLM 20211130 ONLY ALLOW KEY FORMAT OF 0.  THEY DON'T USE NOTES
      *              AND ENTERING OTHER SIZED FIELDS IN INIITKEY CAUSES
      *              SUBSCIPT ERRORS IN SCREEN HANDLER
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/FMMAIN.CBL S35 02/21/24-12:35:32 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".

       01  ERRCD            PIC X       VALUE SPACES.
       01  ONE              PIC S9.
       01  ELE              PIC 9(2).
       01  ACTION.
           03  CHKEY        PIC X(4).
           03  CHNO         PIC 9(2).
       01  IO-TYPE          PIC X(3).
       01  NEW-FLAG         PIC 9.
       01  FIRST-TIME       PIC 9       VALUE 0.

       01  HOLD-FM1-KEY.
           03  HOLD-FM-BRNO    PIC 9(4).
           03  HOLD-FM-INITKEY PIC X(20).
           03  HOLD-FM-FMNO    PIC 9(4).

       01  ORIGINAL-FM-REC     PIC X(FM-SIZE).
       01  CHANGED-FM-REC      PIC X(FM-SIZE).

      *    KEY FORMATS - MODIFY KEY MAX
       01  KEY-IV.
           03  KEY-IV-NO    PIC 9(10) COMP-3.
       01  KEY-NT.
           03  KEY-NT-NO    PIC 9(6)  COMP-3.
       01  KEY-FORMAT       PIC 99.
       01  KEY-VDU          PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(10).
           03  F02         PIC X.
           03  F03         PIC 9(4).
           03  F04         PIC 9(3).
           03  F05         PIC 99/99/99.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(20) VALUE
           "010 020 030 040 050.".

      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 5.
       01  SPEC-TABLE.
      * 1. FM-ZIP
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      * 2. FM-PRINT
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 3. FM-BRNO
           03  FILLER      PIC X(8) VALUE "EN N040 ".
      * 4. FM-STMT-COUNTER
           03  FILLER      PIC X(8) VALUE "EN N030 ".
      * 5. FM-TRDATE
           03  FILLER      PIC X(8) VALUE "E NM060N".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 5 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
       01  MESS-LIST       PIC X(5)   VALUE "MESS.".

       COPY "LIBWI/FMSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

       01 FMMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/FMMAIN_DEF.CPY".
       COPY "LIBMN/FMMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/FMMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE "FMMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FMMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM FORM-RESET.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.
           PERFORM REC-FM-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.
           GO TO MAIN-MODULE.

      ***************************************
       REC-FM-MODULE SECTION.
      ***************************************
       REC-FM-MODULE-NO.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE FM-INITKEY.
       REC-KEY-FORMAT.
           MOVE "F6-SCAN WINDOW, F5-OLD SCAN, F7-EXIT" TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "EN N020KF." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-FM-MODULE.
           IF VDUSTATUS = "1<" OR VDUSTATUS = "1:"
              PERFORM FORM-REQUEST-SCAN
              IF IO-FG NOT = 0
                 GO TO REC-FM-MODULE-NO
              ELSE
                 MOVE FM-BRNO    TO HOLD-FM-BRNO
                 MOVE FM-INITKEY TO HOLD-FM-INITKEY
                 MOVE FM-FMNO    TO HOLD-FM-FMNO
                 GO TO READ-FMFILE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-FM-MODULE-NO.
           MOVE VDUNUM0 TO KEY-FORMAT.

      * KEY-FORMAT MUST BE 0, DON'T USE NOTES
           IF KEY-FORMAT NOT = 0
              MOVE "PLEASE ENTER 0" TO MESS
              PERFORM SEND-MESS
              GO TO REC-KEY-FORMAT.
           GO TO KEY-IV-FILE KEY-NT-FILE
              DEPENDING ON KEY-FORMAT.
           MOVE "E RA200IK." TO KEY-VDU.
           GO TO REC-INIT-KEY.
       KEY-IV-FILE.
           MOVE "EN N100IK." TO KEY-VDU.
           GO TO REC-INIT-KEY.
       KEY-NT-FILE.
           MOVE "EN N130IK." TO KEY-VDU.
           GO TO REC-INIT-KEY.
       REC-INIT-KEY.
           MOVE KEY-VDU TO VDU.
      D    STOP MESS.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO REC-KEY-FORMAT.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-FM-MODULE.
           IF VDUSTATUS = "1<"
             PERFORM FORM-REQUEST-SCAN
             IF IO-FG NOT = 0
                GO TO REC-FM-MODULE-NO
             ELSE
                MOVE FM-INITKEY TO HOLD-FM-INITKEY
                MOVE FM-FMNO    TO HOLD-FM-FMNO
                MOVE FM-BRNO    TO HOLD-FM-BRNO
                GO TO READ-FMFILE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-INIT-KEY.
           GO TO FORMAT-IV FORMAT-NT
              DEPENDING ON KEY-FORMAT.
       UNFORMATTED.
           MOVE VDUBUF TO FM-INITKEY HOLD-FM-INITKEY.
           GO TO ENTER-FMNO.
       FORMAT-IV.
           MOVE VDUNUM0 TO KEY-IV-NO.
           MOVE KEY-IV TO VDUBUF.
           GO TO UNFORMATTED.
       FORMAT-NT.
           MOVE VDUNUM0 TO KEY-NT-NO.
           MOVE KEY-NT TO VDUBUF.
           GO TO UNFORMATTED.
       ENTER-FMNO.
           MOVE SPACES TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "EN N030FM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
             GO TO REC-FM-MODULE-NO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-FM-MODULE.
           IF VDUSTATUS = "1<"
             PERFORM FORM-REQUEST-SCAN
             IF IO-FG NOT = 0
                GO TO REC-FM-MODULE-NO
             ELSE
                MOVE FM-INITKEY TO HOLD-FM-INITKEY
                MOVE FM-FMNO    TO HOLD-FM-FMNO
                MOVE FM-BRNO    TO HOLD-FM-BRNO
                GO TO READ-FMFILE.
           IF VDUSTATUS NOT = "00"
             GO TO ENTER-FMNO.
           MOVE VDUNUM0 TO FM-FMNO FD-FMNO HOLD-FM-FMNO.
       READ-FMFILE.
           PERFORM OPEN-FD1-FILE.
           MOVE FM-FMNO TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF IO-FG = 9
              MOVE "INVALID" TO FD-DESC.
           MOVE "S  A250FMX." TO VDU.
           MOVE FD-DESC TO VDUBUF.
           PERFORM SCREENIO.
           IF IO-FG = 9
             GO TO ENTER-FMNO.
           PERFORM OPEN-FM1-FILE.
           PERFORM READ-FM1-FILE.
           PERFORM CLOSE-FM1-FILE.
           IF IO-FG = 0
              MOVE FM-REC TO ORIGINAL-FM-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.
       END-REC-FM-MODULE.
           EXIT.


      ***********************************************************
       FORM-REQUEST-SCAN SECTION.
      ***********************************************************
           MOVE FM-INITKEY         TO FMSCAN-FM-INITKEY.
           IF VDUSTATUS = "1<"
              MOVE "WI/FMSCN2" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH FMSCAN-BUF
                                    EXIT-PATHNAME
              CANCEL FORM-PROGX
           ELSE
              MOVE "WI/FMSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH FMSCAN-BUF
                                    EXIT-PATHNAME
              CANCEL FORM-PROGX.

           IF ( FMSCAN-STATUS NOT = "00" ) OR FMSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-FORM-REQUEST-SCAN.

           MOVE FMSCAN-FM-INITKEY TO FM-INITKEY.
           MOVE FMSCAN-FM-FMNO    TO FM-FMNO.
           MOVE FM-PATH-OWNBR     TO FM-BRNO.

           MOVE "S  A000IK." TO VDU.
           MOVE FM-INITKEY   TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SNNN030FM." TO VDU.
           MOVE FM-FMNO      TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE 0               TO IO-FG.

       EXIT-FORM-REQUEST-SCAN.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO FM-ZIP.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO FM-PRINT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUNUM0 TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           IF ( IO-BAD )
              MOVE "ERROR: BRANCH NUMBER NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE VDUNUM0 TO FM-BRNO.

           IF ( FM-BRNO NOT = EXT-CONAME-USER-LOC )
              MOVE "WARNING: BRANCH NUMBER NOT SAME AS LOCAL" TO MESS
              PERFORM SEND-MESS.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUNUM0 TO FM-STMT-COUNTER.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDU-DATE-YYYYMMDD TO FM-TRDATE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ************************************
       WRITE-MODULE SECTION.
      ************************************
           PERFORM OPEN-FM1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE FM-REC TO CHANGED-FM-REC.
           IF ORIGINAL-FM-REC = CHANGED-FM-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-FM1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF FM-REC NOT = ORIGINAL-FM-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-FM-REC TO FM-REC.
           PERFORM REWRITE-FM1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-FM1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-FM1-FILE.

      *************************************
       DELETE-MODULE SECTION.
      *************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-FM1-FILE.
           PERFORM READ-FM1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF FM-REC NOT = ORIGINAL-FM-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
            PERFORM DELETE-FM1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-FM1-FILE.

      *************************************
       NEW-RECORD-SETUP SECTION.
      *************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           PERFORM CLEAR-FM-FILE.

           MOVE HOLD-FM1-KEY TO FM1-KEY.
           MOVE 0            TO ELE FM-BRNO FM-STMT-COUNTER.

      *************************************
       DISPLAY-FIELDS SECTION.
      *************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE FM-ZIP TO F01.
           MOVE FM-PRINT TO F02.
           MOVE FM-BRNO TO F03.
           MOVE FM-STMT-COUNTER TO F04.
           MOVE FM-TRDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F05.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * THIS IS ALWAYS OWNING BRANCH FOR CORPORATE PRINTING. NEEDED TO SEE
      * FOR TESTING.
           MOVE "SNNN040BRNO2." TO VDU.
           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-BRNO2 TO VDUNUM0.
           PERFORM SCREENIO.

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO FMMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FMMAIN-SYSDATE.
           DISPLAY SPACES UPON FMMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FMMAIN-SCREEN UPON FMMAIN-WINDOW-HANDLE.
           MOVE FMMAIN-FORM-FIELDS TO WR-BUF.
           MOVE FMMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************
           MOVE FMMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE FMMAIN-HELP--DIR TO HELP-LINK-DIR.

      ******************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/FMMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPFMCL.CPY".

      *************************************
       MISC-PARAGRAPHS SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      *************************************
       IO-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *************************************
       END-ROUTINE SECTION.
      *************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY FMMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
