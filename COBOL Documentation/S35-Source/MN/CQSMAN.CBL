       IDENTIFICATION DIVISION.
       PROGRAM-ID.   CQSMAN.
       DATE-WRITTEN. 020796.
      ******************************************************************
      *
      *                 (MN)
      *
      *  DESCRIPTION:   COLLECTOR QUEUE (HEADER) FILE MAINTENANCE
      *
      * REV.
      * KEC 020896 MOVED " " TO CQ-TYPE WHEN CREATING A NEW RECORD.
      * KEC 031396 MADE CHANGES TO THE FORM AND THE KEY IS READ ON A
      *            CQ-HEAD-COLLID.
      * KEC 031496 REMOVED REFERENCES TO CQ-BRANCH CQ-SEQ.
      * KEC 032296 MADE REFERENCES TO CQ-LAST-DEL TO A PIC 9(5).
      * KEC 072997 CHANGED 'LIBGB/SFTDATE' TO 'LIBGB/EDITOR' AND
      *             COMMENTED OUT.
      * BLM 110323 CHANGED ORIGINAL-REC & CHANGED-REC FROM 49 BYTES TO
      *            52 BYTES, WORLD
      * MJD 131210 A30 CONVERSION COMPLETED
      * KEC 2016.1004 FIXED ORIGINAL-REC & CHANGED-REC TO BE
      *               ORIGINAL-CQ-REC & CHANGED-CQ-REC USING CQ-SIZE.
      *  CS 2019.0130 GLOBAL CQFILE
      *  CS 2019.0215 BREAKOUT OF CQFILE, HEADER IS NOW CQSFILE
      *  CS 2019.0308 RENAMED FROM CQHMAN(HEADER) TO CQSMAN(SUMMARY)
      *  CS 2019.0314 CHANGED PARAGRAPH NAMES TO BE CQS-
      *  CS 2021.0503 SAVED CQSMAN.20210503, COMMENTED OUT CONTRAINT, 
      *              ON FIRST SCAN COULDNT FIND A RECORD, IT WENT BACK
      *              TO ENTER KEY AND THEN YOU HAD TO SCAN AGAIN
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/CQSMAN.CBL S35 02/21/24-12:35:12 >".

      ******************************************************************
      *
      *    P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01CQS.CPY".
       COPY "LIBSP/SP01CQS_SQL.CPY".
       COPY "LIBSP/SPWSCQS.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

       01  ORIGINAL-CQS-REC                     PIC X(CQS-SIZE).
       01  CHANGED-CQS-REC                      PIC X(CQS-SIZE).

       01  ERRCD                               PIC X(1)  VALUE SPACES.
       01  ONE                                 PIC S9.
       01  ELE                                 PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY                           PIC X(4).
           03  CHNO                            PIC 9(2).
       01  IO-TYPE                             PIC X(3).
       01  NEW-FLAG                            PIC 9.
       01  FIRST-SCAN                          PIC 9     VALUE 0.
       01  ENTER-SEQNO                         PIC X.
       01  BR-ALLOW-WINDOWS                    PIC X VALUE "Y".

       01  HOLD-KEY.
           03  HOLD-COLLID                     PIC X(3).

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  F01           PIC ZZ,ZZ9.
           03  F02           PIC ZZ,ZZ9.
           03  F03           PIC ZZ,ZZ9.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  F04           PIC ZZ,ZZ9.
           03  F05           PIC ZZZ,ZZ9.

       01  DISPLAY-LIST.
           03  FILLER        PIC X(20) VALUE
           "010 020 030 040 050.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       01  MAX               PIC 9(2)    VALUE 5.
       01  SPEC-TABLE.
      *- - - - - - - - - 01. CQS-TOT-REC  - - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E NN050 ".

      *- - - - - - - - - 02. CQS-WORKED-REC - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E NN050 ".

      *- - - - - - - - - 03. CQS-TIMED-REC  - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E NN050 ".

      *- - - - - - - - - 04. CQS-LAST-DEL - - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E NN050 ".

      *- - - - - - - - - 05. CQS-LAST-BAL - - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E NN060 ".

      *-----------------------------------------------------------------
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 5 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       01 CQSMAN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/CQSMAN_DEF.CPY".
       COPY "LIBMN/CQSMAN_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/CQSMAN_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "CQSMAN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CQSMAN-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM REC-CQS-MODULE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-CQS-MODULE SECTION.
      ******************************************************************
       REC-CQS-MODULE-NO.

           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           PERFORM FORM-RESET.

           MOVE "Y" TO ENTER-SEQNO.

           IF BR-ALLOW-WINDOWS = "Y"
              MOVE "F7-EXIT, ENTER ACCOUNT NUMBER:" TO MESS
           ELSE
              MOVE "F7-EXIT, ENTER ACCOUNT NUMBER:" TO MESS.

           PERFORM SEND-LEGEND.
           MOVE "E  A030COLLID." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-CQS-MODULE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CQS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-CQS-MODULE-NO.

           MOVE VDUBUF TO CDB-CODE HOLD-COLLID.

       READ-CDFILE1.

           MOVE EXT-FILPATH-BRANCH TO CDB-BRANCH.
           MOVE "CN"               TO CDB-TYPE.
           PERFORM GET-CN.
           IF ( IO-FG = 9 )
              MOVE "INVALID COLLECTOR ID" TO MESS
              PERFORM SEND-MESS
              GO TO REC-CQS-MODULE-NO.

           MOVE "S  A300NAME." TO VDU.
           MOVE CDB-DESC       TO VDUBUF.
           PERFORM SCREENIO.

       READ-CQSFILE.

           MOVE HOLD-COLLID TO CQS-COLLID.
           PERFORM OPEN-CQS1-FILE.
           PERFORM READ-CQS1-FILE.
           PERFORM CLOSE-CQS1-FILE.

           IF ( IO-FG NOT = 0 )
              PERFORM NEW-RECORD-SETUP
           ELSE
              MOVE CQS-REC TO ORIGINAL-CQS-REC
              PERFORM DISPLAY-FIELDS.

       END-REC-CQS-MODULE.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE "F1-RESET, F6-CONT SCAN, ENTER KEY:" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-CQS1-FILE.

           MOVE SPACES TO CQS-COLLID.
           MOVE 0      TO FIRST-SCAN.

       ENTER-SCANKEY-KEY.

           MOVE "E  A030SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCANKEY-CLOSE.

           IF VDUSTATUS = "1<"
              GO TO ENTER-SCANKEY-READ-NEXT.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY-KEY.

           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE CQS-COLLID TO HOLD-COLLID
                 MOVE CQS-REC    TO ORIGINAL-CQS-REC
                 GO TO ENTER-SCANKEY-CLOSE
              ELSE
                 GO TO ENTER-SCANKEY-KEY.

           MOVE VDUBUF TO CQS-COLLID.

      *-----------------------------------------------------------------
       ENTER-SCANKEY-READ-NEXT.

           IF FIRST-SCAN = 0
              PERFORM START-CQS1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-CQS1-FILE-NEXT.
           IF IO-FG = 9 OR (CQS-BRNO NOT = CQS-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCANKEY-CLOSE.

           MOVE CQS-REC TO ORIGINAL-CQS-REC.

           PERFORM ENTER-SCANKEY-DISPLAY.

           PERFORM DISPLAY-FIELDS.

           GO TO ENTER-SCANKEY-KEY.

      *-----------------------------------------------------------------
       ENTER-SCANKEY-DISPLAY.

           MOVE "CN"               TO CDB-TYPE.
           MOVE EXT-FILPATH-BRANCH TO CDB-BRANCH.
           MOVE CQS-COLLID     TO CDB-CODE.
           PERFORM GET-CN.

           MOVE "S  A030COLLID." TO VDU.
           MOVE CQS-COLLID TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A300NAME." TO VDU.
           MOVE CDB-DESC       TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-SCANKEY-CLOSE.

           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

      *-----------------------------------------------------------------
       ENTER-SCANKEY-END.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.

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

           MOVE "F1, 'DELETE', ENTER FIELD #:" TO MESS.
           PERFORM SEND-LEGEND.
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

       F-01.
           MOVE VDUNUM0 TO CQS-TOT-REC.
           GO TO CREATE-ROUTINE.

       F-02.
           MOVE VDUNUM0 TO CQS-WORKED-REC.
           GO TO CREATE-ROUTINE.

       F-03.
           MOVE VDUNUM0 TO CQS-TIMED-REC.
           GO TO CREATE-ROUTINE.

       F-04.
           MOVE VDUNUM0 TO CQS-CQ-REV-DEL.
           GO TO CREATE-ROUTINE.

       F-05.
           MOVE VDUNUM0 TO CQS-CQ-REV-BAL.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-CQS1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE CQS-REC TO CHANGED-CQS-REC.

           IF ORIGINAL-CQS-REC = CHANGED-CQS-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-CQS1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF CQS-REC NOT = ORIGINAL-CQS-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-CQS-REC TO CQS-REC.
           PERFORM REWRITE-CQS1-FILE.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.

           PERFORM WRITE-CQS1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-CQS1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************

           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-CQS1-FILE.
           PERFORM READ-CQS1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF CQS-REC NOT = ORIGINAL-CQS-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-CQS1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-CQS1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

       CLEAR-NEW-RECORD.
           MOVE SPACES              TO CQS-REC.
           MOVE EXT-CONAME-USER-LOC TO CQS-BRNO.
           MOVE HOLD-COLLID         TO CQS-COLLID.


           MOVE ZEROES      TO CQS-TOT-REC  CQS-WORKED-REC 
                               CQS-TIMED-REC CQS-CQ-REV-DEL
                               CQS-CQ-REV-BAL.

           MOVE ZEROS       TO CQS-CQ-REV-BEHAVIOR-RISK, CQS-CQ-DATE
                               CQS-CQ-START-TIME,CQS-CQ-REV-PDUE-DAYS
                               CQS-CQ-SEQ.
       NEW-RECORD-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE " "             TO DISPLAY-BUF.
           MOVE CQS-TOT-REC      TO F01.
           MOVE CQS-WORKED-REC   TO F02.
           MOVE CQS-TIMED-REC    TO F03.
           MOVE CQS-CQ-REV-DEL     TO F04.
           MOVE CQS-CQ-REV-BAL     TO F05.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO CQSMAN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CQSMAN-SYSDATE.
           DISPLAY SPACES UPON CQSMAN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CQSMAN-SCREEN UPON CQSMAN-WINDOW-HANDLE.
           MOVE CQSMAN-FORM-FIELDS TO WR-BUF.
           MOVE CQSMAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE CQSMAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CQSMAN-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/CQSMAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       GET-CN.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "* NOT ON FILE *" TO CDB-DESC.
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
       SEND-LEGEND.
           MOVE "S  A070MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CQS1-FILE.
           PERFORM CLOSE-CDB1-FILE.
       COPY "LIBSP/SPCQSGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBSP/SPCQS1IN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "LP/CPMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY CQSMAN-WINDOW-HANDLE
           EXIT PROGRAM.
