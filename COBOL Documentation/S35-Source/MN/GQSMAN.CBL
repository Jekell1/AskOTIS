       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       GQSMAN.
       DATE-WRITTEN. 2000/11/22.
      ******************************************************************
      *
      *                 (MN)
      *
      *  DESCRIPTION:   GLOBAL COLLECTOR QUEUE (SUMMARY) FILE MAINTENANCE
      *
      * REV.
      * KEC 020896 MOVED " " TO GQ-TYPE WHEN CREATING A NEW RECORD.
      * KEC 031396 MADE CHANGES TO THE FORM AND THE KEY IS READ ON A
      *            GQ-HEAD-COLLID.
      * KEC 031496 REMOVED REFERENCES TO GQ-BRANCH GQ-SEQ.
      * KEC 032296 MADE REFERENCES TO GQ-LAST-DEL TO A PIC 9(5).
      * KEC 072997 CHANGED 'LIBGB/SFTDATE' TO 'LIBGB/EDITOR' AND
      *             COMMENTED OUT.
      * MJD 131210 A30 CONVERSION COMPLETED
      * KEC 2016.0310 ADDED CD-EA-FILLER FOR CHANGE TO CD-EA-REC.
      * BAH 190130 SPLIT CDFILE
      *  CS 190314 SPLIT OUT GQFILE INTO GQFILE(DETAIL) GQHFILE(HEADER)
      *            RENAMED TO GQSMAN
      *  190327 CS GQS-BRNO IS ALWAYS POPULATED WITH ZERO, PER KEN REMOVE
      *            FROM FILE LAYOUT
      *  190328 CS REMOVE BRNO FROM FORM
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *COPY "LIBSP/SPFSGQ.CPY".
       DATA DIVISION.
       FILE SECTION.
      *COPY "LIBSP/SPFDGQ.CPY".


      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/GQSMAN S34 07/28/21-11:03:53 >".

      ******************************************************************
      *
      *    P R O G R A M - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBSP/SPWSGQ.CPY".
       COPY "LIBSP/SP01GQS.CPY".
       COPY "LIBSP/SP01GQS_SQL.CPY".
       COPY "LIBSP/SPWSGQS.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

      *=================================================================
       01  ORIGINAL-GQS-REC                     PIC X(GQS-SIZE).
       01  CHANGED-GQS-REC                      PIC X(GQS-SIZE).

      ******************************************************************
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

       01  HOLD-COLLID                     PIC X(3).

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  F01           PIC ZZ,ZZ9.
           03  F02           PIC ZZ,ZZ9.
           03  F03           PIC ZZ,ZZ9.
           03  F04           PIC ZZ,ZZ9.
           03  F05           PIC ZZZ,ZZ9.
           03  F06           PIC X(6).

       01  DISPLAY-LIST.
           03  FILLER        PIC X(24) VALUE
           "010 020 030 040 050 060.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       01  MAX               PIC 9(2)    VALUE 6.
       01  SPEC-TABLE.
      *- - - - - - - - - 01. GQ-TOT-REC  - - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E NN050 ".

      *- - - - - - - - - 02. GQ-WORKED-REC - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E NN050 ".

      *- - - - - - - - - 03. GQ-TIMED-REC  - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E NN050 ".

      *- - - - - - - - - 04. GQ-LAST-DEL - - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E NN050 ".

      *- - - - - - - - - 05. GQ-LAST-BAL - - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E NN060 ".

      *- - - - - - - - - 06. GQ-GROUP-ID - - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E  A060 ".

      *-----------------------------------------------------------------
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 6 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01 GQSMAN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/GQSMAN_DEF.CPY".
       COPY "LIBMN/GQSMAN_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/GQSMAN_SCN.CPY".

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
      D    STOP MESS.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "GQSMAN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GQSMAN-WINDOW-HANDLE.


       MAIN-MODULE.

           PERFORM REC-GQS-MODULE.

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
       REC-GQS-MODULE SECTION.
      ******************************************************************
       REC-GQS-MODULE-NO.

           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           PERFORM FORM-RESET.

           MOVE "Y" TO ENTER-SEQNO.

       REC-GQS-ENTER-ACCTNO.
           MOVE "F7-EXIT, ENTER COLLECTOR ID:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030COLLID." TO VDU.
           PERFORM SCREENIO.

           IF F6-KEY
              PERFORM ENTER-SCANKEY
              GO TO END-REC-GQ-MODULE.

           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GQ-MODULE.

           IF NOT ENTER-KEY
              GO TO REC-GQS-ENTER-ACCTNO.

           MOVE VDUBUF TO CDB-CODE HOLD-COLLID.

           MOVE ZEROS      TO CDB-BRANCH.
           MOVE "CN"       TO CDB-TYPE.
           PERFORM VERIFY-CN.
           IF ( IO-FG = 9 )
              MOVE "INVALID COLLECTOR ID" TO MESS
              PERFORM SEND-MESS
              GO TO REC-GQS-MODULE-NO.

           MOVE "S  A300NAME." TO VDU.
           MOVE CDB-DESC TO VDUBUF.
           PERFORM SCREENIO.

           MOVE HOLD-COLLID TO GQS-COLLID.
           PERFORM OPEN-GQS1-FILE.
           PERFORM READ-GQS1-FILE.
           PERFORM CLOSE-GQS1-FILE.
           IF ( IO-FG NOT = 0 )
              PERFORM NEW-RECORD-SETUP
           ELSE
              MOVE GQS-REC TO ORIGINAL-GQS-REC
              PERFORM DISPLAY-FIELDS.

       END-REC-GQ-MODULE.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE "F1-RESET, F6-CONT SCAN, ENTER KEY:" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM OPEN-GQS1-FILE.

           MOVE SPACES TO GQS-COLLID.
           MOVE 0      TO FIRST-SCAN.

       ENTER-SCANKEY-KEY.

           MOVE "E  A030SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCANKEY-CLOSE.

           IF F6-KEY
              GO TO ENTER-SCANKEY-READ-NEXT.

           IF NOT ENTER-KEY
              GO TO ENTER-SCANKEY-KEY.

           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE GQS-COLLID      TO HOLD-COLLID
                 MOVE GQS-REC        TO ORIGINAL-GQS-REC
                 GO TO ENTER-SCANKEY-CLOSE
              ELSE
                 GO TO ENTER-SCANKEY-KEY.

           MOVE VDUBUF  TO GQS-COLLID.

       ENTER-SCANKEY-READ-NEXT.

           IF FIRST-SCAN = 0
              PERFORM START-GQS1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-GQS1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCANKEY-CLOSE.

           MOVE GQS-REC TO ORIGINAL-GQS-REC.
           PERFORM ENTER-SCANKEY-DISPLAY.
           IF STAT NOT = "00"
              MOVE SPACES TO DISPLAY-BUF
              MOVE DISPLAY-BUF TO WR-BUF
              MOVE DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO ENTER-SCANKEY-KEY.

           PERFORM DISPLAY-FIELDS.
           GO TO ENTER-SCANKEY-KEY.

       ENTER-SCANKEY-CLOSE.

           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

       ENTER-SCANKEY-END.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY-DISPLAY SECTION.
      ******************************************************************

           MOVE "S  A020BRDESC." TO VDU.
           MOVE "** CORPORATE BRANCH **" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "CN"               TO CDB-TYPE.
           MOVE EXT-FILPATH-BRANCH TO CDB-BRANCH.
           MOVE GQS-COLLID         TO CDB-CODE.
           PERFORM GET-CN.

           MOVE "S  A030COLLID." TO VDU.
           MOVE GQS-COLLID TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A300NAME." TO VDU.
           MOVE CDB-DESC       TO VDUBUF.
           PERFORM SCREENIO.
       EXIT-ENTER-SCANKEY-DISPLAY.
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

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
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

           IF UP-KEY
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

       F-01.
           MOVE VDUNUM0 TO GQS-TOT-REC.
           GO TO CREATE-ROUTINE.

       F-02.
           MOVE VDUNUM0 TO GQS-WORKED-REC.
           GO TO CREATE-ROUTINE.

       F-03.
           MOVE VDUNUM0 TO GQS-TIMED-REC.
           GO TO CREATE-ROUTINE.

       F-04.
           MOVE VDUNUM0 TO GQS-GQ-REV-DEL.
           GO TO CREATE-ROUTINE.

       F-05.
           MOVE VDUNUM0 TO GQS-GQ-REV-BAL.
           GO TO CREATE-ROUTINE.

       F-06.
           MOVE VDUBUF TO CD-CODE GQS-GROUP-ID.
           MOVE "EA"   TO CD-TYPE.
           PERFORM CHECK-CD-GROUP.
           IF IO-BAD OR CD-EA-CQ-FG NOT = "Y"
              MOVE "INVALID QUEUE GROUP!" TO MESS
              PERFORM SEND-MESS.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-GQS1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE GQS-REC TO CHANGED-GQS-REC.

           IF ORIGINAL-GQS-REC = CHANGED-GQS-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-GQS1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF GQS-REC NOT = ORIGINAL-GQS-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-GQS-REC TO GQS-REC.
           PERFORM REWRITE-GQS1-FILE.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.

           PERFORM WRITE-GQS1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-GQS1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************

           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-GQS1-FILE.
           PERFORM READ-GQS1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF GQS-REC NOT = ORIGINAL-GQS-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-GQS1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-GQS1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES      TO GQS-REC.
           MOVE HOLD-COLLID TO GQS-COLLID.

           MOVE ZEROES  TO GQS-GQ-DATE GQS-GQ-START-TIME GQS-GQ-REV-DEL
                               GQS-GQ-REV-BAL GQS-GQ-REV-PDUE-DAYS.

           MOVE ZEROES  TO GQS-TOT-REC  GQS-WORKED-REC GQS-TIMED-REC
                           GQS-GQ-REV-DEL GQS-GQ-REV-BAL.

       NEW-RECORD-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE " "             TO DISPLAY-BUF.
           MOVE GQS-TOT-REC      TO F01.
           MOVE GQS-WORKED-REC   TO F02.
           MOVE GQS-TIMED-REC    TO F03.
           MOVE GQS-GQ-REV-DEL   TO F04.
           MOVE GQS-GQ-REV-BAL   TO F05.
           MOVE GQS-GROUP-ID     TO F06.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO GQSMAN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GQSMAN-SYSDATE.
           DISPLAY SPACES UPON GQSMAN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GQSMAN-SCREEN UPON GQSMAN-WINDOW-HANDLE.
           MOVE GQSMAN-FORM-FIELDS TO WR-BUF.
           MOVE GQSMAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE GQSMAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE GQSMAN-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/GQSMAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       GET-CN.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
       VERIFY-CN.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "* NOT ON FILE *" TO CDB-DESC.
       CHECK-CD-GROUP.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
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
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
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
           PERFORM CLOSE-GQS1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBSP/SPGQSGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBSP/SPGQS1IN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
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
           DESTROY GQSMAN-WINDOW-HANDLE
           EXIT PROGRAM.
