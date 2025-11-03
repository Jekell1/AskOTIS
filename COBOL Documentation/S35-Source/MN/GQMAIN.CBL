       IDENTIFICATION DIVISION.
       PROGRAM-ID.   GQMAIN.
       DATE-WRITTEN. 012786.
      ******************************************************************
      *
      *                 (MN)
      *
      *  DESCRIPTION:   GLOBAL COLLECTOR QUEUE FILE MAINTENANCE
      *
      * REV.
      * KEC 020796 ADDED GQ-DATE.
      * KEC 020896 MOVED "D" TO GQ-TYPE WHEN CREATING NEW RECORD.
      * KEC 031496 REMOVED REFERENCES TO GQ-BRANCH AND GQ-SEQNO.
      * BLL 031496 FIX GQ DATE ROUTINES
      * KEC 042496 ADDED GQ-IN-PROCESS.
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * BLV 000908 CHANGED FIELD 4 (TIME) TO BE SIGNED, MAY BE SIGNED
      *            TIME ZONE FACTOR, TFC PR# 25
      * BLV 000912 ADD FIELD 12, GQ-TIME-ZONE-ERROR, TFC PR# 25
      * BLV 010105 FIX CD-CN-BRANCH SHOULD ALWAYS BE SET TO 0 BECAUSE
      *            THESE ARE GLOBAL QUEUES USING GLOBAL COLLECTORS
      * MJD 131210 A30 CONVERSION COMPLETED
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * KEC 2016.1006 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING LIBSP/SPSETGQ.
      *         ADDED LOGIC OF CONVERTING & SETTING GQ-DATE (CCYYMMDD).
      *         REMOVED USING LN-SEQNO & LN-MORELN.
      * BAH 181211 GLOBAL LNFILE
      *  CS 190315 BROKE OUT GQFILE INTO SUMMARY GQSFILE, AND DETAIL GQFILE
      *            LEAVE COMMENTS IN ABOUT GQ-TYPE, IT MAKES IT OBVIOUS 
      *            AS TO DETAIL OR SUMMARY(GQS) RECORDS
      *  CS 190603 CLEAR FUTURE FIELDS ON NEW RECORD
      *  BAH 20190723 ADDED CLEAR-GQ-FILE, SPGQCL
      * BAH 20190930 REMOVED ACCESS-CALL LNFILE
      *  CS 20191112 REMOVED CHECK ON LN-PATH, FIXED SCAN KEY, CHANGED
      *              SCANKEY-NUMERIC TO BE NUMERIC, PRIOR TO LOAD BRANCH
      *              MODULE, MOVE SCANKEY-BRNO TO BR-NO.
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
           "@(#)MN/GQMAIN.CBL S35 02/21/24-12:37:19 >".

      ******************************************************************
      *
      *    P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01GQ.CPY".
       COPY "LIBSP/SP01GQ_SQL.CPY".
       COPY "LIBSP/SPWSGQ.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *-----------------------------------------------------------------

       01  ERRCD                               PIC X(1)  VALUE SPACES.
       01  ONE                                 PIC S9.
       01  ELE                                 PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY                           PIC X(4).
           03  CHNO                            PIC 9(2).
       01  IO-TYPE                             PIC X(3).
       01  NEW-FLAG                            PIC 9.
       01  FIRST-SCAN                          PIC 9     VALUE 0.
       01  SCANKEY.
           03  SCAN-TYPE                       PIC X(01)  VALUE "D".
           03  SCANKEY-NUMERIC                 PIC 9(12).
           03  SCANKEY-ALPHA REDEFINES SCANKEY-NUMERIC.
               05  SCANKEY-BRNO                PIC 9(04).
               05  SCANKEY-ACCTNO              PIC 9(08).

       01  ENTER-SEQNO                         PIC X.
       01  BR-ALLOW-WINDOWS                    PIC X VALUE "Y".

       01  HOLD-BRNO                       PIC 9(4).
       01  HOLD-ACCTNO                     PIC 9(8).

       01  ORIGINAL-GQ-REC                     PIC X(GQ-SIZE).
       01  CHANGED-GQ-REC                      PIC X(GQ-SIZE).

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  F01           PIC XXX.
           03  F011          PIC X(25).
           03  F02           PIC X.
           03  F03           PIC 99/99/99.
           03  F04           PIC 9999-.
           03  F05           PIC 99999.
           03  F06           PIC 99999.
           03  F07           PIC ZZZ,ZZ9.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  F08           PIC X.
           03  F09           PIC XXX.
           03  F091          PIC X(25).
           03  F10           PIC X.
           03  F11           PIC 99/99/99.
           03  F12           PIC X.

       01  DISPLAY-LIST.
           03  FILLER        PIC X(56) VALUE
           "010 011 020 030 040 050 060 070 080 090 091 100 110 120.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       01  MAX               PIC 9(2)    VALUE 12.
       01  SPEC-TABLE.
      * 01. COLLECTOR ID  - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E  A030 ".
      * 02. SEQ FG  - - - - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E  A010 ".
      * 03. DATE  - - - - - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "ERNM060 ".
      * 04. START TIME  - - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "ENNS040 ".
      * 05. REV. DELINQUENCY  - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "ENNN050 ".
      * 06. REV. PAST DUE DAYS  - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "ENNN050 ".
      * 07. REV. BALANCE  - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E NN060 ".
      * 08. WORKED FLAG - - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E  A010 ".
      * 09. WORK COLLECTOR  - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E  A030 ".
      * 10. WORK IN PROCESS - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E  A010 ".
      * 11. DATE  QUEUED  - - - - - - - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "ERNM060 ".
      * 12. TIME ZONE ERROR FLAG - -- - - - - - - - - -
           03  FILLER        PIC X(8) VALUE "E  A010 ".

      *-----------------------------------------------------------------
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 12 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01 GQMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/GQMAIN_DEF.CPY".
       COPY "LIBMN/GQMAIN_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/GQMAIN_SCN.CPY".

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

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "GQMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GQMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM REC-GQ-MODULE.

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
       REC-GQ-MODULE SECTION.
      ******************************************************************
       REC-GQ-MODULE-NO.

           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE LN-REC.
           PERFORM FORM-RESET.

           MOVE "Y" TO ENTER-SEQNO.

       REC-GQ-ENTER-BRNO.
           IF BR-ALLOW-WINDOWS = "Y"
              MOVE "F7-EXIT, ENTER BRANCH NUMBER:" TO MESS
           ELSE
              MOVE "F7-EXIT, ENTER BRANCH NUMBER:" TO MESS.

           PERFORM SEND-LEGEND.
           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF F6-KEY
              MOVE 0 TO BR-NO
              PERFORM ENTER-SCANKEY
              GO TO END-REC-GQ-MODULE.

           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GQ-MODULE.

           IF NOT ENTER-KEY
              GO TO REC-GQ-MODULE-NO.

           MOVE VDUNUM0 TO BR-NO BR-NO.
           PERFORM LOAD-BRANCH-MODULE.

           MOVE "S  A200BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF
           PERFORM SCREENIO.

           IF ( BR-PATH = SPACES )
              GO TO REC-GQ-ENTER-BRNO.

       REC-GQ-ENTER-ACCTNO.
           IF BR-ALLOW-WINDOWS = "Y"
              MOVE "F7-EXIT, ENTER ACCOUNT NUMBER:" TO MESS
           ELSE
              MOVE "F7-EXIT, ENTER ACCOUNT NUMBER:" TO MESS.

           PERFORM SEND-LEGEND.
           MOVE "EN N080LNNO." TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY  GO TO REC-GQ-ENTER-BRNO.

           IF F6-KEY
              PERFORM ENTER-SCANKEY
              GO TO END-REC-GQ-MODULE.

           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GQ-MODULE.

           IF NOT ENTER-KEY
              GO TO REC-GQ-MODULE-NO.

           MOVE VDUNUM0 TO LN-ACCTNO.

       READ-LOAN-FILE.

           PERFORM GET-LN.
           IF IO-FG = 9
              MOVE "INVALID ACCOUNT NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO REC-GQ-MODULE-NO.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-GQ-MODULE-NO.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-GQ-MODULE-NO.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
              PERFORM SEND-MESS.

           MOVE "SN N080LNNO." TO VDU.
           MOVE LN-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A020LNSEQ." TO VDU.
           PERFORM SCREENIO.

       READ-GQFILE.

           MOVE BR-NO     TO GQ-BRNO.
           MOVE LN-ACCTNO TO GQ-ACCTNO.
           PERFORM OPEN-GQ1-FILE.
           PERFORM READ-GQ1-FILE.
           PERFORM CLOSE-GQ1-FILE.

           IF ( IO-FG = 0 )
               MOVE GQ-REC TO ORIGINAL-GQ-REC
               PERFORM DISPLAY-FIELDS
           ELSE
               MOVE BR-NO     TO HOLD-BRNO
               MOVE LN-ACCTNO TO HOLD-ACCTNO
               PERFORM NEW-RECORD-SETUP.

       END-REC-GQ-MODULE.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE "F1-RESET, F6-CONT SCAN, ENTER KEY:" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-GQ1-FILE.
           MOVE BR-NO TO GQ-BRNO.
           MOVE 0     TO GQ-ACCTNO.
           MOVE 0     TO FIRST-SCAN.

       ENTER-SCANKEY-KEY.

           MOVE "ENNN120SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCANKEY-CLOSE.

           IF VDUSTATUS = "1<"
              GO TO ENTER-SCANKEY-READ-NEXT.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY-KEY.

           IF VDUNUM0 = ZEROS
              IF FIRST-SCAN NOT = 0
                 GO TO ENTER-SCANKEY-CLOSE
              ELSE
                 GO TO ENTER-SCANKEY-KEY.

           MOVE VDUNUM0 TO SCANKEY-NUMERIC.
           MOVE SCANKEY-BRNO TO GQ-BRNO BR-NO.                
           MOVE SCANKEY-ACCTNO TO GQ-ACCTNO.

            
           PERFORM LOAD-BRANCH-MODULE.

           IF ( BR-PATH = SPACES )
              MOVE "BRANCH NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SCANKEY-KEY.

       ENTER-SCANKEY-READ-NEXT.

           IF FIRST-SCAN = 0
              PERFORM START-GQ1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-GQ1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCANKEY-CLOSE.

           MOVE GQ-REC TO ORIGINAL-GQ-REC.

           PERFORM ENTER-SCANKEY-DISPLAY.

           PERFORM DISPLAY-FIELDS.

           GO TO ENTER-SCANKEY-KEY.

       ENTER-SCANKEY-CLOSE.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

       ENTER-SCANKEY-END.
           EXIT.

      **************************************************
       ENTER-SCANKEY-DISPLAY SECTION.
      **************************************************

           MOVE GQ-BRNO TO BR-NO SCANKEY-BRNO.
           PERFORM LOAD-BRANCH-MODULE.

           MOVE "SN N040BRNO." TO VDU.
           MOVE GQ-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A020BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GQ-ACCTNO TO LN-ACCTNO SCANKEY-ACCTNO.
           PERFORM GET-LN.
           MOVE "SN N080LNNO." TO VDU.
           MOVE GQ-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A020LNSEQ." TO VDU.
           MOVE SPACES TO VDUBUF.
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
      *-----------------------------------------------------------------
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.

      *-----------------------------------------------------------------
       CREATE-ROUTINE.

           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-F6.
           IF ( VDUSTATUS = "1<" ) AND ( ELE = 1 OR 10 )
              IF ( BR-ALLOW-WINDOWS = "Y" )
                 PERFORM CODE-SCAN
                 IF ( IO-FG NOT = 0 )
                    GO TO ENTER-ELE
                 ELSE
                    MOVE CDBSCN-CODE TO VDUBUF
                    MOVE "00"        TO VDUSTATUS.

      *-----------------------------------------------------------------
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09
                 F-10 F-11 F-12
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
           MOVE VDUBUF     TO GQ-COLLID CDB-CODE.
           MOVE "CN"       TO CDB-TYPE.
           MOVE 0          TO CDB-BRANCH.
           PERFORM VERIFY-CD.
           IF IO-FG NOT = 0
              MOVE "NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE "S  A250011." TO VDU.
           MOVE CDB-DESC      TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO GQ-SEQ-FG.
           IF NOT (GQ-SEQ-FG = "N" OR "B" OR "R")
              MOVE "ENTER 'B' OR 'N' OR 'R':" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDU-DATE-YYYYMMDD TO GQ-DATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUNUM0 TO GQ-START-TIME.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUNUM0 TO GQ-REV-DEL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUNUM0 TO GQ-REV-PDUE-DAYS.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUNUM0 TO GQ-REV-BAL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO GQ-WORKED-FG.

           IF NOT (GQ-WORKED-FG = "Y" OR "N" OR "R" OR " ")
              MOVE "ENTER 'Y' OR 'N' OR SPACE:" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF ( GQ-WORKED-FG = "N" OR " " )
              MOVE "S  A030090." TO VDU
              MOVE " " TO VDUBUF GQ-WORK-COLLID
              PERFORM SCREENIO
              MOVE "S  A300091." TO VDU
              PERFORM SCREENIO
              MOVE "N" TO CHFG(9)
           ELSE
              MOVE " " TO CHFG(9).

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUBUF     TO GQ-WORK-COLLID CDB-CODE.
           MOVE "CN"       TO CDB-TYPE.
           MOVE 0          TO CDB-BRANCH.
           PERFORM VERIFY-CD.
           IF ( IO-FG = 9 )
              MOVE "NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE "S  A025091." TO VDU.
           MOVE CDB-DESC      TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.

           IF ( VDUBUF = "Y" OR " " )
              MOVE VDUBUF TO GQ-IN-PROCESS
           ELSE
              MOVE "ENTER 'Y' OR SPACE ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDU-DATE-YYYYMMDD TO GQ-DATE-QUEUED.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.

           IF VDUBUF = "Y" OR "N"
              MOVE VDUBUF TO GQ-TIME-ZONE-ERROR
           ELSE
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.


       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-GQ1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE GQ-REC TO CHANGED-GQ-REC.

           IF ORIGINAL-GQ-REC = CHANGED-GQ-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-GQ1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF GQ-REC NOT = ORIGINAL-GQ-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-GQ-REC TO GQ-REC.
           PERFORM REWRITE-GQ1-FILE.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.

           PERFORM WRITE-GQ1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-GQ1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************

           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-GQ1-FILE.
           PERFORM READ-GQ1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF GQ-REC NOT = ORIGINAL-GQ-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-GQ1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-GQ1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-GQ-FILE.

           MOVE HOLD-BRNO   TO GQ-BRNO.
           MOVE HOLD-ACCTNO TO GQ-ACCTNO.

       NEW-RECORD-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE " "                        TO DISPLAY-BUF.
           MOVE GQ-COLLID                  TO F01 CDB-CODE.
           MOVE "CN"                       TO CDB-TYPE.
           MOVE 0                          TO CDB-BRANCH.
           PERFORM VERIFY-CD.
           MOVE CDB-DESC                   TO F011.
           MOVE GQ-SEQ-FG                  TO F02.

           MOVE GQ-DATE                    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F03.

           MOVE GQ-START-TIME              TO F04.
           MOVE GQ-REV-DEL                 TO F05.
           MOVE GQ-REV-PDUE-DAYS           TO F06.
           MOVE GQ-REV-BAL                 TO F07.
           MOVE GQ-WORKED-FG               TO F08.

           IF ( GQ-WORK-COLLID NOT = " " )
              MOVE GQ-WORK-COLLID          TO F09 CDB-CODE
              MOVE "CN"                    TO CDB-TYPE
              MOVE 0                       TO CDB-BRANCH
              PERFORM VERIFY-CD
              MOVE CDB-DESC                TO F091.

           MOVE GQ-IN-PROCESS              TO F10.

           MOVE GQ-DATE-QUEUED             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F11.

           MOVE GQ-TIME-ZONE-ERROR         TO F12.

           MOVE DISPLAY-BUF                TO WR-BUF.
           MOVE DISPLAY-LIST               TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************
           MOVE "CN"               TO CDBSCN-TYPE.
           MOVE EXT-FILPATH-BRANCH TO CDBSCN-BRANCH.
           MOVE SPACES             TO CDBSCN-CODE.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS NOT = "00") OR (CDBSCN-CODE = SPACES) 
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE 0 TO IO-FG.

           MOVE "S  A030  0."  TO VDU.
           MOVE ELE            TO VDUROW.
           MOVE CDBSCN-CODE    TO VDUBUF.
           PERFORM SCREENIO.

       EXIT-CODE-SCAN.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO GQMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GQMAIN-SYSDATE.
           DISPLAY SPACES UPON GQMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GQMAIN-SCREEN UPON GQMAIN-WINDOW-HANDLE.
           MOVE GQMAIN-FORM-FIELDS TO WR-BUF.
           MOVE GQMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE GQMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE GQMAIN-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/GQMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       LOAD-BRANCH-MODULE SECTION.
      ******************************************************************
     
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
     
           IF ( IO-BAD )
              MOVE "* NOT ON FILE *"       TO BR-NAME
              MOVE SPACES                  TO BR-PATH
              GO TO LOAD-BRANCH-MODULE-EXIT.
     
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
     
           MOVE "B"                        TO LN-PATH-OVERRIDE.

       LOAD-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBSP/SPGQCL.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       GET-LN.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
       VERIFY-CD.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG NOT = 0
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
           PERFORM CLOSE-GQ1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBSP/SPGQGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBSP/SPGQ1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "LP/CPMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY GQMAIN-WINDOW-HANDLE
           EXIT PROGRAM.
