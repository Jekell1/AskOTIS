       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FOLLOW.
       DATE-WRITTEN.    09-24-2002.
      ******************************************************************
      *
      *  IDENTIFICATION:  WI/FOLLOW.C
      *
      *  DESCRIPTION   :  ENTER/CHANGE FOLLOW-UP DATE/CODE (F9)
      *
      *  ***NOTE***    : ONLY CALLED FROM CL/CLMENU.C
      *
      *-----------------------------------------------------------------
      * REV:
      * BLV 020924 ***NEW*** (REGACC) PR#: 5
      * BLV 030123 FOLLOW UP CODE WAS NOT GETTING MOVE IN TO LOAN
      *            CORRECTLY!  IT WAS GETTING WIPED OUT & MOVING IN
      *            1ST 4 CHARACTERS OF FOLLOW-UP DESCRIPTION!
      * KEC 2016.0328 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * BAH 2017.0418 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0530 ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 2019.0128 SPLIT CDFILE
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
           "@(#)WI/FOLLOW.CBL S35 05/07/24-08:43:13 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  FOLLOW-LINES            PIC 9(02)     VALUE 07.
       01  FOLLOW-COLUMNS          PIC 9(02)     VALUE 75.
       01  FOLLOW-BEGIN-Y          PIC 9(02)     VALUE 3.
       01  FOLLOW-BEGIN-X          PIC 9(02)     VALUE 07.
       01  FOLLOW-FORM-NAM         PIC X(09)     VALUE "WI/FOLLOW".

       01  ACTION.
           03  CHKEY                   PIC X(04)     VALUE SPACES.
           03  CHNO                    PIC 9(02)     VALUE ZEROES.

       01  BRANCH                      PIC 9(04).
       01  CHAN                        PIC X(04).

       01  DISPLAY-BUF                 VALUE SPACES.
           03  D-OLD-010               PIC 99/99/99.
           03  D-OLD-020               PIC X(4).
           03  D-OLD-DESC              PIC X(20).

       01  DISPLAY-LIST.
           03  FILLER PIC X(24) VALUE "OLD-010 OLD-020 CURDESC.".

       01  DISPLAY-OWNBR-BUF.
           03  FILLER                  PIC X(26)     VALUE
                                           "ACCOUNT OWNED BY BRANCH # ".
           03  DISPLAY-OWNBR           PIC ZZZ9.

       01  ERRCD                       PIC X(1).
       01  HOLD-FOLLOWUP-DATE          PIC 9(8)     VALUE 0.
       01  HOLD-FOLLOWUP-CD            PIC X(4)     VALUE SPACES.

       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  LEGEND-MODIFY-ROUTINE.
           03  FILLER PIC X(30) VALUE " F1-RESET, F7-UPDATE AND EXIT,".
           03  FILLER PIC X(15) VALUE " ENTER FIELD#: ".

       01  LEGEND-MODIFY-ROUTINE-SCAN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-UPDATE & EXIT, F6".
           03  FILLER PIC X(15) VALUE "-SCAN, FIELD#: ".

       01  NEW-FLAG                    PIC 9(01)     VALUE 1.

       01  IO-TYPE                     PIC X(3).
       01  RECORD-SIZE-WORKERS         VALUE SPACES.
           03  ORIGINAL-LN-REC         PIC X(LN-SIZE).
           03  CHANGED-LN-REC          PIC X(LN-SIZE).

       01  ELE                     PIC 9(02).
       01  ONE                     PIC S9(1).

       78  MAX                         VALUE 02.

       01  SPEC-TABLE.
      * 01. NEW FOLLOW-UP DATE             
           03  FILLER PIC X(08) VALUE "ERNM080 ".

      * 02. NEW FOLLOW-UP CODE            
           03  FILLER PIC X(08) VALUE "E  A020 ".

       01  SPEC-TAB                    REDEFINES SPEC-TABLE.
           03  SPEC-REC                OCCURS MAX TIMES.
               05  SPEC                PIC X(07).
               05  CHFG                PIC X(01).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/CLMENUW_EXT.CPY".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/PASSWDW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBWI/CDSCANW.CPY".

       01  FOLLOW-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/FOLLOW_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/FOLLOW_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/FOLLOW_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE "FOLLOW" TO VDU-FORM-NAME.

           MOVE SPACES        TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC  TO BRANCH.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE SPACES TO ERRCD.


              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE FOLLOW-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  FOLLOW-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    FOLLOW-COLUMNS + VDU-WIN-W-MOD
                      LINES   FOLLOW-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS FOLLOW-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-PROGRAM.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.


      ******************************************************************
       ENTER-KEYS SECTION.
      ******************************************************************

           MOVE ZEROES TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF ( IO-BAD )
              MOVE "END" TO IO-TYPE
              MOVE "INVALID ACCOUNT NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-EXIT.

           IF ( LN-POCD-VOID )
              MOVE "END" TO IO-TYPE
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-EXIT.

           IF ( LN-POCD-INRESCISSION )
              MOVE "END" TO IO-TYPE
              MOVE "ACCOUNT IS IN RESCISSION" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-EXIT.

           IF ( LN-POCD-PB-RESC )
              MOVE "END" TO IO-TYPE
              MOVE "RENEWAL ACCOUNT IS IN RESCISSION" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-EXIT.

           IF ( LN-POCD-RESCIND )
              MOVE "END" TO IO-TYPE
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-EXIT.

           MOVE LN-REC TO ORIGINAL-LN-REC.

           PERFORM DISPLAY-FIELDS.

           IF ( LN-OWNBR NOT = BRANCH )
              MOVE LN-OWNBR          TO DISPLAY-OWNBR
              MOVE DISPLAY-OWNBR-BUF TO MESS
              PERFORM SEND-MESS.

           PERFORM VERIFY-PASSWORD
           IF PASSWDW-STAT NOT = "OK"
              MOVE "CHANGES WILL NOT BE ALLOWED" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO CHFG(1) CHFG(2).

       ENTER-KEYS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *-----------------------------------------------------------------
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO ONE.
           MOVE ZEROES TO ELE.

       CREATE-ROUTINE.

           IF ( NEW-FLAG NOT = 1 ) GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE     < 1      ) MOVE 1 TO ELE ONE.
           IF ( ELE NOT > MAX    ) GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.

           MOVE LEGEND-MODIFY-ROUTINE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF (F1-KEY )
              MOVE "CLR" TO IO-TYPE
              GO TO ENTRY-MODULE-EXIT.

           IF (F7-KEY )
              MOVE "RWR" TO IO-TYPE
              GO TO ENTRY-MODULE-EXIT.

           IF ( NOT ENTER-KEY )
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.


           INSPECT ACTION REPLACING ALL SPACES BY ZEROES.

           IF ( CHNO NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( CHNO = ZEROES    ) GO TO MODIFY-ROUTINE.
           IF ( CHNO > MAX       ) GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.

           IF ELE = 2
              MOVE LEGEND-MODIFY-ROUTINE-SCAN TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( CHKEY    NOT = ZEROES ) AND
              ( CHFG (ELE)   = SPACES ) AND
              ( NEW-FLAG NOT = 1      )
              GO TO MODIFY-ROUTINE.

           IF ( CHFG (ELE) = "N" )
              IF ( NEW-FLAG   = 1   )
                 GO TO CREATE-ROUTINE
              ELSE
              IF ( CHKEY NOT = CHAN )
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC (ELE) TO VDU.
           MOVE ELE        TO VDUROW.
           MOVE ZEROES     TO VDUCOL.
           MOVE "."        TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F1-KEY.

           IF ( NOT F1-KEY ) GO TO TEST-UP-KEY.

           MOVE "CLR" TO IO-TYPE.
           GO TO ENTRY-MODULE-EXIT.

      *-----------------------------------------------------------------
       TEST-UP-KEY.

           IF ( NOT UP-KEY ) GO TO TEST-DOWN-KEY.

           IF ( NEW-FLAG = 1 )
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-DOWN-KEY.

           IF ( NOT DOWN-KEY ) GO TO TEST-F6-KEY.

           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-F6-KEY.

           IF ( NOT F6-KEY ) GO TO TEST-ENTER-KEY.

           MOVE 1 TO ONE.
           IF ELE = 2
              PERFORM CODE-SCAN
              IF IO-FG = 0
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.

      *-----------------------------------------------------------------
       TEST-ENTER-KEY.

           IF ( NOT ENTER-KEY ) GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.

           GO TO F-01 F-02
                 DEPENDING ON ELE.

      *-----------------------------------------------------------------
       F-01.
           MOVE VDU-DATE-YYYYMMDD TO LN-FOLLOWUP-DATE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-02.
           IF VDUBUF NOT = SPACES
              MOVE "FU"   TO CD-TYPE
              MOVE VDUBUF TO CD-CODE
              PERFORM GET-CD
              IF IO-FG = 9
                 MOVE " INVALID CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              END-IF
           ELSE
              MOVE SPACES TO CD-DESC.

           MOVE VDUBUF TO LN-FOLLOWUP-CD.

           MOVE "S  A040NEWDESC." TO VDU.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       ENTRY-MODULE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-LN1-FILE.
           MOVE LN-REC TO CHANGED-LN-REC.

           IF ( ORIGINAL-LN-REC = CHANGED-LN-REC )
              GO TO WRITE-MODULE-END.

           PERFORM READ-LN1-FILE.
           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO WRITE-MODULE-END.

           IF ( LN-REC NOT = ORIGINAL-LN-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO WRITE-MODULE-END.

           MOVE CHANGED-LN-REC TO LN-REC.
           PERFORM REWRITE-LN1-FILE.

       WRITE-MODULE-END.
           PERFORM CLOSE-LN1-FILE.

       WRITE-MODULE-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES              TO DISPLAY-BUF.
           MOVE LN-FOLLOWUP-DATE    TO DATE-YYYYMMDD HOLD-FOLLOWUP-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-OLD-010.
           MOVE LN-FOLLOWUP-CD      TO D-OLD-020 HOLD-FOLLOWUP-CD.

           MOVE "FU"           TO CD-TYPE.
           MOVE LN-FOLLOWUP-CD TO CD-CODE.
           PERFORM GET-CD.
           IF IO-GOOD
              MOVE CD-DESC TO D-OLD-DESC
           ELSE
              MOVE "INVALID CODE" TO D-OLD-DESC.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      *******************************************************
       CODE-SCAN SECTION.
      *******************************************************

           MOVE "FU"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A040020." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "FOLLOW" TO PASSWDW-PGM.
           MOVE 0 TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO CHANGE FOLLOWUP INFO...." TO PASSWDW-PROMPT2
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON FOLLOW-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY FOLLOW-SCREEN UPON FOLLOW-WINDOW-HANDLE.
           MOVE FOLLOW-FORM-FIELDS TO WR-BUF.
           MOVE FOLLOW-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE FOLLOW-HELP--FILE TO HELP-LINK-FILE.
           MOVE FOLLOW-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/FOLLOW_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.


       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

       UNDERGONE-CHANGE.
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FOLLOW-SCREEN.
           DESTROY FOLLOW-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
