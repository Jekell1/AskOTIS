       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHTRWF.
       AUTHOR.          KEC.
       DATE-WRITTEN.    05-24-1999.
      ******************************************************************
      *
      *  IDENTIFICATION:  WI/CHTRWF.C
      *
      *  DESCRIPTION   :  CREDIT BUREAU REPORTING (F9)
      *
      *  ***NOTE***    : ONLY CALLED FROM CL/CLMENU.C
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 1999.0524 ***NEW***                         (TFC) PR#: 00023
      * GLF 990830 PASSWORD PROTECTED TRW SPECIAL CODE, ADDED SCAN OF
      *            TRW SPECIAL CODE, AND TEST FOR VALID TRW SPECIAL CODES
      *            ACCORDING TO THOSE ENTERED IN MN/TSMAIN (TFC) PR#: 51
      * BAH 010830 DONT ALLOW JUST "0 " OR " 0" IN TRW-STATUS
      * BAH 030605 ADDED LN-SKIP-TRW VALUE OF "E" TO SKIP REPORTING THE
      *            ENDORSER, JUST REPORT MAKER AND JOINT, ACC PR# 2081
      * BAH 050331 ADDED LN-TRW-B-CONSUMER-IND AND LN-TRW-J-CONSUMER-IND
      *            FOR METRO2 OVERRIDE ABILITY, ACC PR# 2502
      * BAH 050927 ADDED LN-TRW-COMPLIANCE-CODE FOR LIBERAL PR# 2591
      * BLF 070911 CHECK EXT-WI-CL-ARCHIVE-FG & SET UP ARCHIVE PATHS FOR LN
      *             IF LOAN IS FROM ARCHIVE, WORLD PR# 547
      * BAH 131230 ADDED DISPLAY ONLY OF LN-TRW-DOFD-DATE, WORLD PR# 955
      * BAH 140108 ADDED WORLD-SEQ TO PASSWORD ALL FIELDS EXCEPT #3
      *                                                   WORLD PR#1047
      * KEC 2016.0311 CHANGED CD-???? TO CD-TS-???? (TYPE "TS")
      * KEC 2016.0328 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * BAH 170412 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0530 ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 190125 SPLIT CDFILE
      * BAH 2020.02.17 DISPLAY LN-PLDATE AND LN-PLAMT, #1423
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
           "@(#)WI/CHTRWF.CBL S35 05/07/24-08:25:58 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  CHTRWF-LINES            PIC 9(02)     VALUE 12.
       01  CHTRWF-COLUMNS          PIC 9(02)     VALUE 70.
       01  CHTRWF-BEGIN-Y          PIC 9(02)     VALUE 07.
       01  CHTRWF-BEGIN-X          PIC 9(02)     VALUE 07.

       01  ACTION.
           03  CHKEY                   PIC X(04)     VALUE SPACES.
           03  CHNO                    PIC 9(02)     VALUE ZEROES.

       01  BRANCH                      PIC 9(04).
       01  CHAN                        PIC X(04).
       01  TEST-IND                    PIC XX.
           88 VALID-INDICATOR          VALUE "A " "B " "C " "D "
                                             "E " "F " "G " "H "
                                             "I " "J " "K " "L "
                                             "M " "N " "O " "P "
                                             "Q " "R " "S " "T "
                                             "U " "V " "W " "X "
                                             "Y " "Z " "1A" "  ".

       01  DISPLAY-BUF                 VALUE SPACES.
           03  D-OLD-010               PIC X(01).
           03  D-OLD-020               PIC X(02).
           03  D-OLD-030               PIC X(02).
           03  D-OLD-040               PIC X(01).
           03  D-OLD-050               PIC X(02).
           03  D-OLD-060               PIC X(02).
           03  D-OLD-070               PIC X(02).
           03  D-DOFD                  PIC 99/99/99 BLANK ZERO.
           03  D-PLDATE                PIC 99/99/99 BLANK ZERO.
           03  D-PLAMT                 PIC Z(6)9.99.

       01  DISPLAY-LIST.
           03  FILLER PIC X(16) VALUE "OLD-010 OLD-020 ".
           03  FILLER PIC X(16) VALUE "OLD-030 OLD-040 ".
           03  FILLER PIC X(16) VALUE "OLD-050 OLD-060 ".
           03  FILLER PIC X(26) VALUE "OLD-070 DOFD PLDATE PLAMT.".

       01  DISPLAY-OWNBR-BUF.
           03  FILLER                  PIC X(26)     VALUE
                                           "ACCOUNT OWNED BY BRANCH # ".
           03  DISPLAY-OWNBR           PIC ZZZ9.

       01  ERRCD                       PIC X(01).
       01  HOLD-CONFIDENT              PIC X(01)     VALUE SPACES.
       01  HOLD-TRW                    PIC X(01)     VALUE SPACES.
       01  HOLD-TRW-STATUS             PIC X(02)     VALUE SPACES.
       01  HOLD-TRW-SPECIAL-CODE       PIC X(02)     VALUE SPACES.
       01  HOLD-TRW-B-CONSUMER-IND     PIC X(02)     VALUE SPACES.
       01  HOLD-TRW-J-CONSUMER-IND     PIC X(02)     VALUE SPACES.
       01  HOLD-TRW-COMPLIANCE-CODE    PIC X(02)     VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  LEGEND-MODIFY-ROUTINE.
           03  FILLER PIC X(30) VALUE " F1-RESET, F7-UPDATE AND EXIT,".
           03  FILLER PIC X(15) VALUE " ENTER FIELD#: ".

       01  LEGEND-MODIFY-ROUTINE-SCAN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-UPDATE & EXIT, F6".
           03  FILLER PIC X(15) VALUE "-SCAN, FIELD#: ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  NEW-FLAG                    PIC 9(01)     VALUE 1.
       01  IO-TYPE                     PIC X(3).

       01  RECORD-SIZE-WORKERS         VALUE SPACES.
           03  ORIGINAL-LN-REC         PIC X(LN-SIZE).
           03  CHANGED-LN-REC          PIC X(LN-SIZE).

       01  ELE                     PIC 9(02).
       01  ONE                     PIC S9(1).

       78  MAX                         VALUE 07.

      *-----------------------------------------------------------------
       01  SPEC-TABLE.
      * 01. NEW SKIP TRW FLAG
           03  FILLER PIC X(08) VALUE "E  A010 ".

      * 02. NEW TRW STATUS REPORTING CODE
           03  FILLER PIC X(08) VALUE "E  A020 ".

      * 03. NEW TRW SPECIAL CODE
           03  FILLER PIC X(08) VALUE "E  A020 ".

      * 04. CONFIDENTIAL
           03  FILLER PIC X(08) VALUE "E  A010 ".

      * 05. NEW MAKER CONSUMER INDICATOR
           03  FILLER PIC X(08) VALUE "E  A020 ".

      * 06. NEW COMAKER CONSUMER INDICATOR
           03  FILLER PIC X(08) VALUE "E  A020 ".

      * 07. NEW COMMPLIANCE CONDITION CODE
           03  FILLER PIC X(08) VALUE "E  A020 ".

      *-----------------------------------------------------------------
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
       01  SPECD-SEQ             PIC 9 VALUE 0.
       01  CONSUMER-IND-SEQ      PIC 9 VALUE 1.
       01  COMPLIANCE-CODE-SEQ   PIC 9 VALUE 2.
      * PASSWORD EVERY FIELD EXCEPT 3. SPECIAL COMMENT CODE
       01  WORLD-SEQ             PIC 9 VALUE 3.

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       01  CHTRWF-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CHTRWF_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/CHTRWF_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CHTRWF_SCN.CPY".

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

           MOVE "CHTRWF" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE SPACES        TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BRANCH.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE SPACES TO ERRCD.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE CHTRWF-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  CHTRWF-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    CHTRWF-COLUMNS + VDU-WIN-W-MOD
                      LINES   CHTRWF-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS CHTRWF-WINDOW-HANDLE.

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


      *********************************************************
       ENTER-KEYS SECTION.
      *********************************************************
           MOVE 0 TO NEW-FLAG.
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

      * PASSWORD EVERY FIELD EXCEPT 3. SPECIAL COMMENT CODE
           IF ELE NOT = 3
              MOVE WORLD-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE VALUE...." TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

      * TRW SPECIAL CODE
           IF ELE = 3
              IF NEW-FLAG = 0
                 MOVE SPECD-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE TRW SPECIAL CODE.." TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * MAKER OR COMAKER CONSUMER INDICATOR
           IF ELE = 5 OR 6
              IF NEW-FLAG = 0
                 MOVE CONSUMER-IND-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE CONSUMER INDICATOR" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

           IF ELE = 7
              MOVE COMPLIANCE-CODE-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE COMPLIANCE CODE...." TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.
      *-----------------------------------------------------------------
       ENTER-ELE.

           IF ELE = 3
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

       TEST-F1-KEY.
           IF ( F1-KEY )
              MOVE "CLR" TO IO-TYPE
              GO TO ENTRY-MODULE-EXIT.

       TEST-UP-KEY.
           IF ( NOT UP-KEY ) GO TO TEST-DOWN-KEY.

           IF ( NEW-FLAG = 1 )
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

       TEST-DOWN-KEY.
           IF ( NOT DOWN-KEY ) GO TO TEST-F6-KEY.

           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.

       TEST-F6-KEY.
           IF ( NOT F6-KEY ) GO TO TEST-ENTER-KEY.

           MOVE 1 TO ONE.

           IF ELE = 3
              PERFORM CODE-SCAN
              IF IO-FG = 0
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.

       TEST-ENTER-KEY.
           IF ( NOT ENTER-KEY ) GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
                 DEPENDING ON ELE.

      *-----------------------------------------------------------------
       F-01.
           IF NOT ( VDUBUF = "Y" OR "N" OR "E")
              PERFORM INVALID-YES-OR-NO
              GO TO ENTER-ELE.

           MOVE VDUBUF TO LN-SKIP-TRW.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO LN-TRW-STATUS.
           IF LN-TRW-STATUS = "0 " OR " 0"
              MOVE "MUST BE 00 OR SPACES TO CLEAR IT " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LN-TRW-STATUS = "04" OR "21" OR "66" OR "67" OR "68"
                             OR "69" OR "70" OR "85" OR "86" OR
                                  "87" OR "98" OR "42"
              MOVE "NOT VALID WITH METRO2 !!" TO MESS
              PERFORM SEND-MESS
              MOVE "TRY USING A SPECIAL CODE" TO MESS
              PERFORM SEND-MESS.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           IF VDUBUF NOT = SPACES
              MOVE "TS"   TO CD-TYPE
              MOVE VDUBUF TO CD-CODE
              PERFORM GET-CD
              IF IO-FG = 9
                 MOVE " INVALID CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE VDUBUF TO LN-TRW-SPECIAL-CODE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           IF NOT ( VDUBUF = "Y" OR "N" )
              PERFORM INVALID-YES-OR-NO
              GO TO ENTER-ELE.

           MOVE VDUBUF TO LN-CONFIDENTCD.
           GO TO CREATE-ROUTINE.

       F-05.
           MOVE VDUBUF TO LN-TRW-B-CONSUMER-IND TEST-IND.
           IF NOT VALID-INDICATOR
              MOVE "NOT VALID WITH METRO2 !!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-06.
           MOVE VDUBUF TO LN-TRW-J-CONSUMER-IND TEST-IND.
           IF NOT VALID-INDICATOR
              MOVE "NOT VALID WITH METRO2 !!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-07.
           MOVE VDUBUF TO LN-TRW-COMPLIANCE-CODE.
           IF NOT LN-TRW-COMPLIANCE-VALID
              IF LN-TRW-COMPLIANCE-CODE NOT = SPACES
                 MOVE "XA, XB, XC, XD, XE, XF, XG, XH, XJ, XR OR SPACES"
                         TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

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

           PERFORM CREATE-CHANGE-RECORD.

       WRITE-MODULE-END.
           PERFORM CLOSE-LN1-FILE.

       WRITE-MODULE-EXIT.
           EXIT.

      ******************************************************************
       CREATE-CHANGE-RECORD SECTION.
      ******************************************************************

           PERFORM GETDAT.

           PERFORM OPEN-CH-FILE.

           IF ( HOLD-TRW NOT = LN-SKIP-TRW )
              PERFORM CREATE-SETUP-REC
              MOVE "K"         TO CH-CODE
              MOVE 1           TO CH-TYPE
              MOVE HOLD-TRW    TO CH-ORG-CHAR
              MOVE LN-SKIP-TRW TO CH-NEW-CHAR
              PERFORM UPDATE-CH-MEMO-FILE.

           IF ( HOLD-TRW-STATUS NOT = LN-TRW-STATUS )
              PERFORM CREATE-SETUP-REC
              MOVE "K"             TO CH-CODE
              MOVE 2               TO CH-TYPE
              MOVE HOLD-TRW-STATUS TO CH-ORG-CHAR
              MOVE LN-TRW-STATUS   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-MEMO-FILE.

           IF ( HOLD-TRW-SPECIAL-CODE NOT = LN-TRW-SPECIAL-CODE )
              PERFORM CREATE-SETUP-REC
              MOVE "K"                   TO CH-CODE
              MOVE 3                     TO CH-TYPE
              MOVE HOLD-TRW-SPECIAL-CODE TO CH-ORG-CHAR
              MOVE LN-TRW-SPECIAL-CODE   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-MEMO-FILE.

           IF (HOLD-TRW-B-CONSUMER-IND NOT = LN-TRW-B-CONSUMER-IND)
              PERFORM CREATE-SETUP-REC
              MOVE "K"                   TO CH-CODE
              MOVE 4                     TO CH-TYPE
              MOVE HOLD-TRW-B-CONSUMER-IND TO CH-ORG-CHAR
              MOVE LN-TRW-B-CONSUMER-IND   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-MEMO-FILE.

           IF (HOLD-TRW-J-CONSUMER-IND NOT = LN-TRW-J-CONSUMER-IND)
              PERFORM CREATE-SETUP-REC
              MOVE "K"                   TO CH-CODE
              MOVE 5                     TO CH-TYPE
              MOVE HOLD-TRW-J-CONSUMER-IND TO CH-ORG-CHAR
              MOVE LN-TRW-J-CONSUMER-IND   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-MEMO-FILE.

           IF (HOLD-TRW-COMPLIANCE-CODE NOT = LN-TRW-COMPLIANCE-CODE)
              PERFORM CREATE-SETUP-REC
              MOVE "K"                   TO CH-CODE
              MOVE 6                     TO CH-TYPE
              MOVE HOLD-TRW-COMPLIANCE-CODE TO CH-ORG-CHAR
              MOVE LN-TRW-COMPLIANCE-CODE   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-MEMO-FILE.

           IF ( HOLD-CONFIDENT NOT = LN-CONFIDENTCD )
              PERFORM CREATE-SETUP-REC
              MOVE "C"            TO CH-CODE
              MOVE HOLD-CONFIDENT TO CH-ORG-CHAR
              MOVE LN-CONFIDENTCD TO CH-NEW-CHAR
              PERFORM UPDATE-CH-MEMO-FILE.

           PERFORM CLOSE-CH-FILE.

       EXIT-CREATE-CHANGE-REC.
           EXIT.

      ***************************************
       UPDATE-CH-MEMO-FILE SECTION.
      ***************************************
       UPDATE-CH-AGAIN.
           IF CH-CODE = "K"
               MOVE "CREDIT BUREAU REPORTING" TO CH-DESCRIPT
           ELSE
               MOVE "CONFIDENTIAL STATUS CHANGE" TO CH-DESCRIPT.

           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF CH-SEQ < 999
                 ADD 1 TO CH-SEQ
                 GO TO UPDATE-CH-AGAIN.
           

           IF ( BR-CHANGE-MEMO = "Y" )
              MOVE "CL/CHMEMO" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CH-REC
              CANCEL FORM-PROGX.

       UPDATE-CH-MEMO-FILE-EXIT.
           EXIT.

      ***************************
       DISPLAY-FIELDS SECTION.
      ***************************

           MOVE SPACES              TO DISPLAY-BUF.
           MOVE LN-SKIP-TRW         TO D-OLD-010 HOLD-TRW.
           MOVE LN-TRW-STATUS       TO D-OLD-020 HOLD-TRW-STATUS.
           MOVE LN-TRW-SPECIAL-CODE TO D-OLD-030 HOLD-TRW-SPECIAL-CODE.
           MOVE LN-CONFIDENTCD      TO D-OLD-040 HOLD-CONFIDENT.
           MOVE LN-TRW-B-CONSUMER-IND TO D-OLD-050
                                         HOLD-TRW-B-CONSUMER-IND.
           MOVE LN-TRW-J-CONSUMER-IND TO D-OLD-060
                                         HOLD-TRW-J-CONSUMER-IND.

           MOVE LN-TRW-COMPLIANCE-CODE TO D-OLD-070
                                         HOLD-TRW-COMPLIANCE-CODE.
           MOVE LN-TRW-DOFD-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D-DOFD.

           MOVE LN-PLDATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-PLDATE.
           MOVE LN-PLAMT            TO D-PLAMT.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      *********************
       CODE-SCAN SECTION.
      *********************

           MOVE "TS"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A020030." TO VDU.
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
           MOVE "CHTRWF" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
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
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPCHCL.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON CHTRWF-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY CHTRWF-SCREEN UPON CHTRWF-WINDOW-HANDLE.
           MOVE CHTRWF-FORM-FIELDS TO WR-BUF.
           MOVE CHTRWF-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE CHTRWF-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHTRWF-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CHTRWF_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************
       MISC-PARAGRAPHS SECTION.
      ***************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETDAT.CPY".
       COPY "LIBGB/GETENV.CPY".
       CREATE-SETUP-REC.
           PERFORM CLEAR-CH-FILE.
           MOVE BRANCH             TO CH-BRNO.
           MOVE SYS-DATE           TO CH-DATE.
           MOVE EXT-CONAME-USER-ID TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS           TO CH-TIME.
           MOVE 1                  TO CH-SEQ.
           MOVE LN-ACCTNO          TO CH-ACCTNO.
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.

       INVALID-YES-OR-NO.
           MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS.
           PERFORM SEND-MESS.

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

      *********************
       IO-ROUTINES SECTION.
      *********************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ***********************
       END-PROGRAM SECTION.
      ***********************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHTRWF-SCREEN.
           DESTROY CHTRWF-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
