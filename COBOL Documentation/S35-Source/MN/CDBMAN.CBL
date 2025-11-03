       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CDBMAN.
      ******************************************************************
      *
      *                 (MN)
      *  DESCRIPTION:   CODE FILE MAINTENANCE - BRANCH IN KEY
      *
      *=================================================================
      *  MN/CDBMAN IS USING CDBMAN-TYPE (LINKAGE) TO KNOW WHICH
      *  CODE TYPE IS BEING MAINTENANCED.
      *
      *  FOLLOWING ARE THE CODE TYPES (CDB-TYPE) & WHICH MAINTENANCE
      *  PROGRAM IS USED:
      *
      *  [CDBMAN] CN - COLLECTOR ID
      *
      *=================================================================
      * REV:
      *
      * BAH 2019-0129 NEW, WANTED TO SPLIT OUT THE CDBFILE FROM CDMAIN
      * SMR 2023-1107 ADDED VCQFILE LOGIC TO RESOLVE DIFFICULTY WITH
      *          THE CQ2-KEY WORKING WITH A SCAN WINDOW AND/OR USING A
      *          START-GREATER IO PARAGRAPH.  USING A TEMPORARY VISION
      *          CQFILE HELPS KEEP THE SAME EXPECT RESULTS FROM A15.
      * JKC 2024-0201 CHANGED A 'GO TO STOP-RUN' TO USE A NORMAL
      *          EXIT PROGRAM OUT OF 'REC-CD-MODULE' USING "END" IN
      *          IO-TYPE.
      * SMR 2025-0318 CHANGED REFERENCES TO VCQ1 TO VCQ2 S35Q-191
      *
      * JKC 2025-0401 S35Q-191 CHANGED USE OF LIBSP/SPVCQ2IN TO BE
      *         LIBSP/SPVC2RN;DID THIS SO WE COULD REMOVE THE
      *         NEED FOR LIBSP/SPVCQ2IN
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBSP/SPFSVCQ.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBSP/SPFDVCQ.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/CDBMAN S35 04/29/25-10:48:21 >".

      ******************************************************************
      *
      *    F I L E - R E C - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBSP/SPWSVCQ.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBSP/SP01CQ.CPY".
       COPY "LIBSP/SP01CQ_SQL.CPY".
       COPY "LIBSP/SPWSCQ.CPY".

      *=================================================================
       01  CHANGED-CDB-REC              PIC X(CDB-SIZE) VALUE SPACES.
       01  ORIGINAL-CDB-REC             PIC X(CDB-SIZE) VALUE SPACES.

       01  CHANGED-CQ-REC               PIC X(CQ-SIZE)  VALUE SPACES.
       01  ORIGINAL-CQ-REC              PIC X(CQ-SIZE)  VALUE SPACES.

      *=================================================================
       01  HOLD-CDB1-KEY.
           03  HOLD-CDB-TYPE                    PIC X(2).
           03  HOLD-CDB-BRANCH                  PIC 9(4).
           03  HOLD-CDB-CODE                    PIC X(8).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ACTION.
           03  CHKEY                   PIC X(04)   VALUE SPACES.
           03  CHNO                    PIC 9(02)   VALUE ZEROES.

       01  CHAN                        PIC X(04).

       01  ELE                         PIC 9(02)   VALUE ZEROES.

       01  ERRCD                       PIC X(01)   VALUE SPACES.
       01  FIRST-TIME                  PIC 9(01)   VALUE ZEROES.
       01  FIRST-TIME-CN               PIC X(01)   VALUE SPACES.
       01  IO-TYPE                     PIC X(03)   VALUE SPACES.
       01  LEGEND                      PIC X(70)   VALUE SPACES.
       01  NEW-FLAG                    PIC 9(01)   VALUE ZEROES.
       01  ONE                         PIC S9(1)   VALUE ZEROES.
       01  SAVE-CQ-COLLID              PIC XXX.
       01  SAVE-REASSIGN               PIC X(01)     VALUE SPACES.

       01  SCR-PROMPT-1            PIC X(40)     VALUE
           "F6-SCAN, F7-EXIT, ENTER CODE:".

       01  SCR-PROMPT-2            PIC X(40)     VALUE
           "^/V, F1-CLEAR, ENTER BRANCH:".


       01  HOLD-COLLECTOR              PIC X(03)     VALUE SPACES.
       01  HOLD-FORM-PATHNAME          PIC X(22)     VALUE SPACES.
       01  HOLD-RESCHEDULE-COLLID      PIC X(03)     VALUE SPACES.
       01  HOLD-SCR-PROMPT             PIC X(40)     VALUE SPACES.
       01  SHOW-BRNO-ACCT-COLLID       PIC X(17).

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************

       01  DISPLAY-BUF              VALUE SPACES.
           03  F01                  PIC X(25).
           03  F02                  PIC X(12).
           03  F03                  PIC Z(5).
           03  F04                  PIC X(6).

       01  DISPLAY-LIST.
           03  FILLER                  PIC X(16)  VALUE
               "010 020 030 040.".

      *=================================================================
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      *=================================================================
       01  SPEC-TABLE-STD.
      *-------------------------------------- 1. SOURCE CODE DESCRIPTION
           03  FILLER PIC X(8) VALUE "E  A350 ".

      *-------------------------------------- 2. MEMO REQUIRED
           03  FILLER PIC X(8) VALUE "E  A010 ".

      *-------------------------------------- 3. SUPERVISOR ONLY
           03  FILLER PIC X(8) VALUE "E  A010 ".

      *-------------------------------------- 4. WORKED
           03  FILLER PIC X(8) VALUE "E  A010 ".

      *-------------------------------------- 5. RESULT CODE REQUIRED
           03  FILLER PIC X(8) VALUE "E  A050 ".

      *-------------------------------------- 6. RESCHEDULE REQUIRED
           03  FILLER PIC X(8) VALUE "E  A010 ".

      *-------------------------------------- 7. RESCHEDULE TIME
           03  FILLER PIC X(8) VALUE "ENNN030 ".

      *---------------------------------------8. ACTIVITY REQUIRED
           03  FILLER PIC X(8) VALUE "E  A010 ".

      *-----------------------------------------------------------------

       01  SPEC-TABLE-STD2            REDEFINES SPEC-TABLE-STD.
           03  SPEC-REC-STD            OCCURS 8 TIMES.
               05  SPEC-STD            PIC X(07).
               05  CHFG-STD            PIC X(01).

      *=================================================================
      *
      *    M A X
      *
      *=================================================================
       01  MAX                         PIC 9(02)     VALUE 1.
       01  MAX-STD                     PIC 9(02)     VALUE 1.

       01  MAX-CN                      PIC 9(02)     VALUE 4.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/VCQUPD_LNK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBWI/CDBSCNW.CPY".

       01  CDBMAN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/CNMAIN_DEF.CPY".
       COPY "LIBMN/CNMAIN_WKS.CPY".

       COPY "LIBGB/HELPLINK.CPY".

      *=================================================================
      *     L I N K A G E - S E C T I O N
      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       01  CDBMAN-TYPE                 PIC XX.
       01  CHANGE-BRNO-FG              PIC X.

       SCREEN SECTION.
       COPY "LIBMN/CNMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME
                  CDBMAN-TYPE CHANGE-BRNO-FG.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               VCQ-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE VCQ-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM ACCESS-CODE-TYPE.
           IF ( STAT NOT = "00" )
              GO TO END-PROGRAM.

      * HAD TO USE FLOATING BECAUSE I AM NOT EXITING THE MENU, NEEDS TO GO
      * OVERTOP OF IT
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE IS CDBMAN-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

      * THIS MOVE IS TO PREVENT PROBLEMS WITH SCAN WINDOWS WHEN CALLED
      * FROM THE INTERRUPT. (F12 WINDOW DOES NOT CANCEL PROPERLY SO
      * FIRST-TIME IS NOT RESET TO DEFAULT VALUE. MJD 030696.

           MOVE ZEROES TO FIRST-TIME.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE "Y" TO VDU-WIMENU-SELECTION.

           IF ( FIRST-TIME = ZEROES )
              MOVE 1 TO FIRST-TIME.

           PERFORM REC-CD-MODULE.

           IF ( IO-TYPE = "END" ) GO TO EXIT-MAIN-PROGRAM.

           PERFORM ENTRY-MODULE.

           IF ( IO-TYPE = "RWR" ) PERFORM WRITE-MODULE.
           IF ( IO-TYPE = "DEL" ) PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

       EXIT-MAIN-PROGRAM.
           GO TO END-PROGRAM.


      ******************************************************************
       REC-CD-MODULE SECTION.
      ******************************************************************
       REC-CD-MODULE-NO.

           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

      * GETTING BRANCH NUMBER FOR A CD-TYPE OF "CN".

       GET-CN-BRANCH-NUMBER.

           IF NOT ( CDBMAN-TYPE = "CN" )
              GO TO GET-CODE-ENTRY.

           MOVE EXT-FILPATH-BRANCH TO CDB-BRANCH HOLD-CDB-BRANCH BR-NO.
           PERFORM GET-BR.
           IF IO-BAD
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CD-MODULE.

           IF BR-GLOBAL-QUEUE = "Y"
              MOVE ZEROS TO CDB-BRANCH HOLD-CDB-BRANCH.

           IF CHANGE-BRNO-FG = "N"
              MOVE "SN N040BRANCH-NO." TO VDU
              MOVE HOLD-CDB-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              IF BR-GLOBAL-QUEUE = "Y"
                 MOVE "GLOBAL COLLECTOR ID FILE MAINTENANCE" TO WR-BUF
              ELSE
                 MOVE "    COLLECTOR ID FILE MAINTENANCE   " TO WR-BUF
              END-IF
              MOVE "TITLE." TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO GET-CODE-ENTRY.

           MOVE SCR-PROMPT-2 TO HOLD-SCR-PROMPT.
           PERFORM SEND-SCREEN-PROMPT.

           MOVE "ENNN040BRANCH-NO." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "10" OR "1U" )
              GO TO REC-CD-MODULE-NO
           ELSE
           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CD-MODULE
           ELSE
           IF ( VDUSTATUS NOT = "00" )
              GO TO GET-CN-BRANCH-NUMBER
           ELSE
              MOVE VDUNUM0 TO CDB-BRANCH HOLD-CDB-BRANCH BR-NO
              IF BR-NO NOT = 0
                 PERFORM GET-BR
                 IF ( IO-FG NOT = 0 )
                    MOVE "INVALID BRANCH" TO MESS
                    PERFORM SEND-MESS
                    GO TO GET-CN-BRANCH-NUMBER.

           IF HOLD-CDB-BRANCH = ZEROS
              MOVE "GLOBAL COLLECTOR ID FILE MAINTENANCE" TO WR-BUF
           ELSE
              MOVE "    COLLECTOR ID FILE MAINTENANCE   " TO WR-BUF.
           MOVE "TITLE." TO WR-LIST.
           PERFORM CALL-WRFORM.

       GET-CODE-ENTRY.

           MOVE SCR-PROMPT-1 TO HOLD-SCR-PROMPT.
           PERFORM SEND-SCREEN-PROMPT.

           MOVE "ER A030SOURCE." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
               PERFORM CODE-SCAN
               IF ( IO-FG NOT = 0 )
                   GO TO GET-CODE-ENTRY
               ELSE
                  MOVE "00" TO VDUSTATUS.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CD-MODULE.

           IF ( VDUSTATUS = "1U" )
              IF ( CDBMAN-TYPE = "CN" )
                  GO TO GET-CN-BRANCH-NUMBER.

           IF ( VDUSTATUS NOT = "00" )
              GO TO GET-CODE-ENTRY.

           IF ( CDBMAN-TYPE = "CN" )
              MOVE CDBMAN-TYPE TO HOLD-CDB-TYPE   CDB-TYPE
      *-----> SET BEFORE          HOLD-CDB-BRANCH CDB-BRANCH
              MOVE VDUBUF      TO HOLD-CDB-CODE   CDB-CODE.

       READ-CDFILE.

           MOVE SPACES TO HOLD-SCR-PROMPT.
           PERFORM SEND-SCREEN-PROMPT.

           MOVE HOLD-CDB1-KEY TO CDB1-KEY.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF ( IO-FG = 0 )
              MOVE CDB-REC TO ORIGINAL-CDB-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-CD-MODULE.
           EXIT.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************

           MOVE " " TO CDBSCN-WINDOW-BUF.
           MOVE CDBMAN-TYPE TO CDBSCN-TYPE.
           MOVE CDB-BRANCH  TO CDBSCN-BRANCH.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS NOT = "00" )
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           IF (( CDBMAN-TYPE = "CN"           )  AND 
               ( CDBSCN-CODE         = SPACES )) 
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE 0 TO IO-FG.

           IF ( CDBMAN-TYPE = "CN" )
              MOVE "SNNN040BRANCH-NO."  TO VDU
              MOVE CDBSCN-BRANCH        TO VDUNUM0 CDB-BRANCH
                                              HOLD-CDB-BRANCH
              PERFORM SCREENIO
              MOVE "S  A030SOURCE."     TO VDU
              MOVE CDBSCN-CODE          TO VDUBUF.

           PERFORM SCREENIO.

           MOVE CDBSCN-CODE          TO VDUBUF.

       EXIT-CODE-SCAN.
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

      D    STOP MESS.
           MOVE SPACES TO CHFG-STD(4).

           IF ( CDBMAN-TYPE = "CN" ) MOVE MAX-CN  TO MAX
           ELSE
                                     MOVE MAX-STD TO MAX.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO ONE.

       CREATE-ROUTINE.

           IF ( NEW-FLAG NOT = 1 )
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

           IF ( F1-KEY ) MOVE "CLR" TO IO-TYPE
                         GO TO END-ENTRY-MODULE.

           IF NOT ( ENTER-KEY ) GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           CALL "C$JUSTIFY" USING ACTION "R".

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( ACTION = "DELETE" )
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF ( CHNO NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( CHNO   = ZEROES  ) GO TO MODIFY-ROUTINE.
           IF ( CHNO   > MAX     ) GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

       ENTER-ELE.
           IF ( CHKEY NOT     = ZEROES ) AND
              ( CHFG-STD(ELE) = SPACES ) AND
              ( NEW-FLAG  NOT = 1      )
              GO TO MODIFY-ROUTINE.

           IF ( CHFG-STD(ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO CREATE-ROUTINE
              ELSE
              IF ( CHKEY NOT = CHAN )
                 GO TO MODIFY-ROUTINE.

           IF ( CDBMAN-TYPE = "CN" ) AND ( ELE = 2 )
              MOVE "E  A120 " TO VDU
           ELSE
           IF ( CDBMAN-TYPE = "CN" ) AND ( ELE = 3 )
              MOVE "ENNN050 " TO VDU
           ELSE
              MOVE SPEC-STD(ELE) TO VDU.

           MOVE ELE    TO VDUROW.
           MOVE ZEROES TO VDUCOL.
           MOVE "."    TO VDUDECIMAL.
           PERFORM SCREENIO.

       TEST-F1-KEY.

           IF NOT ( F1-KEY ) GO TO TEST-UP-KEY.

           MOVE "CLR" TO IO-TYPE.
           GO TO END-ENTRY-MODULE.

       
       TEST-UP-KEY.

           IF NOT ( UP-KEY ) GO TO TEST-DOWN-KEY.

           IF ( NEW-FLAG = 1 )
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       
       TEST-DOWN-KEY.

           IF NOT ( DOWN-KEY ) GO TO TEST-F6-KEY.

           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.
       
       TEST-F6-KEY.
           IF NOT ( VDUSTATUS = "1<" ) GO TO TEST-ENTER-KEY.

       TEST-ENTER-KEY.

           IF NOT ( ENTER-KEY ) GO TO ENTER-ELE.

           MOVE 1 TO ONE.
       
       GOTO-FIELD.

              GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09
                    F-10 F-11 F-12
                    DEPENDING ON ELE.

       F-01.

           MOVE VDUBUF TO CDB-DESC.
           GO TO CREATE-ROUTINE.
       
       F-02.

           IF CDBMAN-TYPE = "CN"
              PERFORM ENTER-PHONE
              IF TEL-BUFX = " "
                  MOVE 0 TO TEL-BUF
              END-IF
              IF TEL-BUFX IS NOT NUMERIC
                 GO TO ENTER-ELE
              ELSE
                 MOVE TEL-BUF TO CDB-CN-PHONE
                 PERFORM SEND-PHONE
                 MOVE "S  A120020." TO VDU
                 MOVE "-" TO TEL-H1 TEL-H2
                 MOVE TEL-EDIT TO VDUBUF
                 PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       F-03.

           IF CDBMAN-TYPE = "CN"
              MOVE VDUNUM0 TO CDB-CN-PHONE-EXT.
           GO TO CREATE-ROUTINE.

       F-04.
           IF CDBMAN-TYPE = "CN"
              MOVE VDUBUF TO CDB-CN-GROUP-ID.
           GO TO CREATE-ROUTINE.

       F-05.
           GO TO CREATE-ROUTINE.

       F-06.
           GO TO CREATE-ROUTINE.
       
       F-07.
           GO TO CREATE-ROUTINE.
       
       F-08.
           GO TO CREATE-ROUTINE.

       F-09.
           GO TO CREATE-ROUTINE.
       
       F-10.
           GO TO CREATE-ROUTINE.
       
       F-11.
           GO TO CREATE-ROUTINE.
       
       F-12.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-CDB1-FILE.
           IF ( NEW-FLAG NOT = ZEROES )
              GO TO WRITE-RECORD.

           MOVE CDB-REC TO CHANGED-CDB-REC.

           IF ( ORIGINAL-CDB-REC = CHANGED-CDB-REC )
              GO TO END-WRITE-MODULE.

           PERFORM READ-CDB1-FILE.

           IF ( IO-FG NOT = ZEROES )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF ( CDB-REC NOT = ORIGINAL-CDB-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-CDB-REC TO CDB-REC.
           PERFORM REWRITE-CDB1-FILE.
           GO TO END-WRITE-MODULE.

       
       WRITE-RECORD.

           PERFORM WRITE-CDB1-FILE.
           IF ( IO-FG NOT = ZEROES )
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-CDB1-FILE.

       EXIT-WRITE-MODULE.
           EXIT.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************

           IF ( NEW-FLAG NOT = ZEROES )
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           IF ( IO-FG NOT = ZEROES )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF ( CDB-REC NOT = ORIGINAL-CDB-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           MOVE CDB-CODE TO HOLD-COLLECTOR.

           IF ( CDB-TYPE           = "CN" ) AND
              ( BR-COLLECTOR-QUEUE = "Y"  )
              PERFORM CHECK-COLLECTOR-QUEUE.

           IF ( IO-TYPE = "CLR" )
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-CDB1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-CDB1-FILE.

       EXIT-DELETE-MODULE.
           EXIT.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************

           MOVE 1                    TO NEW-FLAG.
           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE ZEROES   TO ELE.

           PERFORM CLEAR-CDB-FILE.

           MOVE HOLD-CDB1-KEY TO CDB1-KEY.

       EXIT-NEW-RECORD-SETUP.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES TO DISPLAY-BUF.

           MOVE CDB-DESC         TO F01.
           IF CDB-CN-PHONE = 0
              MOVE SPACES        TO F02
           ELSE
              MOVE CDB-CN-PHONE  TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT      TO F02.

           MOVE CDB-CN-PHONE-EXT TO F03.
           MOVE CDB-CN-GROUP-ID  TO F04.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       EXIT-DISPLAY-FIELDS.
           EXIT.

      ******************************************************************
       ACCESS-CODE-TYPE SECTION.
      ******************************************************************

           MOVE FORM-PATHNAME TO HOLD-FORM-PATHNAME FORM-PATH.

           IF ( CDBMAN-TYPE = "CN" )
              MOVE "MN/CNMAIN" TO FORM-NAM
           ELSE
              MOVE "XX" TO STAT
              GO TO EXIT-ACCESS-CODE-TYPE.

       END-ACCESS-CODE-TYPE.
           MOVE FORM-PATH TO FORM-PATHNAME.

           MOVE FORM-PROGX TO VDU-FORM-NAME.

      * CHECK SECURITY
       COPY "LIBGB/CHKSEC.CPY".

           MOVE HOLD-FORM-PATHNAME TO FORM-PATHNAME FORM-PATH.

       EXIT-ACCESS-CODE-TYPE.
           EXIT.

      ******************************************************************
       CHECK-COLLECTOR-QUEUE SECTION.
      ******************************************************************

      * CHECK DELETED COLLECTORS QUEUE FOR RESCHEDULES TO SEE IF WE
      * SHOULD DELETE RESCHEDULES OR RE-ASSIGN TO A DIFFERENT COLLECTOR

           MOVE "Y" TO FIRST-TIME-CN.

           PERFORM OPEN-CQ1-FILE.

           IF ( HOLD-CDB-BRANCH = ZEROES )
              MOVE EXT-FILPATH-BASE        TO FILPATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO FILPATH-MACHINE
              MOVE EXT-FILPATH-DATA        TO FILPATH-DATA
           ELSE
              MOVE EXT-FILPATH-BASE        TO FILPATH-BASE
              MOVE BR-MACHINE              TO FILPATH-MACHINE
              MOVE BR-DATA-PATH            TO FILPATH-DATA.

           MOVE FILPATH-BASE               TO VCQ-ALL-BASE.
           MOVE FILPATH-MACHINE            TO VCQ-ALL-MACHINE.
           MOVE FILPATH-BRANCH             TO VCQ-ALL-BRNO.
           MOVE VCQ-ALL-PATH               TO VCQ-PATH.

           MOVE SPACES                     TO VCQ2-KEY.
           MOVE HOLD-COLLECTOR             TO VCQ-COLLID.
           MOVE 0                          TO VCQ-REV-BEHAVIOR-RISK.
           MOVE "R"                        TO VCQ-SEQ-FG.
           MOVE 19000101                   TO VCQ-DATE.
           MOVE 0                          TO VCQ-START-TIME
                                              VCQ-REV-DEL
                                              VCQ-REV-PDUE-DAYS
                                              VCQ-REV-BAL
                                              VCQ-SEQ.

           PERFORM OPEN-VCQ2-FILE.
           PERFORM START-VCQ2-FILE.

       GET-NEXT-CQ.
           PERFORM READ-VCQ2-FILE-NEXT.

           IF IO-FG = 9  
              GO TO END-CHECK-COLLECTOR-QUEUE.

           MOVE VCQ-CQ-REC TO CQ-REC ORIGINAL-CQ-REC.

           IF (CQ-BRNO NOT = CQ-PATH-OWNBR)
              GO TO END-CHECK-COLLECTOR-QUEUE.

           IF CQ-COLLID NOT = HOLD-COLLECTOR
              GO TO END-CHECK-COLLECTOR-QUEUE.                   

           IF FIRST-TIME-CN = "Y"
              PERFORM BEGIN-ASK-CHECK-COLL-Q.

           IF IO-TYPE = "CLR"
              GO TO END-CHECK-COLLECTOR-QUEUE.

           MOVE CQ-REC TO CHANGED-CQ-REC.
           PERFORM READ-CQ1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              MOVE "CLR" TO IO-TYPE
              GO TO END-CHECK-COLLECTOR-QUEUE.

           IF CQ-REC NOT = ORIGINAL-CQ-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              MOVE "CLR" TO IO-TYPE
              GO TO END-CHECK-COLLECTOR-QUEUE.

           MOVE CHANGED-CQ-REC TO CQ-REC.

           IF SAVE-REASSIGN = "R"
              PERFORM ASSIGN-COLL-QUEUE-RESCHEDULE
           ELSE
              PERFORM DELETE-COLL-QUEUE-RESCHEDULE.

           GO TO GET-NEXT-CQ.

       ASSIGN-COLL-QUEUE-RESCHEDULE.

      *    MOVE CQ-COLLID              TO SAVE-CQ-COLLID.

           MOVE HOLD-RESCHEDULE-COLLID TO CQ-COLLID.
           PERFORM REWRITE-CQ1-FILE.

           MOVE "R"       TO VCQUPD-ACTION.            *> REWRITE
           MOVE BR-NO     TO VCQUPD-BRNO.
           MOVE CQ-REC    TO VCQUPD-CQ-REC.
           MOVE SPACES    TO VCQUPD-VCQ-REC.
           PERFORM UPDATE-VCQ1-FILE-VCQUPD.

           IF ( IO-BAD )
              MOVE SPACES TO SHOW-BRNO-ACCT-COLLID
              STRING CQ-BRNO, "-",
                     CQ-ACCTNO, "-",
                     CQ-COLLID
                     DELIMITED BY SIZE INTO SHOW-BRNO-ACCT-COLLID
              DISPLAY MESSAGE BOX
                      "ERROR: QUEUE RECORD UPDATE FAILED", X"0A"
                      " ", X"0A"
                      "COLLECTOR ACCOUNT: ",
                       SHOW-BRNO-ACCT-COLLID, X"0A"
                      "STOP!!! CALL BRANCH SUPPORT IMMEDIATELY!!!"
                      "NEED TO REBUILD COLLECTOR QUEUE"
              GO TO END-PROGRAM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE SAVE-CQ-COLLID         TO CQ-COLLID.

      *    PERFORM START-CQ2-FILE-GREATER.

       DELETE-COLL-QUEUE-RESCHEDULE.
           PERFORM DELETE-CQ1-FILE.
           PERFORM DELETE-VCQ1-FILE.

       END-CHECK-COLLECTOR-QUEUE.
           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-VCQ2-FILE.

       EXIT-CHECK-COLLECTOR-QUEUE.
           EXIT.

      *************************************************
       BEGIN-ASK-CHECK-COLL-Q SECTION.
      *************************************************

           MOVE "S  A440PROMPT1." TO VDU.
           STRING "RESCHEDULES EXIST - DELETE OR REASSIGN, "
                  "(D/R), F1-CANCEL"  DELIMITED BY SIZE INTO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010SEL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ASK-CHECK-COLL-QUEUE.

           IF VDUSTATUS NOT = "00"
              GO TO BEGIN-ASK-CHECK-COLL-Q.

           IF NOT (VDUBUF = "D" OR "R")
              GO TO BEGIN-ASK-CHECK-COLL-Q.

           MOVE VDUBUF TO SAVE-REASSIGN.

           IF SAVE-REASSIGN = "D"
              GO TO END-ASK-CHECK-COLL-QUEUE.

       

       NEXT-ASK-CHECK-COLL-QUEUE.

           MOVE "S  A230PROMPT2." TO VDU.
           MOVE "COLLECTOR TO ASSIGN, F1-CANCEL" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A030COLID." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO BEGIN-ASK-CHECK-COLL-Q.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ASK-CHECK-COLL-QUEUE.

           IF VDUSTATUS NOT = "00"
              GO TO NEXT-ASK-CHECK-COLL-QUEUE.

           MOVE VDUBUF TO CDB-CODE HOLD-RESCHEDULE-COLLID.
           MOVE "CN"            TO CDB-TYPE.
           MOVE HOLD-CDB-BRANCH TO CDB-BRANCH.
           PERFORM READ-CDB1-FILE.
           IF ( IO-FG NOT = ZEROES )
              MOVE "INVALID COLLECTOR ID" TO MESS
              PERFORM SEND-MESS
              GO TO NEXT-ASK-CHECK-COLL-QUEUE.

      * RESTORE ORIGINAL CD REC IN BUFFER FOR DELETE
           MOVE HOLD-CDB1-KEY TO CDB1-KEY.
           PERFORM READ-CDB1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       END-ASK-CHECK-COLL-QUEUE.

           MOVE "N" TO FIRST-TIME-CN.

       EXIT-ASK-CHECK-COLL-QUEUE.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPCDBCL.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           EVALUATE CDBMAN-TYPE
              WHEN "CN" DISPLAY CNMAIN-SCREEN UPON CDBMAN-WINDOW-HANDLE
                        MOVE CNMAIN-FORM-FIELDS TO WR-BUF
                        MOVE CNMAIN-FORM-LIST   TO WR-LIST
           END-EVALUATE.

           PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-NAM-DATA TO WR-BUF.
           MOVE NAM-LST             TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           EVALUATE CDBMAN-TYPE
              WHEN "CN" MOVE CNMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE CNMAIN-HELP--DIR TO HELP-LINK-DIR
           END-EVALUATE.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/CNMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
       SEND-SCREEN-PROMPT.
           MOVE "S  A700MSEL." TO VDU.
           MOVE HOLD-SCR-PROMPT      TO VDUBUF.
           PERFORM SCREENIO.
       SEND-LEGEND.
           MOVE "MSEL." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.
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


      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPCQGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCDB1IN.CPY".
       COPY "LIBSP/SPCQ1IN.CPY".
       COPY "LIBSP/SPCQ2RN.CPY".
       COPY "LIBSP/SPVCQ1IN.CPY".
       COPY "LIBSP/SPVCQ2RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           EVALUATE CDBMAN-TYPE
              WHEN "CN" DESTROY CNMAIN-SCREEN
           END-EVALUATE.

           DESTROY CDBMAN-WINDOW-HANDLE.

           MOVE EXIT-PATHNAME TO FORM-PATH.
           EXIT PROGRAM.
