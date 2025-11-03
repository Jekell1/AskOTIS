       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ZMMAIN.

      ********************************************************
      * THU, AUG 25, 2022  2:31:18 PM
      *
      *                 (UT)
      *
      *  DESCRIPTION:   ZM STATE FILE MAINTENANCE
      *
      * REV:
      *  06/23/2023 MB TRAINING - CONVERTED Z15 ZMMAIN to Z35 YMMAIN
      *                    USE SQL SERVER INSTEAD OF ISAM 
      *  060523 MB  Training - merged files, added fields:
      *          TEST-AMOUNT, TEST-CODE, TEST-DATE, TEST-NUM, TEST-RATE, 
      *          TEST-SSNO
      *  081124 BAH ADDED ST-REINSTATE-WINDOW FOR REGACC PR# 228
      *             IF 'Y', WHEN POSTING 'SX', WI/REINST WILL OPEN
      *  080127 BAH ADDED ST-REPO-RENOTICE FOR REGACC PR# 236
      *  120906 BAH ADDED ST-TITLE-RELEASE-TYPE FOR REGACC PR# 320
      *  140421 BAH ADDED ST-AUCTION-DATE-REQ FOR REGACC PR# 352
      *  140925 KEC {PR#00000} PARADATA <A15> [JAY]
      *         REMOVED LOGIC THAT WAS A TEST ASSIGNMENT FOR ASHLEY.
      *         'ASHLEY-SCAN' WAS CALLING TO WI/ZZSCAN.C
      *
      * KEC 150116 {PR#00356} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *            ADDED SU-FILE; #6 SU-GEOGRAPHIC-CODE
      *  170719 BAH ADDED SU-MAX-LOAN-AVAIL #7. MAX LOAN AVAIL, WORLD PR#1260
      *  180914 BAH ADDED SU-MAX-TAX-LOAN-AMT (#8) AND SU-NSF-FEE-AMT (#9)
      *                                               WORLD PR#1334
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
       DATA DIVISION.
       FILE SECTION.
       
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/ZMMAIN S35 06/27/23-15:47:25 >".

       COPY "LIBUT/UT01ZM.CPY".
       COPY "LIBUT/UT01ZM_SQL.CPY".
       COPY "LIBUT/UTWSZM.CPY".

       01  ERRCD           PIC X VALUE " ".
       01  CHAN            PIC X(4)    VALUE "CHAN".
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-TIME      PIC 9       VALUE 0.
       01  HOLD-STATE      PIC XX.
       01  DISPLAY-FILE-PREFIX         PIC X(02)     VALUE "ZM".

      *=================================================================
      *   ZMCH-REC FROM 'REPLACING LEADING' ON ZMB-REC. (CHANGED-REC)
      *   ZMOR-REC FROM 'REPLACING LEADING' ON ZMB-REC. (ORIGINAL-REC)
      *=================================================================
       COPY "LIBUT/UT01ZM.CPY" REPLACING LEADING "ZMB" BY "ZMBOR".
       COPY "LIBUT/UT01ZM.CPY" REPLACING LEADING "ZMB" BY "ZMBCH".

       77  EXT-IO-ZMFILE               PIC X   VALUE "S".
       01  SUB                         PIC 99  VALUE 0.
       01  WORK-FORMAT-DATE            PIC 99/99/99.
       01  WORK-FORMAT-DATE-RFMT REDEFINES WORK-FORMAT-DATE 
                                       PIC X(8).
       01  WORK-FORMAT-SSNO            PIC 999/99/9999.
       01  WORK-FORMAT-SSNO-RFMT REDEFINES WORK-FORMAT-SSNO 
                                       PIC X(11).

       01  ZMMAIN-WINDOW-HANDLE        PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(16).
           03  F02         PIC X.
           03  F03         PIC X.
           03  F04         PIC X.
           03  F05         PIC X.
           03  F06         PIC 9(04).
           03  F07         PIC Z(6).99.
           03  F08         PIC Z(5).99.
           03  F09         PIC Z(3).99.
           03  F10         PIC Z(7).99.
           03  F11         PIC XX.
           03  F12         PIC X(8).
           03  F13         PIC 9(4).
           03  F14         PIC Z(3).999.
           03  F15         PIC x(11).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(20)  VALUE "010 020 030 040 050 ".
           03  FILLER      PIC X(20)  VALUE "060 070 080 090 100 ".
           03  FILLER      PIC X(20)  VALUE "110 120 130 140 150.".

      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       78  MAX             VALUE 15.
       01  SPEC-TABLE.
      * 1. DESCRIPTION
           03  FILLER      PIC X(8) VALUE "E  A160 ".
      * 2. REINSTATE WINDOW
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 3. REPO RENOTICE FLAG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 4. TITLE RELEASE TYPE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 5. AUCTION DATE REQUIRED
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 6. GEOGRAPHIC CODE
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 7. MAX LOAN AVAILABLE
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 8. MAX TAX LOAN AMOUNT
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * 9. NSF FEE AMOUNT
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      * 10. TEST AMOUNT
           03  FILLER      PIC X(8) VALUE "ENNN072 ".
      * 11. TEST CODE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 12. TEST DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 13. TEST NUM
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 14. TEST RATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 15. TEST SSNO
           03  FILLER      PIC X(8) VALUE "ERNX090 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/GPENVW.CPY".     *> %%%%%%%%%%%%%%%%%%%%%%%%%%
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/ZMMAIN_DEF.CPY".
       COPY "LIBUT/ZMMAIN_WKS.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBUT/ZMSCANW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBUT/ZMMAIN_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ************************
       MAIN-PROGRAM SECTION.
      ************************
      D    STOP MESS.
           PERFORM GET-GPENV.   *> %%%%%%%%%%%%%%%%%%%%
           PERFORM SQL-CONNECT.

      * COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ZMMAIN" TO VDU-FORM-NAME.
           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS ZMMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
      D    STOP MESS.    

           PERFORM FORM-RESET.

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

           PERFORM REC-STATE-CTY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      *****************************
       REC-STATE-CTY-MODULE SECTION.
      *****************************
       REC-STATE-CTY-MODULE-NO.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "E  A020STATE." TO VDU.
           PERFORM SCREENIO.
      D    STOP MESS.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-STATE-CTY-MODULE.

           IF F6-KEY
              PERFORM STATE-SCAN
              IF IO-FG NOT = 0
                 GO TO REC-STATE-CTY-MODULE-NO
              ELSE
                 MOVE "S  A020STATE." TO VDU
                 MOVE ZMSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 MOVE ZMSCAN-KEY TO VDUBUF.

           IF NOT ENTER-KEY
              GO TO REC-STATE-CTY-MODULE-NO.

           IF VDUBUF = SPACES
              GO TO REC-STATE-CTY-MODULE-NO.
           
           MOVE VDUBUF TO ZMB1-KEY HOLD-STATE.

           PERFORM OPEN-ZMB1-FILE.
           PERFORM READ-ZMB1-FILE.
           PERFORM CLOSE-ZMB1-FILE.

           IF IO-BAD
              PERFORM NEW-RECORD-SETUP
              GO TO END-REC-STATE-CTY-MODULE.

           MOVE ZMB-REC TO ZMBOR-REC.
           
           PERFORM DISPLAY-FIELDS.

       END-REC-STATE-CTY-MODULE.
           EXIT.

      *******************
       STATE-SCAN SECTION.
      *******************
           MOVE SPACES      TO ZMSCAN-KEY.
           MOVE ZMSCAN-BUF  TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF    TO ZMSCAN-BUF.

           IF  ( SCAN-STATUS NOT = "00" )
                      OR ZMSCAN-KEY = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-STATE-SCAN.

           MOVE 0 TO IO-FG.

       EXIT-STATE-SCAN.
           EXIT.

      *****************************
       ENTRY-MODULE SECTION.
      *****************************
       BEGIN-ENTRY.

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

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           
           IF ENTER-KEY
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
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                  IF CHKEY NOT = CHAN
                      GO TO MODIFY-ROUTINE.
           
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE 0         TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

       TEST-F4.
           IF UP-KEY
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO GOTO-FIELD.
       TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = VDU---ENTER-KEY
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.

       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06
                 F-07 F-08 F-09 F-10 F-11 F-12 F-13 F-14 F-15
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO ZMB-DESC.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO ZMB-REINSTATE-WINDOW.
           IF VDUBUF NOT = "Y" AND NOT = "N" AND NOT = " "
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO ZMB-REPO-RENOTICE.
           IF VDUBUF NOT = "Y" AND NOT = "N" AND NOT = " "
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO ZMB-TITLE-RELEASE-TYPE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO ZMB-AUCTION-DATE-REQ.
           IF ZMB-AUCTION-DATE-REQ NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM0 TO ZMB-GEOGRAPHIC-CODE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM2 TO ZMB-MAX-LOAN-AVAIL.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO ZMB-MAX-TAX-LOAN-AMT.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO ZMB-NSF-FEE-AMT.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUNUM2 TO ZMB-TEST-AMOUNT.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUBUF TO ZMB-TEST-CODE.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM0 TO ZMB-TEST-DATE.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUNUM0 TO ZMB-TEST-NUM.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUNUM3 TO ZMB-TEST-RATE.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM0 TO ZMB-TEST-SSNO.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *****************************
       WRITE-MODULE SECTION.
      *****************************
       REWRITE-RECORD.
           MOVE "ZM" TO DISPLAY-FILE-PREFIX.
           PERFORM OPEN-ZMB1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE ZMB-REC TO ZMBCH-REC.
           IF ZMBOR-REC = ZMBCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-ZMB1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF ZMB-REC NOT = ZMBOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE ZMBCH-REC TO ZMB-REC.
           PERFORM REWRITE-ZMB1-FILE.

           GO TO END-WRITE-MODULE.
       
       WRITE-RECORD.
           PERFORM WRITE-ZMB1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       
       END-WRITE-MODULE.
           PERFORM CLOSE-ZMB1-FILE.

           MOVE "ZM" TO DISPLAY-FILE-PREFIX.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-ZMB1-FILE.
           
           IF ( NEW-FLAG NOT = ZEROES )
              GO TO DELETE-MODULE-END.

           MOVE "ZM" TO DISPLAY-FILE-PREFIX.
           PERFORM READ-ZMB1-FILE.
           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO DELETE-MODULE-END.

           IF ( ZMB-REC NOT = ZMBOR-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO DELETE-MODULE-END.

           PERFORM DELETE-ZMB1-FILE.
           
       DELETE-MODULE-END.

           PERFORM CLOSE-ZMB1-FILE.

       DELETE-MODULE-EXIT.
           EXIT.

      **************************
       NEW-RECORD-SETUP SECTION.
      **************************
       NEW-RECORD.
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

       CLEAR-NEW-RECORD.
           PERFORM CLEAR-ZM-FILE.

           MOVE HOLD-STATE TO ZMB-CODE.

           MOVE 0 TO ELE.
           MOVE "S  A010020." TO VDU.
           MOVE "N" TO ZMB-REINSTATE-WINDOW VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010030." TO VDU.
           MOVE "N" TO ZMB-REPO-RENOTICE VDUBUF.
           PERFORM SCREENIO.
           
           MOVE "SNNN040060." TO VDU.
           MOVE ZEROES TO ZMB-GEOGRAPHIC-CODE VDUNUM0.
           PERFORM SCREENIO.

      **************************
       DISPLAY-FIELDS SECTION.
      **************************
           MOVE ZMB-DESC               TO F01.
           MOVE ZMB-REINSTATE-WINDOW   TO F02.
           MOVE ZMB-REPO-RENOTICE      TO F03.
           MOVE ZMB-TITLE-RELEASE-TYPE TO F04.
           MOVE ZMB-AUCTION-DATE-REQ   TO F05.
           MOVE ZMB-GEOGRAPHIC-CODE    TO F06.
           MOVE ZMB-MAX-LOAN-AVAIL     TO F07.
           MOVE ZMB-MAX-TAX-LOAN-AMT   TO F08.
           MOVE ZMB-NSF-FEE-AMT        TO F09.

           MOVE ZMB-TEST-AMOUNT        TO F10.         
           MOVE ZMB-TEST-CODE          TO F11.          
           MOVE ZMB-TEST-DATE          TO WORK-FORMAT-DATE.
           MOVE WORK-FORMAT-DATE-RFMT  TO F12.         
           MOVE ZMB-TEST-NUM           TO F13.                  
           MOVE ZMB-TEST-RATE          TO F14.  
           MOVE ZMB-TEST-SSNO          TO WORK-FORMAT-SSNO. 
           INSPECT WORK-FORMAT-SSNO-RFMT REPLACING ALL "/" BY "-".
           MOVE WORK-FORMAT-SSNO-RFMT  TO F15.
 
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.

      ***************************************
       SETUP-LINK-INFO SECTION.
      ***************************************
      *     MOVE STMAIN-HELP--FILE TO HELP-LINK-FILE.
      *     MOVE STMAIN-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME  TO ZMMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ZMMAIN-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY ZMMAIN-SCREEN UPON ZMMAIN-WINDOW-HANDLE.
           MOVE ZMMAIN-FORM-FIELDS TO WR-BUF. 
           MOVE ZMMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.


      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/ZMMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBUT/UTZMCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".     *> %%%%%%%%%%%%%%%%%%%%%%%%%%

      **************************
       MISC-PARAGRAPHS SECTION.
      **************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
  
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED! {##}" TO MESS.
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
           MOVE " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
             TO MESS.
           PERFORM SEND-MESS.

 
      **************************
       IO-ROUTINES SECTION.
      **************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ZMB1-FILE.
       COPY "LIBUT/UTZMGS_SQL.CPY".
       COPY "LIBUT/UTZM1I.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **********************
       END-ROUTINE SECTION.
      **********************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ZMMAIN-SCREEN.
           DESTROY ZMMAIN-WINDOW-HANDLE.
            EXIT PROGRAM.
