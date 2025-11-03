       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CPMAIN.
      ******************************************************************
      *
      *                 (MN)
      *
      *  DESCRIPTION:   BRANCH COLLECTOR POOL FILE MAINTENANCE
      *
      * REV:
      * KEC 042496 ADDED LEGEND-PROMPTS.
      * MJD 001205 ADDED LOGIC TO TEST FOR FIELD IN FORM. THIS FIELD
      *            WILL DETERMINE WHETHER OF NOT TO READ THE LOCAL POOL FILE OF
      *            READ THE GLOBAL POOL FILE.
      * BLV 030702 ALLOW VALUE OF "B" IN ALPHA FLAG; THIS WILL CAUSE
      *            EXTRACTION TO SPLIT ACCOUNTS AMONG POOL SO THAT THE
      *            BALANCES AMONG POOL MEMBERS ARE AS EVEN AS POSSIBLE,
      *            REGACC PR# 0034
      * MJD 131210 ADDED EXIT-PATHNAME, PROGRAM WAS NOT EXITING BACK TO MENU
      *            PROPERLY.
      *
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * KEC 2018-1026 CONVERTED CPFILE FOR A32 & A34; 
      *               CP-PATH-OVERRIDE TO "G" IF GLOBAL QUEUEING.
      * BAH 190129 SPLIT CDFILE
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
           "@(#)MN/CPMAIN.CBL S35 04/24/24-10:09:21 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01CP.CPY".
       COPY "LIBSP/SP01CP_SQL.CPY".
       COPY "LIBSP/SPWSCP.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

       01  ORIGINAL-CP-REC             PIC X(CP-SIZE).
       01  CHANGED-CP-REC              PIC X(CP-SIZE).

       01  HOLD-CP-ID              PIC X(03).

      ******************************************************************
      *
      *    P R O G R A M - W O R K E R S
      *
      ******************************************************************
       01  ERRCD                       PIC X       VALUE SPACES.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  HOLD-COLLID     PIC X(3).
       01  SUB             PIC 999     VALUE 0.
       01  SUB2            PIC 999     VALUE 0.
       01  TNUM            PIC 9(6)    VALUE 0.
       01  MEMBER-SPEC.
           03  MEMBER-ACT  PIC X    VALUE "E".
           03  FILLER      PIC X(8) VALUE "  A03003".
           03  MEMBER-NUM  PIC 99   VALUE 0.
           03  FILLER      PIC X    VALUE ".".

       01  LEGEND-BUF      PIC X(50).
       01  LEGEND-LIST     PIC X(5) VALUE "MSEL.".

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(20).
           03  F02         PIC X.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(8)  VALUE
               "010 020.".

       01  DISPLAY-BUF2.
           03  F03         PIC X(3)   OCCURS 80.
       01  DISPLAY-LIST2.
           03  FILLER      PIC X(50)  VALUE
               "0301 0302 0303 0304 0305 0306 0307 0308 0309 0310 ".
           03  FILLER      PIC X(50)  VALUE
               "0311 0312 0313 0314 0315 0316 0317 0318 0319 0320 ".
           03  FILLER      PIC X(50)  VALUE
               "0321 0322 0323 0324 0325 0326 0327 0328 0329 0330 ".
           03  FILLER      PIC X(50)  VALUE
               "0331 0332 0333 0334 0335 0336 0337 0338 0339 0340 ".
           03  FILLER      PIC X(50)  VALUE
               "0341 0342 0343 0344 0345 0346 0347 0348 0349 0350 ".
           03  FILLER      PIC X(50)  VALUE
               "0351 0352 0353 0354 0355 0356 0357 0358 0359 0360 ".
           03  FILLER      PIC X(50)  VALUE
               "0361 0362 0363 0364 0365 0366 0367 0368 0369 0370 ".
           03  FILLER      PIC X(50)  VALUE
               "0371 0372 0373 0374 0375 0376 0377 0378 0379 0380.".

      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 3.
       01  SPEC-TABLE.
      * 1. COLLECTOR POOL NAME
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      * 2. ALPHA FLAG
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      * 3. POOL MEMBERS
           03  FILLER      PIC X(8) VALUE "E  A040 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 3 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBWI/CPSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/DATERW.CPY".
       01  CPMAIN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/CPMAIN_DEF.CPY".
       COPY "LIBMN/CPMAIN_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  MAINT-TYPE      PIC X(01).

       SCREEN SECTION.
       COPY "LIBMN/CPMAIN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME MAINT-TYPE.

      *******************************************
       MAIN-PROGRAM SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CPMAIN" TO VDU-FORM-NAME.

      * IF THIS WAS STANDARD, IT WOULD NOT CLEAR PREVIOUS MENU
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS CPMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

      * TEST FOR GLOBAL MAINTENANCE:
      * DISPLAY PROPER TITLE WHEN THIS IS A GLOBAL FILE MAINTENANCE,
      * LOCAL "COLLECTOR POOL FILE MAINTENANCE" IS DEFAULT IN FORM.

           IF MAINT-TYPE = "G"
              MOVE EXT-FILPATH-BASE    TO CP-GLOBAL-BASE
              MOVE EXT-FILPATH-MACHINE TO CP-GLOBAL-MACHINE
              MOVE "G"                 TO CP-PATH-OVERRIDE 
              MOVE CP-GLOBAL-PATH      TO CP-PATH
              MOVE "GLOBAL COLLECTOR POOL FILE MAINTENANCE" TO WR-BUF
              MOVE "TITLE." TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-CP1-FILE.

           PERFORM REC-POOL-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-POOL-MODULE SECTION.
      ******************************************************************
       REC-POOL-MODULE-NO.

           MOVE "        F6-SCAN, F7-EXIT, ENTER COLLECTOR POOL ID:"
                                          TO LEGEND-BUF.
           PERFORM SEND-LEGEND.

           MOVE SPACES         TO DISPLAY-BUF2.
           MOVE 0              TO NEW-FLAG.
           MOVE SPACES         TO IO-TYPE.
           MOVE "E  A030CPID." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              IF MAINT-TYPE = "G"
                 MOVE 0   TO CPSCAN-BRNO
                 MOVE "G" TO CPSCAN-TYPE
              ELSE
                 MOVE EXT-FILPATH-BRANCH TO CPSCAN-BRNO
                 MOVE " "                TO CPSCAN-TYPE
              END-IF
              MOVE "WI/CPSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CPSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF CPSCAN-STATUS NOT = "00"
                 GO TO REC-POOL-MODULE-NO
              ELSE
                 MOVE "S  A000CPID." TO VDU
                 MOVE CPSCAN-BRNO TO       CP-BRNO
                 MOVE CPSCAN-KEY TO VDUBUF CP-ID HOLD-CP-ID
                 PERFORM SCREENIO
                 GO TO READ-POOL-FILE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-POOL-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-POOL-MODULE-NO.

           IF VDUBUF = SPACES
              GO TO REC-POOL-MODULE-NO.

           MOVE VDUBUF TO CP-ID HOLD-CP-ID.

      *-----------------------------------------------------------------
       READ-POOL-FILE.

           PERFORM READ-CP1-FILE.
           PERFORM CLOSE-CP1-FILE.
           IF IO-FG = 0
              MOVE CP-REC TO ORIGINAL-CP-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-POOL-MODULE.
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

           MOVE "                         F1-CLEAR, ENTER FIELD # :"
                TO LEGEND-BUF.
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

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

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

           IF ( ELE = 1 )
              MOVE "             F1-CLEAR, ENTER POOL ID DESCRIPTION :"
                   TO LEGEND-BUF
              PERFORM SEND-LEGEND.

           IF ( ELE = 3 )
              GO TO F-03.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

       TEST-UP.

           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO GOTO-FIELD.

       TEST-DOWN.

           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

       TEST-F6.

           IF VDUSTATUS = "1<"
              IF ELE = 2
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE.

       TEST-F1.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.

           GO TO F-01 F-02 F-03
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
           MOVE VDUBUF TO CP-DESC.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO CP-ALPHA-FLAG.
           IF CP-ALPHA-FLAG NOT = "Y"
              IF CP-ALPHA-FLAG NOT = "N"
                 IF CP-ALPHA-FLAG NOT = "B"
                    MOVE "ENTER 'Y'(ALPHA), 'B'(BALANCE) OR 'N'."
                       TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE "            F1, F3-CLEAR, F4-DEL, F5-INS, F7-DONE:"
                                          TO LEGEND-BUF.
           PERFORM SEND-LEGEND.
           MOVE 0 TO TNUM.
       F-03A.
           ADD 1 TO TNUM.
           IF TNUM > 80
              MOVE 1 TO TNUM.
           MOVE "E" TO MEMBER-ACT.
           MOVE TNUM TO MEMBER-NUM.
           MOVE MEMBER-SPEC TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "16"
              MOVE SPACES TO VDUBUF CP-COL-ID(TNUM)
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "18"
              PERFORM F03-DELETE
              SUBTRACT 1 FROM TNUM
              GO TO F-03A.
           IF VDUSTATUS = "1:"
              PERFORM F03-INSERT
              SUBTRACT 1 FROM TNUM
              GO TO F-03A.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO F-03A.
           IF VDUSTATUS = "1U" AND TNUM > 1
              SUBTRACT 2 FROM TNUM
              GO TO F-03A.
           IF VDUSTATUS = "1>"
              MOVE SPACES TO LEGEND-BUF
              PERFORM SEND-LEGEND
              MOVE DISPLAY-BUF2 TO WR-BUF
              MOVE DISPLAY-LIST2 TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO CREATE-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              SUBTRACT 1 FROM TNUM
              GO TO F-03A.
           IF VDUBUF = SPACES
              MOVE CP-COL-ID(TNUM) TO VDUBUF.
           MOVE VDUBUF TO HOLD-COLLID.

           IF HOLD-COLLID = SPACES
              GO TO SKIP-CP-CHECK.

           PERFORM CHECK-COLLECTOR.
           IF IO-FG NOT = 0
              MOVE "INVALID COLLECTOR" TO MESS
              PERFORM SEND-MESS
              SUBTRACT 1 FROM TNUM
              GO TO F-03A.
       SKIP-CP-CHECK.
           MOVE HOLD-COLLID TO F03(TNUM) CP-COL-ID(TNUM).
           MOVE "S" TO MEMBER-ACT.
           MOVE MEMBER-SPEC TO VDU.
           MOVE F03(TNUM) TO VDUBUF.
           PERFORM SCREENIO.
           IF HOLD-COLLID = SPACES
              SUBTRACT 1 FROM TNUM.
           GO TO F-03A.

       END-ENTRY-MODULE.
           EXIT.

      ***********************************************
       CODE-SCAN SECTION.
      ***********************************************

           MOVE "CN"                  TO CDBSCN-TYPE.
           MOVE SPACES                TO CDBSCN-CODE.
           IF MAINT-TYPE = "G"
              MOVE 0                  TO CDBSCN-BRANCH
           ELSE
              MOVE EXT-FILPATH-BRANCH TO CDBSCN-BRANCH.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS NOT = "00") OR (CDBSCN-CODE = SPACES) 
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE CDBSCN-CODE TO F03(TNUM) CP-COL-ID(TNUM).
           MOVE "S" TO MEMBER-ACT.
           MOVE MEMBER-SPEC    TO VDU.
           MOVE CDBSCN-CODE TO VDUBUF.
           PERFORM SCREENIO.

       EXIT-CODE-SCAN.
           EXIT.

      *************************************
       F03-DELETE SECTION.
      *************************************

           MOVE TNUM TO SUB SUB2.
       F03-DELETE-LOOP.
           ADD 1 TO SUB2.
           IF SUB2 > 80
              GO TO EXIT-F03-DELETE.
           MOVE F03(SUB2) TO F03(SUB).
           MOVE CP-COL-ID(SUB2) TO CP-COL-ID(SUB).
           ADD 1 TO SUB.
           GO TO F03-DELETE-LOOP.
       EXIT-F03-DELETE.
           MOVE SPACES TO F03(80) CP-COL-ID(80).
           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************
       F03-INSERT SECTION.
      *************************************
           MOVE 80 TO SUB SUB2.
       F03-INSERT-LOOP.
           SUBTRACT 1 FROM SUB2.
           IF SUB2 < TNUM
              GO TO EXIT-F03-INSERT.
           MOVE F03(SUB2) TO F03(SUB).
           MOVE CP-COL-ID(SUB2) TO CP-COL-ID(SUB).
           SUBTRACT 1 FROM SUB.
           GO TO F03-INSERT-LOOP.
       EXIT-F03-INSERT.
           MOVE SPACES TO F03(TNUM) CP-COL-ID(TNUM).
           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************
       WRITE-MODULE SECTION.
      ***************************************
           PERFORM OPEN-CP1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE CP-REC TO CHANGED-CP-REC.
           IF ORIGINAL-CP-REC = CHANGED-CP-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-CP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF CP-REC NOT = ORIGINAL-CP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-CP-REC TO CP-REC.
           PERFORM REWRITE-CP1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-CP1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-CP1-FILE.

      ***************************************
       DELETE-MODULE SECTION.
      ***************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM OPEN-CP1-FILE.
           PERFORM READ-CP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF CP-REC NOT = ORIGINAL-CP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-CP1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-CP1-FILE.

      ***************************************
       NEW-RECORD-SETUP SECTION.
      ***************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           PERFORM CLEAR-CP-FILE.

           MOVE HOLD-CP-ID TO CP-ID.
           IF MAINT-TYPE NOT = "G"
              MOVE EXT-FILPATH-BRANCH TO CP-BRNO.

           MOVE 1 TO SUB.
           MOVE 0 TO ELE.
              
      ***************************************
       DISPLAY-FIELDS SECTION.
      ***************************************
           MOVE CP-DESC       TO F01.
           MOVE CP-ALPHA-FLAG TO F02.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 80
              MOVE CP-COL-ID(SUB) TO F03(SUB)
           END-PERFORM.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBSP/SPCPCL.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE CPMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CPMAIN-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO CPMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CPMAIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CPMAIN-SCREEN UPON CPMAIN-WINDOW-HANDLE.
           MOVE CPMAIN-FORM-FIELDS TO WR-BUF.
           MOVE CPMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/CPMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND-BUF TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       CHECK-COLLECTOR.
           MOVE "CN"        TO CDB-TYPE.
           MOVE HOLD-COLLID TO CDB-CODE.
           IF MAINT-TYPE  = "G"
              MOVE ZEROS TO CDB-BRANCH
           ELSE
              MOVE EXT-FILPATH-BRANCH TO CDB-BRANCH.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.

       SEND-YN.
           MOVE " PLEASE ENTER 'Y' OR 'N'!" TO MESS.
           PERFORM SEND-MESS.
       INVALID-TYPE.
           MOVE "POOL NOT ON FILE, RE-ENTER!" TO MESS.
           PERFORM SEND-MESS.
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

      ***************************
       IO-ROUTINES SECTION.
      ***************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CP1-FILE.
           PERFORM CLOSE-CDB1-FILE.
       COPY "LIBSP/SPCPGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBSP/SPCP1IN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ************************************
       END-ROUTINE SECTION.
      ************************************
       EXIT-PROG.
           PERFORM CLOSE-FILES.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN  
           DESTROY CPMAIN-SCREEN        
           DESTROY CPMAIN-WINDOW-HANDLE
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
