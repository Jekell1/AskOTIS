       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLUPT2.
      *****************************************************************
      *                   (SP)
      *        INSURANCE CLAIMS TRFILE UPDATE - LIFE OF THE SOUTH
      *           THIS PROGRAM WILL READ THE LIFE OF THE SOUTH CLAIMS
      *           AGAIN AND CREATE TRFILE "I" RECORDS FOR REPORTING
      *           ONLY PURPOSE
      *           THE CLAIMS UPDATE MUST ALREADY BE DONE.
      *           FILE NEEDS TO BE .UPD BEFORE THEY RUN THIS
      *
      * HAD TO MAKE THIS A SEPARATE PROGRAM BECAUSE OF CREATING
      * TR'S ON BOTH SYSTEMS.
      * THE ABOVE STATEMENT ISNT TRUE FOR S35 BUT I LEFT IT HERE AS THEY
      * WILL STILL DO THE SAME STEPS AS A15. BUT THIS PROGRAM WONT SKIP
      * OTHER MACHINES ANYMORE.
      *
      *  FILE MUST BE /USR/DATA/R1/ED/CLM.LOS.YYMMDD.UPD RSZ ???
      *                                                (INCLUDES LF(0A))
      *  THIS UPDATE THEN RENAMES IT TO .UPDTR
      *
      * REV:
      *  040526 BLV SKIP TYPE 538 (NONFILE CLAIMS) IF AMOUNT IS NEGATIVE....
      *             MOSTLY THESE ARE RECOVERIES WITH A FEW VOIDS, BUT UNTIL
      *             ASSURANT CAN SEND A FILE WITH ONLY VOIDS & NO RECOVERIES
      *             WE WILL SKIP.  NOTE, TO GET A TOTAL OF NEGATIVES, YOU
      *             CAN RUN FX/FXCLMS (REPORT ONLY!!)
      *  040621 BLV ADD CLAIM-NUMBER, REMOVE EDIT TO SKIP NEGATIVE NONFILES
      *  050105 BLF REVISE FOR UPDATED LOS FILE LAYOUT
      *  050105 BLF FIX INSURANCE TYPE FOR LOS
      *  050105 BLF FIX LENGTH OF OLD-FILENAME IN MOVE COMMAND
      *  050111 BLF UPDATE TI-INSCOMP TO 30, WORLD PR# 366
      *  050809 BLF IN FEB OF 2005, 554 WAS ADDED TO VALID COVERAGE CODES AS
      *             A VALID IUI CODE, BUT DIDN'T GET ADDED HERE, SO TRI-TYPE
      *             WASN'T UPDATED, IT WAS 0;  WORLD PR# 432
      *  BLM 090318 ADD UPDATE OF TI-DAYS-PAID TI-PMT-TYPE, TI-FROM-DATE,
      *             TI-TO-DATE, WORLD PR# 631
      *  BLM 110615 ADDED COVERAGE CODE 551 TO VALID TYPES FOR TEXAS IUI,
      *             WORLD PR# 756
      *  BLM 110627 ADDED COVERAGE CODE 248 TO VALID TYPES FOR A&H,
      *             WORLD PR# 756
      *  BLM 110628 ADDED COVERAGE CODES: 168, 169, 170, 171 FOR LIFE,
      *                                   523 & 527 FOR PROPERTY
      *             WORLD PR# 757
      *  BAH 130109 MOVED COVERAGE CODE 537 FROM PROPERTY TO AUTO
      *             ADDED COVERAGE CODE 557 FOR "IN" (2XXX) INDIANA IUI
      *                                                      WORLD PR# 842
      *  BLM 151119 ADDED COVERAGE CODE 001 & 002 FOR LIFE, WORLD PR# 1121
      *  BAH 170815 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181101 CHANGED ACCESS CALL ON GBFILE TO DIR ACCESS CALL ON BRANCH
      *             DATA FILE SET, GBFILE
      *  BAH 20190719 ADDED CLEAR-TRI-FILE (LPTRICL) FUTURE FIELDS NOT SET
      *  BAH 20210323 ADDED AUTO-TYPE 636 FOR "UT" FORTEGRA LIMITED PHYSICAL
      *              DAMAGE #1493
      * BAH 20220720 ADDED LIFE-TYPE 112/113 FOR SINGLE/JOINT DECR LIFE
      *              WITH DISMEMBERMENT (NONGILA) AND 227/374 FOR
      *              SINGLE/JOINT DECR LIFE (GILA)  FORTEGRA       #1562
      * 2024.0212 BAH ADD 545 TO UNEMP-TYPE #1642
      * 2024.0520 BAH ADDED COVERAGE CODE 390 TO VALID TYPES A&H #1656 S35Q-29
      * BAH 2024.0530 REMOVE BR-MACHINE NOT = EXT-FILPATH-MACHINE S35Q-57
      *************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIMS-FILE ASSIGN TO CLAIMS-PATH
                  ORGANIZATION RELATIVE
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY CLAIMS-KEY
                  FILE STATUS FILE-STAT.
       COPY "LIBGB/GBFSPR.CPY".
       DATA DIVISION.
       FILE SECTION.
      * RSZ 129 (INCLUDING 0A)
       FD  CLAIMS-FILE
           LABEL RECORDS ARE STANDARD.
       01  CLAIMS-REC.
           03  CLAIM-WAIS                 PIC X(4).
           03  CLAIM-BRNO                 PIC 9(4).
           03  CLAIM-COMPANY-CODE         PIC X(2).
           03  CLAIM-AGENT-NO             PIC X(10).
           03  CLAIM-AGENT-NAME           PIC X(32).
           03  CLAIM-POLICY-NO            PIC X(10).
           03  CLAIM-POLICY-SUFFIX        PIC 9(3).
           03  CLAIM-NUMBER               PIC 9(8).
           03  CLAIM-TYPE                 PIC X.
           03  CLAIM-COVERAGE-CODE        PIC 9(3).
           03  CLAIM-CHECK-NO             PIC 9(8).
      * YYYYMMDD
           03  CLAIM-CHECK-DATE           PIC 9(8).
           03  CLAIM-AMOUNT               PIC S9(9)V99.
           03  CLAIM-POSTED-DATE          PIC 9(8).
           03  CLAIM-LOAN-NUMBER          PIC X(20).
           03  CLAIM-LNAME                PIC X(35).
           03  CLAIM-FNAME                PIC X(25).
           03  CLAIM-ID-NUMBER            PIC X(20).
           03  CLAIM-FROM-DATE            PIC 9(8).
           03  CLAIM-TO-DATE              PIC 9(8).
           03  CLAIM-PMT-TYPE             PIC X(10).
           03  CLAIM-DAYS-PAID            PIC 9(5).
           03  CLAIM-CR                   PIC X.
           03  CLAIM-LF                   PIC X.

       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CLUPT2.CBL S35 02/22/24-07:37:34 >".

      *****************************************************************
      *  PATH OF DATED CLAIMS FILE (FOR VOYAGER INSURNACE CLAIMS)
      * FILE IS LOADED BY WORLD
      *****************************************************************
       01  CLAIMS-PATH.
           03  CLAIMS-PATH-BASE          PIC X(09).
           03  FILLER                    PIC X(01) VALUE "/".
           03  CLAIMS-PATH-MACHINE       PIC X(02).
           03  FILLER                    PIC X(12) VALUE "/ED/CLM.LOS.".
      * YYMMDD
           03  CLAIMS-PATH-DATE          PIC 9(6).
           03  CLAIMS-PATH-UPDATE-TAG    PIC X(4) VALUE ".UPD".

       COPY "LIBLP/LP01TRI.CPY".
       COPY "LIBLP/LP01TRI_SQL.CPY".
       COPY "LIBLP/LPWSTRI.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  CLAIMS-KEY         PIC 9(7)   VALUE 1.
       01  PR-IFN             PIC X(3)   VALUE "PR".

       01  HOLD-BRANCH        PIC 9(6) VALUE 0.

       01  FIRST-TIME         PIC X VALUE "Y".
       01  RECORDS-FOUND      PIC X VALUE " ".
           88  NO-RECORDS-FOUND     VALUE "N".

       01  MESS-LIST         PIC X(5) VALUE "MESS.".
       01  MSEL-LIST         PIC X(5) VALUE "MSEL.".
       01  LEGEND            PIC X(70).

      * NEEDED A PATH ONLY UP TO THE STATE/BRANCH FOR DIR-ACCESS CALL
      * TO SKIP BOGUS BRANCHES
      * /USR/DATA/R1/SC0102
       01  HOLD-TEST-PATH    PIC X(19).


       01  TEST-COVERAGE-CODE PIC 9(3).
           88  LIFE-TYPE      VALUE 001 002 100 101 102 103 104 105
                                    112 113 168 169 170 171 227 374.
           88  AH-TYPE        VALUE 200 201 203 248 390.
           88  FIRE-TYPE      VALUE 511 512 517 522 523.
           88  AUTO-TYPE      VALUE 531 537 636.
           88  NONFILE-TYPE   VALUE 520.
           88  UNEMP-TYPE     VALUE 545 550 551 554 556.
           88  UNEMP-IN-TYPE  VALUE 557.
           88  ADD-TYPE       VALUE 570.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  CLAIMS-DATE     PIC 9(8).
           03  TRANS-DATE      PIC 9(8).

       01  VERSION-CONTROL    PIC X VALUE "A".

       01  LOG-CHANGE-MSG.
           03  LOG-BRNOX      PIC X(4).
           03  LOG-BRNO REDEFINES LOG-BRNOX
                              PIC 9(4).
           03  FILLER         PIC X     VALUE SPACES.
           03  LOG-MESS.
               05  LOG-MESS1      PIC X(25) VALUE SPACES.
               05  FILLER         PIC X(6)  VALUE "DATE: ".
               05  LOG-DATE       PIC 99/99/99.

       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CLUPT2_DEF.CPY".
       COPY "LIBSP/CLUPT2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CLUPT2_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                   CLAIMS-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRI1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE CLAIMS-FILE PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *****************************************
       MAIN-PROGRAM SECTION.
      *****************************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLUPT2" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM OPEN-CLAIMS-FILE.

           MOVE 0 TO HOLD-BRANCH.
           MOVE 0 TO CLAIMS-KEY.
           PERFORM START-CLAIMS-FILE.

       NEXT-CLAIM.
           PERFORM READ-CLAIMS-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

      *    IF CLAIM-TYPE = 538
      *       IF CLAIM-AMOUNT < 0
      *          GO TO NEXT-CLAIM.

           IF HOLD-BRANCH = 0
              MOVE CLAIM-BRNO TO HOLD-BRANCH
           ELSE
              IF HOLD-BRANCH NOT = CLAIM-BRNO
                 MOVE CLAIM-BRNO TO HOLD-BRANCH.

           IF CLAIM-BRNO = 0
              MOVE 9990 TO BR-NO
           ELSE
              MOVE CLAIM-BRNO TO BR-NO.

           PERFORM READ-BR-FILE.
      *    IF IO-FG NOT = 0 OR BR-MACHINE NOT = EXT-FILPATH-MACHINE
           IF IO-FG NOT = 0 
              GO TO NEXT-CLAIM.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B" TO TRI-PATH-OVERRIDE.

      * NEED THIS FOR BOGUS BRANCHES
      * /USR/DATA/R1/SC0001
           PERFORM LOAD-TRI1-FILE.
           MOVE TRI-ALL-PATH TO HOLD-TEST-PATH.
           MOVE HOLD-TEST-PATH TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "INVALID BRANCH PATH SKIPPED, BRANCH # &&&&" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              PERFORM SEND-MESS
              GO TO NEXT-CLAIM.

           MOVE "N" TO FIRST-TIME.

           PERFORM OPEN-TRI1-FILE.

           PERFORM CLEAR-TRI-FILE.

           MOVE BR-NO            TO TI-BRNO.
           MOVE TRANS-DATE       TO TI-KEY-DATE.
           MOVE 1                TO TI-SEQ.

      *    COVERAGE CODES:
      *      LIFE:         001 002 100 101 102 103 104 105
      *                    112 113 168 169 170 171 227 374
      *      A&H:          200 201 203 248 390
      *      FIRE:         511 512 517 522 523
      *      AUTO:         531 537
      *      NONFILE:      520
      *      UNEMP:        545 550 551 554 556
      *      UNEMP "IN":   557
      *      AD&D:         570

           MOVE CLAIM-COVERAGE-CODE TO TEST-COVERAGE-CODE.
           IF (CLAIM-BRNO > 1999 AND < 3000) AND UNEMP-IN-TYPE
              MOVE 454 TO TI-TYPE
           ELSE
           IF LIFE-TYPE MOVE 510 TO TI-TYPE
            ELSE
             IF AH-TYPE MOVE 511 TO TI-TYPE
              ELSE
              IF FIRE-TYPE MOVE 527 TO TI-TYPE
                ELSE
                 IF AUTO-TYPE MOVE 528 TO TI-TYPE
                  ELSE
                   IF NONFILE-TYPE MOVE 538 TO TI-TYPE
                    ELSE
                     IF UNEMP-TYPE MOVE 454 TO TI-TYPE
                      ELSE
                       IF ADD-TYPE MOVE 459 TO TI-TYPE
                        ELSE
                          MOVE 000 TO TI-TYPE.

           MOVE CLAIM-WAIS         TO TI-WAIS.
           MOVE CLAIM-AMOUNT       TO TI-AMOUNT.
           MOVE CLAIM-NUMBER       TO TI-CLAIM-NUMBER.
           MOVE CLAIM-LNAME        TO TI-LNAME.
           MOVE CLAIM-FNAME        TO TI-FNAME.
      * YYYYMMDD
           MOVE CLAIM-CHECK-DATE   TO TI-DATE.
           MOVE CLAIM-CHECK-NO     TO TI-DRAFT.
           MOVE CLAIM-BRNO         TO TI-BRNO.
           MOVE CLAIM-DAYS-PAID    TO TI-DAYS-PAID.
           MOVE CLAIM-PMT-TYPE     TO TI-PMT-TYPE.
           MOVE CLAIM-FROM-DATE    TO TI-FROM-DATE.
           MOVE CLAIM-TO-DATE      TO TI-TO-DATE.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TI-USERID.
           MOVE POST-TIME TO TI-POSTTIME.
           MOVE POST-DATE TO TI-POSTDATE.
           MOVE 30 TO TI-INSCOMP.

       NEXT-TRFILE-WRITE.
           PERFORM WRITE-TRI1-FILE.
           IF IO-FG NOT = 0
              IF TI-SEQ < 9999
                 ADD 1 TO TI-SEQ
                 GO TO NEXT-TRFILE-WRITE.

           PERFORM CLOSE-TRI1-FILE.

           GO TO NEXT-CLAIM.


       END-ROUTINE.
      * RENAME CLAIMS.MMDDYY TO CLAIMS.YYMMDD.UPD
           MOVE "LOS CLAIMS TR UPDATE DONE" TO LOG-MESS1.
           MOVE SPACES                 TO LOG-BRNOX.
           PERFORM LOG-CHANGES.

           MOVE MV-CMD TO MV-BUF.
           MOVE CLAIMS-PATH TO MV-FILE1 MV-FILE2.
           INSPECT MV-FILE2 REPLACING FIRST "UPD  " BY "UPDTR".
           PERFORM SYSTEM-CALL.
           IF STAT-BAD
              MOVE "PROBLEM RENAMING CLAIMS FILE " TO MESS
              PERFORM SEND-MESS.

           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           GO TO END-PROGRAM.

      ****************************
       INITIALIZATION SECTION.
      ****************************
           MOVE CLUPT2-QUEUE-POS TO Q-POS.
           MOVE CLUPT2-COPIES-POS TO C-POS.
           MOVE CLUPT2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "UPDATE IN PROGRESS..............." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE CLAIMS-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO LOG-DATE.

           MOVE "START LOS CLAIMS TR UPD" TO LOG-MESS1.
           MOVE SPACES TO LOG-BRNOX.
           PERFORM LOG-CHANGES.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       EO-EXEC-AUDIT SECTION.
      ****************************
           MOVE EXT-EOBUF-DATE2 TO TRANS-DATE CLAIMS-DATE.

           MOVE CLAIMS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD TO CLAIMS-PATH-DATE.
           PERFORM LOAD-CLAIMS-FILE.
           MOVE CLAIMS-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE "CLAIMS INPUT FILE DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD.

      ****************************
       ENTER-PARAMETERS SECTION.
      ****************************

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

        ENTER-CLAIMS-DATE.
           MOVE "ERNM060CLAIMDATE." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ENTER-CLAIMS-DATE.
           MOVE VDU-DATE-YYYYMMDD TO CLAIMS-DATE.

       VERIFY-CLAIMS-PATH.
           MOVE CLAIMS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD TO CLAIMS-PATH-DATE.
           PERFORM LOAD-CLAIMS-FILE.
           MOVE CLAIMS-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE "CLAIMS INPUT FILE DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CLAIMS-DATE.

      *-----------------------------------------------------------------

        ENTER-TRANS-DATE.
           MOVE "ERNM060TRANSDATE." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-CLAIMS-DATE.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ENTER-TRANS-DATE.
           MOVE VDU-DATE-YYYYMMDD TO TRANS-DATE.
           IF TRANS-DATE = 0
              GO TO ENTER-TRANS-DATE.

       EXIT-PARM.
           EXIT.

      ****************************
       DISPLAY-PARAMETERS SECTION.
      ****************************

      ************************************************
       LOG-CHANGES SECTION.
      ************************************************

      * CODE TO WRITE CHANGE FILE.
      * LOG CHANGES TO LOG IN LG DIRECTORY ERR FILE

           MOVE "W"              TO ERRLOGW-ACTION.

           MOVE LOG-BRNO         TO ERRLOGW-BRNO.
           MOVE LOG-MESS         TO ERRLOGW-EOM-MSG.
           MOVE FORM-PATHNAME    TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       COPY "LIBLP/LPTRICL.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CLUPT2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLUPT2-SYSDATE.
           DISPLAY CLUPT2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CLUPT2-FORM-FIELDS TO WR-BUF.
           MOVE CLUPT2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CLUPT2-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLUPT2-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CLUPT2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************
       MISC SECTION.
      ****************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPTRI1IN.CPY".

       LOAD-CLAIMS-FILE.
           MOVE EXT-FILPATH-BASE     TO CLAIMS-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE  TO CLAIMS-PATH-MACHINE.
       OPEN-CLAIMS-FILE.
           PERFORM OPEN-IT.
           MOVE CLAIMS-PATH TO E-FILE.
           OPEN INPUT CLAIMS-FILE.
           IF IO-FG = 8
              GO TO OPEN-CLAIMS-FILE.
           IF IO-FG = 7
              CLOSE CLAIMS-FILE
              GO TO OPEN-CLAIMS-FILE.
       READ-CLAIMS-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CLAIMS-PATH TO E-FILE.
           MOVE CLAIMS-KEY TO E-KEYX.
           READ CLAIMS-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-CLAIMS-FILE-NEXT.
       CLOSE-CLAIMS-FILE.
           PERFORM CLOSE-IT.
           CLOSE CLAIMS-FILE.
       START-CLAIMS-FILE.
           PERFORM START-IT.
           MOVE CLAIMS-PATH TO E-FILE.
           MOVE CLAIMS-KEY TO E-KEYX.
           START CLAIMS-FILE KEY NOT < CLAIMS-KEY.
       COPY "LIBGB/FERRORS.CPY".

      **********************
       END-PROGRAM SECTION.
      **********************

           PERFORM CLOSE-FILES.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/WRLDMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CLUPT2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
