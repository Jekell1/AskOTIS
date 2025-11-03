       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMCTR.
      *****************************************************************
      *                   (SP)
      *        INSURANCE CLAIMS TRFILE UPDATE - MOTOR CLUB
      *           THIS PROGRAM WILL READ THE MOTOR CLUB CLAIMS
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
      *  FILE MUST BE /USR/DATA/R#/ED/CLM.MC.YYMMDD.UPD
      *  RSZ ??? (INCLUDES LF(0A))
      *  THIS UPDATE THEN RENAMES IT TO .UPDTR
      *
      * REV:
      * BLF 050111 UPDATE INSURANCE COMPANY 50 TO TI-INSCOMP, WORLD PR# 366
      * BLF 050112 IF CLAIM-WAIS BLANK, MOVE "MC" TO TI-WAIS
      * BAH 170815 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181101 CHANGED ACCESS CALL ON GBFILE TO DIR ACCESS CALL ON BRANCH
      *            DATA FILE SET, GBFILE
      * BAH 20190719 ADDED CLEAR-TRI-FILE (LPTRICL) FUTURE FIELDS NOT SET
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
           03  FILLER                     PIC X(4).
           03  CLAIM-TYPE                 PIC 9(3).
           03  CLAIM-AMOUNT               PIC S9(8)V99.
           03  CLAIM-NUMBER               PIC X(9).
           03  FILLER                     PIC X(13).
           03  CLAIM-LNAME                PIC X(15).
           03  CLAIM-FNAME                PIC X(10).
      * YYMMDD
           03  CLAIM-CHECK-DATE           PIC 9(6).
           03  CLAIM-CHECK-NO             PIC 9(8).
           03  FILLER                     PIC X(40).
           03  CLAIM-PRODUCER             PIC 9(6).
           03  CLAIM-PRODUCER-ALP REDEFINES CLAIM-PRODUCER.
               05  CLAIM-PRODUCER1        PIC X.
               05  CLAIM-BRNO             PIC 9(5).
           03  CLAIM-LF                   PIC X.


       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/CLMCTR S35 06/17/24-07:58:29 >".
      *****************************************************************
      *  PATH OF DATED CLAIMS FILE (FOR VOYAGER INSURNACE CLAIMS)
      * FILE IS LOADED BY WORLD
      *****************************************************************
       01  CLAIMS-PATH.
           03  CLAIMS-PATH-BASE          PIC X(09).
           03  FILLER                    PIC X(01)  VALUE "/".
           03  CLAIMS-PATH-MACHINE       PIC X(02).
           03  FILLER                    PIC X(11)  VALUE "/ED/CLM.MC.".
      * YYMMDD
           03  CLAIMS-PATH-DATE          PIC 9(06).
           03  CLAIMS-PATH-UPDATE-TAG    PIC X(4) VALUE ".UPD".

       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRI.CPY".
       COPY "LIBLP/LP01TRI_SQL.CPY".
       COPY "LIBLP/LPWSTRI.CPY".

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
       COPY "LIBSP/CLMCTR_DEF.CPY".
       COPY "LIBSP/CLMCTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CLMCTR_SCN.CPY".

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

      **********************************************
       MAIN-PROGRAM SECTION.
      **********************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLMCTR" TO VDU-FORM-NAME.

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

           IF HOLD-BRANCH = 0
              MOVE CLAIM-BRNO TO HOLD-BRANCH
           ELSE
              IF HOLD-BRANCH NOT = CLAIM-BRNO
                 MOVE CLAIM-BRNO TO HOLD-BRANCH.

           IF CLAIM-BRNO = 0
              MOVE 9990 TO BR-NO
           ELSE
              MOVE CLAIM-BRNO TO BR-NO.

           MOVE BR-NO TO BR-NO.
           PERFORM READ-BR-FILE.
      *    IF IO-FG NOT = 0 OR BR-MACHINE NOT = EXT-FILPATH-MACHINE
           IF IO-FG NOT = 0 
              GO TO NEXT-CLAIM.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO TRI-PATH-OVERRIDE.

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

           MOVE CLAIM-WAIS       TO TI-WAIS.
           IF CLAIM-WAIS = SPACES
              MOVE "MC  "          TO TI-WAIS.
           MOVE CLAIM-TYPE         TO TI-TYPE.
           MOVE CLAIM-AMOUNT       TO TI-AMOUNT.
           MOVE CLAIM-NUMBER       TO TI-CLAIM-NUMBER.
           MOVE CLAIM-LNAME        TO TI-LNAME.
           MOVE CLAIM-FNAME        TO TI-FNAME.
           MOVE CLAIM-CHECK-DATE TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO TI-DATE.
           MOVE CLAIM-CHECK-NO     TO TI-DRAFT.
           MOVE CLAIM-BRNO         TO TI-BRNO.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID        TO TI-USERID.
           MOVE POST-TIME          TO TI-POSTTIME.
           MOVE POST-DATE          TO TI-POSTDATE.
           MOVE 50                 TO TI-INSCOMP.

       NEXT-TRFILE-WRITE.
           PERFORM WRITE-TRI1-FILE.
           IF IO-FG NOT = 0
              IF TI-SEQ < 99
                 ADD 1 TO TI-SEQ
                 GO TO NEXT-TRFILE-WRITE.

           PERFORM CLOSE-TRI1-FILE.

           GO TO NEXT-CLAIM.


      **************************************
       END-ROUTINE.
      * RENAME CLAIMS.MMDDYY TO CLAIMS.YYMMDD.UPD
           MOVE "UMC CLAIMS TR UPDATE DONE" TO LOG-MESS1.
           MOVE SPACES                 TO LOG-BRNOX.
           PERFORM LOG-CHANGES.

           MOVE MV-CMD TO MV-BUF.
           MOVE CLAIMS-PATH TO MV-FILE1 MV-FILE2.
           INSPECT MV-FILE2 REPLACING FIRST "UPD  " BY "UPDTR"
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
           MOVE CLMCTR-QUEUE-POS TO Q-POS.
           MOVE CLMCTR-COPIES-POS TO C-POS.
           MOVE CLMCTR-PRU-POS   TO P-POS.
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
           MOVE "START UMC CLAIMS TR UPD" TO LOG-MESS1.
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

      * LOG CHANGES TO LOG IN LG DIRECTORY ERR FILE
      * CODE TO WRITE CHANGE FILE.
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
           MOVE EXT-CONAME-CONAME TO CLMCTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLMCTR-SYSDATE.
           DISPLAY CLMCTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CLMCTR-FORM-FIELDS TO WR-BUF.
           MOVE CLMCTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CLMCTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLMCTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CLMCTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************
       MISC SECTION.
      ****************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
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
              DESTROY CLMCTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
