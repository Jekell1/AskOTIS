       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWIMP.
       DATE-WRITTEN.    2024/08/27.
      ******************************************************************
      *                 (SP)
      *
      *  IMPORT BORROWER MASTER
      *
      *  /USR/DATA/R1/FTP/BORROWER/YYYYMMDD.NBLC.A
      *
      *  PROGRAM WILL RUN FROM A CRON OR MAY BE EXECUTED FROM A $
      *
      * ERROR LOG IS /USR/DATA/R1/FTP/BORROWER/YYYYMMDD.ERRLOG
      * UPDATE LOG IS /USR/DATA/R1/FTP/BORROWER/YYYYMMDD.UPDLOG
      *
      * FILE GETS MOVED INTO DONE IF NO FILE ERRORS
      *
      * TESTING/IMPLEMENTATION:
      *
      * THIS PROGRAM RUNS FROM A SHELL EXECUTED BY THE CRON OR JUST
      * BY RUNNING THE SHELL.  SINCE IT WAS WRITTEN TO BE "CRON-FRIENDLY"
      * THERE IS NO SCREEN AND NO DISPLAYS
      * THE SHELL IS CALLED BWIMP.CRON, AND IT SETS THE FIL ENVIRONMENT
      * VARIABLE, SO IT MUST BE SET UP DIFFERENTLY TO RUN IN TEST OR DATA!
      * HERE IS THE SHELL SET UP FOR TEST, NOTE THERE ARE 2 REFERENCES TO TEST
      * THAT MUST BE CHANGED TO DATA TO RUN LIVE:
      *
      * THE SHELL MAY BE RUN FROM COMMAND LINE (EITHER ROOT OR SUPER-USERED)
      * BY TYPING:
      *
      */USR/A??/SH/BWIMP.CRON
      *
      * TO RUN FROM THE CRON, ADD A LINE TO THE ROOT CRON USING THE CRONTAB
      * COMMAND.  FOR EXAMPLE, TO RUN THE SHELL ON THE HOUR FROM 9AM TO 6PM
      * EVERYDAY:
      * 0 9,10,11,12,13,14,15,16,17,18 * * * /USR/A15/SH/BWIMP.CRON
      *
      *
      * REV:
      *  2024.0916 BAH NEW "NB" BORROWER CREATE FOR LIVE CHECKS #1678 S35Q-1678
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BWIMP-FILE ASSIGN BWIMP-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.
           SELECT ERRLOG-FILE ASSIGN ERRLOG-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.
           SELECT UPDLOG-FILE ASSIGN UPDLOG-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      *********************************************************
      *   BORROWER IMPORT FILE RECORD
      **********************************************************
      *XFD  FILE=DIFILE
       FD  BWIMP-FILE
           LABEL RECORDS ARE STANDARD.
       01  BWIMP-REC.
           03  BWIMP-BRANCH                  PIC 9(4).
           03  BWIMP-BRANCHX REDEFINES BWIMP-BRANCH PIC X(4).
           03  BWIMP-SSNO                    PIC 9(9).
           03  BWIMP-SSNOX REDEFINES BWIMP-SSNO
                                             PIC X(9).
           03  BWIMP-FNAME                   PIC X(16).
           03  BWIMP-LNAME                   PIC X(16).
           03  BWIMP-ADR1                    PIC X(24).
           03  BWIMP-ADR2                    PIC X(24).
           03  BWIMP-CITY                    PIC X(15).
           03  BWIMP-STATE                   PIC XX.
           03  BWIMP-ZIP                     PIC X(10).
           03  BWIMP-PHONE                   PIC 9(10).
           03  BWIMP-PHONEX REDEFINES BWIMP-PHONE
                                             PIC X(10).
           03  BWIMP-PHONE-STAT              PIC X.
           03  BWIMP-CELL-PHONE              PIC 9(10).
           03  BWIMP-CELL-PHONEX REDEFINES BWIMP-CELL-PHONE
                                             PIC X(10).
           03  BWIMP-CELL-STAT               PIC X.
           03  BWIMP-WORK-PHONE              PIC 9(10).
           03  BWIMP-WORK-PHONEX REDEFINES BWIMP-WORK-PHONE
                                             PIC X(10).
           03  BWIMP-WORK-PHONE-STAT         PIC X.

           03  BWIMP-BDATE                   PIC 9(8).
           03  BWIMP-BDATEX REDEFINES BWIMP-BDATE PIC X(8).

           03  BWIMP-EMAIL                   PIC X(50).
           03  BWIMP-CR                      PIC X.
           03  BWIMP-LF                      PIC X.

      **************************************************
      *  ERRLOG FILE
      **************************************************
       FD  ERRLOG-FILE
           LABEL RECORDS ARE STANDARD.
       01  ERRLOG-REC.
           03  ERRLOG-BRNOX       PIC X(4).
           03  FILLER             PIC X.
           03  ERRLOG-SSNOX       PIC X(9).
           03  FILLER             PIC X.
           03  ERRLOG-MSG         PIC X(39).

      **************************************************
      *  UPDATE LOG FILE
      **************************************************
       FD  UPDLOG-FILE
           LABEL RECORDS ARE STANDARD.
       01  UPDLOG-REC.
           03  UPDLOG-BRNOX       PIC X(4).
           03  FILLER            PIC X.
           03  UPDLOG-COUNT      PIC 9(8).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)SP/BWIMP S35 09/17/24-15:52:09 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       01  BWIMP-PATH.
           03  BWIMP-PATH-BASE       PIC X(09).
           03  FILLER                PIC X       VALUE "/".
           03  BWIMP-PATH-MACHINE    PIC X(02).
           03  BWIMP-PATH-DIR        PIC X(5)    VALUE "/FTP/".
           03  FILLER                PIC X(9)    VALUE "BORROWER/".
           03  BWIMP-PATH-FILENAME.
               05  BWIMP-PATH-DATE-YYYYMMDD PIC 9(8).
               05  FILLER                   PIC X(7)  VALUE ".NBLC.A".

       01  DONE-PATH.
           03  DONE-PATH-BASE      PIC X(09).
           03  FILLER              PIC X       VALUE "/".
           03  DONE-PATH-MACHINE   PIC X(02).
           03  DONE-PATH-DIR       PIC X(5)    VALUE "/FTP/".
           03  FILLER              PIC X(9)    VALUE "BORROWER/".
           03  FILLER              PIC X(5)    VALUE "DONE/".

       01  ERRLOG-PATH.
           03  ERRLOG-PATH-BASE     PIC X(09).
           03  FILLER                PIC X       VALUE "/".
           03  ERRLOG-PATH-MACHINE  PIC X(02).
           03  ERRLOG-PATH-DIR      PIC X(5)    VALUE "/FTP/".
           03  FILLER               PIC X(9)    VALUE "BORROWER/".
           03  ERRLOG-PATHNAME.
               05  ERRLOG-PATH-DATE PIC 9(8).
               05  FILLER           PIC X(7)    VALUE ".ERRLOG".

       01  UPDLOG-PATH.
           03  UPDLOG-PATH-BASE        PIC X(09).
           03  FILLER                  PIC X       VALUE "/".
           03  UPDLOG-PATH-MACHINE     PIC X(02).
           03  UPDLOG-PATH-DIR         PIC X(5)    VALUE "/FTP/".
           03  FILLER                  PIC X(9)    VALUE "BORROWER/".
           03  UPDLOG-PATHNAME.
               05  UPDLOG-PATH-DATE    PIC 9(8).
               05  FILLER              PIC X(7)    VALUE ".UPDLOG".

       01  ERROR-FOUND              PIC X     VALUE SPACES.
       01  ERRCD                    PIC X     VALUE SPACES.
       01  BRANCH-COUNT             PIC 9(9)  VALUE 0.
       01  GRAND-COUNT              PIC 9(9)  VALUE 0.
       01  HOLD-BRNO            PIC 9(4) VALUE 0.
      * YYYYMMDD
       01  TODAYS-DATE              PIC 9(8).

       01  TEST-ZIPCODE    PIC X(10).
       01  TEST-ZIP-GROUPS REDEFINES TEST-ZIPCODE.
           03  TEST-ZIP-5    PIC X(5).
           03  TEST-ZIP-DASH PIC X.
           03  TEST-ZIP-4    PIC X(4).

       01  SAVE-BM1-KEY.
           03  SAVE-BM-BRNO         PIC 9(4).
           03  SAVE-BM-SSNO         PIC 9(9).
           03  SAVE-BM-DATE         PIC 9(8).
           03  SAVE-BM-SEQ          PIC 9(3).


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       01  FORM-PATH.
           03  FILLER         PIC X.
           03  FORM-REL       PIC X(7).
           03  FILLER         PIC X.
           03  FORM-OBJ       PIC XX.
           03  FILLER         PIC X.
           03  FORM-NAM.
               05  FILLER     PIC X(3).
               05  FORM-PROGX PIC X(6).
           03  FILLER         PIC X.
       01  FORM-PROG          PIC X(6) VALUE " ".

       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME.
           03  FILLER         PIC X(12).
           03  EXIT-PATH      PIC X(9).
           03  FILLER         PIC X.

       PROCEDURE DIVISION.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               BWIMP-FILE ERRLOG-FILE UPDLOG-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               BWIMP-FILE ERRLOG-FILE UPDLOG-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************************************************
      *
      *           MAIN PROGRAM MODULE
      *
      *****************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.

      * DO NOT PUT A SQL-CONNECT HERE, IT MUST BE BELOW

           PERFORM INITIALIZATION.
           IF ERRCD NOT = SPACES
              GO TO STOP-RUN.

           PERFORM PROCESS-BORROWERS.

           GO TO END-ROUTINE.

      **************************************************
       INITIALIZATION SECTION.
      **************************************************

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-BAD
              MOVE SPACES           TO ERRLOG-REC
              MOVE "ENV ERROR: REL" TO ERRLOG-MSG
              PERFORM CREATE-LOG
              MOVE "X" TO ERRCD
              GO TO EXIT-INITIALIZE.

           MOVE GETENV-BUF    TO FORM-REL.
           MOVE "CO"          TO FORM-OBJ.
           MOVE "SP/BWIMP"    TO FORM-NAM.

           MOVE "PIGGY" TO PIGGY-BACK-FLAG.

      * NEED TO GET EXTERNAL VARIABLES SET
           MOVE "CO" TO FORM-OBJ.
           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      * THIS MUST STAY HERE, AFTER SETENV FOR DATABASE ENV VARIABLES
           PERFORM SQL-CONNECT.

           MOVE "CO" TO FORM-OBJ.
           MOVE "SP/BWIMP" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

           CALL "C$TOLOWER" USING BWIMP-PATH-DIR, VALUE 5.
           CALL "C$TOLOWER" USING ERRLOG-PATH-DIR, VALUE 5.
           CALL "C$TOLOWER" USING UPDLOG-PATH-DIR, VALUE 5.
           CALL "C$TOLOWER" USING DONE-PATH-DIR, VALUE 5.
           MOVE EXT-FILPATH-BASE TO BWIMP-PATH-BASE
                                    ERRLOG-PATH-BASE
                                    UPDLOG-PATH-BASE
                                    DONE-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO BWIMP-PATH-MACHINE
                                       ERRLOG-PATH-MACHINE
                                       UPDLOG-PATH-MACHINE
                                       DONE-PATH-MACHINE.
           
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO TODAYS-DATE
                                      BWIMP-PATH-DATE-YYYYMMDD
                                      ERRLOG-PATH-DATE
                                      UPDLOG-PATH-DATE.


           MOVE BWIMP-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE SPACES           TO ERRLOG-REC
              MOVE "PROBLEM ACCESSING IMPORT FILE" TO ERRLOG-MSG
              PERFORM CREATE-LOG
              MOVE "X" TO ERRCD
              GO TO EXIT-INITIALIZE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-ST1-FILE.

       EXIT-INITIALIZE.
           EXIT.

      *******************************
       PROCESS-BORROWERS SECTION.
      *******************************
           MOVE 0 TO BRANCH-COUNT GRAND-COUNT.
           MOVE 0 TO HOLD-BRNO.
           PERFORM OPEN-BWIMP-FILE.

       NEXT-BORROWER.
           PERFORM READ-BWIMP-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO PROCESS-FILE-DONE.

           MOVE "N" TO ERROR-FOUND.

           INSPECT BWIMP-BRANCHX REPLACING ALL " " BY "0".
           IF BWIMP-BRANCHX NOT NUMERIC
              MOVE "NON-NUMERIC BRANCH #" TO ERRLOG-MSG
              PERFORM CREATE-BWIMP-LOG.

           IF BWIMP-SSNOX NOT NUMERIC
              MOVE "NON-NUMERIC SSNO #" TO ERRLOG-MSG
              PERFORM CREATE-BWIMP-LOG.

           IF BWIMP-BRANCH NOT = HOLD-BRNO
              PERFORM CREATE-UPDATE-LOG
              MOVE BWIMP-BRANCH TO HOLD-BRNO BR-NO
              PERFORM SET-FILE-PATHS
              IF IO-FG NOT = 0
                 MOVE "INVALID BRANCH #" TO ERRLOG-MSG
                 PERFORM CREATE-BWIMP-LOG
                 MOVE 0 TO HOLD-BRNO
              ELSE
                 PERFORM OPEN-BW1-FILE
                 PERFORM OPEN-BM1-FILE.

      * VALIDATE ZIPCODE FORMATS

           MOVE BWIMP-ZIP TO TEST-ZIPCODE.

      * VALIDATE ZIPCODE FORMAT 12345
           IF TEST-ZIP-DASH = SPACES
              IF TEST-ZIP-5 NOT NUMERIC
                 PERFORM BAD-ZIPCODE-FORMAT
              ELSE
                 IF TEST-ZIP-4 NOT = SPACES
                    PERFORM BAD-ZIPCODE-FORMAT.

      * VALIDATE ZIPCODE FORMAT 12345-1234
           IF TEST-ZIP-DASH NOT = SPACES
              IF TEST-ZIP-5 NOT NUMERIC
                 PERFORM BAD-ZIPCODE-FORMAT
              ELSE
                 IF TEST-ZIP-DASH NOT = "-"
                    PERFORM BAD-ZIPCODE-FORMAT
                 ELSE
                    IF TEST-ZIP-4 NOT NUMERIC
                       PERFORM BAD-ZIPCODE-FORMAT.

      * VALIDATE NUMERIC BDATE
           IF BWIMP-BDATE NOT NUMERIC
              MOVE "NON-NUMERIC BDATE #: &&&&&&&&" TO ERRLOG-MSG
              INSPECT ERRLOG-MSG REPLACING FIRST "&&&&&&&&"
                                                   BY BWIMP-BDATEX
              PERFORM CREATE-BWIMP-LOG.

           MOVE BWIMP-BDATE TO NUM-DATE.
           IF NUM-DATE = 0
              MOVE "INVALID BIRTH DATE: &&&&&&&&" TO ERRLOG-MSG
              INSPECT ERRLOG-MSG REPLACING FIRST "&&&&&&&&"
                                              BY BWIMP-BDATE
              PERFORM CREATE-BWIMP-LOG
           ELSE
      * 00000918
           IF NUM-CCYY = 0
              MOVE "INVALID BIRTH DATE: &&&&&&&&" TO ERRLOG-MSG
              INSPECT ERRLOG-MSG REPLACING FIRST "&&&&&&&&"
                                              BY BWIMP-BDATE
              PERFORM CREATE-BWIMP-LOG
           ELSE
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 MOVE "INVALID BIRTH DATE: &&&&&&&&" TO ERRLOG-MSG
                 INSPECT ERRLOG-MSG REPLACING FIRST "&&&&&&&&"
                                                 BY BWIMP-BDATE
                 PERFORM CREATE-BWIMP-LOG.

      * TEST BIRTH DATE IN THE FUTURE, GREATER THAN TODAY
           IF BWIMP-BDATE NOT < TODAYS-DATE
              MOVE "FUTURE BIRTH DATE : &&&&&&&&" TO ERRLOG-MSG
              INSPECT ERRLOG-MSG REPLACING FIRST "&&&&&&&&"
                                              BY BWIMP-BDATE
              PERFORM CREATE-BWIMP-LOG
           ELSE
      * VALIDATE AGE 18 (19 IN AL)
              PERFORM TEST-AGE-RESTRICTIONS.

      * OTHER EDIT CHECKS
      
           IF BWIMP-LNAME = SPACES
              MOVE "BLANK LAST NAME" TO ERRLOG-MSG
              PERFORM CREATE-BWIMP-LOG.

           MOVE BWIMP-STATE TO ST1-KEY.
           PERFORM READ-ST1-FILE.
           IF IO-BAD
              MOVE "INVALID STATE IN IMPORT FILE: &&" TO ERRLOG-MSG
              INSPECT ERRLOG-MSG REPLACING FIRST "&&" BY BWIMP-STATE
              PERFORM CREATE-BWIMP-LOG.

           MOVE BWIMP-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-GOOD
              MOVE "SSNO EXISTS" TO ERRLOG-MSG
              PERFORM CREATE-BWIMP-LOG.

           IF ERROR-FOUND = "Y"
              GO TO NEXT-BORROWER.

           PERFORM CREATE-NEW-BORROWER.

           IF IO-FG NOT = 0
              MOVE "BAD WRITE BWFILE" TO ERRLOG-MSG
              PERFORM CREATE-BWIMP-LOG
              GO TO NEXT-BORROWER.

           PERFORM UPDATE-BW-MEMO.

           ADD 1 TO BRANCH-COUNT
                    GRAND-COUNT.

           GO TO NEXT-BORROWER.

       PROCESS-FILE-DONE.
           PERFORM CLOSE-BWIMP-FILE.

      * MOVING FILE TO BORROWER/DONE DIRECTORY.
           MOVE SPACES TO SYSTEM-BUF.
           STRING MV-CMD," ",BWIMP-PATH," ",DONE-PATH
                  DELIMITED BY "  " INTO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

       EXIT-PROCESS-BORROWERS.
           EXIT.

      ****************************
       CREATE-NEW-BORROWER SECTION.
      ****************************

           PERFORM CLEAR-BW-FILE.

           MOVE BWIMP-SSNO            TO BW-SSNO.
           MOVE BWIMP-BRANCH          TO BW-ORGBR BW-OWNBR.
           MOVE BWIMP-FNAME           TO BW-FNAME.
           MOVE BWIMP-LNAME           TO BW-LNAME.
           MOVE BWIMP-ADR1            TO BW-ADR1.
           MOVE BWIMP-ADR2            TO BW-ADR2.
           MOVE BWIMP-CITY            TO BW-CITY.
           MOVE BWIMP-STATE           TO BW-STATE.
           MOVE BWIMP-ZIP             TO BW-ZIP.

           MOVE BWIMP-PHONE           TO BW-PHONE.
           MOVE BWIMP-PHONE-STAT      TO BW-BAD-PHONE-FG.

           MOVE BWIMP-CELL-PHONE      TO BW-CELL-PHONE.
           MOVE BWIMP-CELL-STAT       TO BW-BAD-CELL-FG.

           MOVE BWIMP-WORK-PHONE      TO BW-EMPPHONE.
           MOVE BWIMP-WORK-PHONE-STAT TO BW-BAD-EMPPHONE-FG.

           MOVE TODAYS-DATE           TO BW-REFDATE.
           MOVE BWIMP-BDATE           TO BW-BDATE.

           MOVE BWIMP-EMAIL           TO BW-EMAIL.
      * DEFAULTS
           MOVE "N" TO BW-CONFIDENTIAL
                       BW-PRIVACY-OPT-OUT.
           MOVE "Y" TO BW-SOLICIT
                       BW-CALL-ON-SUNDAY.

           PERFORM WRITE-BW1-FILE. 
           IF IO-FG NOT = 0
              MOVE "BAD WRITE BWFILE" TO ERRLOG-MSG
              PERFORM CREATE-BWIMP-LOG
              GO TO EXIT-CREATE-NEW-BORROWER.
           
       EXIT-CREATE-NEW-BORROWER.
           EXIT.

      ****************************************
       UPDATE-BW-MEMO SECTION.
      ****************************************

           MOVE BM-PATH-OWNBR TO SAVE-BM-BRNO.
           MOVE BW-SSNO       TO SAVE-BM-SSNO.
           COMPUTE SAVE-BM-DATE = 99999999 - TODAYS-DATE.
           MOVE 1             TO SAVE-BM-SEQ.
       UPDATE-BM-FILE-AGAIN.
           MOVE SPACES TO BM-REC.
           MOVE SAVE-BM1-KEY TO BM1-KEY.

           MOVE "BORROWER CREATED VIA BWIMP" TO BM-MEMO.
           MOVE "S"                          TO BM-MEMO-TYPE.

           PERFORM WRITE-BM1-FILE.
           IF IO-BAD
              IF SAVE-BM-SEQ < 999
                 ADD 1 TO SAVE-BM-SEQ
                 GO TO UPDATE-BM-FILE-AGAIN.

       UPDATE-BW-CHANGE-MEMO-EXIT.
           EXIT.

      ******************************************
       CREATE-LOG SECTION.
      ******************************************
           MOVE ERRLOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              PERFORM OPEN-ERRLOG-FILE-EXTEND
           ELSE
              PERFORM OPEN-ERRLOG-FILE-OUTPUT.
           PERFORM WRITE-ERRLOG-FILE.
           PERFORM CLOSE-ERRLOG-FILE.

      ******************************************
       CREATE-BWIMP-LOG SECTION.
      ******************************************
           MOVE ERRLOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              PERFORM OPEN-ERRLOG-FILE-EXTEND
           ELSE
              PERFORM OPEN-ERRLOG-FILE-OUTPUT.
           MOVE BWIMP-BRANCHX TO ERRLOG-BRNOX.
           MOVE BWIMP-SSNOX   TO ERRLOG-SSNOX.
           PERFORM WRITE-ERRLOG-FILE.
           PERFORM CLOSE-ERRLOG-FILE.

           MOVE "Y" TO ERROR-FOUND.

      ******************************************
       CREATE-UPDATE-LOG SECTION.
      ******************************************
           IF HOLD-BRNO = 0
              GO TO EXIT-CREATE-UPDATE-LOG.
           MOVE UPDLOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              PERFORM OPEN-UPDLOG-FILE-EXTEND
           ELSE
              PERFORM OPEN-UPDLOG-FILE-OUTPUT.
           MOVE HOLD-BRNO    TO UPDLOG-BRNOX.
           MOVE BRANCH-COUNT TO UPDLOG-COUNT.
           PERFORM WRITE-UPDLOG-FILE.
           PERFORM CLOSE-UPDLOG-FILE.

           MOVE 0 TO BRANCH-COUNT.
       EXIT-CREATE-UPDATE-LOG.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPBWCL.CPY".

      ******************************************
       MISC SECTION.
      ******************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       TEST-AGE-RESTRICTIONS.
      * VALIDATE AGE 18 (19 IN AL)
           MOVE TODAYS-DATE        TO BAGE-TODAY BAGE-REFDATE.
           MOVE BWIMP-BDATE        TO BAGE-DATE.
           PERFORM BAGE-CALCULATION.
           IF BWIMP-STATE = "AL"
              IF BAGE-AGE NOT > 18
                 MOVE "AGE RESTRICTION: &&&&&&&&" TO ERRLOG-MSG
                 INSPECT ERRLOG-MSG REPLACING FIRST "&&&&&&&&"
                                                 BY BWIMP-BDATE
                 PERFORM CREATE-BWIMP-LOG.

           IF BWIMP-STATE NOT = "AL"
              IF BAGE-AGE NOT > 17
                 MOVE "AGE RESTRICTION: &&&&&&&&" TO ERRLOG-MSG
                 INSPECT ERRLOG-MSG REPLACING FIRST "&&&&&&&&"
                                                 BY BWIMP-BDATE
                 PERFORM CREATE-BWIMP-LOG.

       BAD-ZIPCODE-FORMAT.
           MOVE "BAD ZIPCODE FORMAT; &&&&&&&&&&" TO ERRLOG-MSG.
           INSPECT ERRLOG-MSG REPLACING FIRST "&&&&&&&&&&"
                                             BY BWIMP-ZIP.
           PERFORM CREATE-BWIMP-LOG.
       CREATE-GRAND-UPDATE-LOG.
           MOVE UPDLOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              PERFORM OPEN-UPDLOG-FILE-EXTEND
           ELSE
              PERFORM OPEN-UPDLOG-FILE-OUTPUT.
           MOVE "GRND"      TO UPDLOG-BRNOX.
           MOVE GRAND-COUNT TO UPDLOG-COUNT.
           PERFORM WRITE-UPDLOG-FILE.
           PERFORM CLOSE-UPDLOG-FILE.
       SET-FILE-PATHS.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG = 0
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA

              MOVE "B"          TO BW-PATH-OVERRIDE
                                   BM-PATH-OVERRIDE
           ELSE
              MOVE 9 TO IO-FG.


      ******************************************
       IO-ROUTINES SECTION.
      ******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       OPEN-BWIMP-FILE.
           PERFORM OPEN-IT.
           MOVE BWIMP-PATH TO E-FILE.
           OPEN INPUT BWIMP-FILE.
           IF IO-FG = 8
              GO TO OPEN-BWIMP-FILE.
           IF IO-FG = 7
              CLOSE BWIMP-FILE
              GO TO OPEN-BWIMP-FILE.
       READ-BWIMP-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BWIMP-PATH TO E-FILE.
           READ BWIMP-FILE.
           IF IO-FG = 8
              GO TO READ-BWIMP-FILE-NEXT.
       CLOSE-BWIMP-FILE.
           PERFORM CLOSE-IT.
           CLOSE BWIMP-FILE.

       OPEN-ERRLOG-FILE-EXTEND.
           OPEN EXTEND ERRLOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-ERRLOG-FILE-EXTEND.
           IF IO-FG = 7
              CLOSE ERRLOG-FILE
              GO TO OPEN-ERRLOG-FILE-EXTEND.

       OPEN-ERRLOG-FILE-OUTPUT.
           OPEN OUTPUT ERRLOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-ERRLOG-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE ERRLOG-FILE
              GO TO OPEN-ERRLOG-FILE-OUTPUT.

       OPEN-ERRLOG-FILE.
           OPEN INPUT ERRLOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-ERRLOG-FILE.
           IF IO-FG = 7
              CLOSE ERRLOG-FILE
              GO TO OPEN-ERRLOG-FILE.
       WRITE-ERRLOG-FILE.
           WRITE ERRLOG-REC.
       CLOSE-ERRLOG-FILE.
           CLOSE ERRLOG-FILE.

       OPEN-UPDLOG-FILE-EXTEND.
           OPEN EXTEND UPDLOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-UPDLOG-FILE-EXTEND.
           IF IO-FG = 7
              CLOSE UPDLOG-FILE
              GO TO OPEN-UPDLOG-FILE-EXTEND.

       OPEN-UPDLOG-FILE-OUTPUT.
           OPEN OUTPUT UPDLOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-UPDLOG-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE UPDLOG-FILE
              GO TO OPEN-UPDLOG-FILE-OUTPUT.
       OPEN-UPDLOG-FILE.
           OPEN INPUT UPDLOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-UPDLOG-FILE.
           IF IO-FG = 7
              CLOSE UPDLOG-FILE
              GO TO OPEN-UPDLOG-FILE.
       WRITE-UPDLOG-FILE.
           WRITE UPDLOG-REC.
       CLOSE-UPDLOG-FILE.
           CLOSE UPDLOG-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-ROUTINE.

           PERFORM CREATE-UPDATE-LOG.
           PERFORM CREATE-GRAND-UPDATE-LOG.

           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.

           PERFORM SQL-DISCONNECT.  *> NEED TO BE LAST BEFORE `STOP RUN`
           STOP RUN.
