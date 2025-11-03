       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EOCRON.
       DATE-WRITTEN.     6/13/05.
      ******************************************************************
      * DIR :  SP
      * DESC:  EXECUTE BATCH FROM CRON
      *
      * NOTE: IF IT RUNS IN BATCH, IT CAN BE RUN THROUGH EOCRON.
      *       EOCRON DOES THE SAME STUFF AS EOEXEC, BUT INSTEAD OF
      *       SCREEN INPUT, IT GETS THE STEPS & DATE OFFSETS FROM A
      *       SHELL. THE SHELL GETS RUN BY THE CRON. KG OR KM DO THE
      *       SHELL AND THE CRON INPUT. (BARB 3/2014)
      ********
      * IMPORTANT: WHEN GOANYWHERE RUNS A JOB, IT USES SSH WITH A SHELL
      *            SSH A9990GAW@AZLOAD /US2/DA35/R1/CRON/REF_CUSTM.SH
      *            WHEN EXECUTING A SHELL VIA SSH, LINUX DOES NOT EXECUTE
      *            .BASH_PROFILE, IT EXECUTES .BASHRC
      *            IT ALSO DOES NOT EXECUTE /ETC/PROFILE WHICH ELIMINATES
      *            EXECUTING /ETC/LOCALPROFILE WHICH HAS MORE OF OUR
      *            ENVIRONMENT VARIABLES IN IT, MAINLY OPSYS=LIN WHICH IS
      *            USED IN BATCH PAYMENTS REFUPD FOR A SORT COMMAND.
      *       MUST ADD THE FOLLOWING LINE AS THE LAST LINE IN THE
      *       GOANYWHERE LOGIN'S .BASHRC;
      *       IF [ -X /ETC/PARADATA/LOCALPROFILE ] ; THEN
      *             . /ETC/PARADATA/LOCALPROFILE;
      *       FI
      ********
      *
      * STORED IN /USR/DATA/R1/CRON
      *
      * CREATE A SHELL IN SH THAT HAS;
      *
      *UMASK 000
      *EXPORT REL=USR/S35
      *EXPORT TERM=VT220
      *EXPORT POSTNAME=A9990PFS
      *EXPORT LOGNAME=A9990PFS
      *EXPORT FIL=US1/KC34/R1/SC9990
      *EXPORT PATH=/OPT/ACUCORP/104/BIN:/USR/S35/SH:/USR/LOCAL/BIN:/USR/BIN:/USR/SBIN
      *EXPORT LD_LIBRARY_PATH=/OPT/ACUCORP/104/LIB
      *CD /$FIL
      *RUNCBL -C /$REL/SH/XCBLCONFIG /$REL/CO/EOCRON OTH +00000 +00000 0010 0020
      *
      *   /$REL/CO/EOCRON OTH 060105 063005 0001 9999 >/DEV/NULL
      *                   -A- -B---- -C---- -D-- -E--
      *
      *                      A = NAME OF BATCH
      *                      B = BEGINNING DATE (MMDDYY)
      *                      C = ENDING DATE    (MMDDYY)
      *                      D = BEGINNING STEP
      *                      E = ENDING STEP
      *
      *=================================================================
      * JKC 2019-0716, FOR A15 WHICH WILL CARRY INTO A30+, ALL OF
      * WORLD'S EOCRON SHELLS ARE LOCATED IN /USR/DATA/R#/CRON.
      * THE BEGINING AND ENDING DATES CAN BE DESIGNED MULTIPLE WAYS:
      *
      * +00000 = USE CURRENT SYSTEM DATE
      * -00000 = USE CURRENT SYSTEM DATE
      * +00001 = USE CURRENT SYSTEM DATE PLUS 1 DAY
      * +00014 = USE CURRENT SYSTEM DATE PLUS 14 DAYS
      * -00001 = USE CURRENT SYSTEM DATE MINUS 1 DAY
      * -00014 = USE CURRENT SYSTEM DATE MINUS 14 DAYS
      * M00001 = USE CURRENT SYSTEM DATE'S 1ST DAY OF THE MONTH
      * M00031 = USE CURRENT SYSTEM DATE'S 31ST DAY OF THE MONTH
      *          OR LAST DAY (28,29,30) IF 31ST IS INVALID
      * M+0100 = USE CURRENT SYSTEM DATE'S 1ST DAY OF THE MONTH
      *          PLUS 1 MONTH
      * M+0115 = USE CURRENT SYSTEM DATE'S 15TH DAY OF THE MONTH
      *          PLUS 1 MONTH
      * M+0031 = USE CURRENT SYSTEM DATE'S 31ST DAY OF THE MONTH
      *          OR LAST DAY (28,29,30) IF 31ST IS INVALID
      *          PLUS 1 MONTH
      * M-0600 = USE CURRENT SYSTEM DATE'S 1ST DAY OF THE MONTH
      *          MINUS 6 MONTHS
      * M-0615 = USE CURRENT SYSTEM DATE'S 15TH DAY OF THE MONTH
      *          MINUS 6 MONTHS
      * M-0631 = USE CURRENT SYSTEM DATE'S 31ST DAY OF THE MONTH
      *          OR LAST DAY (28,29,30) IF 31ST IS INVALID
      *          MINUS 6 MONTHS
      * 010150 = USE 01/01/1950
      * 123149 = USE 12/31/1949
      *
      * FOR MORE EXAMPLES LOOK AT Z:\DOCUMENTATION\EOCRON SHELLS.ONE
      *
      *=================================================================
      * REV:
      * BLF 050621 ADDED CHMOD 777 OF SPOOL DIRECTORY & CONTENTS
      * BLF 050622 CHANGED BEG & ENDING DATE OFFSETS TO BE 6 LONG, ALLOWING
      *            A SPECIFIC DATE AS A PARAMETER
      * BLF 051013 ADDED REDEFINITION OF FORM-PATHNAME FOR TEST IN
      *            DECLARATIVES
      *  CS 140603 REPLACED MKDIR-CMD WITH C$MAKEDIR
      * BLM 140508 CHANGED CHMOD TO SET 770 INSTEAD OF 777 IF GP-I-AM-REGACC,
      *            ADDED GETENV, REMOVED COPY MEMBER LIBSP/EOCRON.CMD & PUT
      *            CHMOD COMMAND IN COBOL, REGACC PR# 353
      * BLM 101123 ADD NEW DEFINITION OF BEG & END OFFSET DATES.
      *            EXISTING DEFINITIONS:
      *              A LEADING SIGN & 5 DIGITS = OFFSET FROM TODAY'S DATE, SO
      *              +0000000 IS TODAY, -0000001 IS YESTERDAY,
      *              +0000001 IS TOMORROW
      *
      *              CAPITAL M & 7 DIGITS IS A DAY IN THE CURRENT MONTH, SO
      *              M0000001 IS THE 1ST OF THIS MONTH, M0000031 IS THE 31ST.
      *
      *              HARD-CODED DATE1 - DATE2 (YYYYMMDD)
      *
      *              NEW DEFINITION:
      *              CAPITAL M, THEN A SIGN, THEN 2 DIGITS MONTH, 2 DIGITS DAY,
      *                          ENDING WITH 00
      *              SO M-010200 MEANS 2ND OF PRIOR MONTH
      *              SO M+013100 MEANS LAST DAY OF NEXT MONTH
      *              SO M-030000 MEANS CURRENT DAY 3 MONTHS PRIOR
      *            WORLD PR# 1122
      *  20170901 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *  20190718 JKC QA0054
      *           KEVIN MULLEN CHOSE TO CHANGE THE PROGRAM TO ACCEPT THE
      *           EXISTING 6-DIGITS DATES.
      * BAH 20220816 COMMENTED OUT LOGUID
      * JKC 2024-0206 ADDED OTIS TRACE LOGGING LOGIC; DISABLE TRACE
      * BAH 2024-0717 ADDED CHECK OF EXT-ACUSQL-CONNECT-STAT-BAD AFTER
      *               RETURN FROM EACH STEP, IN CASE A PROGRAM CAUSED
      *               AN ERROR SUCH AS MISSING AN SSP, IT SHOULD "FAIL" AND
      *               CONTINUE TO NEXT STEP BUT THE ERROR IN THE CALLED
      *               PROGRAM CAUSED A DISCONNECT TO THE DATABASE. NEEDS
      *               TO RECONNECT TO THE SQL SERVER TO WRITE TO THE EOFILE
      *               STEP THAT IT FAILED. CHANGE WAS MADE IN EOINIT  S35Q-88
      *               IF ( EXT-ACUSQL-CONNECT-STAT-BAD )
      *                  PERFORM SQL-CONNECT
      *
      * JKC 2025-0217 S35Q-114 ADDED LOGIC TO UPDATE THE SQLLOG-FILE
      *          FOR ANY ERRORS THAT OCCUR WHILE PROCESSING; ADDED
      *          WS-SQLLOG WORKERS AND MISC-SQLLOG-UPDATE PARAGRAPH.
      * BAH 2025-0228 CALL TO SETENV NEEDED EXIT-PATHNAME S35Q-114
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSER.CPY".
       COPY "LIBGB/GBFSER.CPY".
       COPY "LIBGB/GBFSSQLLOG.CPY".
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDER.CPY".
       COPY "LIBGB/GBFDER.CPY".
       COPY "LIBGB/GBFDSQLLOG.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/EOCRON S35 03/06/25-08:11:25 >".

      *=================================================================
      *    F I L E - I O - W O R K E R S
      *=================================================================
       COPY "LIBGB/GB01EO.CPY".
       COPY "LIBGB/GB01EO_SQL.CPY".
       COPY "LIBGB/GBWSEO.CPY".
       COPY "LIBLP/LPWSER.CPY".
       COPY "LIBGB/GBWSER.CPY".
       COPY "LIBGB/GBWSSQLLOG.CPY".
       01  ER-FEED                     PIC 9         VALUE 1.

      *=================================================================
      *    M I S C E L L A N E O U S - W O R K E R S
      *=================================================================
       01  AUDIT-ERROR                 PIC 9(1)      VALUE ZEROES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CHMOD 77? ../EO/EOM123114, CHMOD 77? ../EO/EOM123114/* 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  CHMOD-REPORTS-COMMAND.
           03  CHMOD-CMD               PIC X(6).
           03  CHMOD-ARGS              PIC 9(3).
           03  FILLER                  PIC X(1)      VALUE " ".
           03  CHMOD-BASE              PIC X(9).
           03  FILLER                  PIC X(1)      VALUE "/".
           03  CHMOD-MACHINE           PIC X(2).
           03  FILLER                  PIC X(1)      VALUE "/".
           03  CHMOD-DATA              PIC X(6).
           03  FILLER                  PIC X(4)      VALUE "/EO/".
           03  CHMOD-PCODE             PIC X(3).
           03  CHMOD-DATE              PIC 9(6).
           03  CHMOD-STAR              PIC X(2).

       01  DATE1                       PIC 9(8).
       01  DATE2                       PIC 9(8).

       01  DATE-TIME-WORKERS.
           03  ACC-TIME                PIC 9(8).
           03  THIS REDEFINES ACC-TIME.
               05  ACC-HR              PIC 99.
               05  ACC-MN              PIC 99.
               05  ACC-SC              PIC 99.
               05  ACC-MS              PIC 99.
           03  CURR-DATE               PIC 9(8).
           03  CURR-DATEX REDEFINES CURR-DATE.
               05  CURR-CC             PIC 99.
               05  CURR-YY             PIC 99.
               05  CURR-MM             PIC 99.
               05  CURR-DD             PIC 99.
           03  CUR-TIME                PIC 9(6).
           03  THIS REDEFINES CUR-TIME.
               05  CUR-HR              PIC 99.
               05  CUR-MN              PIC 99.
               05  CUR-SC              PIC 99.

       01  FIRST-TIME                  PIC X(1).
       01  HOUR-BUF                    PIC 9(2).
       01  LOAD-STAT                   PIC X(6).
       01  MIN-BUF                     PIC 9(2).

       01  PATHNAME-WORKERS.
           03  EXIT-PATHNAME           PIC X(22).
           03  FORM-PATHNAME           PIC X(22).

       01  PCODE                       PIC X(3).
       01  SECURITY-ERROR              PIC 9(1)      VALUE ZEROES.
       01  SKIP-STEP                   PIC 9(4)      VALUE ZEROES.
       01  SKIP-SWITCH                 PIC 9(1)      VALUE ZEROES.
       01  STEP-COUNT                  PIC 9(4).

       01  STEP-WORKERS.
           03  CURRENT-STEP            PIC 9(4).
           03  DISPLAY-STEP            PIC 9(4).
           03  FIRST-STEP              PIC 9(4)      VALUE 1.
           03  LAST-STEP               PIC 9(4)      VALUE 9999.

       01  WS-PROGRAM-NAME             PIC X(6).
       01  WS-SQLLOG-ERROR-MSG         PIC X(60)     VALUE SPACES.
       01  WS-SQLLOG-PCODE             PIC X(3)      VALUE SPACES.
       01  WS-SQLLOG-STEP              PIC 9(4)      VALUE ZEROES.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/EOBUF_EXT.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

      *=================================================================
      *    C H A I N I N G - W O R K E R S
      *=================================================================
       01  PROCEDURE-CODE      PIC X(3).

       01  BEG-OFFSET.
           03  BEG-OFFSET-SIGN  PIC X.
           03  BEG-OFFSET-DAYS  PIC 9(5).
       01  FILLER               REDEFINES BEG-OFFSET.
           03  FILLER           PIC X.
           03  BEG-OFFSET-SIGN2 PIC X.
           03  BEG-OFFSET-MM    PIC 99.
           03  BEG-OFFSET-DD    PIC 99.
       01  BEG-DATE             REDEFINES BEG-OFFSET
                                PIC 9(6).
       01  END-OFFSET.
           03  END-OFFSET-SIGN  PIC X.
           03  END-OFFSET-DAYS  PIC 9(5).
       01  FILLER               REDEFINES END-OFFSET.
           03  FILLER           PIC X.
           03  END-OFFSET-SIGN2 PIC X.
           03  END-OFFSET-MM    PIC 99.
           03  END-OFFSET-DD    PIC 99.
       01  END-DATE             REDEFINES END-OFFSET
                                PIC 9(6).

       01  BEG-STEPX.
           05  BEG-STEP        PIC 9(4).

       01  END-STEPX.
           05  END-STEP        PIC 9(4).

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION CHAINING PROCEDURE-CODE
                                   BEG-OFFSET
                                   END-OFFSET
                                   BEG-STEPX
                                   END-STEPX.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               ER-FILE SQLLOG-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-EO1-FILE.
           CLOSE
               ER-FILE SQLLOG-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

      * SQL-CONNECT MUST TAKE PLACE AFTER SETENV IS CALLED FURTHER DOWN
      * DO NOT PUT ONE HERE

      D    STOP MESS.
           PERFORM TCLP-TRACE-MODULE.

           MOVE ALL "=" TO EXT-TCLP-TRACE-MESS.
           PERFORM MISC-TRACE-SEND-MESS.
           STRING "BEGIN PROCEDURE: ", PROCEDURE-CODE
                  DELIMITED BY "  " INTO EXT-TCLP-TRACE-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "/   /   /  /" TO FORM-PATH.
           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-BAD
              MOVE "ENV ERROR: REL"        TO ERRLOG-KEY
              PERFORM UPDATE-LG-LOG
              MOVE "???"                   TO WS-SQLLOG-PCODE
              MOVE ZEROES                  TO WS-SQLLOG-STEP
              MOVE ERRLOG-KEY              TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ERRLOG-KEY              TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              PERFORM MISC-TRACE-PROGRAM-FINISH
              GO TO STOP-RUN.

           MOVE GETENV-BUF TO FORM-REL.
           MOVE "CO" TO FORM-OBJ.
           MOVE "SP/EOCRON" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

           MOVE "BEFORE CALL TO SETENV" TO EXT-TCLP-TRACE-MESS.
           PERFORM MISC-TRACE-SEND-MESS.

      * NEED TO GET EXTERNAL VARIABLES SET
           MOVE GETENV-BUF TO FORM-REL.
           MOVE "CO" TO FORM-OBJ.
           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH.
           CANCEL FORM-PROGX.

           MOVE "AFTER  CALL TO SETENV" TO EXT-TCLP-TRACE-MESS.
           PERFORM MISC-TRACE-SEND-MESS.

      * THIS MUST STAY HERE! SETENV MUST BE CALLED FIRST
           MOVE "BEFORE SQL-CONNECT" TO EXT-TCLP-TRACE-MESS.
           PERFORM MISC-TRACE-SEND-MESS.

           PERFORM SQL-CONNECT.

           MOVE "AFTER  SQL-CONNECT" TO EXT-TCLP-TRACE-MESS.
           PERFORM MISC-TRACE-SEND-MESS.

           MOVE EXT-FILPATH-BASE TO ERRLOG-BASE.
           MOVE EXT-FILPATH-MACHINE TO ERRLOG-MACHINE.

           MOVE "SP/EOCRON" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

           IF NOT (BEG-OFFSET-MM NUMERIC AND BEG-OFFSET-DD NUMERIC)
              MOVE "INVALID BEGINNING OFFSET" TO ERRLOG-KEY
              PERFORM UPDATE-LG-LOG
              MOVE "???"                      TO WS-SQLLOG-PCODE
              MOVE ZEROES                     TO WS-SQLLOG-STEP
              MOVE ERRLOG-KEY                 TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ERRLOG-KEY                 TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              PERFORM MISC-TRACE-PROGRAM-FINISH
              GO TO STOP-RUN.

           IF NOT (END-OFFSET-MM NUMERIC AND END-OFFSET-DD NUMERIC)
              MOVE "INVALID ENDING OFFSET" TO ERRLOG-KEY
              PERFORM UPDATE-LG-LOG
              MOVE "???"                   TO WS-SQLLOG-PCODE
              MOVE ZEROES                  TO WS-SQLLOG-STEP
              MOVE ERRLOG-KEY              TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ERRLOG-KEY              TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              PERFORM MISC-TRACE-PROGRAM-FINISH
              GO TO STOP-RUN.

           IF BEG-STEPX NOT NUMERIC
              MOVE "INVALID BEGINNING STEP" TO ERRLOG-KEY
              PERFORM UPDATE-LG-LOG
              MOVE "???"                    TO WS-SQLLOG-PCODE
              MOVE ZEROES                   TO WS-SQLLOG-STEP
              MOVE ERRLOG-KEY               TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ERRLOG-KEY               TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              PERFORM MISC-TRACE-PROGRAM-FINISH
              GO TO STOP-RUN.

           IF END-STEPX NOT NUMERIC
              MOVE "INVALID ENDING STEP"   TO ERRLOG-KEY
              PERFORM UPDATE-LG-LOG
              MOVE "???"                   TO WS-SQLLOG-PCODE
              MOVE ZEROES                  TO WS-SQLLOG-STEP
              MOVE ERRLOG-KEY              TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ERRLOG-KEY              TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              PERFORM MISC-TRACE-PROGRAM-FINISH
              GO TO STOP-RUN.

           MOVE BEG-STEP TO FIRST-STEP.
           MOVE END-STEP TO LAST-STEP.

           MOVE PROCEDURE-CODE TO PCODE EO-SPOOL-PCODE EO-PCODE.

           MOVE ZERO TO EO-STEP.
           PERFORM OPEN-EO1-FILE.
           PERFORM READ-EO1-FILE.
           PERFORM CLOSE-EO1-FILE.

           IF IO-FG = 9
              MOVE "INVALID CODE"          TO ERRLOG-KEY
              PERFORM UPDATE-LG-LOG
              MOVE "???"                   TO WS-SQLLOG-PCODE
              MOVE ZEROES                  TO WS-SQLLOG-STEP
              MOVE ERRLOG-KEY              TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ERRLOG-KEY              TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              PERFORM MISC-TRACE-PROGRAM-FINISH
              GO TO STOP-RUN.

           MOVE 99 TO MIN-BUF HOUR-BUF.

      *-----------------------------------------------------------------
       GET-DATE-RANGE.

           PERFORM GET-DATE-TIME.

           IF BEG-OFFSET-SIGN = "M"
              IF (BEG-OFFSET-SIGN2 = "+" OR BEG-OFFSET-SIGN2 = "-")
                 IF BEG-OFFSET-DD = 0
                    MOVE CURR-DD TO BEG-OFFSET-DD
                 END-IF
                 MOVE CURR-DATE TO NDTE-DATE
                 IF BEG-OFFSET-SIGN2 = "-"
                    COMPUTE NDTE-HOLD = BEG-OFFSET-MM * -1
                 ELSE
                    MOVE BEG-OFFSET-MM TO NDTE-HOLD
                 END-IF
                 PERFORM INCREMENT-MONTHS
                 MOVE BEG-OFFSET-DD TO NDTE-DD
                 PERFORM NEW-DATE-CALCULATION
                 MOVE NDTE-DATE TO DATE1
                 GO TO GET-ENDING-DATE.

           IF BEG-OFFSET-SIGN = "M"
              MOVE CURR-DATE TO NDTE-DATE
              MOVE BEG-OFFSET-DAYS TO NDTE-DD
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO DATE1
              GO TO GET-ENDING-DATE.

           IF NOT (BEG-OFFSET-SIGN = "-" OR BEG-OFFSET-SIGN = "+")
              IF BEG-DATE NUMERIC
                 MOVE BEG-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "INVALID BEGINNING DATE" TO ERRLOG-KEY
                    PERFORM UPDATE-LG-LOG
                    MOVE "???"                    TO WS-SQLLOG-PCODE
                    MOVE ZEROES                   TO WS-SQLLOG-STEP
                    MOVE ERRLOG-KEY               TO WS-SQLLOG-ERROR-MSG
                    PERFORM MISC-SQLLOG-UPDATE
                    MOVE ERRLOG-KEY               TO EXT-TCLP-TRACE-MESS
                    PERFORM MISC-TRACE-SEND-MESS
                    PERFORM MISC-TRACE-PROGRAM-FINISH
                    GO TO STOP-RUN
                 ELSE
                    MOVE NUM-DATE TO DATE1
                    GO TO GET-ENDING-DATE
              ELSE
                 MOVE "NON-NUMERIC BEGINNING DATE" TO ERRLOG-KEY
                 PERFORM UPDATE-LG-LOG
                 MOVE "???"                      TO WS-SQLLOG-PCODE
                 MOVE ZEROES                     TO WS-SQLLOG-STEP
                 MOVE ERRLOG-KEY                 TO WS-SQLLOG-ERROR-MSG
                 PERFORM MISC-SQLLOG-UPDATE
                 MOVE ERRLOG-KEY                 TO EXT-TCLP-TRACE-MESS
                 PERFORM MISC-TRACE-SEND-MESS
                 PERFORM MISC-TRACE-PROGRAM-FINISH
                 GO TO STOP-RUN.

           IF BEG-OFFSET-SIGN = "-"
              COMPUTE NDTE-HOLD = BEG-OFFSET-DAYS * -1
           ELSE
              MOVE BEG-OFFSET-DAYS TO NDTE-HOLD.

           MOVE CURR-DATE TO NDTE-DATE.
           MOVE "D" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO DATE1.

      *-----------------------------------------------------------------
       GET-ENDING-DATE.

           IF END-OFFSET-SIGN = "M"
              IF (END-OFFSET-SIGN2 = "+" OR END-OFFSET-SIGN2 = "-")
                 IF END-OFFSET-DD = 0
                    MOVE CURR-DD TO END-OFFSET-DD
                 END-IF
                 MOVE CURR-DATE TO NDTE-DATE
                 IF END-OFFSET-SIGN2 = "-"
                    COMPUTE NDTE-HOLD = END-OFFSET-MM * -1
                 ELSE
                    MOVE END-OFFSET-MM TO NDTE-HOLD
                 END-IF
                 PERFORM INCREMENT-MONTHS
                 MOVE END-OFFSET-DD TO NDTE-DD
                 PERFORM NEW-DATE-CALCULATION
                 MOVE NDTE-DATE TO DATE2
                 GO TO CHECK-DATE-RANGE.

           IF END-OFFSET-SIGN = "M"
              MOVE CURR-DATE TO NDTE-DATE
              MOVE END-OFFSET-DAYS TO NDTE-DD
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO DATE2
              GO TO CHECK-DATE-RANGE.

           IF NOT (END-OFFSET-SIGN = "-" OR END-OFFSET-SIGN = "+")
              IF END-DATE NUMERIC
                 MOVE END-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "INVALID ENDING DATE" TO ERRLOG-KEY
                    PERFORM UPDATE-LG-LOG
                    MOVE "???"                 TO WS-SQLLOG-PCODE
                    MOVE ZEROES                TO WS-SQLLOG-STEP
                    MOVE ERRLOG-KEY            TO WS-SQLLOG-ERROR-MSG
                    PERFORM MISC-SQLLOG-UPDATE
                    MOVE ERRLOG-KEY            TO EXT-TCLP-TRACE-MESS
                    PERFORM MISC-TRACE-SEND-MESS
                    PERFORM MISC-TRACE-PROGRAM-FINISH
                    GO TO STOP-RUN
                 ELSE
                    MOVE NUM-DATE TO DATE2
                    GO TO CHECK-DATE-RANGE
              ELSE
                 MOVE "NON-NUMERIC ENDING DATE" TO ERRLOG-KEY
                 PERFORM UPDATE-LG-LOG
                 MOVE "???"                     TO WS-SQLLOG-PCODE
                 MOVE ZEROES                    TO WS-SQLLOG-STEP
                 MOVE ERRLOG-KEY                TO WS-SQLLOG-ERROR-MSG
                 PERFORM MISC-SQLLOG-UPDATE
                 MOVE ERRLOG-KEY                TO EXT-TCLP-TRACE-MESS
                 PERFORM MISC-TRACE-SEND-MESS
                 PERFORM MISC-TRACE-PROGRAM-FINISH
                 GO TO STOP-RUN.

           IF END-OFFSET-SIGN = "-"
              COMPUTE NDTE-HOLD = END-OFFSET-DAYS * -1
           ELSE
              MOVE END-OFFSET-DAYS TO NDTE-HOLD.

           MOVE CURR-DATE TO NDTE-DATE.
           MOVE "D" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO DATE2.

      *-----------------------------------------------------------------
       CHECK-DATE-RANGE.

           IF DATE2 < DATE1
              MOVE "INVALID DATE RANGE" TO ERRLOG-KEY
              PERFORM UPDATE-LG-LOG
              MOVE "???"                TO WS-SQLLOG-PCODE
              MOVE ZEROES               TO WS-SQLLOG-STEP
              MOVE ERRLOG-KEY           TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ERRLOG-KEY           TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              PERFORM MISC-TRACE-PROGRAM-FINISH
              GO TO STOP-RUN.

      *-----------------------------------------------------------------
       CREATE-SPOOL-DIR.

           MOVE DATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO EO-SPOOL-DATE.
           MOVE " " TO EO-SPOOL-FILE.
           PERFORM LOAD-EO-SPOOL.
           MOVE EO-SPOOL TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.

           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING EO-SPOOL GIVING STAT-99
              IF STAT NOT = "00"
                 MOVE "CANNOT CREATE SPOOL DIRECTORY" TO ERRLOG-KEY
                 PERFORM UPDATE-LG-LOG
                 MOVE "???"                      TO WS-SQLLOG-PCODE
                 MOVE ZEROES                     TO WS-SQLLOG-STEP
                 MOVE ERRLOG-KEY                 TO WS-SQLLOG-ERROR-MSG
                 PERFORM MISC-SQLLOG-UPDATE
                 MOVE ERRLOG-KEY                 TO EXT-TCLP-TRACE-MESS
                 PERFORM MISC-TRACE-SEND-MESS
                 PERFORM MISC-TRACE-PROGRAM-FINISH
                 GO TO STOP-RUN.

      *-----------------------------------------------------------------
       BEGUPD.

           MOVE "Y" TO STAT.

      *-----------------------------------------------------------------
       START-AUDIT.

           MOVE 0 TO AUDIT-ERROR STEP-COUNT SECURITY-ERROR
                     DISPLAY-STEP.

           PERFORM CLEAR-EO-FILE.

           MOVE "60" TO EXT-ROUTE-BUF.
           MOVE SPACES TO LASTNEXT-BUF.
           MOVE FIRST-STEP TO CURRENT-STEP.
           MOVE "Y" TO FIRST-TIME.

           MOVE SPACES TO EXT-EOBUF-ERRCD.

      *=================================================================
       NEXT-AUDIT.

           PERFORM GET-NEXT-REPORT-ENTRY.

           IF IO-FG = 9
              GO TO END-AUDIT.

           ADD 1 TO STEP-COUNT.

           PERFORM PROGRAM-LOAD.

           PERFORM UPDATE-REPORT-ENTRY.

           IF EO-COMPLETE-STAT = "READY"
              GO TO NEXT-AUDIT.

           IF EO-COMPLETE-STAT = "NOTFND"
              MOVE SPACES TO ER-MESSAGE
              STRING "PROGRAM: " WS-PROGRAM-NAME " IS MISSING"
                     DELIMITED BY SIZE INTO ER-MESSAGE
              MOVE PCODE                   TO WS-SQLLOG-PCODE
              MOVE CURRENT-STEP            TO WS-SQLLOG-STEP
              MOVE ER-MESSAGE              TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ER-MESSAGE              TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
           ELSE
           IF EO-COMPLETE-STAT = "FORMNF"
              MOVE "PROGRAM FORM MISSING" TO ER-MESSAGE
              MOVE PCODE                   TO WS-SQLLOG-PCODE
              MOVE CURRENT-STEP            TO WS-SQLLOG-STEP
              MOVE ER-MESSAGE              TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ER-MESSAGE              TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
           ELSE
           IF EO-COMPLETE-STAT = "SECVIZ"
              MOVE "CANNOT BE RUN--MEMORY OVERFLOW" TO ER-MESSAGE
              MOVE PCODE                   TO WS-SQLLOG-PCODE
              MOVE CURRENT-STEP            TO WS-SQLLOG-STEP
              MOVE ER-MESSAGE              TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ER-MESSAGE              TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
           ELSE
           IF EO-COMPLETE-STAT = "SECVIO"
              MOVE "CANNOT BE RUN--OPTION NOT PURCHASED" TO ER-MESSAGE
              MOVE 1 TO SECURITY-ERROR
              MOVE PCODE                   TO WS-SQLLOG-PCODE
              MOVE CURRENT-STEP            TO WS-SQLLOG-STEP
              MOVE ER-MESSAGE              TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ER-MESSAGE              TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
           ELSE
           IF EO-COMPLETE-STAT = "SECVIS"
              MOVE "CANNOT BE RUN--SECURITY VIOLATION" TO ER-MESSAGE
              MOVE 1 TO SECURITY-ERROR
              MOVE PCODE                   TO WS-SQLLOG-PCODE
              MOVE CURRENT-STEP            TO WS-SQLLOG-STEP
              MOVE ER-MESSAGE              TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ER-MESSAGE              TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
           ELSE
           IF EO-COMPLETE-STAT = "MEMOVF"
              MOVE "MEMORY OVERFLOW OCCURRED" TO ER-MESSAGE
              MOVE PCODE                   TO WS-SQLLOG-PCODE
              MOVE CURRENT-STEP            TO WS-SQLLOG-STEP
              MOVE ER-MESSAGE              TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ER-MESSAGE              TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
           ELSE
           IF EO-COMPLETE-STAT = "D.VERS"
              MOVE "NEW VERSION--RANGES MUST BE RE-ENTERED"
                                         TO ER-MESSAGE
              MOVE PCODE                   TO WS-SQLLOG-PCODE
              MOVE CURRENT-STEP            TO WS-SQLLOG-STEP
              MOVE ER-MESSAGE              TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ER-MESSAGE              TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
           ELSE
              MOVE SPACES TO ER-MESSAGE
              STRING "EXECUTION ERROR OCCURRED (" EO-COMPLETE-STAT
                        ")" DELIMITED BY SIZE INTO ER-MESSAGE
              MOVE PCODE                   TO WS-SQLLOG-PCODE
              MOVE CURRENT-STEP            TO WS-SQLLOG-STEP
              MOVE ER-MESSAGE              TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              MOVE ER-MESSAGE              TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS.

           PERFORM UPDATE-LOG.
           MOVE 1 TO AUDIT-ERROR.

      *-----------------------------------------------------------------
       AUDIT-PAUSE.

           IF SECURITY-ERROR = 0
              GO TO NEXT-AUDIT.

      *-----------------------------------------------------------------
       END-AUDIT.

           IF SECURITY-ERROR = 1
              MOVE "INVALID AUTHORIZATION TO RUN THIS PROCEDURE"
                 TO ER-MESSAGE
              PERFORM UPDATE-LOG
              MOVE ER-MESSAGE TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              GO TO END-PROGRAM.

           IF AUDIT-ERROR = 1
              MOVE "CANNOT RUN UNTIL ERRORS ARE CORRECTED" TO ER-MESSAGE
              PERFORM UPDATE-LOG
              MOVE ER-MESSAGE TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              GO TO END-PROGRAM.

           IF STEP-COUNT = ZERO
              MOVE "INVALID RANGE" TO ER-MESSAGE
              PERFORM UPDATE-LOG
              MOVE ER-MESSAGE TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              GO TO END-PROGRAM.

      *-----------------------------------------------------------------
       START-PROCESS.

           PERFORM CLEAR-EO-FILE.

           MOVE "70" TO EXT-ROUTE-BUF.
           MOVE SPACES TO LASTNEXT-BUF.
           MOVE FIRST-STEP TO CURRENT-STEP.
           MOVE "Y" TO FIRST-TIME.
           MOVE SPACES TO EXT-EOBUF-ERRCD.
           MOVE 0 TO SKIP-SWITCH SKIP-STEP.

      *=================================================================
       NEXT-PROCESS.

           PERFORM GET-NEXT-REPORT-ENTRY.

           IF IO-FG = 9
              GO TO END-PROCESS.

           IF SKIP-SWITCH = 2
              IF EO-STEP NOT < SKIP-STEP
                 MOVE ZERO TO SKIP-SWITCH SKIP-STEP.

           IF SKIP-SWITCH = 0
              PERFORM PROGRAM-LOAD.

           PERFORM UPDATE-REPORT-ENTRY.

           IF EXT-EOBUF-ERRCD = "I"
              PERFORM MISC-TRACE-PROGRAM-FINISH
              GO TO EXIT-PROG.

           GO TO NEXT-PROCESS.

      *=================================================================
       END-PROCESS.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    C L E A R - E O - F I L E
      *
      ******************************************************************
       CLEAR-EO-FILE SECTION.

           MOVE PCODE TO EO-PCODE.
           MOVE FIRST-STEP TO EO-STEP.
           PERFORM OPEN-EO1-FILE.
           PERFORM START-EO1-FILE.

      *=================================================================
       CLEAR-NEXT-EO.

           PERFORM READ-EO1-FILE-NEXT.

           IF IO-FG = 9
              GO TO CLEAR-EO-EXIT.

           IF EO-PCODE NOT = PCODE
              GO TO CLEAR-EO-EXIT.

           IF EO-STEP > LAST-STEP
              GO TO CLEAR-EO-EXIT.

           MOVE ZERO TO EO-START-TIME EO-COMPLETE-TIME
                        EO-START-DATE EO-COMPLETE-DATE.
           MOVE SPACES TO EO-COMPLETE-STAT EO-MESS.
           PERFORM REWRITE-EO1-FILE.

           GO TO CLEAR-NEXT-EO.

      *=================================================================
       CLEAR-EO-EXIT.
           PERFORM CLOSE-EO1-FILE.

      ******************************************************************
      *
      *    G E T - N E X T - R E P O R T - E N T R Y
      *
      ******************************************************************
       GET-NEXT-REPORT-ENTRY SECTION.

           PERFORM OPEN-EO1-FILE.

      *=================================================================
       GET-NEXT-REPORT2.

           MOVE PCODE TO EO-PCODE.
           MOVE CURRENT-STEP TO EO-STEP.

           IF FIRST-TIME = "Y"
              PERFORM START-EO1-FILE
              MOVE "N" TO FIRST-TIME
           ELSE
              PERFORM START-EO1-FILE-GREATER.

           IF IO-FG = 9
              GO TO GET-NEXT-EXIT.

           PERFORM READ-EO1-FILE-NEXT.

           IF IO-FG = 9
              GO TO GET-NEXT-EXIT.

           IF EO-PCODE NOT = PCODE OR EO-STEP > LAST-STEP
              MOVE 9 TO IO-FG
              GO TO GET-NEXT-EXIT.

           MOVE EO-STEP TO CURRENT-STEP.

           IF EXT-ROUTE-BUF = "60"
              GO TO GET-NEXT-EXIT.

           PERFORM GET-DATE-TIME.
           MOVE CURR-DATE TO EO-START-DATE.
           MOVE CUR-TIME TO EO-START-TIME.
           PERFORM REWRITE-EO1-FILE.

      *=================================================================
       GET-NEXT-EXIT.

           MOVE EO-PCODE                   TO EXT-EOBUF-PCODE.
           MOVE SPACES                     TO EXT-EOBUF-MESS.
           MOVE EO-PARAMETERS              TO EXT-EOBUF-DATA.
           MOVE EO-COPIES                  TO EXT-EOBUF-COPIES.
           MOVE DATE1                      TO EXT-EOBUF-DATE1.
           MOVE DATE2                      TO EXT-EOBUF-DATE2.
           MOVE "/"                        TO EO-SPOOL-FILE.
           MOVE EO-PROGRAM                 TO EO-SPOOL-NAME.
           MOVE EO-STEP                    TO EO-SPOOL-STEP.

           IF EO-DIR = "PY"
              MOVE "_P" TO EO-SPOOL-SUF
           ELSE
              MOVE "  " TO EO-SPOOL-SUF.

           PERFORM LOAD-EO-SPOOL.
           MOVE EO-SPOOL TO EXT-EOBUF-PRPATH.
           MOVE "~"      TO EXT-EOBUF-ERRCD.

      * PRINT THE COMMENT IN THE H-TITLE FOR CPCORP
           IF EXT-ROUTE-BUF = "70"
              MOVE EO-COMMENT TO EXT-EOBUF-MESS.

           MOVE EO-PROGRAM TO WS-PROGRAM-NAME FORM-PROGX.
           PERFORM CLOSE-EO1-FILE.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - E N T R Y
      *
      ******************************************************************
       UPDATE-REPORT-ENTRY SECTION.

           PERFORM OPEN-EO1-FILE.

           MOVE PCODE TO EO-PCODE.
           MOVE CURRENT-STEP TO EO-STEP.
           PERFORM READ-EO1-FILE.

           IF IO-FG = 9
              MOVE "ERROR: READ-EO1-FILE" TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              GO TO IO-ERROR.

           MOVE EXT-EOBUF-MESS TO EO-MESS.

           IF LOAD-STAT = "NOTFND"
              MOVE SPACES TO EO-MESS
              STRING "PROGRAM: " WS-PROGRAM-NAME " IS MISSING"
                     DELIMITED BY SIZE INTO EO-MESS.

           IF SKIP-SWITCH = 2
              MOVE "SKIP'D" TO EO-COMPLETE-STAT
           ELSE
              MOVE 1 TO SKIP-SWITCH
              IF LOAD-STAT NOT = SPACES
                 MOVE LOAD-STAT TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "!"
                 MOVE "FATAL!" TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "V"
                 MOVE "D.VERS" TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "|"
                 MOVE "SECVIS" TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "S"
                 MOVE SPACES TO EO-COMPLETE-STAT
                 STRING "ERR :" EXT-EOBUF-ERRCD
                    DELIMITED BY SIZE INTO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "I"
                 MOVE "CANCEL" TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD NOT = " "
                 MOVE SPACES TO EO-COMPLETE-STAT
                 STRING "FAIL:" EXT-EOBUF-ERRCD
                    DELIMITED BY SIZE INTO EO-COMPLETE-STAT
              ELSE
                 MOVE 0 TO SKIP-SWITCH
                 MOVE "RUN OK" TO EO-COMPLETE-STAT.

              STRING "EO-MESS = ", EO-MESS
                     DELIMITED BY "  " INTO EXT-TCLP-TRACE-MESS.
              PERFORM MISC-TRACE-SEND-MESS.
              STRING "EO-COMPLETE-STAT = ", EO-COMPLETE-STAT
                     DELIMITED BY "  " INTO EXT-TCLP-TRACE-MESS.
              PERFORM MISC-TRACE-SEND-MESS.

           IF SKIP-SWITCH = 1
              MOVE 0 TO SKIP-SWITCH.

           IF EXT-ROUTE-BUF = "60"
              IF EO-COMPLETE-STAT = "RUN OK"
                 MOVE EXT-EOBUF-DATA TO EO-PARAMETERS
                 MOVE "READY" TO EO-COMPLETE-STAT
                 GO TO UPDATE-ENTRY-EXIT
              ELSE
                 GO TO UPDATE-ENTRY-EXIT.

           PERFORM GET-DATE-TIME.
           MOVE CUR-TIME TO EO-COMPLETE-TIME.
           MOVE CURR-DATE TO EO-COMPLETE-DATE.

      *=================================================================
       UPDATE-ENTRY-EXIT.

           IF EO-MESS = SPACES GO TO UPDATE-MESS-SKIP.

      *-----------------------------------------------------------------
       UPDATE-MESS-SKIP.

           PERFORM REWRITE-EO1-FILE.

           IF IO-FG = 9
              MOVE "ERROR: REWRITE-EO1-FILE" TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              MOVE PCODE                     TO WS-SQLLOG-PCODE
              MOVE CURRENT-STEP              TO WS-SQLLOG-STEP
              MOVE EXT-TCLP-TRACE-MESS       TO WS-SQLLOG-ERROR-MSG
              PERFORM MISC-SQLLOG-UPDATE
              PERFORM MISC-TRACE-SEND-MESS
              GO TO IO-ERROR.

           PERFORM CLOSE-EO1-FILE.

      ******************************************************************
      *
      *    P R O G R A M - L O A D
      *
      ******************************************************************
       PROGRAM-LOAD SECTION.
       PGM-LOAD.

           MOVE SPACES TO NEXT-PROGRAM.
           MOVE SPACES TO LOAD-STAT.

           IF LOADCO-OP1 = " " OR LOADCO-OP1 = "1"
              GO TO LOAD-IT.

           MOVE FORM-NAM TO LOADCO-ARG.
           MOVE LOADCO-OPT TO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99.

       LOAD-IT.
           MOVE "CO" TO FORM-OBJ.

      *    PERFORM LOGUID-CALL.

           STRING "BEFORE CALL TO (FORM-PROGX): ", FORM-PROGX
                  DELIMITED BY SIZE INTO EXT-TCLP-TRACE-MESS.
           PERFORM MISC-TRACE-SEND-MESS.

           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME ON OVERFLOW
              MOVE "OVERFLOW ON CALL FORM-PROGX" TO EXT-TCLP-TRACE-MESS
              PERFORM MISC-TRACE-SEND-MESS
              MOVE "MEMOVF" TO LOAD-STAT
              GO TO LOAD-POST.

           CANCEL FORM-PROGX.

           STRING "AFTER  CALL TO (FORM-PROGX): ", FORM-PROGX
                  DELIMITED BY SIZE INTO EXT-TCLP-TRACE-MESS.
           PERFORM MISC-TRACE-SEND-MESS.

           IF FORM-OBJ = " "
              MOVE "FORMNF" TO LOAD-STAT
              GO TO LOAD-POST.

      * SECURITY
           IF FORM-OBJ = "3Z"
              MOVE "SECVIZ" TO LOAD-STAT
              GO TO LOAD-POST.

           IF FORM-OBJ = "3O"
              MOVE "SECVIO" TO LOAD-STAT
              GO TO LOAD-POST.

           IF FORM-OBJ = "3S"
              MOVE "SECVIS" TO LOAD-STAT
              GO TO LOAD-POST.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NEED TO RECONNECT TO THE SQL SERVER IF THE CALLING PROGRAM
      * DISCONNECTS DUE TO AN SQL ERROR; IF YOU CANNOT RECONNECT THEN
      * NEED TO STOP.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ACUSQL-CONNECT-STAT-BAD )
              MOVE "SQL DISCONNECT" TO EXT-EOBUF-MESS
              PERFORM SQL-CONNECT
              IF ( EXT-ACUSQL-CONNECT-STAT-BAD )
                 MOVE "E" TO EXT-EOBUF-ERRCD.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%% WHERE EXACTLY SHOULD I GO??????
      *%%% HOW DO WE NOTIFY THAT THERE WAS AN ERROR AND THAT THE 
      *%%% CURRENT STEP & REMAINING STEPS WERE NOT COMPLETED DUE TO
      *%%% NOT BEING ABLE TO RECONNECT TO THE SQL SERVER??????
      *%%%
      *%%% THERE IS A READ-EO1-FILE AND REWRITE-EO1-FILE WHICH
      *%%% USE 'GO TO IO-ERROR' WHICH USE A SEND-MESS.
      *%%% THE SEND-MESS IN LIBGB/DECLRP USE IF ROUTE-BUF = "60" OR
      *%%% "70" WHICH I BELIEVE IS "00" WITHIN HERE SO IT WILL
      *%%% STOP EXECUTION WAITING ON USER ENTRY.  WE SHOULD NOT GO
      *%%% TO IO-ERROR BUT WHERE SHOULD IT GO AND HOW TO FLAG THERE
      *%%% WAS AN EXECTION ERROR?????
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
       LOAD-POST.

           STRING "FORM-PROGX = ", FORM-PROGX
                  DELIMITED BY "  " INTO EXT-TCLP-TRACE-MESS.

           PERFORM MISC-TRACE-SEND-MESS.

           STRING "LOAD-STAT = ", LOAD-STAT
                  DELIMITED BY "  " INTO EXT-TCLP-TRACE-MESS.

           PERFORM MISC-TRACE-SEND-MESS.

           IF EXT-EOBUF-ERRCD NOT = " " OR LOAD-STAT NOT = SPACES
              MOVE SPACES TO LAST-PROGRAM NEXT-PROGRAM
           ELSE
              MOVE WS-PROGRAM-NAME TO LAST-PROGRAM
              IF NEXT-PROGRAM NOT = SPACES
                 MOVE NEXT-PROGRAM TO FORM-NAM WS-PROGRAM-NAME
                 GO TO PGM-LOAD.

      ******************************************************************
      *
      *    U P D A T E - L O G
      *
      ******************************************************************
       UPDATE-LOG SECTION.

           MOVE EO-SPOOL-PCODE  TO ER-ALL-SPOOL-PCODE.
           MOVE EO-SPOOL-DATE   TO ER-ALL-SPOOL-DATE.
           PERFORM LOAD-ER-FILE.
           MOVE ER-PATH         TO ACCESS-BUF.
           PERFORM ACCESS-CALL.

           IF STAT NOT = "00"
              MOVE "ER" TO E-FILE
              OPEN OUTPUT ER-FILE
              CLOSE ER-FILE.

           MOVE E-FILE TO E-FILEX.
           PERFORM OPEN-ER-FILE.
           PERFORM GET-DATE-TIME.
           MOVE CURR-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ER-DATE.
           MOVE CUR-TIME TO ER-TIME.
           INSPECT ER-TIME REPLACING ALL "/" BY ":".
           MOVE EXT-CONAME-USER-ID TO ER-USER.
           MOVE EO-DIR TO ER-DIR.
           MOVE "/" TO ER-SLASH.
           MOVE EO-PROGRAM TO ER-PROGRAM.
           PERFORM WRITE-ER-REC.
           PERFORM CLOSE-ER-FILE.

      ******************************************************************
      *
      *    U P D A T E - L G - L O G
      *
      ******************************************************************
       UPDATE-LG-LOG SECTION.

           PERFORM GET-DATE-TIME.
           MOVE "ERR" TO ERRLOG-LOG-TYPE.
           MOVE CURR-YY TO ERRLOG-YY.
           MOVE CURR-MM TO ERRLOG-MM.
           MOVE CURR-DD TO ERRLOG-DD.
           PERFORM LOAD-ERRLOG-FILE.
           MOVE ERRLOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.

           IF STAT NOT = "00"
              PERFORM OPEN-ERRLOG-FILE-OUTPUT
              PERFORM CLOSE-ERRLOG-FILE.

           PERFORM OPEN-ERRLOG-FILE-EXTEND.
           MOVE CURR-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD TO ERRLOG-DATE.
           MOVE CUR-TIME TO ERRLOG-TIME.
           INSPECT ERRLOG-TIME REPLACING ALL "/" BY ":".
           MOVE EXT-CONAME-USER-ID TO ERRLOG-USERID.
           MOVE "SP/EOCRON" TO ERRLOG-PROG.
           MOVE PROCEDURE-CODE TO ERRLOG-EOCRON-ID.
           PERFORM WRITE-ERRLOG-FILE.
           PERFORM CLOSE-ERRLOG-FILE.

      ******************************************************************
      *
      *    T C L P - T R A C E - M O D U L E 
      *
      *=================================================================
      * IN  : FORM-REL
      * OUT : EXT-TCLP-TRACE-WORKERS
      * DESC: TCLP/OTIS TRACE LOGGING.  CHECK IF `TCLP_TRACE`
      *       ENVIRONMENT VARIABLE IS "Y" (ENABLE TRACE WITHOUT BRANCH)
      *       OR IS "B" (ENABLE TRACE WITH READ OF BRANCH FILE; BRFILE).
      *       IF TRACE IS ENABLE THEN SEND TO LOG THE LAUNCH OF OTIS.
      ******************************************************************
       TCLP-TRACE-MODULE SECTION.

           MOVE ZEROES TO EXT-TCLP-TRACE-LEVEL.
           MOVE SPACES TO EXT-TCLP-TRACE-FORM-PATH
                          EXT-TCLP-TRACE-MESS.

      * SKIP EVENT

           MOVE "N"    TO EXT-TCLP-TRACE-SKIP-YN. 

           MOVE "TCLP_TRACE"  TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF ( STAT-BAD )
      * DISABLE TRACE
              MOVE "N" TO EXT-TCLP-TRACE
              GO TO TCLP-TRACE-MODULE-EXIT.

           MOVE GETENV-BUF TO EXT-TCLP-TRACE.

           IF ( NOT EXT-TCLP-TRACE-VALID   ) OR
              (     EXT-TCLP-TRACE-DISABLE )
      * DISABLE TRACE
              MOVE "N" TO EXT-TCLP-TRACE 
              GO TO TCLP-TRACE-MODULE-EXIT.

      * GET USERID
      *KC  MOVE "ROOT" TO MESS.
      *KC  CALL "C$TOLOWER" USING MESS, VALUE 10.
      *KC  PERFORM UNAME-CALL.

      * DISABLE FOR ROOT
      *KC  IF ( UNAME-BUF = MESS )
      *KC     MOVE "N" TO EXT-TCLP-TRACE
      *KC     GO TO TCLP-TRACE-MODULE-EXIT.

           MOVE ALL "="                    TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "TCLP"                     TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

           STRING "OTIS SESSION STARTED /", FORM-REL
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "TCLP"                     TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

      *=================================================================
       TCLP-TRACE-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S 
      *
      ******************************************************************
       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GBFRSQLLOG.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      *=================================================================
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
      *COPY "LIBGB/LOGUID.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *=================================================================
       GET-DATE-TIME.
           ACCEPT CURR-DATE FROM CENTURY-DATE.
           ACCEPT ACC-TIME FROM TIME.
           MOVE ACC-HR TO CUR-HR.
           MOVE ACC-MN TO CUR-MN.
           MOVE ACC-SC TO CUR-SC.

      *-----------------------------------------------------------------
       MISC-SQLLOG-UPDATE.
           MOVE SPACES TO MESS.

           STRING WS-SQLLOG-PCODE, " ",
                  WS-SQLLOG-STEP,  " ",
                  "####"
                  DELIMITED BY "  " INTO MESS.

           MOVE FORM-PATHNAME              TO SQLLOG-PROGRAM.
           MOVE MESS                       TO SQLLOG-RANGES.
           MOVE "FAIL"                     TO SQLLOG-STATUS.
           MOVE ERRLOG-KEY                 TO SQLLOG-MESS.
           PERFORM UPDATE-SQLLOG-FILE.

      *-----------------------------------------------------------------
       MISC-TRACE-PROGRAM-FINISH.
           MOVE "PROGRAM FINISH (MANUAL CODE)" TO EXT-TCLP-TRACE-MESS.
           PERFORM MISC-TRACE-SEND-MESS.

      *-----------------------------------------------------------------
       MISC-TRACE-SEND-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "EOCRON"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBEOGS_SQL.CPY".
       COPY "LIBGB/EOSPOOLIO.CPY".
       COPY "LIBGB/GBEO1IN.CPY".
       COPY "LIBGB/GBERI.CPY".
       COPY "LIBGB/GBSQLLOGIN.CPY".
       COPY "LIBLP/LPPRINT.CPY".

      *=================================================================
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.

      * CHMOD 77? ../EO/EOM123114, CHMOD 77? ../EO/EOM123114/* 

           MOVE "CHMOD"             TO CHMOD-CMD.
           MOVE 777                 TO CHMOD-ARGS.
           MOVE EXT-FILPATH-BASE    TO CHMOD-BASE.
           MOVE EXT-FILPATH-MACHINE TO CHMOD-MACHINE.
           MOVE EXT-FILPATH-DATA    TO CHMOD-DATA.
           CALL "C$TOLOWER" USING CHMOD-REPORTS-COMMAND, VALUE 29.
           MOVE PCODE       TO CHMOD-PCODE.
           MOVE DATE2       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO CHMOD-DATE.
           MOVE SPACES      TO CHMOD-STAR.
           MOVE CHMOD-REPORTS-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           MOVE "/*"       TO CHMOD-STAR.
           MOVE CHMOD-REPORTS-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           PERFORM MISC-TRACE-PROGRAM-FINISH.

      *=================================================================
       ENDIT.
           EXIT.
       EXIT-PROG.
           EXIT.
       STOP-RUN.
           EXIT PROGRAM.
