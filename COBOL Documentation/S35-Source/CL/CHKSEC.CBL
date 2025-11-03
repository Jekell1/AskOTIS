       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CHKSEC.
      ******************************************************************
      *
      *  CL/CHKSEC   COMPANY OPTION & USER SECURITY CHECK
      *
      *=================================================================
      * REV:
      * BLM 101201 IF INVALID READ ON SEFILE WAS MOVING "XX" TO STAT &
      *            EXITING, BAD STAT MEANT YOU  COULD NOT RUN.  CHANGED
      *            TO JUST EXIT SO YOU CAN RUN IF NO REC IN SEFILE.
      * BLM 130219 MOVED THIS PROGRAM FROM GB TO CL
      * BLM 130329 REMOVE CMD CALL TO C DATE ROUTINE.  REPLACED WITH
      *            JUST THE PARTS OF DATER NEEDED TO COMPARE 2 DATES.
      *            DID NOT WANT TO COPY ENTIRE DATER SINCE THIS DOUBLES
      *            THE SIZE OF THIS OBJECT & IT IS CALLED EVERY TIME
      *            A PROGRAM EXECUTES.
      * KEC 2015.1113 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USE OF SE-DIR
      * BLM 161220 MOVE EXECUTION OF LOGUID TO EXIT-PROG.  IF SE RECORD
      *            DOES NOT EXIST, USER ACTIVITY WASN'T BEING LOGGED
      * BLM 170323 CHANGE CHECK ON SE-EXP-DATE TO SKIP IF 99991231,
      *            REMOVE DATE COMPARE ROUTINES, PD0006
      * BAH 20220803 COMMENTED OUT LOGUID
      * BAH 20220920 ADDED DATERW COPY MEMBER, REMOVED DATE-YYYYMMDD FROM
      *              BARBS CHANGE ON 130329
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  3B.
       OBJECT-COMPUTER.  3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)CL/CHKSEC S35 03/20/25-09:35:45 >".
      ****************************
      *  PROGRAM WORKERS
      ****************************
       COPY "LIBGB/GB01SE.CPY".
       COPY "LIBGB/GB01SE_SQL.CPY".
       COPY "LIBGB/GBWSSE.CPY".
       01  ERRCD                      PIC X     VALUE SPACES.
       01  EXP-MESS.
           03  FILLER                 PIC X(28)
                 VALUE "FREE TRIAL PERIOD WILL END ".
           03  EXP-MESS-DATE          PIC 99/99/9999.
           03  FILLER REDEFINES EXP-MESS-DATE.
               05  EXP-DATE-MM        PIC 99.
               05  EXP-SLASH1         PIC X.
               05  EXP-DATE-DD        PIC 99.
               05  EXP-SLASH2         PIC X.
               05  EXP-DATE-YYYY      PIC 9999.
       01  FORM-PATH.
           03  FILLER                 PIC X(9).
           03  FORM-OBJ               PIC XX.
           03  FILLER                 PIC X.
           03  FORM-NAM.
               05  FILLER             PIC X(3).
               05  FORM-PROGX         PIC X(6).
           03  FILLER                 PIC X.
       01  UNAME-BUF.
           03  UNAME-LETTER           PIC X.
           03  FILLER                 PIC X(7).
       01  TODAYS-DATE                PIC 9(8).

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       01  EXIT-PATHNAME          PIC X(22).
       LINKAGE SECTION.
       01  CHKSEC-FIELDS.
           03  FORM-PATHNAME          PIC X(22).
           03  FILLER REDEFINES FORM-PATHNAME.
               05  FILLER             PIC X(15).
               05  FORM-NAME          PIC X(7).
           03  CHKSEC-STAT            PIC X(02).

       PROCEDURE DIVISION
           USING CHKSEC-FIELDS.

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************

      D    STOP MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATH BEFORE GOING INTO
      *    'SQL-CONNECT' SO THAT IT CAN GET THE EXACT PROGRAM NAME
      *    IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CHKSEC"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM SQL-CONNECT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "00" TO CHKSEC-STAT.

           PERFORM OPEN-SE1-FILE.
           MOVE FORM-NAME TO SE-FORM.
           PERFORM READ-SE1-FILE.
           PERFORM CLOSE-SE1-FILE.
           IF IO-BAD
              IF SE-FORM NOT = "LPMENU"
                 MOVE "BAD READ ON SE, KEY = &&&&&&" TO MESS
                 INSPECT MESS REPLACING FIRST "&&&&&&" BY SE-FORM
      *          PERFORM SEND-MESS
              END-IF
              GO TO EXIT-PROG.

           IF SE-PURCHASABLE = "Y"
              IF SE-EXP-DATE NOT = 99991231
                 ACCEPT TODAYS-DATE FROM CENTURY-DATE
                 IF TODAYS-DATE > SE-EXP-DATE
                    MOVE "3O" TO CHKSEC-STAT
                    MOVE "ACCESS DENIED - OPTION NOT PURCHASED" TO MESS
                    PERFORM SEND-MESS
                    GO TO EXIT-PROG
                 ELSE
                    MOVE SE-EXP-DATE TO DATE-YYYYMMDD
                    MOVE DATE-YYYYMMDD-MM   TO EXP-DATE-MM
                    MOVE DATE-YYYYMMDD-DD   TO EXP-DATE-DD
                    MOVE DATE-YYYYMMDD-YYYY TO EXP-DATE-YYYY
                    MOVE "/" TO EXP-SLASH1 EXP-SLASH2
                    MOVE EXP-MESS TO MESS
                    PERFORM SEND-MESS.

           ACCEPT UNAME-BUF FROM ENVIRONMENT "LOGNAME".
           PERFORM GET-SESUB.
           IF SESUB < 27
              IF SE-PROF(SESUB) NOT = "Y"
                 MOVE "3S" TO CHKSEC-STAT
              END-IF
           ELSE
              MOVE "3S" TO CHKSEC-STAT.
           IF CHKSEC-STAT = "3S"
              MOVE "ACCESS DENIED - SECURITY VIOLATION" TO MESS
              PERFORM SEND-MESS.

           GO TO EXIT-PROG.

       COPY "LIBGB/DATER.CPY".

      *********************************************
       GET-SESUB SECTION.
      *********************************************
           EVALUATE UNAME-LETTER
              WHEN X'61' MOVE 1 TO SESUB
              WHEN X'62' MOVE 2 TO SESUB
              WHEN X'63' MOVE 3 TO SESUB
              WHEN X'64' MOVE 4 TO SESUB
              WHEN X'65' MOVE 5 TO SESUB
              WHEN X'66' MOVE 6 TO SESUB
              WHEN X'67' MOVE 7 TO SESUB
              WHEN X'68' MOVE 8 TO SESUB
              WHEN X'69' MOVE 9 TO SESUB
              WHEN X'6A' MOVE 10 TO SESUB
              WHEN X'6B' MOVE 11 TO SESUB
              WHEN X'6C' MOVE 12 TO SESUB
              WHEN X'6D' MOVE 13 TO SESUB
              WHEN X'6E' MOVE 14 TO SESUB
              WHEN X'6F' MOVE 15 TO SESUB
              WHEN X'70' MOVE 16 TO SESUB
              WHEN X'71' MOVE 17 TO SESUB
              WHEN X'72' MOVE 18 TO SESUB
              WHEN X'73' MOVE 19 TO SESUB
              WHEN X'74' MOVE 20 TO SESUB
              WHEN X'75' MOVE 21 TO SESUB
              WHEN X'76' MOVE 22 TO SESUB
              WHEN X'77' MOVE 23 TO SESUB
              WHEN X'78' MOVE 24 TO SESUB
              WHEN X'79' MOVE 25 TO SESUB
              WHEN X'7A' MOVE 26 TO SESUB
              WHEN OTHER 
                       MOVE 99 TO SESUB.

      **********************************************
       IO-ROUTINE SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SE1-FILE.
       COPY "LIBGB/GBSEGS_SQL.CPY".
       COPY "LIBGB/GBSE1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **********************************************
       END-PROGRAM SECTION.
      **********************************************
       EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATH BEFORE GOING INTO
      *    'CLOSE-FILES' (LIBGB/*DECLARE*) SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CHKSEC"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM CLOSE-FILES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      *    IF CHKSEC-STAT = "00"
      *       CALL "LOGUID" USING FORM-PATHNAME FORM-NAME EXIT-PATHNAME
      *       CANCEL "LOGUID".

       STOP-RUN.
           EXIT PROGRAM.
