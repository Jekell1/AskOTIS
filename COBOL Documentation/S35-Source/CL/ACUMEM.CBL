       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ACUMEM.
       AUTHOR.          JKC.
       DATE-WRITTEN.    08-30-2023.
      ******************************************************************
      *
      * IDENTIFICATION: CL/ACUMEM
      *
      * DESCRIPTION   : ACUCOBOL MEMORY USAGE LOGGING PROGRAM
      *
      *                 CALLED FROM THE 'CLOSE-FILES' PARAGRAPHS
      *                 WITHIN THE FOLLOWING:
      *                   + LIBGB/DECLARE
      *                   + LIBGB/DECLAREOD
      *                   + LIBGB/DECLARE_SQL
      *                   + LIBGB/DECLRP
      *                   + LIBGB/DECLRP_SQL
      *
      * SHELL SCRIPT  : /USR/???/SH/TCLP_ACUMEM.SH
      *                 TASC-OPTION = 1
      *                   + USED TO FIND THE SYSTEM PROCESS ID (PID)
      *                     FOR A GIVEN RUNCBL SESSION THEN MAKE A 
      *                     MEMORY USAGE LOG FILE.
      *                 TASC-OPTION = 2
      *                   + SECONDARY CALLS TO THIS SCRIPT WILL RECORD
      *                     MEMORY USAGE TO THE LOG FILE FOR A GIVEN
      *                     PROGRAM AFTER EXECUTION.
      *
      *=================================================================
      * REV:
      * JKC 2023-0830 ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-----------------------------------------------------------------
      * TCLP-ACUMEM FILE - /USR/TMP/???MKTEMP???
      *-----------------------------------------------------------------
           SELECT TCLP-ACUMEM-FILE ASSIGN TO TCLP-ACUMEM-PATH
                          ORGANIZATION LINE SEQUENTIAL
                          ACCESS SEQUENTIAL
                          LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                          FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      *-----------------------------------------------------------------
      * TCLP-ACUMEM FILE - /USR/TMP/???MKTEMP???
      *-----------------------------------------------------------------
       FD  TCLP-ACUMEM-FILE
           RECORD IS VARYING IN SIZE FROM 0 TO 20 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  TCLP-ACUMEM-REC               PIC X(10).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)CL/ACUMEM S35 08/20/24-07:42:43 >".

      *=================================================================
       01  ERRCD                       PIC X(01)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  FORM-PATH.
           03  FILLER                  PIC X(1).
           03  FORM-REL                PIC X(7).
           03  FILLER                  REDEFINES FORM-REL.
               05  FILLER              PIC X(4).
               05  LOGUID-REL          PIC X(3).
           03  FILLER                  PIC X(1).
           03  FORM-OBJ                PIC X(2).
           03  FILLER                  PIC X(1).
           03  FORM-NAM.
               05  FORM-DIRX           PIC X(2).
               05  FILLER              PIC X(1).
               05  FORM-PROGX          PIC X(6).
           03  FILLER                  PIC X(1).

      *-----------------------------------------------------------------
       01  EXIT-PATHNAME               PIC X(22)     VALUE SPACES.
       01  TCLP-ACUMEM-PATH            PIC X(40)     VALUE SPACES.

       01  TCLP-ACUMEM-SHELL-COMMAND. 
           03  FILLER                 PIC X(09) VALUE "/$REL/SH/".
           03  TASC-EC                PIC X(03) VALUE "EC ".
           03  FILLER                 PIC X(09) VALUE "/$REL/SH/".
           03  TASC-SCRIPT            PIC X(15) VALUE "TCLP_ACUMEM.SH ".
           03  TASC-OPTION            PIC 9(01) VALUE ZEROES.
           03  FILLER                 PIC X(01) VALUE SPACES.
           03  TASC-MKTEMP            PIC X(35) VALUE SPACES.
           03  TASC-FORM-PROGX        REDEFINES TASC-MKTEMP
                                      PIC X(35).
           03  FILLER                 PIC X(01) VALUE SPACES.
           03  TASC-PID               PIC X(10) VALUE SPACES.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

      ******************************************************************
      *
      *    L I N K A G E - S E C T I O N
      *
      ******************************************************************
       LINKAGE SECTION.
       01  FORM-PATHNAME               PIC X(22).

      ******************************************************************
      *
      *    P R O C E D U E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 TCLP-ACUMEM-FILE.

       COPY "LIBGB/DECLARE.CPY".

           CLOSE TCLP-ACUMEM-FILE.

       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP FORM-PATHNAME.

           IF ( EXT-TCLP-ACUMEM-DISABLED )
              GO TO MAIN-PROGRAM-EXIT.

           IF ( EXT-TCLP-ACUMEM-CREATE )
              PERFORM ACUMEM-CREATE-FILE
           ELSE
           IF ( EXT-TCLP-ACUMEM-UPDATE )
              PERFORM ACUMEM-UPDATE-FILE.

      *=================================================================
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    A C U M E M - C R E A T E - F I L E
      *
      ******************************************************************
       ACUMEM-CREATE-FILE SECTION.

           CALL "C$TOLOWER" USING TASC-EC, VALUE 2.
           CALL "C$TOLOWER" USING TASC-SCRIPT, VALUE 15.

           MOVE EXT-TCLP-ACUMEM            TO TASC-OPTION.

           MOVE MKTEMP-DEFAULT             TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF                 TO TCLP-ACUMEM-PATH
                                              TASC-MKTEMP.

           MOVE SPACES                     TO TASC-PID.

           MOVE TCLP-ACUMEM-SHELL-COMMAND  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           IF ( STAT-BAD )
      * DISABLE ACUMEM
              MOVE "N"    TO EXT-TCLP-ACUMEM
              MOVE SPACES TO EXT-TCLP-ACUMEM-PID
              GO TO ACUMEM-CREATE-FILE-EXIT.

      * CHECK IF TCLP-ACUMEM-FILE WAS CREATED; IF SO THEN READ IT
      * TO GET THIS RUNCBL'S PROCESS ID (PID).
      
           MOVE SPACES TO TCLP-ACUMEM-REC.
           PERFORM OPEN-TCLP-ACUMEM-FILE.
           PERFORM READ-TCLP-ACUMEM-FILE.
           PERFORM CLOSE-TCLP-ACUMEM-FILE.

           IF ( IO-BAD                   ) OR 
              ( TCLP-ACUMEM-REC = SPACES )
      * DISABLE ACUMEM
              MOVE "N"    TO EXT-TCLP-ACUMEM
              MOVE SPACES TO EXT-TCLP-ACUMEM-PID
              GO TO ACUMEM-CREATE-FILE-EXIT.

      * UPDATE ACUMEM
           MOVE "2"             TO EXT-TCLP-ACUMEM.
           MOVE TCLP-ACUMEM-REC TO EXT-TCLP-ACUMEM-PID.

      *=================================================================
       ACUMEM-CREATE-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    A C U M E M - U P D A T E - F I L E
      *
      ******************************************************************
       ACUMEM-UPDATE-FILE SECTION.

           CALL "C$TOLOWER" USING TASC-EC, VALUE 2.
           CALL "C$TOLOWER" USING TASC-SCRIPT, VALUE 15.

           MOVE EXT-TCLP-ACUMEM            TO TASC-OPTION.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE FORM-PROGX                 TO TASC-FORM-PROGX.

           MOVE EXT-TCLP-ACUMEM-PID        TO TASC-PID.

           MOVE TCLP-ACUMEM-SHELL-COMMAND  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      *=================================================================
       ACUMEM-UPDATE-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".

      *=================================================================
       OPEN-TCLP-ACUMEM-FILE.

           PERFORM OPEN-IT.
           MOVE "TCLP-ACUMEM" TO E-FILE.
           OPEN INPUT TCLP-ACUMEM-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-TCLP-ACUMEM-FILE.

           IF ( IO-ALREADY-OPEN )
              CLOSE TCLP-ACUMEM-FILE
              GO TO OPEN-TCLP-ACUMEM-FILE.

      *-----------------------------------------------------------------
       READ-TCLP-ACUMEM-FILE.

           PERFORM READ-IT.
           MOVE "TCLP-ACUMEM" TO E-FILE.
           READ TCLP-ACUMEM-FILE.

           IF ( IO-LOCKED )
              GO TO READ-TCLP-ACUMEM-FILE.

      *-----------------------------------------------------------------
       CLOSE-TCLP-ACUMEM-FILE.

           PERFORM CLOSE-IT.
           CLOSE TCLP-ACUMEM-FILE.

      *==================================[ NEEDS TO BE LAST IN SECTION ]
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
       END-RUN.

           MOVE TCLP-ACUMEM-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
