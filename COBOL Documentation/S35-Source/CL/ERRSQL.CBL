      *A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ERRSQL.
       AUTHOR.        KEC.
       DATE-WRITTEN.  05-24-2021.
      ******************************************************************
      *
      * DESCRIPTION : CALLED PROGRAM TO LOG SQL ERRORS
      *
      * NEEDED ITEMS: LIBCL/ERRLOGW.CPY
      *               SQLCA (HARDCODED FROM IO/IOCONN.CBL)
      *
      * OUTPUT FILE :  /USR/DATA/R1/LG/ERRSQL-YYMMDD
      *                 ... .... .  ..                 ...  = LOWERCASE
      *
      *=================================================================
      * REV:
      * JKC 2021-0524 ***NEW***
      * JKC 2023-1206 ADDED HEX-NUL TO BE USED TO INSPECT SQLERRMC AND
      *               SQLERRPC REPLACING ALL HEX-NUL BY SPACES.
      * JKC 2024-0917 ADDED LOGIC TO UPDATE THE SQLLOG FILE FOR CRNOFL
      *         AND EOMALL USING SQLERRPC (BATCH CODE AND BRANCH/GROUP).
      *         ADDED LOGIC TO UPDATE THE SQLTAG FILE FOR CRNOFL AND
      *         EOMALL USING SQLERRPC (BATCH CODE AND BRANCH/GROUP).
      * JKC 2024-1007 ADDED LOGIC FOR THE 'FAIL' FOR CRNOFL & EOMALL
      *         TO SHOW THE DATABASE NAME.
      * JKC 2024-1009 REMOVED SQLTAG FILE LOGIC SINCE NO LONGER NEEDED.
      *         ADDED SQLLOG FILE LOGIC FOR UPDATE THE SQLLOG USING
      *         VISION FILE SYSTEM INSTEAD OF A LINUX ECHO COMMAND.
      * JKC 2024-1114 ADDED 'IF ( IO-FG = 8 )' IN WRITE-ERRSQL-FILE.
      * JKC 2025-0128 ADDED THE PROGRAMS EOMASC AND SQLCLR TO
      *         UPDATE SPECIAL SQL PROCESSING LOG (SQLLOG)
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSSQLLOG.CPY".

           SELECT ERRSQL-FILE ASSIGN OUTPUT ERRSQL-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDSQLLOG.CPY".

       FD  ERRSQL-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 3000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  ERRSQL-REC.
           03  ERRSQL-BYTE             PIC X(01)     OCCURS 3000 TIMES.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/ERRSQL.CBL S35 01/28/25-10:39:44 >".

       COPY "LIBGB/GBWSSQLLOG.CPY".

      *==================================[ EXTRACT DISPLAY FIELD (EDF) ]
       01  EDF-WORKERS.
           03  EDF-SQLCABC             PIC 9(9)-.
           03  EDF-SQLCODE             PIC 9(9)-.
           03  EDF-SQLERRML            PIC 9(4)-.
           03  EDF-SQLERRD             PIC 9(9)- OCCURS 6 TIMES.
           03  EDF-SQLERRPL            PIC 9(4)-.
           03  EDF-TIME-HH-MM-SS       PIC 99/99/99.

      *=========================================================[ PATH ]
      * OUTPUT FILE :  /USR/DATA/R1/LG/ERRSQL-YYMMDD
      *                 ... .... .  ..                 ...  = LOWERCASE
      *=================================================================
       01  ERRSQL-PATH                 PIC X(40)     VALUE SPACES.
       01  FILLER                      REDEFINES ERRSQL-PATH.
           03  FILLER                  PIC X(01).   
           03  ERRSQL-PATH-FIL         PIC X(39).    
       01  FILLER                      REDEFINES ERRSQL-PATH.
           03  ERRSQL-PATH-SLASH-1     PIC X(01).    
           03  ERRSQL-PATH-DIR-1       PIC X(03).    
           03  ERRSQL-PATH-SLASH-2     PIC X(01).    
           03  ERRSQL-PATH-DIR-2       PIC X(04).    
           03  ERRSQL-PATH-SLASH-3     PIC X(01).    
           03  ERRSQL-PATH-DIR-3       PIC X(02).    
           03  ERRSQL-PATH-SLASH-4     PIC X(01).    
           03  ERRSQL-PATH-DIR-4       PIC X(02).    
           03  ERRSQL-PATH-SLASH-5     PIC X(01).    
           03  ERRSQL-PATH-FILENAME    PIC X(07).    
           03  ERRSQL-PATH-DATE-YYMMDD PIC 9(06).    

      *=================================================================
       01  CURRENT-DATE-YYMMDD         PIC 9(06).
       01  CURRENT-DATE-YYYY-MM-DD     PIC 9999/99/99.
       01  TODAY-DATE-6.
           03  TODAY-DATE-YY           PIC 9(02).
           03  TODAY-DATE-MM           PIC 9(02).
           03  TODAY-DATE-DD           PIC 9(02).
       01  TODAY-DATE-CC               PIC 9(02).
       01  TODAY-DATE-10               PIC X(10).
       01  WS-REL-ENVIRONMENT.
           03  FILLER                  PIC X(04)     VALUE "USR/".
           03  WS-REL-VERSION          PIC X(03)     VALUE "A##".
       01  WS-DATABASE-NAME            PIC X(10)     VALUE "STEVE".
       01  WS-SERVER-NAME              PIC X(10)     VALUE "RAAB".

      *=================================================================
       01  SQLCA.
           05  SQLCAID                 PIC X(8).
           05  SQLCABC          COMP-5 PIC S9(9).
           05  SQLCODE          COMP-5 PIC S9(9).
           05  SQLERRM.
               10  SQLERRML     COMP-5 PIC S9(4).
               10  SQLERRMC            PIC X(70).
           05  SQLERRP                 PIC X(8).
           05  SQLERRD OCCURS 6 TIMES COMP-5 PIC S9(9).
           05  SQLWARN.
               10  SQLWARN0            PIC X.
               10  SQLWARN1            PIC X.
               10  SQLWARN2            PIC X.
               10  SQLWARN3            PIC X.
               10  SQLWARN4            PIC X.
               10  SQLWARN5            PIC X.
               10  SQLWARN6            PIC X.
               10  SQLWARN7            PIC X.
               10  SQLWARN8            PIC X.
               10  SQLWARN9            PIC X.
               10  SQLWARNA            PIC X.
           05  SQLSTATE                PIC X(5).
           05  SQLERRM-PREFIX.
               10  SQLERRPL     COMP-5 PIC S9(4).
               10  SQLERRPC            PIC X(70).

      *=================================================================
       01  SQLTAG-WORKERS.
           03  SQLTAG-BATCH            PIC X(3).
           03  SQLTAG-CURRENT-BRNO     PIC X(4).
           03  SQLTAG-FILE             PIC X(35).
           03  SQLTAG-RANGE            PIC X(4).
           03  SQLTAG-RANGE-TYPE       PIC X(2).
           03  SQLTAG-USR-TMP          PIC X(9).

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(2).
           03  SUB-FROM                PIC 9(2).
           03  SUB-SIZE                PIC 9(2).
           03  SUB-TYPE                PIC 9(1).

       01  USERID-WORKERS.
           03  LOGNAME-USERID          PIC X(10).
           03  POSTNAME-USERID         PIC X(10).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
      *COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
      *=================================================================
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *=================================================================
       COPY "LIBGB/HEX_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

      *=================================================================
      *    L I N K A G E - S E C T I O N
      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       01  ERRLOG-LINK       PIC X(95).
       01  SQLCA-LINK        PIC X(208).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME ERRLOG-LINK SQLCA-LINK EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.

           MOVE SQLCA-LINK  TO SQLCA.
           MOVE ERRLOG-LINK TO ERRLOGW-AREA.

           ACCEPT CURRENT-DATE-YYMMDD FROM DATE.
           MOVE CURRENT-DATE-YYMMDD TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO CURRENT-DATE-YYYY-MM-DD.
           INSPECT CURRENT-DATE-YYYY-MM-DD REPLACING ALL "/" BY "-".

      * GET OTIS VERSION
           MOVE "REL"                  TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF (STAT-BAD)
              MOVE "USR/A??"           TO GETENV-BUF.
        
           MOVE GETENV-BUF             TO WS-REL-ENVIRONMENT.    

           MOVE EXT-ACUSQL-DB-NAME     TO WS-DATABASE-NAME. 
           MOVE EXT-ACUSQL-SERVER-NAME TO WS-SERVER-NAME.    

           MOVE "POSTNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO POSTNAME-USERID.

           MOVE "LOGNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO LOGNAME-USERID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    REPLACE NULL CHARACTERS IN SQL FIELDS WITH SPACES SO THAT
      *    DISPLAY CORRECTLY SUCH AS WITHIN MS EXCEL.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           INSPECT SQLERRMC REPLACING ALL HEX-NUL BY " ".
           INSPECT SQLERRPC REPLACING ALL HEX-NUL BY " ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET ERRSQL-PATH BY GETTING CURRENT USERS MACHINE VIA FIL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "/"                        TO ERRSQL-PATH-SLASH-1.
           MOVE EXT-FILPATH-FIL            TO ERRSQL-PATH-FIL.
           MOVE "LG"                       TO ERRSQL-PATH-DIR-4.
           MOVE "/"                        TO ERRSQL-PATH-SLASH-5.
           MOVE "ERRSQL."                  TO ERRSQL-PATH-FILENAME.
           MOVE CURRENT-DATE-YYMMDD        TO ERRSQL-PATH-DATE-YYMMDD.
           MOVE ERRSQL-PATH                TO ACCESS-BUF.
           PERFORM ACCESS-CALL.

           IF ( STAT-GOOD )
              PERFORM OPEN-ERRSQL-FILE-EXTEND
           ELSE
              PERFORM OPEN-ERRSQL-FILE-OUTPUT
              MOVE SPACES                  TO ERRSQL-REC
              STRING "DATE",                  HEX-TAB,
                     "TIME",                  HEX-TAB,
                     "REL",                   HEX-TAB,
                     "DATABASE",              HEX-TAB,
                     "SERVER",                HEX-TAB,
                     "LOGNAME",               HEX-TAB,
                     "POSTNAME",              HEX-TAB,
                     "ERRLOGW-PROGRAM",       HEX-TAB,
                     "ERRLOGW-TYPE",          HEX-TAB,
                     "ERRLOGW-FILE",          HEX-TAB,
                     "ERRLOGW-KEY",           HEX-TAB,
                     "SQLCAID",               HEX-TAB,
                     "SQLCABC",               HEX-TAB,
                     "SQLCODE",               HEX-TAB,
                     "SQLERRML",              HEX-TAB,
                     "SQLERRMC",              HEX-TAB,
                     "SQLERRP",               HEX-TAB,
                     "SQLERRD(1)",            HEX-TAB,
                     "SQLERRD(2)",            HEX-TAB,
                     "SQLERRD(3)",            HEX-TAB,
                     "SQLERRD(4)",            HEX-TAB,
                     "SQLERRD(5)",            HEX-TAB,
                     "SQLERRD(6)",            HEX-TAB,
                     "SQLWARN0",              HEX-TAB,
                     "SQLWARN1",              HEX-TAB,
                     "SQLWARN2",              HEX-TAB,
                     "SQLWARN3",              HEX-TAB,
                     "SQLWARN4",              HEX-TAB,
                     "SQLWARN5",              HEX-TAB,
                     "SQLWARN6",              HEX-TAB,
                     "SQLWARN7",              HEX-TAB,
                     "SQLWARN8",              HEX-TAB,
                     "SQLWARN9",              HEX-TAB,
                     "SQLWARNA",              HEX-TAB,
                     "SQLSTATE",              HEX-TAB,
                     "SQLERRPL",              HEX-TAB,
                     "SQLERRPC",              HEX-CARRIAGE-RETURN
                     DELIMITED BY SIZE   INTO ERRSQL-REC
              PERFORM WRITE-ERRSQL-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET COMP FIELDS TO BE NON-COMP FIELDS (EDF) FOR STRING
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "Y"      TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE HH-MM-SS TO EDF-TIME-HH-MM-SS.
           INSPECT EDF-TIME-HH-MM-SS REPLACING ALL "/" BY ":".

           MOVE SQLCABC                    TO EDF-SQLCABC.
           MOVE SQLCODE                    TO EDF-SQLCODE.
           MOVE SQLERRML                   TO EDF-SQLERRML.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                IF ( SQLERRD(SUB) NOT NUMERIC )
                   MOVE ZEROES             TO EDF-SQLERRD(SUB)
                ELSE
                   MOVE SQLERRD(SUB)       TO EDF-SQLERRD(SUB)
                END-IF
           END-PERFORM.

           MOVE SQLERRPL                   TO EDF-SQLERRPL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET SQLWARN FIELDS TO SPACES IF NOT VALID;
      *    BELIEVE IT IS NOT ALPHABETIC OR NUMERIC (PROBABLY NULL)
      *    WHICH CAUSES ISSUES WITH THE STRING AND THE TABS (HEX-TAB).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQLWARN0 NOT ALPHABETIC ) OR
              ( SQLWARN0 NOT NUMERIC    )
              MOVE SPACES TO SQLWARN0.

           IF ( SQLWARN1 NOT ALPHABETIC ) OR
              ( SQLWARN1 NOT NUMERIC    )
              MOVE SPACES TO SQLWARN1.

           IF ( SQLWARN2 NOT ALPHABETIC ) OR
              ( SQLWARN2 NOT NUMERIC    )
              MOVE SPACES TO SQLWARN2.

           IF ( SQLWARN3 NOT ALPHABETIC ) OR
              ( SQLWARN3 NOT NUMERIC    )
              MOVE SPACES TO SQLWARN3.

           IF ( SQLWARN4 NOT ALPHABETIC ) OR
              ( SQLWARN4 NOT NUMERIC    )
              MOVE SPACES TO SQLWARN4.

           IF ( SQLWARN5 NOT ALPHABETIC ) OR
              ( SQLWARN5 NOT NUMERIC    )
              MOVE SPACES TO SQLWARN5.

           IF ( SQLWARN6 NOT ALPHABETIC ) OR
              ( SQLWARN6 NOT NUMERIC    )
              MOVE SPACES TO SQLWARN6.

           IF ( SQLWARN7 NOT ALPHABETIC ) OR
              ( SQLWARN7 NOT NUMERIC    )
              MOVE SPACES TO SQLWARN7.

           IF ( SQLWARN8 NOT ALPHABETIC ) OR
              ( SQLWARN8 NOT NUMERIC    )
              MOVE SPACES TO SQLWARN8.

           IF ( SQLWARN9 NOT ALPHABETIC ) OR
              ( SQLWARN9 NOT NUMERIC    )
              MOVE SPACES TO SQLWARN9.

           IF ( SQLWARNA NOT ALPHABETIC ) OR
              ( SQLWARNA NOT NUMERIC    )
              MOVE SPACES TO SQLWARNA.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE RECORD AND WRITE TO FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO ERRSQL-REC.

           STRING CURRENT-DATE-YYYY-MM-DD,    HEX-TAB,
                  EDF-TIME-HH-MM-SS,          HEX-TAB,
                  WS-REL-VERSION,             HEX-TAB,
                  WS-DATABASE-NAME,           HEX-TAB,
                  WS-SERVER-NAME,             HEX-TAB,
                  LOGNAME-USERID,             HEX-TAB,
                  POSTNAME-USERID,            HEX-TAB,
                  ERRLOGW-PROGRAM,            HEX-TAB,
                  ERRLOGW-TYPE,               HEX-TAB,
                  ERRLOGW-FILE,               HEX-TAB,
                  ERRLOGW-KEY,                HEX-TAB,
                  SQLCAID,                    HEX-TAB,
                  EDF-SQLCABC,                HEX-TAB,
                  EDF-SQLCODE,                HEX-TAB,
                  EDF-SQLERRML,               HEX-TAB,
                  SQLERRMC,                   HEX-TAB,
                  SQLERRP,                    HEX-TAB,
                  EDF-SQLERRD(1),             HEX-TAB,
                  EDF-SQLERRD(2),             HEX-TAB,
                  EDF-SQLERRD(3),             HEX-TAB,
                  EDF-SQLERRD(4),             HEX-TAB,
                  EDF-SQLERRD(5),             HEX-TAB,
                  EDF-SQLERRD(6),             HEX-TAB,
                  SQLWARN0,                   HEX-TAB,
                  SQLWARN1,                   HEX-TAB,
                  SQLWARN2,                   HEX-TAB,
                  SQLWARN3,                   HEX-TAB,
                  SQLWARN4,                   HEX-TAB,
                  SQLWARN5,                   HEX-TAB,
                  SQLWARN6,                   HEX-TAB,
                  SQLWARN7,                   HEX-TAB,
                  SQLWARN8,                   HEX-TAB,
                  SQLWARN9,                   HEX-TAB,
                  SQLWARNA,                   HEX-TAB,
                  SQLSTATE,                   HEX-TAB,
                  EDF-SQLERRPL,               HEX-TAB,
                  SQLERRPC,                   HEX-CARRIAGE-RETURN
                  DELIMITED BY SIZE      INTO ERRSQL-REC.

           PERFORM WRITE-ERRSQL-FILE.
           PERFORM CLOSE-ERRSQL-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * UPDATE SPECIAL SQL PROCESSING LOG (FIL/../LG/SQLLOG.YYMMDD)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( ERRLOGW-PROGRAM NOT = "CRNOFL" ) AND
              ( ERRLOGW-PROGRAM NOT = "EOMALL" ) AND
              ( ERRLOGW-PROGRAM NOT = "EOMASC" ) AND
              ( ERRLOGW-PROGRAM NOT = "SQLCLR" )
              GO TO END-PROGRAM.

           MOVE ERRLOGW-PROGX              TO SQLLOG-PROGRAM.
           MOVE SQLERRPC                   TO SQLLOG-RANGES.
           MOVE "FAIL"                     TO SQLLOG-STATUS.
           MOVE SQLERRMC                   TO SQLLOG-MESS.
           PERFORM UPDATE-SQLLOG-FILE.

           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
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
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *=================================================================

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/GBSQLLOGIN.CPY".
  
      *=================================================================
       OPEN-ERRSQL-FILE-EXTEND.
           OPEN EXTEND ERRSQL-FILE.

           IF ( IO-FG = 8 )
             GO TO OPEN-ERRSQL-FILE-EXTEND.

           IF ( IO-FG = 7 )
             CLOSE ERRSQL-FILE
             GO TO OPEN-ERRSQL-FILE-EXTEND.

      *-----------------------------------------------------------------
       OPEN-ERRSQL-FILE-OUTPUT.
           OPEN OUTPUT ERRSQL-FILE.

           IF ( IO-FG = 8 )
             GO TO OPEN-ERRSQL-FILE-OUTPUT.

           IF ( IO-FG = 7 )
             CLOSE ERRSQL-FILE
             GO TO OPEN-ERRSQL-FILE-OUTPUT.

      *-----------------------------------------------------------------
       WRITE-ERRSQL-FILE.
           WRITE ERRSQL-REC.

           IF ( IO-FG = 8 )
             GO TO WRITE-ERRSQL-FILE.

      *-----------------------------------------------------------------
       CLOSE-ERRSQL-FILE.
           CLOSE ERRSQL-FILE.

      *-----------------------------------------------------------------
      *START-MISC-CSR.
      *    MOVE "SSP_TCLP_SELECT" TO E-FILE.
      *    MOVE SPACES            TO E-KEYX.
      *    MOVE "START ERROR"     TO E-MSG.
      *    MOVE "GB/SQLINF"       TO E-PROG.

      *    IF ( MISC-CURSOR-STAT-GOOD )
      *       PERFORM CLOSE-MISC-CSR.

      *    MOVE "SELECT DB_NAME(),@@SERVERNAME,@@VERSION
      *       TO PASS-SQL.

      *    EXEC SQL
      *       DECLARE MISC_CURSOR CURSOR FOR
      *              :MISC-RETURN-CODE =
      *               EXEC SSP_TCLP_SELECT
      *                ( :PASS-SQL)
      *    END-EXEC.

      *    EXEC SQL
      *        OPEN MISC_CURSOR
      *    END-EXEC.

      *    IF SQLCODE NOT = 0
      *       PERFORM SQL-ERROR.

      *    MOVE STAT---GOOD TO MISC-CURSOR-STAT.
      *------------------------------------------------------------
      *READ-MISC-CSR-NEXT.
      *    MOVE "SSP_TCLP_SELECT" TO E-FILE.
      *    MOVE SPACES            TO E-KEYX.
      *    MOVE "READ ERROR"      TO E-MSG.
      *    MOVE "GB/SQLINF"       TO E-PROG.

      *    IF ( MISC-CURSOR-STAT-GOOD )
      *       EXEC SQL
      *          FETCH MISC_CURSOR INTO
      *            :RET-DB-NAME,
      *            :RET-SERVER-NAME,
      *            :RET-VERSION
      *       END-EXEC
      *    ELSE
      *       MOVE 9             TO SQLCODE
      *       MOVE "RDNXCR"      TO SQLSTATE.

      *    IF SQLCODE NOT = 0
      *       PERFORM SQL-ERROR.

      *------------------------------------------------------------
      *CLOSE-MISC-CSR.
      *    MOVE "SSP_TCLP_SELECT" TO E-FILE.
      *    MOVE SPACES           TO E-KEYX.
      *    MOVE "CLOSE ERROR"    TO E-MSG.
      *    MOVE "GB/SQLINF"      TO E-PROG.

      *    IF ( MISC-CURSOR-STAT-GOOD )
      *       EXEC SQL
      *            CLOSE MISC_CURSOR
      *       END-EXEC
      *       MOVE STAT---BAD TO MISC-CURSOR-STAT.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
