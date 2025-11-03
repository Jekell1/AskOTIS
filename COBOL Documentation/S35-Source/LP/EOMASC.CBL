       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EOMASC.
       AUTHOR.        MARK BOOMGAARD.
       DATE-WRITTEN.  10-07-2024.
      ******************************************************************
      *  DESCRIPTION: END OF MONTH CLEAR & PROFILE UPDATE    S35Q-104
      *
      *  APPLIES CHANGES/INSERTS INTO SQL TABLES FOR FILE OUTPUT FROM
      *  EOMALL LNFILE, RCFILE, BYAFILE, BYBFILE, GBFILE
      *  
      * EOMASC CALLS A SCRIPT TO RUN BCPS (SQL BATCH LOADS) LOADING THE
      * EOMALL OUTPUT RECORDS INTO A WORK TABLES.
      * ONCE COMPLETED, A SCRIPT IS CALLED TO USE SQLCMD TO EXECUTE
      * UPDATE (REWRITE) QUERY. BYAFILE/BYBFILE PROCESSING ALSO USES
      * SQLCMD TO EXECUTE INSERT (WRITE) QUERIES.
      *  
      * EOMASC LOOKS FOR FILES CREATED BY EOMALL - SEE TCLP-LS-COMMAND
      * WHICH RUNS LINUX LS -L COMMAND OUTPUTTING TO A FILE READ BY
      * EOMASC.  THIS FILE WILL CONTAIN THE FILES CREATED BY EOMALL FOR
      * EOMASC TO USE TO APPLY CHANGES TO TABLES.
      *
      * IN INITIALIZATION, SEE LOADING OF FILETYPE-ARRAY WHICH CONTAINS
      * THE DIFFERENT FILES TO SEARCH FOR AND WORK TABLE TO COPY/USE.
      * FOR EACH FILE/TABLE TYPE (LN,RC,BYA,BYB,GB) A WORK TABLE IS
      * COPIED TO A TEMP TABLE AND USED FOR THE UPDATE PROCESS.  THE
      * WORK TABLE CANNOT BE USED JUST IN CASE MULTIPLE EOMASC ARE
      * RUNNING AT SAME TIME AND WOULD HAVE ISSUES INSERTING INTO AND
      * TRUNCATING SAME WORK TABLE.
      *
      ******************************************************************
      *   @@@@   ERROR 27 WHEN CALLING SCRIPT   @@@@
      *  THERE IS A PROBLEM CALLING EC PROGRAM THAT RUNS THE SHELL .SH
      *  FILE, THE LAST ISSUE WAS:
      *    MISSING LINUX LOAD;
      *    NEED GLIBC.I686 PACKAGE INSTALLED
      *    SUDO YUM INSTALL GLIBC.I686
      *
      *  ALSO WATCH PERMISSIONS ON .SH FILES
      ******************************************************************
      * REV:
      * MFB 2024-1007 ***NEW*** S35Q-104
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSSQLLOG.CPY".

           SELECT FILELIST-IN-FILE ASSIGN TCLP-LS-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS FILE-STAT.

           SELECT SCRIPT-RESULT-FILE ASSIGN SCRIPT-RESULT-FILE-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

           SELECT STAT-FILE ASSIGN STAT-FILE-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

           SELECT GBDATA-IN-FILE ASSIGN GBDATA-IN-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

           SELECT RCDATA-IN-FILE ASSIGN RCDATA-IN-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBGB/GBFDSQLLOG.CPY".

       FD  FILELIST-IN-FILE
           LABEL RECORDS ARE STANDARD.
       01  FILELIST-IN-REC             PIC X(200).

       FD  SCRIPT-RESULT-FILE
           LABEL RECORDS ARE STANDARD.
       01  SCRIPT-RESULT-REC           PIC X(200).

       FD  STAT-FILE
           LABEL RECORDS ARE STANDARD.
       01  STAT-REC                    PIC X(200).

       FD  GBDATA-IN-FILE
           LABEL RECORDS ARE STANDARD.
       01  GBDATA-IN-REC               PIC X(24).

       FD  RCDATA-IN-FILE
           LABEL RECORDS ARE STANDARD.
       01  RCDATA-IN-REC               PIC X(16).

      *****************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/EOMASC.CBL S35 04/15/25-13:41:07 >".

       COPY "LIBGB/GBWSSQLLOG.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-SQL                    PIC X(200).
       01  MISC-RETURN-CODE            PIC S9(4)   COMP-5.
        EXEC SQL END DECLARE SECTION END-EXEC.

      *****************************************************************
      *     F I L E   I F N
      *****************************************************************
      *----------------------------------------------------------------
       01  STAT-FILES-EOM              PIC X(6)     VALUE SPACES.
       01  STAT-FILES-OK-IND           PIC X        VALUE "Y".
       01  STAT-FILE-PATH              PIC X(150)   VALUE SPACES.
       01  MISC-MSG                    PIC X(150)   VALUE SPACES.
       01  TRIGGER-LN-ENABLED-SW       PIC X        VALUE "Y".
       01  FOUND-SW                    PIC X        VALUE "N".
       01  PROC-FILES-LINE-CTR1        PIC ZZ9.
       01  PROC-FILES-LINE-CTR2        PIC ZZ9.
       01  USE-PROC-ID                 PIC X(6)      VALUE SPACES.
       01  ECHO-CMD                    PIC X(8)      VALUE "ECHO -E ".
       01  PROC-SUB                    PIC 9(5)      VALUE 0.
       01  TOT-FILES-PROC-CTR          PIC 9(5)      VALUE 0.
       01  DASH-CTR                    PIC 9(3)      VALUE 0.
       01  FILES-CTR                   PIC 9(5)      VALUE 0.
       01  FILES-CTR2                  PIC 9(5)      VALUE 0.
       01  MISC-CTR                    PIC 9(9)      VALUE 0.
       01  FILES-TO-PROCESS-CTR        PIC 9(5)      VALUE 0.
       01  FILELIST-EOF-SW             PIC X         VALUE "N".
       01  END-BRANCH                  PIC 9(4)      VALUE ZEROS.
       01  PR-IFN                      PIC XX        VALUE "PR".
       01  STARTING-TIME               PIC X(8)      VALUE SPACES.
       01  PROC-TIME                   PIC X(8)      VALUE SPACES.
       01  RESULT-FILE-EOF-SW          PIC X         VALUE "N".
       01  SCRIPT-CALL-ERROR-SW        PIC X         VALUE "N".
       01  SQL-TYPE                    PIC X(12)     VALUE SPACES.
       01  SCRIPT-TYPE                 PIC X(3)      VALUE SPACES.
       01  SCRIPT-ERROR-MSG            PIC X(150)    VALUE SPACES.
       01  SCRIPT-ERROR-DETAIL-MSG     PIC X(150)    VALUE SPACES.
       01  SCRIPT-SQL-FILE             PIC X(30)     VALUE SPACES.
       01  SQLLOG-FINAL-MSG            PIC X(300)    VALUE SPACES.
       01  SQLLOG-FINAL-STAT           PIC X(4)      VALUE "FAIL".
       01  SQL-TEMP-TABLE              PIC X(40)     VALUE SPACES.
       01  SQL-TEMP-TABLE-EXIST-SW     PIC X         VALUE "N".
       01  CURRENT-DATE-YYYY-MM-DD     PIC 9999/99/99.
       01  CURRENT-DATE-YYMMDD         PIC 9(06).
       01  LAST-BRACKET-POS            PIC 9(3)      VALUE ZEROES.
       01  WS-TIME-DISP.
           03 WS-TIME-DISP-HH          PIC 99.
           03 FILLER                   PIC X         VALUE ":".
           03 WS-TIME-DISP-MM          PIC 99.
           03 FILLER                   PIC X         VALUE ":".
           03 WS-TIME-DISP-SS          PIC 99.

       01  STAT-FILES-ARRAY-CTR        PIC 9(5)      VALUE 0.
       01  STAT-FILES-ARRAY            OCCURS 30 TIMES
                                       PIC X(200).

       01  FILETYPE-ARRAY              OCCURS 5 TIMES.
           03 FILEARRAY-SRCH-STR       PIC X(3).
           03 FILEARRAY-WORK-TBL       PIC X(40).
           03 FILEARRAY-TEMP-TBL       PIC X(40).
           03 FILEARRAY-REWRITE-SQL    PIC X(40).
           03 FILEARRAY-WRITE-SQL      PIC X(40).
      *==================================[ EXTRACT DISPLAY FIELD (EDF) ]
       01  EDF-WORKERS.
           03  EDF-SQLCABC             PIC 9(9)-.
           03  EDF-SQLCODE             PIC 9(9)-.
           03  EDF-SQLERRML            PIC 9(4)-.
           03  EDF-SQLERRD             PIC 9(9)- OCCURS 6 TIMES.
           03  EDF-SQLERRPL            PIC 9(4)-.
           03  EDF-TIME-HH-MM-SS       PIC 99/99/99.

       01  GBDATA-IN-PATH              PIC X(41).
       01  GBDATA-WS-REC.
           03  GBDATA-IN-BRNO          PIC 9(4).
           03  FILLER                  PIC X.
           03  GBDATA-IN-NO            PIC 9.
           03  FILLER                  PIC X.
           03  GBDATA-IN-EOM-TAG-CCYYMM PIC 9(6).
           03  FILLER                  PIC X.
           03  GBDATA-IN-EOM-UPDATE-USERID   PIC X(10).

       01  RCDATA-IN-PATH              PIC X(41).
       01  RCDATA-WS-REC.
           03  RCDATA-IN-BRNO          PIC 9(4).
           03  FILLER                  PIC X.
           03  RCDATA-IN-TRANS-DATE    PIC 9(8).
           03  FILLER                  PIC X.
           03  RCDATA-IN-STATUS        PIC XX.

       01  SCRIPT-RESULT-FILE-PATH.
           03  SRF-PATH-DIR.
              05  SRF-PATH-BASE        PIC X(9).
              05  FILLER               PIC X       VALUE "/".
              05  SRF-PATH-MACHINE     PIC X(2).
              05  FILLER               PIC X(3)    VALUE "/WK".
           03  FILLER                  PIC X(8)    VALUE "/EOMASC-".
           03  SRF-PATH-PROC-ID        PIC X(6).
           03  FILLER                  PIC X       VALUE "-".
           03  SRF-PATH-BEGBRNO        PIC X(4).
           03  SRF-PATH-RES            PIC X(4)    VALUE ".RES".

       01  EOMALL-WORK-DIR-PATH.
           03  EWD-PATH-BASE           PIC X(9).
           03  FILLER                  PIC X        VALUE "/".
           03  EWD-PATH-MACHINE        PIC X(2).
           03  FILLER                  PIC X(3)    VALUE "/WK".

       01  TCLP-SHELL-COMMAND. 
           03  TCLP-SHELL-PGM.
              05  FILLER               PIC X(9)   VALUE "/$REL/SH/".
              05  TASC-EC              PIC X(3)   VALUE "EC ".
              05  FILLER               PIC X(9)   VALUE "/$REL/SH/".
              05  TASC-SCRIPT          PIC X(20)  VALUE SPACES.
           03  TCLP-SHELL-PARMS        PIC X(200) VALUE SPACES.

       01  TCLP-LS-COMMAND. 
          03  TCLP-LS-LS               PIC X(6)   VALUE "LS -1 ".
          03  TCLP-LS-SRCH-DIR         PIC X(15)  VALUE SPACES.
          03  TCLP-LS-FILETYPE         PIC X(20)  VALUE SPACES.
          03  FILLER                   PIC X(3)   VALUE " > ".
          03  TCLP-LS-PATH             PIC X(35)  VALUE SPACES.

       01  TCLP-MV-COMMAND. 
          03  TCLP-MV-MV               PIC X(3)   VALUE "MV ".
          03  TCLP-MV-SRC-FILE         PIC X(70)  VALUE SPACES.
          03  FILLER                   PIC X(2)   VALUE SPACES.
          03  TCLP-MV-TGT-DIR          PIC X(70)  VALUE SPACES.

      *****************************************************************
      *   MISC WORKERS     *
      *****************************************************************
       01  SUB                     PIC 9(6)       VALUE 0.
       01  MESS-LIST               PIC X(5)       VALUE "MESS.".
       01  WARNING-LIST            PIC X(8)       VALUE "WARNING.".
       01  LEGEND                  PIC X(70)      VALUE SPACES.

      *****************************************************************
      *     S C R E E N   F I E L D S   *
      *****************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH          PIC 9(4).
           03  GROUP-FLAG          PIC X.
           03  ENT-GROUP           PIC X(4).

       01 VERSION-CONTROL          PIC X VALUE "B".
      ********************************
      *   EOMUPD - COMMANDS
      ********************************
      * "/$REL/SH/EC /$REL/SH/EOM2.SH ".
       01  REMOVE-CMD.
           03  FILLER         PIC X    VALUE "/".
           03  REMOVE-REL     PIC X(7).
           03  FILLER         PIC X(4) VALUE "/SH/".
           03  REMOVE-EC      PIC X(3) VALUE "EC ".
           03  FILLER         PIC X    VALUE "/".
           03  REMOVE-REL2    PIC X(7).
           03  FILLER         PIC X(4) VALUE "/SH/".
           03  REMOVE-SHELL   PIC X(8) VALUE "EOM2.SH ".
           03  REMOVE-MM      PIC 99.
           03  FILLER         PIC X.
           03  REMOVE-YY      PIC 99.
           03  FILLER         PIC X.
           03  REMOVE-MACHINE PIC XX.
           03  FILLER         PIC X.
           03  REMOVE-DATA    PIC X(6).
           03  FILLER         PIC X.
           03  REMOVE-EOD-MM  PIC 99.
           03  FILLER         PIC X.
           03  REMOVE-EOD-YY  PIC 99.
           03  FILLER         PIC X.
           03  REMOVE-FIL-PATH PIC X(8).

      ******************************************************************
      *    P R I N T - W O R K E R S
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED                 PIC 99        VALUE 0.
           03  LN-COUNT                PIC 99        VALUE 61.
           03  LN-WK                   PIC 99        VALUE 0.
       01  PAGE-NUMBER                 PIC 9(4)      VALUE 0.

      *=================================================================
       01  DETAIL-LINE                 PIC X(132)    VALUE SPACES.

       01  RANGE-LN          REDEFINES DETAIL-LINE.
           03  RANGE-LINE              PIC X(31).
           03  RANGE-SELECTION         PIC X(101).

      *=================================================================
       01  HEAD1.
           03  H-DATE                  PIC X(8)      VALUE " ".
           03  FILLER                  PIC XX        VALUE " ".
           03  H-HR                    PIC 99        VALUE 0.
           03  FILLER                  PIC X         VALUE ":".
           03  H-MIN                   PIC 99        VALUE 0.
           03  FILLER                  PIC X(31)     VALUE " ".
           03  H-NAME                  PIC X(40)     VALUE " ".
           03  FILLER                  PIC X(37)     VALUE " ".
           03  FILLER                  PIC X(5)      VALUE "PAGE ".
           03  H-PAGE-NO               PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H-USERID                PIC X(10).
           03  FILLER                  PIC X(28)     VALUE " ".
           03  FILLER                  PIC X(40)     VALUE 
                          "END OF MONTH CLEAR & PROFILE UPDATE".

      *****************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      *****************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/RANGE_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/HEX_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EOMASC_DEF.CPY".
       COPY "LIBLP/EOMASC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EOMASC_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE FILELIST-IN-FILE SCRIPT-RESULT-FILE STAT-FILE
               GBDATA-IN-FILE RCDATA-IN-FILE.
       COPY "LIBGB/DECLRP_ASCII.CPY".
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************************************************
      *
      *    M A I N    M O D U L E
      *
      *****************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EOMASC" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

      D    STOP MESS.
           PERFORM EOM-UPDATE.

      * SEND MESSAGE TO SCREEN IF ERROR OCCURRED DURING BCP OR SQLCMD PROCESSING
           IF SCRIPT-CALL-ERROR-SW = "Y"
             MOVE SPACES TO MESS
             STRING "ERROR: ", SCRIPT-ERROR-MSG INTO MESS
             PERFORM SEND-MESS
             GO TO END-PROGRAM.

      * CREATE PASS ENTRY IN SQLLOG
           MOVE "PASS" TO SQLLOG-FINAL-STAT.
           STRING "FILES PROCESSED: ", TOT-FILES-PROC-CTR
                  " | ", EXT-ACUSQL-DB-NAME
                  DELIMITED BY "  " INTO SQLLOG-FINAL-MSG.
           PERFORM CREATE-SQLLOG-ENTRY.

           PERFORM PRINT-RANGE-LINES.

       END-ROUTINE.
           GO TO END-PROGRAM.

      *****************************************************************
       EOM-UPDATE SECTION.
      *****************************************************************
      D    STOP MESS.

           MOVE SPACES TO DETAIL-LINE.
           STRING "PROCESSING FILES:", TCLP-LS-SRCH-DIR
              DELIMITED BY "  " INTO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PROCESS 5 FILE TYPES (LN, BYA, BYB, GB AND RC)
           PERFORM VARYING PROC-SUB FROM 1 BY 1 
            UNTIL PROC-SUB > 5
              MOVE 0 TO FILES-CTR FILES-CTR2  
              MOVE FILEARRAY-TEMP-TBL(PROC-SUB) TO SQL-TEMP-TABLE

              PERFORM GET-TIME
              MOVE TIME-EDIT TO PROC-TIME
              MOVE SPACES TO DETAIL-LINE
              STRING " PROCESSING FILES ",FILEARRAY-SRCH-STR(PROC-SUB),
                 " ", PROC-TIME
                 DELIMITED BY "  "INTO DETAIL-LINE
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE

      * DISABLE LNFILE TRIGGER IF STARTING TO PROCESS LN FILES
              IF PROC-SUB = 1 
                 PERFORM DISABLE-LNFILE-TRIGGER
              END-IF

      * CREATE TEMP TABLES FOR LN, BYA AND BYB PROCESSING
              IF PROC-SUB = 1 OR 2 OR 3
                 PERFORM CREATE-TEMP-TABLE
                 PERFORM CREATE-TEMP-TABLE-INDEX
              END-IF

      * RETREIVE FILES TO PROCESS INTO FILELIST-FILE
              PERFORM RETR-FILE-LIST 

              PERFORM OPEN-FILELIST-FILE
              MOVE FILES-TO-PROCESS-CTR TO PROC-FILES-LINE-CTR2
 
              MOVE SPACES TO DETAIL-LINE
              STRING "  FILES TO PROCESS: ", PROC-FILES-LINE-CTR2
                  INTO DETAIL-LINE
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE

              PERFORM READ-FILELIST-FILE-NEXT
 
      * LOOP THRU FILES TO PROCESS
              PERFORM UNTIL FILELIST-EOF-SW = "Y"
                 ADD 1 TO FILES-CTR FILES-CTR2
                 MOVE FILES-CTR TO PROC-FILES-LINE-CTR1
                 PERFORM UPDATE-PROC-MSG

                 PERFORM GET-TIME
                 MOVE CUR-HH TO WS-TIME-DISP-HH
                 MOVE CUR-MM TO WS-TIME-DISP-MM
                 MOVE CUR-SS TO WS-TIME-DISP-SS
                 MOVE SPACES    TO DETAIL-LINE
                 STRING "   PROCESSING ", FILELIST-IN-REC,
                     " ", WS-TIME-DISP 
                     DELIMITED BY "    "INTO DETAIL-LINE
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE

                 IF PROC-SUB = 1
                    PERFORM PROCESS-LN-FILE
                 ELSE
                    IF PROC-SUB = 2 OR 3
                       PERFORM PROCESS-BY-FILE
                    ELSE
                       IF PROC-SUB = 4
                          PERFORM PROCESS-GB-FILE
                       ELSE
                          PERFORM PROCESS-RC-FILE
                       END-IF
                    END-IF
                 END-IF

                 IF SCRIPT-CALL-ERROR-SW = "Y"
                    EXIT PERFORM 
                 END-IF

      * REMOVE PROCESSED FILE
                 MOVE FILELIST-IN-REC TO ACCESS-BUF
                 PERFORM REMOVE-WORKFILE

                 PERFORM READ-FILELIST-FILE-NEXT

              END-PERFORM  

              PERFORM CLOSE-FILELIST-FILE

              IF SCRIPT-CALL-ERROR-SW = "Y"
                 EXIT PERFORM 
              END-IF

              IF PROC-SUB = 1
                 PERFORM ENABLE-LNFILE-TRIGGER
              END-IF

              IF PROC-SUB = 1 OR 2 OR 3
                 PERFORM DROP-TEMP-TABLE
              END-IF


      * REMOVE FILELIST FILE
              MOVE TCLP-LS-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE

              MOVE 1      TO LN-FEED
              MOVE SPACES TO DETAIL-LINE
              STRING "  FILES PROCESSED: ",PROC-FILES-LINE-CTR1 
                        INTO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE

              PERFORM CLEAR-PROC-MSG

              ADD FILES-CTR TO TOT-FILES-PROC-CTR

           END-PERFORM.

           PERFORM CLEAR-PROC-MSG.

           IF SCRIPT-CALL-ERROR-SW = "Y"
              GO TO EOM-UPDATE-EXIT 
           END-IF.

      * REMOVE STAT FILES
           PERFORM VARYING PROC-SUB FROM 1 BY 1
            UNTIL PROC-SUB > STAT-FILES-ARRAY-CTR
              MOVE STAT-FILES-ARRAY(PROC-SUB) TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE
           END-PERFORM.

       EOM-UPDATE-EXIT.
           EXIT. 


       CALL-BCP-SCRIPT SECTION.
           MOVE SPACES TO SQL-TYPE.

           MOVE "TCLP_BCP.SH " TO TASC-SCRIPT.
           CALL "C$TOLOWER" USING TASC-SCRIPT VALUE 20.

      * RUN BCP TO LOAD WORK TABLE
           MOVE "BCP"  TO SCRIPT-TYPE.
           MOVE SPACES TO TCLP-SHELL-PARMS.
           STRING EXT-ACUSQL-DSN-NAME, " ",
                  EXT-ACUSQL-DB-NAME , " ",
                  EXT-ACUSQL-LOGIN   , " ",
                  EXT-ACUSQL-PSWD    , " ",
                  SQL-TEMP-TABLE     , " ", 
                  FILELIST-IN-REC    , " ", 
                  SCRIPT-RESULT-FILE-PATH
             DELIMITED BY "  " INTO TCLP-SHELL-PARMS.
           MOVE TCLP-SHELL-COMMAND  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF (STAT-BAD)
              GO TO CALL-BCP-SCRIPT-ERROR.

      * ERROR IF RESULT FILE NOT FOUND
           MOVE "RES"                   TO SCRIPT-TYPE.
           MOVE SCRIPT-RESULT-FILE-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO CALL-BCP-SCRIPT-ERROR.

      * RETRIEVE RESULT FILE COMPLETION MESSAGE
      * ERROR IF NOT FOUND
           MOVE "BCP" TO SCRIPT-TYPE.
           PERFORM RETR-RESULT-MSG.
           IF FOUND-SW = "N"
              GO TO CALL-BCP-SCRIPT-ERROR.

      * REMOVE RESULT FILE
           MOVE SCRIPT-RESULT-FILE-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           GO TO CALL-BCP-SCRIPT-EXIT.

       CALL-BCP-SCRIPT-ERROR.
           IF SCRIPT-TYPE = "BCP"  
              STRING "ERROR CALLING BCP SCRIPT: ", TASC-SCRIPT
               INTO SCRIPT-ERROR-MSG
           ELSE
              STRING TASC-SCRIPT, " RESULT FILE NOT FOUND" 
               INTO SCRIPT-ERROR-MSG
           END-IF.

           PERFORM SCRIPT-ERROR-HANDLER.
           MOVE "Y" TO SCRIPT-CALL-ERROR-SW.

       CALL-BCP-SCRIPT-EXIT.
           EXIT.


       CALL-SQLCMD-SCRIPT SECTION.
      *** WARNING: CALLING SQLCMD SCRIPT WILL NOT RETURN ERROR CODE LIKE BCP SCRIPT DOES!!!
      ***  ALWAYS NEED TO READ RESULT FILE  
           MOVE "EOMASC_SQLCMDDBU.SH " TO TASC-SCRIPT.
           CALL "C$TOLOWER" USING TASC-SCRIPT VALUE 20.

           MOVE "CMD"  TO SCRIPT-TYPE.
           MOVE SPACES TO TCLP-SHELL-PARMS.
           STRING EXT-ACUSQL-DSN-NAME, " ",
                  EXT-ACUSQL-DB-NAME, " ",
                  EXT-ACUSQL-LOGIN, " ",
                  EXT-ACUSQL-PSWD, " ",
                  SQL-TEMP-TABLE, " ",
                  SCRIPT-SQL-FILE, " ", 
                  SCRIPT-RESULT-FILE-PATH, " ",
                  USE-PROC-ID
             DELIMITED BY "  " INTO TCLP-SHELL-PARMS.
           MOVE TCLP-SHELL-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF (STAT-BAD)
               GO TO CALL-SQLCMD-SCRIPT-ERROR.

      * ERROR IF RESULT FILE NOT FOUND
           MOVE "RES"                   TO SCRIPT-TYPE.
           MOVE SCRIPT-RESULT-FILE-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO CALL-SQLCMD-SCRIPT-ERROR.

      * RETRIEVE RESULT FILE COMPLETION MESSAGE
      * IF NOT FOUND, ASSUME ERROR IN SQL
           MOVE "CMD"  TO SCRIPT-TYPE.
           PERFORM RETR-RESULT-MSG.
           IF FOUND-SW = "N"
              MOVE "CMD"  TO SCRIPT-TYPE
              GO TO CALL-SQLCMD-SCRIPT-ERROR.

      * REMOVE RESULT FILE
           MOVE SCRIPT-RESULT-FILE-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           GO TO CALL-SQLCMD-SCRIPT-EXIT.

       CALL-SQLCMD-SCRIPT-ERROR.
           IF SCRIPT-TYPE = "CMD"  
              STRING "ERROR CALLING SQLCMD SCRIPT: ", TASC-SCRIPT
               INTO SCRIPT-ERROR-MSG
           ELSE
              STRING TASC-SCRIPT, " RESULT FILE NOT FOUND" 
               INTO SCRIPT-ERROR-MSG
           END-IF.

           PERFORM SCRIPT-ERROR-HANDLER.
           MOVE "Y" TO SCRIPT-CALL-ERROR-SW.

       CALL-SQLCMD-SCRIPT-EXIT.
           EXIT. 


       PROCESS-LN-FILE SECTION.
           PERFORM TRUNCATE-TEMP-TABLE.

      * LOAD TEMP TABLE USING BCP
           PERFORM SHOW-BCP-MSG.
           PERFORM CALL-BCP-SCRIPT.
           IF SCRIPT-CALL-ERROR-SW = "Y"
              GO TO PROCESS-LN-FILE-EXIT.

      * UPDATE/REWRITE LNFILE TABLE FROM TEMP TABLE
           MOVE "** UPDATE **" TO SQL-TYPE.
           MOVE FILEARRAY-REWRITE-SQL(PROC-SUB) TO SCRIPT-SQL-FILE.
           PERFORM SHOW-SQLCMD-MSG.
           PERFORM CALL-SQLCMD-SCRIPT.
           IF SCRIPT-CALL-ERROR-SW = "Y"
              GO TO PROCESS-LN-FILE-EXIT.

           PERFORM CLEAR-WR-LIST-MSG.

      * CHECK EVERY 5 FILES PROCESSED
      * MAKE SURE LNFILE TRIGGER IS DISABLED.  IF MULTIPLE EOMASC RUNNING OTHER RUN MAY
      *  HAVE TURNED TRIGGER BACK ON IF IT FINISHED FIRST
           IF FILES-CTR2 = 5
              PERFORM DISABLE-LNFILE-TRIGGER
              MOVE 0 TO FILES-CTR2.

       PROCESS-LN-FILE-EXIT.
           EXIT.


       PROCESS-BY-FILE SECTION.
           PERFORM TRUNCATE-TEMP-TABLE.

      * LOAD TEMP TABLE USING BCP
           PERFORM SHOW-BCP-MSG.
           PERFORM CALL-BCP-SCRIPT.
           IF SCRIPT-CALL-ERROR-SW = "Y"
              GO TO PROCESS-BY-FILE-EXIT.

      * UPDATE BYFILE TABLE FROM TEMP TABLE
           MOVE "** UPDATE **" TO SQL-TYPE.
           MOVE FILEARRAY-REWRITE-SQL(PROC-SUB) TO SCRIPT-SQL-FILE.
           PERFORM SHOW-SQLCMD-MSG.
           PERFORM CALL-SQLCMD-SCRIPT.
           IF SCRIPT-CALL-ERROR-SW = "Y"
              GO TO PROCESS-BY-FILE-EXIT.

      * INSERT BYFILE TABLE FROM TEMP TABLE
           MOVE "** INSERT **" TO SQL-TYPE.
           MOVE FILEARRAY-WRITE-SQL(PROC-SUB) TO SCRIPT-SQL-FILE.
           PERFORM CALL-SQLCMD-SCRIPT.
           IF SCRIPT-CALL-ERROR-SW = "Y"
              GO TO PROCESS-BY-FILE-EXIT.

           PERFORM CLEAR-WR-LIST-MSG.

           MOVE SPACES TO SQL-TYPE.

       PROCESS-BY-FILE-EXIT.
           EXIT.


      * READ RCFILE INPUT RECORDS; PERFORM UPDATE/REWRITE SQL
       PROCESS-RC-FILE SECTION.
           MOVE 0   TO MISC-CTR.
           MOVE "Y" TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE SPACE TO TIME-MILITARY-FG.

           PERFORM LOAD-RC1-FILE.
           PERFORM OPEN-RC1-FILE.

           MOVE FILELIST-IN-REC TO RCDATA-IN-PATH.
           PERFORM OPEN-RCDATA-FILE.
           PERFORM READ-RCDATA-FILE-NEXT.

           PERFORM UNTIL IO-BAD
              ADD 1 TO MISC-CTR
              MOVE RCDATA-IN-REC        TO RCDATA-WS-REC
              MOVE RCDATA-IN-BRNO       TO RC-BRNO
              MOVE RCDATA-IN-TRANS-DATE TO RC-TRANS-DATE
              MOVE RCDATA-IN-STATUS     TO RC-STATUS
              MOVE HH-MM-SS             TO RC-EOM-UPDATED-TIME
              ACCEPT RC-EOM-UPDATED-DATE FROM CENTURY-DATE
              PERFORM REWRITE-RC1-FILE
              PERFORM READ-RCDATA-FILE-NEXT
           END-PERFORM.

           PERFORM CLOSE-RCDATA-FILE.
           PERFORM CLOSE-RC1-FILE.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           STRING "   ** UPDATE **  RECORDS PROCESSED: ", MISC-CTR 
             INTO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

       PROCESS-RC-FILE-EXIT.
           EXIT.

      * READ GBFILE INPUT RECORDS; PERFORM UPDATE/REWRITE SQL
       PROCESS-GB-FILE SECTION.
           MOVE 0   TO MISC-CTR.
           MOVE "Y" TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE SPACE TO TIME-MILITARY-FG.

           PERFORM LOAD-GB-FILE.
           PERFORM OPEN-GB-FILE.

           MOVE FILELIST-IN-REC TO GBDATA-IN-PATH.
           PERFORM OPEN-GBDATA-FILE.
           PERFORM READ-GBDATA-FILE-NEXT.

           PERFORM UNTIL IO-BAD
              ADD 1 TO MISC-CTR
              MOVE GBDATA-IN-REC  TO GBDATA-WS-REC
              MOVE GBDATA-IN-BRNO TO GB-BRNO
              MOVE GBDATA-IN-NO   TO GB-NO
              MOVE GBDATA-IN-EOM-TAG-CCYYMM    TO GB-EOM-TAG-CCYYMM
              MOVE GBDATA-IN-EOM-UPDATE-USERID TO GB-EOM-UPDATE-USERID 
              MOVE HH-MM-SS       TO GB-EOM-UPDATE-TIME
              ACCEPT GB-EOM-UPDATE-DATE FROM CENTURY-DATE
              PERFORM REWRITE-GB-FILE
              PERFORM READ-GBDATA-FILE-NEXT
           END-PERFORM.

           PERFORM CLOSE-GBDATA-FILE.
           PERFORM CLOSE-GB-FILE.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           STRING "   ** UPDATE **  RECORDS PROCESSED: ", MISC-CTR 
             INTO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

       PROCESS-GB-FILE-EXIT.
           EXIT.

      *****************************************************************
      *
      *   I N I T I A L I Z A T I O N
      *
      *****************************************************************
       INITIALIZATION SECTION.

           PERFORM GET-GPENV.

           MOVE EOMASC-QUEUE-POS  TO Q-POS.
           MOVE EOMASC-COPIES-POS TO C-POS.
           MOVE EOMASC-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           MOVE EXT-FILPATH-BASE    TO EWD-PATH-BASE
                                       SRF-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO EWD-PATH-MACHINE
                                       SRF-PATH-MACHINE.

      *** MUST DO BEFORE ENTER-PARAMETERS !!!!!!!!
      * CALL MKTEMP TO CREATE FILELIST FILE
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE MKTEMP-BUF TO TCLP-LS-PATH.
           MOVE PROCESS-ID-BUF TO USE-PROC-ID.
           INSPECT USE-PROC-ID REPLACING ALL " " BY "X". 


      * LOAD FILEARRAY WITH FILE TYPE INFORMATION
      *    FILEARRAY-SRCH-STR    -> FILE TYPE - USED TO FIND FILES TO PROCESS (LN, BYA, BYB, GB, RC)
      *    FILEARRAY-WORK-TBL    -> TABLE TO COPY AS TEMP TABLE
      *    FILEARRAY-REWRITE-SQL -> REWRITE/UPDATE SQL 
      *    FILEARRAY-WRITE-SQL   -> WRITE/INSERT SQL (ONLY PERFORMED FOR BYA/BYB)
      *    FILEARRAY-TEMP-TBL    -> TEMP TABLE TO CREATE = WORK TABLE NAME + USE-PROC-ID

      * INITIALIZE FILEARRAY-WRITE-SQL TO SPACES (ONLY USED FOR BYA/BYB)
           PERFORM VARYING PROC-SUB FROM 1 BY 1
            UNTIL PROC-SUB > 5
              MOVE SPACES TO FILEARRAY-WRITE-SQL(PROC-SUB)
           END-PERFORM.

      * LNFILE
           MOVE "LN "               TO FILEARRAY-SRCH-STR(1).
           MOVE "DBO.EOMASC_LNFILE" TO FILEARRAY-WORK-TBL(1).
           MOVE "EOMASC_LNFILE_REWRITE" 
             TO FILEARRAY-REWRITE-SQL(1).

      * BYAFILE
           MOVE "BYA"                TO FILEARRAY-SRCH-STR(2).
           MOVE "DBO.EOMASC_BYAFILE" TO FILEARRAY-WORK-TBL(2).
           MOVE "EOMASC_BYAFILE_REWRITE" 
             TO FILEARRAY-REWRITE-SQL(2).
           MOVE "EOMASC_BYAFILE_WRITE" 
             TO FILEARRAY-WRITE-SQL(2).

      * BYBFILE
           MOVE "BYB"                TO FILEARRAY-SRCH-STR(3).
           MOVE "DBO.EOMASC_BYBFILE" TO FILEARRAY-WORK-TBL(3).
           MOVE "EOMASC_BYBFILE_REWRITE" 
             TO FILEARRAY-REWRITE-SQL(3).
           MOVE "EOMASC_BYBFILE_WRITE" 
             TO FILEARRAY-WRITE-SQL(3).

      * GBFILE
           MOVE "GB "               TO FILEARRAY-SRCH-STR(4).
           MOVE "DBO.EOMASC_GBFILE" TO FILEARRAY-WORK-TBL(4).
           MOVE "EOMASC_GBFILE_REWRITE" 
             TO FILEARRAY-REWRITE-SQL(4).

      * RCFILE
           MOVE "RC "               TO FILEARRAY-SRCH-STR(5).
           MOVE "DBO.EOMASC_RCFILE" TO FILEARRAY-WORK-TBL(5).
           MOVE "EOMASC_RCFILE_REWRITE" 
             TO FILEARRAY-REWRITE-SQL(5).

      * CREATE TEMP TABLE NAME = WORK TABLE NAME + USE-PROC-ID  
           PERFORM VARYING PROC-SUB FROM 1 BY 1
            UNTIL PROC-SUB > 5
              STRING FILEARRAY-WORK-TBL(PROC-SUB), "_", USE-PROC-ID
               DELIMITED BY " " INTO FILEARRAY-TEMP-TBL(PROC-SUB)
           END-PERFORM.

           CALL "C$TOLOWER" USING TASC-EC      VALUE 3.
           CALL "C$TOLOWER" USING TCLP-LS-LS   VALUE 5.
           CALL "C$TOLOWER" USING SRF-PATH-RES VALUE 4.
           CALL "C$TOLOWER" USING TCLP-MV-MV   VALUE 3.

           MOVE "Y" TO NOACCEPT-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.
           PERFORM OPEN-PR-FILE.

           IF EXT-ROUTE-BUF = "00"
      * PERFORM TIMER-START FUNCTION; SET BEG TIME FIELD.
              MOVE ZEROES TO P-TIMER-DAYSEC-BEG
              IF ( P-TIMER-TYPE-SET-VALID )
                  MOVE P-TIMER-TYPE-SET TO P-TIMER-TYPE
              ELSE
                  MOVE "R" TO P-TIMER-TYPE *> REPORT OPTION
              END-IF
              MOVE "GB/TIMER" TO FORM-NAM
              CALL FORM-PROGX USING P-TIMER-LINK-BUF
              CANCEL FORM-PROGX.

      * CREATE SCRIPT RESULT FILE (SRF) PATH
            MOVE USE-PROC-ID             TO SRF-PATH-PROC-ID.
      *     MOVE WS-BR-GR-TEXT           TO SRF-PATH-BEGBRNO.
           MOVE "ALLX"                  TO SRF-PATH-BEGBRNO.
           INSPECT SRF-PATH-BEGBRNO REPLACING ALL " " BY "X".

           MOVE "B"                        TO GB-PATH-OVERRIDE
                                              RC-PATH-OVERRIDE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
      *     EO-EXEC-AUDIT SECTION.
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

       EO-EXEC-EXIT.
           EXIT.

      *****************************************************************
      *    ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *----------------------------------------------------------------
       ENTER-PARM.
           MOVE SPACES TO WR-BUF.
           MOVE WARNING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM RETR-STAT-FILES.

           MOVE "S  A600WARNING." TO VDU.
           MOVE SPACES            TO VDUBUF.

           IF STAT-FILES-OK-IND = "Y"
              MOVE "               ACCEPT TO CONTINUE"     TO VDUBUF
           ELSE
           IF STAT-FILES-OK-IND = "A"
             MOVE "ERROR - MORE THAN 4 EOMALL STATUS FILES FOUND" 
                                                           TO VDUBUF
           ELSE
           IF STAT-FILES-OK-IND = "B"
              MOVE "ERROR - EOMALL NOT COMPLETED"          TO VDUBUF
           ELSE
           IF STAT-FILES-OK-IND = "C"
              MOVE "ERROR - EOMALL STATUS FILES NOT FOUND" TO VDUBUF
           IF STAT-FILES-OK-IND = "D"
              MOVE "ERROR - EOMALL STATUS FILES NOT FOUND" TO VDUBUF
           IF STAT-FILES-OK-IND = "E"
              STRING "ERROR - EOMALL STATUS FILES LIST CONTAINS ",
                     "INVALID FILE NAME"                 INTO VDUBUF
           IF STAT-FILES-OK-IND = "E"
              MOVE "ERROR - EOMALL STATUS CONTAIN MULT EOM DATES" 
                                                           TO VDUBUF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF.

           PERFORM SCREENIO.

           IF STAT-FILES-OK-IND NOT = "Y"
              GO TO EOINIT-SETPR.

      *----------------------------------------------------------------
       ACCEPT-IT.
           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"     GO TO ENDIT.          *> F7
           IF VDUSTATUS = "10"     GO TO EOINIT-SETPR.   *> F1
           IF VDUSTATUS = "1U"     GO TO EOINIT-ENTER-MENU.  *> UP KEY
           IF VDUSTATUS = "1<"     GO TO ACCEPT-IT.          *> F6
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF VDUBUF = "N"         GO TO EOINIT-ENTER-MENU.
           IF VDUBUF NOT = "Y"     GO TO ACCEPT-IT.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

      *----------------------------------------------------------------
       EXIT-PARM.
           EXIT.

      *****************************************************************
      *    DISPLAY-PARAMETERS SECTION.
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *----------------------------------------------------------------
       DISPLAY-PARM.
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

      *****************************************************************
      *
      *    C O P Y    L I B R A R Y    W O R K E R S
      *
      *****************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GBFRSQLLOG.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO EOMASC-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EOMASC-SYSDATE.
           DISPLAY EOMASC-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EOMASC-FORM-FIELDS TO WR-BUF.
           MOVE EOMASC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE EOMASC-HELP--FILE TO HELP-LINK-FILE.
           MOVE EOMASC-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EOMASC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *    W R I T E - D E T A I L - L I N E
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM WRITE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *    P R I N T - R A N G E - L I N E S
      ******************************************************************
       PRINT-RANGE-LINES SECTION.

           MOVE 2 TO LN-FEED.

           MOVE "STARTING TIME      :"    TO RANGE-LINE.
           MOVE STARTING-TIME             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME      :"    TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *    W R I T E - H E A D E R
      ******************************************************************
       WRITE-HEADER SECTION.

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
      
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "="  TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 5 TO LN-COUNT.
           MOVE 1 TO LN-FEED.

      *=================================================================
       WRITE-HEADER-EXIT.
           EXIT.

      *****************************************************************
       MISC-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       RETR-STAT-FILES.
           MOVE "Y" TO STAT-FILES-OK-IND.

           MOVE "S  A500STAT1FILE." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A500STAT2FILE." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A500STAT3FILE." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A500STAT4FILE." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE EOMALL-WORK-DIR-PATH TO TCLP-LS-SRCH-DIR.
           MOVE '/EOMALL-STAT-*'     TO TCLP-LS-FILETYPE.
           MOVE TCLP-LS-COMMAND      TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           PERFORM OPEN-FILELIST-FILE.
           PERFORM READ-FILELIST-FILE-NEXT.

           MOVE 0 TO FILES-TO-PROCESS-CTR.
           PERFORM UNTIL IO-BAD 
              ADD 1       TO FILES-TO-PROCESS-CTR
                             STAT-FILES-ARRAY-CTR
              MOVE SPACES TO STAT-REC MISC-MSG

              MOVE FILELIST-IN-REC TO 
                  STAT-FILES-ARRAY(STAT-FILES-ARRAY-CTR)

              MOVE FILELIST-IN-REC TO STAT-FILE-PATH
              PERFORM OPEN-STAT-FILE
              PERFORM READ-STAT-FILE-NEXT
              PERFORM CLOSE-STAT-FILE

      * FIND FILE NAME WITHOUT DIRECTORY
              PERFORM VARYING PROC-SUB FROM 1 BY 1 
               UNTIL PROC-SUB > 200
                 IF FILELIST-IN-REC(PROC-SUB:11) = "EOMALL-STAT"
                   STRING FILELIST-IN-REC(PROC-SUB:50), " -> ", STAT-REC 
                    DELIMITED BY "  " INTO MISC-MSG 
                   EXIT PERFORM
                 END-IF
              END-PERFORM

              IF MISC-MSG = SPACES
                 MOVE "D" TO STAT-FILES-OK-IND
              ELSE
                 IF FILES-TO-PROCESS-CTR = 1
                    MOVE "S  A500STAT1FILE." TO VDU
                 ELSE
                    IF FILES-TO-PROCESS-CTR = 2
                       MOVE "S  A500STAT2FILE." TO VDU
                    ELSE
                       IF FILES-TO-PROCESS-CTR = 3
                          MOVE "S  A500STAT3FILE." TO VDU
                       ELSE
                          IF FILES-TO-PROCESS-CTR = 4
                             MOVE "S  A500STAT4FILE." TO VDU
                          ELSE
                             MOVE "A" TO STAT-FILES-OK-IND
                             EXIT PERFORM
                          END-IF
                      END-IF
                    END-IF
                 END-IF
                 MOVE MISC-MSG TO VDUBUF
                 PERFORM SCREENIO
      * FIND EOM DATE IN FILE NAME (LAST 6 CHARS AFTER 3RD DASH)
                 MOVE 0 TO DASH-CTR
                 PERFORM VARYING PROC-SUB FROM 1 BY 1 
                  UNTIL PROC-SUB > 200
                    IF MISC-MSG(PROC-SUB:1) = "-"
                       ADD 1 TO DASH-CTR
                       IF DASH-CTR = 3 
                          EXIT PERFORM
                       END-IF
                    END-IF
                 END-PERFORM
                 IF DASH-CTR NOT = 3
                    MOVE "E" TO STAT-FILES-OK-IND
                 ELSE 
                    ADD 1 TO PROC-SUB
                    IF STAT-FILES-EOM = SPACES
                       MOVE MISC-MSG(PROC-SUB:6) TO STAT-FILES-EOM
                    ELSE 
                       IF MISC-MSG(PROC-SUB:6) NOT = STAT-FILES-EOM
                          MOVE "F" TO STAT-FILES-OK-IND
                       END-IF
                    END-IF
                 END-IF
              END-IF

              IF STAT-REC(1:5) NOT = "COMPL"
                 MOVE "B" TO STAT-FILES-OK-IND
              END-IF

              PERFORM READ-FILELIST-FILE-NEXT
           END-PERFORM.

           PERFORM CLOSE-FILELIST-FILE.

           IF FILES-TO-PROCESS-CTR = 0
              MOVE "C" TO STAT-FILES-OK-IND.


      * FIND SCRIPT ERROR MESSAGE. CREATE SQLLOG ENTRY
       SCRIPT-ERROR-HANDLER.
           IF SCRIPT-TYPE NOT = "RES"
              PERFORM FIND-SCRIPT-ERROR-DETAIL-MSG.

           STRING SCRIPT-ERROR-MSG, " | ",
                  SCRIPT-ERROR-DETAIL-MSG
                  DELIMITED BY "  " INTO SQLLOG-FINAL-MSG.
           MOVE "FAIL" TO SQLLOG-STATUS.
           PERFORM CREATE-SQLLOG-ENTRY.


      * FIND SCRIPT ERROR MESSAGE.  LOOK FOR "ERROR" OR "INVALID" IN FIRST 6/7 BYTES
       FIND-SCRIPT-ERROR-DETAIL-MSG.
           PERFORM OPEN-SCRIPT-RESULT-FILE.

           PERFORM READ-SCRIPT-RESULT-FILE-NEXT.
           PERFORM UNTIL RESULT-FILE-EOF-SW = "Y"
              CALL "C$TOUPPER"  USING SCRIPT-RESULT-REC, VALUE 200
              IF SCRIPT-RESULT-REC(1:6) = "ERROR"
                 MOVE 0 TO LAST-BRACKET-POS
                 PERFORM VARYING SUB FROM 1 BY 1 
                  UNTIL SUB > 200
                    IF SCRIPT-RESULT-REC(SUB:1) = "]"
                       MOVE SUB TO LAST-BRACKET-POS
                    END-IF 
                 END-PERFORM
                 EXIT PERFORM 
              ELSE
                 IF SCRIPT-RESULT-REC(1:7) = "INVALID"
                    MOVE SCRIPT-RESULT-REC TO SCRIPT-ERROR-DETAIL-MSG
                    MOVE 0                 TO LAST-BRACKET-POS
                    EXIT PERFORM 
                 ELSE
                    PERFORM READ-SCRIPT-RESULT-FILE-NEXT
                 END-IF
              END-IF 
           END-PERFORM.

           PERFORM CLOSE-SCRIPT-RESULT-FILE.

           IF LAST-BRACKET-POS > 0 AND LAST-BRACKET-POS < 199
               ADD 1 TO LAST-BRACKET-POS
               COMPUTE SUB = 200 - LAST-BRACKET-POS
               MOVE SCRIPT-RESULT-REC(LAST-BRACKET-POS:SUB)
                  TO SCRIPT-ERROR-DETAIL-MSG.

           MOVE SPACES TO MISC-MSG
           STRING SCRIPT-ERROR-MSG, " | ", SCRIPT-ERROR-DETAIL-MSG
             DELIMITED BY "   " INTO MISC-MSG.
           STRING "     ", SCRIPT-TYPE, " RESULT: ", MISC-MSG
             INTO DETAIL-LINE.
           MOVE 1 TO LN-FEED
           PERFORM WRITE-DETAIL-LINE.


       CREATE-SQLLOG-ENTRY.
           IF EXT-ROUTE-BUF NOT = "00"  
           IF EXT-ROUTE-BUF NOT = "70"
              EXIT PARAGRAPH.

           MOVE SPACES TO MESS.
           STRING EXT-EOBUF-PCODE, " ALL"
             DELIMITED BY "  " INTO MESS.

           MOVE FORM-PATHNAME     TO SQLLOG-PROGRAM.
           MOVE MESS              TO SQLLOG-RANGES.
           MOVE SQLLOG-FINAL-STAT TO SQLLOG-STATUS.
           MOVE SQLLOG-FINAL-MSG  TO SQLLOG-MESS.
           PERFORM UPDATE-SQLLOG-FILE.

      * RETRIEVE COMPLETION MESSAGE IN RESULT FILE.
      * IF BCP LOOK FOR "COPIED"
      * ELSE (CMD) LOOK FOR "AFFECTED"
       RETR-RESULT-MSG.
           MOVE "N" TO FOUND-SW.
           PERFORM OPEN-SCRIPT-RESULT-FILE.
           PERFORM READ-SCRIPT-RESULT-FILE-NEXT.
           PERFORM UNTIL IO-BAD
              CALL "C$TOUPPER"  USING SCRIPT-RESULT-REC, VALUE 200
              IF SCRIPT-TYPE = "BCP"
                 PERFORM VARYING SUB FROM 1 BY 1
                    UNTIL SUB > 100
                    IF SCRIPT-RESULT-REC(SUB:6) = "COPIED"
                       MOVE "Y" TO FOUND-SW 
                       EXIT PERFORM
                    END-IF 
                 END-PERFORM
              ELSE
                 PERFORM VARYING SUB FROM 1 BY 1
                    UNTIL SUB > 100
                    IF SCRIPT-RESULT-REC(SUB:8) = "AFFECTED"
                       MOVE "Y" TO FOUND-SW 
                       EXIT PERFORM
                    END-IF 
                 END-PERFORM
              END-IF 
              IF FOUND-SW = "Y"
                 EXIT PERFORM
              END-IF
              PERFORM READ-SCRIPT-RESULT-FILE-NEXT
           END-PERFORM.
           PERFORM CLOSE-SCRIPT-RESULT-FILE.

           IF FOUND-SW = "Y"
              PERFORM GET-TIME
              MOVE CUR-HH TO WS-TIME-DISP-HH
              MOVE CUR-MM TO WS-TIME-DISP-MM
              MOVE CUR-SS TO WS-TIME-DISP-SS
              STRING "     ", SCRIPT-TYPE, " RESULT: ", 
               SCRIPT-RESULT-REC, " ", WS-TIME-DISP, "   ", SQL-TYPE
               DELIMITED BY "      " INTO DETAIL-LINE
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.


      * RETREIVE FILE LIST WITH LINUX COMMMAND INTO TCLP-LS-PATH
       RETR-FILE-LIST.
           MOVE EOMALL-WORK-DIR-PATH TO TCLP-LS-SRCH-DIR.
           MOVE SPACES TO TCLP-LS-FILETYPE.
           STRING  "/EOMALL-", FILEARRAY-SRCH-STR(PROC-SUB), "-*"
             DELIMITED BY " " INTO TCLP-LS-FILETYPE.
           MOVE TCLP-LS-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           MOVE 0 TO FILES-TO-PROCESS-CTR.

           PERFORM OPEN-FILELIST-FILE.
           PERFORM READ-FILELIST-FILE-NEXT.
           PERFORM UNTIL IO-BAD 
              ADD 1 TO FILES-TO-PROCESS-CTR 
              PERFORM READ-FILELIST-FILE-NEXT
           END-PERFORM.
           PERFORM CLOSE-FILELIST-FILE.


      * CREATE TEMP SQL TABLE FROM WORK SQL TABLE WITH "SELECT * INTO..."
       CREATE-TEMP-TABLE.
           MOVE SPACES TO PASS-SQL.
           STRING "SELECT * INTO ", SQL-TEMP-TABLE,
                  " FROM ", FILEARRAY-WORK-TBL(PROC-SUB)
                  " WHERE 1 = 0;"
              DELIMITED BY "  " INTO PASS-SQL.
           PERFORM MISC-SQL-CALL-UPD.
           MOVE "Y" TO SQL-TEMP-TABLE-EXIST-SW.

       CREATE-TEMP-TABLE-INDEX.
           MOVE SPACES TO PASS-SQL.
           IF PROC-SUB = 1 
              STRING "CREATE UNIQUE INDEX PK_EOMASC_", 
               USE-PROC-ID, " ON ", SQL-TEMP-TABLE, 
               "(WRKLN_OWNBR ASC, WRKLN_ACCTNO ASC);" 
               DELIMITED BY "  " INTO PASS-SQL
           ELSE
           IF PROC-SUB = 2
              STRING "CREATE UNIQUE INDEX PK_EOMASC_", 
               USE-PROC-ID, " ON ", SQL-TEMP-TABLE, 
               "(WRKBYA_DATE_CCYYMM ASC, WRKBYA_BRNO ASC,"
               " WRKBYA_CLASS ASC);" 
               DELIMITED BY "  " INTO PASS-SQL
           ELSE
           IF PROC-SUB = 3
              STRING "CREATE UNIQUE INDEX PK_EOMASC_", 
               USE-PROC-ID, " ON ", SQL-TEMP-TABLE, 
               "(WRKBYB_DATE_CCYYMM ASC, WRKBYB_BRNO ASC,"
               " WRKBYB_CLASS ASC);" 
               DELIMITED BY "  " INTO PASS-SQL
           ELSE
           IF PROC-SUB = 4
              STRING "CREATE UNIQUE INDEX PK_EOMASC_", 
               USE-PROC-ID, " ON ", SQL-TEMP-TABLE, 
               "(WRKGB_BRNO ASC, WRKGB_NO ASC);" 
               DELIMITED BY "  " INTO PASS-SQL
           ELSE
           IF PROC-SUB = 5
              STRING "CREATE UNIQUE INDEX PK_EOMASC_", 
               USE-PROC-ID, " ON ", SQL-TEMP-TABLE, 
               "(WRKRC_BRNO ASC, WRKRC_TRANS_DATE ASC);" 
               DELIMITED BY "  " INTO PASS-SQL
           END-IF.

           PERFORM MISC-SQL-CALL-UPD.

       TRUNCATE-TEMP-TABLE.
           MOVE SPACES TO PASS-SQL.
           STRING "TRUNCATE TABLE ", SQL-TEMP-TABLE, ";" 
             DELIMITED BY "  " INTO PASS-SQL.
           PERFORM MISC-SQL-CALL-UPD.

       DROP-TEMP-TABLE.
           MOVE "N"    TO SQL-TEMP-TABLE-EXIST-SW.
           MOVE SPACES TO PASS-SQL.
           STRING "DROP TABLE ", SQL-TEMP-TABLE,";" 
             DELIMITED BY "  " INTO PASS-SQL.
           PERFORM MISC-SQL-CALL-UPD.


       DISABLE-LNFILE-TRIGGER.
           MOVE SPACES TO PASS-SQL.
           MOVE "DISABLE TRIGGER DBO.CT_LNFILE ON DBO.LNFILE;"
             TO PASS-SQL.
           PERFORM MISC-SQL-CALL-UPD.
           MOVE "N" TO TRIGGER-LN-ENABLED-SW.

       ENABLE-LNFILE-TRIGGER.
           MOVE SPACES TO PASS-SQL.
           MOVE "ENABLE TRIGGER DBO.CT_LNFILE ON DBO.LNFILE;"
             TO PASS-SQL.
           PERFORM MISC-SQL-CALL-UPD.
           MOVE "Y" TO TRIGGER-LN-ENABLED-SW.


       CLEAR-WR-LIST-MSG.
           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES    TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       SHOW-SQLCMD-MSG.
           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "PROCESSING SQLCMD: ", FILELIST-IN-REC
                DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       SHOW-BCP-MSG.
           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "PROCESSING BCP: ", FILELIST-IN-REC
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       SHOW-SSP-MSG.
           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "PROCESSING SSP: ", FILELIST-IN-REC
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       UPDATE-PROC-MSG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "S  A600WARNING." TO VDU
              MOVE SPACES TO VDUBUF
              STRING " PROCESSING ",FILEARRAY-SRCH-STR(PROC-SUB), 
               "FILE: (", PROC-FILES-LINE-CTR1, "/"
               PROC-FILES-LINE-CTR2,")"
               DELIMITED BY "    " INTO VDUBUF
              PERFORM SCREENIO.

       CLEAR-PROC-MSG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "S  A600WARNING." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

      *****************************************************************
      *
      *    I O - R O U T I N E
      *
      *****************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBSQLLOGIN.CPY".

      * COPYMEMBER: LIBGB/GBGBIN
       LOAD-GB-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO GB-ALL-DATA
              MOVE GB-ALL-PATH             TO GB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GB-ALL-BASE
              MOVE FILPATH-MACHINE         TO GB-ALL-MACHINE
              MOVE FILPATH-DATA            TO GB-ALL-DATA
              MOVE GB-ALL-PATH             TO GB-PATH.

      *-----------------------------------------------------------------
       OPEN-GB-FILE.
           PERFORM LOAD-GB-FILE.
           PERFORM OPEN-IT.
           MOVE GB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       REWRITE-GB-FILE.

           ACCEPT GB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GB-PATH-SQL    TO E-FILE.
           MOVE GB-KEY         TO E-KEYX.
           PERFORM SET-GB-FIELDS.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.GBFILE
             SET
              GBFILE.GB_LTOUCH_DATE       = :QGB-LTOUCH-DATE,
              GBFILE.GB_EOM_TAG_CCYYMM    = :QGB-EOM-TAG-CCYYMM,
              GBFILE.GB_EOM_UPDATE_DATE   = :QGB-EOM-UPDATE-DATE,
              GBFILE.GB_EOM_UPDATE_TIME   = :QGB-EOM-UPDATE-TIME,
              GBFILE.GB_EOM_UPDATE_USERID = :QGB-EOM-UPDATE-USERID
             WHERE GBFILE.GB_BRNO = :QGB-BRNO
               AND GBFILE.GB_NO   = :QGB-NO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GB-FILE.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-GB-FILE.

           PERFORM CLOSE-IT.


      * COPYMEMBER: LIBGB/GBRC1IN
       LOAD-RC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RC-ALL-BASE
              MOVE FILPATH-MACHINE         TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH.

      *-----------------------------------------------------------------
       OPEN-RC1-FILE.
           PERFORM LOAD-RC1-FILE.
           PERFORM OPEN-IT.
           MOVE RC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       REWRITE-RC1-FILE.
           ACCEPT RC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY     TO E-KEYX.
           PERFORM SET-RC-FIELDS.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.RCFILE
             SET
              RCFILE.RC_STATUS           = :QRC-STATUS,
              RCFILE.RC_LTOUCH_DATE      = :QRC-LTOUCH-DATE,
              RCFILE.RC_EOM_UPDATED_DATE = :QRC-EOM-UPDATED-DATE,
              RCFILE.RC_EOM_UPDATED_TIME = :QRC-EOM-UPDATED-TIME
            WHERE RCFILE.RC_BRNO       = :QRC-BRNO
              AND RCFILE.RC_TRANS_DATE = :QRC-TRANS-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-RC1-FILE.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-RC1-FILE.
           PERFORM CLOSE-IT.

      *-------------------------------------------------------------------------------*
      * PERFORM SQL IN PASS-SQL.                                                      *
      *  THIS IS DONE BY TRICKING ACUSQL THAT THE SQL IS A CURSOR - WHICH IT IS NOT.  *
      *  THE "OPEN CURSOR" WILL ACTUALLY EXECUTE THE SQL (SQL WILL NOT CONTAIN A      * 
      *  "SELECT"!!!!!).  NEED TO PERFORM A "CLOSE CURSOR" SINCE ACUSQL THINKS THERE  *
      *  IS A CURSOR OPENED.                                                          *
      *  CURRENTLY, THE SQLS WILL BE ONE OF THE FOLLOWING: CREATE TABLE, CREATE INDEX,*
      *    TRUNCATE TABLE, DROP TABLE, DISABLE/ENABLE LNFILE TRIGGER.                 *
      *-------------------------------------------------------------------------------*
       MISC-SQL-CALL-UPD.
           PERFORM START-IT.
           MOVE SQL-TEMP-TABLE TO E-FILE.
           MOVE SPACES         TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DECLARE MISC1_CURSOR CURSOR FOR
                :MISC-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                 END-EXEC.

           EXEC SQL
               OPEN MISC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL
                CLOSE MISC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *=================================================================
       OPEN-FILELIST-FILE.
           PERFORM OPEN-IT.
           MOVE TCLP-LS-PATH TO E-FILE.
           OPEN INPUT FILELIST-IN-FILE.
           MOVE "N" TO FILELIST-EOF-SW.
       READ-FILELIST-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TCLP-LS-PATH TO E-FILE.
           MOVE SPACES TO FILELIST-IN-REC.
           READ FILELIST-IN-FILE NEXT.
           IF ( IO-LOCKED )
              GO TO READ-FILELIST-FILE-NEXT.
           IF IO-BAD
              MOVE "Y" TO FILELIST-EOF-SW.
       CLOSE-FILELIST-FILE.
           PERFORM CLOSE-IT.
           CLOSE FILELIST-IN-FILE.


       OPEN-SCRIPT-RESULT-FILE.
           PERFORM OPEN-IT.
           MOVE SCRIPT-RESULT-FILE-PATH TO E-FILE.
           OPEN INPUT SCRIPT-RESULT-FILE.
           MOVE "N" TO RESULT-FILE-EOF-SW.
       READ-SCRIPT-RESULT-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SCRIPT-RESULT-FILE-PATH TO E-FILE.
           MOVE SPACES TO SCRIPT-RESULT-REC.
           READ SCRIPT-RESULT-FILE NEXT
             AT END MOVE "Y" TO RESULT-FILE-EOF-SW
           END-READ.
           IF ( IO-LOCKED )
              GO TO READ-SCRIPT-RESULT-FILE-NEXT.
       CLOSE-SCRIPT-RESULT-FILE.
           PERFORM CLOSE-IT.
           CLOSE SCRIPT-RESULT-FILE.


       OPEN-STAT-FILE.
           PERFORM OPEN-IT.
           MOVE STAT-FILE-PATH TO E-FILE.
           OPEN INPUT STAT-FILE.
       READ-STAT-FILE-NEXT.
           PERFORM READ-IT.
           MOVE STAT-FILE-PATH TO E-FILE.
           MOVE SPACES TO STAT-REC.
           READ STAT-FILE NEXT.
           IF ( IO-LOCKED )
              GO TO READ-STAT-FILE-NEXT.
       CLOSE-STAT-FILE.
           PERFORM CLOSE-IT.
           CLOSE STAT-FILE.


       OPEN-GBDATA-FILE.
           PERFORM OPEN-IT.
           MOVE GBDATA-IN-PATH TO E-FILE.
           OPEN INPUT GBDATA-IN-FILE.
       READ-GBDATA-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GBDATA-IN-PATH TO E-FILE.
           MOVE SPACES TO STAT-REC.
           READ GBDATA-IN-FILE NEXT.
           IF ( IO-LOCKED )
              GO TO READ-GBDATA-FILE-NEXT.
       CLOSE-GBDATA-FILE.
           PERFORM CLOSE-IT.
           CLOSE GBDATA-IN-FILE.

       OPEN-RCDATA-FILE.
           PERFORM OPEN-IT.
           MOVE RCDATA-IN-PATH TO E-FILE.
           OPEN INPUT RCDATA-IN-FILE.
       READ-RCDATA-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RCDATA-IN-PATH TO E-FILE.
           MOVE SPACES TO STAT-REC.
           READ RCDATA-IN-FILE NEXT.
           IF ( IO-LOCKED )
              GO TO READ-RCDATA-FILE-NEXT.
       CLOSE-RCDATA-FILE.
           PERFORM CLOSE-IT.
           CLOSE RCDATA-IN-FILE.

      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *****************************************************************
      *
      *    E N D     P R O G R A M
      *
      *****************************************************************
       END-PROGRAM SECTION.
       END-RUN.

      * STOPPING TIMER AND DISPLAYING WINDOW IF NOT BATCH - S35Q-104
           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

           PERFORM CLOSE-FILES.

      *----------------------------------------------------------------
       EXIT-PROG.
 
      * NEED TO MAKE SURE LNFILE TRIGGER IS RE-ENABLED !!!!!
           IF EXT-ACUSQL-CONNECT-STAT-GOOD
              IF TRIGGER-LN-ENABLED-SW = "N"
                 PERFORM ENABLE-LNFILE-TRIGGER.

           IF SQL-TEMP-TABLE-EXIST-SW = "Y"
              PERFORM DROP-TEMP-TABLE.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME
              PERFORM CLEAR-PROC-MSG.


       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EOMASC-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
