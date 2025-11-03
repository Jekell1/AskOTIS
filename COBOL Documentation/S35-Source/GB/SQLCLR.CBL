       IDENTIFICATION DIVISION. 
       PROGRAM-ID.    SQLCLR.
       AUTHOR.        RON ZAMISKA.
       DATE-WRITTEN.  11-21-2024.
      ***************************************************************** 
      *  
      *  DESCRIPTION: CLEAR SQL CACHE FROM SQL SERVER   S35Q-104
      *  APPLIES CHANGES/INSERTS INTO SQL TABLE - DBO._CONTROL                  
      *  
      * TO INCREASE PERFORMANCE TO OTHER PROGRAMS, THIS PROGRAM WILL SET THE
      *   COLUMN CONTROLVALUE TO A '1' SO THAT AN SQL TRIGGER WILL EXECUTE
      *   AND CLEAR THE CACHE ON TABLE DBO.CONTROL.  ONCE THE CACHE IS 
      *   CLEARED AND COLUMN IS SET BACK TO A '0', THIS PROGRAM WILL END 
      *   NORMALLY.  IF FOR SOME REASON THE COLUMN IS NOT RESET BACK TO
      *   '0', AN ERROR MESSAGE WILL APPEAR AFTER WAITING APPROXIMATELY
      *   70 SECONDS (SLEEPING 7 SECONDS UP TO 10 TIMES).     
      *
      *=================================================================
      * REV:
      * RMZ 2024-1126 S35Q-104 ***NEW*** 
      * JKC 2025-0220 FIXED THE UPDATE OF PASS/FAIL TO SQLLOG FOR BATCH
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSSQLLOG.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDSQLLOG.CPY".

      *****************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/SQLCLR.CBL S35 02/20/25-13:33:33 >".

       COPY "LIBGB/GBWSSQLLOG.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  CONTROLFIELD           PIC X(32).
       01  CONTROLVALUE           PIC X(128).
       01  CONTROL-LASTUPDATED    PIC X(19).
       01  CONTROL-MESSAGE        PIC X(128).
       EXEC SQL END DECLARE SECTION END-EXEC.

      *****************************************************************
      *    MISC WORKERS
      *****************************************************************
       01  MESS-LIST               PIC X(05)      VALUE "MESS.".
       01  LEGEND                  PIC X(70)      VALUE SPACES.
       01  SUB                     PIC 9(06)      VALUE 0.

       01  TCLP-SLEEP-COMMAND. 
          03  TCLP-SLEEP           PIC X(08)      VALUE "SLEEP 7 ".
 
       01  WARNING-LIST            PIC X(08)      VALUE "WARNING.".

      *****************************************************************
      *     S C R E E N   F I E L D S   *
      *****************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
 
       01 VERSION-CONTROL          PIC X VALUE "A".

      *****************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      *****************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/SQLCLR_DEF.CPY".
       COPY "LIBGB/SQLCLR_WKS.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGB/SQLCLR_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 SQLLOG-FILE.
       COPY "LIBGB/DECLRP.CPY".
           CLOSE SQLLOG-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************************************************
      *
      *    M A I N    M O D U L E
      *
      *****************************************************************
       MAIN-MODULE SECTION.
      D    STOP MESS.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SQLCLR" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM SQLCLR-UPDATE.

       END-ROUTINE.
           GO TO END-PROGRAM.

      *****************************************************************
       SQLCLR-UPDATE SECTION.
      *****************************************************************

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              MOVE "PROCESSING IN PROGRESS....." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-CONTROL-FILE.
           PERFORM READ-CONTROL-FILE.
 
            IF IO-GOOD
              MOVE "1"                     TO CONTROLVALUE
              MOVE "CLEAR CACHE REQUESTED" TO CONTROL-MESSAGE
              PERFORM REWRITE-CONTROL-FILE
           ELSE 
              MOVE "DROPCLEANBUFFERS"      TO CONTROLFIELD
              MOVE "1"                     TO CONTROLVALUE
              MOVE "CLEAR CACHE REQUESTED" TO CONTROL-MESSAGE
              PERFORM WRITE-CONTROL-FILE.
 
      * READ CONTROL FILE UNTIL CONTROVALUE = O OR YOU TRIED 10 TIMES.
           PERFORM VARYING SUB FROM 1 BY 1 
            UNTIL SUB > 10 OR 
                  (IO-GOOD AND CONTROLVALUE = "0")
              MOVE " " TO CONTROLVALUE
              PERFORM READ-CONTROL-FILE   
              IF (IO-GOOD AND CONTROLVALUE = "0")
                 NEXT SENTENCE
              ELSE
                 CALL "C$TOLOWER" USING TCLP-SLEEP  VALUE 8
                 MOVE TCLP-SLEEP-COMMAND       TO SYSTEM-BUF
                 PERFORM SYSTEM-CALL
              END-IF
           END-PERFORM.

           MOVE SPACES TO WR-BUF
           MOVE MESS-LIST TO WR-LIST
           PERFORM CALL-WRFORM.

           MOVE "S  A600WARNING."              TO VDU.

           IF (IO-GOOD AND CONTROLVALUE = "0") 
              PERFORM MISC-UPDATE-SQLLOG-PASS
              MOVE "     CLEAR CACHE COMPLETED - PRESS ENTER TO EXIT" 
                TO VDUBUF
           ELSE
              PERFORM MISC-UPDATE-SQLLOG-FAIL
              MOVE "     CLEAR CACHE FAILED - PRESS ENTER TO EXIT"
                TO VDUBUF.

           IF EXT-ROUTE-BUF NOT = "00"
              GO TO EOM-UPDATE-EXIT.

           PERFORM SCREENIO.

           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.

       EOM-UPDATE-EXIT.
           EXIT. 

      *****************************************************************
      *
      *   I N I T I A L I Z A T I O N
      *
      *****************************************************************
       INITIALIZATION SECTION.

           PERFORM GET-GPENV.

           MOVE SQLCLR-QUEUE-POS  TO Q-POS.
           MOVE SQLCLR-COPIES-POS TO C-POS.
           MOVE SQLCLR-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
 
           CALL "C$TOLOWER" USING TCLP-SLEEP   VALUE 8.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES            TO WR-BUF.
           MOVE WARNING-LIST      TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
      *     EO-EXEC-AUDIT SECTION.
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

      *****************************************************************
      *    ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARAMETERS SECTION.

      *****************************************************************
      *    DISPLAY-PARAMETERS SECTION.
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

      *****************************************************************
      *
      *    C O P Y    L I B R A R Y    W O R K E R S
      *
      *****************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GBFRSQLLOG.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME  TO SQLCLR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SQLCLR-SYSDATE.
           DISPLAY SQLCLR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SQLCLR-FORM-FIELDS TO WR-BUF.
           MOVE SQLCLR-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE SQLCLR-HELP--FILE TO HELP-LINK-FILE.
           MOVE SQLCLR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/SQLCLR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
 
      *-----------------------------------------------------------------
       MISC-UPDATE-SQLLOG-FAIL.

           MOVE SPACES TO MESS
                          SQLLOG-REC.

           IF ( EXT-ROUTE-BUF = "00" )
              STRING "ZZZ",
                     " 0000~9999 9999",
                     DELIMITED BY "  " INTO MESS
           ELSE
              STRING EXT-EOBUF-PCODE,
                     " 0000~9999 9999",
                     DELIMITED BY "  " INTO MESS.

           MOVE FORM-PATHNAME            TO SQLLOG-PROGRAM.
           MOVE MESS                     TO SQLLOG-RANGES.
           MOVE "FAIL"                   TO SQLLOG-STATUS.
           MOVE "CLEAR SQL SERVER CACHE" TO SQLLOG-MESS.
           PERFORM UPDATE-SQLLOG-FILE.

      *-----------------------------------------------------------------
       MISC-UPDATE-SQLLOG-PASS.

           MOVE SPACES TO MESS
                          SQLLOG-REC.

           IF ( EXT-ROUTE-BUF = "00" )
              STRING "ZZZ",
                     " 0000~9999 9999",
                     DELIMITED BY "  " INTO MESS
           ELSE
              STRING EXT-EOBUF-PCODE,
                     " 0000~9999 9999",
                     DELIMITED BY "  " INTO MESS.

           MOVE FORM-PATHNAME            TO SQLLOG-PROGRAM.
           MOVE MESS                     TO SQLLOG-RANGES.
           MOVE "PASS"                   TO SQLLOG-STATUS.
           MOVE "CLEAR SQL SERVER CACHE" TO SQLLOG-MESS.
           PERFORM UPDATE-SQLLOG-FILE.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND    TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************************
      *
      *    I O - R O U T I N E
      *
      *****************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBSQLLOGIN.CPY".

      *-----------------------------------------------------------------
       OPEN-CONTROL-FILE.
           PERFORM OPEN-IT.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-CONTROL-FILE.
           PERFORM READ-IT. 
           MOVE "SQL-CONTROL" TO E-FILE.
           MOVE SPACES        TO E-KEYX.
           STRING CONTROLFIELD, "-", CONTROLVALUE
                  DELIMITED BY "  " INTO E-KEYX.

           EXEC SQL
            SELECT CONTROLFIELD,
                   CONTROLVALUE,
                   LASTUPDATED,
                   MESSAGE 
              INTO 
                  :CONTROLFIELD,
                  :CONTROLVALUE,
                  :CONTROL-LASTUPDATED
                  :CONTROL-MESSAGE
            FROM DBO.CONTROL
            WHERE CONTROLFIELD = 'DROPCLEANBUFFERS' 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CONTROL-FILE.

      *-----------------------------------------------------------------
       WRITE-CONTROL-FILE.

           PERFORM WRITE-IT. 
           MOVE "SQL-CONTROL" TO E-FILE.
           MOVE SPACES        TO E-KEYX.
           STRING CONTROLFIELD, "-", CONTROLVALUE
                  DELIMITED BY "  " INTO E-KEYX.

           EXEC SQL
            INSERT INTO DBO.CONTROL 
             (CONTROLFIELD,
              CONTROLVALUE,
              LASTUPDATED,
              MESSAGE)     
            VALUES
             (:CONTROLFIELD, 
              :CONTROLVALUE,
               CURRENT_TIMESTAMP,  
              :CONTROL-MESSAGE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-CONTROL-FILE.

      *-----------------------------------------------------------------
       REWRITE-CONTROL-FILE.
 
           PERFORM REWRITE-IT. 
           MOVE "SQL-CONTROL" TO E-FILE.
           MOVE SPACES        TO E-KEYX.
           STRING CONTROLFIELD, "-", CONTROLVALUE
                  DELIMITED BY "  " INTO E-KEYX.

           EXEC SQL
            UPDATE DBO.CONTROL
               SET
                 CONTROLVALUE   = '1',
                 LASTUPDATED    = CURRENT_TIMESTAMP,
                 MESSAGE        = :CONTROL-MESSAGE 
             WHERE CONTROLFIELD = 'DROPCLEANBUFFERS'
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CONTROL-FILE.

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

      *----------------------------------------------------------------
       EXIT-PROG.
 
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SQLCLR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
