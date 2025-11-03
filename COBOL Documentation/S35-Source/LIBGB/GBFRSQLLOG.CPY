      * COPYMEMBER: LIBGB/GBFRSQLLOG
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/GBFRSQLLOG.CPY
      *
      * DESCRIPTION   : FILE ROUTINES FOR SQLLOG
      *
      *=================================================================
      * REV:
      * JKC 2024-1009 ***NEW***
      *
      ******************************************************************
      *
      *   U P D A T E - S Q L L O G 
      *
      *=================================================================
      * IN  : SQLLOG-MESS
      *       SQLLOG-PROGRAM    [/USR/X##/CO/??????]
      *       SQLLOG-RANGES     [??? ####~#### ####]
      *       SQLLOG-STATUS     [FAIL/PASS]
      * OUT : SQLLOG-REC
      * DESC: UPDATE THE CURRENT DATE'S SQLLOG FILE.
      ******************************************************************
       UPDATE-SQLLOG-FILE SECTION.

           MOVE SPACES                     TO SQLLOG-REC.

           ACCEPT SQLLOG-DATE-YYMMDD     FROM DATE.
           MOVE SQLLOG-DATE-YYMMDD         TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD              TO SQLLOG-DATE-YYYY-MM-DD.
           INSPECT SQLLOG-DATE-YYYY-MM-DD REPLACING ALL "/" BY "-".

           MOVE "Y"                        TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE HH-MM-SS                   TO SQLLOG-TIME.
           INSPECT SQLLOG-TIME REPLACING ALL "/" BY ":".

           MOVE SQLLOG-PROGRAM             TO SQLLOG-PROGRAM-NAME.
           MOVE SQLLOG-RANGES              TO SQLLOG-PROGRAM-RANGES.
           MOVE SQLLOG-STATUS              TO SQLLOG-STATUS-CODE.
           MOVE SQLLOG-MESS                TO SQLLOG-DESCRIPTION.
           MOVE EXT-ACUSQL-DB-NAME         TO SQLLOG-DB-NAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM LOAD-SQLLOG-FILE.
           MOVE SQLLOG-PATH                TO ACCESS-BUF.
           PERFORM ACCESS-CALL.

           IF ( STAT-BAD )
              PERFORM OPEN-SQLLOG-FILE-OUTPUT
              PERFORM CLOSE-SQLLOG-FILE.
             
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM OPEN-SQLLOG-FILE-EXTEND.
           PERFORM WRITE-SQLLOG-FILE.
           PERFORM CLOSE-SQLLOG-FILE.

      *=================================================================
       UPDATE-SQLLOG-FILE-EXIT.
           EXIT.
