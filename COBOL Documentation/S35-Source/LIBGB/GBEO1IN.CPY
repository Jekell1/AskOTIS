      ******************************************************************
      * COPYMEMBER: LIBGB/GBEO1IN
      *=================================================================
      * REV:
      * JKC 2024-0217 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       LOAD-EO1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( EO-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( EO-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO EO-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO EO-ALL-MACHINE
              MOVE EO-ALL-PATH             TO EO-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( EO-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO EO-ALL-BASE
              MOVE FILPATH-MACHINE         TO EO-ALL-MACHINE
              MOVE EO-ALL-PATH             TO EO-PATH.

      *-----------------------------------------------------------------
       OPEN-EO1-FILE.
           PERFORM LOAD-EO1-FILE.
           PERFORM OPEN-IT.
           MOVE EO-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-EO1-FILE.
           PERFORM START-IT.
           MOVE EO-PATH-SQL TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.

           IF ( EO1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-EO1-FILE.

           IF ( QEO1-WEND-STEP = ZEROES    ) OR
              ( QEO1-WEND-STEP NOT NUMERIC )
              MOVE EO-PCODE             TO QEO1-WBEG-PCODE
              MOVE ALL "Z"              TO QEO1-WEND-PCODE
              MOVE EO-STEP              TO QEO1-WBEG-STEP
              MOVE ALL "9"              TO QEO1-WEND-STEP.

           EXEC SQL
            DECLARE EO1_CURSOR CURSOR FOR
            SELECT EOFILE.EO_PCODE,
                   EOFILE.EO_STEP,
                   CAST(EOFILE.EO_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   EOFILE.EO_DIR,
                   EOFILE.EO_PROGRAM,
                   EOFILE.EO_COPIES,
                   EOFILE.EO_COMMENT,
                   EOFILE.EO_MESS,
                   CAST(EOFILE.EO_START_DATE
                                                  AS VARCHAR(10)),
                   EOFILE.EO_START_TIME,
                   CAST(EOFILE.EO_COMPLETE_DATE
                                                  AS VARCHAR(10)),
                   EOFILE.EO_COMPLETE_TIME,
                   EOFILE.EO_COMPLETE_STAT,
                   EOFILE.EO_PARAMETERS,
                   EOFILE.EO_ERROR_STEP_X,
                   EOFILE.EO_TRANSFER,
                   EOFILE.EO_LEVEL
            FROM DBO.EOFILE
            WHERE EOFILE.EO_PCODE >= :QEO1-WBEG-PCODE
              AND EOFILE.EO_PCODE <= :QEO1-WEND-PCODE
              AND EOFILE.EO_STEP >= :QEO1-WBEG-STEP
              AND EOFILE.EO_STEP <= :QEO1-WEND-STEP
            ORDER BY EOFILE.EO_PCODE,
                     EOFILE.EO_STEP
           END-EXEC.

           EXEC SQL
               OPEN EO1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QEO1-WBEG-PCODE
                          QEO1-WEND-PCODE.

           MOVE ZEROES TO QEO1-WBEG-STEP
                          QEO1-WEND-STEP.

           MOVE STAT---GOOD TO EO1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-EO1-FILE-GREATER.
           PERFORM START-IT.
           MOVE EO-PATH-SQL TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.

           IF ( EO1-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-EO1-FILE.

           IF ( QEO1-WEND-STEP = ZEROES    ) OR
              ( QEO1-WEND-STEP NOT NUMERIC )
              MOVE EO-PCODE             TO QEO1-WBEG-PCODE
              MOVE ALL "Z"              TO QEO1-WEND-PCODE
              MOVE EO-STEP              TO QEO1-WBEG-STEP
              MOVE ALL "9"              TO QEO1-WEND-STEP.

           EXEC SQL
            DECLARE EO1_CURSOR_GR CURSOR FOR
            SELECT EOFILE.EO_PCODE,
                   EOFILE.EO_STEP,
                   CAST(EOFILE.EO_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   EOFILE.EO_DIR,
                   EOFILE.EO_PROGRAM,
                   EOFILE.EO_COPIES,
                   EOFILE.EO_COMMENT,
                   EOFILE.EO_MESS,
                   CAST(EOFILE.EO_START_DATE
                                                  AS VARCHAR(10)),
                   EOFILE.EO_START_TIME,
                   CAST(EOFILE.EO_COMPLETE_DATE
                                                  AS VARCHAR(10)),
                   EOFILE.EO_COMPLETE_TIME,
                   EOFILE.EO_COMPLETE_STAT,
                   EOFILE.EO_PARAMETERS,
                   EOFILE.EO_ERROR_STEP_X,
                   EOFILE.EO_TRANSFER,
                   EOFILE.EO_LEVEL
            FROM DBO.EOFILE
      * WONT WORK WITH PCODE >, HAS TO BE >=
            WHERE EOFILE.EO_PCODE >= :QEO1-WBEG-PCODE
              AND EOFILE.EO_PCODE <= :QEO1-WEND-PCODE
              AND EOFILE.EO_STEP > :QEO1-WBEG-STEP
              AND EOFILE.EO_STEP <= :QEO1-WEND-STEP
            ORDER BY EOFILE.EO_PCODE,
                     EOFILE.EO_STEP
           END-EXEC.

           EXEC SQL
               OPEN EO1_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QEO1-WBEG-PCODE
                          QEO1-WEND-PCODE.

           MOVE ZEROES TO QEO1-WBEG-STEP
                          QEO1-WEND-STEP.

           MOVE STAT---GOOD TO EO1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-EO1-FILE.
           PERFORM READ-IT.
           MOVE EO-PATH-SQL TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.
           INITIALIZE QEO-REC.

           MOVE EO-PCODE    TO QEO-PCODE. 
           MOVE EO-STEP     TO QEO-STEP.

           EXEC SQL
            SELECT EOFILE.EO_PCODE,
                   EOFILE.EO_STEP,
                   CAST(EOFILE.EO_LTOUCH_DATE     AS VARCHAR(10)),
                   EOFILE.EO_DIR,
                   EOFILE.EO_PROGRAM,
                   EOFILE.EO_COPIES,
                   EOFILE.EO_COMMENT,
                   EOFILE.EO_MESS,
                   CAST(EOFILE.EO_START_DATE      AS VARCHAR(10)),
                   EOFILE.EO_START_TIME,
                   CAST(EOFILE.EO_COMPLETE_DATE   AS VARCHAR(10)),
                   EOFILE.EO_COMPLETE_TIME,
                   EOFILE.EO_COMPLETE_STAT,
                   EOFILE.EO_PARAMETERS,
                   EOFILE.EO_ERROR_STEP_X,
                   EOFILE.EO_TRANSFER,
                   EOFILE.EO_LEVEL
            INTO :QEO-REC
            FROM DBO.EOFILE
            WHERE EOFILE.EO_PCODE = :QEO-PCODE
              AND EOFILE.EO_STEP = :QEO-STEP
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EO1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-EO-FIELDS.

      *-----------------------------------------------------------------
       READ-EO1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE EO-PATH-SQL TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.
           INITIALIZE QEO-REC.

           IF ( EO1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH EO1_CURSOR INTO :QEO-REC
              END-EXEC
           ELSE
           IF ( EO1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH EO1_CURSOR_GR INTO :QEO-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EO1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-EO-FIELDS.

           IF ( IO-GOOD               ) AND
              ( EXT-TCLP-TRACE-BRANCH )
              STRING "READ EO | CODE = ", EO-PCODE,
                     " | STEP = ", EO-STEP,
                     " | PROGRAM = ", EO-PROGRAM
                     DELIMITED BY SIZE INTO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

      *-----------------------------------------------------------------
       WRITE-EO1-FILE.

           ACCEPT EO-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE EO-PATH-SQL TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.
           PERFORM SET-EO-FIELDS.

           EXEC SQL
            INSERT INTO DBO.EOFILE
             (EOFILE.EO_PCODE,
              EOFILE.EO_STEP,
              EOFILE.EO_LTOUCH_DATE,
              EOFILE.EO_DIR,
              EOFILE.EO_PROGRAM,
              EOFILE.EO_COPIES,
              EOFILE.EO_COMMENT,
              EOFILE.EO_MESS,
              EOFILE.EO_START_DATE,
              EOFILE.EO_START_TIME,
              EOFILE.EO_COMPLETE_DATE,
              EOFILE.EO_COMPLETE_TIME,
              EOFILE.EO_COMPLETE_STAT,
              EOFILE.EO_PARAMETERS,
              EOFILE.EO_ERROR_STEP_X,
              EOFILE.EO_TRANSFER,
              EOFILE.EO_LEVEL)
            VALUES
             (:QEO-PCODE,
              :QEO-STEP,
              :QEO-LTOUCH-DATE,
              :QEO-DIR,
              :QEO-PROGRAM,
              :QEO-COPIES,
              :QEO-COMMENT,
              :QEO-MESS,
              :QEO-START-DATE,
              :QEO-START-TIME,
              :QEO-COMPLETE-DATE,
              :QEO-COMPLETE-TIME,
              :QEO-COMPLETE-STAT,
              :QEO-PARAMETERS,
              :QEO-ERROR-STEP-X,
              :QEO-TRANSFER,
              :QEO-LEVEL)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-EO1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-EO1-FILE.

           ACCEPT EO-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE EO-PATH-SQL TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.
           PERFORM SET-EO-FIELDS.

           EXEC SQL
            UPDATE DBO.EOFILE
             SET
              EOFILE.EO_PCODE = :QEO-PCODE,
              EOFILE.EO_STEP = :QEO-STEP,
              EOFILE.EO_LTOUCH_DATE = :QEO-LTOUCH-DATE,
              EOFILE.EO_DIR = :QEO-DIR,
              EOFILE.EO_PROGRAM = :QEO-PROGRAM,
              EOFILE.EO_COPIES = :QEO-COPIES,
              EOFILE.EO_COMMENT = :QEO-COMMENT,
              EOFILE.EO_MESS = :QEO-MESS,
              EOFILE.EO_START_DATE = :QEO-START-DATE,
              EOFILE.EO_START_TIME = :QEO-START-TIME,
              EOFILE.EO_COMPLETE_DATE = :QEO-COMPLETE-DATE,
              EOFILE.EO_COMPLETE_TIME = :QEO-COMPLETE-TIME,
              EOFILE.EO_COMPLETE_STAT = :QEO-COMPLETE-STAT,
              EOFILE.EO_PARAMETERS = :QEO-PARAMETERS,
              EOFILE.EO_ERROR_STEP_X = :QEO-ERROR-STEP-X,
              EOFILE.EO_TRANSFER = :QEO-TRANSFER,
              EOFILE.EO_LEVEL = :QEO-LEVEL
             WHERE EOFILE.EO_PCODE = :QEO-PCODE
               AND EOFILE.EO_STEP = :QEO-STEP
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-EO1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-EO1-FILE.

           PERFORM DELETE-IT.
           MOVE EO-PATH-SQL TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.
           PERFORM SET-EO-FIELDS.

           EXEC SQL
            DELETE FROM DBO.EOFILE
             WHERE EOFILE.EO_PCODE = :QEO-PCODE
               AND EOFILE.EO_STEP = :QEO-STEP
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-EO1-FILE.

      *-----------------------------------------------------------------
       CLOSE-EO1-FILE.

           PERFORM CLOSE-IT.

           IF ( EO1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE EO1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO EO1-CURSOR-STAT.

           IF ( EO1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE EO1_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO EO1-CURSOR-GR-STAT.

