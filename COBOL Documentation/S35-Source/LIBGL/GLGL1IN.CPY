      * COPYMEMBER: LIBGL/GLGL1IN
       LOAD-GL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GL-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GL-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GL-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GL-ALL-BASE
              MOVE FILPATH-MACHINE         TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH.

      *-----------------------------------------------------------------
       OPEN-GL1-FILE.
           PERFORM LOAD-GL1-FILE.
           PERFORM OPEN-IT.
           MOVE GL-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GL1-FILE.
           PERFORM START-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.

           IF ( GL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GL1-FILE.

           IF ( QGL1-WEND-ACCOUNT = ZEROES    ) OR
              ( QGL1-WEND-ACCOUNT NOT NUMERIC )
              MOVE GL-BRANCH        TO QGL1-WBEG-BRANCH
              MOVE GL-ACCOUNT       TO QGL1-WBEG-ACCOUNT
              MOVE ALL "9"          TO QGL1-WEND-BRANCH
                                       QGL1-WEND-ACCOUNT.

           EXEC SQL
            DECLARE GL1_CURSOR CURSOR FOR
            SELECT GLFILE.GL_FLAG,
                   GLFILE.GL_BR_ACCOUNT,
                   CAST(GLFILE.GL_LTOUCH_DATE AS VARCHAR(10)),
                   GLFILE.GL_DESCRIPTION,
                   GLFILE.GL_CONSOL,
                   GLFILE.GL_OPENBAL,
                   GLFILE.GL_CYBAL_1,
                   GLFILE.GL_CYBAL_2,
                   GLFILE.GL_CYBAL_3,
                   GLFILE.GL_CYBAL_4,
                   GLFILE.GL_CYBAL_5,
                   GLFILE.GL_CYBAL_6,
                   GLFILE.GL_CYBAL_7,
                   GLFILE.GL_CYBAL_8,
                   GLFILE.GL_CYBAL_9,
                   GLFILE.GL_CYBAL_10,
                   GLFILE.GL_CYBAL_11,
                   GLFILE.GL_CYBAL_12,
                   GLFILE.GL_LYBAL_1,
                   GLFILE.GL_LYBAL_2,
                   GLFILE.GL_LYBAL_3,
                   GLFILE.GL_LYBAL_4,
                   GLFILE.GL_LYBAL_5,
                   GLFILE.GL_LYBAL_6,
                   GLFILE.GL_LYBAL_7,
                   GLFILE.GL_LYBAL_8,
                   GLFILE.GL_LYBAL_9,
                   GLFILE.GL_LYBAL_10,
                   GLFILE.GL_LYBAL_11,
                   GLFILE.GL_LYBAL_12,
                   GLFILE.GL_CYBUD_1,
                   GLFILE.GL_CYBUD_2,
                   GLFILE.GL_CYBUD_3,
                   GLFILE.GL_CYBUD_4,
                   GLFILE.GL_CYBUD_5,
                   GLFILE.GL_CYBUD_6,
                   GLFILE.GL_CYBUD_7,
                   GLFILE.GL_CYBUD_8,
                   GLFILE.GL_CYBUD_9,
                   GLFILE.GL_CYBUD_10,
                   GLFILE.GL_CYBUD_11,
                   GLFILE.GL_CYBUD_12,
                   GLFILE.GL_LYBUD_1,
                   GLFILE.GL_LYBUD_2,
                   GLFILE.GL_LYBUD_3,
                   GLFILE.GL_LYBUD_4,
                   GLFILE.GL_LYBUD_5,
                   GLFILE.GL_LYBUD_6,
                   GLFILE.GL_LYBUD_7,
                   GLFILE.GL_LYBUD_8,
                   GLFILE.GL_LYBUD_9,
                   GLFILE.GL_LYBUD_10,
                   GLFILE.GL_LYBUD_11,
                   GLFILE.GL_LYBUD_12,
                   GLFILE.GL_NYBUD_1,
                   GLFILE.GL_NYBUD_2,
                   GLFILE.GL_NYBUD_3,
                   GLFILE.GL_NYBUD_4,
                   GLFILE.GL_NYBUD_5,
                   GLFILE.GL_NYBUD_6,
                   GLFILE.GL_NYBUD_7,
                   GLFILE.GL_NYBUD_8,
                   GLFILE.GL_NYBUD_9,
                   GLFILE.GL_NYBUD_10,
                   GLFILE.GL_NYBUD_11,
                   GLFILE.GL_NYBUD_12,
                   GLFILE.GL_TYPE,
                   GLFILE.GL_CLEAR,
                   GLFILE.GL_NUMBER1,
                   GLFILE.GL_NUMBER2,
                   GLFILE.GL_FS_FLAG,
                   GLFILE.GL_REPORTS_1,
                   GLFILE.GL_REPORTS_2,
                   GLFILE.GL_REPORTS_3,
                   GLFILE.GL_REPORTS_4,
                   GLFILE.GL_REPORTS_5,
                   GLFILE.GL_LYOPENBAL,
                   GLFILE.GL_DLALLOC_GL,
                   GLFILE.GL_SPANISH_DESC
            FROM DBO.GLFILE
            WHERE GLFILE.GL_BR_ACCOUNT >= :QGL1-WBEG-BR-ACCOUNT
              AND GLFILE.GL_BR_ACCOUNT <= :QGL1-WEND-BR-ACCOUNT
            ORDER BY GLFILE.GL_BR_ACCOUNT
           END-EXEC.

           EXEC SQL
               OPEN GL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGL1-WBEG-BRANCH 
                          QGL1-WBEG-ACCOUNT
                          QGL1-WEND-BRANCH
                          QGL1-WEND-ACCOUNT.

           MOVE STAT---GOOD TO GL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-GL1-FILE-GREATER.
           PERFORM START-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.

           IF ( GL1-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-GL1-FILE.

           IF ( QGL1-WEND-ACCOUNT = ZEROES    ) OR
              ( QGL1-WEND-ACCOUNT NOT NUMERIC )
              MOVE GL-BRANCH         TO QGL1-WBEG-BRANCH
              MOVE GL-ACCOUNT        TO QGL1-WBEG-ACCOUNT
              MOVE ALL "9"           TO QGL1-WEND-BRANCH
                                        QGL1-WEND-ACCOUNT.

           EXEC SQL
            DECLARE GL1_CURSOR_GR CURSOR FOR
            SELECT GLFILE.GL_FLAG,
                   GLFILE.GL_BR_ACCOUNT,
                   CAST(GLFILE.GL_LTOUCH_DATE AS VARCHAR(10)),
                   GLFILE.GL_DESCRIPTION,
                   GLFILE.GL_CONSOL,
                   GLFILE.GL_OPENBAL,
                   GLFILE.GL_CYBAL_1,
                   GLFILE.GL_CYBAL_2,
                   GLFILE.GL_CYBAL_3,
                   GLFILE.GL_CYBAL_4,
                   GLFILE.GL_CYBAL_5,
                   GLFILE.GL_CYBAL_6,
                   GLFILE.GL_CYBAL_7,
                   GLFILE.GL_CYBAL_8,
                   GLFILE.GL_CYBAL_9,
                   GLFILE.GL_CYBAL_10,
                   GLFILE.GL_CYBAL_11,
                   GLFILE.GL_CYBAL_12,
                   GLFILE.GL_LYBAL_1,
                   GLFILE.GL_LYBAL_2,
                   GLFILE.GL_LYBAL_3,
                   GLFILE.GL_LYBAL_4,
                   GLFILE.GL_LYBAL_5,
                   GLFILE.GL_LYBAL_6,
                   GLFILE.GL_LYBAL_7,
                   GLFILE.GL_LYBAL_8,
                   GLFILE.GL_LYBAL_9,
                   GLFILE.GL_LYBAL_10,
                   GLFILE.GL_LYBAL_11,
                   GLFILE.GL_LYBAL_12,
                   GLFILE.GL_CYBUD_1,
                   GLFILE.GL_CYBUD_2,
                   GLFILE.GL_CYBUD_3,
                   GLFILE.GL_CYBUD_4,
                   GLFILE.GL_CYBUD_5,
                   GLFILE.GL_CYBUD_6,
                   GLFILE.GL_CYBUD_7,
                   GLFILE.GL_CYBUD_8,
                   GLFILE.GL_CYBUD_9,
                   GLFILE.GL_CYBUD_10,
                   GLFILE.GL_CYBUD_11,
                   GLFILE.GL_CYBUD_12,
                   GLFILE.GL_LYBUD_1,
                   GLFILE.GL_LYBUD_2,
                   GLFILE.GL_LYBUD_3,
                   GLFILE.GL_LYBUD_4,
                   GLFILE.GL_LYBUD_5,
                   GLFILE.GL_LYBUD_6,
                   GLFILE.GL_LYBUD_7,
                   GLFILE.GL_LYBUD_8,
                   GLFILE.GL_LYBUD_9,
                   GLFILE.GL_LYBUD_10,
                   GLFILE.GL_LYBUD_11,
                   GLFILE.GL_LYBUD_12,
                   GLFILE.GL_NYBUD_1,
                   GLFILE.GL_NYBUD_2,
                   GLFILE.GL_NYBUD_3,
                   GLFILE.GL_NYBUD_4,
                   GLFILE.GL_NYBUD_5,
                   GLFILE.GL_NYBUD_6,
                   GLFILE.GL_NYBUD_7,
                   GLFILE.GL_NYBUD_8,
                   GLFILE.GL_NYBUD_9,
                   GLFILE.GL_NYBUD_10,
                   GLFILE.GL_NYBUD_11,
                   GLFILE.GL_NYBUD_12,
                   GLFILE.GL_TYPE,
                   GLFILE.GL_CLEAR,
                   GLFILE.GL_NUMBER1,
                   GLFILE.GL_NUMBER2,
                   GLFILE.GL_FS_FLAG,
                   GLFILE.GL_REPORTS_1,
                   GLFILE.GL_REPORTS_2,
                   GLFILE.GL_REPORTS_3,
                   GLFILE.GL_REPORTS_4,
                   GLFILE.GL_REPORTS_5,
                   GLFILE.GL_LYOPENBAL,
                   GLFILE.GL_DLALLOC_GL,
                   GLFILE.GL_SPANISH_DESC
            FROM DBO.GLFILE
            WHERE GLFILE.GL_BR_ACCOUNT > :QGL1-WBEG-BR-ACCOUNT
              AND GLFILE.GL_BR_ACCOUNT <= :QGL1-WEND-BR-ACCOUNT
            ORDER BY GLFILE.GL_BR_ACCOUNT
           END-EXEC.

           EXEC SQL
               OPEN GL1_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGL1-WBEG-BRANCH 
                          QGL1-WBEG-ACCOUNT
                          QGL1-WEND-BRANCH
                          QGL1-WEND-ACCOUNT.

           MOVE STAT---GOOD TO GL1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-GL1-FILE.
           PERFORM READ-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.
           INITIALIZE QGL-REC.

           MOVE GL-BR-ACCOUNT TO QGL-BR-ACCOUNT.

           EXEC SQL
            SELECT GLFILE.GL_FLAG,
                   GLFILE.GL_BR_ACCOUNT,
                   CAST(GLFILE.GL_LTOUCH_DATE AS VARCHAR(10)),
                   GLFILE.GL_DESCRIPTION,
                   GLFILE.GL_CONSOL,
                   GLFILE.GL_OPENBAL,
                   GLFILE.GL_CYBAL_1,
                   GLFILE.GL_CYBAL_2,
                   GLFILE.GL_CYBAL_3,
                   GLFILE.GL_CYBAL_4,
                   GLFILE.GL_CYBAL_5,
                   GLFILE.GL_CYBAL_6,
                   GLFILE.GL_CYBAL_7,
                   GLFILE.GL_CYBAL_8,
                   GLFILE.GL_CYBAL_9,
                   GLFILE.GL_CYBAL_10,
                   GLFILE.GL_CYBAL_11,
                   GLFILE.GL_CYBAL_12,
                   GLFILE.GL_LYBAL_1,
                   GLFILE.GL_LYBAL_2,
                   GLFILE.GL_LYBAL_3,
                   GLFILE.GL_LYBAL_4,
                   GLFILE.GL_LYBAL_5,
                   GLFILE.GL_LYBAL_6,
                   GLFILE.GL_LYBAL_7,
                   GLFILE.GL_LYBAL_8,
                   GLFILE.GL_LYBAL_9,
                   GLFILE.GL_LYBAL_10,
                   GLFILE.GL_LYBAL_11,
                   GLFILE.GL_LYBAL_12,
                   GLFILE.GL_CYBUD_1,
                   GLFILE.GL_CYBUD_2,
                   GLFILE.GL_CYBUD_3,
                   GLFILE.GL_CYBUD_4,
                   GLFILE.GL_CYBUD_5,
                   GLFILE.GL_CYBUD_6,
                   GLFILE.GL_CYBUD_7,
                   GLFILE.GL_CYBUD_8,
                   GLFILE.GL_CYBUD_9,
                   GLFILE.GL_CYBUD_10,
                   GLFILE.GL_CYBUD_11,
                   GLFILE.GL_CYBUD_12,
                   GLFILE.GL_LYBUD_1,
                   GLFILE.GL_LYBUD_2,
                   GLFILE.GL_LYBUD_3,
                   GLFILE.GL_LYBUD_4,
                   GLFILE.GL_LYBUD_5,
                   GLFILE.GL_LYBUD_6,
                   GLFILE.GL_LYBUD_7,
                   GLFILE.GL_LYBUD_8,
                   GLFILE.GL_LYBUD_9,
                   GLFILE.GL_LYBUD_10,
                   GLFILE.GL_LYBUD_11,
                   GLFILE.GL_LYBUD_12,
                   GLFILE.GL_NYBUD_1,
                   GLFILE.GL_NYBUD_2,
                   GLFILE.GL_NYBUD_3,
                   GLFILE.GL_NYBUD_4,
                   GLFILE.GL_NYBUD_5,
                   GLFILE.GL_NYBUD_6,
                   GLFILE.GL_NYBUD_7,
                   GLFILE.GL_NYBUD_8,
                   GLFILE.GL_NYBUD_9,
                   GLFILE.GL_NYBUD_10,
                   GLFILE.GL_NYBUD_11,
                   GLFILE.GL_NYBUD_12,
                   GLFILE.GL_TYPE,
                   GLFILE.GL_CLEAR,
                   GLFILE.GL_NUMBER1,
                   GLFILE.GL_NUMBER2,
                   GLFILE.GL_FS_FLAG,
                   GLFILE.GL_REPORTS_1,
                   GLFILE.GL_REPORTS_2,
                   GLFILE.GL_REPORTS_3,
                   GLFILE.GL_REPORTS_4,
                   GLFILE.GL_REPORTS_5,
                   GLFILE.GL_LYOPENBAL,
                   GLFILE.GL_DLALLOC_GL,
                   GLFILE.GL_SPANISH_DESC
            INTO :QGL-REC
            FROM DBO.GLFILE
            WHERE GLFILE.GL_BR_ACCOUNT  = :QGL-BR-ACCOUNT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       READ-GL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.
           INITIALIZE QGL-REC.

           IF ( GL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GL1_CURSOR INTO :QGL-REC
              END-EXEC
           ELSE
           IF ( GL1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH GL1_CURSOR_GR INTO :QGL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       WRITE-GL1-FILE.

           ACCEPT GL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.
           PERFORM SET-GL-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GLFILE
             (GLFILE.GL_FLAG,
              GLFILE.GL_BR_ACCOUNT,
              GLFILE.GL_LTOUCH_DATE,
              GLFILE.GL_DESCRIPTION,
              GLFILE.GL_CONSOL,
              GLFILE.GL_OPENBAL,
              GLFILE.GL_CYBAL_1,
              GLFILE.GL_CYBAL_2,
              GLFILE.GL_CYBAL_3,
              GLFILE.GL_CYBAL_4,
              GLFILE.GL_CYBAL_5,
              GLFILE.GL_CYBAL_6,
              GLFILE.GL_CYBAL_7,
              GLFILE.GL_CYBAL_8,
              GLFILE.GL_CYBAL_9,
              GLFILE.GL_CYBAL_10,
              GLFILE.GL_CYBAL_11,
              GLFILE.GL_CYBAL_12,
              GLFILE.GL_LYBAL_1,
              GLFILE.GL_LYBAL_2,
              GLFILE.GL_LYBAL_3,
              GLFILE.GL_LYBAL_4,
              GLFILE.GL_LYBAL_5,
              GLFILE.GL_LYBAL_6,
              GLFILE.GL_LYBAL_7,
              GLFILE.GL_LYBAL_8,
              GLFILE.GL_LYBAL_9,
              GLFILE.GL_LYBAL_10,
              GLFILE.GL_LYBAL_11,
              GLFILE.GL_LYBAL_12,
              GLFILE.GL_CYBUD_1,
              GLFILE.GL_CYBUD_2,
              GLFILE.GL_CYBUD_3,
              GLFILE.GL_CYBUD_4,
              GLFILE.GL_CYBUD_5,
              GLFILE.GL_CYBUD_6,
              GLFILE.GL_CYBUD_7,
              GLFILE.GL_CYBUD_8,
              GLFILE.GL_CYBUD_9,
              GLFILE.GL_CYBUD_10,
              GLFILE.GL_CYBUD_11,
              GLFILE.GL_CYBUD_12,
              GLFILE.GL_LYBUD_1,
              GLFILE.GL_LYBUD_2,
              GLFILE.GL_LYBUD_3,
              GLFILE.GL_LYBUD_4,
              GLFILE.GL_LYBUD_5,
              GLFILE.GL_LYBUD_6,
              GLFILE.GL_LYBUD_7,
              GLFILE.GL_LYBUD_8,
              GLFILE.GL_LYBUD_9,
              GLFILE.GL_LYBUD_10,
              GLFILE.GL_LYBUD_11,
              GLFILE.GL_LYBUD_12,
              GLFILE.GL_NYBUD_1,
              GLFILE.GL_NYBUD_2,
              GLFILE.GL_NYBUD_3,
              GLFILE.GL_NYBUD_4,
              GLFILE.GL_NYBUD_5,
              GLFILE.GL_NYBUD_6,
              GLFILE.GL_NYBUD_7,
              GLFILE.GL_NYBUD_8,
              GLFILE.GL_NYBUD_9,
              GLFILE.GL_NYBUD_10,
              GLFILE.GL_NYBUD_11,
              GLFILE.GL_NYBUD_12,
              GLFILE.GL_TYPE,
              GLFILE.GL_CLEAR,
              GLFILE.GL_NUMBER1,
              GLFILE.GL_NUMBER2,
              GLFILE.GL_FS_FLAG,
              GLFILE.GL_REPORTS_1,
              GLFILE.GL_REPORTS_2,
              GLFILE.GL_REPORTS_3,
              GLFILE.GL_REPORTS_4,
              GLFILE.GL_REPORTS_5,
              GLFILE.GL_LYOPENBAL,
              GLFILE.GL_DLALLOC_GL,
              GLFILE.GL_SPANISH_DESC)
            VALUES
             (:QGL-FLAG,
              :QGL-BR-ACCOUNT,
              :QGL-LTOUCH-DATE,
              :QGL-DESCRIPTION,
              :QGL-CONSOL,
              :QGL-OPENBAL,
              :QGL-CYBAL-1,
              :QGL-CYBAL-2,
              :QGL-CYBAL-3,
              :QGL-CYBAL-4,
              :QGL-CYBAL-5,
              :QGL-CYBAL-6,
              :QGL-CYBAL-7,
              :QGL-CYBAL-8,
              :QGL-CYBAL-9,
              :QGL-CYBAL-10,
              :QGL-CYBAL-11,
              :QGL-CYBAL-12,
              :QGL-LYBAL-1,
              :QGL-LYBAL-2,
              :QGL-LYBAL-3,
              :QGL-LYBAL-4,
              :QGL-LYBAL-5,
              :QGL-LYBAL-6,
              :QGL-LYBAL-7,
              :QGL-LYBAL-8,
              :QGL-LYBAL-9,
              :QGL-LYBAL-10,
              :QGL-LYBAL-11,
              :QGL-LYBAL-12,
              :QGL-CYBUD-1,
              :QGL-CYBUD-2,
              :QGL-CYBUD-3,
              :QGL-CYBUD-4,
              :QGL-CYBUD-5,
              :QGL-CYBUD-6,
              :QGL-CYBUD-7,
              :QGL-CYBUD-8,
              :QGL-CYBUD-9,
              :QGL-CYBUD-10,
              :QGL-CYBUD-11,
              :QGL-CYBUD-12,
              :QGL-LYBUD-1,
              :QGL-LYBUD-2,
              :QGL-LYBUD-3,
              :QGL-LYBUD-4,
              :QGL-LYBUD-5,
              :QGL-LYBUD-6,
              :QGL-LYBUD-7,
              :QGL-LYBUD-8,
              :QGL-LYBUD-9,
              :QGL-LYBUD-10,
              :QGL-LYBUD-11,
              :QGL-LYBUD-12,
              :QGL-NYBUD-1,
              :QGL-NYBUD-2,
              :QGL-NYBUD-3,
              :QGL-NYBUD-4,
              :QGL-NYBUD-5,
              :QGL-NYBUD-6,
              :QGL-NYBUD-7,
              :QGL-NYBUD-8,
              :QGL-NYBUD-9,
              :QGL-NYBUD-10,
              :QGL-NYBUD-11,
              :QGL-NYBUD-12,
              :QGL-TYPE,
              :QGL-CLEAR,
              :QGL-NUMBER1,
              :QGL-NUMBER2,
              :QGL-FS-FLAG,
              :QGL-REPORTS-1,
              :QGL-REPORTS-2,
              :QGL-REPORTS-3,
              :QGL-REPORTS-4,
              :QGL-REPORTS-5,
              :QGL-LYOPENBAL,
              :QGL-DLALLOC-GL,
              :QGL-SPANISH-DESC)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GL1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GL1-FILE.

           ACCEPT GL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.
           PERFORM SET-GL-FIELDS.

           EXEC SQL
            UPDATE DBO.GLFILE
             SET
              GLFILE.GL_FLAG = :QGL-FLAG,
              GLFILE.GL_BR_ACCOUNT = :QGL-BR-ACCOUNT,
              GLFILE.GL_LTOUCH_DATE = :QGL-LTOUCH-DATE,
              GLFILE.GL_DESCRIPTION = :QGL-DESCRIPTION,
              GLFILE.GL_CONSOL = :QGL-CONSOL,
              GLFILE.GL_OPENBAL = :QGL-OPENBAL,
              GLFILE.GL_CYBAL_1 = :QGL-CYBAL-1,
              GLFILE.GL_CYBAL_2 = :QGL-CYBAL-2,
              GLFILE.GL_CYBAL_3 = :QGL-CYBAL-3,
              GLFILE.GL_CYBAL_4 = :QGL-CYBAL-4,
              GLFILE.GL_CYBAL_5 = :QGL-CYBAL-5,
              GLFILE.GL_CYBAL_6 = :QGL-CYBAL-6,
              GLFILE.GL_CYBAL_7 = :QGL-CYBAL-7,
              GLFILE.GL_CYBAL_8 = :QGL-CYBAL-8,
              GLFILE.GL_CYBAL_9 = :QGL-CYBAL-9,
              GLFILE.GL_CYBAL_10 = :QGL-CYBAL-10,
              GLFILE.GL_CYBAL_11 = :QGL-CYBAL-11,
              GLFILE.GL_CYBAL_12 = :QGL-CYBAL-12,
              GLFILE.GL_LYBAL_1 = :QGL-LYBAL-1,
              GLFILE.GL_LYBAL_2 = :QGL-LYBAL-2,
              GLFILE.GL_LYBAL_3 = :QGL-LYBAL-3,
              GLFILE.GL_LYBAL_4 = :QGL-LYBAL-4,
              GLFILE.GL_LYBAL_5 = :QGL-LYBAL-5,
              GLFILE.GL_LYBAL_6 = :QGL-LYBAL-6,
              GLFILE.GL_LYBAL_7 = :QGL-LYBAL-7,
              GLFILE.GL_LYBAL_8 = :QGL-LYBAL-8,
              GLFILE.GL_LYBAL_9 = :QGL-LYBAL-9,
              GLFILE.GL_LYBAL_10 = :QGL-LYBAL-10,
              GLFILE.GL_LYBAL_11 = :QGL-LYBAL-11,
              GLFILE.GL_LYBAL_12 = :QGL-LYBAL-12,
              GLFILE.GL_CYBUD_1 = :QGL-CYBUD-1,
              GLFILE.GL_CYBUD_2 = :QGL-CYBUD-2,
              GLFILE.GL_CYBUD_3 = :QGL-CYBUD-3,
              GLFILE.GL_CYBUD_4 = :QGL-CYBUD-4,
              GLFILE.GL_CYBUD_5 = :QGL-CYBUD-5,
              GLFILE.GL_CYBUD_6 = :QGL-CYBUD-6,
              GLFILE.GL_CYBUD_7 = :QGL-CYBUD-7,
              GLFILE.GL_CYBUD_8 = :QGL-CYBUD-8,
              GLFILE.GL_CYBUD_9 = :QGL-CYBUD-9,
              GLFILE.GL_CYBUD_10 = :QGL-CYBUD-10,
              GLFILE.GL_CYBUD_11 = :QGL-CYBUD-11,
              GLFILE.GL_CYBUD_12 = :QGL-CYBUD-12,
              GLFILE.GL_LYBUD_1 = :QGL-LYBUD-1,
              GLFILE.GL_LYBUD_2 = :QGL-LYBUD-2,
              GLFILE.GL_LYBUD_3 = :QGL-LYBUD-3,
              GLFILE.GL_LYBUD_4 = :QGL-LYBUD-4,
              GLFILE.GL_LYBUD_5 = :QGL-LYBUD-5,
              GLFILE.GL_LYBUD_6 = :QGL-LYBUD-6,
              GLFILE.GL_LYBUD_7 = :QGL-LYBUD-7,
              GLFILE.GL_LYBUD_8 = :QGL-LYBUD-8,
              GLFILE.GL_LYBUD_9 = :QGL-LYBUD-9,
              GLFILE.GL_LYBUD_10 = :QGL-LYBUD-10,
              GLFILE.GL_LYBUD_11 = :QGL-LYBUD-11,
              GLFILE.GL_LYBUD_12 = :QGL-LYBUD-12,
              GLFILE.GL_NYBUD_1 = :QGL-NYBUD-1,
              GLFILE.GL_NYBUD_2 = :QGL-NYBUD-2,
              GLFILE.GL_NYBUD_3 = :QGL-NYBUD-3,
              GLFILE.GL_NYBUD_4 = :QGL-NYBUD-4,
              GLFILE.GL_NYBUD_5 = :QGL-NYBUD-5,
              GLFILE.GL_NYBUD_6 = :QGL-NYBUD-6,
              GLFILE.GL_NYBUD_7 = :QGL-NYBUD-7,
              GLFILE.GL_NYBUD_8 = :QGL-NYBUD-8,
              GLFILE.GL_NYBUD_9 = :QGL-NYBUD-9,
              GLFILE.GL_NYBUD_10 = :QGL-NYBUD-10,
              GLFILE.GL_NYBUD_11 = :QGL-NYBUD-11,
              GLFILE.GL_NYBUD_12 = :QGL-NYBUD-12,
              GLFILE.GL_TYPE = :QGL-TYPE,
              GLFILE.GL_CLEAR = :QGL-CLEAR,
              GLFILE.GL_NUMBER1 = :QGL-NUMBER1,
              GLFILE.GL_NUMBER2 = :QGL-NUMBER2,
              GLFILE.GL_FS_FLAG = :QGL-FS-FLAG,
              GLFILE.GL_REPORTS_1 = :QGL-REPORTS-1,
              GLFILE.GL_REPORTS_2 = :QGL-REPORTS-2,
              GLFILE.GL_REPORTS_3 = :QGL-REPORTS-3,
              GLFILE.GL_REPORTS_4 = :QGL-REPORTS-4,
              GLFILE.GL_REPORTS_5 = :QGL-REPORTS-5,
              GLFILE.GL_LYOPENBAL = :QGL-LYOPENBAL,
              GLFILE.GL_DLALLOC_GL = :QGL-DLALLOC-GL,
              GLFILE.GL_SPANISH_DESC = :QGL-SPANISH-DESC
             WHERE GLFILE.GL_BR_ACCOUNT = :QGL-BR-ACCOUNT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GL1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GL1-FILE.

           PERFORM DELETE-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.
           PERFORM SET-GL-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GLFILE
             WHERE GLFILE.GL_BR_ACCOUNT = :QGL-BR-ACCOUNT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GL1-FILE.

      *-----------------------------------------------------------------
       CLOSE-GL1-FILE.

           PERFORM CLOSE-IT.

           IF ( GL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GL1-CURSOR-STAT.

           IF ( GL1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE GL1_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO GL1-CURSOR-GR-STAT.

