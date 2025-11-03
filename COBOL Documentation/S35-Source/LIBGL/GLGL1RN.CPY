      * COPYMEMBER: LIBGL/GLGL1RN
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
            WHERE GLFILE.GL_BR_ACCOUNT = :QGL-BR-ACCOUNT
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
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GL1-FILE.
           PERFORM CLOSE-IT.
           IF ( GL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GL1-CURSOR-STAT.

