      * COPYMEMBER: LIBGB/GBSE1R
       LOAD-SE1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SE-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SE-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SE-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SE-ALL-MACHINE
              MOVE SE-ALL-PATH             TO SE-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SE-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SE-ALL-BASE
              MOVE FILPATH-MACHINE         TO SE-ALL-MACHINE
              MOVE SE-ALL-PATH             TO SE-PATH.

      *-----------------------------------------------------------------
       OPEN-SE1-FILE.
           PERFORM LOAD-SE1-FILE.
           PERFORM OPEN-IT.
           MOVE SE-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SE1-FILE.
           PERFORM START-IT.
           MOVE SE-PATH-SQL TO E-FILE.
           MOVE SE1-KEY TO E-KEYX.

           IF ( SE1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SE1-FILE.

           IF ( QSE1-WEND-FORM = SPACES    )
               MOVE SE-FORM   TO QSE1-WBEG-FORM
               MOVE ALL "Z"   TO QSE1-WEND-FORM.

           EXEC SQL
            DECLARE SE1_CURSOR CURSOR FOR
            SELECT SEFILE.SE_FORM,
                   CAST(SEFILE.SE_LTOUCH_DATE AS VARCHAR(10)),
                   CAST(SEFILE.SE_EXP_DATE AS VARCHAR(10)),
                   SEFILE.SE_PURCHASABLE,
                   SEFILE.SE_PROF_1,
                   SEFILE.SE_PROF_2,
                   SEFILE.SE_PROF_3,
                   SEFILE.SE_PROF_4,
                   SEFILE.SE_PROF_5,
                   SEFILE.SE_PROF_6,
                   SEFILE.SE_PROF_7,
                   SEFILE.SE_PROF_8,
                   SEFILE.SE_PROF_9,
                   SEFILE.SE_PROF_10,
                   SEFILE.SE_PROF_11,
                   SEFILE.SE_PROF_12,
                   SEFILE.SE_PROF_13,
                   SEFILE.SE_PROF_14,
                   SEFILE.SE_PROF_15,
                   SEFILE.SE_PROF_16,
                   SEFILE.SE_PROF_17,
                   SEFILE.SE_PROF_18,
                   SEFILE.SE_PROF_19,
                   SEFILE.SE_PROF_20,
                   SEFILE.SE_PROF_21,
                   SEFILE.SE_PROF_22,
                   SEFILE.SE_PROF_23,
                   SEFILE.SE_PROF_24,
                   SEFILE.SE_PROF_25,
                   SEFILE.SE_PROF_26
            FROM DBO.SEFILE
            WHERE SEFILE.SE_FORM >= :QSE1-WBEG-FORM
              AND SEFILE.SE_FORM <= :QSE1-WEND-FORM
            ORDER BY SEFILE.SE_FORM
           END-EXEC.

           EXEC SQL
               OPEN SE1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSE1-WBEG-FORM
                          QSE1-WEND-FORM.

           MOVE STAT---GOOD TO SE1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SE1-FILE.
           PERFORM READ-IT.
           MOVE SE-PATH-SQL TO E-FILE.
           MOVE SE1-KEY TO E-KEYX.
           INITIALIZE QSE-REC.

           MOVE SE-FORM    TO QSE-FORM.

           EXEC SQL
            SELECT SEFILE.SE_FORM,
                   CAST(SEFILE.SE_LTOUCH_DATE AS VARCHAR(10)),
                   CAST(SEFILE.SE_EXP_DATE AS VARCHAR(10)),
                   SEFILE.SE_PURCHASABLE,
                   SEFILE.SE_PROF_1,
                   SEFILE.SE_PROF_2,
                   SEFILE.SE_PROF_3,
                   SEFILE.SE_PROF_4,
                   SEFILE.SE_PROF_5,
                   SEFILE.SE_PROF_6,
                   SEFILE.SE_PROF_7,
                   SEFILE.SE_PROF_8,
                   SEFILE.SE_PROF_9,
                   SEFILE.SE_PROF_10,
                   SEFILE.SE_PROF_11,
                   SEFILE.SE_PROF_12,
                   SEFILE.SE_PROF_13,
                   SEFILE.SE_PROF_14,
                   SEFILE.SE_PROF_15,
                   SEFILE.SE_PROF_16,
                   SEFILE.SE_PROF_17,
                   SEFILE.SE_PROF_18,
                   SEFILE.SE_PROF_19,
                   SEFILE.SE_PROF_20,
                   SEFILE.SE_PROF_21,
                   SEFILE.SE_PROF_22,
                   SEFILE.SE_PROF_23,
                   SEFILE.SE_PROF_24,
                   SEFILE.SE_PROF_25,
                   SEFILE.SE_PROF_26
            INTO :QSE-REC
            FROM DBO.SEFILE
            WHERE SEFILE.SE_FORM = :QSE-FORM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SE1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SE-FIELDS.

      *-----------------------------------------------------------------
       READ-SE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SE-PATH-SQL TO E-FILE.
           MOVE SE1-KEY TO E-KEYX.
           INITIALIZE QSE-REC.

           IF ( SE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SE1_CURSOR INTO :QSE-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SE1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SE-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SE1-FILE.
           PERFORM CLOSE-IT.
           IF ( SE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SE1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SE1-CURSOR-STAT.

