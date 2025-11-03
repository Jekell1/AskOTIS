      * COPYMEMBER: LIBGB/GBSN2RN
       LOAD-SN2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SN-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SN-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SN-ALL-MACHINE
              MOVE SN-ALL-PATH             TO SN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SN-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SN-ALL-BASE
              MOVE FILPATH-MACHINE         TO SN-ALL-MACHINE
              MOVE SN-ALL-PATH             TO SN-PATH.

      *-----------------------------------------------------------------
       OPEN-SN2-FILE.
           PERFORM LOAD-SN2-FILE.
           PERFORM OPEN-IT.
           MOVE SN-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SN2-FILE.
           PERFORM START-IT.
           MOVE SN-PATH-SQL TO E-FILE.
           MOVE SN2-KEY TO E-KEYX.

           IF ( SN2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SN2-FILE.

           IF ( QSN2-WEND-SEQ = 0    ) OR
              ( QSN2-WEND-SEQ NOT NUMERIC )
               MOVE SN-SEQ    TO QSN2-WBEG-SEQ
               MOVE ALL "9"   TO QSN2-WEND-SEQ
               MOVE SN-FORM   TO QSN2-WBEG-FORM
               MOVE ALL "Z"   TO QSN2-WEND-FORM.

           EXEC SQL
            DECLARE SN2_CURSOR CURSOR FOR
            SELECT SNFILE.SN_SEQ,
                   SNFILE.SN_FORM,
                   CAST(SNFILE.SN_LTOUCH_DATE AS VARCHAR(10)),
                   SNFILE.SN_DESC,
                   SNFILE.SN_PURCHASABLE
            FROM DBO.SNFILE
            WHERE SNFILE.SN_SEQ >= :QSN2-WBEG-SEQ
              AND SNFILE.SN_SEQ <= :QSN2-WEND-SEQ
              AND SNFILE.SN_FORM >= :QSN2-WBEG-FORM
              AND SNFILE.SN_FORM <= :QSN2-WEND-FORM
            ORDER BY SNFILE.SN_SEQ,
                     SNFILE.SN_FORM
           END-EXEC.

           EXEC SQL
               OPEN SN2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE 0      TO QSN2-WBEG-SEQ
                          QSN2-WEND-SEQ.
           MOVE SPACES TO QSN2-WBEG-FORM
                          QSN2-WEND-FORM.

           MOVE STAT---GOOD TO SN2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SN2-FILE.
           PERFORM READ-IT.
           MOVE SN-PATH-SQL TO E-FILE.
           MOVE SN2-KEY TO E-KEYX.
           INITIALIZE QSN-REC.

           MOVE SN-SEQ     TO QSN-SEQ.
           MOVE SN-FORM    TO QSN-FORM.

           EXEC SQL
            SELECT SNFILE.SN_SEQ,
                   SNFILE.SN_FORM,
                   CAST(SNFILE.SN_LTOUCH_DATE AS VARCHAR(10)),
                   SNFILE.SN_DESC,
                   SNFILE.SN_PURCHASABLE
            INTO :QSN-REC
            FROM DBO.SNFILE
            WHERE SNFILE.SN_SEQ = :QSN-SEQ
              AND SNFILE.SN_FORM = :QSN-FORM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SN2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SN-FIELDS.

      *-----------------------------------------------------------------
       READ-SN2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SN-PATH-SQL TO E-FILE.
           MOVE SN2-KEY TO E-KEYX.
           INITIALIZE QSN-REC.

           IF ( SN2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SN2_CURSOR INTO :QSN-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SN2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SN2-FILE.
           PERFORM CLOSE-IT.
           IF ( SN2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SN2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SN2-CURSOR-STAT.

