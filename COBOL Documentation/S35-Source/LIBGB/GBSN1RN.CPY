      * COPYMEMBER: LIBGB/GBSN1RN
       LOAD-SN1-FILE.
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
       OPEN-SN1-FILE.
           PERFORM LOAD-SN1-FILE.
           PERFORM OPEN-IT.
           MOVE SN-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SN1-FILE.
           PERFORM START-IT.
           MOVE SN-PATH-SQL TO E-FILE.
           MOVE SN1-KEY TO E-KEYX.

           IF ( SN1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SN1-FILE.

           IF ( QSN1-WEND-FORM = SPACES    )
               MOVE SN-FORM   TO QSN1-WBEG-FORM
               MOVE ALL "Z"   TO QSN1-WEND-FORM.

           EXEC SQL
            DECLARE SN1_CURSOR CURSOR FOR
            SELECT SNFILE.SN_SEQ,
                   SNFILE.SN_FORM,
                   CAST(SNFILE.SN_LTOUCH_DATE AS VARCHAR(10)),
                   SNFILE.SN_DESC,
                   SNFILE.SN_PURCHASABLE
            FROM DBO.SNFILE
            WHERE SNFILE.SN_FORM >= :QSN1-WBEG-FORM
              AND SNFILE.SN_FORM <= :QSN1-WEND-FORM
            ORDER BY SNFILE.SN_FORM
           END-EXEC.

           EXEC SQL
               OPEN SN1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSN1-WBEG-FORM
                          QSN1-WEND-FORM.

           MOVE STAT---GOOD TO SN1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SN1-FILE.
           PERFORM READ-IT.
           MOVE SN-PATH-SQL TO E-FILE.
           MOVE SN1-KEY TO E-KEYX.
           INITIALIZE QSN-REC.

           MOVE SN-FORM    TO QSN-FORM.

           EXEC SQL
            SELECT SNFILE.SN_SEQ,
                   SNFILE.SN_FORM,
                   CAST(SNFILE.SN_LTOUCH_DATE AS VARCHAR(10)),
                   SNFILE.SN_DESC,
                   SNFILE.SN_PURCHASABLE
            INTO :QSN-REC
            FROM DBO.SNFILE
            WHERE SNFILE.SN_FORM = :QSN-FORM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SN1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SN-FIELDS.

      *-----------------------------------------------------------------
       READ-SN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SN-PATH-SQL TO E-FILE.
           MOVE SN1-KEY TO E-KEYX.
           INITIALIZE QSN-REC.

           IF ( SN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SN1_CURSOR INTO :QSN-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SN1-FILE.
           PERFORM CLOSE-IT.
           IF ( SN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SN1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SN1-CURSOR-STAT.

