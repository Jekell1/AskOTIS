      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.
      *-----------------------------------------------------------------
       COUNT-BR-FILE.
           PERFORM COUNT-IT.
           MOVE BR-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.

           EXEC SQL
            SELECT COUNT(*)
            INTO :BR1-ROW-COUNT
            FROM DBO.BRFILE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO COUNT-BR-FILE.

      *-----------------------------------------------------------------
