      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.
      *-----------------------------------------------------------------
       COUNT-GI1-FILE.
           PERFORM COUNT-IT.
           MOVE GI-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           MOVE GI-BRANCH      TO QGI-BRANCH.

           EXEC SQL
            SELECT COUNT(*)
              INTO :GI1-ROW-COUNT
              FROM DBO.GIFILE
             WHERE GI_BRANCH = :QGI-BRANCH
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO COUNT-GI1-FILE.

      *-----------------------------------------------------------------
