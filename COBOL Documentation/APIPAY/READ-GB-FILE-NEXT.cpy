      *-----------------------------------------------------------------
       READ-GB-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GB-PATH-OWNBR  TO GB-BRNO.
           MOVE GB-PATH-SQL    TO E-FILE.
           MOVE GB-KEY         TO E-KEYX.
           INITIALIZE QGB-REC.

           IF ( GB-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GB_CURSOR INTO :QGB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GB-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GB-FIELDS.

      *-----------------------------------------------------------------
