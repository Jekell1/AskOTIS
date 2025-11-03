      *-----------------------------------------------------------------
       READ-GI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GI-PATH-SQL TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.
           INITIALIZE QGI-REC.

           IF ( GI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GI1_CURSOR INTO :QGI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GI-FIELDS.

      *-----------------------------------------------------------------
