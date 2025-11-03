      *-----------------------------------------------------------------
       READ-RC2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC2-KEY        TO E-KEYX.
           INITIALIZE QRC-REC.

           IF ( RC2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RC2_CURSOR INTO :QRC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
