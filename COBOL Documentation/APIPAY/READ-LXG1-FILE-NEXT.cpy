      *-----------------------------------------------------------------
       READ-LXG1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LXG-PATH-OWNBR  TO LXG-BRNO.
           MOVE LXG-PATH-SQL TO E-FILE.
           MOVE LXG1-KEY        TO E-KEYX.
           INITIALIZE QLXG-REC.

           IF ( LXG1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LXG1_CURSOR INTO :QLXG-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LXG1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LXG-FIELDS.

      *-----------------------------------------------------------------
