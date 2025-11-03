      *-----------------------------------------------------------------
       READ-LXE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LXE-PATH-OWNBR  TO LXE-BRNO.
           MOVE LXE-PATH-SQL    TO E-FILE.
           MOVE LXE1-KEY        TO E-KEYX.
           INITIALIZE QLXE-REC.

           IF ( LXE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LXE1_CURSOR INTO :QLXE-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LXE1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LXE-FIELDS.

      *-----------------------------------------------------------------
