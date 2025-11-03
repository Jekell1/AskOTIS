      *-----------------------------------------------------------------
       READ-CD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           INITIALIZE QCD-REC.

           IF ( CD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CD1_CURSOR INTO :QCD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
