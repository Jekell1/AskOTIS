      *-----------------------------------------------------------------
       READ-CD1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           INITIALIZE QCD-REC.

           IF ( CD1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH CD1_CURSOR_NG INTO :QCD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
