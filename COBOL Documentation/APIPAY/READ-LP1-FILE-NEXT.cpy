      *-----------------------------------------------------------------
       READ-LP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_CURSOR INTO :QLP-REC
              END-EXEC
           ELSE
           IF ( LP1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_CURSOR_SC INTO :QLP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
