      *-----------------------------------------------------------------
       READ-LN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.

           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN1_CURSOR INTO :QLN-REC
              END-EXEC
           ELSE
           IF ( LN1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH LN1_CURSOR_GR INTO :QLN-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
