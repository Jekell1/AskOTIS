      *-----------------------------------------------------------------
       READ-LTI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           MOVE LTI-PATH-SQL    TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.
           INITIALIZE QLTI-REC.

           IF ( LTI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTI1_CURSOR INTO :QLTI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTI-FIELDS.

      *-----------------------------------------------------------------
