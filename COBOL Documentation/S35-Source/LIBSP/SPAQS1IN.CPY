      * COPYMEMBER: LIBSP/SPAQS1IN
      ************************************************
      *                (SPAQS1IN)
      * REV:
      * 040519 AMM NEW AQS COPY MEMBER
      ************************************************
       LOAD-AQS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( AQS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( AQS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO AQS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO AQS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO AQS-ALL-DATA
              MOVE AQS-ALL-PATH             TO AQS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( AQS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO AQS-ALL-BASE
              MOVE FILPATH-MACHINE         TO AQS-ALL-MACHINE
              MOVE FILPATH-DATA            TO AQS-ALL-DATA
              MOVE AQS-ALL-PATH             TO AQS-PATH.

      *-----------------------------------------------------------------
       OPEN-AQS1-FILE.
           PERFORM LOAD-AQS1-FILE.
           PERFORM OPEN-IT.
           MOVE AQS-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-AQS1-FILE.
           PERFORM START-IT.
           MOVE AQS-PATH-OWNBR  TO AQS-BRNO.
           MOVE AQS-PATH-SQL    TO E-FILE.
           MOVE AQS1-KEY        TO E-KEYX.

           IF ( AQS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-AQS1-FILE.

           IF ( QAQS1-WEND-BRNO = ZEROES    ) OR
              ( QAQS1-WEND-BRNO NOT NUMERIC )
              MOVE AQS-PATH-OWNBR        TO QAQS1-WBEG-BRNO
              MOVE AQS-PATH-OWNBR        TO QAQS1-WEND-BRNO.

           EXEC SQL
            DECLARE AQS1_CURSOR CURSOR FOR
            SELECT AQSFILE.AQS_BRNO,
                   AQSFILE.AQS_NEXT_SEQNO,
                   AQSFILE.AQS_TOT_REC,
                   AQSFILE.AQS_WORKED_REC,
                   CAST(AQSFILE.AQS_LTOUCH_DATE AS VARCHAR(10))
            FROM DBO.AQSFILE
            WHERE AQSFILE.AQS_BRNO = :QAQS1-WBEG-BRNO
            ORDER BY AQSFILE.AQS_BRNO
           END-EXEC.

           EXEC SQL
               OPEN AQS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QAQS1-WBEG-BRNO 
                          QAQS1-WEND-BRNO.

           MOVE STAT---GOOD TO AQS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-AQS1-FILE.
           PERFORM READ-IT.
           MOVE AQS-PATH-OWNBR  TO AQS-BRNO. 
           MOVE AQS-PATH-SQL    TO E-FILE.
           MOVE AQS1-KEY        TO E-KEYX.
           INITIALIZE QAQS-REC.

           MOVE AQS-BRNO   TO QAQS-BRNO.

           EXEC SQL
            SELECT AQSFILE.AQS_BRNO,
                   AQSFILE.AQS_NEXT_SEQNO,
                   AQSFILE.AQS_TOT_REC,
                   AQSFILE.AQS_WORKED_REC,
                   CAST(AQSFILE.AQS_LTOUCH_DATE AS VARCHAR(10))
            INTO :QAQS-REC
            FROM DBO.AQSFILE
            WHERE AQSFILE.AQS_BRNO = :QAQS-BRNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-AQS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-AQS-FIELDS.

       READ-AQS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE AQS-PATH-OWNBR  TO AQS-BRNO.
           MOVE AQS-PATH-SQL    TO E-FILE.
           MOVE AQS1-KEY        TO E-KEYX.
           INITIALIZE QAQS-REC.

           IF ( AQS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH AQS1_CURSOR INTO :QAQS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-AQS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-AQS-FIELDS.

      *-----------------------------------------------------------------
       WRITE-AQS1-FILE.

           MOVE AQS-PATH-OWNBR TO AQS-BRNO. 
           ACCEPT AQS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE AQS-PATH-SQL    TO E-FILE.
           MOVE AQS1-KEY        TO E-KEYX.
           PERFORM SET-AQS-FIELDS.

           EXEC SQL
            INSERT INTO DBO.AQSFILE
             (AQSFILE.AQS_BRNO,
              AQSFILE.AQS_NEXT_SEQNO,
              AQSFILE.AQS_TOT_REC,
              AQSFILE.AQS_WORKED_REC,
              AQSFILE.AQS_LTOUCH_DATE)
            VALUES
             (:QAQS-BRNO,
              :QAQS-NEXT-SEQNO,
              :QAQS-TOT-REC,
              :QAQS-WORKED-REC,
              :QAQS-LTOUCH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-AQS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-AQS1-FILE.

           MOVE AQS-PATH-OWNBR TO AQS-BRNO. 
           ACCEPT AQS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE AQS-PATH-SQL    TO E-FILE.
           MOVE AQS1-KEY        TO E-KEYX.
           PERFORM SET-AQS-FIELDS.

           EXEC SQL
            UPDATE DBO.AQSFILE
             SET
              AQSFILE.AQS_BRNO = :QAQS-BRNO,
              AQSFILE.AQS_NEXT_SEQNO = :QAQS-NEXT-SEQNO,
              AQSFILE.AQS_TOT_REC = :QAQS-TOT-REC,
              AQSFILE.AQS_WORKED_REC = :QAQS-WORKED-REC,
              AQSFILE.AQS_LTOUCH_DATE = :QAQS-LTOUCH-DATE
             WHERE AQSFILE.AQS_BRNO = :QAQS-BRNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-AQS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-AQS1-FILE.

           MOVE AQS-PATH-OWNBR TO AQS-BRNO. 
           PERFORM DELETE-IT.
           MOVE AQS-PATH-SQL    TO E-FILE.
           MOVE AQS1-KEY        TO E-KEYX.
           PERFORM SET-AQS-FIELDS.

           EXEC SQL
            DELETE FROM DBO.AQSFILE
             WHERE AQSFILE.AQS_BRNO = :QAQS-BRNO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-AQS1-FILE.

      *-----------------------------------------------------------------
       CLOSE-AQS1-FILE.

           PERFORM CLOSE-IT.

           IF ( AQS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE AQS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO AQS1-CURSOR-STAT.

