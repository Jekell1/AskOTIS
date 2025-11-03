      * COPYMEMBER: LIBLP/LPFM1IN
       LOAD-FM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( FM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( FM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO FM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO FM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO FM-ALL-DATA
              MOVE FM-ALL-PATH             TO FM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( FM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO FM-ALL-BASE
              MOVE FILPATH-MACHINE         TO FM-ALL-MACHINE
              MOVE FILPATH-DATA            TO FM-ALL-DATA
              MOVE FM-ALL-PATH             TO FM-PATH.

      *-----------------------------------------------------------------
       OPEN-FM1-FILE.
           PERFORM LOAD-FM1-FILE.
           PERFORM OPEN-IT.
           MOVE FM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-FM1-FILE.
           PERFORM START-IT.
           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.

           IF ( FM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-FM1-FILE.

           IF ( QFM1-WEND-FMNO = ZEROES    ) OR
              ( QFM1-WEND-FMNO NOT NUMERIC )
              MOVE FM-PATH-OWNBR        TO QFM1-WBEG-BRNO
              MOVE FM-PATH-OWNBR        TO QFM1-WEND-BRNO
              MOVE FM-INITKEY           TO QFM1-WBEG-INITKEY
              MOVE ALL "Z"              TO QFM1-WEND-INITKEY
              MOVE FM-FMNO              TO QFM1-WBEG-FMNO
              MOVE ALL "9"              TO QFM1-WEND-FMNO.

           EXEC SQL
            DECLARE FM1_CURSOR CURSOR FOR
            SELECT FMFILE.FM_BRNO,
                   FMFILE.FM_INITKEY,
                   FMFILE.FM_FMNO,
                   FMFILE.FM_ZIP,
                   FMFILE.FM_PRINT,
                   FMFILE.FM_STMT_COUNTER,
                   CAST(FMFILE.FM_TRDATE AS VARCHAR(10)),
                   FMFILE.FM_USERID,
                   CAST(FMFILE.FM_LTOUCH_DATE
                                                  AS VARCHAR(10))
            FROM DBO.FMFILE
            WHERE FMFILE.FM_BRNO = :QFM1-WBEG-BRNO
              AND FMFILE.FM_INITKEY >= :QFM1-WBEG-INITKEY
              AND FMFILE.FM_INITKEY <= :QFM1-WEND-INITKEY
              AND FMFILE.FM_FMNO >= :QFM1-WBEG-FMNO
              AND FMFILE.FM_FMNO <= :QFM1-WEND-FMNO
            ORDER BY FMFILE.FM_BRNO,
                     FMFILE.FM_INITKEY,
                     FMFILE.FM_FMNO
           END-EXEC.

           EXEC SQL
               OPEN FM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QFM1-WBEG-BRNO 
                          QFM1-WEND-BRNO
                          QFM1-WBEG-FMNO
                          QFM1-WEND-FMNO.
           MOVE SPACES TO QFM1-WBEG-INITKEY
                          QFM1-WEND-INITKEY.
                            

           MOVE STAT---GOOD TO FM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-FM1-FILE.
           PERFORM READ-IT.
           MOVE FM-PATH-OWNBR  TO FM-BRNO. 
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.
           INITIALIZE QFM-REC.

           MOVE FM-BRNO    TO QFM-BRNO.
           MOVE FM-INITKEY TO QFM-INITKEY.
           MOVE FM-FMNO    TO QFM-FMNO.

           EXEC SQL
            SELECT FMFILE.FM_BRNO,
                   FMFILE.FM_INITKEY,
                   FMFILE.FM_FMNO,
                   FMFILE.FM_ZIP,
                   FMFILE.FM_PRINT,
                   FMFILE.FM_STMT_COUNTER,
                   CAST(FMFILE.FM_TRDATE AS VARCHAR(10)),
                   FMFILE.FM_USERID,
                   CAST(FMFILE.FM_LTOUCH_DATE
                                                  AS VARCHAR(10))
            INTO :QFM-REC
            FROM DBO.FMFILE
            WHERE FMFILE.FM_BRNO = :QFM-BRNO
              AND FMFILE.FM_INITKEY = :QFM-INITKEY
              AND FMFILE.FM_FMNO = :QFM-FMNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-FM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-FM-FIELDS.

      *-----------------------------------------------------------------
       READ-FM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.
           INITIALIZE QFM-REC.

           IF ( FM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH FM1_CURSOR INTO :QFM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-FM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-FM-FIELDS.

      *-----------------------------------------------------------------
       WRITE-FM1-FILE.

           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           ACCEPT FM-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.
           PERFORM SET-FM-FIELDS.

           EXEC SQL
            INSERT INTO DBO.FMFILE
             (FMFILE.FM_BRNO,
              FMFILE.FM_INITKEY,
              FMFILE.FM_FMNO,
              FMFILE.FM_ZIP,
              FMFILE.FM_PRINT,
              FMFILE.FM_STMT_COUNTER,
              FMFILE.FM_TRDATE,
              FMFILE.FM_USERID,
              FMFILE.FM_LTOUCH_DATE)
            VALUES
             (:QFM-BRNO,
              :QFM-INITKEY,
              :QFM-FMNO,
              :QFM-ZIP,
              :QFM-PRINT,
              :QFM-STMT-COUNTER,
              :QFM-TRDATE,
              :QFM-USERID,
              :QFM-LTOUCH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-FM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-FM1-FILE.

           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           ACCEPT FM-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.
           PERFORM SET-FM-FIELDS.

           EXEC SQL
            UPDATE DBO.FMFILE
             SET
              FMFILE.FM_BRNO = :QFM-BRNO,
              FMFILE.FM_INITKEY = :QFM-INITKEY,
              FMFILE.FM_FMNO = :QFM-FMNO,
              FMFILE.FM_ZIP = :QFM-ZIP,
              FMFILE.FM_PRINT = :QFM-PRINT,
              FMFILE.FM_STMT_COUNTER = :QFM-STMT-COUNTER,
              FMFILE.FM_TRDATE = :QFM-TRDATE,
              FMFILE.FM_USERID = :QFM-USERID,
              FMFILE.FM_LTOUCH_DATE = :QFM-LTOUCH-DATE
            WHERE FMFILE.FM_BRNO = :QFM-BRNO
              AND FMFILE.FM_INITKEY = :QFM-INITKEY
              AND FMFILE.FM_FMNO = :QFM-FMNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-FM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-FM1-FILE.

           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           PERFORM DELETE-IT.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.
           PERFORM SET-FM-FIELDS.

           EXEC SQL
            DELETE FROM DBO.FMFILE
             WHERE FMFILE.FM_BRNO = :QFM-BRNO
               AND FMFILE.FM_INITKEY = :QFM-INITKEY
               AND FMFILE.FM_FMNO = :QFM-FMNO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-FM1-FILE.

      *-----------------------------------------------------------------
       CLOSE-FM1-FILE.

           PERFORM CLOSE-IT.

           IF ( FM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE FM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO FM1-CURSOR-STAT.

