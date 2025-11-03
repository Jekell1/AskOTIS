      * COPYMEMBER: LIBLP/LPIX1IN
       LOAD-IX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( IX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( IX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO IX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO IX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO IX-ALL-DATA
              MOVE IX-ALL-PATH             TO IX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( IX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO IX-ALL-BASE
              MOVE FILPATH-MACHINE         TO IX-ALL-MACHINE
              MOVE FILPATH-DATA            TO IX-ALL-DATA
              MOVE IX-ALL-PATH             TO IX-PATH.

      *-----------------------------------------------------------------
       OPEN-IX1-FILE.
           PERFORM LOAD-IX1-FILE.
           PERFORM OPEN-IT.
           MOVE IX-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-IX1-FILE.
           PERFORM START-IT.
           MOVE IX-PATH-OWNBR  TO IX-BRNO.
           MOVE IX-PATH-SQL    TO E-FILE.
           MOVE IX1-KEY        TO E-KEYX.

           IF ( IX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-IX1-FILE.

           IF ( QIX1-WEND-SEQNO = ZEROES    ) OR
              ( QIX1-WEND-SEQNO NOT NUMERIC )
              MOVE IX-BRNO              TO QIX1-WBEG-BRNO
              MOVE IX-BRNO              TO QIX1-WEND-BRNO
              MOVE IX-CODE              TO QIX1-WBEG-CODE
              MOVE ALL "Z"              TO QIX1-WEND-CODE
              MOVE IX-ACCTNO            TO QIX1-WBEG-ACCTNO
              MOVE ALL "9"              TO QIX1-WEND-ACCTNO
              MOVE IX-SEQNO             TO QIX1-WBEG-SEQNO
              MOVE ALL "9"              TO QIX1-WEND-SEQNO.

           EXEC SQL
            DECLARE IX1_CURSOR CURSOR FOR
            SELECT IXFILE.IX_BRNO,
                   IXFILE.IX_CODE,
                   IXFILE.IX_ACCTNO,
                   IXFILE.IX_SEQNO,
                   IXFILE.IX_VIN,
                   CAST(IXFILE.IX_LTOUCH_DATE
                                                  AS VARCHAR(10))
            FROM DBO.IXFILE
            WHERE IXFILE.IX_BRNO  = :QIX1-WBEG-BRNO
              AND IXFILE.IX_CODE >= :QIX1-WBEG-CODE
              AND IXFILE.IX_CODE <= :QIX1-WEND-CODE
              AND IXFILE.IX_ACCTNO >= :QIX1-WBEG-ACCTNO
              AND IXFILE.IX_ACCTNO <= :QIX1-WEND-ACCTNO
              AND IXFILE.IX_SEQNO >= :QIX1-WBEG-SEQNO
              AND IXFILE.IX_SEQNO <= :QIX1-WEND-SEQNO
            ORDER BY IXFILE.IX_BRNO,
                     IXFILE.IX_CODE,
                     IXFILE.IX_ACCTNO,
                     IXFILE.IX_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN IX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QIX1-WBEG-CODE 
                          QIX1-WEND-CODE.

           MOVE ZEROES TO QIX1-WBEG-BRNO 
                          QIX1-WEND-BRNO
                          QIX1-WBEG-ACCTNO
                          QIX1-WEND-ACCTNO
                          QIX1-WBEG-SEQNO
                          QIX1-WEND-SEQNO.

           MOVE STAT---GOOD TO IX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-IX1-FILE.
           PERFORM READ-IT.
           MOVE IX-PATH-OWNBR  TO IX-BRNO. 
           MOVE IX-PATH-SQL    TO E-FILE.
           MOVE IX1-KEY        TO E-KEYX.
           INITIALIZE QIX-REC.

           MOVE IX-BRNO   TO QIX-BRNO.
           MOVE IX-CODE   TO QIX-CODE.
           MOVE IX-ACCTNO TO QIX-ACCTNO.
           MOVE IX-SEQNO  TO QIX-SEQNO.

           EXEC SQL
            SELECT IXFILE.IX_BRNO,
                   IXFILE.IX_CODE,
                   IXFILE.IX_ACCTNO,
                   IXFILE.IX_SEQNO,
                   IXFILE.IX_VIN,
                   CAST(IXFILE.IX_LTOUCH_DATE
                                                  AS VARCHAR(10))
            INTO :QIX-REC
            FROM DBO.IXFILE
            WHERE IXFILE.IX_BRNO = :QIX-BRNO
              AND IXFILE.IX_CODE = :QIX-CODE
              AND IXFILE.IX_ACCTNO = :QIX-ACCTNO
              AND IXFILE.IX_SEQNO = :QIX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-IX-FIELDS.

      *-----------------------------------------------------------------
       READ-IX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE IX-PATH-OWNBR  TO IX-BRNO.
           MOVE IX-PATH-SQL    TO E-FILE.
           MOVE IX1-KEY        TO E-KEYX.
           INITIALIZE QIX-REC.

           IF ( IX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH IX1_CURSOR INTO :QIX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-IX-FIELDS.

      *-----------------------------------------------------------------
       WRITE-IX1-FILE.

           MOVE IX-PATH-OWNBR  TO IX-BRNO.
           ACCEPT IX-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE IX-PATH-SQL    TO E-FILE.
           MOVE IX1-KEY        TO E-KEYX.
           PERFORM SET-IX-FIELDS.

           EXEC SQL
            INSERT INTO DBO.IXFILE
             (IXFILE.IX_BRNO,
              IXFILE.IX_CODE,
              IXFILE.IX_ACCTNO,
              IXFILE.IX_SEQNO,
              IXFILE.IX_VIN,
              IXFILE.IX_LTOUCH_DATE)
            VALUES
             (:QIX-BRNO,
              :QIX-CODE,
              :QIX-ACCTNO,
              :QIX-SEQNO,
              :QIX-VIN,
              :QIX-LTOUCH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-IX1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-IX1-FILE.

           MOVE IX-PATH-OWNBR  TO IX-BRNO.
           ACCEPT IX-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE IX-PATH-SQL    TO E-FILE.
           MOVE IX1-KEY        TO E-KEYX.
           PERFORM SET-IX-FIELDS.

           EXEC SQL
            UPDATE DBO.IXFILE
             SET
              IXFILE.IX_BRNO = :QIX-BRNO,
              IXFILE.IX_CODE = :QIX-CODE,
              IXFILE.IX_ACCTNO = :QIX-ACCTNO,
              IXFILE.IX_SEQNO = :QIX-SEQNO,
              IXFILE.IX_VIN = :QIX-VIN,
              IXFILE.IX_LTOUCH_DATE = :QIX-LTOUCH-DATE
             WHERE IXFILE.IX_BRNO = :QIX-BRNO
               AND IXFILE.IX_CODE = :QIX-CODE
               AND IXFILE.IX_ACCTNO = :QIX-ACCTNO
               AND IXFILE.IX_SEQNO = :QIX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-IX1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-IX1-FILE.

           MOVE IX-PATH-OWNBR  TO IX-BRNO.
           PERFORM DELETE-IT.
           MOVE IX-PATH-SQL    TO E-FILE.
           MOVE IX1-KEY        TO E-KEYX.
           PERFORM SET-IX-FIELDS.

           EXEC SQL
            DELETE FROM DBO.IXFILE
             WHERE IXFILE.IX_BRNO = :QIX-BRNO
               AND IXFILE.IX_CODE = :QIX-CODE
               AND IXFILE.IX_ACCTNO = :QIX-ACCTNO
               AND IXFILE.IX_SEQNO = :QIX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-IX1-FILE.

      *-----------------------------------------------------------------
       CLOSE-IX1-FILE.

           PERFORM CLOSE-IT.

           IF ( IX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE IX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO IX1-CURSOR-STAT.

