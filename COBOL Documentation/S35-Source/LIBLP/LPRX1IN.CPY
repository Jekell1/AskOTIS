      * COPYMEMBER: LIBLP/LPRX1IN
      *=================================================================
      *    IO-ROUTINES FOR RX-FILE
      *=================================================================
       LOAD-RX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO RX-ALL-DATA
              MOVE RX-ALL-PATH             TO RX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RX-ALL-BASE
              MOVE FILPATH-MACHINE         TO RX-ALL-MACHINE
              MOVE FILPATH-DATA            TO RX-ALL-DATA
              MOVE RX-ALL-PATH             TO RX-PATH.

      *-----------------------------------------------------------------
       OPEN-RX1-FILE.
           PERFORM LOAD-RX1-FILE.
           PERFORM OPEN-IT.
           MOVE RX-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RX1-FILE.
           PERFORM START-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           MOVE RX-PATH-SQL    TO E-FILE.
           MOVE RX1-KEY        TO E-KEYX.

           IF ( RX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RX1-FILE.

           IF ( QRX1-WEND-ACCTNO = ZEROES    ) OR
              ( QRX1-WEND-ACCTNO NOT NUMERIC )
              MOVE RX-PATH-OWNBR        TO QRX1-WBEG-BRNO
              MOVE RX-PATH-OWNBR        TO QRX1-WEND-BRNO
              MOVE RX-ACCTNO            TO QRX1-WBEG-ACCTNO
              MOVE ALL "9"              TO QRX1-WEND-ACCTNO.

           EXEC SQL
            DECLARE RX1_CURSOR CURSOR FOR
            SELECT RXFILE.RX_BRNO,
                   RXFILE.RX_ACCTNO,
                   RXFILE.RX_DLNO,
                   CAST(RXFILE.RX_INTDATE AS VARCHAR(10)),
                   RXFILE.RX_LNAME,
                   RXFILE.RX_FNAME,
                   CAST(RXFILE.RX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(RXFILE.RX_LOANDATE AS VARCHAR(10)),
                   RXFILE.RX_LOANAMT,
                   RXFILE.RX_PROCEEDS,
                   RXFILE.RX_STATUS
            FROM DBO.RXFILE
            WHERE RXFILE.RX_BRNO  = :QRX1-WBEG-BRNO
              AND RXFILE.RX_ACCTNO >= :QRX1-WBEG-ACCTNO
              AND RXFILE.RX_ACCTNO <= :QRX1-WEND-ACCTNO
            ORDER BY RXFILE.RX_BRNO,
                     RXFILE.RX_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN RX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRX1-WBEG-BRNO 
                          QRX1-WEND-BRNO
                          QRX1-WBEG-ACCTNO
                          QRX1-WEND-ACCTNO.
 
           MOVE STAT---GOOD TO RX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RX1-FILE.
           PERFORM READ-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO. 
           MOVE RX-PATH-SQL    TO E-FILE.
           MOVE RX1-KEY        TO E-KEYX.
           INITIALIZE QRX-REC.

           MOVE RX-BRNO   TO QRX-BRNO.
           MOVE RX-ACCTNO TO QRX-ACCTNO.

           EXEC SQL
            SELECT RXFILE.RX_BRNO,
                   RXFILE.RX_ACCTNO,
                   RXFILE.RX_DLNO,
                   CAST(RXFILE.RX_INTDATE AS VARCHAR(10)),
                   RXFILE.RX_LNAME,
                   RXFILE.RX_FNAME,
                   CAST(RXFILE.RX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(RXFILE.RX_LOANDATE AS VARCHAR(10)),
                   RXFILE.RX_LOANAMT,
                   RXFILE.RX_PROCEEDS,
                   RXFILE.RX_STATUS
            INTO :QRX-REC
            FROM DBO.RXFILE
            WHERE RXFILE.RX_BRNO = :QRX-BRNO
              AND RXFILE.RX_ACCTNO = :QRX-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RX-FIELDS.

      *-----------------------------------------------------------------
       READ-RX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           MOVE RX-PATH-SQL    TO E-FILE.
           MOVE RX1-KEY        TO E-KEYX.
           INITIALIZE QRX-REC.

           IF ( RX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RX1_CURSOR INTO :QRX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RX-FIELDS.

      *-----------------------------------------------------------------
       WRITE-RX1-FILE.

           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           ACCEPT RX-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE RX-PATH-SQL    TO E-FILE.
           MOVE RX1-KEY        TO E-KEYX.
           PERFORM SET-RX-FIELDS.

           EXEC SQL
            INSERT INTO DBO.RXFILE
             (RXFILE.RX_BRNO,
              RXFILE.RX_ACCTNO,
              RXFILE.RX_DLNO,
              RXFILE.RX_INTDATE,
              RXFILE.RX_LNAME,
              RXFILE.RX_FNAME,
              RXFILE.RX_LTOUCH_DATE,
              RXFILE.RX_LOANDATE,
              RXFILE.RX_LOANAMT,
              RXFILE.RX_PROCEEDS,
              RXFILE.RX_STATUS)
            VALUES
             (:QRX-BRNO,
              :QRX-ACCTNO,
              :QRX-DLNO,
              :QRX-INTDATE,
              :QRX-LNAME,
              :QRX-FNAME,
              :QRX-LTOUCH-DATE,
              :QRX-LOANDATE,
              :QRX-LOANAMT,
              :QRX-PROCEEDS,
              :QRX-STATUS)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-RX1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-RX1-FILE.

           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           ACCEPT RX-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE RX-PATH-SQL    TO E-FILE.
           MOVE RX1-KEY        TO E-KEYX.
           PERFORM SET-RX-FIELDS.

           EXEC SQL
            UPDATE DBO.RXFILE
             SET
              RXFILE.RX_BRNO = :QRX-BRNO,
              RXFILE.RX_ACCTNO = :QRX-ACCTNO,
              RXFILE.RX_DLNO = :QRX-DLNO,
              RXFILE.RX_INTDATE = :QRX-INTDATE,
              RXFILE.RX_LNAME = :QRX-LNAME,
              RXFILE.RX_FNAME = :QRX-FNAME,
              RXFILE.RX_LTOUCH_DATE = :QRX-LTOUCH-DATE,
              RXFILE.RX_LOANDATE = :QRX-LOANDATE,
              RXFILE.RX_LOANAMT = :QRX-LOANAMT,
              RXFILE.RX_PROCEEDS = :QRX-PROCEEDS,
              RXFILE.RX_STATUS = :QRX-STATUS
            WHERE RXFILE.RX_BRNO = :QRX-BRNO
              AND RXFILE.RX_ACCTNO = :QRX-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-RX1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-RX1-FILE.

           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           PERFORM DELETE-IT.
           MOVE RX-PATH-SQL    TO E-FILE.
           MOVE RX1-KEY        TO E-KEYX.
           PERFORM SET-RX-FIELDS.

           EXEC SQL
            DELETE FROM DBO.RXFILE
            WHERE RXFILE.RX_BRNO = :QRX-BRNO
              AND RXFILE.RX_ACCTNO = :QRX-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-RX1-FILE.

      *-----------------------------------------------------------------
       CLOSE-RX1-FILE.

           PERFORM CLOSE-IT.

           IF ( RX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RX1-CURSOR-STAT.

