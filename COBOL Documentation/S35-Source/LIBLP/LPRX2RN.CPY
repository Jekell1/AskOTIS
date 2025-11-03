      * COPYMEMBER: LIBLP/LPRX2RN
      *=================================================================
      *    IO-ROUTINES FOR RX-FILE
      *=================================================================
       LOAD-RX2-FILE.
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
       OPEN-RX2-FILE.
           PERFORM LOAD-RX2-FILE.
           PERFORM OPEN-IT.
           MOVE RX-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RX2-FILE.
           PERFORM START-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           MOVE RX-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING RX-BRNO, RX-LNAME, RX-FNAME
                  DELIMITED BY SIZE INTO E-KEYX.

           IF ( RX2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RX2-FILE.

           IF ( QRX2-WEND-BRNO = ZEROES    ) OR
              ( QRX2-WEND-BRNO NOT NUMERIC )
            MOVE RX-PATH-OWNBR        TO QRX2-WBEG-BRNO
            MOVE RX-PATH-OWNBR        TO QRX2-WEND-BRNO
            MOVE RX-LNAME             TO QRX2-WBEG-LNAME
            MOVE ALL "Z"              TO QRX2-WEND-LNAME
            MOVE RX-FNAME             TO QRX2-WBEG-FNAME
            MOVE ALL "Z"              TO QRX2-WEND-FNAME.

           EXEC SQL
            DECLARE RX2_CURSOR CURSOR FOR
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
            WHERE RXFILE.RX_BRNO  = :QRX2-WBEG-BRNO
              AND RXFILE.RX_LNAME >= :QRX2-WBEG-LNAME
              AND RXFILE.RX_LNAME <= :QRX2-WEND-LNAME
              AND RXFILE.RX_FNAME >= :QRX2-WBEG-FNAME
              AND RXFILE.RX_FNAME <= :QRX2-WEND-FNAME
            ORDER BY RXFILE.RX_BRNO,
                     RXFILE.RX_LNAME,
                     RXFILE.RX_FNAME,
                     RXFILE.RX_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN RX2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRX2-WBEG-BRNO 
                          QRX2-WEND-BRNO.

           MOVE SPACES TO QRX2-WBEG-LNAME
                          QRX2-WEND-LNAME
                          QRX2-WBEG-FNAME
                          QRX2-WEND-FNAME.
 
           MOVE STAT---GOOD TO RX2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RX2-FILE.
           PERFORM READ-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO. 
           MOVE RX-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING RX-BRNO, RX-LNAME, RX-FNAME
                  DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QRX-REC.

           MOVE RX-BRNO   TO QRX-BRNO.
           MOVE RX-LNAME TO QRX-LNAME.
           MOVE RX-FNAME TO QRX-FNAME.

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
              AND RXFILE.RX_LNAME = :QRX-LNAME
              AND RXFILE.RX_FNAME = :QRX-FNAME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RX2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RX-FIELDS.

      *-----------------------------------------------------------------
       READ-RX2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           MOVE RX-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING RX-BRNO, RX-LNAME, RX-FNAME
                  DELIMITED BY SIZE INTO E-KEYX.

           INITIALIZE QRX-REC.

           IF ( RX2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RX2_CURSOR INTO :QRX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RX2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-RX2-FILE.

           PERFORM CLOSE-IT.
           IF ( RX2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RX2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RX2-CURSOR-STAT.

