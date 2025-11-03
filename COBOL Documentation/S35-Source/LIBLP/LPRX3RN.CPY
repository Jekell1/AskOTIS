      * COPYMEMBER: LIBLP/LPRX3RN
      *=================================================================
      *    IO-ROUTINES FOR RX-FILE
      *=================================================================
       LOAD-RX3-FILE.
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
       OPEN-RX3-FILE.
           PERFORM LOAD-RX3-FILE.
           PERFORM OPEN-IT.
           MOVE RX-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RX3-FILE.
           PERFORM START-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           MOVE RX-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING RX-BRNO, RX-LNAME, RX-FNAME
                           DELIMITED BY SIZE INTO E-KEYX.

           IF ( RX3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RX3-FILE.

           IF ( QRX3-WEND-BRNO = ZEROES    ) OR
              ( QRX3-WEND-BRNO NOT NUMERIC )
              MOVE RX-PATH-OWNBR        TO QRX3-WBEG-BRNO
              MOVE RX-PATH-OWNBR        TO QRX3-WEND-BRNO
              MOVE RX-DLNO              TO QRX3-WBEG-DLNO
              MOVE ALL "9"              TO QRX3-WEND-DLNO
              MOVE RX-INTDATE           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRX3-WBEG-INTDATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRX3-WEND-INTDATE
              MOVE RX-LNAME            TO QRX3-WBEG-LNAME
              MOVE ALL "Z"              TO QRX3-WEND-LNAME
              MOVE RX-FNAME            TO QRX3-WBEG-FNAME
              MOVE ALL "Z"              TO QRX3-WEND-FNAME.

           EXEC SQL
            DECLARE RX3_CURSOR CURSOR FOR
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
            WHERE RXFILE.RX_BRNO  = :QRX3-WBEG-BRNO
              AND RXFILE.RX_DLNO >= :QRX3-WBEG-DLNO
              AND RXFILE.RX_DLNO <= :QRX3-WEND-DLNO
              AND RXFILE.RX_INTDATE >= :QRX3-WBEG-INTDATE
              AND RXFILE.RX_INTDATE <= :QRX3-WEND-INTDATE
              AND RXFILE.RX_LNAME >= :QRX3-WBEG-LNAME
              AND RXFILE.RX_LNAME <= :QRX3-WEND-LNAME
              AND RXFILE.RX_FNAME >= :QRX3-WBEG-FNAME
              AND RXFILE.RX_FNAME <= :QRX3-WEND-FNAME
            ORDER BY RXFILE.RX_BRNO,
                     RXFILE.RX_DLNO,
                     RXFILE.RX_INTDATE,
                     RXFILE.RX_LNAME,
                     RXFILE.RX_FNAME,
                     RXFILE.RX_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN RX3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRX3-WBEG-BRNO 
                          QRX3-WEND-BRNO
                          QRX3-WBEG-DLNO
                          QRX3-WEND-DLNO.

           MOVE SPACES TO QRX3-WBEG-LNAME
                          QRX3-WEND-LNAME
                          QRX3-WBEG-FNAME
                          QRX3-WEND-FNAME
                          QRX3-WBEG-INTDATE
                          QRX3-WEND-INTDATE.
 
           MOVE STAT---GOOD TO RX3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RX3-FILE.
           PERFORM READ-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO. 
           MOVE RX-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING RX-BRNO, RX-LNAME, RX-FNAME
                           DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QRX-REC.
           
           MOVE RX-BRNO             TO QRX-BRNO.
           MOVE RX-DLNO             TO QRX-DLNO.
           MOVE RX-INTDATE          TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRX-INTDATE.
           MOVE RX-LNAME            TO QRX-LNAME.
           MOVE RX-FNAME            TO QRX-FNAME.

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
              AND RXFILE.RX_DLNO = :QRX-DLNO
              AND RXFILE.RX_INTDATE = :QRX-INTDATE
              AND RXFILE.RX_LNAME = :QRX-LNAME
              AND RXFILE.RX_FNAME = :QRX-FNAME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RX3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RX-FIELDS.

      *-----------------------------------------------------------------
       READ-RX3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           MOVE RX-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING RX-BRNO, RX-LNAME, RX-FNAME
                           DELIMITED BY SIZE INTO E-KEYX.

           INITIALIZE QRX-REC.

           IF ( RX3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RX3_CURSOR INTO :QRX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RX3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-RX3-FILE.

           PERFORM CLOSE-IT.
           IF ( RX3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RX3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RX3-CURSOR-STAT.

