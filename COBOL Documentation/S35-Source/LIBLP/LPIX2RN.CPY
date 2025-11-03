      * COPYMEMBER: LIBLP/LPIX2RN
       LOAD-IX2-FILE.
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
       OPEN-IX2-FILE.
           PERFORM LOAD-IX2-FILE.
           PERFORM OPEN-IT.
           MOVE IX-PATH-SQL TO E-FILE.
           MOVE SPACES TO E-KEYX.
           STRING IX-BRNO, IX-VIN
              DELIMITED BY SIZE INTO E-KEYX.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-IX2-FILE.
           PERFORM START-IT.
           MOVE IX-PATH-OWNBR  TO IX-BRNO.
           MOVE IX-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING IX-BRNO, IX-VIN DELIMITED BY SIZE INTO E-KEYX.

           IF ( IX2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-IX2-FILE.

           IF ( QIX2-WEND-BRNO = ZEROES    ) OR
              ( QIX2-WEND-BRNO NOT NUMERIC )
              MOVE IX-PATH-OWNBR        TO QIX2-WBEG-BRNO
              MOVE IX-PATH-OWNBR        TO QIX2-WEND-BRNO
              MOVE IX-VIN               TO QIX2-WBEG-VIN
              MOVE ALL "Z"              TO QIX2-WEND-VIN.

           EXEC SQL
            DECLARE IX2_CURSOR CURSOR FOR
            SELECT IXFILE.IX_BRNO,
                   IXFILE.IX_CODE,
                   IXFILE.IX_ACCTNO,
                   IXFILE.IX_SEQNO,
                   IXFILE.IX_VIN,
                   CAST(IXFILE.IX_LTOUCH_DATE
                                                  AS VARCHAR(10))
            FROM DBO.IXFILE
            WHERE IXFILE.IX_BRNO = :QIX2-WBEG-BRNO
              AND IXFILE.IX_VIN >= :QIX2-WBEG-VIN
              AND IXFILE.IX_VIN <= :QIX2-WEND-VIN
            ORDER BY IXFILE.IX_BRNO,
                     IXFILE.IX_VIN
           END-EXEC.

           EXEC SQL
               OPEN IX2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QIX2-WBEG-VIN 
                          QIX2-WEND-VIN.

           MOVE ZEROES TO QIX2-WBEG-BRNO 
                          QIX2-WEND-BRNO.

           MOVE STAT---GOOD TO IX2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-IX2-FILE.
           PERFORM READ-IT.
           MOVE IX-PATH-OWNBR  TO IX-BRNO. 
           MOVE IX-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING IX-BRNO, IX-VIN DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QIX-REC. 

           MOVE IX-BRNO   TO QIX-BRNO.
           MOVE IX-VIN    TO QIX-VIN.

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
              AND IXFILE.IX_VIN = :QIX-VIN 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IX2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-IX-FIELDS.

      *------------------------------------------------------
       READ-IX2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE IX-PATH-OWNBR  TO IX-BRNO.
           MOVE IX-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING IX-BRNO, IX-VIN DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QIX-REC.

           IF ( IX2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH IX2_CURSOR INTO :QIX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IX2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-IX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-IX2-FILE.

           PERFORM CLOSE-IT.
           IF ( IX2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE IX2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO IX2-CURSOR-STAT.
 
