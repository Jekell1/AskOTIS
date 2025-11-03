      * COPYMEMBER: LIBLP/LPBI1RN
       LOAD-BI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BI-ALL-BASE
              MOVE BI-ALL-PATH             TO BI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BI-ALL-BASE
              MOVE BI-ALL-PATH             TO BI-PATH.

      *-----------------------------------------------------------------
       OPEN-BI1-FILE.
           PERFORM LOAD-BI1-FILE.
           PERFORM OPEN-IT.
           MOVE BI-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BI1-FILE.
           PERFORM START-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.

           IF ( BI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BI1-FILE.

           IF ( QBI1-WEND-OWNBR = ZEROES    ) OR
              ( QBI1-WEND-OWNBR NOT NUMERIC )
              MOVE BI-SSNO        TO QBI1-WBEG-SSNO
              MOVE ALL "9"        TO QBI1-WEND-SSNO
              MOVE BI-ACCTNO      TO QBI1-WBEG-ACCTNO
              MOVE ALL "9"        TO QBI1-WEND-ACCTNO
              MOVE BI-OWNBR       TO QBI1-WBEG-OWNBR
              MOVE ALL "9"        TO QBI1-WEND-OWNBR.

           EXEC SQL
            DECLARE BI1_CURSOR CURSOR FOR
            SELECT BIFILE.BI_SSNO,
                   BIFILE.BI_ACCTNO,
                   BIFILE.BI_OWNBR,
                   BIFILE.BI_LNAME,
                   BIFILE.BI_FNAME,
                   CAST(BIFILE.BI_LTOUCH_DATE AS VARCHAR(10)),
                   BIFILE.BI_MAKERCD,
                   BIFILE.BI_ORGST,
                   BIFILE.BI_GIFTNET,
                   CAST(BIFILE.BI_POFFDATE AS VARCHAR(10)),
                   CAST(BIFILE.BI_ENTDATE AS VARCHAR(10)),
                   BIFILE.BI_SALEFIN,
                   BIFILE.BI_CONVERCD,
                   BIFILE.BI_PLCD,
                   BIFILE.BI_SMALLOAN,
                   CAST(BIFILE.BI_PLDATE AS VARCHAR(10))
            FROM DBO.BIFILE
            WHERE BIFILE.BI_SSNO >= :QBI1-WBEG-SSNO
              AND BIFILE.BI_SSNO <= :QBI1-WEND-SSNO
              AND BIFILE.BI_ACCTNO >= :QBI1-WBEG-ACCTNO
              AND BIFILE.BI_ACCTNO <= :QBI1-WEND-ACCTNO
              AND BIFILE.BI_OWNBR >= :QBI1-WBEG-OWNBR
              AND BIFILE.BI_OWNBR <= :QBI1-WEND-OWNBR
            ORDER BY BIFILE.BI_SSNO, BIFILE.BI_ACCTNO,
                       BIFILE.BI_OWNBR
           END-EXEC.

           EXEC SQL
               OPEN BI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBI1-WBEG-SSNO
                          QBI1-WEND-SSNO
                          QBI1-WBEG-ACCTNO
                          QBI1-WEND-ACCTNO
                          QBI1-WBEG-OWNBR
                          QBI1-WEND-OWNBR.

           MOVE STAT---GOOD TO BI1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BI1-FILE.
           PERFORM READ-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.
           INITIALIZE QBI-REC.

           MOVE BI-SSNO     TO QBI-SSNO.
           MOVE BI-ACCTNO   TO QBI-ACCTNO.
           MOVE BI-OWNBR    TO QBI-OWNBR.

           EXEC SQL
            SELECT BIFILE.BI_SSNO,
                   BIFILE.BI_ACCTNO,
                   BIFILE.BI_OWNBR,
                   BIFILE.BI_LNAME,
                   BIFILE.BI_FNAME,
                   CAST(BIFILE.BI_LTOUCH_DATE AS VARCHAR(10)),
                   BIFILE.BI_MAKERCD,
                   BIFILE.BI_ORGST,
                   BIFILE.BI_GIFTNET,
                   CAST(BIFILE.BI_POFFDATE AS VARCHAR(10)),
                   CAST(BIFILE.BI_ENTDATE AS VARCHAR(10)),
                   BIFILE.BI_SALEFIN,
                   BIFILE.BI_CONVERCD,
                   BIFILE.BI_PLCD,
                   BIFILE.BI_SMALLOAN,
                   CAST(BIFILE.BI_PLDATE AS VARCHAR(10))
            INTO :QBI-REC
            FROM DBO.BIFILE
            WHERE BIFILE.BI_SSNO = :QBI-SSNO
              AND BIFILE.BI_ACCTNO = :QBI-ACCTNO
              AND BIFILE.BI_OWNBR = :QBI-OWNBR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BI1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BI-FIELDS.

      *-----------------------------------------------------------------
       READ-BI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.
           INITIALIZE QBI-REC.

           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BI1_CURSOR INTO :QBI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BI-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BI1-FILE.
           PERFORM CLOSE-IT.
           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BI1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BI1-CURSOR-STAT.

