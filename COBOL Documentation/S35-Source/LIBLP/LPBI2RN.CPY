      * COPYMEMBER: LIBLP/LPBI2RN
      *=================================================================
       LOAD-BI2-FILE.
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
       OPEN-BI2-FILE.
           PERFORM LOAD-BI2-FILE.
           PERFORM OPEN-IT.
           MOVE BI-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BI2-FILE.
           PERFORM START-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI2-KEY TO E-KEYX.

           IF ( BI2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BI2-FILE.

           IF ( QBI2-WEND-FNAME = SPACES )
              MOVE BI-LNAME       TO QBI2-WBEG-LNAME
              MOVE ALL "Z"        TO QBI2-WEND-LNAME
              MOVE BI-FNAME       TO QBI2-WBEG-FNAME
              MOVE ALL "Z"        TO QBI2-WEND-FNAME.

           EXEC SQL
            DECLARE BI2_CURSOR CURSOR FOR
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
            WHERE BIFILE.BI_LNAME >= :QBI2-WBEG-LNAME
              AND BIFILE.BI_LNAME <= :QBI2-WEND-LNAME
              AND BIFILE.BI_FNAME >= :QBI2-WBEG-FNAME
              AND BIFILE.BI_FNAME <= :QBI2-WEND-FNAME
            ORDER BY BIFILE.BI_LNAME, BIFILE.BI_FNAME
           END-EXEC.

           EXEC SQL
               OPEN BI2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QBI2-WBEG-LNAME
                          QBI2-WEND-LNAME
                          QBI2-WBEG-FNAME
                          QBI2-WEND-FNAME.

           MOVE STAT---GOOD TO BI2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BI2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI2-KEY TO E-KEYX.
           INITIALIZE QBI-REC.

           IF ( BI2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BI2_CURSOR INTO :QBI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BI2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BI-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BI2-FILE.
           PERFORM CLOSE-IT.
           IF ( BI2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BI2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BI2-CURSOR-STAT.

