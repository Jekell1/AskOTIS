      * COPYMEMBER: LIBSP/SPBB2RN
      *-----------------------------------------------------------------
       LOAD-BB2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BB-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO BB-PATH-MACHINE
              MOVE BB-PATH-FORMAT          TO BB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BB-PATH-BASE
              MOVE FILPATH-MACHINE         TO BB-PATH-MACHINE
              MOVE BB-PATH-FORMAT          TO BB-PATH.

      *-----------------------------------------------------------------
       OPEN-BB2-FILE.
           PERFORM LOAD-BB2-FILE.
           PERFORM OPEN-IT.
           MOVE BB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BB2-FILE.
           PERFORM START-IT.
           MOVE BB-PATH-SQL TO E-FILE.
           MOVE SPACES TO E-KEYX.
           STRING BB-CATAGORY, BB-SUBCATAGORY, BB-NAME
                DELIMITED BY SIZE INTO E-KEYX.

           IF ( BB2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BB2-FILE.

           IF ( QBB2-WEND-CATAGORY = SPACES )
              MOVE BB-CATAGORY    TO QBB2-WBEG-CATAGORY
              MOVE ALL "Z"        TO QBB2-WEND-CATAGORY
              MOVE BB-SUBCATAGORY TO QBB2-WBEG-SUBCAT
              MOVE ALL "Z"        TO QBB2-WEND-SUBCAT
              MOVE BB-NAME        TO QBB2-WBEG-NAME
              MOVE ALL "Z"        TO QBB2-WEND-NAME.

           EXEC SQL
            DECLARE BB2_CURSOR CURSOR FOR
            SELECT BBFILE.BB_CATAGORY,
                   BBFILE.BB_SUBCATAGORY,
                   BBFILE.BB_USERID,
                   BBFILE.BB_SEQNO,
                   BBFILE.BB_NAME,
                   BBFILE.BB_LOGNAME,
                   BBFILE.BB_POSTNAME,
                   BBFILE.BB_OTHERBR,
                   CAST(BBFILE.BB_POSTDATE AS VARCHAR(10)),
                   BBFILE.BB_POSTTIME,
                   BBFILE.BB_PASSWORD,
                   BBFILE.BB_GROUP,
                   BBFILE.BB_BRNO,
                   BBFILE.BB_ACTION,
                   BBFILE.BB_MEMO_LINE_1,
                   BBFILE.BB_MEMO_LINE_2,
                   BBFILE.BB_MEMO_LINE_3,
                   BBFILE.BB_MEMO_LINE_4,
                   BBFILE.BB_MEMO_LINE_5,
                   BBFILE.BB_MEMO_LINE_6,
                   BBFILE.BB_MEMO_LINE_7,
                   BBFILE.BB_MEMO_LINE_8,
                   BBFILE.BB_MEMO_LINE_9,
                   BBFILE.BB_MEMO_LINE_10,
                   CAST(BBFILE.BB_LTOUCH_DATE AS VARCHAR(10))
            FROM DBO.BBFILE
            WHERE BBFILE.BB_CATAGORY >= :QBB2-WBEG-CATAGORY
              AND BBFILE.BB_CATAGORY <= :QBB2-WEND-CATAGORY
              AND BBFILE.BB_SUBCATAGORY >= :QBB2-WBEG-SUBCAT
              AND BBFILE.BB_SUBCATAGORY <= :QBB2-WEND-SUBCAT
              AND BBFILE.BB_NAME >= :QBB2-WBEG-NAME
              AND BBFILE.BB_NAME <= :QBB2-WEND-NAME
            ORDER BY BBFILE.BB_CATAGORY,
                     BBFILE.BB_SUBCATAGORY,
                     BBFILE.BB_NAME,
                     BBFILE.BB_USERID,
                     BBFILE.BB_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN BB2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QBB2-WBEG-CATAGORY
                          QBB2-WEND-CATAGORY
                          QBB2-WBEG-SUBCAT
                          QBB2-WEND-SUBCAT
                          QBB2-WBEG-NAME
                          QBB2-WEND-NAME.

           MOVE STAT---GOOD TO BB2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BB2-FILE.
           PERFORM READ-IT.
           MOVE BB-PATH-SQL TO E-FILE.
           MOVE SPACES TO E-KEYX.
           STRING BB-CATAGORY, BB-SUBCATAGORY, BB-NAME
                DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QBB-REC.

           MOVE BB-CATAGORY    TO QBB-CATAGORY.
           MOVE BB-SUBCATAGORY TO QBB-SUBCATAGORY.
           MOVE BB-NAME        TO QBB-NAME.

           EXEC SQL
            SELECT BBFILE.BB_CATAGORY,
                   BBFILE.BB_SUBCATAGORY,
                   BBFILE.BB_USERID,
                   BBFILE.BB_SEQNO,
                   BBFILE.BB_NAME,
                   BBFILE.BB_LOGNAME,
                   BBFILE.BB_POSTNAME,
                   BBFILE.BB_OTHERBR,
                   CAST(BBFILE.BB_POSTDATE AS VARCHAR(10)),
                   BBFILE.BB_POSTTIME,
                   BBFILE.BB_PASSWORD,
                   BBFILE.BB_GROUP,
                   BBFILE.BB_BRNO,
                   BBFILE.BB_ACTION,
                   BBFILE.BB_MEMO_LINE_1,
                   BBFILE.BB_MEMO_LINE_2,
                   BBFILE.BB_MEMO_LINE_3,
                   BBFILE.BB_MEMO_LINE_4,
                   BBFILE.BB_MEMO_LINE_5,
                   BBFILE.BB_MEMO_LINE_6,
                   BBFILE.BB_MEMO_LINE_7,
                   BBFILE.BB_MEMO_LINE_8,
                   BBFILE.BB_MEMO_LINE_9,
                   BBFILE.BB_MEMO_LINE_10,
                   CAST(BBFILE.BB_LTOUCH_DATE AS VARCHAR(10))
            INTO :QBB-REC
            FROM DBO.BBFILE
            WHERE BBFILE.BB_CATAGORY = :QBB-CATAGORY
              AND BBFILE.BB_SUBCATAGORY = :QBB-SUBCATAGORY
              AND BBFILE.BB_NAME = :QBB-NAME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BB2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BB-FIELDS.

      *-----------------------------------------------------------------
       READ-BB2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BB-PATH-SQL TO E-FILE.
           MOVE SPACES TO E-KEYX.
           STRING BB-CATAGORY, BB-SUBCATAGORY, BB-NAME
                DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QBB-REC.

           IF ( BB2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BB2_CURSOR INTO :QBB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BB2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BB2-FILE.
           PERFORM CLOSE-IT.
           IF ( BB2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BB2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BB2-CURSOR-STAT.

