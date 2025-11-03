      * COPYMEMBER: LIBSP/SPBB1IN
       LOAD-BB1-FILE.
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
       OPEN-BB1-FILE.
           PERFORM LOAD-BB1-FILE.
           PERFORM OPEN-IT.
           MOVE BB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BB1-FILE.
           PERFORM START-IT.
           MOVE BB-PATH-SQL TO E-FILE.
           MOVE BB1-KEY TO E-KEYX.

           IF ( BB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BB1-FILE.

           IF ( QBB1-WEND-CATAGORY = SPACES )
              MOVE BB-CATAGORY    TO QBB1-WBEG-CATAGORY
              MOVE ALL "Z"        TO QBB1-WEND-CATAGORY
              MOVE BB-SUBCATAGORY TO QBB1-WBEG-SUBCAT
              MOVE ALL "Z"        TO QBB1-WEND-SUBCAT
              MOVE BB-USERID      TO QBB1-WBEG-USERID
              MOVE ALL "Z"        TO QBB1-WEND-USERID
              MOVE BB-SEQNO       TO QBB1-WBEG-SEQNO
              MOVE ALL "9"        TO QBB1-WEND-SEQNO.

           EXEC SQL
            DECLARE BB1_CURSOR CURSOR FOR
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
                   CAST(BBFILE.BB_LTOUCH_DATE
                                                  AS VARCHAR(10))
            FROM DBO.BBFILE
            WHERE BBFILE.BB_CATAGORY >= :QBB1-WBEG-CATAGORY
              AND BBFILE.BB_CATAGORY <= :QBB1-WEND-CATAGORY
              AND BBFILE.BB_SUBCATAGORY >= :QBB1-WBEG-SUBCAT
              AND BBFILE.BB_SUBCATAGORY <= :QBB1-WEND-SUBCAT
              AND BBFILE.BB_USERID >= :QBB1-WBEG-USERID
              AND BBFILE.BB_USERID <= :QBB1-WEND-USERID
              AND BBFILE.BB_SEQNO >= :QBB1-WBEG-SEQNO
              AND BBFILE.BB_SEQNO <= :QBB1-WEND-SEQNO
            ORDER BY BBFILE.BB_CATAGORY,
                     BBFILE.BB_SUBCATAGORY,
                     BBFILE.BB_USERID,
                     BFILE.BB_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN BB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QBB1-WBEG-CATAGORY
                          QBB1-WEND-CATAGORY
                          QBB1-WBEG-SUBCAT
                          QBB1-WEND-SUBCAT
                          QBB1-WBEG-USERID
                          QBB1-WEND-USERID.

           MOVE ZEROES TO QBB1-WBEG-SEQNO
                          QBB1-WEND-SEQNO.

           MOVE STAT---GOOD TO BB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BB1-FILE.
           PERFORM READ-IT.
           MOVE BB-PATH-SQL TO E-FILE.
           MOVE BB1-KEY TO E-KEYX.
           INITIALIZE QBB-REC.

           MOVE BB-CATAGORY    TO QBB-CATAGORY.
           MOVE BB-SUBCATAGORY TO QBB-SUBCATAGORY.
           MOVE BB-USERID      TO QBB-USERID.
           MOVE BB-SEQNO       TO QBB-SEQNO.

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
              AND BBFILE.BB_USERID = :QBB-USERID
              AND BBFILE.BB_SEQNO = :QBB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BB-FIELDS.

      *-----------------------------------------------------------------
       READ-BB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BB-PATH-SQL TO E-FILE.
           MOVE BB1-KEY TO E-KEYX.
           INITIALIZE QBB-REC.

           IF ( BB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BB1_CURSOR INTO :QBB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-BB1-FILE.

           ACCEPT BB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE BB-PATH-SQL TO E-FILE.
           MOVE BB1-KEY TO E-KEYX.
           PERFORM SET-BB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.BBFILE
             (BBFILE.BB_CATAGORY,
              BBFILE.BB_SUBCATAGORY,
              BBFILE.BB_USERID,
              BBFILE.BB_SEQNO,
              BBFILE.BB_NAME,
              BBFILE.BB_LOGNAME,
              BBFILE.BB_POSTNAME,
              BBFILE.BB_OTHERBR,
              BBFILE.BB_POSTDATE,
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
              BBFILE.BB_LTOUCH_DATE)
            VALUES
             (:QBB-CATAGORY,
              :QBB-SUBCATAGORY,
              :QBB-USERID,
              :QBB-SEQNO,
              :QBB-NAME,
              :QBB-LOGNAME,
              :QBB-POSTNAME,
              :QBB-OTHERBR,
              :QBB-POSTDATE,
              :QBB-POSTTIME,
              :QBB-PASSWORD,
              :QBB-GROUP,
              :QBB-BRNO,
              :QBB-ACTION,
              :QBB-MEMO-LINE-1,
              :QBB-MEMO-LINE-2,
              :QBB-MEMO-LINE-3,
              :QBB-MEMO-LINE-4,
              :QBB-MEMO-LINE-5,
              :QBB-MEMO-LINE-6,
              :QBB-MEMO-LINE-7,
              :QBB-MEMO-LINE-8,
              :QBB-MEMO-LINE-9,
              :QBB-MEMO-LINE-10,
              :QBB-LTOUCH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-BB1-FILE.

           ACCEPT BB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE BB-PATH-SQL TO E-FILE.
           MOVE BB1-KEY TO E-KEYX.
           PERFORM SET-BB-FIELDS.

           EXEC SQL
            UPDATE DBO.BBFILE
             SET
              BBFILE.BB_CATAGORY = :QBB-CATAGORY,
              BBFILE.BB_SUBCATAGORY = :QBB-SUBCATAGORY,
              BBFILE.BB_USERID = :QBB-USERID,
              BBFILE.BB_SEQNO = :QBB-SEQNO,
              BBFILE.BB_NAME = :QBB-NAME,
              BBFILE.BB_LOGNAME = :QBB-LOGNAME,
              BBFILE.BB_POSTNAME = :QBB-POSTNAME,
              BBFILE.BB_OTHERBR = :QBB-OTHERBR,
              BBFILE.BB_POSTDATE = :QBB-POSTDATE,
              BBFILE.BB_POSTTIME = :QBB-POSTTIME,
              BBFILE.BB_PASSWORD = :QBB-PASSWORD,
              BBFILE.BB_GROUP = :QBB-GROUP,
              BBFILE.BB_BRNO = :QBB-BRNO,
              BBFILE.BB_ACTION = :QBB-ACTION,
              BBFILE.BB_MEMO_LINE_1 = :QBB-MEMO-LINE-1,
              BBFILE.BB_MEMO_LINE_2 = :QBB-MEMO-LINE-2,
              BBFILE.BB_MEMO_LINE_3 = :QBB-MEMO-LINE-3,
              BBFILE.BB_MEMO_LINE_4 = :QBB-MEMO-LINE-4,
              BBFILE.BB_MEMO_LINE_5 = :QBB-MEMO-LINE-5,
              BBFILE.BB_MEMO_LINE_6 = :QBB-MEMO-LINE-6,
              BBFILE.BB_MEMO_LINE_7 = :QBB-MEMO-LINE-7,
              BBFILE.BB_MEMO_LINE_8 = :QBB-MEMO-LINE-8,
              BBFILE.BB_MEMO_LINE_9 = :QBB-MEMO-LINE-9,
              BBFILE.BB_MEMO_LINE_10 = :QBB-MEMO-LINE-10,
              BBFILE.BB_LTOUCH_DATE = :QBB-LTOUCH-DATE
             WHERE BBFILE.BB_CATAGORY = :QBB-CATAGORY
               AND BBFILE.BB_SUBCATAGORY = :QBB-SUBCATAGORY
               AND BBFILE.BB_USERID = :QBB-USERID
               AND BBFILE.BB_SEQNO = :QBB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BB1-FILE.
              PERFORM SQL-IO-VALIDATION.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-BB1-FILE.

           PERFORM DELETE-IT.
           MOVE BB-PATH-SQL TO E-FILE.
           MOVE BB1-KEY TO E-KEYX.
           PERFORM SET-BB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.BBFILE
             WHERE BBFILE.BB_CATAGORY = :QBB-CATAGORY
               AND BBFILE.BB_SUBCATAGORY = :QBB-SUBCATAGORY
               AND BBFILE.BB_USERID = :QBB-USERID
               AND BBFILE.BB_SEQNO = :QBB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-BB1-FILE.
           PERFORM CLOSE-IT.
           IF ( BB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BB1-CURSOR-STAT.

