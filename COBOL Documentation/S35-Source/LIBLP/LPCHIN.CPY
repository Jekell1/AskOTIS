      * COPYMEMBER: LIBLP/LPCHIN
       LOAD-CH-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CH-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CH-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CH-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CH-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CH-ALL-DATA
              MOVE CH-ALL-PATH             TO CH-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CH-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CH-ALL-BASE
              MOVE FILPATH-MACHINE         TO CH-ALL-MACHINE
              MOVE FILPATH-DATA            TO CH-ALL-DATA
              MOVE CH-ALL-PATH             TO CH-PATH.

      *-----------------------------------------------------------------
       OPEN-CH-FILE.
           PERFORM LOAD-CH-FILE.
           PERFORM OPEN-IT.
           MOVE CH-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CH-FILE.
           PERFORM START-IT.
           MOVE CH-PATH-OWNBR  TO CH-BRNO.
           MOVE CH-PATH-SQL    TO E-FILE.
           MOVE CH-KEY         TO E-KEYX.

           IF ( CH-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CH-FILE.

           IF ( QCH-WEND-SEQ = ZEROES    ) OR
              ( QCH-WEND-SEQ NOT NUMERIC )
              MOVE CH-PATH-OWNBR       TO QCH-WBEG-BRNO
              MOVE CH-PATH-OWNBR       TO QCH-WEND-BRNO
              MOVE CH-DATE             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCH-WBEG-DATE
              MOVE EXT-JULIAN-END      TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCH-WEND-DATE
              MOVE CH-CODE             TO QCH-WBEG-CODE
              MOVE ALL "Z"             TO QCH-WEND-CODE
              MOVE CH-USERID           TO QCH-WBEG-USERID
              MOVE ALL "Z"             TO QCH-WEND-USERID
              MOVE CH-TIME             TO QCH-WBEG-TIME
              MOVE ALL "9"             TO QCH-WEND-TIME
              MOVE CH-SEQ              TO QCH-WBEG-SEQ
              MOVE ALL "9"             TO QCH-WEND-SEQ.


           EXEC SQL
            DECLARE CH_CURSOR CURSOR FOR
            SELECT CHFILE.CH_BRNO,
                   CAST(CHFILE.CH_DATE AS VARCHAR(10)),
                   CHFILE.CH_CODE,
                   CHFILE.CH_USERID,
                   CHFILE.CH_TIME,
                   CHFILE.CH_SEQ,
                   CAST(CHFILE.CH_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CHFILE.CH_DESCRIPT,
                   CHFILE.CH_ACCTNO,
                   CHFILE.CH_SSNO,
                   CHFILE.CH_TYPE,
                   CHFILE.CH_ORG_CHAR,
                   CHFILE.CH_NEW_CHAR
            FROM DBO.CHFILE
            WHERE CHFILE.CH_BRNO = :QCH-WBEG-BRNO
              AND CHFILE.CH_DATE >= :QCH-WBEG-DATE 
              AND CHFILE.CH_DATE <= :QCH-WEND-DATE
              AND CHFILE.CH_CODE >= :QCH-WBEG-CODE
              AND CHFILE.CH_CODE <= :QCH-WEND-CODE
              AND CHFILE.CH_USERID >= :QCH-WBEG-USERID
              AND CHFILE.CH_USERID <= :QCH-WEND-USERID
              AND CHFILE.CH_TIME >= :QCH-WBEG-TIME
              AND CHFILE.CH_TIME <= :QCH-WEND-TIME
              AND CHFILE.CH_SEQ >= :QCH-WBEG-SEQ
              AND CHFILE.CH_SEQ <= :QCH-WEND-SEQ
              ORDER BY CHFILE.CH_BRNO,
                       CHFILE.CH_DATE,
                       CHFILE.CH_CODE,
                       CHFILE.CH_USERID,
                       CHFILE.CH_TIME,
                       CHFILE.CH_SEQ
           END-EXEC.

           EXEC SQL
               OPEN CH_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCH-WBEG-BRNO 
                          QCH-WEND-BRNO
                          QCH-WBEG-TIME
                          QCH-WEND-TIME
                          QCH-WBEG-SEQ
                          QCH-WEND-SEQ.
 
           MOVE SPACES TO QCH-WBEG-DATE
                          QCH-WEND-DATE
                          QCH-WBEG-CODE
                          QCH-WEND-CODE
                          QCH-WBEG-USERID
                          QCH-WEND-USERID.

           MOVE STAT---GOOD TO CH-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CH-FILE.
           PERFORM READ-IT.
           MOVE CH-PATH-OWNBR  TO CH-BRNO. 
           MOVE CH-PATH-SQL    TO E-FILE.
           MOVE CH-KEY         TO E-KEYX.
           INITIALIZE QCH-REC.

           MOVE CH-BRNO             TO QCH-BRNO.
           MOVE CH-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCH-DATE.
           MOVE CH-CODE             TO QCH-CODE.
           MOVE CH-USERID           TO QCH-USERID.
           MOVE CH-TIME             TO QCH-TIME.
           MOVE CH-SEQ              TO QCH-SEQ.


           EXEC SQL
            SELECT CHFILE.CH_BRNO,
                   CAST(CHFILE.CH_DATE AS VARCHAR(10)),
                   CHFILE.CH_CODE,
                   CHFILE.CH_USERID,
                   CHFILE.CH_TIME,
                   CHFILE.CH_SEQ,
                   CAST(CHFILE.CH_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CHFILE.CH_DESCRIPT,
                   CHFILE.CH_ACCTNO,
                   CHFILE.CH_SSNO,
                   CHFILE.CH_TYPE,
                   CHFILE.CH_ORG_CHAR,
                   CHFILE.CH_NEW_CHAR
            INTO :QCH-REC
            FROM DBO.CHFILE
            WHERE CHFILE.CH_BRNO = :QCH-BRNO
              AND CHFILE.CH_DATE = :QCH-DATE
              AND CHFILE.CH_CODE = :QCH-CODE
              AND CHFILE.CH_USERID = :QCH-USERID
              AND CHFILE.CH_TIME = :QCH-TIME
              AND CHFILE.CH_SEQ = :QCH-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CH-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CH-FIELDS.

      *-----------------------------------------------------------------
       READ-CH-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CH-PATH-OWNBR  TO CH-BRNO.
           MOVE CH-PATH-SQL    TO E-FILE.
           MOVE CH-KEY         TO E-KEYX.
           INITIALIZE QCH-REC.

           IF ( CH-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CH_CURSOR INTO :QCH-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CH-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CH-FIELDS.

      *-----------------------------------------------------------------
       WRITE-CH-FILE.

           MOVE CH-PATH-OWNBR TO CH-BRNO. 
           ACCEPT CH-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE CH-PATH-SQL    TO E-FILE.
           MOVE CH-KEY         TO E-KEYX.
           PERFORM SET-CH-FIELDS.

           EXEC SQL
            INSERT INTO DBO.CHFILE
             (CHFILE.CH_BRNO,
              CHFILE.CH_DATE,
              CHFILE.CH_CODE,
              CHFILE.CH_USERID,
              CHFILE.CH_TIME,
              CHFILE.CH_SEQ,
              CHFILE.CH_LTOUCH_DATE,
              CHFILE.CH_DESCRIPT,
              CHFILE.CH_ACCTNO,
              CHFILE.CH_SSNO,
              CHFILE.CH_TYPE,
              CHFILE.CH_ORG_CHAR,
              CHFILE.CH_NEW_CHAR)
            VALUES
             (:QCH-BRNO,
              :QCH-DATE,
              :QCH-CODE,
              :QCH-USERID,
              :QCH-TIME,
              :QCH-SEQ,
              :QCH-LTOUCH-DATE,
              :QCH-DESCRIPT,
              :QCH-ACCTNO,
              :QCH-SSNO,
              :QCH-TYPE,
              :QCH-ORG-CHAR,
              :QCH-NEW-CHAR)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-CH-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-CH-FILE.

           MOVE CH-PATH-OWNBR TO CH-BRNO. 
           ACCEPT CH-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE CH-PATH-SQL    TO E-FILE.
           MOVE CH-KEY         TO E-KEYX.
           PERFORM SET-CH-FIELDS.

           EXEC SQL
            UPDATE DBO.CHFILE
             SET
              CHFILE.CH_BRNO = :QCH-BRNO,
              CHFILE.CH_DATE = :QCH-DATE,
              CHFILE.CH_CODE = :QCH-CODE,
              CHFILE.CH_USERID = :QCH-USERID,
              CHFILE.CH_TIME = :QCH-TIME,
              CHFILE.CH_SEQ = :QCH-SEQ,
              CHFILE.CH_LTOUCH_DATE = :QCH-LTOUCH-DATE,
              CHFILE.CH_DESCRIPT = :QCH-DESCRIPT,
              CHFILE.CH_ACCTNO = :QCH-ACCTNO,
              CHFILE.CH_SSNO = :QCH-SSNO,
              CHFILE.CH_TYPE = :QCH-TYPE,
              CHFILE.CH_ORG_CHAR = :QCH-ORG-CHAR,
              CHFILE.CH_NEW_CHAR = :QCH-NEW-CHAR
             WHERE CHFILE.CH_BRNO = :QCH-BRNO
               AND CHFILE.CH_DATE = :QCH-DATE
               AND CHFILE.CH_CODE = :QCH-CODE
               AND CHFILE.CH_USERID = :QCH-USERID
               AND CHFILE.CH_TIME = :QCH-TIME
               AND CHFILE.CH_SEQ = :QCH-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CH-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-CH-FILE.

           MOVE CH-PATH-OWNBR TO CH-BRNO. 
           PERFORM DELETE-IT.
           MOVE CH-PATH-SQL    TO E-FILE.
           MOVE CH-KEY         TO E-KEYX.
           PERFORM SET-CH-FIELDS.

           EXEC SQL
            DELETE FROM DBO.CHFILE
             WHERE CHFILE.CH_BRNO = :QCH-BRNO
               AND CHFILE.CH_DATE = :QCH-DATE
               AND CHFILE.CH_CODE = :QCH-CODE
               AND CHFILE.CH_USERID = :QCH-USERID
               AND CHFILE.CH_TIME = :QCH-TIME
               AND CHFILE.CH_SEQ = :QCH-SEQ
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-CH-FILE.

      *-----------------------------------------------------------------
       CLOSE-CH-FILE.

           PERFORM CLOSE-IT.

           IF ( CH-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CH_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CH-CURSOR-STAT.

