      * COPYMEMBER: LIBSP/SPBH1IN
       LOAD-BH1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BH-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BH-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BH-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO BH-PATH-MACHINE
              MOVE BH-PATH-FORMAT          TO BH-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BH-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BH-PATH-BASE
              MOVE FILPATH-MACHINE         TO BH-PATH-MACHINE
              MOVE BH-PATH-FORMAT          TO BH-PATH.

      *-----------------------------------------------------------------
       OPEN-BH1-FILE.
           PERFORM LOAD-BH1-FILE.
           PERFORM OPEN-IT.
           MOVE BH-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BH1-FILE.
           PERFORM START-IT.
           MOVE BH-PATH-SQL TO E-FILE.
           MOVE BH1-KEY TO E-KEYX.

           IF ( BH1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BH1-FILE.

           IF ( QBH1-WEND-USERID = SPACES )
              MOVE BH-USERID        TO QBH1-WBEG-USERID
              MOVE ALL "Z"          TO QBH1-WEND-USERID
              MOVE BH-DATE-REVERSED TO QBH1-WBEG-DATE
              MOVE ALL "9"          TO QBH1-WEND-DATE
              MOVE BH-TIME          TO QBH1-WBEG-TIME
              MOVE ALL "9"          TO QBH1-WEND-TIME.

           EXEC SQL
            DECLARE BH1_CURSOR CURSOR FOR
            SELECT BHFILE.BH_USERID,
                   BHFILE.BH_DATE_REVERSED,
                   BHFILE.BH_TIME,
                   BHFILE.BH_GROUP,
                   BHFILE.BH_ACTION,
                   CAST(BHFILE.BH_LTOUCH_DATE AS VARCHAR(10))
            FROM DBO.BHFILE
            WHERE BHFILE.BH_USERID >= :QBH1-WBEG-USERID
              AND BHFILE.BH_USERID <= :QBH1-WEND-USERID
              AND BHFILE.BH_DATE_REVERSED >= :QBH1-WBEG-DATE
              AND BHFILE.BH_DATE_REVERSED <= :QBH1-WEND-DATE
              AND BHFILE.BH_TIME >= :QBH1-WBEG-TIME
              AND BHFILE.BH_TIME <= :QBH1-WEND-TIME
            ORDER BY BHFILE.BH_USERID,
                     BHFILE.BH_DATE_REVERSED,
                     BHFILE.BH_TIME
           END-EXEC.

           EXEC SQL
               OPEN BH1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QBH1-WBEG-USERID
                          QBH1-WEND-USERID.

           MOVE ZEROES TO QBH1-WBEG-DATE
                          QBH1-WEND-DATE
                          QBH1-WBEG-TIME
                          QBH1-WEND-TIME.

           MOVE STAT---GOOD TO BH1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BH1-FILE.
           PERFORM READ-IT.
           MOVE BH-PATH-SQL TO E-FILE.
           MOVE BH1-KEY TO E-KEYX.
           INITIALIZE QBH-REC.

           MOVE BH-USERID         TO QBH-USERID.
           MOVE BH-DATE-REVERSED  TO QBH-DATE-REVERSED.
           MOVE BH-TIME           TO QBH-TIME.

           EXEC SQL
            SELECT BHFILE.BH_USERID,
                   BHFILE.BH_DATE_REVERSED,
                   BHFILE.BH_TIME,
                   BHFILE.BH_GROUP,
                   BHFILE.BH_ACTION,
                   CAST(BHFILE.BH_LTOUCH_DATE AS VARCHAR(10))
            INTO :QBH-REC
            FROM DBO.BHFILE
            WHERE BHFILE.BH_USERID = :QBH-USERID
              AND BHFILE.BH_DATE_REVERSED = :QBH-DATE-REVERSED
              AND BHFILE.BH_TIME = :QBH-TIME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BH1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BH-FIELDS.

      *-----------------------------------------------------------------
       READ-BH1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BH-PATH-SQL TO E-FILE.
           MOVE BH1-KEY TO E-KEYX.
           INITIALIZE QBH-REC.

           IF ( BH1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BH1_CURSOR INTO :QBH-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BH1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BH-FIELDS.

      *-----------------------------------------------------------------
       WRITE-BH1-FILE.

           ACCEPT BH-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE BH-PATH-SQL TO E-FILE.
           MOVE BH1-KEY TO E-KEYX.
           PERFORM SET-BH-FIELDS.

           EXEC SQL
            INSERT INTO DBO.BHFILE
             (BHFILE.BH_USERID,
              BHFILE.BH_DATE_REVERSED,
              BHFILE.BH_TIME,
              BHFILE.BH_GROUP,
              BHFILE.BH_ACTION,
              BHFILE.BH_LTOUCH_DATE)
            VALUES
             (:QBH-USERID,
              :QBH-DATE-REVERSED,
              :QBH-TIME,
              :QBH-GROUP,
              :QBH-ACTION,
              :QBH-LTOUCH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BH1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-BH1-FILE.

           ACCEPT BH-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE BH-PATH-SQL TO E-FILE.
           MOVE BH1-KEY TO E-KEYX.
           PERFORM SET-BH-FIELDS.

           EXEC SQL
            UPDATE DBO.BHFILE
             SET
              BHFILE.BH_USERID = :QBH-USERID,
              BHFILE.BH_DATE_REVERSED = :QBH-DATE-REVERSED,
              BHFILE.BH_TIME = :QBH-TIME,
              BHFILE.BH_GROUP = :QBH-GROUP,
              BHFILE.BH_ACTION = :QBH-ACTION,
              BHFILE.BH_LTOUCH_DATE = :QBH-LTOUCH-DATE
            WHERE BHFILE.BH_USERID = :QBH-USERID
              AND BHFILE.BH_DATE_REVERSED = :QBH-DATE-REVERSED
              AND BHFILE.BH_TIME = :QBH-TIME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BH1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-BH1-FILE.

           PERFORM DELETE-IT.
           MOVE BH-PATH-SQL TO E-FILE.
           MOVE BH1-KEY TO E-KEYX.
           PERFORM SET-BH-FIELDS.

           EXEC SQL
            DELETE FROM DBO.BHFILE
             WHERE BHFILE.BH_USERID = :QBH-USERID
               AND BHFILE.BH_DATE_REVERSED = :QBH-DATE-REVERSED
               AND BHFILE.BH_TIME = :QBH-TIME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BH1-FILE.

      *-----------------------------------------------------------------
       CLOSE-BH1-FILE.

           PERFORM CLOSE-IT.

           IF ( BH1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BH1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BH1-CURSOR-STAT.

