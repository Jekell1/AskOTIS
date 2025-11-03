      * COPYMEMBER: LIBGB/GBAU1IN
       LOAD-AU1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( AU-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( AU-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO AU-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO AU-ALL-MACHINE
              MOVE AU-ALL-PATH             TO AU-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( AU-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO AU-ALL-BASE
              MOVE FILPATH-MACHINE         TO AU-ALL-MACHINE
              MOVE AU-ALL-PATH             TO AU-PATH.

      *-----------------------------------------------------------------
       OPEN-AU1-FILE.
           PERFORM LOAD-AU1-FILE.
           PERFORM OPEN-IT.
           MOVE AU-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-AU1-FILE.
           PERFORM START-IT.
           MOVE AU-PATH-SQL TO E-FILE.
           MOVE AU1-KEY TO E-KEYX.

           IF ( AU1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-AU1-FILE.

           IF ( QAU1-WEND-SEQ = 0    ) OR
              ( QAU1-WEND-SEQ NOT NUMERIC )
              MOVE AU-FILE-NAME        TO QAU1-WBEG-FILE-NAME
              MOVE ALL "Z"             TO QAU1-WEND-FILE-NAME
              MOVE AU-DATE             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QAU1-WBEG-DATE
              MOVE EXT-JULIAN-END      TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QAU1-WEND-DATE
              MOVE AU-SEQ              TO QAU1-WBEG-SEQ
              MOVE ALL "9"             TO QAU1-WEND-SEQ.

           EXEC SQL
            DECLARE AU1_CURSOR CURSOR FOR
            SELECT AUFILE.AU_FILE_NAME,
                   CAST(AUFILE.AU_DATE AS VARCHAR(10)),
                   AUFILE.AU_SEQ,
                   AUFILE.AU_ACTION,
                   AUFILE.AU_FILE_TYPE,
                   AUFILE.AU_BRANCH,
                   AUFILE.AU_FILE_KEY,
                   AUFILE.AU_FIELD_DESC,
                   AUFILE.AU_ORIG_VALUE,
                   AUFILE.AU_NEW_VALUE,
                   AUFILE.AU_TICKET,
                   AUFILE.AU_PROGRAM,
                   AUFILE.AU_TIME,
                   AUFILE.AU_USERID,
                   CAST(AUFILE.AU_LTOUCH_DATE AS VARCHAR(10))
            FROM DBO.AUFILE
            WHERE AUFILE.AU_FILE_NAME >= :QAU1-WBEG-FILE-NAME
              AND AUFILE.AU_FILE_NAME <= :QAU1-WEND-FILE-NAME
              AND AUFILE.AU_DATE      >= :QAU1-WBEG-DATE 
              AND AUFILE.AU_DATE      <= :QAU1-WEND-DATE 
              AND AUFILE.AU_SEQ       >= :QAU1-WBEG-SEQ
              AND AUFILE.AU_SEQ       <= :QAU1-WEND-SEQ
            ORDER BY AUFILE.AU_FILE_NAME,
                     AUFILE.AU_DATE,
                     AUFILE.AU_SEQ
           END-EXEC.

           EXEC SQL
               OPEN AU1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QAU1-WBEG-SEQ
                          QAU1-WEND-SEQ.

           MOVE SPACES TO QAU1-WBEG-FILE-NAME
                          QAU1-WEND-FILE-NAME
                          QAU1-WBEG-DATE
                          QAU1-WEND-DATE.
                          

           MOVE STAT---GOOD TO AU1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-AU1-FILE.
           PERFORM READ-IT.
           MOVE AU-PATH-SQL TO E-FILE.
           MOVE AU1-KEY TO E-KEYX.

           MOVE AU-FILE-NAME        TO QAU-FILE-NAME.
           MOVE AU-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QAU-DATE.
           MOVE AU-SEQ              TO QAU-SEQ.

           EXEC SQL
            SELECT AUFILE.AU_FILE_NAME,
                   CAST(AUFILE.AU_DATE AS VARCHAR(10)),
                   AUFILE.AU_SEQ,
                   AUFILE.AU_ACTION,
                   AUFILE.AU_FILE_TYPE,
                   AUFILE.AU_BRANCH,
                   AUFILE.AU_FILE_KEY,
                   AUFILE.AU_FIELD_DESC,
                   AUFILE.AU_ORIG_VALUE,
                   AUFILE.AU_NEW_VALUE,
                   AUFILE.AU_TICKET,
                   AUFILE.AU_PROGRAM,
                   AUFILE.AU_TIME,
                   AUFILE.AU_USERID,
                   CAST(AUFILE.AU_LTOUCH_DATE AS VARCHAR(10))
            INTO :QAU-REC
            FROM DBO.AUFILE
            WHERE AUFILE.AU_FILE_NAME = :QAU-FILE-NAME
              AND AUFILE.AU_DATE      = :QAU-DATE
              AND AUFILE.AU_SEQ       = :QAU-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-AU1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-AU-FIELDS.

      *-----------------------------------------------------------------
       READ-AU1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE AU-PATH-SQL TO E-FILE.
           MOVE AU1-KEY TO E-KEYX.
           INITIALIZE QAU-REC.

           IF ( AU1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH AU1_CURSOR INTO :QAU-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-AU1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-AU-FIELDS.


      *-----------------------------------------------------------------
       WRITE-AU1-FILE.

           ACCEPT AU-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE AU-PATH-SQL TO E-FILE.
           MOVE AU1-KEY TO E-KEYX.
           PERFORM SET-AU-FIELDS.

           EXEC SQL
            INSERT INTO DBO.AUFILE
             (AUFILE.AU_FILE_NAME,
              AUFILE.AU_DATE,
              AUFILE.AU_SEQ,
              AUFILE.AU_ACTION,
              AUFILE.AU_FILE_TYPE,
              AUFILE.AU_BRANCH,
              AUFILE.AU_FILE_KEY,
              AUFILE.AU_FIELD_DESC,
              AUFILE.AU_ORIG_VALUE,
              AUFILE.AU_NEW_VALUE,
              AUFILE.AU_TICKET,
              AUFILE.AU_PROGRAM,
              AUFILE.AU_TIME,
              AUFILE.AU_USERID,
              AUFILE.AU_LTOUCH_DATE)
            VALUES
             (:QAU-FILE-NAME,
              :QAU-DATE,
              :QAU-SEQ,
              :QAU-ACTION,
              :QAU-FILE-TYPE,
              :QAU-BRANCH,
              :QAU-FILE-KEY,
              :QAU-FIELD-DESC,
              :QAU-ORIG-VALUE,
              :QAU-NEW-VALUE,
              :QAU-TICKET,
              :QAU-PROGRAM,
              :QAU-TIME,
              :QAU-USERID,
              :QAU-LTOUCH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-AU1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-AU1-FILE.

           ACCEPT AU-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE AU-PATH-SQL TO E-FILE.
           MOVE AU1-KEY TO E-KEYX.
           PERFORM SET-AU-FIELDS.

           EXEC SQL
            UPDATE DBO.AUFILE
             SET
              AUFILE.AU_FILE_NAME = :QAU-FILE-NAME,
              AUFILE.AU_DATE = :QAU-DATE,
              AUFILE.AU_SEQ = :QAU-SEQ,
              AUFILE.AU_ACTION = :QAU-ACTION,
              AUFILE.AU_FILE_TYPE = :QAU-FILE-TYPE,
              AUFILE.AU_BRANCH = :QAU-BRANCH,
              AUFILE.AU_FILE_KEY = :QAU-FILE-KEY,
              AUFILE.AU_FIELD_DESC = :QAU-FIELD-DESC,
              AUFILE.AU_ORIG_VALUE = :QAU-ORIG-VALUE,
              AUFILE.AU_NEW_VALUE = :QAU-NEW-VALUE,
              AUFILE.AU_TICKET = :QAU-TICKET,
              AUFILE.AU_PROGRAM = :QAU-PROGRAM,
              AUFILE.AU_TIME = :QAU-TIME,
              AUFILE.AU_USERID = :QAU-USERID,
              AUFILE.AU_LTOUCH_DATE = :QAU-LTOUCH-DATE
            WHERE AUFILE.AU_FILE_NAME = :QAU-FILE-NAME
              AND AUFILE.AU_DATE      = :QAU-DATE
              AND AUFILE.AU_SEQ       = :QAU-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-AU1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-AU1-FILE.

           PERFORM DELETE-IT.
           MOVE AU-PATH-SQL TO E-FILE.
           MOVE AU1-KEY TO E-KEYX.
           PERFORM SET-AU-FIELDS.

           EXEC SQL
            DELETE FROM DBO.AUFILE
            WHERE AUFILE.AU_FILE_NAME = :QAU-FILE-NAME
              AND AUFILE.AU_DATE      = :QAU-DATE
              AND AUFILE.AU_SEQ       = :QAU-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-AU1-FILE.

      *-----------------------------------------------------------------
       CLOSE-AU1-FILE.

           PERFORM CLOSE-IT.

           IF ( AU1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE AU1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO AU1-CURSOR-STAT.

