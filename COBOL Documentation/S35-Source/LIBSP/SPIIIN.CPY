      * COPYMEMBER: LIBSP/SPIII
       LOAD-II-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( II-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( II-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO II-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO II-ALL-MACHINE
              MOVE II-ALL-PATH             TO II-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( II-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO II-ALL-BASE
              MOVE FILPATH-MACHINE         TO II-ALL-MACHINE
              MOVE II-ALL-PATH             TO II-PATH.

      *-----------------------------------------------------------------
       OPEN-II-FILE.
           PERFORM LOAD-II-FILE.
           PERFORM OPEN-IT.
           MOVE II-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-II-FILE.
           PERFORM START-IT.
           MOVE II-PATH-SQL TO E-FILE.
           MOVE II-KEY TO E-KEYX.

           IF ( II-CURSOR-STAT-GOOD )
              PERFORM CLOSE-II-FILE.

           IF ( QII-WEND-KEY = ZEROES    ) OR
              ( QII-WEND-KEY NOT NUMERIC )
              MOVE II-KEY               TO QII-WBEG-KEY
              MOVE 99                   TO QII-WEND-KEY.

           EXEC SQL
            DECLARE II_CURSOR CURSOR FOR
            SELECT IIFILE.II_KEY,
                   CAST(IIFILE.II_LTOUCH_DATE AS VARCHAR(10)),
                   IIFILE.II_EMPLOYER_ID,
                   IIFILE.II_NAME_CONTROL_NO,
                   IIFILE.II_TRANS_CONTROL_CODE,
                   IIFILE.II_FOREIGN_CORP,
                   IIFILE.II_TRANSFER_AGENT_FG,
                   IIFILE.II_PAYER_NAME,
                   IIFILE.II_SECOND_PAYER,
                   IIFILE.II_PAYER_ADDRESS,
                   IIFILE.II_PAYER_CITY,
                   IIFILE.II_PAYER_STATE,
                   IIFILE.II_PAYER_ZIP,
                   IIFILE.II_PAYER_PHONE,
                   IIFILE.II_PAYER_EXT,
                   IIFILE.II_CONTACT,
                   IIFILE.II_CONTACT_EMAIL,
                   IIFILE.II_TAX_YEAR,
                   IIFILE.II_CURRENT_LAST,
                   IIFILE.II_LASER_FORM
            FROM DBO.IIFILE
            WHERE IIFILE.II_KEY >= :QII-WBEG-KEY
              AND IIFILE.II_KEY <= :QII-WEND-KEY
            ORDER BY IIFILE.II_KEY
           END-EXEC.

           EXEC SQL
               OPEN II_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QII-WBEG-KEY
                          QII-WEND-KEY.

           MOVE STAT---GOOD TO II-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-II-FILE.
           PERFORM READ-IT.
           MOVE II-PATH-SQL TO E-FILE.
           MOVE II-KEY TO E-KEYX.
           INITIALIZE QII-REC.

           MOVE II-KEY    TO QII-KEY.  

           EXEC SQL
            SELECT IIFILE.II_KEY,
                   CAST(IIFILE.II_LTOUCH_DATE AS VARCHAR(10)),
                   IIFILE.II_EMPLOYER_ID,
                   IIFILE.II_NAME_CONTROL_NO,
                   IIFILE.II_TRANS_CONTROL_CODE,
                   IIFILE.II_FOREIGN_CORP,
                   IIFILE.II_TRANSFER_AGENT_FG,
                   IIFILE.II_PAYER_NAME,
                   IIFILE.II_SECOND_PAYER,
                   IIFILE.II_PAYER_ADDRESS,
                   IIFILE.II_PAYER_CITY,
                   IIFILE.II_PAYER_STATE,
                   IIFILE.II_PAYER_ZIP,
                   IIFILE.II_PAYER_PHONE,
                   IIFILE.II_PAYER_EXT,
                   IIFILE.II_CONTACT,
                   IIFILE.II_CONTACT_EMAIL,
                   IIFILE.II_TAX_YEAR,
                   IIFILE.II_CURRENT_LAST,
                   IIFILE.II_LASER_FORM
            INTO :QII-REC
            FROM DBO.IIFILE
            WHERE IIFILE.II_KEY = :QII-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-II-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-II-FIELDS.

      *-----------------------------------------------------------------
       READ-II-FILE-NEXT.
           PERFORM READ-IT.
           MOVE II-PATH-SQL TO E-FILE.
           MOVE II-KEY TO E-KEYX.
           INITIALIZE QII-REC.

           IF ( II-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH II_CURSOR INTO :QII-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-II-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-II-FIELDS.

      *-----------------------------------------------------------------
       WRITE-II-FILE.

           ACCEPT II-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE II-PATH-SQL TO E-FILE.
           MOVE II-KEY TO E-KEYX.
           PERFORM SET-II-FIELDS.

           EXEC SQL
            INSERT INTO DBO.IIFILE
             (IIFILE.II_KEY,
              IIFILE.II_LTOUCH_DATE,
              IIFILE.II_EMPLOYER_ID,
              IIFILE.II_NAME_CONTROL_NO,
              IIFILE.II_TRANS_CONTROL_CODE,
              IIFILE.II_FOREIGN_CORP,
              IIFILE.II_TRANSFER_AGENT_FG,
              IIFILE.II_PAYER_NAME,
              IIFILE.II_SECOND_PAYER,
              IIFILE.II_PAYER_ADDRESS,
              IIFILE.II_PAYER_CITY,
              IIFILE.II_PAYER_STATE,
              IIFILE.II_PAYER_ZIP,
              IIFILE.II_PAYER_PHONE,
              IIFILE.II_PAYER_EXT,
              IIFILE.II_CONTACT,
              IIFILE.II_CONTACT_EMAIL,
              IIFILE.II_TAX_YEAR,
              IIFILE.II_CURRENT_LAST,
              IIFILE.II_LASER_FORM)
            VALUES
             (:QII-KEY,
              :QII-LTOUCH-DATE,
              :QII-EMPLOYER-ID,
              :QII-NAME-CONTROL-NO,
              :QII-TRANS-CONTROL-CODE,
              :QII-FOREIGN-CORP,
              :QII-TRANSFER-AGENT-FG,
              :QII-PAYER-NAME,
              :QII-SECOND-PAYER,
              :QII-PAYER-ADDRESS,
              :QII-PAYER-CITY,
              :QII-PAYER-STATE,
              :QII-PAYER-ZIP,
              :QII-PAYER-PHONE,
              :QII-PAYER-EXT,
              :QII-CONTACT,
              :QII-CONTACT-EMAIL,
              :QII-TAX-YEAR,
              :QII-CURRENT-LAST,
              :QII-LASER-FORM)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-II-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-II-FILE.

           ACCEPT II-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE II-PATH-SQL TO E-FILE.
           MOVE II-KEY TO E-KEYX.
           PERFORM SET-II-FIELDS.

           EXEC SQL
            UPDATE DBO.IIFILE
             SET
              IIFILE.II_KEY = :QII-KEY,
              IIFILE.II_LTOUCH_DATE = :QII-LTOUCH-DATE,
              IIFILE.II_EMPLOYER_ID = :QII-EMPLOYER-ID,
              IIFILE.II_NAME_CONTROL_NO = :QII-NAME-CONTROL-NO,
              IIFILE.II_TRANS_CONTROL_CODE =
                   :QII-TRANS-CONTROL-CODE,
              IIFILE.II_FOREIGN_CORP = :QII-FOREIGN-CORP,
              IIFILE.II_TRANSFER_AGENT_FG =
                   :QII-TRANSFER-AGENT-FG,
              IIFILE.II_PAYER_NAME = :QII-PAYER-NAME,
              IIFILE.II_SECOND_PAYER = :QII-SECOND-PAYER,
              IIFILE.II_PAYER_ADDRESS = :QII-PAYER-ADDRESS,
              IIFILE.II_PAYER_CITY = :QII-PAYER-CITY,
              IIFILE.II_PAYER_STATE = :QII-PAYER-STATE,
              IIFILE.II_PAYER_ZIP = :QII-PAYER-ZIP,
              IIFILE.II_PAYER_PHONE = :QII-PAYER-PHONE,
              IIFILE.II_PAYER_EXT = :QII-PAYER-EXT,
              IIFILE.II_CONTACT = :QII-CONTACT,
              IIFILE.II_CONTACT_EMAIL = :QII-CONTACT-EMAIL,
              IIFILE.II_TAX_YEAR = :QII-TAX-YEAR,
              IIFILE.II_CURRENT_LAST = :QII-CURRENT-LAST,
              IIFILE.II_LASER_FORM = :QII-LASER-FORM
             WHERE IIFILE.II_KEY = :QII-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-II-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-II-FILE.

           PERFORM DELETE-IT.
           MOVE II-PATH-SQL TO E-FILE.
           MOVE II-KEY TO E-KEYX.
           PERFORM SET-II-FIELDS.

           EXEC SQL
            DELETE FROM DBO.IIFILE
             WHERE IIFILE.II_KEY = :QII-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-II-FILE.

      *-----------------------------------------------------------------
       CLOSE-II-FILE.

           PERFORM CLOSE-IT.

           IF ( II-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE II_CURSOR
              END-EXEC
              MOVE STAT---BAD TO II-CURSOR-STAT.

