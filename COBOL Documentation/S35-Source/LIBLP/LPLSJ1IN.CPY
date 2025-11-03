      * COPYMEMBER: LIBLP/LPLSJ1IN
       LOAD-LSJ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSJ-ALL-DATA
              MOVE LSJ-ALL-PATH             TO LSJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSJ-ALL-DATA
              MOVE LSJ-ALL-PATH             TO LSJ-PATH.

      *-----------------------------------------------------------------
       OPEN-LSJ1-FILE.
           PERFORM LOAD-LSJ1-FILE.
           PERFORM OPEN-IT.
           MOVE LSJ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSJ1-FILE.
           PERFORM START-IT.
           MOVE LSJ-PATH-OWNBR  TO LSJ-BRNO.
           MOVE LSJ-PATH-SQL    TO E-FILE.
           MOVE LSJ1-KEY        TO E-KEYX.

           IF ( LSJ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSJ1-FILE.

           IF ( QLSJ1-WEND-BRNO = ZEROES    ) OR
              ( QLSJ1-WEND-BRNO NOT NUMERIC )
              MOVE LSJ-PATH-OWNBR        TO QLSJ1-WBEG-BRNO
              MOVE LSJ-PATH-OWNBR        TO QLSJ1-WEND-BRNO
              MOVE LSJ-ACCTNO            TO QLSJ1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSJ1-WEND-ACCTNO
              MOVE LSJ-SEQNO             TO QLSJ1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSJ1-WEND-SEQNO.

           EXEC SQL
            DECLARE LSJ1_CURSOR CURSOR FOR
            SELECT LSJFILE.LSJ_BRNO,
                   LSJFILE.LSJ_ACCTNO,
                   LSJFILE.LSJ_SEQNO,
                   CAST(LSJFILE.LSJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSJFILE.LSJ_FILEDATE
                                                  AS VARCHAR(10)),
                   LSJFILE.LSJ_CASE_NO,
                   LSJFILE.LSJ_AMT,
                   LSJFILE.LSJ_COUNTY,
                   LSJFILE.LSJ_CLERK_NAME,
                   LSJFILE.LSJ_CLERK_ADDR,
                   LSJFILE.LSJ_CLERK_CITY,
                   LSJFILE.LSJ_CLERK_STATE,
                   LSJFILE.LSJ_CLERK_ZIP,
                   LSJFILE.LSJ_CLERK_PHONE,
                   LSJFILE.LSJ_SHERIFF_FEE,
                   LSJFILE.LSJ_COURT_FEE,
                   LSJFILE.LSJ_ATTY_FEE,
                   CAST(LSJFILE.LSJ_DISMISS_DATE
                                                  AS VARCHAR(10)),
                   LSJFILE.LSJ_GARN_INFO,
                   LSJFILE.LSJ_EMPLOY_NAME,
                   LSJFILE.LSJ_EMPLOY_ADDR,
                   LSJFILE.LSJ_EMPLOY_CITY,
                   LSJFILE.LSJ_EMPLOY_STATE,
                   LSJFILE.LSJ_EMPLOY_ZIP,
                   CAST(LSJFILE.LSJ_GARN_DATE
                                                  AS VARCHAR(10)),
                   LSJFILE.LSJ_JD_FILED_ON,
                   LSJFILE.LSJ_GARN_FILED_ON,
                   LSJFILE.LSJ_ATTY_NAME,
                   LSJFILE.LSJ_ATTY_ADDR,
                   LSJFILE.LSJ_ATTY_CITY,
                   LSJFILE.LSJ_ATTY_STATE,
                   LSJFILE.LSJ_ATTY_ZIP,
                   LSJFILE.LSJ_ATTY_PHONE
            FROM DBO.LSJFILE
            WHERE LSJFILE.LSJ_BRNO  = :QLSJ1-WBEG-BRNO
              AND LSJFILE.LSJ_ACCTNO >= :QLSJ1-WBEG-ACCTNO
              AND LSJFILE.LSJ_ACCTNO <= :QLSJ1-WEND-ACCTNO
              AND LSJFILE.LSJ_SEQNO >= :QLSJ1-WBEG-SEQNO
              AND LSJFILE.LSJ_SEQNO <= :QLSJ1-WEND-SEQNO
            ORDER BY LSJFILE.LSJ_BRNO,
                     LSJFILE.LSJ_ACCTNO,
                     LSJFILE.LSJ_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSJ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSJ1-WBEG-BRNO
                          QLSJ1-WEND-BRNO
                          QLSJ1-WBEG-ACCTNO
                          QLSJ1-WEND-ACCTNO
                          QLSJ1-WBEG-SEQNO
                          QLSJ1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSJ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSJ1-FILE.
           PERFORM READ-IT.
           MOVE LSJ-PATH-OWNBR  TO LSJ-BRNO. 
           MOVE LSJ-PATH-SQL    TO E-FILE.
           MOVE LSJ1-KEY        TO E-KEYX.
           INITIALIZE QLSJ-REC.

           MOVE LSJ-BRNO   TO QLSJ-BRNO.
           MOVE LSJ-ACCTNO TO QLSJ-ACCTNO.
           MOVE LSJ-SEQNO  TO QLSJ-SEQNO.

           EXEC SQL
            SELECT LSJFILE.LSJ_BRNO,
                   LSJFILE.LSJ_ACCTNO,
                   LSJFILE.LSJ_SEQNO,
                   CAST(LSJFILE.LSJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSJFILE.LSJ_FILEDATE
                                                  AS VARCHAR(10)),
                   LSJFILE.LSJ_CASE_NO,
                   LSJFILE.LSJ_AMT,
                   LSJFILE.LSJ_COUNTY,
                   LSJFILE.LSJ_CLERK_NAME,
                   LSJFILE.LSJ_CLERK_ADDR,
                   LSJFILE.LSJ_CLERK_CITY,
                   LSJFILE.LSJ_CLERK_STATE,
                   LSJFILE.LSJ_CLERK_ZIP,
                   LSJFILE.LSJ_CLERK_PHONE,
                   LSJFILE.LSJ_SHERIFF_FEE,
                   LSJFILE.LSJ_COURT_FEE,
                   LSJFILE.LSJ_ATTY_FEE,
                   CAST(LSJFILE.LSJ_DISMISS_DATE
                                                  AS VARCHAR(10)),
                   LSJFILE.LSJ_GARN_INFO,
                   LSJFILE.LSJ_EMPLOY_NAME,
                   LSJFILE.LSJ_EMPLOY_ADDR,
                   LSJFILE.LSJ_EMPLOY_CITY,
                   LSJFILE.LSJ_EMPLOY_STATE,
                   LSJFILE.LSJ_EMPLOY_ZIP,
                   CAST(LSJFILE.LSJ_GARN_DATE
                                                  AS VARCHAR(10)),
                   LSJFILE.LSJ_JD_FILED_ON,
                   LSJFILE.LSJ_GARN_FILED_ON,
                   LSJFILE.LSJ_ATTY_NAME,
                   LSJFILE.LSJ_ATTY_ADDR,
                   LSJFILE.LSJ_ATTY_CITY,
                   LSJFILE.LSJ_ATTY_STATE,
                   LSJFILE.LSJ_ATTY_ZIP,
                   LSJFILE.LSJ_ATTY_PHONE
            INTO :QLSJ-REC
            FROM DBO.LSJFILE
            WHERE LSJFILE.LSJ_BRNO = :QLSJ-BRNO
              AND LSJFILE.LSJ_ACCTNO = :QLSJ-ACCTNO
              AND LSJFILE.LSJ_SEQNO = :QLSJ-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSJ1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSJ-FIELDS.

      *-----------------------------------------------------------------
       READ-LSJ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSJ-PATH-OWNBR  TO LSJ-BRNO.
           MOVE LSJ-PATH-SQL    TO E-FILE.
           MOVE LSJ1-KEY        TO E-KEYX.
           INITIALIZE QLSJ-REC.

           IF ( LSJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSJ1_CURSOR INTO :QLSJ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSJ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSJ-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LSJ1-FILE.

           MOVE LSJ-PATH-OWNBR  TO LSJ-BRNO.
           ACCEPT LSJ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LSJ-PATH-SQL    TO E-FILE.
           MOVE LSJ1-KEY        TO E-KEYX.
           PERFORM SET-LSJ-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LSJFILE
             (LSJFILE.LSJ_BRNO,
              LSJFILE.LSJ_ACCTNO,
              LSJFILE.LSJ_SEQNO,
              LSJFILE.LSJ_LTOUCH_DATE,
              LSJFILE.LSJ_FILEDATE,
              LSJFILE.LSJ_CASE_NO,
              LSJFILE.LSJ_AMT,
              LSJFILE.LSJ_COUNTY,
              LSJFILE.LSJ_CLERK_NAME,
              LSJFILE.LSJ_CLERK_ADDR,
              LSJFILE.LSJ_CLERK_CITY,
              LSJFILE.LSJ_CLERK_STATE,
              LSJFILE.LSJ_CLERK_ZIP,
              LSJFILE.LSJ_CLERK_PHONE,
              LSJFILE.LSJ_SHERIFF_FEE,
              LSJFILE.LSJ_COURT_FEE,
              LSJFILE.LSJ_ATTY_FEE,
              LSJFILE.LSJ_DISMISS_DATE,
              LSJFILE.LSJ_GARN_INFO,
              LSJFILE.LSJ_EMPLOY_NAME,
              LSJFILE.LSJ_EMPLOY_ADDR,
              LSJFILE.LSJ_EMPLOY_CITY,
              LSJFILE.LSJ_EMPLOY_STATE,
              LSJFILE.LSJ_EMPLOY_ZIP,
              LSJFILE.LSJ_GARN_DATE,
              LSJFILE.LSJ_JD_FILED_ON,
              LSJFILE.LSJ_GARN_FILED_ON,
              LSJFILE.LSJ_ATTY_NAME,
              LSJFILE.LSJ_ATTY_ADDR,
              LSJFILE.LSJ_ATTY_CITY,
              LSJFILE.LSJ_ATTY_STATE,
              LSJFILE.LSJ_ATTY_ZIP,
              LSJFILE.LSJ_ATTY_PHONE)
            VALUES
             (:QLSJ-BRNO,
              :QLSJ-ACCTNO,
              :QLSJ-SEQNO,
              :QLSJ-LTOUCH-DATE,
              :QLSJ-FILEDATE,
              :QLSJ-CASE-NO,
              :QLSJ-AMT,
              :QLSJ-COUNTY,
              :QLSJ-CLERK-NAME,
              :QLSJ-CLERK-ADDR,
              :QLSJ-CLERK-CITY,
              :QLSJ-CLERK-STATE,
              :QLSJ-CLERK-ZIP,
              :QLSJ-CLERK-PHONE,
              :QLSJ-SHERIFF-FEE,
              :QLSJ-COURT-FEE,
              :QLSJ-ATTY-FEE,
              :QLSJ-DISMISS-DATE,
              :QLSJ-GARN-INFO,
              :QLSJ-EMPLOY-NAME,
              :QLSJ-EMPLOY-ADDR,
              :QLSJ-EMPLOY-CITY,
              :QLSJ-EMPLOY-STATE,
              :QLSJ-EMPLOY-ZIP,
              :QLSJ-GARN-DATE,
              :QLSJ-JD-FILED-ON,
              :QLSJ-GARN-FILED-ON,
              :QLSJ-ATTY-NAME,
              :QLSJ-ATTY-ADDR,
              :QLSJ-ATTY-CITY,
              :QLSJ-ATTY-STATE,
              :QLSJ-ATTY-ZIP,
              :QLSJ-ATTY-PHONE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LSJ1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LSJ1-FILE.

           MOVE LSJ-PATH-OWNBR  TO LSJ-BRNO.
           ACCEPT LSJ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LSJ-PATH-SQL    TO E-FILE.
           MOVE LSJ1-KEY        TO E-KEYX.
           PERFORM SET-LSJ-FIELDS.

           EXEC SQL
            UPDATE DBO.LSJFILE
             SET
              LSJFILE.LSJ_BRNO = :QLSJ-BRNO,
              LSJFILE.LSJ_ACCTNO = :QLSJ-ACCTNO,
              LSJFILE.LSJ_SEQNO = :QLSJ-SEQNO,
              LSJFILE.LSJ_LTOUCH_DATE =
                 :QLSJ-LTOUCH-DATE,
              LSJFILE.LSJ_FILEDATE =
                 :QLSJ-FILEDATE,
              LSJFILE.LSJ_CASE_NO = :QLSJ-CASE-NO,
              LSJFILE.LSJ_AMT = :QLSJ-AMT,
              LSJFILE.LSJ_COUNTY = :QLSJ-COUNTY,
              LSJFILE.LSJ_CLERK_NAME = :QLSJ-CLERK-NAME,
              LSJFILE.LSJ_CLERK_ADDR = :QLSJ-CLERK-ADDR,
              LSJFILE.LSJ_CLERK_CITY = :QLSJ-CLERK-CITY,
              LSJFILE.LSJ_CLERK_STATE = :QLSJ-CLERK-STATE,
              LSJFILE.LSJ_CLERK_ZIP = :QLSJ-CLERK-ZIP,
              LSJFILE.LSJ_CLERK_PHONE = :QLSJ-CLERK-PHONE,
              LSJFILE.LSJ_SHERIFF_FEE = :QLSJ-SHERIFF-FEE,
              LSJFILE.LSJ_COURT_FEE = :QLSJ-COURT-FEE,
              LSJFILE.LSJ_ATTY_FEE = :QLSJ-ATTY-FEE,
              LSJFILE.LSJ_DISMISS_DATE = :QLSJ-DISMISS-DATE,
              LSJFILE.LSJ_GARN_INFO = :QLSJ-GARN-INFO,
              LSJFILE.LSJ_EMPLOY_NAME = :QLSJ-EMPLOY-NAME,
              LSJFILE.LSJ_EMPLOY_ADDR = :QLSJ-EMPLOY-ADDR,
              LSJFILE.LSJ_EMPLOY_CITY = :QLSJ-EMPLOY-CITY,
              LSJFILE.LSJ_EMPLOY_STATE = :QLSJ-EMPLOY-STATE,
              LSJFILE.LSJ_EMPLOY_ZIP = :QLSJ-EMPLOY-ZIP,
              LSJFILE.LSJ_GARN_DATE = :QLSJ-GARN-DATE,
              LSJFILE.LSJ_JD_FILED_ON = :QLSJ-JD-FILED-ON,
              LSJFILE.LSJ_GARN_FILED_ON = :QLSJ-GARN-FILED-ON,
              LSJFILE.LSJ_ATTY_NAME = :QLSJ-ATTY-NAME,
              LSJFILE.LSJ_ATTY_ADDR = :QLSJ-ATTY-ADDR,
              LSJFILE.LSJ_ATTY_CITY = :QLSJ-ATTY-CITY,
              LSJFILE.LSJ_ATTY_STATE = :QLSJ-ATTY-STATE,
              LSJFILE.LSJ_ATTY_ZIP = :QLSJ-ATTY-ZIP,
              LSJFILE.LSJ_ATTY_PHONE = :QLSJ-ATTY-PHONE
             WHERE LSJFILE.LSJ_BRNO = :QLSJ-BRNO
               AND LSJFILE.LSJ_ACCTNO = :QLSJ-ACCTNO
               AND LSJFILE.LSJ_SEQNO = :QLSJ-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LSJ1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LSJ1-FILE.

           MOVE LSJ-PATH-OWNBR  TO LSJ-BRNO.
           PERFORM DELETE-IT.
           MOVE LSJ-PATH-SQL    TO E-FILE.
           MOVE LSJ1-KEY        TO E-KEYX.
           PERFORM SET-LSJ-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LSJFILE
             WHERE LSJFILE.LSJ_BRNO = :QLSJ-BRNO
               AND LSJFILE.LSJ_ACCTNO = :QLSJ-ACCTNO
               AND LSJFILE.LSJ_SEQNO = :QLSJ-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LSJ1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LSJ1-FILE.

           PERFORM CLOSE-IT.

           IF ( LSJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSJ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSJ1-CURSOR-STAT.

