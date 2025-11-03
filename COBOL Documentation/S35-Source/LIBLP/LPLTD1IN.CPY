      * COPYMEMBER: LIBLP/LPLTD1IN DEBIT CARD TRAILER
      * REV:
      *
       LOAD-LTD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTD-ALL-DATA
              MOVE LTD-ALL-PATH             TO LTD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTD-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTD-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTD-ALL-DATA
              MOVE LTD-ALL-PATH             TO LTD-PATH.

      *-----------------------------------------------------------------
       OPEN-LTD1-FILE.
           PERFORM LOAD-LTD1-FILE.
           PERFORM OPEN-IT.
           MOVE LTD-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTD1-FILE.
           PERFORM START-IT.
           MOVE LTD-PATH-OWNBR  TO LTD-BRNO.
           MOVE LTD-PATH-SQL    TO E-FILE.
           MOVE LTD1-KEY        TO E-KEYX.

           IF ( LTD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTD1-FILE.

           IF ( QLTD1-WEND-BRNO = ZEROES    ) OR
              ( QLTD1-WEND-BRNO NOT NUMERIC )
              MOVE LTD-PATH-OWNBR        TO QLTD1-WBEG-BRNO
              MOVE LTD-PATH-OWNBR        TO QLTD1-WEND-BRNO
              MOVE LTD-ACCTNO            TO QLTD1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTD1-WEND-ACCTNO
              MOVE LTD-SEQNO             TO QLTD1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTD1-WEND-SEQNO.


           EXEC SQL
            DECLARE LTD1_CURSOR CURSOR FOR
            SELECT LTDFILE.LTD_BRNO,
                   LTDFILE.LTD_ACCTNO,
                   LTDFILE.LTD_SEQNO,
                   CAST(LTDFILE.LTD_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LTDFILE.LTD_DEBIT_CARD_NUMBER,
                   LTDFILE.LTD_DEBIT_CARD_AMT,
                   CAST(LTDFILE.LTD_LOANDATE
                                                  AS VARCHAR(10)),
                   CAST(LTDFILE.LTD_VOID_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTDFILE.LTD_CLEARED_DATE
                                                  AS VARCHAR(10)),
                   LTDFILE.LTD_CLEARED_AMT
            FROM DBO.LTDFILE
            WHERE LTDFILE.LTD_BRNO  = :QLTD1-WBEG-BRNO
              AND LTDFILE.LTD_ACCTNO >= :QLTD1-WBEG-ACCTNO
              AND LTDFILE.LTD_ACCTNO <= :QLTD1-WEND-ACCTNO
              AND LTDFILE.LTD_SEQNO >= :QLTD1-WBEG-SEQNO
              AND LTDFILE.LTD_SEQNO <= :QLTD1-WEND-SEQNO
            ORDER BY LTDFILE.LTD_BRNO,
                     LTDFILE.LTD_ACCTNO,
                     LTDFILE.LTD_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTD1-WBEG-BRNO
                          QLTD1-WEND-BRNO
                          QLTD1-WBEG-ACCTNO
                          QLTD1-WEND-ACCTNO
                          QLTD1-WBEG-SEQNO
                          QLTD1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTD1-FILE.
           PERFORM READ-IT.
           MOVE LTD-PATH-OWNBR  TO LTD-BRNO. 
           MOVE LTD-PATH-SQL    TO E-FILE.
           MOVE LTD1-KEY        TO E-KEYX.
           INITIALIZE QLTD-REC.

           MOVE LTD-BRNO   TO QLTD-BRNO.
           MOVE LTD-ACCTNO TO QLTD-ACCTNO.
           MOVE LTD-SEQNO  TO QLTD-SEQNO.

           EXEC SQL
            SELECT LTDFILE.LTD_BRNO,
                   LTDFILE.LTD_ACCTNO,
                   LTDFILE.LTD_SEQNO,
                   CAST(LTDFILE.LTD_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LTDFILE.LTD_DEBIT_CARD_NUMBER,
                   LTDFILE.LTD_DEBIT_CARD_AMT,
                   CAST(LTDFILE.LTD_LOANDATE
                                                  AS VARCHAR(10)),
                   CAST(LTDFILE.LTD_VOID_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTDFILE.LTD_CLEARED_DATE
                                                  AS VARCHAR(10)),
                   LTDFILE.LTD_CLEARED_AMT
            INTO :QLTD-REC
            FROM DBO.LTDFILE
            WHERE LTDFILE.LTD_BRNO = :QLTD-BRNO
              AND LTDFILE.LTD_ACCTNO = :QLTD-ACCTNO
              AND LTDFILE.LTD_SEQNO = :QLTD-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTD-FIELDS.

      *-----------------------------------------------------------------
       READ-LTD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTD-PATH-OWNBR  TO LTD-BRNO.
           MOVE LTD-PATH-SQL    TO E-FILE.
           MOVE LTD1-KEY        TO E-KEYX.
           INITIALIZE QLTD-REC.

           IF ( LTD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTD1_CURSOR INTO :QLTD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTD-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LTD1-FILE.

           MOVE LTD-PATH-OWNBR  TO LTD-BRNO.
           ACCEPT LTD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTD-PATH-SQL    TO E-FILE.
           MOVE LTD1-KEY        TO E-KEYX.
           PERFORM SET-LTD-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LTDFILE
             (LTDFILE.LTD_BRNO,
              LTDFILE.LTD_ACCTNO,
              LTDFILE.LTD_SEQNO,
              LTDFILE.LTD_LTOUCH_DATE,
              LTDFILE.LTD_DEBIT_CARD_NUMBER,
              LTDFILE.LTD_DEBIT_CARD_AMT,
              LTDFILE.LTD_LOANDATE,
              LTDFILE.LTD_VOID_DATE,
              LTDFILE.LTD_CLEARED_DATE,
              LTDFILE.LTD_CLEARED_AMT)
            VALUES
             (:QLTD-BRNO,
              :QLTD-ACCTNO,
              :QLTD-SEQNO,
              :QLTD-LTOUCH-DATE,
              :QLTD-DEBIT-CARD-NUMBER,
              :QLTD-DEBIT-CARD-AMT,
              :QLTD-LOANDATE,
              :QLTD-VOID-DATE,
              :QLTD-CLEARED-DATE,
              :QLTD-CLEARED-AMT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTD1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LTD1-FILE.

           MOVE LTD-PATH-OWNBR  TO LTD-BRNO.
           ACCEPT LTD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTD-PATH-SQL    TO E-FILE.
           MOVE LTD1-KEY        TO E-KEYX.
           PERFORM SET-LTD-FIELDS.

           EXEC SQL
            UPDATE DBO.LTDFILE
             SET
              LTDFILE.LTD_BRNO = :QLTD-BRNO,
              LTDFILE.LTD_ACCTNO = :QLTD-ACCTNO,
              LTDFILE.LTD_SEQNO = :QLTD-SEQNO,
              LTDFILE.LTD_LTOUCH_DATE = :QLTD-LTOUCH-DATE,
              LTDFILE.LTD_DEBIT_CARD_NUMBER =
                   :QLTD-DEBIT-CARD-NUMBER,
              LTDFILE.LTD_DEBIT_CARD_AMT = :QLTD-DEBIT-CARD-AMT,
              LTDFILE.LTD_LOANDATE = :QLTD-LOANDATE,
              LTDFILE.LTD_VOID_DATE = :QLTD-VOID-DATE,
              LTDFILE.LTD_CLEARED_DATE = :QLTD-CLEARED-DATE,
              LTDFILE.LTD_CLEARED_AMT = :QLTD-CLEARED-AMT
             WHERE LTDFILE.LTD_BRNO = :QLTD-BRNO
               AND LTDFILE.LTD_ACCTNO = :QLTD-ACCTNO
               AND LTDFILE.LTD_SEQNO = :QLTD-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTD1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LTD1-FILE.

           MOVE LTD-PATH-OWNBR  TO LTD-BRNO.
           PERFORM DELETE-IT.
           MOVE LTD-PATH-SQL    TO E-FILE.
           MOVE LTD1-KEY        TO E-KEYX.
           PERFORM SET-LTD-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LTDFILE
             WHERE LTDFILE.LTD_BRNO = :QLTD-BRNO
               AND LTDFILE.LTD_ACCTNO = :QLTD-ACCTNO
               AND LTDFILE.LTD_SEQNO = :QLTD-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LTD1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LTD1-FILE.

           PERFORM CLOSE-IT.

           IF ( LTD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTD1-CURSOR-STAT.

