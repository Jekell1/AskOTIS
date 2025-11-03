      * COPYMEMBER: LIBLP/LPLSR1IN
       LOAD-LSR1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSR-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSR-ALL-DATA
              MOVE LSR-ALL-PATH             TO LSR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSR-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSR-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSR-ALL-DATA
              MOVE LSR-ALL-PATH             TO LSR-PATH.

      *-----------------------------------------------------------------
       OPEN-LSR1-FILE.
           PERFORM LOAD-LSR1-FILE.
           PERFORM OPEN-IT.
           MOVE LSR-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSR1-FILE.
           PERFORM START-IT.
           MOVE LSR-PATH-OWNBR  TO LSR-BRNO.
           MOVE LSR-PATH-SQL    TO E-FILE.
           MOVE LSR1-KEY        TO E-KEYX.

           IF ( LSR1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSR1-FILE.

           IF ( QLSR1-WEND-BRNO = ZEROES    ) OR
              ( QLSR1-WEND-BRNO NOT NUMERIC )
              MOVE LSR-PATH-OWNBR        TO QLSR1-WBEG-BRNO
              MOVE LSR-PATH-OWNBR        TO QLSR1-WEND-BRNO
              MOVE LSR-ACCTNO            TO QLSR1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSR1-WEND-ACCTNO
              MOVE LSR-SEQNO             TO QLSR1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSR1-WEND-SEQNO.

           EXEC SQL
            DECLARE LSR1_CURSOR CURSOR FOR
            SELECT LSRFILE.LSR_BRNO,
                   LSRFILE.LSR_ACCTNO,
                   LSRFILE.LSR_SEQNO,
                   CAST(LSRFILE.LSR_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LSRFILE.LSR_SCRA_ACTIVE,
                   CAST(LSRFILE.LSR_SCRA_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSRFILE.LSR_SCRA_END_DATE
                                                  AS VARCHAR(10)),
                   LSRFILE.LSR_SCRA_BORROWER,
                   CAST(LSRFILE.LSR_CURRENT_RESCHED_DATE
                                                  AS VARCHAR(10)),
                   LSRFILE.LSR_CURRENT_REASON_CODE,
                   CAST(LSRFILE.LSR_INITIAL_RESCHED_DATE
                                                  AS VARCHAR(10)),
                   LSRFILE.LSR_INITIAL_REASON_CODE,
                   CAST(LSRFILE.LSR_INITIAL_TDR_DATE
                                                  AS VARCHAR(10)),
                   LSRFILE.LSR_INITIAL_TDR_REASON,
                   CAST(LSRFILE.LSR_CURRENT_TDR_DATE
                                                  AS VARCHAR(10)),
                   LSRFILE.LSR_CURRENT_TDR_REASON
            FROM DBO.LSRFILE
            WHERE LSRFILE.LSR_BRNO  = :QLSR1-WBEG-BRNO
              AND LSRFILE.LSR_ACCTNO >= :QLSR1-WBEG-ACCTNO
              AND LSRFILE.LSR_ACCTNO <= :QLSR1-WEND-ACCTNO
              AND LSRFILE.LSR_SEQNO >= :QLSR1-WBEG-SEQNO
              AND LSRFILE.LSR_SEQNO <= :QLSR1-WEND-SEQNO
            ORDER BY LSRFILE.LSR_BRNO,
                     LSRFILE.LSR_ACCTNO,
                     LSRFILE.LSR_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSR1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSR1-WBEG-BRNO
                          QLSR1-WEND-BRNO
                          QLSR1-WBEG-ACCTNO
                          QLSR1-WEND-ACCTNO
                          QLSR1-WBEG-SEQNO
                          QLSR1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSR1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSR1-FILE.
           PERFORM READ-IT.
           MOVE LSR-PATH-OWNBR  TO LSR-BRNO. 
           MOVE LSR-PATH-SQL    TO E-FILE.
           MOVE LSR1-KEY        TO E-KEYX.
           INITIALIZE QLSR-REC.

           MOVE LSR-BRNO   TO QLSR-BRNO.
           MOVE LSR-ACCTNO TO QLSR-ACCTNO.
           MOVE LSR-SEQNO TO QLSR-SEQNO.

           EXEC SQL
            SELECT LSRFILE.LSR_BRNO,
                   LSRFILE.LSR_ACCTNO,
                   LSRFILE.LSR_SEQNO,
                   CAST(LSRFILE.LSR_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LSRFILE.LSR_SCRA_ACTIVE,
                   CAST(LSRFILE.LSR_SCRA_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSRFILE.LSR_SCRA_END_DATE
                                                  AS VARCHAR(10)),
                   LSRFILE.LSR_SCRA_BORROWER,
                   CAST(LSRFILE.LSR_CURRENT_RESCHED_DATE
                                                  AS VARCHAR(10)),
                   LSRFILE.LSR_CURRENT_REASON_CODE,
                   CAST(LSRFILE.LSR_INITIAL_RESCHED_DATE
                                                  AS VARCHAR(10)),
                   LSRFILE.LSR_INITIAL_REASON_CODE,
                   CAST(LSRFILE.LSR_INITIAL_TDR_DATE
                                                  AS VARCHAR(10)),
                   LSRFILE.LSR_INITIAL_TDR_REASON,
                   CAST(LSRFILE.LSR_CURRENT_TDR_DATE
                                                  AS VARCHAR(10)),
                   LSRFILE.LSR_CURRENT_TDR_REASON
            INTO :QLSR-REC
            FROM DBO.LSRFILE
            WHERE LSRFILE.LSR_BRNO = :QLSR-BRNO
              AND LSRFILE.LSR_ACCTNO = :QLSR-ACCTNO
              AND LSRFILE.LSR_SEQNO = :QLSR-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSR1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSR-FIELDS.

      *-----------------------------------------------------------------
       READ-LSR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSR-PATH-OWNBR  TO LSR-BRNO.
           MOVE LSR-PATH-SQL    TO E-FILE.
           MOVE LSR1-KEY        TO E-KEYX.
           INITIALIZE QLSR-REC.

           IF ( LSR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSR1_CURSOR INTO :QLSR-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSR1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSR-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LSR1-FILE.

           MOVE LSR-PATH-OWNBR  TO LSR-BRNO.
           ACCEPT LSR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LSR-PATH-SQL    TO E-FILE.
           MOVE LSR1-KEY        TO E-KEYX.
           PERFORM SET-LSR-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LSRFILE
             (LSRFILE.LSR_BRNO,
              LSRFILE.LSR_ACCTNO,
              LSRFILE.LSR_SEQNO,
              LSRFILE.LSR_LTOUCH_DATE,
              LSRFILE.LSR_SCRA_ACTIVE,
              LSRFILE.LSR_SCRA_START_DATE,
              LSRFILE.LSR_SCRA_END_DATE,
              LSRFILE.LSR_SCRA_BORROWER,
              LSRFILE.LSR_CURRENT_RESCHED_DATE,
              LSRFILE.LSR_CURRENT_REASON_CODE,
              LSRFILE.LSR_INITIAL_RESCHED_DATE,
              LSRFILE.LSR_INITIAL_REASON_CODE,
              LSRFILE.LSR_INITIAL_TDR_DATE,
              LSRFILE.LSR_INITIAL_TDR_REASON,
              LSRFILE.LSR_CURRENT_TDR_DATE,
              LSRFILE.LSR_CURRENT_TDR_REASON)
            VALUES
             (:QLSR-BRNO,
              :QLSR-ACCTNO,
              :QLSR-SEQNO,
              :QLSR-LTOUCH-DATE,
              :QLSR-SCRA-ACTIVE,
              :QLSR-SCRA-START-DATE,
              :QLSR-SCRA-END-DATE,
              :QLSR-SCRA-BORROWER,
              :QLSR-CURRENT-RESCHED-DATE,
              :QLSR-CURRENT-REASON-CODE,
              :QLSR-INITIAL-RESCHED-DATE,
              :QLSR-INITIAL-REASON-CODE,
              :QLSR-INITIAL-TDR-DATE,
              :QLSR-INITIAL-TDR-REASON,
              :QLSR-CURRENT-TDR-DATE,
              :QLSR-CURRENT-TDR-REASON)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LSR1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LSR1-FILE.

           MOVE LSR-PATH-OWNBR  TO LSR-BRNO.
           ACCEPT LSR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LSR-PATH-SQL    TO E-FILE.
           MOVE LSR1-KEY        TO E-KEYX.
           PERFORM SET-LSR-FIELDS.

           EXEC SQL
            UPDATE DBO.LSRFILE
             SET
              LSRFILE.LSR_BRNO = :QLSR-BRNO,
              LSRFILE.LSR_ACCTNO = :QLSR-ACCTNO,
              LSRFILE.LSR_SEQNO = :QLSR-SEQNO,
              LSRFILE.LSR_LTOUCH_DATE =
                 :QLSR-LTOUCH-DATE,
              LSRFILE.LSR_SCRA_ACTIVE = :QLSR-SCRA-ACTIVE,
              LSRFILE.LSR_SCRA_START_DATE =
                 :QLSR-SCRA-START-DATE,
              LSRFILE.LSR_SCRA_END_DATE =
                 :QLSR-SCRA-END-DATE,
              LSRFILE.LSR_SCRA_BORROWER = :QLSR-SCRA-BORROWER,
              LSRFILE.LSR_CURRENT_RESCHED_DATE =
                 :QLSR-CURRENT-RESCHED-DATE,
              LSRFILE.LSR_CURRENT_REASON_CODE =
                   :QLSR-CURRENT-REASON-CODE,
              LSRFILE.LSR_INITIAL_RESCHED_DATE =
                 :QLSR-INITIAL-RESCHED-DATE,
              LSRFILE.LSR_INITIAL_REASON_CODE =
                   :QLSR-INITIAL-REASON-CODE,
              LSRFILE.LSR_INITIAL_TDR_DATE = :QLSR-INITIAL-TDR-DATE,
              LSRFILE.LSR_INITIAL_TDR_REASON =
                   :QLSR-INITIAL-TDR-REASON,
              LSRFILE.LSR_CURRENT_TDR_DATE = :QLSR-CURRENT-TDR-DATE,
              LSRFILE.LSR_CURRENT_TDR_REASON =
                   :QLSR-CURRENT-TDR-REASON
             WHERE LSRFILE-LSR_BRNO = :QLSR-BRNO
               AND LSRFILE-LSR_ACCTNO = :QLSR-ACCTNO
               AND LSRFILE-LSR_SEQNO = :QLSR-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LSR1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LSR1-FILE.

           MOVE LSR-PATH-OWNBR  TO LSR-BRNO.
           PERFORM DELETE-IT.
           MOVE LSR-PATH-SQL    TO E-FILE.
           MOVE LSR1-KEY        TO E-KEYX.
           PERFORM SET-LSR-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LSRFILE
             WHERE LSRFILE.LSR_BRNO = :QLSR-BRNO
               AND LSRFILE.LSR_ACCTNO = :QLSR-ACCTNO
               AND LSRFILE.LSR_SEQNO = :QLSR-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LSR1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LSR1-FILE.

           PERFORM CLOSE-IT.

           IF ( LSR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSR1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSR1-CURSOR-STAT.

