      * COPYMEMBER: LIBLP/LPLSR1RN
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
       CLOSE-LSR1-FILE.

           PERFORM CLOSE-IT.
           IF ( LSR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSR1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSR1-CURSOR-STAT.


