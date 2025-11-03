      * COPYMEMBER: LIBLP/LPLSP1RN
       LOAD-LSP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSP-ALL-DATA
              MOVE LSP-ALL-PATH             TO LSP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSP-ALL-DATA
              MOVE LSP-ALL-PATH             TO LSP-PATH.

      *-----------------------------------------------------------------
       OPEN-LSP1-FILE.
           PERFORM LOAD-LSP1-FILE.
           PERFORM OPEN-IT.
           MOVE LSP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSP1-FILE.
           PERFORM START-IT.
           MOVE LSP-PATH-OWNBR  TO LSP-BRNO.
           MOVE LSP-PATH-SQL    TO E-FILE.
           MOVE LSP1-KEY        TO E-KEYX.

           IF ( LSP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSP1-FILE.

           IF ( QLSP1-WEND-BRNO = ZEROES    ) OR
              ( QLSP1-WEND-BRNO NOT NUMERIC )
              MOVE LSP-PATH-OWNBR        TO QLSP1-WBEG-BRNO
              MOVE LSP-PATH-OWNBR        TO QLSP1-WEND-BRNO
              MOVE LSP-ACCTNO            TO QLSP1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSP1-WEND-ACCTNO
              MOVE LSP-SEQNO             TO QLSP1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSP1-WEND-SEQNO.

           EXEC SQL
            DECLARE LSP1_CURSOR CURSOR FOR
            SELECT LSPFILE.LSP_BRNO,
                   LSPFILE.LSP_ACCTNO,
                   LSPFILE.LSP_SEQNO,
                   CAST(LSPFILE.LSP_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LSPFILE.LSP_REASON_CODE,
                   LSPFILE.LSP_PERIODS_TO_DEFER,
                   LSPFILE.LSP_REASON_FOR_DEFER,
                   LSPFILE.LSP_MORTGAGE_PAST_DUE,
                   LSPFILE.LSP_MORTGAGE_BALANCE,
                   LSPFILE.LSP_ATTEMPT_REFINANCE,
                   CAST(LSPFILE.LSP_INFO_DATE
                                                  AS VARCHAR(10)),
                   LSPFILE.LSP_INFO_USERID,
                   LSPFILE.LSP_BR_MANAGER_COMMENTS,
                   CAST(LSPFILE.LSP_BR_MANAGER_DATE
                                                  AS VARCHAR(10)),
                   LSPFILE.LSP_BR_MANAGER_USERID,
                   LSPFILE.LSP_AVP_COMMENTS,
                   CAST(LSPFILE.LSP_AVP_DATE
                                                  AS VARCHAR(10)),
                   LSPFILE.LSP_AVP_USERID,
                   LSPFILE.LSP_AVP_APPROVE,
                   LSPFILE.LSP_VP_COMMENTS,
                   CAST(LSPFILE.LSP_VP_DATE
                                                  AS VARCHAR(10)),
                   LSPFILE.LSP_VP_USERID,
                   LSPFILE.LSP_VP_APPROVE
            FROM DBO.LSPFILE
            WHERE LSPFILE.LSP_BRNO  = :QLSP1-WBEG-BRNO
              AND LSPFILE.LSP_ACCTNO >= :QLSP1-WBEG-ACCTNO
              AND LSPFILE.LSP_ACCTNO <= :QLSP1-WEND-ACCTNO
              AND LSPFILE.LSP_SEQNO >= :QLSP1-WBEG-SEQNO
              AND LSPFILE.LSP_SEQNO <= :QLSP1-WEND-SEQNO
            ORDER BY LSPFILE.LSP_BRNO,
                     LSPFILE.LSP_ACCTNO,
                     LSPFILE.LSP_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSP1-WBEG-BRNO
                          QLSP1-WEND-BRNO
                          QLSP1-WBEG-ACCTNO
                          QLSP1-WEND-ACCTNO
                          QLSP1-WBEG-SEQNO
                          QLSP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSP1-FILE.
           PERFORM READ-IT.
           MOVE LSP-PATH-OWNBR  TO LSP-BRNO. 
           MOVE LSP-PATH-SQL    TO E-FILE.
           MOVE LSP1-KEY        TO E-KEYX.
           INITIALIZE QLSP-REC.

           MOVE LSP-BRNO   TO QLSP-BRNO.
           MOVE LSP-ACCTNO TO QLSP-ACCTNO.
           MOVE LSP-SEQNO TO QLSP-SEQNO.

           EXEC SQL
            SELECT LSPFILE.LSP_BRNO,
                   LSPFILE.LSP_ACCTNO,
                   LSPFILE.LSP_SEQNO,
                   CAST(LSPFILE.LSP_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LSPFILE.LSP_REASON_CODE,
                   LSPFILE.LSP_PERIODS_TO_DEFER,
                   LSPFILE.LSP_REASON_FOR_DEFER,
                   LSPFILE.LSP_MORTGAGE_PAST_DUE,
                   LSPFILE.LSP_MORTGAGE_BALANCE,
                   LSPFILE.LSP_ATTEMPT_REFINANCE,
                   CAST(LSPFILE.LSP_INFO_DATE
                                                  AS VARCHAR(10)),
                   LSPFILE.LSP_INFO_USERID,
                   LSPFILE.LSP_BR_MANAGER_COMMENTS,
                   CAST(LSPFILE.LSP_BR_MANAGER_DATE
                                                  AS VARCHAR(10)),
                   LSPFILE.LSP_BR_MANAGER_USERID,
                   LSPFILE.LSP_AVP_COMMENTS,
                   CAST(LSPFILE.LSP_AVP_DATE
                                                  AS VARCHAR(10)),
                   LSPFILE.LSP_AVP_USERID,
                   LSPFILE.LSP_AVP_APPROVE,
                   LSPFILE.LSP_VP_COMMENTS,
                   CAST(LSPFILE.LSP_VP_DATE
                                                  AS VARCHAR(10)),
                   LSPFILE.LSP_VP_USERID,
                   LSPFILE.LSP_VP_APPROVE
            INTO :QLSP-REC
            FROM DBO.LSPFILE
            WHERE LSPFILE.LSP_BRNO = :QLSP-BRNO
              AND LSPFILE.LSP_ACCTNO = :QLSP-ACCTNO
              AND LSPFILE.LSP_SEQNO = :QLSP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSP-FIELDS.

      *-----------------------------------------------------------------
       READ-LSP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSP-PATH-OWNBR  TO LSP-BRNO.
           MOVE LSP-PATH-SQL    TO E-FILE.
           MOVE LSP1-KEY        TO E-KEYX.
           INITIALIZE QLSP-REC.

           IF ( LSP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSP1_CURSOR INTO :QLSP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LSP1-FILE.

           PERFORM CLOSE-IT.
           IF ( LSP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSP1-CURSOR-STAT.

