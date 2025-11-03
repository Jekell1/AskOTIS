      * COPYMEMBER: LIBLP/LPLSP1IN
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
       WRITE-LSP1-FILE.

           MOVE LSP-PATH-OWNBR  TO LSP-BRNO.
           ACCEPT LSP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LSP-PATH-SQL    TO E-FILE.
           MOVE LSP1-KEY        TO E-KEYX.
           PERFORM SET-LSP-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LSPFILE
             (LSPFILE.LSP_BRNO,
              LSPFILE.LSP_ACCTNO,
              LSPFILE.LSP_SEQNO,
              LSPFILE.LSP_LTOUCH_DATE,
              LSPFILE.LSP_REASON_CODE,
              LSPFILE.LSP_PERIODS_TO_DEFER,
              LSPFILE.LSP_REASON_FOR_DEFER,
              LSPFILE.LSP_MORTGAGE_PAST_DUE,
              LSPFILE.LSP_MORTGAGE_BALANCE,
              LSPFILE.LSP_ATTEMPT_REFINANCE,
              LSPFILE.LSP_INFO_DATE,
              LSPFILE.LSP_INFO_USERID,
              LSPFILE.LSP_BR_MANAGER_COMMENTS,
              LSPFILE.LSP_BR_MANAGER_DATE,
              LSPFILE.LSP_BR_MANAGER_USERID,
              LSPFILE.LSP_AVP_COMMENTS,
              LSPFILE.LSP_AVP_DATE,
              LSPFILE.LSP_AVP_USERID,
              LSPFILE.LSP_AVP_APPROVE,
              LSPFILE.LSP_VP_COMMENTS,
              LSPFILE.LSP_VP_DATE,
              LSPFILE.LSP_VP_USERID,
              LSPFILE.LSP_VP_APPROVE)
            VALUES
             (:QLSP-BRNO,
              :QLSP-ACCTNO,
              :QLSP-SEQNO,
              :QLSP-LTOUCH-DATE,
              :QLSP-REASON-CODE,
              :QLSP-PERIODS-TO-DEFER,
              :QLSP-REASON-FOR-DEFER,
              :QLSP-MORTGAGE-PAST-DUE,
              :QLSP-MORTGAGE-BALANCE,
              :QLSP-ATTEMPT-REFINANCE,
              :QLSP-INFO-DATE,
              :QLSP-INFO-USERID,
              :QLSP-BR-MANAGER-COMMENTS,
              :QLSP-BR-MANAGER-DATE,
              :QLSP-BR-MANAGER-USERID,
              :QLSP-AVP-COMMENTS,
              :QLSP-AVP-DATE,
              :QLSP-AVP-USERID,
              :QLSP-AVP-APPROVE,
              :QLSP-VP-COMMENTS,
              :QLSP-VP-DATE,
              :QLSP-VP-USERID,
              :QLSP-VP-APPROVE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LSP1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LSP1-FILE.

           MOVE LSP-PATH-OWNBR  TO LSP-BRNO.
           ACCEPT LSP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LSP-PATH-SQL    TO E-FILE.
           MOVE LSP1-KEY        TO E-KEYX.
           PERFORM SET-LSP-FIELDS.

           EXEC SQL
            UPDATE DBO.LSPFILE
             SET
              LSPFILE.LSP_BRNO = :QLSP-BRNO,
              LSPFILE.LSP_ACCTNO = :QLSP-ACCTNO,
              LSPFILE.LSP_SEQNO = :QLSP-SEQNO,
              LSPFILE.LSP_LTOUCH_DATE =
                 :QLSP-LTOUCH-DATE,
              LSPFILE.LSP_REASON_CODE = :QLSP-REASON-CODE,
              LSPFILE.LSP_PERIODS_TO_DEFER =
                   :QLSP-PERIODS-TO-DEFER,
              LSPFILE.LSP_REASON_FOR_DEFER =
                   :QLSP-REASON-FOR-DEFER,
              LSPFILE.LSP_MORTGAGE_PAST_DUE =
                   :QLSP-MORTGAGE-PAST-DUE,
              LSPFILE.LSP_MORTGAGE_BALANCE =
                   :QLSP-MORTGAGE-BALANCE,
              LSPFILE.LSP_ATTEMPT_REFINANCE =
                   :QLSP-ATTEMPT-REFINANCE,
              LSPFILE.LSP_INFO_DATE =
                 :QLSP-INFO-DATE,
              LSPFILE.LSP_INFO_USERID = :QLSP-INFO-USERID,
              LSPFILE.LSP_BR_MANAGER_COMMENTS =
                   :QLSP-BR-MANAGER-COMMENTS,
              LSPFILE.LSP_BR_MANAGER_DATE =
                 :QLSP-BR-MANAGER-DATE,
              LSPFILE.LSP_BR_MANAGER_USERID =
                   :QLSP-BR-MANAGER-USERID,
              LSPFILE.LSP_AVP_COMMENTS = :QLSP-AVP-COMMENTS,
              LSPFILE.LSP_AVP_DATE =
                 :QLSP-AVP-DATE,
              LSPFILE.LSP_AVP_USERID = :QLSP-AVP-USERID,
              LSPFILE.LSP_AVP_APPROVE = :QLSP-AVP-APPROVE,
              LSPFILE.LSP_VP_COMMENTS = :QLSP-VP-COMMENTS,
              LSPFILE.LSP_VP_DATE = :QLSP-VP-DATE,
              LSPFILE.LSP_VP_USERID = :QLSP-VP-USERID,
              LSPFILE.LSP_VP_APPROVE = :QLSP-VP-APPROVE
             WHERE LSPFILE.LSP_BRNO = :QLSP-BRNO
               AND LSPFILE.LSP_ACCTNO = :QLSP-ACCTNO
               AND LSPFILE.LSP_SEQNO = :QLSP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LSP1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LSP1-FILE.

           MOVE LSP-PATH-OWNBR  TO LSP-BRNO.
           PERFORM DELETE-IT.
           MOVE LSP-PATH-SQL    TO E-FILE.
           MOVE LSP1-KEY        TO E-KEYX.
           PERFORM SET-LSP-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LSPFILE
             WHERE LSPFILE.LSP_BRNO = :QLSP-BRNO
               AND LSPFILE.LSP_ACCTNO = :QLSP-ACCTNO
               AND LSPFILE.LSP_SEQNO = :QLSP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LSP1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LSP1-FILE.

           PERFORM CLOSE-IT.

           IF ( LSP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSP1-CURSOR-STAT.

