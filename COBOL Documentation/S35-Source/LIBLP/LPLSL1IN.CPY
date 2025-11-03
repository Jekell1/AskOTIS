      * COPYMEMBER: LIBLP/LPLSL1IN
       LOAD-LSL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSL-ALL-DATA
              MOVE LSL-ALL-PATH             TO LSL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSL-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSL-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSL-ALL-DATA
              MOVE LSL-ALL-PATH             TO LSL-PATH.

      *-----------------------------------------------------------------
       OPEN-LSL1-FILE.
           PERFORM LOAD-LSL1-FILE.
           PERFORM OPEN-IT.
           MOVE LSL-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSL1-FILE.
           PERFORM START-IT.
           MOVE LSL-PATH-OWNBR  TO LSL-BRNO.
           MOVE LSL-PATH-SQL    TO E-FILE.
           MOVE LSL1-KEY        TO E-KEYX.

           IF ( LSL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSL1-FILE.

           IF ( QLSL1-WEND-BRNO = ZEROES    ) OR
              ( QLSL1-WEND-BRNO NOT NUMERIC )
              MOVE LSL-PATH-OWNBR        TO QLSL1-WBEG-BRNO
              MOVE LSL-PATH-OWNBR        TO QLSL1-WEND-BRNO
              MOVE LSL-ACCTNO            TO QLSL1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSL1-WEND-ACCTNO
              MOVE LSL-SEQNO             TO QLSL1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSL1-WEND-SEQNO.


           EXEC SQL
            DECLARE LSL1_CURSOR CURSOR FOR
            SELECT LSLFILE.LSL_BRNO,
                   LSLFILE.LSL_ACCTNO,
                   LSLFILE.LSL_SEQNO,
                   CAST(LSLFILE.LSL_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_DATE_SUIT_AUTH
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_DATE_SUIT_AUTH_BY,
                   LSLFILE.LSL_ATTORNEY_NET_ASSIGN,
                   LSLFILE.LSL_LOCAL_ATTORNEY,
                   LSLFILE.LSL_ATTORNEY_ADDRESS,
                   LSLFILE.LSL_ATTORNEY_PHONE,
                   CAST(LSLFILE.LSL_DATE_ASSIGNED
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_ATTORNEY_ACCTNO,
                   LSLFILE.LSL_PRIN_BAL_SENT,
                   CAST(LSLFILE.LSL_RECEIVED_ACKNOW_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_ATTORNEY_LETTER_SENT_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_HOLD_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_RESPONSE_RECEIVED_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_SUIT_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_SUIT_FILED_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_SERVICE_DATE
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_CUSTOMER_SERVED,
                   CAST(LSLFILE.LSL_ANSWER_FILED_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_COURT_DATE
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_JUDGMENT_TYPE,
                   CAST(LSLFILE.LSL_JUDGMENT_OBTAINED_DATE
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_JUDGMENT_COPY_FILED,
                   LSLFILE.LSL_JUDGMENT_AMT,
                   LSLFILE.LSL_JUDGMENT_PRIN,
                   LSLFILE.LSL_POST_JUDG_INT_RATE,
                   LSLFILE.LSL_JUDG_INT_AMT,
                   LSLFILE.LSL_ATTORNEY_FEES_ALLOWED,
                   LSLFILE.LSL_OTHER_FEES_ALLOWED,
                   LSLFILE.LSL_JUDG_INT_RATE,
                   LSLFILE.LSL_ASSET_USED_IN_SUIT,
                   CAST(LSLFILE.LSL_GARN_FILED_DATE
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_GARN_TERM_DAYS,
                   CAST(LSLFILE.LSL_INTERROGATORY_SENT_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_INTERROGATORY_ANSW_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_FIFA_SENT_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_FIFA_ANSWERED_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_DATE_TO_RECV_PAY
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_FIFA_RENEWED_DATE
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_PAYS_RECV_ATTORNEY,
                   CAST(LSLFILE.LSL_DATE_CLOSED
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_REASON_CLOSED
            FROM DBO.LSLFILE
            WHERE LSLFILE.LSL_BRNO  = :QLSL1-WBEG-BRNO
              AND LSLFILE.LSL_ACCTNO >= :QLSL1-WBEG-ACCTNO
              AND LSLFILE.LSL_ACCTNO <= :QLSL1-WEND-ACCTNO
              AND LSLFILE.LSL_SEQNO >= :QLSL1-WBEG-SEQNO
              AND LSLFILE.LSL_SEQNO <= :QLSL1-WEND-SEQNO
            ORDER BY LSLFILE.LSL_BRNO,
                     LSLFILE.LSL_ACCTNO,
                     LSLFILE.LSL_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSL1-WBEG-BRNO
                          QLSL1-WEND-BRNO
                          QLSL1-WBEG-ACCTNO
                          QLSL1-WEND-ACCTNO
                          QLSL1-WBEG-SEQNO
                          QLSL1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSL1-FILE.
           PERFORM READ-IT.
           MOVE LSL-PATH-OWNBR  TO LSL-BRNO. 
           MOVE LSL-PATH-SQL    TO E-FILE.
           MOVE LSL1-KEY        TO E-KEYX.
           INITIALIZE QLSL-REC.

           MOVE LSL-BRNO   TO QLSL-BRNO.
           MOVE LSL-ACCTNO TO QLSL-ACCTNO.
           MOVE LSL-SEQNO TO QLSL-SEQNO.

           EXEC SQL
            SELECT LSLFILE.LSL_BRNO,
                   LSLFILE.LSL_ACCTNO,
                   LSLFILE.LSL_SEQNO,
                   CAST(LSLFILE.LSL_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_DATE_SUIT_AUTH
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_DATE_SUIT_AUTH_BY,
                   LSLFILE.LSL_ATTORNEY_NET_ASSIGN,
                   LSLFILE.LSL_LOCAL_ATTORNEY,
                   LSLFILE.LSL_ATTORNEY_ADDRESS,
                   LSLFILE.LSL_ATTORNEY_PHONE,
                   CAST(LSLFILE.LSL_DATE_ASSIGNED
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_ATTORNEY_ACCTNO,
                   LSLFILE.LSL_PRIN_BAL_SENT,
                   CAST(LSLFILE.LSL_RECEIVED_ACKNOW_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_ATTORNEY_LETTER_SENT_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_HOLD_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_RESPONSE_RECEIVED_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_SUIT_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_SUIT_FILED_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_SERVICE_DATE
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_CUSTOMER_SERVED,
                   CAST(LSLFILE.LSL_ANSWER_FILED_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_COURT_DATE
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_JUDGMENT_TYPE,
                   CAST(LSLFILE.LSL_JUDGMENT_OBTAINED_DATE
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_JUDGMENT_COPY_FILED,
                   LSLFILE.LSL_JUDGMENT_AMT,
                   LSLFILE.LSL_JUDGMENT_PRIN,
                   LSLFILE.LSL_POST_JUDG_INT_RATE,
                   LSLFILE.LSL_JUDG_INT_AMT,
                   LSLFILE.LSL_ATTORNEY_FEES_ALLOWED,
                   LSLFILE.LSL_OTHER_FEES_ALLOWED,
                   LSLFILE.LSL_JUDG_INT_RATE,
                   LSLFILE.LSL_ASSET_USED_IN_SUIT,
                   CAST(LSLFILE.LSL_GARN_FILED_DATE
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_GARN_TERM_DAYS,
                   CAST(LSLFILE.LSL_INTERROGATORY_SENT_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_INTERROGATORY_ANSW_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_FIFA_SENT_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_FIFA_ANSWERED_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_DATE_TO_RECV_PAY
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSLFILE.LSL_FIFA_RENEWED_DATE
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_PAYS_RECV_ATTORNEY,
                   CAST(LSLFILE.LSL_DATE_CLOSED
                                                  AS VARCHAR(10)),
                   LSLFILE.LSL_REASON_CLOSED
            INTO :QLSL-REC
            FROM DBO.LSLFILE
            WHERE LSLFILE.LSL_BRNO = :QLSL-BRNO
              AND LSLFILE.LSL_ACCTNO = :QLSL-ACCTNO
              AND LSLFILE.LSL_SEQNO = :QLSL-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSL-FIELDS.

      *-----------------------------------------------------------------
       READ-LSL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSL-PATH-OWNBR  TO LSL-BRNO.
           MOVE LSL-PATH-SQL    TO E-FILE.
           MOVE LSL1-KEY        TO E-KEYX.
           INITIALIZE QLSL-REC.

           IF ( LSL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSL1_CURSOR INTO :QLSL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSL-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LSL1-FILE.

           MOVE LSL-PATH-OWNBR  TO LSL-BRNO.
           ACCEPT LSL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LSL-PATH-SQL    TO E-FILE.
           MOVE LSL1-KEY        TO E-KEYX.
           PERFORM SET-LSL-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LSLFILE
             (LSLFILE.LSL_BRNO,
              LSLFILE.LSL_ACCTNO,
              LSLFILE.LSL_SEQNO,
              LSLFILE.LSL_LTOUCH_DATE,
              LSLFILE.LSL_DATE_SUIT_AUTH,
              LSLFILE.LSL_DATE_SUIT_AUTH_BY,
              LSLFILE.LSL_ATTORNEY_NET_ASSIGN,
              LSLFILE.LSL_LOCAL_ATTORNEY,
              LSLFILE.LSL_ATTORNEY_ADDRESS,
              LSLFILE.LSL_ATTORNEY_PHONE,
              LSLFILE.LSL_DATE_ASSIGNED,
              LSLFILE.LSL_ATTORNEY_ACCTNO,
              LSLFILE.LSL_PRIN_BAL_SENT,
              LSLFILE.LSL_RECEIVED_ACKNOW_DATE,
              LSLFILE.LSL_ATTORNEY_LETTER_SENT_DATE,
              LSLFILE.LSL_HOLD_DATE,
              LSLFILE.LSL_RESPONSE_RECEIVED_DATE,
              LSLFILE.LSL_SUIT_DATE,
              LSLFILE.LSL_SUIT_FILED_DATE,
              LSLFILE.LSL_SERVICE_DATE,
              LSLFILE.LSL_CUSTOMER_SERVED,
              LSLFILE.LSL_ANSWER_FILED_DATE,
              LSLFILE.LSL_COURT_DATE,
              LSLFILE.LSL_JUDGMENT_TYPE,
              LSLFILE.LSL_JUDGMENT_OBTAINED_DATE,
              LSLFILE.LSL_JUDGMENT_COPY_FILED,
              LSLFILE.LSL_JUDGMENT_AMT,
              LSLFILE.LSL_JUDGMENT_PRIN,
              LSLFILE.LSL_POST_JUDG_INT_RATE,
              LSLFILE.LSL_JUDG_INT_AMT,
              LSLFILE.LSL_ATTORNEY_FEES_ALLOWED,
              LSLFILE.LSL_OTHER_FEES_ALLOWED,
              LSLFILE.LSL_JUDG_INT_RATE,
              LSLFILE.LSL_ASSET_USED_IN_SUIT,
              LSLFILE.LSL_GARN_FILED_DATE,
              LSLFILE.LSL_GARN_TERM_DAYS,
              LSLFILE.LSL_INTERROGATORY_SENT_DATE,
              LSLFILE.LSL_INTERROGATORY_ANSW_DATE,
              LSLFILE.LSL_FIFA_SENT_DATE,
              LSLFILE.LSL_FIFA_ANSWERED_DATE,
              LSLFILE.LSL_DATE_TO_RECV_PAY,
              LSLFILE.LSL_FOLLOWUP_DATE,
              LSLFILE.LSL_FIFA_RENEWED_DATE,
              LSLFILE.LSL_PAYS_RECV_ATTORNEY,
              LSLFILE.LSL_DATE_CLOSED,
              LSLFILE.LSL_REASON_CLOSED)
            VALUES
             (:QLSL-BRNO,
              :QLSL-ACCTNO,
              :QLSL-SEQNO,
              :QLSL-LTOUCH-DATE,
              :QLSL-DATE-SUIT-AUTH,
              :QLSL-DATE-SUIT-AUTH-BY,
              :QLSL-ATTORNEY-NET-ASSIGN,
              :QLSL-LOCAL-ATTORNEY,
              :QLSL-ATTORNEY-ADDRESS,
              :QLSL-ATTORNEY-PHONE,
              :QLSL-DATE-ASSIGNED,
              :QLSL-ATTORNEY-ACCTNO,
              :QLSL-PRIN-BAL-SENT,
              :QLSL-RECEIVED-ACKNOW-DATE,
              :QLSL-ATTORNEY-LETTER-SENT-DATE,
              :QLSL-HOLD-DATE,
              :QLSL-RESPONSE-RECEIVED-DATE,
              :QLSL-SUIT-DATE,
              :QLSL-SUIT-FILED-DATE,
              :QLSL-SERVICE-DATE,
              :QLSL-CUSTOMER-SERVED,
              :QLSL-ANSWER-FILED-DATE,
              :QLSL-COURT-DATE,
              :QLSL-JUDGMENT-TYPE,
              :QLSL-JUDGMENT-OBTAINED-DATE,
              :QLSL-JUDGMENT-COPY-FILED,
              :QLSL-JUDGMENT-AMT,
              :QLSL-JUDGMENT-PRIN,
              :QLSL-POST-JUDG-INT-RATE,
              :QLSL-JUDG-INT-AMT,
              :QLSL-ATTORNEY-FEES-ALLOWED,
              :QLSL-OTHER-FEES-ALLOWED,
              :QLSL-JUDG-INT-RATE,
              :QLSL-ASSET-USED-IN-SUIT,
              :QLSL-GARN-FILED-DATE,
              :QLSL-GARN-TERM-DAYS,
              :QLSL-INTERROGATORY-SENT-DATE,
              :QLSL-INTERROGATORY-ANSW-DATE,
              :QLSL-FIFA-SENT-DATE,
              :QLSL-FIFA-ANSWERED-DATE,
              :QLSL-DATE-TO-RECV-PAY,
              :QLSL-FOLLOWUP-DATE,
              :QLSL-FIFA-RENEWED-DATE,
              :QLSL-PAYS-RECV-ATTORNEY,
              :QLSL-DATE-CLOSED,
              :QLSL-REASON-CLOSED)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LSL1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LSL1-FILE.

           MOVE LSL-PATH-OWNBR  TO LSL-BRNO.
           ACCEPT LSL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LSL-PATH-SQL    TO E-FILE.
           MOVE LSL1-KEY        TO E-KEYX.
           PERFORM SET-LSL-FIELDS.

           EXEC SQL
            UPDATE DBO.LSLFILE
             SET
              LSLFILE.LSL_BRNO = :QLSL-BRNO,
              LSLFILE.LSL_ACCTNO = :QLSL-ACCTNO,
              LSLFILE.LSL_SEQNO = :QLSL-SEQNO,
              LSLFILE.LSL_LTOUCH_DATE =
                 :QLSL-LTOUCH-DATE,
              LSLFILE.LSL_DATE_SUIT_AUTH =
                 :QLSL-DATE-SUIT-AUTH,
              LSLFILE.LSL_DATE_SUIT_AUTH_BY =
                   :QLSL-DATE-SUIT-AUTH-BY,
              LSLFILE.LSL_ATTORNEY_NET_ASSIGN =
                   :QLSL-ATTORNEY-NET-ASSIGN,
              LSLFILE.LSL_LOCAL_ATTORNEY = :QLSL-LOCAL-ATTORNEY,
              LSLFILE.LSL_ATTORNEY_ADDRESS =
                   :QLSL-ATTORNEY-ADDRESS,
              LSLFILE.LSL_ATTORNEY_PHONE = :QLSL-ATTORNEY-PHONE,
              LSLFILE.LSL_DATE_ASSIGNED =
                 :QLSL-DATE-ASSIGNED,
              LSLFILE.LSL_ATTORNEY_ACCTNO = :QLSL-ATTORNEY-ACCTNO,
              LSLFILE.LSL_PRIN_BAL_SENT = :QLSL-PRIN-BAL-SENT,
              LSLFILE.LSL_RECEIVED_ACKNOW_DATE =
                 :QLSL-RECEIVED-ACKNOW-DATE,
              LSLFILE.LSL_ATTORNEY_LETTER_SENT_DATE =
                 :QLSL-ATTORNEY-LETTER-SENT-DATE,
              LSLFILE.LSL_HOLD_DATE =
                 :QLSL-HOLD-DATE,
              LSLFILE.LSL_RESPONSE_RECEIVED_DATE =
                 :QLSL-RESPONSE-RECEIVED-DATE,
              LSLFILE.LSL_SUIT_DATE =
                 :QLSL-SUIT-DATE,
              LSLFILE.LSL_SUIT_FILED_DATE =
                 :QLSL-SUIT-FILED-DATE,
              LSLFILE.LSL_SERVICE_DATE =
                 :QLSL-SERVICE-DATE,
              LSLFILE.LSL_CUSTOMER_SERVED = :QLSL-CUSTOMER-SERVED,
              LSLFILE.LSL_ANSWER_FILED_DATE =
                 :QLSL-ANSWER-FILED-DATE,
              LSLFILE.LSL_COURT_DATE =
                 :QLSL-COURT-DATE,
              LSLFILE.LSL_JUDGMENT_TYPE = :QLSL-JUDGMENT-TYPE,
              LSLFILE.LSL_JUDGMENT_OBTAINED_DATE =
                 :QLSL-JUDGMENT-OBTAINED-DATE,
              LSLFILE.LSL_JUDGMENT_COPY_FILED =
                   :QLSL-JUDGMENT-COPY-FILED,
              LSLFILE.LSL_JUDGMENT_AMT = :QLSL-JUDGMENT-AMT,
              LSLFILE.LSL_JUDGMENT_PRIN = :QLSL-JUDGMENT-PRIN,
              LSLFILE.LSL_POST_JUDG_INT_RATE =
                   :QLSL-POST-JUDG-INT-RATE,
              LSLFILE.LSL_JUDG_INT_AMT = :QLSL-JUDG-INT-AMT,
              LSLFILE.LSL_ATTORNEY_FEES_ALLOWED =
                   :QLSL-ATTORNEY-FEES-ALLOWED,
              LSLFILE.LSL_OTHER_FEES_ALLOWED =
                   :QLSL-OTHER-FEES-ALLOWED,
              LSLFILE.LSL_JUDG_INT_RATE = :QLSL-JUDG-INT-RATE,
              LSLFILE.LSL_ASSET_USED_IN_SUIT =
                   :QLSL-ASSET-USED-IN-SUIT,
              LSLFILE.LSL_GARN_FILED_DATE =
                 :QLSL-GARN-FILED-DATE,
              LSLFILE.LSL_GARN_TERM_DAYS = :QLSL-GARN-TERM-DAYS,
              LSLFILE.LSL_INTERROGATORY_SENT_DATE =
                 :QLSL-INTERROGATORY-SENT-DATE,
              LSLFILE.LSL_INTERROGATORY_ANSW_DATE =
                 :QLSL-INTERROGATORY-ANSW-DATE,
              LSLFILE.LSL_FIFA_SENT_DATE =
                 :QLSL-FIFA-SENT-DATE,
              LSLFILE.LSL_FIFA_ANSWERED_DATE =
                 :QLSL-FIFA-ANSWERED-DATE,
              LSLFILE.LSL_DATE_TO_RECV_PAY =
                 :QLSL-DATE-TO-RECV-PAY,
              LSLFILE.LSL_FOLLOWUP_DATE =
                 :QLSL-FOLLOWUP-DATE,
              LSLFILE.LSL_FIFA_RENEWED_DATE =
                 :QLSL-FIFA-RENEWED-DATE,
              LSLFILE.LSL_PAYS_RECV_ATTORNEY =
                   :QLSL-PAYS-RECV-ATTORNEY,
              LSLFILE.LSL_DATE_CLOSED = :QLSL-DATE-CLOSED,
              LSLFILE.LSL_REASON_CLOSED = :QLSL-REASON-CLOSED
             WHERE LSLFILE.LSL_BRNO = :QLSL-BRNO
               AND LSLFILE.LSL_ACCTNO = :QLSL-ACCTNO
               AND LSLFILE.LSL_SEQNO = :QLSL-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LSL1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LSL1-FILE.

           MOVE LSL-PATH-OWNBR  TO LSL-BRNO.
           PERFORM DELETE-IT.
           MOVE LSL-PATH-SQL    TO E-FILE.
           MOVE LSL1-KEY        TO E-KEYX.
           PERFORM SET-LSL-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LSLFILE
             WHERE LSLFILE.LSL_BRNO = :QLSL-BRNO
               AND LSLFILE.LSL_ACCTNO = :QLSL-ACCTNO
               AND LSLFILE.LSL_SEQNO = :QLSL-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LSL1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LSL1-FILE.

           PERFORM CLOSE-IT.

           IF ( LSL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSL1-CURSOR-STAT.

