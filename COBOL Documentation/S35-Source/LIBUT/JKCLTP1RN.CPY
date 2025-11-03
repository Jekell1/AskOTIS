      * COPYMEMBER: LIBLP/LPLTP1RN
      *=================================================================
       GET-LT-P-REC.
           MOVE 0 TO IO-FG.
           IF NOT (LN-ACCTNO = LTP-ACCTNO)
              MOVE LN-OWNBR  TO LTP-BRNO
              MOVE LN-ACCTNO TO LTP-ACCTNO
              MOVE 1         TO LTP-SEQNO
              PERFORM OPEN-LTP1-FILE
              PERFORM READ-LTP1-FILE
              PERFORM CLOSE-LTP1-FILE.

      *-----------------------------------------------------------------
       LOAD-LTP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTP-ALL-DATA
              MOVE LTP-ALL-PATH             TO LTP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTP-ALL-DATA
              MOVE LTP-ALL-PATH             TO LTP-PATH.

      *-----------------------------------------------------------------
       OPEN-LTP1-FILE.
           PERFORM LOAD-LTP1-FILE.
           PERFORM OPEN-IT.
           MOVE LTP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTP1-FILE.
           PERFORM START-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           MOVE LTP-PATH-SQL    TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.

           IF ( LTP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTP1-FILE.

           IF ( QLTP1-WEND-BRANCH = ZEROES    ) OR
              ( QLTP1-WEND-BRANCH NOT NUMERIC )
              MOVE LTP-PATH-OWNBR        TO QLTP1-WBEG-BRANCH
              MOVE LTP-PATH-OWNBR        TO QLTP1-WEND-BRANCH
              MOVE LTP-ACCTNO            TO QLTP1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTP1-WEND-ACCTNO
              MOVE LTP-SEQNO             TO QLTP1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTP1-WEND-SEQNO.

           EXEC SQL
            DECLARE LTP1_CURSOR CURSOR FOR
            SELECT LTPFILE.LTP_BRNO,
                   LTPFILE.LTP_ACCTNO,
                   LTPFILE.LTP_SEQNO,
                   ISNULL(CAST(LTPFILE.LTP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LTPFILE.LTP_SCHLD_TERM_1,
                   LTPFILE.LTP_SCHLD_PAYMNT_1,
                   LTPFILE.LTP_SCHLD_TERM_2,
                   LTPFILE.LTP_SCHLD_PAYMNT_2,
                   LTPFILE.LTP_SCHLD_TERM_3,
                   LTPFILE.LTP_SCHLD_PAYMNT_3,
                   LTPFILE.LTP_SCHLD_TERM_4,
                   LTPFILE.LTP_SCHLD_PAYMNT_4,
                   LTPFILE.LTP_SCHLD_TERM_5,
                   LTPFILE.LTP_SCHLD_PAYMNT_5,
                   LTPFILE.LTP_SCHLD_TERM_6,
                   LTPFILE.LTP_SCHLD_PAYMNT_6,
                   LTPFILE.LTP_SCHLD_TERM_7,
                   LTPFILE.LTP_SCHLD_PAYMNT_7,
                   LTPFILE.LTP_SCHLD_TERM_8,
                   LTPFILE.LTP_SCHLD_PAYMNT_8,
                   LTPFILE.LTP_SCHLD_TERM_9,
                   LTPFILE.LTP_SCHLD_PAYMNT_9,
                   LTPFILE.LTP_SCHLD_TERM_10,
                   LTPFILE.LTP_SCHLD_PAYMNT_10,
                   LTPFILE.LTP_SCHLD_TERM_11,
                   LTPFILE.LTP_SCHLD_PAYMNT_11,
                   LTPFILE.LTP_SCHLD_TERM_12,
                   LTPFILE.LTP_SCHLD_PAYMNT_12,
                   LTPFILE.LTP_SCHLD_TERM_13,
                   LTPFILE.LTP_SCHLD_PAYMNT_13,
                   LTPFILE.LTP_SCHLD_TERM_14,
                   LTPFILE.LTP_SCHLD_PAYMNT_14,
                   LTPFILE.LTP_SCHLD_TERM_15,
                   LTPFILE.LTP_SCHLD_PAYMNT_15,
                   LTPFILE.LTP_SCHLD_TERM_16,
                   LTPFILE.LTP_SCHLD_PAYMNT_16,
                   LTPFILE.LTP_SCHLD_TERM_17,
                   LTPFILE.LTP_SCHLD_PAYMNT_17,
                   LTPFILE.LTP_SCHLD_TERM_18,
                   LTPFILE.LTP_SCHLD_PAYMNT_18,
                   LTPFILE.LTP_SCHLD_TERM_19,
                   LTPFILE.LTP_SCHLD_PAYMNT_19,
                   LTPFILE.LTP_SCHLD_TERM_20,
                   LTPFILE.LTP_SCHLD_PAYMNT_20,
                   LTPFILE.LTP_SCHLD_TERM_21,
                   LTPFILE.LTP_SCHLD_PAYMNT_21,
                   LTPFILE.LTP_SCHLD_TERM_22,
                   LTPFILE.LTP_SCHLD_PAYMNT_22,
                   LTPFILE.LTP_SCHLD_TERM_23,
                   LTPFILE.LTP_SCHLD_PAYMNT_23,
                   LTPFILE.LTP_SCHLD_TERM_24,
                   LTPFILE.LTP_SCHLD_PAYMNT_24,
                   LTPFILE.LTP_SCHLD_TERM_25,
                   LTPFILE.LTP_SCHLD_PAYMNT_25,
                   LTPFILE.LTP_SCHLD_TERM_26,
                   LTPFILE.LTP_SCHLD_PAYMNT_26,
                   LTPFILE.LTP_SCHLD_TERM_27,
                   LTPFILE.LTP_SCHLD_PAYMNT_27,
                   LTPFILE.LTP_SCHLD_TERM_28,
                   LTPFILE.LTP_SCHLD_PAYMNT_28,
                   LTPFILE.LTP_SCHLD_TERM_29,
                   LTPFILE.LTP_SCHLD_PAYMNT_29,
                   LTPFILE.LTP_SCHLD_TERM_30,
                   LTPFILE.LTP_SCHLD_PAYMNT_30,
                   LTPFILE.LTP_SCHLD_TERM_31,
                   LTPFILE.LTP_SCHLD_PAYMNT_31,
                   LTPFILE.LTP_SCHLD_TERM_32,
                   LTPFILE.LTP_SCHLD_PAYMNT_32,
                   LTPFILE.LTP_SCHLD_TERM_33,
                   LTPFILE.LTP_SCHLD_PAYMNT_33,
                   LTPFILE.LTP_SCHLD_TERM_34,
                   LTPFILE.LTP_SCHLD_PAYMNT_34,
                   LTPFILE.LTP_SCHLD_TERM_35,
                   LTPFILE.LTP_SCHLD_PAYMNT_35,
                   LTPFILE.LTP_SCHLD_TERM_36,
                   LTPFILE.LTP_SCHLD_PAYMNT_36,
                   LTPFILE.LTP_SCHLD_TERM_37,
                   LTPFILE.LTP_SCHLD_PAYMNT_37,
                   LTPFILE.LTP_SCHLD_TERM_38,
                   LTPFILE.LTP_SCHLD_PAYMNT_38,
                   LTPFILE.LTP_SCHLD_TERM_39,
                   LTPFILE.LTP_SCHLD_PAYMNT_39,
                   LTPFILE.LTP_SCHLD_TERM_40,
                   LTPFILE.LTP_SCHLD_PAYMNT_40,
                   LTPFILE.LTP_SCHLD_TERM_41,
                   LTPFILE.LTP_SCHLD_PAYMNT_41
            FROM DBO.LTPFILE
            WHERE LTPFILE.LTP_BRNO >= :QLTP1-WBEG-BRANCH
              AND LTPFILE.LTP_BRNO <= :QLTP1-WEND-BRANCH
              AND LTPFILE.LTP_ACCTNO >= :QLTP1-WBEG-ACCTNO
              AND LTPFILE.LTP_ACCTNO <= :QLTP1-WEND-ACCTNO
              AND LTPFILE.LTP_SEQNO >= :QLTP1-WEND-SEQNO
              AND LTPFILE.LTP_SEQNO <= :QLTP1-WEND-SEQNO
            ORDER BY LTPFILE.LTP_BRNO,
                     LTPFILE.LTP_ACCTNO,
                     LTPFILE.LTP_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTP1-WBEG-BRANCH 
                          QLTP1-WEND-BRANCH
                          QLTP1-WBEG-ACCTNO
                          QLTP1-WEND-ACCTNO
                          QLTP1-WBEG-SEQNO
                          QLTP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTP1-FILE.
           PERFORM READ-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO. 
           MOVE LTP-PATH-SQL    TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.

           MOVE LTP-BRNO   TO QLTP-BRNO.
           MOVE LTP-ACCTNO TO QLTP-ACCTNO.
           MOVE LTP-SEQNO  TO QLTP-SEQNO.

           EXEC SQL
            SELECT LTPFILE.LTP_BRNO,
                   LTPFILE.LTP_ACCTNO,
                   LTPFILE.LTP_SEQNO,
                   ISNULL(CAST(LTPFILE.LTP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LTPFILE.LTP_SCHLD_TERM_1,
                   LTPFILE.LTP_SCHLD_PAYMNT_1,
                   LTPFILE.LTP_SCHLD_TERM_2,
                   LTPFILE.LTP_SCHLD_PAYMNT_2,
                   LTPFILE.LTP_SCHLD_TERM_3,
                   LTPFILE.LTP_SCHLD_PAYMNT_3,
                   LTPFILE.LTP_SCHLD_TERM_4,
                   LTPFILE.LTP_SCHLD_PAYMNT_4,
                   LTPFILE.LTP_SCHLD_TERM_5,
                   LTPFILE.LTP_SCHLD_PAYMNT_5,
                   LTPFILE.LTP_SCHLD_TERM_6,
                   LTPFILE.LTP_SCHLD_PAYMNT_6,
                   LTPFILE.LTP_SCHLD_TERM_7,
                   LTPFILE.LTP_SCHLD_PAYMNT_7,
                   LTPFILE.LTP_SCHLD_TERM_8,
                   LTPFILE.LTP_SCHLD_PAYMNT_8,
                   LTPFILE.LTP_SCHLD_TERM_9,
                   LTPFILE.LTP_SCHLD_PAYMNT_9,
                   LTPFILE.LTP_SCHLD_TERM_10,
                   LTPFILE.LTP_SCHLD_PAYMNT_10,
                   LTPFILE.LTP_SCHLD_TERM_11,
                   LTPFILE.LTP_SCHLD_PAYMNT_11,
                   LTPFILE.LTP_SCHLD_TERM_12,
                   LTPFILE.LTP_SCHLD_PAYMNT_12,
                   LTPFILE.LTP_SCHLD_TERM_13,
                   LTPFILE.LTP_SCHLD_PAYMNT_13,
                   LTPFILE.LTP_SCHLD_TERM_14,
                   LTPFILE.LTP_SCHLD_PAYMNT_14,
                   LTPFILE.LTP_SCHLD_TERM_15,
                   LTPFILE.LTP_SCHLD_PAYMNT_15,
                   LTPFILE.LTP_SCHLD_TERM_16,
                   LTPFILE.LTP_SCHLD_PAYMNT_16,
                   LTPFILE.LTP_SCHLD_TERM_17,
                   LTPFILE.LTP_SCHLD_PAYMNT_17,
                   LTPFILE.LTP_SCHLD_TERM_18,
                   LTPFILE.LTP_SCHLD_PAYMNT_18,
                   LTPFILE.LTP_SCHLD_TERM_19,
                   LTPFILE.LTP_SCHLD_PAYMNT_19,
                   LTPFILE.LTP_SCHLD_TERM_20,
                   LTPFILE.LTP_SCHLD_PAYMNT_20,
                   LTPFILE.LTP_SCHLD_TERM_21,
                   LTPFILE.LTP_SCHLD_PAYMNT_21,
                   LTPFILE.LTP_SCHLD_TERM_22,
                   LTPFILE.LTP_SCHLD_PAYMNT_22,
                   LTPFILE.LTP_SCHLD_TERM_23,
                   LTPFILE.LTP_SCHLD_PAYMNT_23,
                   LTPFILE.LTP_SCHLD_TERM_24,
                   LTPFILE.LTP_SCHLD_PAYMNT_24,
                   LTPFILE.LTP_SCHLD_TERM_25,
                   LTPFILE.LTP_SCHLD_PAYMNT_25,
                   LTPFILE.LTP_SCHLD_TERM_26,
                   LTPFILE.LTP_SCHLD_PAYMNT_26,
                   LTPFILE.LTP_SCHLD_TERM_27,
                   LTPFILE.LTP_SCHLD_PAYMNT_27,
                   LTPFILE.LTP_SCHLD_TERM_28,
                   LTPFILE.LTP_SCHLD_PAYMNT_28,
                   LTPFILE.LTP_SCHLD_TERM_29,
                   LTPFILE.LTP_SCHLD_PAYMNT_29,
                   LTPFILE.LTP_SCHLD_TERM_30,
                   LTPFILE.LTP_SCHLD_PAYMNT_30,
                   LTPFILE.LTP_SCHLD_TERM_31,
                   LTPFILE.LTP_SCHLD_PAYMNT_31,
                   LTPFILE.LTP_SCHLD_TERM_32,
                   LTPFILE.LTP_SCHLD_PAYMNT_32,
                   LTPFILE.LTP_SCHLD_TERM_33,
                   LTPFILE.LTP_SCHLD_PAYMNT_33,
                   LTPFILE.LTP_SCHLD_TERM_34,
                   LTPFILE.LTP_SCHLD_PAYMNT_34,
                   LTPFILE.LTP_SCHLD_TERM_35,
                   LTPFILE.LTP_SCHLD_PAYMNT_35,
                   LTPFILE.LTP_SCHLD_TERM_36,
                   LTPFILE.LTP_SCHLD_PAYMNT_36,
                   LTPFILE.LTP_SCHLD_TERM_37,
                   LTPFILE.LTP_SCHLD_PAYMNT_37,
                   LTPFILE.LTP_SCHLD_TERM_38,
                   LTPFILE.LTP_SCHLD_PAYMNT_38,
                   LTPFILE.LTP_SCHLD_TERM_39,
                   LTPFILE.LTP_SCHLD_PAYMNT_39,
                   LTPFILE.LTP_SCHLD_TERM_40,
                   LTPFILE.LTP_SCHLD_PAYMNT_40,
                   LTPFILE.LTP_SCHLD_TERM_41,
                   LTPFILE.LTP_SCHLD_PAYMNT_41
            INTO :QLTP-REC
            FROM DBO.LTPFILE
            WHERE LTPFILE.LTP_BRNO = :QLTP-BRNO
              AND LTPFILE.LTP_ACCTNO = :QLTP-ACCTNO
              AND LTPFILE.LTP_SEQNO = :QLTP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTP-FIELDS.

      *-----------------------------------------------------------------
       READ-LTP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           MOVE LTP-PATH-SQL    TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.
           INITIALIZE QLTP-REC.

           IF ( LTP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTP1_CURSOR INTO :QLTP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTP1-FILE.

           IF ( LTP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTP1-CURSOR-STAT.

