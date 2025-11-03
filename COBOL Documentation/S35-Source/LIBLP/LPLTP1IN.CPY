      * COPYMEMBER: LIBLP/LPLTP1IN LOAN PAYMENT SCHEDULE TRAILER
      * REV:
      *
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
                   CAST(LTPFILE.LTP_LTOUCH_DATE
                                                  AS VARCHAR(10)),
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
            WHERE LTPFILE.LTP_BRNO  = :QLTP1-WBEG-BRANCH
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
           INITIALIZE QLTP-REC.

           MOVE LTP-BRNO   TO QLTP-BRNO.
           MOVE LTP-ACCTNO TO QLTP-ACCTNO.
           MOVE LTP-SEQNO  TO QLTP-SEQNO.

           EXEC SQL
            SELECT LTPFILE.LTP_BRNO,
                   LTPFILE.LTP_ACCTNO,
                   LTPFILE.LTP_SEQNO,
                   CAST(LTPFILE.LTP_LTOUCH_DATE
                                                  AS VARCHAR(10)),
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
       WRITE-LTP1-FILE.

           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           ACCEPT LTP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTP-PATH-SQL    TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.
           PERFORM SET-LTP-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LTPFILE
             (LTPFILE.LTP_BRNO,
              LTPFILE.LTP_ACCTNO,
              LTPFILE.LTP_SEQNO,
              LTPFILE.LTP_LTOUCH_DATE,
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
              LTPFILE.LTP_SCHLD_PAYMNT_41)
            VALUES
             (:QLTP-BRNO,
              :QLTP-ACCTNO,
              :QLTP-SEQNO,
              :QLTP-LTOUCH-DATE,
              :QLTP-SCHLD-TERM-1,
              :QLTP-SCHLD-PAYMNT-1,
              :QLTP-SCHLD-TERM-2,
              :QLTP-SCHLD-PAYMNT-2,
              :QLTP-SCHLD-TERM-3,
              :QLTP-SCHLD-PAYMNT-3,
              :QLTP-SCHLD-TERM-4,
              :QLTP-SCHLD-PAYMNT-4,
              :QLTP-SCHLD-TERM-5,
              :QLTP-SCHLD-PAYMNT-5,
              :QLTP-SCHLD-TERM-6,
              :QLTP-SCHLD-PAYMNT-6,
              :QLTP-SCHLD-TERM-7,
              :QLTP-SCHLD-PAYMNT-7,
              :QLTP-SCHLD-TERM-8,
              :QLTP-SCHLD-PAYMNT-8,
              :QLTP-SCHLD-TERM-9,
              :QLTP-SCHLD-PAYMNT-9,
              :QLTP-SCHLD-TERM-10,
              :QLTP-SCHLD-PAYMNT-10,
              :QLTP-SCHLD-TERM-11,
              :QLTP-SCHLD-PAYMNT-11,
              :QLTP-SCHLD-TERM-12,
              :QLTP-SCHLD-PAYMNT-12,
              :QLTP-SCHLD-TERM-13,
              :QLTP-SCHLD-PAYMNT-13,
              :QLTP-SCHLD-TERM-14,
              :QLTP-SCHLD-PAYMNT-14,
              :QLTP-SCHLD-TERM-15,
              :QLTP-SCHLD-PAYMNT-15,
              :QLTP-SCHLD-TERM-16,
              :QLTP-SCHLD-PAYMNT-16,
              :QLTP-SCHLD-TERM-17,
              :QLTP-SCHLD-PAYMNT-17,
              :QLTP-SCHLD-TERM-18,
              :QLTP-SCHLD-PAYMNT-18,
              :QLTP-SCHLD-TERM-19,
              :QLTP-SCHLD-PAYMNT-19,
              :QLTP-SCHLD-TERM-20,
              :QLTP-SCHLD-PAYMNT-20,
              :QLTP-SCHLD-TERM-21,
              :QLTP-SCHLD-PAYMNT-21,
              :QLTP-SCHLD-TERM-22,
              :QLTP-SCHLD-PAYMNT-22,
              :QLTP-SCHLD-TERM-23,
              :QLTP-SCHLD-PAYMNT-23,
              :QLTP-SCHLD-TERM-24,
              :QLTP-SCHLD-PAYMNT-24,
              :QLTP-SCHLD-TERM-25,
              :QLTP-SCHLD-PAYMNT-25,
              :QLTP-SCHLD-TERM-26,
              :QLTP-SCHLD-PAYMNT-26,
              :QLTP-SCHLD-TERM-27,
              :QLTP-SCHLD-PAYMNT-27,
              :QLTP-SCHLD-TERM-28,
              :QLTP-SCHLD-PAYMNT-28,
              :QLTP-SCHLD-TERM-29,
              :QLTP-SCHLD-PAYMNT-29,
              :QLTP-SCHLD-TERM-30,
              :QLTP-SCHLD-PAYMNT-30,
              :QLTP-SCHLD-TERM-31,
              :QLTP-SCHLD-PAYMNT-31,
              :QLTP-SCHLD-TERM-32,
              :QLTP-SCHLD-PAYMNT-32,
              :QLTP-SCHLD-TERM-33,
              :QLTP-SCHLD-PAYMNT-33,
              :QLTP-SCHLD-TERM-34,
              :QLTP-SCHLD-PAYMNT-34,
              :QLTP-SCHLD-TERM-35,
              :QLTP-SCHLD-PAYMNT-35,
              :QLTP-SCHLD-TERM-36,
              :QLTP-SCHLD-PAYMNT-36,
              :QLTP-SCHLD-TERM-37,
              :QLTP-SCHLD-PAYMNT-37,
              :QLTP-SCHLD-TERM-38,
              :QLTP-SCHLD-PAYMNT-38,
              :QLTP-SCHLD-TERM-39,
              :QLTP-SCHLD-PAYMNT-39,
              :QLTP-SCHLD-TERM-40,
              :QLTP-SCHLD-PAYMNT-40,
              :QLTP-SCHLD-TERM-41,
              :QLTP-SCHLD-PAYMNT-41)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTP1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LTP1-FILE.

           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           ACCEPT LTP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTP-PATH-SQL    TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.
           PERFORM SET-LTP-FIELDS.

           EXEC SQL
            UPDATE DBO.LTPFILE
             SET
              LTPFILE.LTP_BRNO = :QLTP-BRNO,
              LTPFILE.LTP_ACCTNO = :QLTP-ACCTNO,
              LTPFILE.LTP_SEQNO = :QLTP-SEQNO,
              LTPFILE.LTP_LTOUCH_DATE = :QLTP-LTOUCH-DATE,
              LTPFILE.LTP_SCHLD_TERM_1 = :QLTP-SCHLD-TERM-1,
              LTPFILE.LTP_SCHLD_PAYMNT_1 = :QLTP-SCHLD-PAYMNT-1,
              LTPFILE.LTP_SCHLD_TERM_2 = :QLTP-SCHLD-TERM-2,
              LTPFILE.LTP_SCHLD_PAYMNT_2 = :QLTP-SCHLD-PAYMNT-2,
              LTPFILE.LTP_SCHLD_TERM_3 = :QLTP-SCHLD-TERM-3,
              LTPFILE.LTP_SCHLD_PAYMNT_3 = :QLTP-SCHLD-PAYMNT-3,
              LTPFILE.LTP_SCHLD_TERM_4 = :QLTP-SCHLD-TERM-4,
              LTPFILE.LTP_SCHLD_PAYMNT_4 = :QLTP-SCHLD-PAYMNT-4,
              LTPFILE.LTP_SCHLD_TERM_5 = :QLTP-SCHLD-TERM-5,
              LTPFILE.LTP_SCHLD_PAYMNT_5 = :QLTP-SCHLD-PAYMNT-5,
              LTPFILE.LTP_SCHLD_TERM_6 = :QLTP-SCHLD-TERM-6,
              LTPFILE.LTP_SCHLD_PAYMNT_6 = :QLTP-SCHLD-PAYMNT-6,
              LTPFILE.LTP_SCHLD_TERM_7 = :QLTP-SCHLD-TERM-7,
              LTPFILE.LTP_SCHLD_PAYMNT_7 = :QLTP-SCHLD-PAYMNT-7,
              LTPFILE.LTP_SCHLD_TERM_8 = :QLTP-SCHLD-TERM-8,
              LTPFILE.LTP_SCHLD_PAYMNT_8 = :QLTP-SCHLD-PAYMNT-8,
              LTPFILE.LTP_SCHLD_TERM_9 = :QLTP-SCHLD-TERM-9,
              LTPFILE.LTP_SCHLD_PAYMNT_9 = :QLTP-SCHLD-PAYMNT-9,
              LTPFILE.LTP_SCHLD_TERM_10 = :QLTP-SCHLD-TERM-10,
              LTPFILE.LTP_SCHLD_PAYMNT_10 = :QLTP-SCHLD-PAYMNT-10,
              LTPFILE.LTP_SCHLD_TERM_11 = :QLTP-SCHLD-TERM-11,
              LTPFILE.LTP_SCHLD_PAYMNT_11 = :QLTP-SCHLD-PAYMNT-11,
              LTPFILE.LTP_SCHLD_TERM_12 = :QLTP-SCHLD-TERM-12,
              LTPFILE.LTP_SCHLD_PAYMNT_12 = :QLTP-SCHLD-PAYMNT-12,
              LTPFILE.LTP_SCHLD_TERM_13 = :QLTP-SCHLD-TERM-13,
              LTPFILE.LTP_SCHLD_PAYMNT_13 = :QLTP-SCHLD-PAYMNT-13,
              LTPFILE.LTP_SCHLD_TERM_14 = :QLTP-SCHLD-TERM-14,
              LTPFILE.LTP_SCHLD_PAYMNT_14 = :QLTP-SCHLD-PAYMNT-14,
              LTPFILE.LTP_SCHLD_TERM_15 = :QLTP-SCHLD-TERM-15,
              LTPFILE.LTP_SCHLD_PAYMNT_15 = :QLTP-SCHLD-PAYMNT-15,
              LTPFILE.LTP_SCHLD_TERM_16 = :QLTP-SCHLD-TERM-16,
              LTPFILE.LTP_SCHLD_PAYMNT_16 = :QLTP-SCHLD-PAYMNT-16,
              LTPFILE.LTP_SCHLD_TERM_17 = :QLTP-SCHLD-TERM-17,
              LTPFILE.LTP_SCHLD_PAYMNT_17 = :QLTP-SCHLD-PAYMNT-17,
              LTPFILE.LTP_SCHLD_TERM_18 = :QLTP-SCHLD-TERM-18,
              LTPFILE.LTP_SCHLD_PAYMNT_18 = :QLTP-SCHLD-PAYMNT-18,
              LTPFILE.LTP_SCHLD_TERM_19 = :QLTP-SCHLD-TERM-19,
              LTPFILE.LTP_SCHLD_PAYMNT_19 = :QLTP-SCHLD-PAYMNT-19,
              LTPFILE.LTP_SCHLD_TERM_20 = :QLTP-SCHLD-TERM-20,
              LTPFILE.LTP_SCHLD_PAYMNT_20 = :QLTP-SCHLD-PAYMNT-20,
              LTPFILE.LTP_SCHLD_TERM_21 = :QLTP-SCHLD-TERM-21,
              LTPFILE.LTP_SCHLD_PAYMNT_21 = :QLTP-SCHLD-PAYMNT-21,
              LTPFILE.LTP_SCHLD_TERM_22 = :QLTP-SCHLD-TERM-22,
              LTPFILE.LTP_SCHLD_PAYMNT_22 = :QLTP-SCHLD-PAYMNT-22,
              LTPFILE.LTP_SCHLD_TERM_23 = :QLTP-SCHLD-TERM-23,
              LTPFILE.LTP_SCHLD_PAYMNT_23 = :QLTP-SCHLD-PAYMNT-23,
              LTPFILE.LTP_SCHLD_TERM_24 = :QLTP-SCHLD-TERM-24,
              LTPFILE.LTP_SCHLD_PAYMNT_24 = :QLTP-SCHLD-PAYMNT-24,
              LTPFILE.LTP_SCHLD_TERM_25 = :QLTP-SCHLD-TERM-25,
              LTPFILE.LTP_SCHLD_PAYMNT_25 = :QLTP-SCHLD-PAYMNT-25,
              LTPFILE.LTP_SCHLD_TERM_26 = :QLTP-SCHLD-TERM-26,
              LTPFILE.LTP_SCHLD_PAYMNT_26 = :QLTP-SCHLD-PAYMNT-26,
              LTPFILE.LTP_SCHLD_TERM_27 = :QLTP-SCHLD-TERM-27,
              LTPFILE.LTP_SCHLD_PAYMNT_27 = :QLTP-SCHLD-PAYMNT-27,
              LTPFILE.LTP_SCHLD_TERM_28 = :QLTP-SCHLD-TERM-28,
              LTPFILE.LTP_SCHLD_PAYMNT_28 = :QLTP-SCHLD-PAYMNT-28,
              LTPFILE.LTP_SCHLD_TERM_29 = :QLTP-SCHLD-TERM-29,
              LTPFILE.LTP_SCHLD_PAYMNT_29 = :QLTP-SCHLD-PAYMNT-29,
              LTPFILE.LTP_SCHLD_TERM_30 = :QLTP-SCHLD-TERM-30,
              LTPFILE.LTP_SCHLD_PAYMNT_30 = :QLTP-SCHLD-PAYMNT-30,
              LTPFILE.LTP_SCHLD_TERM_31 = :QLTP-SCHLD-TERM-31,
              LTPFILE.LTP_SCHLD_PAYMNT_31 = :QLTP-SCHLD-PAYMNT-31,
              LTPFILE.LTP_SCHLD_TERM_32 = :QLTP-SCHLD-TERM-32,
              LTPFILE.LTP_SCHLD_PAYMNT_32 = :QLTP-SCHLD-PAYMNT-32,
              LTPFILE.LTP_SCHLD_TERM_33 = :QLTP-SCHLD-TERM-33,
              LTPFILE.LTP_SCHLD_PAYMNT_33 = :QLTP-SCHLD-PAYMNT-33,
              LTPFILE.LTP_SCHLD_TERM_34 = :QLTP-SCHLD-TERM-34,
              LTPFILE.LTP_SCHLD_PAYMNT_34 = :QLTP-SCHLD-PAYMNT-34,
              LTPFILE.LTP_SCHLD_TERM_35 = :QLTP-SCHLD-TERM-35,
              LTPFILE.LTP_SCHLD_PAYMNT_35 = :QLTP-SCHLD-PAYMNT-35,
              LTPFILE.LTP_SCHLD_TERM_36 = :QLTP-SCHLD-TERM-36,
              LTPFILE.LTP_SCHLD_PAYMNT_36 = :QLTP-SCHLD-PAYMNT-36,
              LTPFILE.LTP_SCHLD_TERM_37 = :QLTP-SCHLD-TERM-37,
              LTPFILE.LTP_SCHLD_PAYMNT_37 = :QLTP-SCHLD-PAYMNT-37,
              LTPFILE.LTP_SCHLD_TERM_38 = :QLTP-SCHLD-TERM-38,
              LTPFILE.LTP_SCHLD_PAYMNT_38 = :QLTP-SCHLD-PAYMNT-38,
              LTPFILE.LTP_SCHLD_TERM_39 = :QLTP-SCHLD-TERM-39,
              LTPFILE.LTP_SCHLD_PAYMNT_39 = :QLTP-SCHLD-PAYMNT-39,
              LTPFILE.LTP_SCHLD_TERM_40 = :QLTP-SCHLD-TERM-40,
              LTPFILE.LTP_SCHLD_PAYMNT_40 = :QLTP-SCHLD-PAYMNT-40,
              LTPFILE.LTP_SCHLD_TERM_41 = :QLTP-SCHLD-TERM-41,
              LTPFILE.LTP_SCHLD_PAYMNT_41 = :QLTP-SCHLD-PAYMNT-41
             WHERE LTPFILE.LTP_BRNO = :QLTP-BRNO
               AND LTPFILE.LTP_ACCTNO = :QLTP-ACCTNO
               AND LTPFILE.LTP_SEQNO = :QLTP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTP1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LTP1-FILE.

           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           PERFORM DELETE-IT.
           MOVE LTP-PATH-SQL    TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.
           PERFORM SET-LTP-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LTPFILE
             WHERE LTPFILE.LTP_BRNO = :QLTP-BRNO
               AND LTPFILE.LTP_ACCTNO = :QLTP-ACCTNO
               AND LTPFILE.LTP_SEQNO = :QLTP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LTP1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LTP1-FILE.

           PERFORM CLOSE-IT.

           IF ( LTP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTP1-CURSOR-STAT.

