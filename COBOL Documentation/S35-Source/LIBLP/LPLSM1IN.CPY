      * COPYMEMBER: LIBLP/LPLSM1IN
      * 20220923 BAH ADDED LSM-MISPAY-90-DATE #1570
      * 20240116 BAH ADDED LSM-SECOND-BEN INFO #1637
       LOAD-LSM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSM-ALL-DATA
              MOVE LSM-ALL-PATH             TO LSM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSM-ALL-DATA
              MOVE LSM-ALL-PATH             TO LSM-PATH.

      *-----------------------------------------------------------------
       OPEN-LSM1-FILE.
           PERFORM LOAD-LSM1-FILE.
           PERFORM OPEN-IT.
           MOVE LSM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSM1-FILE.
           PERFORM START-IT.
           MOVE LSM-PATH-OWNBR  TO LSM-BRNO.
           MOVE LSM-PATH-SQL    TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.

           IF ( LSM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSM1-FILE.

           IF ( QLSM1-WEND-BRNO = ZEROES    ) OR
              ( QLSM1-WEND-BRNO NOT NUMERIC )
              MOVE LSM-PATH-OWNBR        TO QLSM1-WBEG-BRNO
              MOVE LSM-PATH-OWNBR        TO QLSM1-WEND-BRNO
              MOVE LSM-ACCTNO            TO QLSM1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSM1-WEND-ACCTNO
              MOVE LSM-SEQNO             TO QLSM1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSM1-WEND-SEQNO.

           EXEC SQL
            DECLARE LSM1_CURSOR CURSOR FOR
            SELECT LSMFILE.LSM_BRNO,
                   LSMFILE.LSM_ACCTNO,
                   LSMFILE.LSM_SEQNO,
                   CAST(LSMFILE.LSM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LSMFILE.LSM_MISC_FIELD_1,
                   LSMFILE.LSM_MISC_FIELD_2,
                   LSMFILE.LSM_MISC_FIELD_3,
                   LSMFILE.LSM_MISC_FIELD_4,
                   LSMFILE.LSM_MISC_FIELD_5,
                   LSMFILE.LSM_MISC_FIELD_6,
                   LSMFILE.LSM_MISC_FIELD_7,
                   LSMFILE.LSM_MISC_FIELD_8,
                   LSMFILE.LSM_MISC_FIELD_9,
                   LSMFILE.LSM_MISC_FIELD_10,
                   LSMFILE.LSM_MISC_FIELD_11,
                   LSMFILE.LSM_MISC_FIELD_12,
                   LSMFILE.LSM_MISC_FIELD_13,
                   LSMFILE.LSM_MISC_FIELD_14,
                   LSMFILE.LSM_MISC_FIELD_15,
                   LSMFILE.LSM_MISC_FIELD_16,
                   LSMFILE.LSM_MISC_FIELD_17,
                   LSMFILE.LSM_MISC_FIELD_18,
                   LSMFILE.LSM_MISC_FIELD_19,
                   LSMFILE.LSM_MISC_FIELD_20,
                   LSMFILE.LSM_MISC_FIELD_21,
                   LSMFILE.LSM_MISC_FIELD_22,
                   LSMFILE.LSM_MISC_FIELD_23,
                   LSMFILE.LSM_MISC_FIELD_24,
                   LSMFILE.LSM_MISC_FIELD_25,
                   LSMFILE.LSM_MISC_FIELD_26,
                   LSMFILE.LSM_MISC_FIELD_27,
                   LSMFILE.LSM_MISC_FIELD_28,
                   LSMFILE.LSM_MISC_FIELD_29,
                   LSMFILE.LSM_MISC_FIELD_30,
                   LSMFILE.LSM_MISC_FIELD_31,
                   LSMFILE.LSM_MISC_FIELD_32,
                   LSMFILE.LSM_TRANS_FROM_BRNO,
                   LSMFILE.LSM_TRANS_FROM_ACCTNO,
                   CAST(LSMFILE.LSM_TRANS_FROM_DATE
                                       AS VARCHAR(10)),
                   CAST(LSMFILE.LSM_MISPAY_90_DATE
                                       AS VARCHAR(10)),
                   LSMFILE.LSM_SECOND_BEN_NAME,
                   LSMFILE.LSM_SECOND_BEN_ADR,
                   LSMFILE.LSM_SECOND_BEN_CITY,
                   LSMFILE.LSM_SECOND_BEN_ST,
                   LSMFILE.LSM_SECOND_BEN_ZIP,
                   LSMFILE.LSM_SECOND_BEN_PHONE,
                   LSMFILE.LSM_SECOND_BEN_RELATION
            FROM DBO.LSMFILE
            WHERE LSMFILE.LSM_BRNO  = :QLSM1-WBEG-BRNO
              AND LSMFILE.LSM_ACCTNO >= :QLSM1-WBEG-ACCTNO
              AND LSMFILE.LSM_ACCTNO <= :QLSM1-WEND-ACCTNO
              AND LSMFILE.LSM_SEQNO >= :QLSM1-WBEG-SEQNO
              AND LSMFILE.LSM_SEQNO <= :QLSM1-WEND-SEQNO
            ORDER BY LSMFILE.LSM_BRNO,
                     LSMFILE.LSM_ACCTNO,
                     LSMFILE.LSM_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSM1-WBEG-BRNO
                          QLSM1-WEND-BRNO
                          QLSM1-WBEG-ACCTNO
                          QLSM1-WEND-ACCTNO
                          QLSM1-WBEG-SEQNO
                          QLSM1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSM1-FILE.
           PERFORM READ-IT.
           MOVE LSM-PATH-OWNBR  TO LSM-BRNO. 
           MOVE LSM-PATH-SQL    TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.
           INITIALIZE QLSM-REC.

           MOVE LSM-BRNO   TO QLSM-BRNO.
           MOVE LSM-ACCTNO TO QLSM-ACCTNO.
           MOVE LSM-SEQNO TO QLSM-SEQNO.

           EXEC SQL
            SELECT LSMFILE.LSM_BRNO,
                   LSMFILE.LSM_ACCTNO,
                   LSMFILE.LSM_SEQNO,
                   CAST(LSMFILE.LSM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LSMFILE.LSM_MISC_FIELD_1,
                   LSMFILE.LSM_MISC_FIELD_2,
                   LSMFILE.LSM_MISC_FIELD_3,
                   LSMFILE.LSM_MISC_FIELD_4,
                   LSMFILE.LSM_MISC_FIELD_5,
                   LSMFILE.LSM_MISC_FIELD_6,
                   LSMFILE.LSM_MISC_FIELD_7,
                   LSMFILE.LSM_MISC_FIELD_8,
                   LSMFILE.LSM_MISC_FIELD_9,
                   LSMFILE.LSM_MISC_FIELD_10,
                   LSMFILE.LSM_MISC_FIELD_11,
                   LSMFILE.LSM_MISC_FIELD_12,
                   LSMFILE.LSM_MISC_FIELD_13,
                   LSMFILE.LSM_MISC_FIELD_14,
                   LSMFILE.LSM_MISC_FIELD_15,
                   LSMFILE.LSM_MISC_FIELD_16,
                   LSMFILE.LSM_MISC_FIELD_17,
                   LSMFILE.LSM_MISC_FIELD_18,
                   LSMFILE.LSM_MISC_FIELD_19,
                   LSMFILE.LSM_MISC_FIELD_20,
                   LSMFILE.LSM_MISC_FIELD_21,
                   LSMFILE.LSM_MISC_FIELD_22,
                   LSMFILE.LSM_MISC_FIELD_23,
                   LSMFILE.LSM_MISC_FIELD_24,
                   LSMFILE.LSM_MISC_FIELD_25,
                   LSMFILE.LSM_MISC_FIELD_26,
                   LSMFILE.LSM_MISC_FIELD_27,
                   LSMFILE.LSM_MISC_FIELD_28,
                   LSMFILE.LSM_MISC_FIELD_29,
                   LSMFILE.LSM_MISC_FIELD_30,
                   LSMFILE.LSM_MISC_FIELD_31,
                   LSMFILE.LSM_MISC_FIELD_32,
                   LSMFILE.LSM_TRANS_FROM_BRNO,
                   LSMFILE.LSM_TRANS_FROM_ACCTNO,
                   CAST(LSMFILE.LSM_TRANS_FROM_DATE
                                       AS VARCHAR(10)),
                   CAST(LSMFILE.LSM_MISPAY_90_DATE
                                       AS VARCHAR(10)),
                   LSMFILE.LSM_SECOND_BEN_NAME,
                   LSMFILE.LSM_SECOND_BEN_ADR,
                   LSMFILE.LSM_SECOND_BEN_CITY,
                   LSMFILE.LSM_SECOND_BEN_ST,
                   LSMFILE.LSM_SECOND_BEN_ZIP,
                   LSMFILE.LSM_SECOND_BEN_PHONE,
                   LSMFILE.LSM_SECOND_BEN_RELATION
            INTO :QLSM-REC
            FROM DBO.LSMFILE
            WHERE LSMFILE.LSM_BRNO = :QLSM-BRNO
              AND LSMFILE.LSM_ACCTNO = :QLSM-ACCTNO
              AND LSMFILE.LSM_SEQNO = :QLSM-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSM-FIELDS.

      *-----------------------------------------------------------------
       READ-LSM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSM-PATH-OWNBR  TO LSM-BRNO.
           MOVE LSM-PATH-SQL    TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.
           INITIALIZE QLSM-REC.

           IF ( LSM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSM1_CURSOR INTO :QLSM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSM-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LSM1-FILE.

           MOVE LSM-PATH-OWNBR  TO LSM-BRNO.
           ACCEPT LSM-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LSM-PATH-SQL    TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.
           PERFORM SET-LSM-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LSMFILE
             (LSMFILE.LSM_BRNO,
              LSMFILE.LSM_ACCTNO,
              LSMFILE.LSM_SEQNO,
              LSMFILE.LSM_LTOUCH_DATE,
              LSMFILE.LSM_MISC_FIELD_1,
              LSMFILE.LSM_MISC_FIELD_2,
              LSMFILE.LSM_MISC_FIELD_3,
              LSMFILE.LSM_MISC_FIELD_4,
              LSMFILE.LSM_MISC_FIELD_5,
              LSMFILE.LSM_MISC_FIELD_6,
              LSMFILE.LSM_MISC_FIELD_7,
              LSMFILE.LSM_MISC_FIELD_8,
              LSMFILE.LSM_MISC_FIELD_9,
              LSMFILE.LSM_MISC_FIELD_10,
              LSMFILE.LSM_MISC_FIELD_11,
              LSMFILE.LSM_MISC_FIELD_12,
              LSMFILE.LSM_MISC_FIELD_13,
              LSMFILE.LSM_MISC_FIELD_14,
              LSMFILE.LSM_MISC_FIELD_15,
              LSMFILE.LSM_MISC_FIELD_16,
              LSMFILE.LSM_MISC_FIELD_17,
              LSMFILE.LSM_MISC_FIELD_18,
              LSMFILE.LSM_MISC_FIELD_19,
              LSMFILE.LSM_MISC_FIELD_20,
              LSMFILE.LSM_MISC_FIELD_21,
              LSMFILE.LSM_MISC_FIELD_22,
              LSMFILE.LSM_MISC_FIELD_23,
              LSMFILE.LSM_MISC_FIELD_24,
              LSMFILE.LSM_MISC_FIELD_25,
              LSMFILE.LSM_MISC_FIELD_26,
              LSMFILE.LSM_MISC_FIELD_27,
              LSMFILE.LSM_MISC_FIELD_28,
              LSMFILE.LSM_MISC_FIELD_29,
              LSMFILE.LSM_MISC_FIELD_30,
              LSMFILE.LSM_MISC_FIELD_31,
              LSMFILE.LSM_MISC_FIELD_32,
              LSMFILE.LSM_TRANS_FROM_BRNO,
              LSMFILE.LSM_TRANS_FROM_ACCTNO,
              LSMFILE.LSM_TRANS_FROM_DATE,
              LSMFILE.LSM_MISPAY_90_DATE,
              LSMFILE.LSM_SECOND_BEN_NAME,
              LSMFILE.LSM_SECOND_BEN_ADR,
              LSMFILE.LSM_SECOND_BEN_CITY,
              LSMFILE.LSM_SECOND_BEN_ST,
              LSMFILE.LSM_SECOND_BEN_ZIP,
              LSMFILE.LSM_SECOND_BEN_PHONE,
              LSMFILE.LSM_SECOND_BEN_RELATION)
            VALUES
             (:QLSM-BRNO,
              :QLSM-ACCTNO,
              :QLSM-SEQNO,
              :QLSM-LTOUCH-DATE,
              :QLSM-MISC-FIELD-1,
              :QLSM-MISC-FIELD-2,
              :QLSM-MISC-FIELD-3,
              :QLSM-MISC-FIELD-4,
              :QLSM-MISC-FIELD-5,
              :QLSM-MISC-FIELD-6,
              :QLSM-MISC-FIELD-7,
              :QLSM-MISC-FIELD-8,
              :QLSM-MISC-FIELD-9,
              :QLSM-MISC-FIELD-10,
              :QLSM-MISC-FIELD-11,
              :QLSM-MISC-FIELD-12,
              :QLSM-MISC-FIELD-13,
              :QLSM-MISC-FIELD-14,
              :QLSM-MISC-FIELD-15,
              :QLSM-MISC-FIELD-16,
              :QLSM-MISC-FIELD-17,
              :QLSM-MISC-FIELD-18,
              :QLSM-MISC-FIELD-19,
              :QLSM-MISC-FIELD-20,
              :QLSM-MISC-FIELD-21,
              :QLSM-MISC-FIELD-22,
              :QLSM-MISC-FIELD-23,
              :QLSM-MISC-FIELD-24,
              :QLSM-MISC-FIELD-25,
              :QLSM-MISC-FIELD-26,
              :QLSM-MISC-FIELD-27,
              :QLSM-MISC-FIELD-28,
              :QLSM-MISC-FIELD-29,
              :QLSM-MISC-FIELD-30,
              :QLSM-MISC-FIELD-31,
              :QLSM-MISC-FIELD-32,
              :QLSM-TRANS-FROM-BRNO,
              :QLSM-TRANS-FROM-ACCTNO,
              :QLSM-TRANS-FROM-DATE,
              :QLSM-MISPAY-90-DATE,
              :QLSM-SECOND-BEN-NAME,
              :QLSM-SECOND-BEN-ADR,
              :QLSM-SECOND-BEN-CITY,
              :QLSM-SECOND-BEN-STATE,
              :QLSM-SECOND-BEN-ZIP,
              :QLSM-SECOND-BEN-PHONE,
              :QLSM-SECOND-BEN-RELATION)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LSM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LSM1-FILE.

           MOVE LSM-PATH-OWNBR  TO LSM-BRNO.
           ACCEPT LSM-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LSM-PATH-SQL    TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.
           PERFORM SET-LSM-FIELDS.

           EXEC SQL
            UPDATE DBO.LSMFILE
             SET
              LSMFILE.LSM_BRNO = :QLSM-BRNO,
              LSMFILE.LSM_ACCTNO = :QLSM-ACCTNO,
              LSMFILE.LSM_SEQNO = :QLSM-SEQNO,
              LSMFILE.LSM_LTOUCH_DATE = :QLSM-LTOUCH-DATE,
              LSMFILE.LSM_MISC_FIELD_1 = :QLSM-MISC-FIELD-1,
              LSMFILE.LSM_MISC_FIELD_2 = :QLSM-MISC-FIELD-2,
              LSMFILE.LSM_MISC_FIELD_3 = :QLSM-MISC-FIELD-3,
              LSMFILE.LSM_MISC_FIELD_4 = :QLSM-MISC-FIELD-4,
              LSMFILE.LSM_MISC_FIELD_5 = :QLSM-MISC-FIELD-5,
              LSMFILE.LSM_MISC_FIELD_6 = :QLSM-MISC-FIELD-6,
              LSMFILE.LSM_MISC_FIELD_7 = :QLSM-MISC-FIELD-7,
              LSMFILE.LSM_MISC_FIELD_8 = :QLSM-MISC-FIELD-8,
              LSMFILE.LSM_MISC_FIELD_9 = :QLSM-MISC-FIELD-9,
              LSMFILE.LSM_MISC_FIELD_10 = :QLSM-MISC-FIELD-10,
              LSMFILE.LSM_MISC_FIELD_11 = :QLSM-MISC-FIELD-11,
              LSMFILE.LSM_MISC_FIELD_12 = :QLSM-MISC-FIELD-12,
              LSMFILE.LSM_MISC_FIELD_13 = :QLSM-MISC-FIELD-13,
              LSMFILE.LSM_MISC_FIELD_14 = :QLSM-MISC-FIELD-14,
              LSMFILE.LSM_MISC_FIELD_15 = :QLSM-MISC-FIELD-15,
              LSMFILE.LSM_MISC_FIELD_16 = :QLSM-MISC-FIELD-16,
              LSMFILE.LSM_MISC_FIELD_17 = :QLSM-MISC-FIELD-17,
              LSMFILE.LSM_MISC_FIELD_18 = :QLSM-MISC-FIELD-18,
              LSMFILE.LSM_MISC_FIELD_19 = :QLSM-MISC-FIELD-19,
              LSMFILE.LSM_MISC_FIELD_20 = :QLSM-MISC-FIELD-20,
              LSMFILE.LSM_MISC_FIELD_21 = :QLSM-MISC-FIELD-21,
              LSMFILE.LSM_MISC_FIELD_22 = :QLSM-MISC-FIELD-22,
              LSMFILE.LSM_MISC_FIELD_23 = :QLSM-MISC-FIELD-23,
              LSMFILE.LSM_MISC_FIELD_24 = :QLSM-MISC-FIELD-24,
              LSMFILE.LSM_MISC_FIELD_25 = :QLSM-MISC-FIELD-25,
              LSMFILE.LSM_MISC_FIELD_26 = :QLSM-MISC-FIELD-26,
              LSMFILE.LSM_MISC_FIELD_27 = :QLSM-MISC-FIELD-27,
              LSMFILE.LSM_MISC_FIELD_28 = :QLSM-MISC-FIELD-28,
              LSMFILE.LSM_MISC_FIELD_29 = :QLSM-MISC-FIELD-29,
              LSMFILE.LSM_MISC_FIELD_30 = :QLSM-MISC-FIELD-30,
              LSMFILE.LSM_MISC_FIELD_31 = :QLSM-MISC-FIELD-31,
              LSMFILE.LSM_MISC_FIELD_32 = :QLSM-MISC-FIELD-32,
              LSMFILE.LSM_TRANS_FROM_BRNO = :QLSM-TRANS-FROM-BRNO,
              LSMFILE.LSM_TRANS_FROM_ACCTNO =
                                          :QLSM-TRANS-FROM-ACCTNO,
              LSMFILE.LSM_TRANS_FROM_DATE = :QLSM-TRANS-FROM-DATE,
              LSMFILE.LSM_MISPAY_90_DATE = :QLSM-MISPAY-90-DATE,
              LSMFILE.LSM_SECOND_BEN_NAME = :QLSM-SECOND-BEN-NAME,
              LSMFILE.LSM_SECOND_BEN_ADR = :QLSM-SECOND-BEN-ADR,
              LSMFILE.LSM_SECOND_BEN_CITY = :QLSM-SECOND-BEN-CITY,
              LSMFILE.LSM_SECOND_BEN_ST = :QLSM-SECOND-BEN-STATE,
              LSMFILE.LSM_SECOND_BEN_ZIP = :QLSM-SECOND-BEN-ZIP,
              LSMFILE.LSM_SECOND_BEN_PHONE =
                                           :QLSM-SECOND-BEN-PHONE,
              LSMFILE.LSM_SECOND_BEN_RELATION =
                                         :QLSM-SECOND-BEN-RELATION
             WHERE LSMFILE.LSM_BRNO = :QLSM-BRNO
               AND LSMFILE.LSM_ACCTNO = :QLSM-ACCTNO
               AND LSMFILE.LSM_SEQNO = :QLSM-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LSM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LSM1-FILE.

           MOVE LSM-PATH-OWNBR  TO LSM-BRNO.
           PERFORM DELETE-IT.
           MOVE LSM-PATH-SQL    TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.
           PERFORM SET-LSM-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LSMFILE
             WHERE LSMFILE.LSM_BRNO = :QLSM-BRNO
               AND LSMFILE.LSM_ACCTNO = :QLSM-ACCTNO
               AND LSMFILE.LSM_SEQNO = :QLSM-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LSM1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LSM1-FILE.

           PERFORM CLOSE-IT.

           IF ( LSM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSM1-CURSOR-STAT.

