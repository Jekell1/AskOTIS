      * COPYMEMBER: LIBLP/LPLS1RN
      * 20220922 BAH ADDED LSM-MISPAY-90-DATE #1570
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
            WHERE LSMFILE.LSM_BRNO =  :QLSM1-WBEG-BRNO
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
       CLOSE-LSM1-FILE.

           PERFORM CLOSE-IT.
           IF ( LSM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSM1-CURSOR-STAT.

