      * COPYMEMBER: LIBLP/LPLTB1RN
       LOAD-LTB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTB-ALL-DATA
              MOVE LTB-ALL-PATH             TO LTB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTB-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTB-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTB-ALL-DATA
              MOVE LTB-ALL-PATH             TO LTB-PATH.

      *-----------------------------------------------------------------
       OPEN-LTB1-FILE.
           PERFORM LOAD-LTB1-FILE.
           PERFORM OPEN-IT.
           MOVE LTB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTB1-FILE.
           PERFORM START-IT.
           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.

           IF ( LTB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTB1-FILE.

           IF ( QLTB1-WEND-BRNO = ZEROES    ) OR
              ( QLTB1-WEND-BRNO NOT NUMERIC )
              MOVE LTB-PATH-OWNBR        TO QLTB1-WBEG-BRNO
              MOVE LTB-PATH-OWNBR        TO QLTB1-WEND-BRNO
              MOVE LTB-ACCTNO            TO QLTB1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTB1-WEND-ACCTNO
              MOVE LTB-SEQNO             TO QLTB1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTB1-WEND-SEQNO.


           EXEC SQL
            DECLARE LTB1_CURSOR CURSOR FOR
            SELECT LTBFILE.LTB_BRNO,
                   LTBFILE.LTB_ACCTNO,
                   LTBFILE.LTB_SEQNO,
                   CAST(LTBFILE.LTB_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_FILEDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_CASE_NO,
                   LTBFILE.LTB_CHAPTER,
                   LTBFILE.LTB_MISC1,
                   LTBFILE.LTB_MISC2,
                   LTBFILE.LTB_MISC3,
                   CAST(LTBFILE.LTB_BDDATE AS VARCHAR(10)),
                   LTBFILE.LTB_WHO_FILED,
                   LTBFILE.LTB_ENDORSER_FILED,
                   LTBFILE.LTB_CLAIM_AMT,
                   LTBFILE.LTB_ACTIVE_INACTIVE,
                   LTBFILE.LTB_INT_LC_DUE,
                   LTBFILE.LTB_OTH_DUE,
                   LTBFILE.LTB_TDUEAMT,
                   LTBFILE.LTB_NAME_1,
                   LTBFILE.LTB_ADR1_1,
                   LTBFILE.LTB_ADR2_1,
                   LTBFILE.LTB_CITY_1,
                   LTBFILE.LTB_STATE_1,
                   LTBFILE.LTB_ZIP_1,
                   LTBFILE.LTB_PHONE_1,
                   LTBFILE.LTB_NAME_2,
                   LTBFILE.LTB_ADR1_2,
                   LTBFILE.LTB_ADR2_2,
                   LTBFILE.LTB_CITY_2,
                   LTBFILE.LTB_STATE_2,
                   LTBFILE.LTB_ZIP_2,
                   LTBFILE.LTB_PHONE_2,
                   LTBFILE.LTB_NAME_3,
                   LTBFILE.LTB_ADR1_3,
                   LTBFILE.LTB_ADR2_3,
                   LTBFILE.LTB_CITY_3,
                   LTBFILE.LTB_STATE_3,
                   LTBFILE.LTB_ZIP_3,
                   LTBFILE.LTB_PHONE_3,
                   CAST(LTBFILE.LTB_REAFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_REAFF_SYSDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_REAFF_BALANCE,
                   LTBFILE.LTB_REAFF_PAYAMT,
                   LTBFILE.LTB_REAFF_NO_PAYS,
                   LTBFILE.LTB_INTRATE,
                   CAST(LTBFILE.LTB_RESCIND_DATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_PARTIAL_FULL,
                   CAST(LTBFILE.LTB_DISCHARGE_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_DISCHARGE_SYSDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_GROSS_BAL
            FROM DBO.LTBFILE
            WHERE LTBFILE.LTB_BRNO  = :QLTB1-WBEG-BRNO
              AND LTBFILE.LTB_ACCTNO >= :QLTB1-WBEG-ACCTNO
              AND LTBFILE.LTB_ACCTNO <= :QLTB1-WEND-ACCTNO
              AND LTBFILE.LTB_SEQNO >= :QLTB1-WBEG-SEQNO
              AND LTBFILE.LTB_SEQNO <= :QLTB1-WEND-SEQNO
            ORDER BY LTBFILE.LTB_BRNO,
                     LTBFILE.LTB_ACCTNO,
                     LTBFILE.LTB_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTB1-WBEG-BRNO
                          QLTB1-WEND-BRNO
                          QLTB1-WBEG-ACCTNO
                          QLTB1-WEND-ACCTNO
                          QLTB1-WBEG-SEQNO
                          QLTB1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTB1-FILE.
           PERFORM READ-IT.
           MOVE LTB-PATH-OWNBR  TO LTB-BRNO. 
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           INITIALIZE QLTB-REC.

           MOVE LTB-BRNO   TO QLTB-BRNO.
           MOVE LTB-ACCTNO TO QLTB-ACCTNO.
           MOVE LTB-SEQNO  TO QLTB-SEQNO.

           EXEC SQL
            SELECT LTBFILE.LTB_BRNO,
                   LTBFILE.LTB_ACCTNO,
                   LTBFILE.LTB_SEQNO,
                   CAST(LTBFILE.LTB_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_FILEDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_CASE_NO,
                   LTBFILE.LTB_CHAPTER,
                   LTBFILE.LTB_MISC1,
                   LTBFILE.LTB_MISC2,
                   LTBFILE.LTB_MISC3,
                   CAST(LTBFILE.LTB_BDDATE AS VARCHAR(10)),
                   LTBFILE.LTB_WHO_FILED,
                   LTBFILE.LTB_ENDORSER_FILED,
                   LTBFILE.LTB_CLAIM_AMT,
                   LTBFILE.LTB_ACTIVE_INACTIVE,
                   LTBFILE.LTB_INT_LC_DUE,
                   LTBFILE.LTB_OTH_DUE,
                   LTBFILE.LTB_TDUEAMT,
                   LTBFILE.LTB_NAME_1,
                   LTBFILE.LTB_ADR1_1,
                   LTBFILE.LTB_ADR2_1,
                   LTBFILE.LTB_CITY_1,
                   LTBFILE.LTB_STATE_1,
                   LTBFILE.LTB_ZIP_1,
                   LTBFILE.LTB_PHONE_1,
                   LTBFILE.LTB_NAME_2,
                   LTBFILE.LTB_ADR1_2,
                   LTBFILE.LTB_ADR2_2,
                   LTBFILE.LTB_CITY_2,
                   LTBFILE.LTB_STATE_2,
                   LTBFILE.LTB_ZIP_2,
                   LTBFILE.LTB_PHONE_2,
                   LTBFILE.LTB_NAME_3,
                   LTBFILE.LTB_ADR1_3,
                   LTBFILE.LTB_ADR2_3,
                   LTBFILE.LTB_CITY_3,
                   LTBFILE.LTB_STATE_3,
                   LTBFILE.LTB_ZIP_3,
                   LTBFILE.LTB_PHONE_3,
                   CAST(LTBFILE.LTB_REAFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_REAFF_SYSDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_REAFF_BALANCE,
                   LTBFILE.LTB_REAFF_PAYAMT,
                   LTBFILE.LTB_REAFF_NO_PAYS,
                   LTBFILE.LTB_INTRATE,
                   CAST(LTBFILE.LTB_RESCIND_DATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_PARTIAL_FULL,
                   CAST(LTBFILE.LTB_DISCHARGE_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_DISCHARGE_SYSDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_GROSS_BAL
            INTO :QLTB-REC
            FROM DBO.LTBFILE
            WHERE LTBFILE.LTB_BRNO = :QLTB-BRNO
              AND LTBFILE.LTB_ACCTNO = :QLTB-ACCTNO
              AND LTBFILE.LTB_SEQNO = :QLTB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTB-FIELDS.

      *-----------------------------------------------------------------
       READ-LTB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           INITIALIZE QLTB-REC.

           IF ( LTB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTB1_CURSOR INTO :QLTB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTB1-FILE.

           PERFORM CLOSE-IT.
           IF ( LTB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTB1-CURSOR-STAT.

