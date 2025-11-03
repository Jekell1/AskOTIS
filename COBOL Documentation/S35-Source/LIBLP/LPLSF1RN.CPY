      * COPYMEMBER: LIBLP/LPLSF1R
       LOAD-LSF1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSF-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSF-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSF-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSF-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSF-ALL-DATA
              MOVE LSF-ALL-PATH             TO LSF-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSF-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSF-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSF-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSF-ALL-DATA
              MOVE LSF-ALL-PATH             TO LSF-PATH.

      *-----------------------------------------------------------------
       OPEN-LSF1-FILE.
           PERFORM LOAD-LSF1-FILE.
           PERFORM OPEN-IT.
           MOVE LSF-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSF1-FILE.
           PERFORM START-IT.
           MOVE LSF-PATH-OWNBR  TO LSF-BRNO.
           MOVE LSF-PATH-SQL    TO E-FILE.
           MOVE LSF1-KEY        TO E-KEYX.

           IF ( LSF1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSF1-FILE.

           IF ( QLSF1-WEND-BRNO = ZEROES    ) OR
              ( QLSF1-WEND-BRNO NOT NUMERIC )
              MOVE LSF-PATH-OWNBR        TO QLSF1-WBEG-BRNO
              MOVE LSF-PATH-OWNBR        TO QLSF1-WEND-BRNO
              MOVE LSF-ACCTNO            TO QLSF1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSF1-WEND-ACCTNO
              MOVE LSF-SEQNO             TO QLSF1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSF1-WEND-SEQNO.

           EXEC SQL
            DECLARE LSF1_CURSOR CURSOR FOR
            SELECT LSFFILE.LSF_BRNO,
                   LSFFILE.LSF_ACCTNO,
                   LSFFILE.LSF_SEQNO,
                   CAST(LSFFILE.LSF_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_SKIP_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_SKIP_END_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_OFR_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_OFR_END_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_IMPOUND_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_IMPOUND_END_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_BNKRPT_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_BNKRPT_END_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_INSUR_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_INSUR_END_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_COLLECT_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_COLLECT_END_DATE
                                                  AS VARCHAR(10))
            FROM DBO.LSFFILE
            WHERE LSFFILE.LSF_BRNO  = :QLSF1-WBEG-BRNO
              AND LSFFILE.LSF_ACCTNO >= :QLSF1-WBEG-ACCTNO
              AND LSFFILE.LSF_ACCTNO <= :QLSF1-WEND-ACCTNO
              AND LSFFILE.LSF_SEQNO >= :QLSF1-WBEG-SEQNO
              AND LSFFILE.LSF_SEQNO <= :QLSF1-WEND-SEQNO
            ORDER BY LSFFILE.LSF_BRNO,
                     LSFFILE.LSF_ACCTNO,
                     LSFFILE.LSF_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSF1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSF1-WBEG-BRNO
                          QLSF1-WEND-BRNO
                          QLSF1-WBEG-ACCTNO
                          QLSF1-WEND-ACCTNO
                          QLSF1-WBEG-SEQNO
                          QLSF1-WEND-SEQNO.


           MOVE STAT---GOOD TO LSF1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSF1-FILE.
           PERFORM READ-IT.
           MOVE LSF-PATH-OWNBR  TO LSF-BRNO. 
           MOVE LSF-PATH-SQL    TO E-FILE.
           MOVE LSF1-KEY        TO E-KEYX.
           INITIALIZE QLSF-REC.

           MOVE LSF-BRNO   TO QLSF-BRNO.
           MOVE LSF-ACCTNO TO QLSF-ACCTNO.
           MOVE LSF-SEQNO  TO QLSF-SEQNO.

           EXEC SQL
            SELECT LSFFILE.LSF_BRNO,
                   LSFFILE.LSF_ACCTNO,
                   LSFFILE.LSF_SEQNO,
                   CAST(LSFFILE.LSF_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_SKIP_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_SKIP_END_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_OFR_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_OFR_END_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_IMPOUND_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_IMPOUND_END_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_BNKRPT_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_BNKRPT_END_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_INSUR_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_INSUR_END_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_COLLECT_START_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSFFILE.LSF_COLLECT_END_DATE
                                                  AS VARCHAR(10))
            INTO :QLSF-REC
            FROM DBO.LSFFILE
            WHERE LSFFILE.LSF_BRNO = :QLSF-BRNO
              AND LSFFILE.LSF_ACCTNO = :QLSF-ACCTNO
              AND LSFFILE.LSF_SEQNO = :QLSF-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSF1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSF-FIELDS.

      *-----------------------------------------------------------------
       READ-LSF1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSF-PATH-OWNBR  TO LSF-BRNO.
           MOVE LSF-PATH-SQL    TO E-FILE.
           MOVE LSF1-KEY        TO E-KEYX.
           INITIALIZE QLSF-REC.

           IF ( LSF1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSF1_CURSOR INTO :QLSF-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSF1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSF-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LSF1-FILE.

           PERFORM CLOSE-IT.
           IF ( LSF1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSF1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSF1-CURSOR-STAT.

