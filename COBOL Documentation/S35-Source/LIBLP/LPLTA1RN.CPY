      * COPYMEMBER: LIBLP/LPLTA1RN
       LOAD-LTA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTA-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTA-ALL-DATA
              MOVE LTA-ALL-PATH             TO LTA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTA-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTA-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTA-ALL-DATA
              MOVE LTA-ALL-PATH             TO LTA-PATH.

      *-----------------------------------------------------------------
       OPEN-LTA1-FILE.
           PERFORM LOAD-LTA1-FILE.
           PERFORM OPEN-IT.
           MOVE LTA-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTA1-FILE.
           PERFORM START-IT.
           MOVE LTA-PATH-OWNBR  TO LTA-BRNO.
           MOVE LTA-PATH-SQL    TO E-FILE.
           MOVE LTA1-KEY        TO E-KEYX.

           IF ( LTA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTA1-FILE.

           IF ( QLTA1-WEND-BRNO = ZEROES    ) OR
              ( QLTA1-WEND-BRNO NOT NUMERIC )
              MOVE LTA-PATH-OWNBR        TO QLTA1-WBEG-BRNO
              MOVE LTA-PATH-OWNBR        TO QLTA1-WEND-BRNO
              MOVE LTA-ACCTNO            TO QLTA1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTA1-WEND-ACCTNO
              MOVE LTA-SEQNO             TO QLTA1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTA1-WEND-SEQNO.

           EXEC SQL
            DECLARE LTA1_CURSOR CURSOR FOR
            SELECT LTAFILE.LTA_BRNO,
                   LTAFILE.LTA_ACCTNO,
                   LTAFILE.LTA_SEQNO,
                   CAST(LTAFILE.LTA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LTAFILE.LTA_DLR_REQ,
                   LTAFILE.LTA_INS_TYPE,
                   LTAFILE.LTA_INS_CO,
                   LTAFILE.LTA_POLICY_NO,
                   LTAFILE.LTA_AGENCY,
                   LTAFILE.LTA_AGENT,
                   LTAFILE.LTA_ADR1,
                   LTAFILE.LTA_ADR2,
                   LTAFILE.LTA_CITY,
                   LTAFILE.LTA_STATE,
                   LTAFILE.LTA_ZIP,
                   LTAFILE.LTA_PHONE,
                   CAST(LTAFILE.LTA_EFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTAFILE.LTA_EXP_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTAFILE.LTA_CAN_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTAFILE.LTA_CONF_DATE
                                                  AS VARCHAR(10)),
                   LTAFILE.LTA_LAST_FORM,
                   LTAFILE.LTA_COV_TYPE,
                   LTAFILE.LTA_BIL_COV,
                   LTAFILE.LTA_PDL_COV,
                   LTAFILE.LTA_PIP_COV,
                   LTAFILE.LTA_UMB_COV,
                   LTAFILE.LTA_COL_COV,
                   LTAFILE.LTA_LOSS_PAYEE,
                   LTAFILE.LTA_COM_COV,
                   LTAFILE.LTA_TRACK_ONLY,
                   LTAFILE.LTA_INSURANCE,
                   LTAFILE.LTA_TRACK_MONTHS,
                   LTAFILE.LTA_INS_MONTHS,
                   LTAFILE.LTA_CPP_MONTHS
            FROM DBO.LTAFILE
            WHERE LTAFILE.LTA_BRNO  = :QLTA1-WBEG-BRNO
              AND LTAFILE.LTA_ACCTNO >= :QLTA1-WBEG-ACCTNO
              AND LTAFILE.LTA_ACCTNO <= :QLTA1-WEND-ACCTNO
              AND LTAFILE.LTA_SEQNO >= :QLTA1-WBEG-SEQNO
              AND LTAFILE.LTA_SEQNO <= :QLTA1-WEND-SEQNO
            ORDER BY LTAFILE.LTA_BRNO,
                     LTAFILE.LTA_ACCTNO,
                     LTAFILE.LTA_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTA1-WBEG-BRNO
                          QLTA1-WEND-BRNO
                          QLTA1-WBEG-ACCTNO
                          QLTA1-WEND-ACCTNO
                          QLTA1-WBEG-SEQNO
                          QLTA1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTA1-FILE.
           PERFORM READ-IT.
           MOVE LTA-PATH-OWNBR  TO LTA-BRNO. 
           MOVE LTA-PATH-SQL    TO E-FILE.
           MOVE LTA1-KEY        TO E-KEYX.
           INITIALIZE QLTA-REC.

           MOVE LTA-BRNO   TO QLTA-BRNO.
           MOVE LTA-ACCTNO TO QLTA-ACCTNO.
           MOVE LTA-SEQNO  TO QLTA-SEQNO.

           EXEC SQL
            SELECT LTAFILE.LTA_BRNO,
                   LTAFILE.LTA_ACCTNO,
                   LTAFILE.LTA_SEQNO,
                   CAST(LTAFILE.LTA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LTAFILE.LTA_DLR_REQ,
                   LTAFILE.LTA_INS_TYPE,
                   LTAFILE.LTA_INS_CO,
                   LTAFILE.LTA_POLICY_NO,
                   LTAFILE.LTA_AGENCY,
                   LTAFILE.LTA_AGENT,
                   LTAFILE.LTA_ADR1,
                   LTAFILE.LTA_ADR2,
                   LTAFILE.LTA_CITY,
                   LTAFILE.LTA_STATE,
                   LTAFILE.LTA_ZIP,
                   LTAFILE.LTA_PHONE,
                   CAST(LTAFILE.LTA_EFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTAFILE.LTA_EXP_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTAFILE.LTA_CAN_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTAFILE.LTA_CONF_DATE
                                                  AS VARCHAR(10)),
                   LTAFILE.LTA_LAST_FORM,
                   LTAFILE.LTA_COV_TYPE,
                   LTAFILE.LTA_BIL_COV,
                   LTAFILE.LTA_PDL_COV,
                   LTAFILE.LTA_PIP_COV,
                   LTAFILE.LTA_UMB_COV,
                   LTAFILE.LTA_COL_COV,
                   LTAFILE.LTA_LOSS_PAYEE,
                   LTAFILE.LTA_COM_COV,
                   LTAFILE.LTA_TRACK_ONLY,
                   LTAFILE.LTA_INSURANCE,
                   LTAFILE.LTA_TRACK_MONTHS,
                   LTAFILE.LTA_INS_MONTHS,
                   LTAFILE.LTA_CPP_MONTHS
            INTO :QLTA-REC
            FROM DBO.LTAFILE
            WHERE LTAFILE.LTA_BRNO = :QLTA-BRNO
              AND LTAFILE.LTA_ACCTNO = :QLTA-ACCTNO
              AND LTAFILE.LTA_SEQNO = :QLTA-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTA-FIELDS.

      *-----------------------------------------------------------------
       READ-LTA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTA-PATH-OWNBR  TO LTA-BRNO.
           MOVE LTA-PATH-SQL    TO E-FILE.
           MOVE LTA1-KEY        TO E-KEYX.
           INITIALIZE QLTA-REC.

           IF ( LTA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTA1_CURSOR INTO :QLTA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTA-FIELDS.


      *-----------------------------------------------------------------
       CLOSE-LTA1-FILE.

           PERFORM CLOSE-IT.
           IF ( LTA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTA1-CURSOR-STAT.

