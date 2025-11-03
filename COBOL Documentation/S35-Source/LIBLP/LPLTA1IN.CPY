      * COPYMEMBER: LIBLP/LPLTA1IN AUTO INSURANCE TRAILER
      * REV:
      *
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
       WRITE-LTA1-FILE.

           MOVE LTA-PATH-OWNBR  TO LTA-BRNO.
           ACCEPT LTA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTA-PATH-SQL    TO E-FILE.
           MOVE LTA1-KEY        TO E-KEYX.
           PERFORM SET-LTA-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LTAFILE
             (LTAFILE.LTA_BRNO,
              LTAFILE.LTA_ACCTNO,
              LTAFILE.LTA_SEQNO,
              LTAFILE.LTA_LTOUCH_DATE,
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
              LTAFILE.LTA_EFF_DATE,
              LTAFILE.LTA_EXP_DATE,
              LTAFILE.LTA_CAN_DATE,
              LTAFILE.LTA_CONF_DATE,
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
              LTAFILE.LTA_CPP_MONTHS)
            VALUES
             (:QLTA-BRNO,
              :QLTA-ACCTNO,
              :QLTA-SEQNO,
              :QLTA-LTOUCH-DATE,
              :QLTA-DLR-REQ,
              :QLTA-INS-TYPE,
              :QLTA-INS-CO,
              :QLTA-POLICY-NO,
              :QLTA-AGENCY,
              :QLTA-AGENT,
              :QLTA-ADR1,
              :QLTA-ADR2,
              :QLTA-CITY,
              :QLTA-STATE,
              :QLTA-ZIP,
              :QLTA-PHONE,
              :QLTA-EFF-DATE,
              :QLTA-EXP-DATE,
              :QLTA-CAN-DATE,
              :QLTA-CONF-DATE,
              :QLTA-LAST-FORM,
              :QLTA-COV-TYPE,
              :QLTA-BIL-COV,
              :QLTA-PDL-COV,
              :QLTA-PIP-COV,
              :QLTA-UMB-COV,
              :QLTA-COL-COV,
              :QLTA-LOSS-PAYEE,
              :QLTA-COM-COV,
              :QLTA-TRACK-ONLY,
              :QLTA-INSURANCE,
              :QLTA-TRACK-MONTHS,
              :QLTA-INS-MONTHS,
              :QLTA-CPP-MONTHS)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTA1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LTA1-FILE.

           MOVE LTA-PATH-OWNBR  TO LTA-BRNO.
           ACCEPT LTA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTA-PATH-SQL    TO E-FILE.
           MOVE LTA1-KEY        TO E-KEYX.
           PERFORM SET-LTA-FIELDS.

           EXEC SQL
            UPDATE DBO.LTAFILE
             SET
              LTAFILE.LTA_BRNO = :QLTA-BRNO,
              LTAFILE.LTA_ACCTNO = :QLTA-ACCTNO,
              LTAFILE.LTA_SEQNO = :QLTA-SEQNO,
              LTAFILE.LTA_LTOUCH_DATE =
                 :QLTA-LTOUCH-DATE,
              LTAFILE.LTA_DLR_REQ = :QLTA-DLR-REQ,
              LTAFILE.LTA_INS_TYPE = :QLTA-INS-TYPE,
              LTAFILE.LTA_INS_CO = :QLTA-INS-CO,
              LTAFILE.LTA_POLICY_NO = :QLTA-POLICY-NO,
              LTAFILE.LTA_AGENCY = :QLTA-AGENCY,
              LTAFILE.LTA_AGENT = :QLTA-AGENT,
              LTAFILE.LTA_ADR1 = :QLTA-ADR1,
              LTAFILE.LTA_ADR2 = :QLTA-ADR2,
              LTAFILE.LTA_CITY = :QLTA-CITY,
              LTAFILE.LTA_STATE = :QLTA-STATE,
              LTAFILE.LTA_ZIP = :QLTA-ZIP,
              LTAFILE.LTA_PHONE = :QLTA-PHONE,
              LTAFILE.LTA_EFF_DATE = :QLTA-EFF-DATE,
              LTAFILE.LTA_EXP_DATE = :QLTA-EXP-DATE,
              LTAFILE.LTA_CAN_DATE = :QLTA-CAN-DATE,
              LTAFILE.LTA_CONF_DATE = :QLTA-CONF-DATE,
              LTAFILE.LTA_LAST_FORM = :QLTA-LAST-FORM,
              LTAFILE.LTA_COV_TYPE = :QLTA-COV-TYPE,
              LTAFILE.LTA_BIL_COV = :QLTA-BIL-COV,
              LTAFILE.LTA_PDL_COV = :QLTA-PDL-COV,
              LTAFILE.LTA_PIP_COV = :QLTA-PIP-COV,
              LTAFILE.LTA_UMB_COV = :QLTA-UMB-COV,
              LTAFILE.LTA_COL_COV = :QLTA-COL-COV,
              LTAFILE.LTA_LOSS_PAYEE = :QLTA-LOSS-PAYEE,
              LTAFILE.LTA_COM_COV = :QLTA-COM-COV,
              LTAFILE.LTA_TRACK_ONLY = :QLTA-TRACK-ONLY,
              LTAFILE.LTA_INSURANCE = :QLTA-INSURANCE,
              LTAFILE.LTA_TRACK_MONTHS = :QLTA-TRACK-MONTHS,
              LTAFILE.LTA_INS_MONTHS = :QLTA-INS-MONTHS,
              LTAFILE.LTA_CPP_MONTHS = :QLTA-CPP-MONTHS
             WHERE LTAFILE.LTA_BRNO = :QLTA-BRNO
               AND LTAFILE.LTA_ACCTNO = :QLTA-ACCTNO
               AND LTAFILE.LTA_SEQNO = :QLTA-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTA1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LTA1-FILE.

           MOVE LTA-PATH-OWNBR  TO LTA-BRNO.
           PERFORM DELETE-IT.
           MOVE LTA-PATH-SQL    TO E-FILE.
           MOVE LTA1-KEY        TO E-KEYX.
           PERFORM SET-LTA-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LTAFILE
             WHERE LTAFILE.LTA_BRNO = :QLTA-BRNO
               AND LTAFILE.LTA_ACCTNO = :QLTA-ACCTNO
               AND LTAFILE.LTA_SEQNO = :QLTA-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LTA1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LTA1-FILE.

           PERFORM CLOSE-IT.

           IF ( LTA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTA1-CURSOR-STAT.

