      * COPYMEMBER: LIBLP/LPCK1IN
       LOAD-CK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CK-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CK-ALL-BASE
              MOVE FILPATH-MACHINE         TO CK-ALL-MACHINE
              MOVE FILPATH-DATA            TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH.

      *-----------------------------------------------------------------
       OPEN-CK1-FILE.
           PERFORM LOAD-CK1-FILE.
           PERFORM OPEN-IT.
           MOVE CK-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CK1-FILE.
           PERFORM START-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.

           IF ( CK1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CK1-FILE.

           IF ( QCK1-WEND-CKNO = ZEROES    ) OR
              ( QCK1-WEND-CKNO NOT NUMERIC )
              MOVE CK-PATH-OWNBR        TO QCK1-WBEG-BRNO
              MOVE CK-PATH-OWNBR        TO QCK1-WEND-BRNO
              MOVE CK-NO                TO QCK1-WBEG-CKNO
              MOVE ALL "9"              TO QCK1-WEND-CKNO.

           EXEC SQL
            DECLARE CK1_CURSOR CURSOR FOR
            SELECT CKFILE.CK_BRNO,
                   CKFILE.CK_NO,
                   CAST(CKFILE.CK_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CKFILE.CK_TYPE,
                   CAST(CKFILE.CK_DATE AS VARCHAR(10)),
                   CKFILE.CK_AMT,
                   CKFILE.CK_PAYEE,
                   CKFILE.CK_ADR1,
                   CKFILE.CK_ADR2,
                   CKFILE.CK_CITY,
                   CKFILE.CK_STATE,
                   CKFILE.CK_ZIP,
                   CKFILE.CK_REF,
                   CKFILE.CK_REASON,
                   CKFILE.CK_GLNO_1,
                   CKFILE.CK_GLAMT_1,
                   CKFILE.CK_GLNO_2,
                   CKFILE.CK_GLAMT_2,
                   CKFILE.CK_GLNO_3,
                   CKFILE.CK_GLAMT_3,
                   CKFILE.CK_GLNO_4,
                   CKFILE.CK_GLAMT_4,
                   CKFILE.CK_GLNO_5,
                   CKFILE.CK_GLAMT_5,
                   CKFILE.CK_VOID,
                   CKFILE.CK_PAID,
                   CKFILE.CK_CLASS,
                   CKFILE.CK_DLNO,
                   CKFILE.CK_PGM,
                   CKFILE.CK_NC_SEQNO,
                   CAST(CKFILE.CK_VOID_DATE
                                                  AS VARCHAR(10)),
                   CKFILE.CK_ACCTNO,
                   CKFILE.CK_DLRVNO,
                   CKFILE.CK_CLEARED_FG,
                   CKFILE.CK_SSNO,
                   CKFILE.CK_BATCHID,
                   CKFILE.CK_UNPAID,
                   CAST(CKFILE.CK_CLEARED_DATE
                                                  AS VARCHAR(10)),
                   CKFILE.CK_TILLNO,
                   CKFILE.CK_EVENT_CD,
                   CKFILE.CK_PAYEE_NO,
                   CKFILE.CK_FREE1,
                   CKFILE.CK_FREE2,
                   CKFILE.CK_BWPROC
            FROM DBO.CKFILE
            WHERE CKFILE.CK_BRNO = :QCK1-WBEG-BRNO
              AND CKFILE.CK_NO >= :QCK1-WBEG-CKNO
              AND CKFILE.CK_NO <= :QCK1-WEND-CKNO
              ORDER BY CKFILE.CK_BRNO,
                       CKFILE.CK_NO
           END-EXEC.

           EXEC SQL
               OPEN CK1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCK1-WBEG-BRNO 
                          QCK1-WEND-BRNO
                          QCK1-WBEG-CKNO
                          QCK1-WEND-CKNO.

           MOVE STAT---GOOD TO CK1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CK1-FILE.
           PERFORM READ-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO. 
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.
           INITIALIZE QCK-REC.

           MOVE CK-BRNO   TO QCK-BRNO.
           MOVE CK-NO   TO QCK-NO.

           EXEC SQL
            SELECT CKFILE.CK_BRNO,
                   CKFILE.CK_NO,
                   CAST(CKFILE.CK_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CKFILE.CK_TYPE,
                   CAST(CKFILE.CK_DATE AS VARCHAR(10)),
                   CKFILE.CK_AMT,
                   CKFILE.CK_PAYEE,
                   CKFILE.CK_ADR1,
                   CKFILE.CK_ADR2,
                   CKFILE.CK_CITY,
                   CKFILE.CK_STATE,
                   CKFILE.CK_ZIP,
                   CKFILE.CK_REF,
                   CKFILE.CK_REASON,
                   CKFILE.CK_GLNO_1,
                   CKFILE.CK_GLAMT_1,
                   CKFILE.CK_GLNO_2,
                   CKFILE.CK_GLAMT_2,
                   CKFILE.CK_GLNO_3,
                   CKFILE.CK_GLAMT_3,
                   CKFILE.CK_GLNO_4,
                   CKFILE.CK_GLAMT_4,
                   CKFILE.CK_GLNO_5,
                   CKFILE.CK_GLAMT_5,
                   CKFILE.CK_VOID,
                   CKFILE.CK_PAID,
                   CKFILE.CK_CLASS,
                   CKFILE.CK_DLNO,
                   CKFILE.CK_PGM,
                   CKFILE.CK_NC_SEQNO,
                   CAST(CKFILE.CK_VOID_DATE
                                                  AS VARCHAR(10)),
                   CKFILE.CK_ACCTNO,
                   CKFILE.CK_DLRVNO,
                   CKFILE.CK_CLEARED_FG,
                   CKFILE.CK_SSNO,
                   CKFILE.CK_BATCHID,
                   CKFILE.CK_UNPAID,
                   CAST(CKFILE.CK_CLEARED_DATE
                                                  AS VARCHAR(10)),
                   CKFILE.CK_TILLNO,
                   CKFILE.CK_EVENT_CD,
                   CKFILE.CK_PAYEE_NO,
                   CKFILE.CK_FREE1,
                   CKFILE.CK_FREE2,
                   CKFILE.CK_BWPROC
            INTO :QCK-REC
            FROM DBO.CKFILE
            WHERE CKFILE.CK_BRNO = :QCK-BRNO
              AND CKFILE.CK_NO = :QCK-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CK1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CK-FIELDS.

      *-----------------------------------------------------------------
       READ-CK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.
           INITIALIZE QCK-REC.

           IF ( CK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CK1_CURSOR INTO :QCK-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CK1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CK-FIELDS.

      *-----------------------------------------------------------------
       WRITE-CK1-FILE.

           MOVE CK-PATH-OWNBR  TO CK-BRNO. 
           ACCEPT CK-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.
           PERFORM SET-CK-FIELDS.

           EXEC SQL
            INSERT INTO DBO.CKFILE
             (CKFILE.CK_BRNO,
              CKFILE.CK_NO,
              CKFILE.CK_LTOUCH_DATE,
              CKFILE.CK_TYPE,
              CKFILE.CK_DATE,
              CKFILE.CK_AMT,
              CKFILE.CK_PAYEE,
              CKFILE.CK_ADR1,
              CKFILE.CK_ADR2,
              CKFILE.CK_CITY,
              CKFILE.CK_STATE,
              CKFILE.CK_ZIP,
              CKFILE.CK_REF,
              CKFILE.CK_REASON,
              CKFILE.CK_GLNO_1,
              CKFILE.CK_GLAMT_1,
              CKFILE.CK_GLNO_2,
              CKFILE.CK_GLAMT_2,
              CKFILE.CK_GLNO_3,
              CKFILE.CK_GLAMT_3,
              CKFILE.CK_GLNO_4,
              CKFILE.CK_GLAMT_4,
              CKFILE.CK_GLNO_5,
              CKFILE.CK_GLAMT_5,
              CKFILE.CK_VOID,
              CKFILE.CK_PAID,
              CKFILE.CK_CLASS,
              CKFILE.CK_DLNO,
              CKFILE.CK_PGM,
              CKFILE.CK_NC_SEQNO,
              CKFILE.CK_VOID_DATE,
              CKFILE.CK_ACCTNO,
              CKFILE.CK_DLRVNO,
              CKFILE.CK_CLEARED_FG,
              CKFILE.CK_SSNO,
              CKFILE.CK_BATCHID,
              CKFILE.CK_UNPAID,
              CKFILE.CK_CLEARED_DATE,
              CKFILE.CK_TILLNO,
              CKFILE.CK_EVENT_CD,
              CKFILE.CK_PAYEE_NO,
              CKFILE.CK_FREE1,
              CKFILE.CK_FREE2,
              CKFILE.CK_BWPROC)
            VALUES
             (:QCK-BRNO,
              :QCK-NO,
              :QCK-LTOUCH-DATE,
              :QCK-TYPE,
              :QCK-DATE,
              :QCK-AMT,
              :QCK-PAYEE,
              :QCK-ADR1,
              :QCK-ADR2,
              :QCK-CITY,
              :QCK-STATE,
              :QCK-ZIP,
              :QCK-REF,
              :QCK-REASON,
              :QCK-GLNO-1,
              :QCK-GLAMT-1,
              :QCK-GLNO-2,
              :QCK-GLAMT-2,
              :QCK-GLNO-3,
              :QCK-GLAMT-3,
              :QCK-GLNO-4,
              :QCK-GLAMT-4,
              :QCK-GLNO-5,
              :QCK-GLAMT-5,
              :QCK-VOID,
              :QCK-PAID,
              :QCK-CLASS,
              :QCK-DLNO,
              :QCK-PGM,
              :QCK-NC-SEQNO,
              :QCK-VOID-DATE,
              :QCK-ACCTNO,
              :QCK-DLRVNO,
              :QCK-CLEARED-FG,
              :QCK-SSNO,
              :QCK-BATCHID,
              :QCK-UNPAID,
              :QCK-CLEARED-DATE,
              :QCK-TILLNO,
              :QCK-EVENT-CD,
              :QCK-PAYEE-NO,
              :QCK-FREE1,
              :QCK-FREE2,
              :QCK-BWPROC)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-CK1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-CK1-FILE.

           MOVE CK-PATH-OWNBR  TO CK-BRNO. 
           ACCEPT CK-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.
           PERFORM SET-CK-FIELDS.

           EXEC SQL
            UPDATE DBO.CKFILE
             SET
              CKFILE.CK_BRNO = :QCK-BRNO,
              CKFILE.CK_NO = :QCK-NO,
              CKFILE.CK_LTOUCH_DATE =
                 :QCK-LTOUCH-DATE,
              CKFILE.CK_TYPE = :QCK-TYPE,
              CKFILE.CK_DATE =
                 :QCK-DATE,
              CKFILE.CK_AMT = :QCK-AMT,
              CKFILE.CK_PAYEE = :QCK-PAYEE,
              CKFILE.CK_ADR1 = :QCK-ADR1,
              CKFILE.CK_ADR2 = :QCK-ADR2,
              CKFILE.CK_CITY = :QCK-CITY,
              CKFILE.CK_STATE = :QCK-STATE,
              CKFILE.CK_ZIP = :QCK-ZIP,
              CKFILE.CK_REF = :QCK-REF,
              CKFILE.CK_REASON = :QCK-REASON,
              CKFILE.CK_GLNO_1 = :QCK-GLNO-1,
              CKFILE.CK_GLAMT_1 = :QCK-GLAMT-1,
              CKFILE.CK_GLNO_2 = :QCK-GLNO-2,
              CKFILE.CK_GLAMT_2 = :QCK-GLAMT-2,
              CKFILE.CK_GLNO_3 = :QCK-GLNO-3,
              CKFILE.CK_GLAMT_3 = :QCK-GLAMT-3,
              CKFILE.CK_GLNO_4 = :QCK-GLNO-4,
              CKFILE.CK_GLAMT_4 = :QCK-GLAMT-4,
              CKFILE.CK_GLNO_5 = :QCK-GLNO-5,
              CKFILE.CK_GLAMT_5 = :QCK-GLAMT-5,
              CKFILE.CK_VOID = :QCK-VOID,
              CKFILE.CK_PAID = :QCK-PAID,
              CKFILE.CK_CLASS = :QCK-CLASS,
              CKFILE.CK_DLNO = :QCK-DLNO,
              CKFILE.CK_PGM = :QCK-PGM,
              CKFILE.CK_NC_SEQNO = :QCK-NC-SEQNO,
              CKFILE.CK_VOID_DATE =
                 :QCK-VOID-DATE,
              CKFILE.CK_ACCTNO = :QCK-ACCTNO,
              CKFILE.CK_DLRVNO = :QCK-DLRVNO,
              CKFILE.CK_CLEARED_FG = :QCK-CLEARED-FG,
              CKFILE.CK_SSNO = :QCK-SSNO,
              CKFILE.CK_BATCHID = :QCK-BATCHID,
              CKFILE.CK_UNPAID = :QCK-UNPAID,
              CKFILE.CK_CLEARED_DATE = :QCK-CLEARED-DATE,
              CKFILE.CK_TILLNO = :QCK-TILLNO,
              CKFILE.CK_EVENT_CD = :QCK-EVENT-CD,
              CKFILE.CK_PAYEE_NO = :QCK-PAYEE-NO,
              CKFILE.CK_FREE1 = :QCK-FREE1,
              CKFILE.CK_FREE2 = :QCK-FREE2,
              CKFILE.CK_BWPROC = :QCK-BWPROC
             WHERE CKFILE.CK_BRNO = :QCK-BRNO
               AND CKFILE.CK_NO = :QCK-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CK1-FILE.

      *    IF ( IO-FG NOT = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-CK1-FILE.

           MOVE CK-PATH-OWNBR  TO CK-BRNO. 
           PERFORM DELETE-IT.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.
           PERFORM SET-CK-FIELDS.

           EXEC SQL
            DELETE FROM DBO.CKFILE
             WHERE CKFILE.CK_BRNO = :QCK-BRNO
               AND CKFILE.CK_NO = :QCK-NO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-CK1-FILE.

      *-----------------------------------------------------------------
       CLOSE-CK1-FILE.

           PERFORM CLOSE-IT.

           IF ( CK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CK1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CK1-CURSOR-STAT.

