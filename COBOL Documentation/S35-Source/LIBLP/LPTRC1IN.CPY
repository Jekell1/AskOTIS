      * COPYMEMBER: LIBLP/LPTRC1IN
       LOAD-TRC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRC-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRC-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH.

      *-----------------------------------------------------------------
       OPEN-TRC1-FILE.
           PERFORM LOAD-TRC1-FILE.
           PERFORM OPEN-IT.
           MOVE TRC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRC1-FILE.
           PERFORM START-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.

           IF ( TRC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRC1-FILE.

           IF ( QTRC1-WEND-SEQ = ZEROES    ) OR
              ( QTRC1-WEND-SEQ NOT NUMERIC )
              MOVE TRC-PATH-OWNBR        TO QTRC1-WBEG-BRNO
              MOVE TRC-PATH-OWNBR        TO QTRC1-WEND-BRNO
              MOVE TC-KEY-DATE           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRC1-WBEG-KEY-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRC1-WEND-KEY-DATE
              MOVE TC-CKNO              TO QTRC1-WBEG-CKNO
              MOVE TC-CLASS             TO QTRC1-WBEG-CLASS
              MOVE TC-SEQ               TO QTRC1-WBEG-SEQ
              MOVE ALL "9"              TO QTRC1-WEND-CKNO
                                           QTRC1-WEND-CLASS
                                           QTRC1-WEND-SEQ.

           EXEC SQL
            DECLARE TRC1_CURSOR CURSOR FOR
            SELECT TRCFILE.TC_BRNO,
                   CAST(TRCFILE.TC_KEY_DATE
                                                  AS VARCHAR(10)),
                   TRCFILE.TC_CKNO,
                   TRCFILE.TC_CLASS,
                   TRCFILE.TC_SEQ,
                   CAST(TRCFILE.TC_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRCFILE.TC_TYPE,
                   CAST(TRCFILE.TC_DATE AS VARCHAR(10)),
                   TRCFILE.TC_AMT,
                   TRCFILE.TC_PAYEE,
                   TRCFILE.TC_REF,
                   TRCFILE.TC_REASON,
                   TRCFILE.TC_GLNO_1,
                   TRCFILE.TC_GLAMT_1,
                   TRCFILE.TC_GLNO_2,
                   TRCFILE.TC_GLAMT_2,
                   TRCFILE.TC_GLNO_3,
                   TRCFILE.TC_GLAMT_3,
                   TRCFILE.TC_GLNO_4,
                   TRCFILE.TC_GLAMT_4,
                   TRCFILE.TC_GLNO_5,
                   TRCFILE.TC_GLAMT_5,
                   TRCFILE.TC_VOID,
                   TRCFILE.TC_ACCTNO,
                   TRCFILE.TC_PAID,
                   TRCFILE.TC_PGM,
                   TRCFILE.TC_DLRVNO,
                   TRCFILE.TC_SPRCLASS,
                   TRCFILE.TC_DLNO,
                   TRCFILE.TC_USERID,
                   CAST(TRCFILE.TC_POSTDATE
                                                  AS VARCHAR(10)),
                   TRCFILE.TC_POSTTIME,
                   TRCFILE.TC_TILLNO,
                   TRCFILE.TC_ADR1,
                   TRCFILE.TC_ADR2,
                   TRCFILE.TC_CITY,
                   TRCFILE.TC_STATE,
                   TRCFILE.TC_ZIP,
                   TRCFILE.TC_PAYEE_NO,
                   TRCFILE.TC_ACH_FG,
                   TRCFILE.TC_EXTRACT_FG,
                   TRCFILE.TC_DEBIT_REFCD
            FROM DBO.TRCFILE
            WHERE TRCFILE.TC_BRNO  = :QTRC1-WBEG-BRNO
              AND TRCFILE.TC_KEY_DATE >= :QTRC1-WBEG-KEY-DATE 
              AND TRCFILE.TC_KEY_DATE <= :QTRC1-WEND-KEY-DATE
              AND TRCFILE.TC_CKNO >= :QTRC1-WBEG-CKNO
              AND TRCFILE.TC_CKNO <= :QTRC1-WEND-CKNO
              AND TRCFILE.TC_CLASS >= :QTRC1-WBEG-CLASS
              AND TRCFILE.TC_CLASS <= :QTRC1-WEND-CLASS
              AND TRCFILE.TC_SEQ >= :QTRC1-WBEG-SEQ
              AND TRCFILE.TC_SEQ <= :QTRC1-WEND-SEQ
            ORDER BY TRCFILE.TC_BRNO,
                     TRCFILE.TC_KEY_DATE,
                     TRCFILE.TC_CKNO,
                     TRCFILE.TC_CLASS,
                     TRCFILE.TC_SEQ
           END-EXEC.

           EXEC SQL
               OPEN TRC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRC1-WBEG-BRNO
                          QTRC1-WEND-BRNO
                          QTRC1-WBEG-CKNO
                          QTRC1-WEND-CKNO
                          QTRC1-WBEG-CLASS
                          QTRC1-WEND-CLASS
                          QTRC1-WBEG-SEQ
                          QTRC1-WEND-SEQ.
           MOVE SPACES TO QTRC1-WBEG-KEY-DATE
                          QTRC1-WEND-KEY-DATE.

           MOVE STAT---GOOD TO TRC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRC1-FILE.
           PERFORM READ-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO. 
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           INITIALIZE QTRC-REC.

           MOVE TC-BRNO       TO QTC-BRNO.

           MOVE TC-KEY-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTC-KEY-DATE.

           MOVE TC-CKNO  TO QTC-CKNO.
           MOVE TC-CLASS TO QTC-CLASS.
           MOVE TC-SEQ   TO QTC-SEQ.

           EXEC SQL
            SELECT TRCFILE.TC_BRNO,
                   CAST(TRCFILE.TC_KEY_DATE
                                                  AS VARCHAR(10)),
                   TRCFILE.TC_CKNO,
                   TRCFILE.TC_CLASS,
                   TRCFILE.TC_SEQ,
                   CAST(TRCFILE.TC_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRCFILE.TC_TYPE,
                   CAST(TRCFILE.TC_DATE AS VARCHAR(10)),
                   TRCFILE.TC_AMT,
                   TRCFILE.TC_PAYEE,
                   TRCFILE.TC_REF,
                   TRCFILE.TC_REASON,
                   TRCFILE.TC_GLNO_1,
                   TRCFILE.TC_GLAMT_1,
                   TRCFILE.TC_GLNO_2,
                   TRCFILE.TC_GLAMT_2,
                   TRCFILE.TC_GLNO_3,
                   TRCFILE.TC_GLAMT_3,
                   TRCFILE.TC_GLNO_4,
                   TRCFILE.TC_GLAMT_4,
                   TRCFILE.TC_GLNO_5,
                   TRCFILE.TC_GLAMT_5,
                   TRCFILE.TC_VOID,
                   TRCFILE.TC_ACCTNO,
                   TRCFILE.TC_PAID,
                   TRCFILE.TC_PGM,
                   TRCFILE.TC_DLRVNO,
                   TRCFILE.TC_SPRCLASS,
                   TRCFILE.TC_DLNO,
                   TRCFILE.TC_USERID,
                   CAST(TRCFILE.TC_POSTDATE
                                                  AS VARCHAR(10)),
                   TRCFILE.TC_POSTTIME,
                   TRCFILE.TC_TILLNO,
                   TRCFILE.TC_ADR1,
                   TRCFILE.TC_ADR2,
                   TRCFILE.TC_CITY,
                   TRCFILE.TC_STATE,
                   TRCFILE.TC_ZIP,
                   TRCFILE.TC_PAYEE_NO,
                   TRCFILE.TC_ACH_FG,
                   TRCFILE.TC_EXTRACT_FG,
                   TRCFILE.TC_DEBIT_REFCD
            INTO :QTRC-REC
            FROM DBO.TRCFILE
            WHERE TRCFILE.TC_BRNO = :QTC-BRNO
              AND TRCFILE.TC_KEY_DATE = :QTC-KEY-DATE
              AND TRCFILE.TC_CKNO = :QTC-CKNO
              AND TRCFILE.TC_CLASS = :QTC-CLASS
              AND TRCFILE.TC_SEQ = :QTC-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRC-FIELDS.

      *-----------------------------------------------------------------
       READ-TRC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           INITIALIZE QTRC-REC.

           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRC1_CURSOR INTO :QTRC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRC-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRC1-FILE.

           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           ACCEPT TC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           PERFORM SET-TRC-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRCFILE
             (TRCFILE.TC_BRNO,
              TRCFILE.TC_KEY_DATE,
              TRCFILE.TC_CKNO,
              TRCFILE.TC_CLASS,
              TRCFILE.TC_SEQ,
              TRCFILE.TC_LTOUCH_DATE,
              TRCFILE.TC_TYPE,
              TRCFILE.TC_DATE,
              TRCFILE.TC_AMT,
              TRCFILE.TC_PAYEE,
              TRCFILE.TC_REF,
              TRCFILE.TC_REASON,
              TRCFILE.TC_GLNO_1,
              TRCFILE.TC_GLAMT_1,
              TRCFILE.TC_GLNO_2,
              TRCFILE.TC_GLAMT_2,
              TRCFILE.TC_GLNO_3,
              TRCFILE.TC_GLAMT_3,
              TRCFILE.TC_GLNO_4,
              TRCFILE.TC_GLAMT_4,
              TRCFILE.TC_GLNO_5,
              TRCFILE.TC_GLAMT_5,
              TRCFILE.TC_VOID,
              TRCFILE.TC_ACCTNO,
              TRCFILE.TC_PAID,
              TRCFILE.TC_PGM,
              TRCFILE.TC_DLRVNO,
              TRCFILE.TC_SPRCLASS,
              TRCFILE.TC_DLNO,
              TRCFILE.TC_USERID,
              TRCFILE.TC_POSTDATE,
              TRCFILE.TC_POSTTIME,
              TRCFILE.TC_TILLNO,
              TRCFILE.TC_ADR1,
              TRCFILE.TC_ADR2,
              TRCFILE.TC_CITY,
              TRCFILE.TC_STATE,
              TRCFILE.TC_ZIP,
              TRCFILE.TC_PAYEE_NO,
              TRCFILE.TC_ACH_FG,
              TRCFILE.TC_EXTRACT_FG,
              TRCFILE.TC_DEBIT_REFCD)
            VALUES
             (:QTC-BRNO,
              :QTC-KEY-DATE,
              :QTC-CKNO,
              :QTC-CLASS,
              :QTC-SEQ,
              :QTC-LTOUCH-DATE,
              :QTC-TYPE,
              :QTC-DATE,
              :QTC-AMT,
              :QTC-PAYEE,
              :QTC-REF,
              :QTC-REASON,
              :QTC-GLNO-1,
              :QTC-GLAMT-1,
              :QTC-GLNO-2,
              :QTC-GLAMT-2,
              :QTC-GLNO-3,
              :QTC-GLAMT-3,
              :QTC-GLNO-4,
              :QTC-GLAMT-4,
              :QTC-GLNO-5,
              :QTC-GLAMT-5,
              :QTC-VOID,
              :QTC-ACCTNO,
              :QTC-PAID,
              :QTC-PGM,
              :QTC-DLRVNO,
              :QTC-SPRCLASS,
              :QTC-DLNO,
              :QTC-USERID,
              :QTC-POSTDATE,
              :QTC-POSTTIME,
              :QTC-TILLNO,
              :QTC-ADR1,
              :QTC-ADR2,
              :QTC-CITY,
              :QTC-STATE,
              :QTC-ZIP,
              :QTC-PAYEE-NO,
              :QTC-ACH-FG,
              :QTC-EXTRACT-FG,
              :QTC-DEBIT-REFCD)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRC1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRC1-FILE.

           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           ACCEPT TC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           PERFORM SET-TRC-FIELDS.

           EXEC SQL
            UPDATE DBO.TRCFILE
             SET
              TRCFILE.TC_BRNO = :QTC-BRNO,
              TRCFILE.TC_KEY_DATE =
                 :QTC-KEY-DATE,
              TRCFILE.TC_CKNO = :QTC-CKNO,
              TRCFILE.TC_CLASS = :QTC-CLASS,
              TRCFILE.TC_SEQ = :QTC-SEQ,
              TRCFILE.TC_LTOUCH_DATE =
                 :QTC-LTOUCH-DATE,
              TRCFILE.TC_TYPE = :QTC-TYPE,
              TRCFILE.TC_DATE =
                 :QTC-DATE,
              TRCFILE.TC_AMT = :QTC-AMT,
              TRCFILE.TC_PAYEE = :QTC-PAYEE,
              TRCFILE.TC_REF = :QTC-REF,
              TRCFILE.TC_REASON = :QTC-REASON,
              TRCFILE.TC_GLNO_1 = :QTC-GLNO-1,
              TRCFILE.TC_GLAMT_1 = :QTC-GLAMT-1,
              TRCFILE.TC_GLNO_2 = :QTC-GLNO-2,
              TRCFILE.TC_GLAMT_2 = :QTC-GLAMT-2,
              TRCFILE.TC_GLNO_3 = :QTC-GLNO-3,
              TRCFILE.TC_GLAMT_3 = :QTC-GLAMT-3,
              TRCFILE.TC_GLNO_4 = :QTC-GLNO-4,
              TRCFILE.TC_GLAMT_4 = :QTC-GLAMT-4,
              TRCFILE.TC_GLNO_5 = :QTC-GLNO-5,
              TRCFILE.TC_GLAMT_5 = :QTC-GLAMT-5,
              TRCFILE.TC_VOID = :QTC-VOID,
              TRCFILE.TC_ACCTNO = :QTC-ACCTNO,
              TRCFILE.TC_PAID = :QTC-PAID,
              TRCFILE.TC_PGM = :QTC-PGM,
              TRCFILE.TC_DLRVNO = :QTC-DLRVNO,
              TRCFILE.TC_SPRCLASS = :QTC-SPRCLASS,
              TRCFILE.TC_DLNO = :QTC-DLNO,
              TRCFILE.TC_USERID = :QTC-USERID,
              TRCFILE.TC_POSTDATE = :QTC-POSTDATE,
              TRCFILE.TC_POSTTIME = :QTC-POSTTIME,
              TRCFILE.TC_TILLNO = :QTC-TILLNO,
              TRCFILE.TC_ADR1 = :QTC-ADR1,
              TRCFILE.TC_ADR2 = :QTC-ADR2,
              TRCFILE.TC_CITY = :QTC-CITY,
              TRCFILE.TC_STATE = :QTC-STATE,
              TRCFILE.TC_ZIP = :QTC-ZIP,
              TRCFILE.TC_PAYEE_NO = :QTC-PAYEE-NO,
              TRCFILE.TC_ACH_FG = :QTC-ACH-FG,
              TRCFILE.TC_EXTRACT_FG = :QTC-EXTRACT-FG,
              TRCFILE.TC_DEBIT_REFCD = :QTC-DEBIT-REFCD
            WHERE TRCFILE.TC_BRNO = :QTC-BRNO
              AND TRCFILE.TC_KEY_DATE = :QTC-KEY-DATE
              AND TRCFILE.TC_CKNO = :QTC-CKNO
              AND TRCFILE.TC_CLASS = :QTC-CLASS
              AND TRCFILE.TC_SEQ = :QTC-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRC1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRC1-FILE.

           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           PERFORM DELETE-IT.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           PERFORM SET-TRC-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRCFILE
            WHERE TRCFILE.TC_BRNO = :QTC-BRNO
              AND TRCFILE.TC_KEY_DATE = :QTC-KEY-DATE
              AND TRCFILE.TC_CKNO = :QTC-CKNO
              AND TRCFILE.TC_CLASS = :QTC-CLASS
              AND TRCFILE.TC_SEQ = :QTC-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRC1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRC1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRC1-CURSOR-STAT.

