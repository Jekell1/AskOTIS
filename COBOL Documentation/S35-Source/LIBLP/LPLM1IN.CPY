      * COPYMEMBER: LIBLP/LPLM1IN
      * 2024.0219 SET LM-LTOUCH-TIME-HHMM AND LM-LTOUCH-USERID
       LOAD-LM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH.

      *-----------------------------------------------------------------
       OPEN-LM1-FILE.
           PERFORM LOAD-LM1-FILE.
           PERFORM OPEN-IT.
           MOVE LM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LM1-FILE.
           PERFORM START-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.

           IF ( LM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LM1-FILE.

           IF ( QLM1-WEND-SEQ = ZEROES    ) OR
              ( QLM1-WEND-SEQ NOT NUMERIC )
              MOVE LM-PATH-OWNBR        TO QLM1-WBEG-BRNO
              MOVE LM-PATH-OWNBR        TO QLM1-WEND-BRNO
              MOVE LM-ACCTNO            TO QLM1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLM1-WEND-ACCTNO
              MOVE LM-DATE-REVERSED     TO QLM1-WBEG-DATE-REV
              MOVE ALL "9"              TO QLM1-WEND-DATE-REV
              MOVE LM-SEQ               TO QLM1-WBEG-SEQ
              MOVE ALL "9"              TO QLM1-WEND-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE LM1_CURSOR CURSOR FOR
            SELECT LMFILE.LM_BRNO,
                   LMFILE.LM_ACCTNO,
                   LMFILE.LM_DATE_REVERSED,
                   LMFILE.LM_SEQ,
                   CAST(LMFILE.LM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LMFILE.LM_LTOUCH_TIME_HHMM,
                   LMFILE.LM_LTOUCH_USERID,
                   LMFILE.LM_MEMO,
                   LMFILE.LM_MEMO_TYPE
            FROM DBO.LMFILE
            WHERE (  LMFILE.LM_BRNO           = :QLM1-WBEG-BRNO )
              AND (  LMFILE.LM_ACCTNO        >= :QLM1-WBEG-ACCTNO
              AND    LMFILE.LM_DATE_REVERSED >= :QLM1-WBEG-DATE-REV
              AND    LMFILE.LM_SEQ           >= :QLM1-WBEG-SEQ       )
              AND (  LMFILE.LM_ACCTNO        <= :QLM1-WEND-ACCTNO
              AND    LMFILE.LM_DATE_REVERSED <= :QLM1-WEND-DATE-REV
              AND    LMFILE.LM_SEQ           <= :QLM1-WEND-SEQ       )
            ORDER BY LMFILE.LM_BRNO,
                     LMFILE.LM_ACCTNO,
                     LMFILE.LM_DATE_REVERSED,
                     LMFILE.LM_SEQ
           END-EXEC.

           EXEC SQL
               OPEN LM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLM1-WBEG-BRNO 
                          QLM1-WEND-BRNO
                          QLM1-WBEG-ACCTNO
                          QLM1-WEND-ACCTNO
                          QLM1-WBEG-DATE-REV
                          QLM1-WEND-DATE-REV
                          QLM1-WBEG-SEQ
                          QLM1-WEND-SEQ.


           MOVE STAT---GOOD TO LM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-LM1-FILE-SCAN.
           PERFORM START-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.

           IF ( LM1-CURSOR-SC-STAT-GOOD )
              PERFORM CLOSE-LM1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QLM1-WEND-SEQ = ZEROES    ) OR
              ( QLM1-WEND-SEQ NOT NUMERIC )
              MOVE LM-PATH-OWNBR        TO QLM1-WBEG-BRNO
              MOVE LM-PATH-OWNBR        TO QLM1-WEND-BRNO
              MOVE LM-ACCTNO            TO QLM1-WBEG-ACCTNO
              MOVE LM-ACCTNO            TO QLM1-WEND-ACCTNO
              MOVE LM-DATE-REVERSED     TO QLM1-WBEG-DATE-REV
              MOVE ALL "9"              TO QLM1-WEND-DATE-REV
              MOVE LM-SEQ               TO QLM1-WBEG-SEQ
              MOVE ALL "9"              TO QLM1-WEND-SEQ
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
      *       SKIP; NOT NEEDED             QLM1-WSBEG-BRNO
      *       SKIP; NOT NEEDED             QLM1-WSBEG-ACCTNO
      *-   -   -{ MEMO DATE DO NOT ADD IF PRIORITY MEMOS (0) }
                 COMPUTE QLM1-WSBEG-DATE-REV = LM-DATE-REVERSED + 1.
      *       SKIP; NOT NEEDED             QLM1-WSBEG-SEQ

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE LM1_CURSOR_SC CURSOR FOR
            SELECT LMFILE.LM_BRNO,
                   LMFILE.LM_ACCTNO,
                   LMFILE.LM_DATE_REVERSED,
                   LMFILE.LM_SEQ,
                   CAST(LMFILE.LM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LMFILE.LM_LTOUCH_TIME_HHMM,
                   LMFILE.LM_LTOUCH_USERID,
                   LMFILE.LM_MEMO,
                   LMFILE.LM_MEMO_TYPE
            FROM DBO.LMFILE
            WHERE  ( LMFILE.LM_BRNO           = :QLM1-WBEG-BRNO )
              AND (( LMFILE.LM_ACCTNO         = :QLM1-WBEG-ACCTNO
              AND    LMFILE.LM_DATE_REVERSED >= :QLM1-WBEG-DATE-REV
              AND    LMFILE.LM_SEQ           >= :QLM1-WBEG-SEQ       )
               OR  ( LMFILE.LM_ACCTNO         = :QLM1-WBEG-ACCTNO
              AND    LMFILE.LM_DATE_REVERSED >= :QLM1-WSBEG-DATE-REV
              AND    LMFILE.LM_SEQ           >= 0                    ))
              AND (  LMFILE.LM_ACCTNO         = :QLM1-WEND-ACCTNO
              AND    LMFILE.LM_DATE_REVERSED <= :QLM1-WEND-DATE-REV
              AND    LMFILE.LM_SEQ           <= :QLM1-WEND-SEQ        )
            ORDER BY LMFILE.LM_BRNO,
                     LMFILE.LM_ACCTNO,
                     LMFILE.LM_DATE_REVERSED,
                     LMFILE.LM_SEQ
           END-EXEC.

           EXEC SQL
               OPEN LM1_CURSOR_SC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLM1-WBEG-BRNO 
                          QLM1-WEND-BRNO
                          QLM1-WBEG-ACCTNO
                          QLM1-WEND-ACCTNO
                          QLM1-WBEG-DATE-REV
                          QLM1-WEND-DATE-REV
                          QLM1-WBEG-SEQ
                          QLM1-WEND-SEQ
                          QLM1-WSBEG-DATE-REV.

           MOVE STAT---GOOD TO LM1-CURSOR-SC-STAT.

      *-----------------------------------------------------------------
       START-LM1-FILE-LESS-THAN.
           PERFORM START-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.

           IF ( LM1-CURSOR-LT-STAT-GOOD )
              PERFORM CLOSE-LM1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO
      *  !!SUBTRACT!! ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QLM1-WEND-SEQ = ZEROES    ) OR
              ( QLM1-WEND-SEQ NOT NUMERIC )
              MOVE LM-PATH-OWNBR        TO QLM1-WBEG-BRNO
              MOVE LM-PATH-OWNBR        TO QLM1-WEND-BRNO
              MOVE LM-ACCTNO            TO QLM1-WBEG-ACCTNO
              MOVE LM-ACCTNO            TO QLM1-WEND-ACCTNO
              MOVE LM-DATE-REVERSED     TO QLM1-WBEG-DATE-REV
              MOVE LM-DATE-REVERSED     TO QLM1-WEND-DATE-REV
              MOVE LM-SEQ               TO QLM1-WBEG-SEQ
              MOVE LM-SEQ               TO QLM1-WEND-SEQ
      *-   -   -{ SUBTRACT ONE TO WHERE SCAN BEG FIELDS }
      *       SKIP; NOT NEEDED             QLM1-WSBEG-BRNO
      *       SKIP; NOT NEEDED             QLM1-WSBEG-ACCTNO
      *-   -   -{ MEMO DATE DO NOT SUBTRACT IF PRIORITY MEMOS (0) }
              IF ( LM-DATE-REVERSED = 0 )
                 MOVE LM-DATE-REVERSED  TO QLM1-WSBEG-DATE-REV
                 COMPUTE QLM1-WSBEG-SEQ      = LM-SEQ           - 1
              ELSE
                 COMPUTE QLM1-WSBEG-DATE-REV = LM-DATE-REVERSED - 1 
                 MOVE 999               TO QLM1-WSBEG-SEQ.
      *       SKIP; NOT NEEDED             QLM1-WSBEG-SEQ

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE LM1_CURSOR_LT CURSOR FOR
            SELECT LMFILE.LM_BRNO,
                   LMFILE.LM_ACCTNO,
                   LMFILE.LM_DATE_REVERSED,
                   LMFILE.LM_SEQ,
                   CAST(LMFILE.LM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LMFILE.LM_LTOUCH_TIME_HHMM,
                   LMFILE.LM_LTOUCH_USERID,
                   LMFILE.LM_MEMO,
                   LMFILE.LM_MEMO_TYPE
            FROM DBO.LMFILE
            WHERE  ( LMFILE.LM_BRNO           = :QLM1-WBEG-BRNO )
              AND (( LMFILE.LM_ACCTNO         = :QLM1-WBEG-ACCTNO
              AND    LMFILE.LM_DATE_REVERSED <= :QLM1-WBEG-DATE-REV
              AND    LMFILE.LM_SEQ            < :QLM1-WBEG-SEQ       )
               OR  ( LMFILE.LM_ACCTNO         = :QLM1-WBEG-ACCTNO
              AND    LMFILE.LM_DATE_REVERSED <= :QLM1-WSBEG-DATE-REV
              AND    LMFILE.LM_SEQ           <= :QLM1-WSBEG-SEQ      ))
            ORDER BY LMFILE.LM_BRNO DESC,
                     LMFILE.LM_ACCTNO DESC,
                     LMFILE.LM_DATE_REVERSED DESC,
                     LMFILE.LM_SEQ DESC
           END-EXEC.

           EXEC SQL
               OPEN LM1_CURSOR_LT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLM1-WBEG-BRNO 
                          QLM1-WEND-BRNO
                          QLM1-WBEG-ACCTNO
                          QLM1-WEND-ACCTNO
                          QLM1-WBEG-DATE-REV
                          QLM1-WEND-DATE-REV
                          QLM1-WBEG-SEQ
                          QLM1-WEND-SEQ
                          QLM1-WSBEG-DATE-REV.

           MOVE STAT---GOOD TO LM1-CURSOR-LT-STAT.

      *-----------------------------------------------------------------
       READ-LM1-FILE.
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO. 
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           MOVE LM-BRNO          TO QLM-BRNO.
           MOVE LM-ACCTNO        TO QLM-ACCTNO.
           MOVE LM-DATE-REVERSED TO QLM-DATE-REVERSED.
           MOVE LM-SEQ           TO QLM-SEQ.

           EXEC SQL
            SELECT LMFILE.LM_BRNO,
                   LMFILE.LM_ACCTNO,
                   LMFILE.LM_DATE_REVERSED,
                   LMFILE.LM_SEQ,
                   CAST(LMFILE.LM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LMFILE.LM_LTOUCH_TIME_HHMM,
                   LMFILE.LM_LTOUCH_USERID,
                   LMFILE.LM_MEMO,
                   LMFILE.LM_MEMO_TYPE
            INTO :QLM-REC
            FROM DBO.LMFILE
            WHERE LMFILE.LM_BRNO = :QLM-BRNO
              AND LMFILE.LM_ACCTNO = :QLM-ACCTNO
              AND LMFILE.LM_DATE_REVERSED = :QLM-DATE-REVERSED
              AND LMFILE.LM_SEQ = :QLM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS.

      *-----------------------------------------------------------------
       READ-LM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LM1_CURSOR INTO :QLM-REC
              END-EXEC
           ELSE
           IF ( LM1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   FETCH LM1_CURSOR_SC INTO :QLM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS.

      *-----------------------------------------------------------------
       READ-LM1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           IF ( LM1-CURSOR-LT-STAT-GOOD )
              EXEC SQL
                   FETCH LM1_CURSOR_LT INTO :QLM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LM1-FILE.

           MOVE LM-PATH-OWNBR         TO LM-BRNO.
           ACCEPT LM-LTOUCH-DATE      FROM CENTURY-DATE.
           ACCEPT WS-LM-LTOUCH-TIME   FROM TIME.
           MOVE WS-LM-LTOUCH-HHMM     TO LM-LTOUCH-TIME-HHMM.
           MOVE EXT-CONAME-USER-ID    TO LM-LTOUCH-USERID.
           PERFORM WRITE-IT.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           PERFORM SET-LM-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LMFILE
             (LMFILE.LM_BRNO,
              LMFILE.LM_ACCTNO,
              LMFILE.LM_DATE_REVERSED,
              LMFILE.LM_SEQ,
              LMFILE.LM_LTOUCH_DATE,
              LMFILE.LM_LTOUCH_TIME_HHMM,
              LMFILE.LM_LTOUCH_USERID,
              LMFILE.LM_MEMO,
              LMFILE.LM_MEMO_TYPE)
            VALUES
             (:QLM-BRNO,
              :QLM-ACCTNO,
              :QLM-DATE-REVERSED,
              :QLM-SEQ,
              :QLM-LTOUCH-DATE,
              :QLM-LTOUCH-TIME-HHMM,
              :QLM-LTOUCH-USERID,
              :QLM-MEMO,
              :QLM-MEMO-TYPE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LM1-FILE.

           MOVE LM-PATH-OWNBR         TO LM-BRNO.
           ACCEPT LM-LTOUCH-DATE      FROM CENTURY-DATE.
           ACCEPT WS-LM-LTOUCH-TIME   FROM TIME.
           MOVE WS-LM-LTOUCH-HHMM     TO LM-LTOUCH-TIME-HHMM.
           MOVE EXT-CONAME-USER-ID    TO LM-LTOUCH-USERID.
           PERFORM REWRITE-IT.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           PERFORM SET-LM-FIELDS.

           EXEC SQL
            UPDATE DBO.LMFILE
             SET
              LMFILE.LM_BRNO = :QLM-BRNO,
              LMFILE.LM_ACCTNO = :QLM-ACCTNO,
              LMFILE.LM_DATE_REVERSED = :QLM-DATE-REVERSED,
              LMFILE.LM_SEQ = :QLM-SEQ,
              LMFILE.LM_LTOUCH_DATE = :QLM-LTOUCH-DATE,
              LMFILE.LM_LTOUCH_TIME_HHMM = :QLM-LTOUCH-TIME-HHMM,
              LMFILE.LM_LTOUCH_USERID = :QLM-LTOUCH-USERID,
              LMFILE.LM_MEMO = :QLM-MEMO,
              LMFILE.LM_MEMO_TYPE = :QLM-MEMO-TYPE
             WHERE LMFILE.LM_BRNO = :QLM-BRNO
               AND LMFILE.LM_ACCTNO = :QLM-ACCTNO
               AND LMFILE.LM_DATE_REVERSED = :QLM-DATE-REVERSED
               AND LMFILE.LM_SEQ = :QLM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LM1-FILE.

           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           PERFORM DELETE-IT.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           PERFORM SET-LM-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LMFILE
             WHERE LMFILE.LM_BRNO = :QLM-BRNO
               AND LMFILE.LM_ACCTNO = :QLM-ACCTNO
               AND LMFILE.LM_DATE_REVERSED = :QLM-DATE-REVERSED
               AND LMFILE.LM_SEQ = :QLM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LM1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LM1-FILE.

           PERFORM CLOSE-IT.

           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LM1-CURSOR-STAT.

           IF ( LM1-CURSOR-LT-STAT-GOOD )
              EXEC SQL
                   CLOSE LM1_CURSOR_LT
              END-EXEC
              MOVE STAT---BAD TO LM1-CURSOR-LT-STAT.

           IF ( LM1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE LM1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO LM1-CURSOR-SC-STAT.

