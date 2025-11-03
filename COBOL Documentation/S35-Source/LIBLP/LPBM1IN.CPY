      * COPYMEMBER: LIBLP/LPBM1IN
      * 2024.0219 SET BM-LTOUCH-TIME-HHMM AND BM-LTOUCH-USERID

       LOAD-BM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BM-ALL-DATA
              MOVE BM-ALL-PATH             TO BM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BM-ALL-BASE
              MOVE FILPATH-MACHINE         TO BM-ALL-MACHINE
              MOVE FILPATH-DATA            TO BM-ALL-DATA
              MOVE BM-ALL-PATH             TO BM-PATH.

      *-----------------------------------------------------------------
       OPEN-BM1-FILE.
           PERFORM LOAD-BM1-FILE.
           PERFORM OPEN-IT.
           MOVE BM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BM1-FILE.
           PERFORM START-IT.
           MOVE BM-PATH-OWNBR  TO BM-BRNO.
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY        TO E-KEYX.

           IF ( BM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BM1-FILE.

           IF ( QBM1-WEND-SEQ = ZEROES    ) OR
              ( QBM1-WEND-SEQ NOT NUMERIC )
              MOVE BM-PATH-OWNBR        TO QBM1-WBEG-BRNO
              MOVE BM-PATH-OWNBR        TO QBM1-WEND-BRNO
              MOVE BM-SSNO              TO QBM1-WBEG-SSNO
              MOVE ALL "9"              TO QBM1-WEND-SSNO
              MOVE BM-DATE-REVERSED     TO QBM1-WBEG-DATE-REV
              MOVE ALL "9"              TO QBM1-WEND-DATE-REV
              MOVE BM-SEQ               TO QBM1-WBEG-SEQ
              MOVE ALL "9"              TO QBM1-WEND-SEQ.
 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE BM1_CURSOR CURSOR FOR
            SELECT BMFILE.BM_BRNO,
                   BMFILE.BM_SSNO,
                   BMFILE.BM_DATE_REVERSED,
                   BMFILE.BM_SEQ,
                   CAST(BMFILE.BM_LTOUCH_DATE AS VARCHAR(10)),
                   BMFILE.BM_LTOUCH_TIME_HHMM,
                   BMFILE.BM_LTOUCH_USERID,
                   BMFILE.BM_MEMO,
                   BMFILE.BM_MEMO_TYPE
            FROM DBO.BMFILE
            WHERE   BMFILE.BM_BRNO           = :QBM1-WBEG-BRNO
              AND ( BMFILE.BM_SSNO          >= :QBM1-WBEG-SSNO
              AND   BMFILE.BM_DATE_REVERSED >= :QBM1-WBEG-DATE-REV
              AND   BMFILE.BM_SEQ           >= :QBM1-WBEG-SEQ      )
              AND ( BMFILE.BM_SSNO          <= :QBM1-WEND-SSNO
              AND   BMFILE.BM_DATE_REVERSED <= :QBM1-WEND-DATE-REV
              AND   BMFILE.BM_SEQ           <= :QBM1-WEND-SEQ      )
            ORDER BY BMFILE.BM_BRNO,
                     BMFILE.BM_SSNO,
                     BMFILE.BM_DATE_REVERSED,
                     BMFILE.BM_SEQ
           END-EXEC.

           EXEC SQL
               OPEN BM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBM1-WBEG-BRNO
                          QBM1-WEND-BRNO
                          QBM1-WBEG-SSNO
                          QBM1-WEND-SSNO
                          QBM1-WBEG-DATE-REV
                          QBM1-WEND-DATE-REV
                          QBM1-WBEG-SEQ
                          QBM1-WEND-SEQ.

           MOVE STAT---GOOD TO BM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-BM1-FILE-SCAN.
           PERFORM START-IT.
           MOVE BM-PATH-OWNBR  TO BM-BRNO.
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY TO E-KEYX.

           IF ( BM1-CURSOR-SC-STAT-GOOD )
              PERFORM CLOSE-BM1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO 
      *  !!SUBTRACT!! ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QBM1-WEND-SEQ = ZEROES    ) OR
              ( QBM1-WEND-SEQ NOT NUMERIC )
              MOVE BM-PATH-OWNBR        TO QBM1-WBEG-BRNO
              MOVE BM-PATH-OWNBR        TO QBM1-WEND-BRNO
              MOVE BM-SSNO              TO QBM1-WBEG-SSNO
              MOVE BM-SSNO              TO QBM1-WEND-SSNO
              MOVE BM-DATE-REVERSED     TO QBM1-WBEG-DATE-REV
              MOVE ALL "9"              TO QBM1-WEND-DATE-REV
              MOVE BM-SEQ               TO QBM1-WBEG-SEQ
              MOVE ALL "9"              TO QBM1-WEND-SEQ
      *-   -   -{ SUBTRACT ONE TO WHERE SCAN BEG FIELDS }
      *       SKIP; NOT NEEDED             QBM1-WSBEG-BRNO
      *       SKIP; NOT NEEDED             QBM1-WSBEG-SSNO
      *-   -   -{ MEMO DATE DO NOT ADD IF PRIORITY MEMOS (0) }
      *       IF ( BM-DATE-REVERSED = 0 )
      *          MOVE BM-DATE-REVERSED  TO QBM1-WSBEG-DATE-REV
      *       ELSE
                 COMPUTE QBM1-WSBEG-DATE-REV = BM-DATE-REVERSED + 1.
      *       SKIP; NOT NEEDED             QBM1-WSBEG-SEQ

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE BM1_CURSOR_SC CURSOR FOR
            SELECT BMFILE.BM_BRNO,
                   BMFILE.BM_SSNO,
                   BMFILE.BM_DATE_REVERSED,
                   BMFILE.BM_SEQ,
                   CAST(BMFILE.BM_LTOUCH_DATE AS VARCHAR(10)),
                   BMFILE.BM_LTOUCH_TIME_HHMM,
                   BMFILE.BM_LTOUCH_USERID,
                   BMFILE.BM_MEMO,
                   BMFILE.BM_MEMO_TYPE
            FROM DBO.BMFILE
            WHERE    BMFILE.BM_BRNO           = :QBM1-WBEG-BRNO
              AND (( BMFILE.BM_SSNO           = :QBM1-WBEG-SSNO
              AND    BMFILE.BM_DATE_REVERSED >= :QBM1-WBEG-DATE-REV
              AND    BMFILE.BM_SEQ           >= :QBM1-WBEG-SEQ       )
               OR  ( BMFILE.BM_SSNO           = :QBM1-WBEG-SSNO
              AND    BMFILE.BM_DATE_REVERSED >= :QBM1-WSBEG-DATE-REV
              AND    BMFILE.BM_SEQ           >= 0                    ))
              AND (  BMFILE.BM_SSNO           = :QBM1-WBEG-SSNO
              AND    BMFILE.BM_DATE_REVERSED <= :QBM1-WEND-DATE-REV
              AND    BMFILE.BM_SEQ           <= :QBM1-WEND-SEQ        )
            ORDER BY BMFILE.BM_BRNO,
                     BMFILE.BM_SSNO,
                     BMFILE.BM_DATE_REVERSED,
                     BMFILE.BM_SEQ 
           END-EXEC.

           EXEC SQL
               OPEN BM1_CURSOR_SC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBM1-WBEG-BRNO
                          QBM1-WEND-BRNO
                          QBM1-WBEG-SSNO
                          QBM1-WEND-SSNO
                          QBM1-WBEG-DATE-REV
                          QBM1-WEND-DATE-REV
                          QBM1-WBEG-SEQ
                          QBM1-WEND-SEQ
                          QBM1-WSBEG-DATE-REV.

           MOVE STAT---GOOD TO BM1-CURSOR-SC-STAT.

      *-----------------------------------------------------------------
       START-BM1-FILE-LESS-THAN.
           PERFORM START-IT.
           MOVE BM-PATH-OWNBR  TO BM-BRNO.
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY TO E-KEYX.

           IF ( BM1-CURSOR-LT-STAT-GOOD )
              PERFORM CLOSE-BM1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO 
      *  !!SUBTRACT!! ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QBM1-WEND-SEQ = ZEROES    ) OR
              ( QBM1-WEND-SEQ NOT NUMERIC )
              MOVE BM-PATH-OWNBR        TO QBM1-WBEG-BRNO
              MOVE BM-PATH-OWNBR        TO QBM1-WEND-BRNO
              MOVE BM-SSNO              TO QBM1-WBEG-SSNO
              MOVE BM-SSNO              TO QBM1-WEND-SSNO
              MOVE BM-DATE-REVERSED     TO QBM1-WBEG-DATE-REV
              MOVE BM-DATE-REVERSED     TO QBM1-WEND-DATE-REV
              MOVE BM-SEQ               TO QBM1-WBEG-SEQ
              MOVE BM-SEQ               TO QBM1-WEND-SEQ
      *-   -   -{ SUBTRACT ONE TO WHERE SCAN BEG FIELDS }
      *       SKIP; NOT NEEDED             QBM1-WSBEG-BRNO
      *       SKIP;                        QBM1-WSBEG-SSNO
      *-   -   -{ MEMO DATE DO NOT SUBTRACT IF PRIORITY MEMOS (0) }

              IF ( BM-DATE-REVERSED = 0 )
                 MOVE BM-DATE-REVERSED  TO QBM1-WSBEG-DATE-REV
                 COMPUTE QBM1-WSBEG-SEQ     = BM-SEQ - 1
              ELSE
                 COMPUTE QBM1-WSBEG-DATE-REV = BM-DATE-REVERSED - 1
                 MOVE 999               TO QBM1-WSBEG-SEQ.
      *       SKIP; NOT NEEDED             QBM1-WSBEG-SEQ

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE BM1_CURSOR_LT CURSOR FOR
            SELECT BMFILE.BM_BRNO,
                   BMFILE.BM_SSNO,
                   BMFILE.BM_DATE_REVERSED,
                   BMFILE.BM_SEQ,
                   CAST(BMFILE.BM_LTOUCH_DATE AS VARCHAR(10)),
                   BMFILE.BM_LTOUCH_TIME_HHMM,
                   BMFILE.BM_LTOUCH_USERID,
                   BMFILE.BM_MEMO,
                   BMFILE.BM_MEMO_TYPE
            FROM DBO.BMFILE
            WHERE    BMFILE.BM_BRNO           = :QBM1-WBEG-BRNO
              AND (  BMFILE.BM_SSNO           = :QBM1-WBEG-SSNO
              AND    BMFILE.BM_DATE_REVERSED <= :QBM1-WBEG-DATE-REV
              AND    BMFILE.BM_SEQ            < :QBM1-WBEG-SEQ       )
               OR (  BMFILE.BM_SSNO           = :QBM1-WBEG-SSNO
              AND    BMFILE.BM_DATE_REVERSED <= :QBM1-WSBEG-DATE-REV
              AND    BMFILE.BM_SEQ           <= :QBM1-WSBEG-SEQ      )

            ORDER BY BMFILE.BM_BRNO DESC,
                     BMFILE.BM_SSNO DESC,
                     BMFILE.BM_DATE_REVERSED DESC,
                     BMFILE.BM_SEQ DESC
           END-EXEC.

           EXEC SQL
               OPEN BM1_CURSOR_LT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBM1-WBEG-BRNO
                          QBM1-WEND-BRNO
                          QBM1-WBEG-SSNO
                          QBM1-WEND-SSNO
                          QBM1-WBEG-DATE-REV
                          QBM1-WEND-DATE-REV
                          QBM1-WBEG-SEQ
                          QBM1-WEND-SEQ
                          QBM1-WBEG-DATE-REV.

           MOVE STAT---GOOD TO BM1-CURSOR-LT-STAT.

      *-----------------------------------------------------------------
       READ-BM1-FILE.
           PERFORM READ-IT.
           MOVE BM-PATH-OWNBR  TO BM-BRNO. 
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY        TO E-KEYX.
           INITIALIZE QBM-REC.

           MOVE BM-BRNO          TO QBM-BRNO.
           MOVE BM-SSNO          TO QBM-SSNO.
           MOVE BM-DATE-REVERSED TO QBM-DATE-REVERSED.
           MOVE BM-SEQ           TO QBM-SEQ.

           EXEC SQL
            SELECT BMFILE.BM_BRNO,
                   BMFILE.BM_SSNO,
                   BMFILE.BM_DATE_REVERSED,
                   BMFILE.BM_SEQ,
                   CAST(BMFILE.BM_LTOUCH_DATE AS VARCHAR(10)),
                   BMFILE.BM_LTOUCH_TIME_HHMM,
                   BMFILE.BM_LTOUCH_USERID,
                   BMFILE.BM_MEMO,
                   BMFILE.BM_MEMO_TYPE
            INTO :QBM-REC
            FROM DBO.BMFILE
            WHERE BMFILE.BM_BRNO = :QBM-BRNO
              AND BMFILE.BM_SSNO = :QBM-SSNO
              AND BMFILE.BM_DATE_REVERSED = :QBM-DATE-REVERSED
              AND BMFILE.BM_SEQ = :QBM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BM-FIELDS.

      *-----------------------------------------------------------------
       READ-BM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BM-PATH-OWNBR  TO BM-BRNO.
           MOVE BM-PATH-SQL TO E-FILE.
           MOVE BM1-KEY        TO E-KEYX.
           INITIALIZE QBM-REC.

           IF ( BM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BM1_CURSOR INTO :QBM-REC
              END-EXEC
           ELSE
           IF ( BM1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   FETCH BM1_CURSOR_SC INTO :QBM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BM-FIELDS.

      *-----------------------------------------------------------------
       READ-BM1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE BM-PATH-OWNBR  TO BM-BRNO.
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY TO E-KEYX.
           INITIALIZE QBM-REC.

           IF ( BM1-CURSOR-LT-STAT-GOOD )
              EXEC SQL
                   FETCH BM1_CURSOR_LT INTO :QBM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BM1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-BM-FIELDS.

      *-----------------------------------------------------------------
       WRITE-BM1-FILE.

           MOVE BM-PATH-OWNBR         TO BM-BRNO. 
           ACCEPT BM-LTOUCH-DATE      FROM CENTURY-DATE.
           ACCEPT WS-BM-LTOUCH-TIME   FROM TIME.
           MOVE WS-BM-LTOUCH-HHMM     TO BM-LTOUCH-TIME-HHMM.
           MOVE EXT-CONAME-USER-ID    TO BM-LTOUCH-USERID.
           PERFORM WRITE-IT.
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY        TO E-KEYX.
           PERFORM SET-BM-FIELDS.

           EXEC SQL
            INSERT INTO DBO.BMFILE
             (BMFILE.BM_BRNO,
              BMFILE.BM_SSNO,
              BMFILE.BM_DATE_REVERSED,
              BMFILE.BM_SEQ,
              BMFILE.BM_LTOUCH_DATE,
              BMFILE.BM_LTOUCH_TIME_HHMM,
              BMFILE.BM_LTOUCH_USERID,
              BMFILE.BM_MEMO,
              BMFILE.BM_MEMO_TYPE)
            VALUES
             (:QBM-BRNO,
              :QBM-SSNO,
              :QBM-DATE-REVERSED,
              :QBM-SEQ,
              :QBM-LTOUCH-DATE,
              :QBM-LTOUCH-TIME-HHMM,
              :QBM-LTOUCH-USERID,
              :QBM-MEMO,
              :QBM-MEMO-TYPE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-BM1-FILE.

           MOVE BM-PATH-OWNBR         TO BM-BRNO. 
           ACCEPT BM-LTOUCH-DATE      FROM CENTURY-DATE.
           ACCEPT WS-BM-LTOUCH-TIME   FROM TIME.
           MOVE WS-BM-LTOUCH-HHMM     TO BM-LTOUCH-TIME-HHMM.
           MOVE EXT-CONAME-USER-ID    TO BM-LTOUCH-USERID.
           PERFORM REWRITE-IT.
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY        TO E-KEYX.
           PERFORM SET-BM-FIELDS.

           EXEC SQL
            UPDATE DBO.BMFILE
             SET
              BMFILE.BM_BRNO = :QBM-BRNO,
              BMFILE.BM_SSNO = :QBM-SSNO,
              BMFILE.BM_DATE_REVERSED = :QBM-DATE-REVERSED,
              BMFILE.BM_SEQ = :QBM-SEQ,
              BMFILE.BM_LTOUCH_DATE = :QBM-LTOUCH-DATE,
              BMFILE.BM_LTOUCH_TIME_HHMM = :QBM-LTOUCH-TIME-HHMM,
              BMFILE.BM_LTOUCH_USERID = :QBM-LTOUCH-USERID,
              BMFILE.BM_MEMO = :QBM-MEMO,
              BMFILE.BM_MEMO_TYPE = :QBM-MEMO-TYPE
            WHERE BMFILE.BM_BRNO = :QBM-BRNO
              AND BMFILE.BM_SSNO = :QBM-SSNO
              AND BMFILE.BM_DATE_REVERSED = :QBM-DATE-REVERSED
              AND BMFILE.BM_SEQ = :QBM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-BM1-FILE.

           MOVE BM-PATH-OWNBR TO BM-BRNO. 
           PERFORM DELETE-IT.
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY        TO E-KEYX.
           PERFORM SET-BM-FIELDS.

           EXEC SQL
            DELETE FROM DBO.BMFILE
            WHERE BMFILE.BM_BRNO = :QBM-BRNO
              AND BMFILE.BM_SSNO = :QBM-SSNO
              AND BMFILE.BM_DATE_REVERSED = :QBM-DATE-REVERSED
              AND BMFILE.BM_SEQ = :QBM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BM1-FILE.

      *-----------------------------------------------------------------
       CLOSE-BM1-FILE.

           PERFORM CLOSE-IT.

           IF ( BM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BM1-CURSOR-STAT.

           IF ( BM1-CURSOR-LT-STAT-GOOD )
              EXEC SQL
                   CLOSE BM1_CURSOR_LT
              END-EXEC
              MOVE STAT---BAD TO BM1-CURSOR-LT-STAT.

           IF ( BM1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE BM1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO BM1-CURSOR-SC-STAT.

