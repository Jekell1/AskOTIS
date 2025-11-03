      * COPYMEMBER: LIBLP/LPDM1IN
      * 2024.0219 SET DM-LTOUCH-TIME-HHMM AND DM-LTOUCH-USERID
       LOAD-DM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( DM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( DM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO DM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO DM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO DM-ALL-DATA
              MOVE DM-ALL-PATH             TO DM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( DM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO DM-ALL-BASE
              MOVE FILPATH-MACHINE         TO DM-ALL-MACHINE
              MOVE FILPATH-DATA            TO DM-ALL-DATA
              MOVE DM-ALL-PATH             TO DM-PATH.

      *-----------------------------------------------------------------
       OPEN-DM1-FILE.
           PERFORM LOAD-DM1-FILE.
           PERFORM OPEN-IT.
           MOVE DM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DM1-FILE.
           PERFORM START-IT.
           MOVE DM-PATH-OWNBR  TO DM-BRNO.
           MOVE DM-PATH-SQL    TO E-FILE.
           MOVE DM1-KEY        TO E-KEYX.

           IF ( DM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DM1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QDM1-WEND-SEQ = ZEROES    ) OR
              ( QDM1-WEND-SEQ NOT NUMERIC )
              MOVE DM-PATH-OWNBR        TO QDM1-WBEG-BRNO
              MOVE DM-PATH-OWNBR        TO QDM1-WEND-BRNO
              MOVE DM-DL-DLNO           TO QDM1-WBEG-DLNO
              MOVE ALL "9"              TO QDM1-WEND-DLNO
              MOVE DM-DATE-REVERSED     TO QDM1-WBEG-DATE-REV
              MOVE ALL "9"              TO QDM1-WEND-DATE-REV
              MOVE DM-SEQ               TO QDM1-WBEG-SEQ
              MOVE ALL "9"              TO QDM1-WEND-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE DM1_CURSOR CURSOR FOR
            SELECT DMFILE.DM_BRNO,
                   DMFILE.DM_DL_DLNO,
                   DMFILE.DM_DATE_REVERSED,
                   DMFILE.DM_SEQ,
                   CAST(DMFILE.DM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   DMFILE.DM_LTOUCH_TIME_HHMM,
                   DMFILE.DM_LTOUCH_USERID,
                   DMFILE.DM_MEMO,
                   DMFILE.DM_MEMO_TYPE
            FROM DBO.DMFILE
            WHERE ( DMFILE.DM_BRNO           = :QDM1-WBEG-BRNO )
              AND ( DMFILE.DM_DL_DLNO       >= :QDM1-WBEG-DLNO
              AND   DMFILE.DM_DATE_REVERSED >= :QDM1-WBEG-DATE-REV
              AND   DMFILE.DM_SEQ           >= :QDM1-WBEG-SEQ       )
              AND ( DMFILE.DM_DL_DLNO       <= :QDM1-WEND-DLNO
              AND   DMFILE.DM_DATE_REVERSED <= :QDM1-WEND-DATE-REV
              AND   DMFILE.DM_SEQ           <= :QDM1-WEND-SEQ       )
            ORDER BY DMFILE.DM_BRNO,
                     DMFILE.DM_DL_DLNO,
                     DMFILE.DM_DATE_REVERSED,
                     DMFILE.DM_SEQ
           END-EXEC.

           EXEC SQL
               OPEN DM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDM1-WBEG-BRNO
                          QDM1-WEND-BRNO
                          QDM1-WBEG-DLNO
                          QDM1-WEND-DLNO
                          QDM1-WBEG-DATE-REV
                          QDM1-WEND-DATE-REV
                          QDM1-WBEG-SEQ
                          QDM1-WEND-SEQ.

           MOVE STAT---GOOD TO DM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-DM1-FILE-SCAN.
           PERFORM START-IT.
           MOVE DM-PATH-OWNBR  TO DM-BRNO.
           MOVE DM-PATH-SQL    TO E-FILE.
           MOVE DM1-KEY        TO E-KEYX.

           IF ( DM1-CURSOR-SC-STAT-GOOD )
              PERFORM CLOSE-DM1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QDM1-WEND-SEQ = ZEROES    ) OR
              ( QDM1-WEND-SEQ NOT NUMERIC )
              MOVE DM-PATH-OWNBR        TO QDM1-WBEG-BRNO
              MOVE DM-PATH-OWNBR        TO QDM1-WEND-BRNO
              MOVE DM-DL-DLNO           TO QDM1-WBEG-DLNO
              MOVE DM-DL-DLNO           TO QDM1-WEND-DLNO
              MOVE DM-DATE-REVERSED     TO QDM1-WBEG-DATE-REV
              MOVE ALL "9"              TO QDM1-WEND-DATE-REV
              MOVE DM-SEQ               TO QDM1-WBEG-SEQ
              MOVE ALL "9"              TO QDM1-WEND-SEQ
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
      *       SKIP; NOT NEEDED             QDM1-WSBEG-BRNO
      *       SKIP; NOT NEEDED             QDM1-WSBEG-DLNO
      *-   -   -{ MEMO DATE DO NOT ADD IF PRIORITY MEMOS (0) }
      *       IF ( DM-DATE-REVERSED = 0 )
      *          MOVE DM-DATE-REVERSED  TO QDM1-WSBEG-DATE-REV
      *       ELSE
                 COMPUTE QDM1-WSBEG-DATE-REV = DM-DATE-REVERSED + 1.
      *       SKIP; NOT NEEDED             QDM1-WSBEG-SEQ

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE DM1_CURSOR_SC CURSOR FOR
            SELECT DMFILE.DM_BRNO,
                   DMFILE.DM_DL_DLNO,
                   DMFILE.DM_DATE_REVERSED,
                   DMFILE.DM_SEQ,
                   CAST(DMFILE.DM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   DMFILE.DM_LTOUCH_TIME_HHMM,
                   DMFILE.DM_LTOUCH_USERID,
                   DMFILE.DM_MEMO,
                   DMFILE.DM_MEMO_TYPE
            FROM DBO.DMFILE
            WHERE  ( DMFILE.DM_BRNO           = :QDM1-WBEG-BRNO )
              AND (( DMFILE.DM_DL_DLNO        = :QDM1-WBEG-DLNO
              AND    DMFILE.DM_DATE_REVERSED >= :QDM1-WBEG-DATE-REV
              AND    DMFILE.DM_SEQ           >= :QDM1-WBEG-SEQ       )
               
               OR  ( DMFILE.DM_DL_DLNO        = :QDM1-WBEG-DLNO
              AND    DMFILE.DM_DATE_REVERSED >= :QDM1-WSBEG-DATE-REV
              AND    DMFILE.DM_SEQ           >= 0                    ))
              
              AND (  DMFILE.DM_DL_DLNO        = :QDM1-WEND-DLNO
              AND    DMFILE.DM_DATE_REVERSED <= :QDM1-WEND-DATE-REV
              AND    DMFILE.DM_SEQ           <= :QDM1-WEND-SEQ       )
            ORDER BY DMFILE.DM_BRNO,
                     DMFILE.DM_DL_DLNO,
                     DMFILE.DM_DATE_REVERSED,
                     DMFILE.DM_SEQ
           END-EXEC.

           EXEC SQL
               OPEN DM1_CURSOR_SC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDM1-WBEG-BRNO
                          QDM1-WEND-BRNO
                          QDM1-WBEG-DLNO
                          QDM1-WEND-DLNO
                          QDM1-WBEG-DATE-REV
                          QDM1-WEND-DATE-REV
                          QDM1-WBEG-SEQ
                          QDM1-WEND-SEQ
                          QDM1-WBEG-DATE-REV.

           MOVE STAT---GOOD TO DM1-CURSOR-SC-STAT.

      *-----------------------------------------------------------------
       START-DM1-FILE-LESS-THAN.
           PERFORM START-IT.
           MOVE DM-PATH-OWNBR  TO DM-BRNO.
           MOVE DM-PATH-SQL    TO E-FILE.
           MOVE DM1-KEY        TO E-KEYX.

           IF ( DM1-CURSOR-LT-STAT-GOOD )
              PERFORM CLOSE-DM1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO 
      *  !!SUBTRACT!! ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QDM1-WEND-SEQ = ZEROES    ) OR
              ( QDM1-WEND-SEQ NOT NUMERIC )
              MOVE DM-PATH-OWNBR        TO QDM1-WBEG-BRNO
              MOVE DM-PATH-OWNBR        TO QDM1-WEND-BRNO
              MOVE DM-DL-DLNO           TO QDM1-WBEG-DLNO
              MOVE DM-DL-DLNO           TO QDM1-WEND-DLNO
              MOVE DM-DATE-REVERSED     TO QDM1-WBEG-DATE-REV
              MOVE DM-DATE-REVERSED     TO QDM1-WEND-DATE-REV
              MOVE DM-SEQ               TO QDM1-WBEG-SEQ
              MOVE DM-SEQ               TO QDM1-WEND-SEQ.
      *-   -   -{ SUBTRACT ONE TO WHERE SCAN BEG FIELDS }
      *       SKIP; NOT NEEDED             QDM1-WSBEG-BRNO
      *       SKIP; NOT NEEDED             QDM1-WSBEG-DLNO
      *-   -   -{ MEMO DATE DO NOT SUBTRACT IF PRIORITY MEMOS (0) }
              IF ( DM-DATE-REVERSED = 0 )
                 MOVE DM-DATE-REVERSED  TO QDM1-WSBEG-DATE-REV
                 COMPUTE QDM1-WSBEG-SEQ      = DM-SEQ           - 1
              ELSE
                 COMPUTE QDM1-WSBEG-DATE-REV = DM-DATE-REVERSED - 1 
                 MOVE 999               TO QDM1-WSBEG-SEQ.
      *       SKIP; NOT NEEDED             QDM1-WSBEG-SEQ

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE DM1_CURSOR_LT CURSOR FOR
            SELECT DMFILE.DM_BRNO,
                   DMFILE.DM_DL_DLNO,
                   DMFILE.DM_DATE_REVERSED,
                   DMFILE.DM_SEQ,
                   CAST(DMFILE.DM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   DMFILE.DM_LTOUCH_TIME_HHMM,
                   DMFILE.DM_LTOUCH_USERID,
                   DMFILE.DM_MEMO,
                   DMFILE.DM_MEMO_TYPE
            FROM DBO.DMFILE
            WHERE  ( DMFILE.DM_BRNO           = :QDM1-WBEG-BRNO )
              AND (( DMFILE.DM_DL_DLNO        = :QDM1-WBEG-DLNO
              AND    DMFILE.DM_DATE_REVERSED <= :QDM1-WBEG-DATE-REV
              AND    DMFILE.DM_SEQ            < :QDM1-WBEG-SEQ       )

               OR  ( DMFILE.DM_DL_DLNO        = :QDM1-WBEG-DLNO
              AND    DMFILE.DM_DATE_REVERSED <= :QDM1-WSBEG-DATE-REV
              AND    DMFILE.DM_SEQ           <= :QDM1-WSBEG-SEQ     ))
            ORDER BY DMFILE.DM_BRNO DESC,
                     DMFILE.DM_DL_DLNO DESC,
                     DMFILE.DM_DATE_REVERSED DESC,
                     DMFILE.DM_SEQ DESC
           END-EXEC.

           EXEC SQL
               OPEN DM1_CURSOR_LT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDM1-WBEG-BRNO
                          QDM1-WEND-BRNO
                          QDM1-WBEG-DLNO
                          QDM1-WEND-DLNO
                          QDM1-WBEG-DATE-REV
                          QDM1-WEND-DATE-REV
                          QDM1-WBEG-SEQ
                          QDM1-WEND-SEQ
                          QDM1-WSBEG-DATE-REV.

           MOVE STAT---GOOD TO DM1-CURSOR-LT-STAT.

      *-----------------------------------------------------------------
       READ-DM1-FILE.
           PERFORM READ-IT.
           MOVE DM-PATH-OWNBR  TO DM-BRNO. 
           MOVE DM-PATH-SQL    TO E-FILE.
           MOVE DM1-KEY        TO E-KEYX.
           INITIALIZE QDM-REC.

           MOVE DM-BRNO          TO QDM-BRNO.
           MOVE DM-DL-DLNO       TO QDM-DL-DLNO.
           MOVE DM-DATE-REVERSED TO QDM-DATE-REVERSED.
           MOVE DM-SEQ           TO QDM-SEQ.

           EXEC SQL
            SELECT DMFILE.DM_BRNO,
                   DMFILE.DM_DL_DLNO,
                   DMFILE.DM_DATE_REVERSED,
                   DMFILE.DM_SEQ,
                   CAST(DMFILE.DM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   DMFILE.DM_LTOUCH_TIME_HHMM,
                   DMFILE.DM_LTOUCH_USERID,
                   DMFILE.DM_MEMO,
                   DMFILE.DM_MEMO_TYPE
            INTO :QDM-REC
            FROM DBO.DMFILE
            WHERE DMFILE.DM_BRNO = :QDM-BRNO
              AND DMFILE.DM_DL_DLNO = :QDM-DL-DLNO
              AND DMFILE.DM_DATE_REVERSED = :QDM-DATE-REVERSED
              AND DMFILE.DM_SEQ = :QDM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DM-FIELDS.

      *-----------------------------------------------------------------
       READ-DM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DM-PATH-OWNBR  TO DM-BRNO.
           MOVE DM-PATH-SQL    TO E-FILE.
           MOVE DM1-KEY        TO E-KEYX.
           INITIALIZE QDM-REC.

           IF ( DM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DM1_CURSOR INTO :QDM-REC
              END-EXEC
           ELSE
           IF ( DM1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   FETCH DM1_CURSOR_SC INTO :QDM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DM-FIELDS.

      *-----------------------------------------------------------------
       READ-DM1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE DM-PATH-OWNBR  TO DM-BRNO.
           MOVE DM-PATH-SQL    TO E-FILE.
           MOVE DM1-KEY        TO E-KEYX.
           INITIALIZE QDM-REC.

           IF ( DM1-CURSOR-LT-STAT-GOOD )
              EXEC SQL
                   FETCH DM1_CURSOR_LT INTO :QDM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DM1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-DM-FIELDS.

      *-----------------------------------------------------------------
       WRITE-DM1-FILE.

           MOVE DM-PATH-OWNBR         TO DM-BRNO.
           ACCEPT DM-LTOUCH-DATE      FROM CENTURY-DATE.
           ACCEPT WS-DM-LTOUCH-TIME   FROM TIME.
           MOVE WS-DM-LTOUCH-HHMM     TO DM-LTOUCH-TIME-HHMM.
           MOVE EXT-CONAME-USER-ID    TO DM-LTOUCH-USERID.
           PERFORM WRITE-IT.
           MOVE DM-PATH-SQL    TO E-FILE.
           MOVE DM1-KEY        TO E-KEYX.
           PERFORM SET-DM-FIELDS.

           EXEC SQL
            INSERT INTO DBO.DMFILE
             (DMFILE.DM_BRNO,
              DMFILE.DM_DL_DLNO,
              DMFILE.DM_DATE_REVERSED,
              DMFILE.DM_SEQ,
              DMFILE.DM_LTOUCH_DATE,
              DMFILE.DM_LTOUCH_TIME_HHMM,
              DMFILE.DM_LTOUCH_USERID,
              DMFILE.DM_MEMO,
              DMFILE.DM_MEMO_TYPE)
            VALUES
             (:QDM-BRNO,
              :QDM-DL-DLNO,
              :QDM-DATE-REVERSED,
              :QDM-SEQ,
              :QDM-LTOUCH-DATE,
              :QDM-LTOUCH-TIME-HHMM,
              :QDM-LTOUCH-USERID,
              :QDM-MEMO,
              :QDM-MEMO-TYPE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-DM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-DM1-FILE.

           MOVE DM-PATH-OWNBR         TO DM-BRNO.
           ACCEPT DM-LTOUCH-DATE      FROM CENTURY-DATE.
           ACCEPT WS-DM-LTOUCH-TIME   FROM TIME.
           MOVE WS-DM-LTOUCH-HHMM     TO DM-LTOUCH-TIME-HHMM.
           MOVE EXT-CONAME-USER-ID    TO DM-LTOUCH-USERID.
           PERFORM REWRITE-IT.
           MOVE DM-PATH-SQL    TO E-FILE.
           MOVE DM1-KEY        TO E-KEYX.
           PERFORM SET-DM-FIELDS.

           EXEC SQL
            UPDATE DBO.DMFILE
             SET
              DMFILE.DM_BRNO = :QDM-BRNO,
              DMFILE.DM_DL_DLNO = :QDM-DL-DLNO,
              DMFILE.DM_DATE_REVERSED = :QDM-DATE-REVERSED,
              DMFILE.DM_SEQ = :QDM-SEQ,
              DMFILE.DM_LTOUCH_DATE = :QDM-LTOUCH-DATE,
              DMFILE.DM_LTOUCH_TIME_HHMM = :QDM-LTOUCH-TIME-HHMM,
              DMFILE.DM_LTOUCH_USERID = :QDM-LTOUCH-USERID,
              DMFILE.DM_MEMO = :QDM-MEMO,
              DMFILE.DM_MEMO_TYPE = :QDM-MEMO-TYPE
             WHERE DMFILE.DM_BRNO = :QDM-BRNO
               AND DMFILE.DM_DL_DLNO = :QDM-DL-DLNO
               AND DMFILE.DM_DATE_REVERSED = :QDM-DATE-REVERSED
               AND DMFILE.DM_SEQ = :QDM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-DM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-DM1-FILE.

           MOVE DM-PATH-OWNBR  TO DM-BRNO.
           PERFORM DELETE-IT.
           MOVE DM-PATH-SQL    TO E-FILE.
           MOVE DM1-KEY        TO E-KEYX.
           PERFORM SET-DM-FIELDS.

           EXEC SQL
            DELETE FROM DBO.DMFILE
             WHERE DMFILE.DM_BRNO = :QDM-BRNO
               AND DMFILE.DM_DL_DLNO = :QDM-DL-DLNO
               AND DMFILE.DM_DATE_REVERSED = :QDM-DATE-REVERSED
               AND DMFILE.DM_SEQ = :QDM-SEQ
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-DM1-FILE.

      *-----------------------------------------------------------------
       CLOSE-DM1-FILE.

           PERFORM CLOSE-IT.

           IF ( DM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DM1-CURSOR-STAT.

           IF ( DM1-CURSOR-LT-STAT-GOOD )
              EXEC SQL
                   CLOSE DM1_CURSOR_LT
              END-EXEC
              MOVE STAT---BAD TO DM1-CURSOR-LT-STAT.

           IF ( DM1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE DM1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO DM1-CURSOR-SC-STAT.
