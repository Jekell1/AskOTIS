      * COPYMEMBER: LIBLP/LPSM1IN
      * 2024.0219 SET SM-LTOUCH-TIME-HHMM AND SM-LTOUCH-USERID
       LOAD-SM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO SM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SM-ALL-MACHINE
              MOVE SM-ALL-PATH             TO SM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO SM-ALL-BASE
              MOVE FILPATH-MACHINE         TO SM-ALL-MACHINE
              MOVE SM-ALL-PATH             TO SM-PATH.

      *-----------------------------------------------------------------
       OPEN-SM1-FILE.
           PERFORM LOAD-SM1-FILE.
           PERFORM OPEN-IT.
           MOVE SM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SM1-FILE.
           PERFORM START-IT.
           MOVE SM-PATH-SQL    TO E-FILE.
           MOVE SM1-KEY        TO E-KEYX.

           IF ( SM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SM1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QSM1-WEND-SEQ = ZEROES    ) OR
              ( QSM1-WEND-SEQ NOT NUMERIC )

              MOVE SM-SP-ORGST          TO QSM1-WBEG-ORGST
              MOVE SM-SP-ORGST          TO QSM1-WEND-ORGST
              MOVE SM-SP-SPRCLASS       TO QSM1-WBEG-SPRCLASS
              MOVE SM-SP-SPRCLASS       TO QSM1-WEND-SPRCLASS
              MOVE SM-SP-SUBCLASS       TO QSM1-WBEG-SUBCLASS
              MOVE SM-SP-SUBCLASS       TO QSM1-WEND-SUBCLASS
              MOVE SM-SP-LAWCODE        TO QSM1-WBEG-LAWCODE
              MOVE SM-SP-LAWCODE        TO QSM1-WEND-LAWCODE
              MOVE SM-DATE-REVERSED     TO QSM1-WBEG-DATE-REVERSED
              MOVE ALL "9"              TO QSM1-WEND-DATE-REVERSED
              MOVE SM-SEQ               TO QSM1-WBEG-SEQ
              MOVE ALL "9"              TO QSM1-WEND-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE SM1_CURSOR CURSOR FOR

            SELECT SMFILE.SM_SP_ORGST,
                   SMFILE.SM_SP_SPRCLASS,
                   SMFILE.SM_SP_SUBCLASS,
                   SMFILE.SM_SP_LAWCODE,
                   SMFILE.SM_DATE_REVERSED,
                   SMFILE.SM_SEQ,
                   CAST(SMFILE.SM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   SMFILE.SM_LTOUCH_TIME_HHMM,
                   SMFILE.SM_LTOUCH_USERID,
                   SMFILE.SM_MEMO1,
                   SMFILE.SM_MEMO2,
                   SMFILE.SM_MEMO_TYPE
            FROM DBO.SMFILE
            WHERE ( SMFILE.SM_SP_ORGST      >= :QSM1-WBEG-ORGST
              AND   SMFILE.SM_SP_SPRCLASS   >= :QSM1-WBEG-SPRCLASS
              AND   SMFILE.SM_SP_SUBCLASS   >= :QSM1-WBEG-SUBCLASS
              AND   SMFILE.SM_SP_LAWCODE    >= :QSM1-WBEG-LAWCODE
              AND   SMFILE.SM_DATE_REVERSED >= :QSM1-WBEG-DATE-REVERSED
              AND   SMFILE.SM_SEQ           >= :QSM1-WBEG-SEQ       )

              AND ( SMFILE.SM_SP_ORGST      <= :QSM1-WEND-ORGST
              AND   SMFILE.SM_SP_SPRCLASS   <= :QSM1-WEND-SPRCLASS
              AND   SMFILE.SM_SP_SUBCLASS   <= :QSM1-WEND-SUBCLASS
              AND   SMFILE.SM_SP_LAWCODE    <= :QSM1-WEND-LAWCODE
              AND   SMFILE.SM_DATE_REVERSED <= :QSM1-WEND-DATE-REVERSED
              AND   SMFILE.SM_SEQ           <= :QSM1-WEND-SEQ       )

            ORDER BY SMFILE.SM_SP_ORGST,
                     SMFILE.SM_SP_SPRCLASS,
                     SMFILE.SM_SP_SUBCLASS,
                     SMFILE.SM_SP_LAWCODE,
                     SMFILE.SM_DATE_REVERSED,
                     SMFILE.SM_SEQ
           END-EXEC.

           EXEC SQL
               OPEN SM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSM1-WBEG-ORGST
                          QSM1-WEND-ORGST.

           MOVE ZEROES TO QSM1-WBEG-SPRCLASS
                          QSM1-WEND-SPRCLASS
                          QSM1-WBEG-SUBCLASS
                          QSM1-WEND-SUBCLASS
                          QSM1-WBEG-LAWCODE
                          QSM1-WEND-LAWCODE
                          QSM1-WBEG-DATE-REVERSED
                          QSM1-WEND-DATE-REVERSED
                          QSM1-WBEG-SEQ
                          QSM1-WEND-SEQ.

           MOVE STAT---GOOD TO SM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-SM1-FILE-SCAN.
           PERFORM START-IT.
           MOVE SM-PATH-SQL    TO E-FILE.
           MOVE SM1-KEY        TO E-KEYX.

           IF ( SM1-CURSOR-SC-STAT-GOOD )
              PERFORM CLOSE-SM1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QSM1-WEND-SEQ = ZEROES    ) OR
              ( QSM1-WEND-SEQ NOT NUMERIC )
              MOVE SM-SP-ORGST          TO QSM1-WBEG-ORGST
              MOVE SM-SP-ORGST          TO QSM1-WEND-ORGST
              MOVE SM-SP-SPRCLASS       TO QSM1-WBEG-SPRCLASS
              MOVE SM-SP-SPRCLASS       TO QSM1-WEND-SPRCLASS
              MOVE SM-SP-SUBCLASS       TO QSM1-WBEG-SUBCLASS
              MOVE SM-SP-SUBCLASS       TO QSM1-WEND-SUBCLASS
              MOVE SM-SP-LAWCODE        TO QSM1-WBEG-LAWCODE
              MOVE SM-SP-LAWCODE        TO QSM1-WEND-LAWCODE
              MOVE SM-DATE-REVERSED     TO QSM1-WBEG-DATE-REVERSED
              MOVE ALL "9"              TO QSM1-WEND-DATE-REVERSED
              MOVE SM-SEQ               TO QSM1-WBEG-SEQ
              MOVE ALL "9"              TO QSM1-WEND-SEQ
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
      *       SKIP; NOT NEEDED             QSM1-WSBEG-BRNO
      *       SKIP; NOT NEEDED             QSM1-WSBEG-DLNO
      *-   -   -{ MEMO DATE DO NOT ADD IF PRIORITY MEMOS (0) }
      *       IF ( SM-DATE-REVERSED = 0 )
      *          MOVE SM-DATE-REVERSED  TO QSM1-WSBEG-DATE-REVERSED
      *       ELSE
                 COMPUTE QSM1-WSBEG-DATE-REVERSED
                                    = SM-DATE-REVERSED + 1.
      *       SKIP; NOT NEEDED             QSM1-WSBEG-SEQ

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE SM1_CURSOR_SC CURSOR FOR
            SELECT SMFILE.SM_SP_ORGST,
                   SMFILE.SM_SP_SPRCLASS,
                   SMFILE.SM_SP_SUBCLASS,
                   SMFILE.SM_SP_LAWCODE,
                   SMFILE.SM_DATE_REVERSED,
                   SMFILE.SM_SEQ,
                   CAST(SMFILE.SM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   SMFILE.SM_LTOUCH_TIME_HHMM,
                   SMFILE.SM_LTOUCH_USERID,
                   SMFILE.SM_MEMO1,
                   SMFILE.SM_MEMO2,
                   SMFILE.SM_MEMO_TYPE
            FROM DBO.SMFILE

            WHERE (( SMFILE.SM_SP_ORGST       = :QSM1-WBEG-ORGST
              AND    SMFILE.SM_SP_SPRCLASS    = :QSM1-WBEG-SPRCLASS
              AND    SMFILE.SM_SP_SUBCLASS    = :QSM1-WBEG-SUBCLASS
              AND    SMFILE.SM_SP_LAWCODE     = :QSM1-WBEG-LAWCODE
              AND    SMFILE.SM_DATE_REVERSED >= 
                                              :QSM1-WBEG-DATE-REVERSED
              AND    SMFILE.SM_SEQ           >= :QSM1-WBEG-SEQ       )

               OR  ( SMFILE.SM_SP_ORGST       = :QSM1-WBEG-ORGST
              AND    SMFILE.SM_SP_SPRCLASS    = :QSM1-WBEG-SPRCLASS
              AND    SMFILE.SM_SP_SUBCLASS    = :QSM1-WBEG-SUBCLASS
              AND    SMFILE.SM_SP_LAWCODE     = :QSM1-WBEG-LAWCODE
              AND    SMFILE.SM_DATE_REVERSED >= 
                                              :QSM1-WSBEG-DATE-REVERSED
              AND    SMFILE.SM_SEQ           >= 0                    ))

              AND  ( SMFILE.SM_SP_ORGST       = :QSM1-WBEG-ORGST
              AND    SMFILE.SM_SP_SPRCLASS    = :QSM1-WBEG-SPRCLASS
              AND    SMFILE.SM_SP_SUBCLASS    = :QSM1-WBEG-SUBCLASS
              AND    SMFILE.SM_SP_LAWCODE     = :QSM1-WBEG-LAWCODE
              AND    SMFILE.SM_DATE_REVERSED <=
                                              :QSM1-WEND-DATE-REVERSED
              AND    SMFILE.SM_SEQ           <= :QSM1-WEND-SEQ       )

            ORDER BY SMFILE.SM_SP_ORGST,
                     SMFILE.SM_SP_SPRCLASS,
                     SMFILE.SM_SP_SUBCLASS,
                     SMFILE.SM_SP_LAWCODE,
                     SMFILE.SM_DATE_REVERSED,
                     SMFILE.SM_SEQ
           END-EXEC.
 
           EXEC SQL
               OPEN SM1_CURSOR_SC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSM1-WBEG-ORGST
                          QSM1-WEND-ORGST.

           MOVE ZEROES TO QSM1-WBEG-SPRCLASS
                          QSM1-WEND-SPRCLASS
                          QSM1-WBEG-SUBCLASS
                          QSM1-WEND-SUBCLASS
                          QSM1-WBEG-LAWCODE
                          QSM1-WEND-LAWCODE
                          QSM1-WBEG-DATE-REVERSED
                          QSM1-WEND-DATE-REVERSED
                          QSM1-WBEG-SEQ
                          QSM1-WEND-SEQ.

           MOVE STAT---GOOD TO SM1-CURSOR-SC-STAT.

      *-----------------------------------------------------------------
       START-SM1-FILE-LESS-THAN.
           PERFORM START-IT.
           MOVE SM-PATH-SQL    TO E-FILE.
           MOVE SM1-KEY        TO E-KEYX.

           IF ( SM1-CURSOR-LT-STAT-GOOD )
              PERFORM CLOSE-SM1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO 
      *  !!SUBTRACT!! ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QSM1-WEND-SEQ = ZEROES    ) OR
              ( QSM1-WEND-SEQ NOT NUMERIC )
              MOVE SM-SP-ORGST          TO QSM1-WBEG-ORGST
              MOVE SM-SP-ORGST          TO QSM1-WEND-ORGST
              MOVE SM-SP-SPRCLASS       TO QSM1-WBEG-SPRCLASS
              MOVE SM-SP-SPRCLASS       TO QSM1-WEND-SPRCLASS
              MOVE SM-SP-SUBCLASS       TO QSM1-WBEG-SUBCLASS
              MOVE SM-SP-SUBCLASS       TO QSM1-WEND-SUBCLASS
              MOVE SM-SP-LAWCODE        TO QSM1-WBEG-LAWCODE
              MOVE SM-SP-LAWCODE        TO QSM1-WEND-LAWCODE
              MOVE SM-DATE-REVERSED     TO QSM1-WBEG-DATE-REVERSED
              MOVE SM-DATE-REVERSED     TO QSM1-WEND-DATE-REVERSED
              MOVE SM-SEQ               TO QSM1-WBEG-SEQ
              MOVE SM-SEQ               TO QSM1-WEND-SEQ
      *-   -   -{ SUBTRACT ONE TO WHERE SCAN BEG FIELDS }
      *       SKIP; NOT NEEDED             QSM1-WSBEG-BRNO
      *       SKIP; NOT NEEDED             QSM1-WSBEG-DLNO
      *-   -   -{ MEMO DATE DO NOT SUBTRACT IF PRIORITY MEMOS (0) }
              IF ( SM-DATE-REVERSED = 0 )
                 MOVE SM-DATE-REVERSED  TO QSM1-WSBEG-DATE-REVERSED
                 COMPUTE QSM1-WSBEG-SEQ      = SM-SEQ           - 1
              ELSE
                 COMPUTE QSM1-WSBEG-DATE-REVERSED = SM-DATE-REVERSED - 1 
                 MOVE 999               TO QSM1-WSBEG-SEQ.
      *       SKIP; NOT NEEDED             QSM1-WSBEG-SEQ

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE SM1_CURSOR_LT CURSOR FOR
            SELECT SMFILE.SM_SP_ORGST,
                   SMFILE.SM_SP_SPRCLASS,
                   SMFILE.SM_SP_SUBCLASS,
                   SMFILE.SM_SP_LAWCODE,
                   SMFILE.SM_DATE_REVERSED,
                   SMFILE.SM_SEQ,
                   CAST(SMFILE.SM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   SMFILE.SM_LTOUCH_TIME_HHMM,
                   SMFILE.SM_LTOUCH_USERID,
                   SMFILE.SM_MEMO1,
                   SMFILE.SM_MEMO2,
                   SMFILE.SM_MEMO_TYPE
            FROM DBO.SMFILE
            WHERE (( SMFILE.SM_SP_ORGST       = :QSM1-WEND-ORGST
              AND    SMFILE.SM_SP_SPRCLASS    = :QSM1-WEND-SPRCLASS
              AND    SMFILE.SM_SP_SUBCLASS    = :QSM1-WEND-SUBCLASS
              AND    SMFILE.SM_SP_LAWCODE     = :QSM1-WEND-LAWCODE
              AND    SMFILE.SM_DATE_REVERSED <= 
                                              :QSM1-WBEG-DATE-REVERSED
              AND    SMFILE.SM_SEQ            < :QSM1-WBEG-SEQ       )

               OR  ( SMFILE.SM_SP_ORGST       = :QSM1-WEND-ORGST
              AND    SMFILE.SM_SP_SPRCLASS    = :QSM1-WEND-SPRCLASS
              AND    SMFILE.SM_SP_SUBCLASS    = :QSM1-WEND-SUBCLASS
              AND    SMFILE.SM_SP_LAWCODE     = :QSM1-WEND-LAWCODE
              AND    SMFILE.SM_DATE_REVERSED <= 
                                              :QSM1-WSBEG-DATE-REVERSED
              AND    SMFILE.SM_SEQ           <= :QSM1-WSBEG-SEQ     ))

            ORDER BY SMFILE.SM_SP_ORGST      DESC,
                     SMFILE.SM_SP_SPRCLASS   DESC,
                     SMFILE.SM_SP_SUBCLASS   DESC,
                     SMFILE.SM_SP_LAWCODE    DESC,
                     SMFILE.SM_DATE_REVERSED DESC,
                     SMFILE.SM_SEQ           DESC
           END-EXEC.

           EXEC SQL
               OPEN SM1_CURSOR_LT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSM1-WBEG-ORGST
                          QSM1-WEND-ORGST.

           MOVE ZEROES TO QSM1-WBEG-SPRCLASS
                          QSM1-WEND-SPRCLASS
                          QSM1-WBEG-SUBCLASS
                          QSM1-WEND-SUBCLASS
                          QSM1-WBEG-LAWCODE
                          QSM1-WEND-LAWCODE
                          QSM1-WBEG-DATE-REVERSED
                          QSM1-WEND-DATE-REVERSED
                          QSM1-WBEG-SEQ
                          QSM1-WEND-SEQ.

           MOVE STAT---GOOD TO SM1-CURSOR-LT-STAT.

      *-----------------------------------------------------------------
       READ-SM1-FILE.
           PERFORM READ-IT.
           MOVE SM-PATH-SQL            TO E-FILE.
           MOVE SM1-KEY                TO E-KEYX.
           INITIALIZE QSM-REC.

           MOVE SM-SP-ORGST            TO QSM-SP-ORGST.
           MOVE SM-SP-SPRCLASS         TO QSM-SP-SPRCLASS.
           MOVE SM-SP-SUBCLASS         TO QSM-SP-SUBCLASS.
           MOVE SM-SP-LAWCODE          TO QSM-SP-LAWCODE.
           MOVE SM-DATE-REVERSED       TO QSM-DATE-REVERSED.
           MOVE SM-SEQ                 TO QSM-SEQ.

           EXEC SQL
            SELECT SMFILE.SM_SP_ORGST,
                   SMFILE.SM_SP_SPRCLASS,
                   SMFILE.SM_SP_SUBCLASS,
                   SMFILE.SM_SP_LAWCODE,
                   SMFILE.SM_DATE_REVERSED,
                   SMFILE.SM_SEQ,
                   CAST(SMFILE.SM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   SMFILE.SM_LTOUCH_TIME_HHMM,
                   SMFILE.SM_LTOUCH_USERID,
                   SMFILE.SM_MEMO1,
                   SMFILE.SM_MEMO2,
                   SMFILE.SM_MEMO_TYPE
            INTO :QSM-REC
            FROM DBO.SMFILE
            WHERE SMFILE.SM_SP_ORGST      = :QSM-SP-ORGST
              AND SMFILE.SM_SP_SPRCLASS   = :QSM-SP-SPRCLASS
              AND SMFILE.SM_SP_SUBCLASS   = :QSM-SP-SUBCLASS
              AND SMFILE.SM_SP_LAWCODE    = :QSM-SP-LAWCODE
              AND SMFILE.SM_DATE_REVERSED = :QSM-DATE-REVERSED
              AND SMFILE.SM_SEQ           = :QSM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SM-FIELDS.

      *-----------------------------------------------------------------
       READ-SM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SM-PATH-SQL    TO E-FILE.
           MOVE SM1-KEY        TO E-KEYX.
           INITIALIZE QSM-REC.

           IF ( SM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SM1_CURSOR INTO :QSM-REC
              END-EXEC
           ELSE
           IF ( SM1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   FETCH SM1_CURSOR_SC INTO :QSM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SM-FIELDS.

      *-----------------------------------------------------------------
       READ-SM1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE SM-PATH-SQL    TO E-FILE.
           MOVE SM1-KEY        TO E-KEYX.
           INITIALIZE QSM-REC.

           IF ( SM1-CURSOR-LT-STAT-GOOD )
              EXEC SQL
                   FETCH SM1_CURSOR_LT INTO :QSM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SM1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-SM-FIELDS.

      *-----------------------------------------------------------------
       WRITE-SM1-FILE.

           PERFORM WRITE-IT.
           ACCEPT SM-LTOUCH-DATE      FROM CENTURY-DATE.
           ACCEPT WS-SM-LTOUCH-TIME   FROM TIME.
           MOVE WS-SM-LTOUCH-HHMM     TO SM-LTOUCH-TIME-HHMM.
           MOVE EXT-CONAME-USER-ID    TO SM-LTOUCH-USERID.
           MOVE SM-PATH-SQL    TO E-FILE.
           MOVE SM1-KEY        TO E-KEYX.
           PERFORM SET-SM-FIELDS.

           EXEC SQL
            INSERT INTO DBO.SMFILE
             (SMFILE.SM_SP_ORGST,
              SMFILE.SM_SP_SPRCLASS,
              SMFILE.SM_SP_SUBCLASS,
              SMFILE.SM_SP_LAWCODE,
              SMFILE.SM_DATE_REVERSED,
              SMFILE.SM_SEQ,
              SMFILE.SM_LTOUCH_DATE,
              SMFILE.SM_LTOUCH_TIME_HHMM,
              SMFILE.SM_LTOUCH_USERID,
              SMFILE.SM_MEMO1,
              SMFILE.SM_MEMO2,
              SMFILE.SM_MEMO_TYPE)
            VALUES
             (:QSM-SP-ORGST,
              :QSM-SP-SPRCLASS,
              :QSM-SP-SUBCLASS,
              :QSM-SP-LAWCODE,
              :QSM-DATE-REVERSED,
              :QSM-SEQ,
              :QSM-LTOUCH-DATE,
              :QSM-LTOUCH-TIME-HHMM,
              :QSM-LTOUCH-USERID,
              :QSM-MEMO1,
              :QSM-MEMO2,
              :QSM-MEMO-TYPE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-SM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-SM1-FILE.

           PERFORM REWRITE-IT.
           ACCEPT SM-LTOUCH-DATE      FROM CENTURY-DATE.
           ACCEPT WS-SM-LTOUCH-TIME   FROM TIME.
           MOVE WS-SM-LTOUCH-HHMM     TO SM-LTOUCH-TIME-HHMM.
           MOVE EXT-CONAME-USER-ID    TO SM-LTOUCH-USERID.
           MOVE SM-PATH-SQL    TO E-FILE.
           MOVE SM1-KEY        TO E-KEYX.
           PERFORM SET-SM-FIELDS.

           EXEC SQL
            UPDATE DBO.SMFILE
             SET
              SMFILE.SM_SP_ORGST          = :QSM-SP-ORGST,
              SMFILE.SM_SP_SPRCLASS       = :QSM-SP-SPRCLASS,
              SMFILE.SM_SP_SUBCLASS       = :QSM-SP-SUBCLASS,
              SMFILE.SM_SP_LAWCODE        = :QSM-SP-LAWCODE,
              SMFILE.SM_DATE_REVERSED     = :QSM-DATE-REVERSED,
              SMFILE.SM_SEQ               = :QSM-SEQ,
              SMFILE.SM_LTOUCH_DATE       = :QSM-LTOUCH-DATE,
              SMFILE.SM_LTOUCH_TIME_HHMM  = :QSM-LTOUCH-TIME-HHMM,
              SMFILE.SM_LTOUCH_USERID     = :QSM-LTOUCH-USERID,
              SMFILE.SM_MEMO1             = :QSM-MEMO1,
              SMFILE.SM_MEMO2             = :QSM-MEMO2,
              SMFILE.SM_MEMO_TYPE         = :QSM-MEMO-TYPE
            WHERE SMFILE.SM_SP_ORGST      = :QSM-SP-ORGST
              AND SMFILE.SM_SP_SPRCLASS   = :QSM-SP-SPRCLASS
              AND SMFILE.SM_SP_SUBCLASS   = :QSM-SP-SUBCLASS
              AND SMFILE.SM_SP_LAWCODE    = :QSM-SP-LAWCODE
              AND SMFILE.SM_DATE_REVERSED = :QSM-DATE-REVERSED
              AND SMFILE.SM_SEQ           = :QSM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-SM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-SM1-FILE.

           PERFORM DELETE-IT.
           MOVE SM-PATH-SQL    TO E-FILE.
           MOVE SM1-KEY        TO E-KEYX.
           PERFORM SET-SM-FIELDS.

           EXEC SQL
            DELETE FROM DBO.SMFILE
            WHERE SMFILE.SM_SP_ORGST      = :QSM-SP-ORGST
              AND SMFILE.SM_SP_SPRCLASS   = :QSM-SP-SPRCLASS
              AND SMFILE.SM_SP_SUBCLASS   = :QSM-SP-SUBCLASS
              AND SMFILE.SM_SP_LAWCODE    = :QSM-SP-LAWCODE
              AND SMFILE.SM_DATE_REVERSED = :QSM-DATE-REVERSED
              AND SMFILE.SM_SEQ           = :QSM-SEQ
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-SM1-FILE.

      *-----------------------------------------------------------------
       CLOSE-SM1-FILE.

           PERFORM CLOSE-IT.

           IF ( SM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SM1-CURSOR-STAT.

           IF ( SM1-CURSOR-LT-STAT-GOOD )
              EXEC SQL
                   CLOSE SM1_CURSOR_LT
              END-EXEC
              MOVE STAT---BAD TO SM1-CURSOR-LT-STAT.

           IF ( SM1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE SM1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO SM1-CURSOR-SC-STAT.
