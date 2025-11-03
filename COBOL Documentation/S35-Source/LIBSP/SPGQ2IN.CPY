      * COPYMEMBER: LIBSP/SPGQ2IN

      *----------------------------------------------------------------
       LOAD-GQ2-NON-COMP-KEY.
      *    MOVE GQ-BRNO                 TO SPWSGQ-GQ2-BRNO-NC.
           MOVE GQ-COLLID               TO SPWSGQ-GQ2-COLLID-NC.
           MOVE GQ-SEQ-FG               TO SPWSGQ-GQ2-SEQ-FG-NC.
           MOVE GQ-DATE                 TO SPWSGQ-GQ2-DATE-NC.
           MOVE GQ-START-TIME           TO SPWSGQ-GQ2-START-TIME-NC.
           MOVE GQ-REV-DEL              TO SPWSGQ-GQ2-REV-DEL-NC.
           MOVE GQ-MISC-KEY-FLD         TO SPWSGQ-GQ2-MISC-KEY-FLD-NC.
           MOVE GQ-REV-PDUE-DAYS        TO SPWSGQ-GQ2-REV-PDUE-DAYS-NC.
           MOVE GQ-REV-BAL              TO SPWSGQ-GQ2-REV-BAL-NC.
           MOVE GQ-SEQ                  TO SPWSGQ-GQ2-SEQ-NC.

      *-----------------------------------------------------------------
      *    USED TO CLEAR (SPACES/ZEROES) ALL GQ2-KEY FIELDS.
      *-----------------------------------------------------------------
       CLEAR-GQ2-KEY.
           MOVE SPACES                       TO GQ-COLLID.
           MOVE SPACES                       TO GQ-SEQ-FG.
           MOVE ZEROES                       TO GQ-DATE.
           MOVE ZEROES                       TO GQ-START-TIME.
           MOVE ZEROES                       TO GQ-REV-DEL.
           MOVE SPACES                       TO GQ-MISC-KEY-FLD.
           MOVE ZEROES                       TO GQ-REV-PDUE-DAYS.
           MOVE ZEROES                       TO GQ-REV-BAL.
           MOVE ZEROES                       TO GQ-SEQ.

      *-----------------------------------------------------------------
      *    USED TO LOAD THE GQ2-KEY FIELDS FROM THE HOLD-GQ2-KEY OR
      *    SAVE-GQ2-KEY FIELDS (SPWSGQ-GQ2-KEY).
      *    SPWSGQ-GQ2-KEY FIELDS ARE USED TO POPULATE GQ2-KEY FIELDS.
      *    PSEUDO CODE:  MOVE SPWSGQ-GQ2-KEY TO GQ2-KEY.
      *-----------------------------------------------------------------
       LOAD-GQ2-KEY.
      *    MOVE SPWSGQ-GQ2-BRNO              TO GQ-BRNO.
           MOVE SPWSGQ-GQ2-COLLID            TO GQ-COLLID.
           MOVE SPWSGQ-GQ2-SEQ-FG            TO GQ-SEQ-FG.
           MOVE SPWSGQ-GQ2-DATE              TO GQ-DATE.
           MOVE SPWSGQ-GQ2-START-TIME        TO GQ-START-TIME.
           MOVE SPWSGQ-GQ2-REV-DEL           TO GQ-REV-DEL.
           MOVE SPWSGQ-GQ2-MISC-KEY-FLD      TO GQ-MISC-KEY-FLD.
           MOVE SPWSGQ-GQ2-REV-PDUE-DAYS     TO GQ-REV-PDUE-DAYS.
           MOVE SPWSGQ-GQ2-REV-BAL           TO GQ-REV-BAL.
           MOVE SPWSGQ-GQ2-SEQ               TO GQ-SEQ.

      *-----------------------------------------------------------------
      *    USED TO SAVE THE GQ2-KEY FIELDS TO THE HOLD-GQ2-KEY OR
      *    SAVE-GQ2-KEY FIELDS (SPWSGQ-GQ2-KEY).
      *    GQ2-KEY FIELDS ARE USED TO POPULATE SPWSGQ-GQ2-KEY FIELDS.
      *    PSEUDO CODE:  MOVE GQ2-KEY TO SPWSGQ-GQ2-KEY.
      *-----------------------------------------------------------------
       SAVE-GQ2-KEY.
           MOVE GQ-COLLID               TO SPWSGQ-GQ2-COLLID.
           MOVE GQ-SEQ-FG               TO SPWSGQ-GQ2-SEQ-FG.
           MOVE GQ-DATE                 TO SPWSGQ-GQ2-DATE.
           MOVE GQ-START-TIME           TO SPWSGQ-GQ2-START-TIME.
           MOVE GQ-REV-DEL              TO SPWSGQ-GQ2-REV-DEL.
           MOVE GQ-MISC-KEY-FLD         TO SPWSGQ-GQ2-MISC-KEY-FLD.
           MOVE GQ-REV-PDUE-DAYS        TO SPWSGQ-GQ2-REV-PDUE-DAYS.
           MOVE GQ-REV-BAL              TO SPWSGQ-GQ2-REV-BAL.
           MOVE GQ-SEQ                  TO SPWSGQ-GQ2-SEQ.

      *----------------------------------------------------------------
       LOAD-GQ2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GQ-ALL-MACHINE
              MOVE GQ-ALL-PATH             TO GQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO GQ-ALL-MACHINE
              MOVE GQ-ALL-PATH             TO GQ-PATH.

      *-----------------------------------------------------------------
       OPEN-GQ2-FILE.
           PERFORM LOAD-GQ2-FILE.
           PERFORM OPEN-IT.
           MOVE GQ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GQ2-FILE.
           PERFORM START-IT.
           MOVE GQ-PATH-SQL TO E-FILE.
           PERFORM LOAD-GQ2-NON-COMP-KEY.
           MOVE SPWSGQ-GQ2-KEY-NON-COMP TO E-KEYX.

           IF ( GQ2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GQ2-FILE.

           IF ( QGQ2-WEND-SEQ = ZEROES    ) OR
              ( QGQ2-WEND-SEQ NOT NUMERIC )
              MOVE GQ-COLLID            TO QGQ2-WBEG-COLLID
              MOVE ALL "Z"              TO QGQ2-WEND-COLLID
              MOVE GQ-SEQ-FG            TO QGQ2-WBEG-SEQ-FG
              MOVE ALL "Z"              TO QGQ2-WEND-SEQ-FG
              MOVE GQ-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGQ2-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGQ2-WEND-DATE
              MOVE GQ-START-TIME        TO QGQ2-WBEG-START-TIME
              MOVE ALL "9"              TO QGQ2-WEND-START-TIME
              MOVE GQ-REV-DEL           TO QGQ2-WBEG-REV-DEL
              MOVE ALL "9"              TO QGQ2-WEND-REV-DEL
              MOVE GQ-MISC-KEY-FLD      TO QGQ2-WBEG-MISC-KEY-FLD
              MOVE ALL "Z"              TO QGQ2-WEND-MISC-KEY-FLD
              MOVE GQ-REV-PDUE-DAYS     TO QGQ2-WBEG-REV-PDUE-DAYS
              MOVE ALL "9"              TO QGQ2-WEND-REV-PDUE-DAYS
              MOVE GQ-REV-BAL           TO QGQ2-WBEG-REV-BAL
              MOVE ALL "9"              TO QGQ2-WEND-REV-BAL
              MOVE GQ-SEQ               TO QGQ2-WBEG-SEQ
              MOVE ALL "9"              TO QGQ2-WEND-SEQ.

           EXEC SQL
            DECLARE GQ2_CURSOR CURSOR FOR
            SELECT GQFILE.GQ_BRNO,
                   GQFILE.GQ_ACCTNO,
                   GQFILE.GQ_COLLID,
                   GQFILE.GQ_SEQ_FG,
                   CAST(GQFILE.GQ_DATE AS VARCHAR(10)),
                   GQFILE.GQ_START_TIME,
                   GQFILE.GQ_REV_DEL,
                   GQFILE.GQ_MISC_KEY_FLD,
                   GQFILE.GQ_REV_PDUE_DAYS,
                   GQFILE.GQ_REV_BAL,
                   GQFILE.GQ_SEQ,
                   CAST(GQFILE.GQ_LTOUCH_DATE AS VARCHAR(10)),
                   GQFILE.GQ_WORK_COLLID,
                   GQFILE.GQ_WORKED_FG,
                   GQFILE.GQ_IN_PROCESS,
                   CAST(GQFILE.GQ_DATE_QUEUED AS VARCHAR(10)),
                   GQFILE.GQ_TIME_ZONE_ERROR
            FROM DBO.GQFILE
            WHERE GQFILE.GQ_COLLID >= :QGQ2-WBEG-COLLID
              AND GQFILE.GQ_COLLID <= :QGQ2-WEND-COLLID
              AND GQFILE.GQ_SEQ_FG >= :QGQ2-WBEG-SEQ-FG
              AND GQFILE.GQ_SEQ_FG <= :QGQ2-WEND-SEQ-FG
              AND GQFILE.GQ_DATE   >= :QGQ2-WBEG-DATE
              AND GQFILE.GQ_DATE   <= :QGQ2-WEND-DATE
              AND GQFILE.GQ_START_TIME >= :QGQ2-WBEG-START-TIME
              AND GQFILE.GQ_START_TIME <= :QGQ2-WEND-START-TIME
              AND GQFILE.GQ_REV_DEL >= :QGQ2-WBEG-REV-DEL
              AND GQFILE.GQ_REV_DEL <= :QGQ2-WEND-REV-DEL
              AND GQFILE.GQ_MISC_KEY_FLD >= :QGQ2-WBEG-MISC-KEY-FLD
              AND GQFILE.GQ_MISC_KEY_FLD <= :QGQ2-WEND-MISC-KEY-FLD
              AND GQFILE.GQ_REV_PDUE_DAYS>= :QGQ2-WBEG-REV-PDUE-DAYS
              AND GQFILE.GQ_REV_PDUE_DAYS<= :QGQ2-WEND-REV-PDUE-DAYS
              AND GQFILE.GQ_REV_BAL >= :QGQ2-WBEG-REV-BAL
              AND GQFILE.GQ_REV_BAL <= :QGQ2-WEND-REV-BAL
              AND GQFILE.GQ_SEQ >= :QGQ2-WBEG-SEQ
              AND GQFILE.GQ_SEQ <= :QGQ2-WEND-SEQ
            ORDER BY GQFILE.GQ_COLLID,
                     GQFILE.GQ_SEQ_FG,
                     GQFILE.GQ_DATE,
                     GQFILE.GQ_START_TIME,
                     GQFILE.GQ_REV_DEL,
                     GQFILE.GQ_MISC_KEY_FLD,
                     GQFILE.GQ_REV_PDUE_DAYS,
                     GQFILE.GQ_REV_BAL,
                     GQFILE.GQ_SEQ,
                     GQFILE.GQ_BRNO,
                     GQFILE.GQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN GQ2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGQ2-WBEG-COLLID
                          QGQ2-WEND-COLLID
                          QGQ2-WBEG-SEQ-FG
                          QGQ2-WEND-SEQ-FG
                          QGQ2-WBEG-DATE
                          QGQ2-WEND-DATE
                          QGQ2-WBEG-MISC-KEY-FLD
                          QGQ2-WEND-MISC-KEY-FLD.
      
           MOVE ZEROES TO QGQ2-WBEG-START-TIME
                          QGQ2-WEND-START-TIME
                          QGQ2-WBEG-REV-DEL
                          QGQ2-WEND-REV-DEL
                          QGQ2-WBEG-REV-PDUE-DAYS
                          QGQ2-WEND-REV-PDUE-DAYS
                          QGQ2-WBEG-REV-BAL
                          QGQ2-WEND-REV-BAL
                          QGQ2-WBEG-SEQ
                          QGQ2-WEND-SEQ.

           MOVE STAT---GOOD TO GQ2-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-GQ2-FILE-GREATER.
           PERFORM START-IT.
           MOVE GQ-PATH-SQL TO E-FILE.
           PERFORM LOAD-GQ2-NON-COMP-KEY.
           MOVE SPWSGQ-GQ2-KEY-NON-COMP TO E-KEYX.

           IF ( GQ2-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-GQ2-FILE.

           IF ( QGQ2-WEND-SEQ = ZEROES    ) OR
              ( QGQ2-WEND-SEQ NOT NUMERIC )
              MOVE GQ-COLLID            TO QGQ2-WBEG-COLLID
              MOVE ALL "Z"              TO QGQ2-WEND-COLLID
              MOVE GQ-SEQ-FG            TO QGQ2-WBEG-SEQ-FG
              MOVE ALL "Z"              TO QGQ2-WEND-SEQ-FG
              MOVE GQ-DATE             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQ2-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQ2-WEND-DATE
              MOVE GQ-START-TIME        TO QGQ2-WBEG-START-TIME
              MOVE ALL "9"              TO QGQ2-WEND-START-TIME
              MOVE GQ-REV-DEL           TO QGQ2-WBEG-REV-DEL
              MOVE ALL "9"              TO QGQ2-WEND-REV-DEL
              MOVE GQ-MISC-KEY-FLD      TO QGQ2-WBEG-MISC-KEY-FLD
              MOVE ALL "Z"              TO QGQ2-WEND-MISC-KEY-FLD
              MOVE GQ-REV-PDUE-DAYS     TO QGQ2-WBEG-REV-PDUE-DAYS
              MOVE ALL "9"              TO QGQ2-WEND-REV-PDUE-DAYS
              MOVE GQ-REV-BAL           TO QGQ2-WBEG-REV-BAL
              MOVE ALL "9"              TO QGQ2-WEND-REV-BAL
              MOVE GQ-SEQ               TO QGQ2-WBEG-SEQ
              MOVE ALL "9"              TO QGQ2-WEND-SEQ.

           EXEC SQL
            DECLARE GQ2_CURSOR_GR CURSOR FOR
            SELECT GQFILE.GQ_BRNO,
                   GQFILE.GQ_ACCTNO,
                   GQFILE.GQ_COLLID,
                   GQFILE.GQ_SEQ_FG,
                   CAST(GQFILE.GQ_DATE AS VARCHAR(10)),
                   GQFILE.GQ_START_TIME,
                   GQFILE.GQ_REV_DEL,
                   GQFILE.GQ_MISC_KEY_FLD,
                   GQFILE.GQ_REV_PDUE_DAYS,
                   GQFILE.GQ_REV_BAL,
                   GQFILE.GQ_SEQ,
                   CAST(GQFILE.GQ_LTOUCH_DATE AS VARCHAR(10)),
                   GQFILE.GQ_WORK_COLLID,
                   GQFILE.GQ_WORKED_FG,
                   GQFILE.GQ_IN_PROCESS,
                   CAST(GQFILE.GQ_DATE_QUEUED AS VARCHAR(10)),
                   GQFILE.GQ_TIME_ZONE_ERROR
            FROM DBO.GQFILE
            WHERE GQFILE.GQ_COLLID > :QGQ2-WBEG-COLLID
              AND GQFILE.GQ_COLLID <= :QGQ2-WEND-COLLID
              AND GQFILE.GQ_SEQ_FG > :QGQ2-WBEG-SEQ-FG
              AND GQFILE.GQ_SEQ_FG <= :QGQ2-WEND-SEQ-FG
              AND GQFILE.GQ_DATE   >  :QGQ2-WBEG-DATE
              AND GQFILE.GQ_DATE   <= :QGQ2-WEND-DATE
              AND GQFILE.GQ_START_TIME > :QGQ2-WBEG-START-TIME
              AND GQFILE.GQ_START_TIME <= :QGQ2-WEND-START-TIME
              AND GQFILE.GQ_REV_DEL > :QGQ2-WBEG-REV-DEL
              AND GQFILE.GQ_REV_DEL <= :QGQ2-WEND-REV-DEL
              AND GQFILE.GQ_MISC_KEY_FLD > :QGQ2-WBEG-MISC-KEY-FLD
              AND GQFILE.GQ_MISC_KEY_FLD <= :QGQ2-WEND-MISC-KEY-FLD
              AND GQFILE.GQ_REV_PDUE_DAYS> :QGQ2-WBEG-REV-PDUE-DAYS
              AND GQFILE.GQ_REV_PDUE_DAYS<= :QGQ2-WEND-REV-PDUE-DAYS
              AND GQFILE.GQ_REV_BAL > :QGQ2-WBEG-REV-BAL
              AND GQFILE.GQ_REV_BAL <= :QGQ2-WEND-REV-BAL
              AND GQFILE.GQ_SEQ > :QGQ2-WBEG-SEQ
              AND GQFILE.GQ_SEQ <= :QGQ2-WEND-SEQ
            ORDER BY GQFILE.GQ_COLLID,
                     GQFILE.GQ_SEQ_FG,
                     GQFILE.GQ_DATE,
                     GQFILE.GQ_START_TIME,
                     GQFILE.GQ_REV_DEL,
                     GQFILE.GQ_MISC_KEY_FLD,
                     GQFILE.GQ_REV_PDUE_DAYS,
                     GQFILE.GQ_REV_BAL,
                     GQFILE.GQ_SEQ,
                     GQFILE.GQ_BRNO,
                     GQFILE.GQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN GQ2_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGQ2-WBEG-COLLID
                          QGQ2-WEND-COLLID
                          QGQ2-WBEG-SEQ-FG
                          QGQ2-WEND-SEQ-FG
                          QGQ2-WBEG-DATE  
                          QGQ2-WEND-DATE  
                          QGQ2-WBEG-MISC-KEY-FLD
                          QGQ2-WEND-MISC-KEY-FLD.
      
           MOVE ZEROES TO QGQ2-WBEG-START-TIME
                          QGQ2-WEND-START-TIME
                          QGQ2-WBEG-REV-DEL
                          QGQ2-WEND-REV-DEL
                          QGQ2-WBEG-REV-PDUE-DAYS
                          QGQ2-WEND-REV-PDUE-DAYS
                          QGQ2-WBEG-REV-BAL
                          QGQ2-WEND-REV-BAL
                          QGQ2-WBEG-SEQ
                          QGQ2-WEND-SEQ.

           MOVE STAT---GOOD TO GQ2-CURSOR-GR-STAT.

      *-------------------------------------------------------
       READ-GQ2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GQ-PATH-SQL TO E-FILE.
           PERFORM LOAD-GQ2-NON-COMP-KEY.
           MOVE SPWSGQ-GQ2-KEY-NON-COMP TO E-KEYX.

           INITIALIZE QGQ-REC.

           IF ( GQ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GQ2_CURSOR INTO :QGQ-REC
              END-EXEC
           ELSE
           IF ( GQ2-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH GQ2_CURSOR_GR INTO :QGQ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GQ2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GQ-FIELDS.

      *-----------------------------------------------------------------
       WRITE-GQ2-FILE.

           ACCEPT GQ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GQ-PATH-SQL TO E-FILE.
           PERFORM LOAD-GQ2-NON-COMP-KEY.
           MOVE SPWSGQ-GQ2-KEY-NON-COMP TO E-KEYX.
           PERFORM SET-GQ-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GQFILE
             (GQFILE.GQ_BRNO,
              GQFILE.GQ_ACCTNO,
              GQFILE.GQ_COLLID,
              GQFILE.GQ_SEQ_FG,
              GQFILE.GQ_DATE,
              GQFILE.GQ_START_TIME,
              GQFILE.GQ_REV_DEL,
              GQFILE.GQ_MISC_KEY_FLD,
              GQFILE.GQ_REV_PDUE_DAYS,
              GQFILE.GQ_REV_BAL,
              GQFILE.GQ_SEQ,
              GQFILE.GQ_LTOUCH_DATE,
              GQFILE.GQ_WORK_COLLID,
              GQFILE.GQ_WORKED_FG,
              GQFILE.GQ_IN_PROCESS,
              GQFILE.GQ_DATE_QUEUED,
              GQFILE.GQ_TIME_ZONE_ERROR)
            VALUES
             (:QGQ-BRNO,
              :QGQ-ACCTNO,
              :QGQ-COLLID,
              :QGQ-SEQ-FG,
              :QGQ-DATE,
              :QGQ-START-TIME,
              :QGQ-REV-DEL,
              :QGQ-MISC-KEY-FLD,
              :QGQ-REV-PDUE-DAYS,
              :QGQ-REV-BAL,
              :QGQ-SEQ,
              :QGQ-LTOUCH-DATE,
              :QGQ-WORK-COLLID,
              :QGQ-WORKED-FG,
              :QGQ-IN-PROCESS,
              :QGQ-DATE-QUEUED,
              :QGQ-TIME-ZONE-ERROR)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GQ2-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GQ2-FILE.

           ACCEPT GQ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GQ-PATH-SQL TO E-FILE.
           PERFORM LOAD-GQ2-NON-COMP-KEY.
           MOVE SPWSGQ-GQ2-KEY-NON-COMP TO E-KEYX.
           PERFORM SET-GQ-FIELDS.

           EXEC SQL
            UPDATE DBO.GQFILE
             SET
              GQFILE.GQ_BRNO = :QGQ-BRNO,
              GQFILE.GQ_ACCTNO = :QGQ-ACCTNO,
              GQFILE.GQ_COLLID = :QGQ-COLLID,
              GQFILE.GQ_SEQ_FG = :QGQ-SEQ-FG,
              GQFILE.GQ_DATE =
                 :QGQ-DATE,
              GQFILE.GQ_START_TIME = :QGQ-START-TIME,
              GQFILE.GQ_REV_DEL = :QGQ-REV-DEL,
              GQFILE.GQ_MISC_KEY_FLD = :QGQ-MISC-KEY-FLD,
              GQFILE.GQ_REV_PDUE_DAYS = :QGQ-REV-PDUE-DAYS,
              GQFILE.GQ_REV_BAL = :QGQ-REV-BAL,
              GQFILE.GQ_SEQ = :QGQ-SEQ,
              GQFILE.GQ_LTOUCH_DATE =
                 :QGQ-LTOUCH-DATE,
              GQFILE.GQ_WORK_COLLID = :QGQ-WORK-COLLID,
              GQFILE.GQ_WORKED_FG = :QGQ-WORKED-FG,
              GQFILE.GQ_IN_PROCESS = :QGQ-IN-PROCESS,
              GQFILE.GQ_DATE_QUEUED = :QGQ-DATE-QUEUED,
              GQFILE.GQ_TIME_ZONE_ERROR = :QGQ-TIME-ZONE-ERROR
            WHERE GQFILE.GQ_BRNO = :QGQ-BRNO
              AND GQFILE.GQ_ACCTNO = :QGQ-ACCTNO
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NOTE: THE WHERE CONSTRAINT IN THE ABOVE REWRITE NEEDS TO USE
      *       THE PRIMARY KEY WHERE CONSTRAINT; CANNOT DO A REWRITE ON
      *       THE SECONDARY KEY SINCE THE Q??#-KEY IS CHANGED AND WILL
      *       NOT FIND THE ORIGINAL KEY, AS WELL AS Q??#-KEY COULD
      *       MATCH ANOTHER RECORD/ROW THAT SHOULD NOT CHANGE!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GQ2-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GQ2-FILE.

           PERFORM DELETE-IT.
           MOVE GQ-PATH-SQL TO E-FILE.
           PERFORM LOAD-GQ2-NON-COMP-KEY.
           MOVE SPWSGQ-GQ2-KEY-NON-COMP TO E-KEYX.
           PERFORM SET-GQ-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GQFILE
            WHERE GQFILE.GQ_COLLID = :QGQ-COLLID
              AND GQFILE.GQ_SEQ_FG = :QGQ-SEQ-FG
              AND GQFILE.GQ_DATE = :QGQ-DATE
              AND GQFILE.GQ_START_TIME = :QGQ-START-TIME
              AND GQFILE.GQ_REV_DEL = :QGQ-REV-DEL
              AND GQFILE.GQ_MISC_KEY_FLD = :QGQ-MISC-KEY-FLD
              AND GQFILE.GQ_REV_PDUE_DAYS = :QGQ-REV-PDUE-DAYS
              AND GQFILE.GQ_REV_BAL = :QGQ-REV-BAL
              AND GQFILE.GQ_SEQ = :QGQ-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GQ2-FILE.

      *-----------------------------------------------------------------
       CLOSE-GQ2-FILE.

           PERFORM CLOSE-IT.

           IF ( GQ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GQ2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GQ2-CURSOR-STAT.

           IF ( GQ2-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE GQ2_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO GQ2-CURSOR-GR-STAT.

