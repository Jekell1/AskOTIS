      * COPYMEMBER: LIBLP/LPTRI1IN
       LOAD-TRI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRI-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRI-ALL-DATA
              MOVE TRI-ALL-PATH             TO TRI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRI-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRI-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRI-ALL-DATA
              MOVE TRI-ALL-PATH             TO TRI-PATH.

      *-----------------------------------------------------------------
       OPEN-TRI1-FILE.
           PERFORM LOAD-TRI1-FILE.
           PERFORM OPEN-IT.
           MOVE TRI-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRI1-FILE.
           PERFORM START-IT.
           MOVE TRI-PATH-OWNBR  TO TI-BRNO.
           MOVE TRI-PATH-SQL    TO E-FILE.
           MOVE TRI1-KEY        TO E-KEYX.

           IF ( TRI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRI1-FILE.

           IF ( QTRI1-WEND-BRNO = ZEROES    ) OR
              ( QTRI1-WEND-BRNO NOT NUMERIC )
              MOVE TRI-PATH-OWNBR       TO QTRI1-WBEG-BRNO
              MOVE TRI-PATH-OWNBR       TO QTRI1-WEND-BRNO
              MOVE TI-KEY-DATE          TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRI1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRI1-WEND-DATE
              MOVE TI-SEQ               TO QTRI1-WBEG-SEQ
              MOVE ALL "9"              TO QTRI1-WEND-SEQ.


           EXEC SQL
            DECLARE TRI1_CURSOR CURSOR FOR
            SELECT TRIFILE.TI_BRNO,
                   CAST(TRIFILE.TI_KEY_DATE
                                                  AS VARCHAR(10)),
                   TRIFILE.TI_SEQ,
                   CAST(TRIFILE.TI_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRIFILE.TI_WAIS,
                   TRIFILE.TI_FILLER,
                   TRIFILE.TI_TYPE,
                   TRIFILE.TI_AMOUNT,
                   TRIFILE.TI_CLAIM_NUMBER,
                   TRIFILE.TI_FILLER2,
                   TRIFILE.TI_LNAME,
                   TRIFILE.TI_FNAME,
                   CAST(TRIFILE.TI_DATE AS VARCHAR(10)),
                   TRIFILE.TI_DRAFT,
                   TRIFILE.TI_FILLER3,
                   TRIFILE.TI_PRODUCER,
                   TRIFILE.TI_USERID,
                   TRIFILE.TI_POSTTIME,
                   CAST(TRIFILE.TI_POSTDATE
                                                  AS VARCHAR(10)),
                   TRIFILE.TI_INSCOMP,
                   TRIFILE.TI_DAYS_PAID,
                   TRIFILE.TI_PMT_TYPE,
                   CAST(TRIFILE.TI_FROM_DATE
                                                  AS VARCHAR(10)),
                   CAST(TRIFILE.TI_TO_DATE AS VARCHAR(10))
            FROM DBO.TRIFILE
            WHERE TRIFILE.TI_BRNO  = :QTRI1-WBEG-BRNO
              AND TRIFILE.TI_KEY_DATE >= :QTRI1-WBEG-DATE 
              AND TRIFILE.TI_KEY_DATE <= :QTRI1-WEND-DATE
              AND TRIFILE.TI_SEQ >= :QTRI1-WBEG-SEQ
              AND TRIFILE.TI_SEQ <= :QTRI1-WEND-SEQ
            ORDER BY TRIFILE.TI_BRNO,
                     TRIFILE.TI_KEY_DATE,
                     TRIFILE.TI_SEQ
           END-EXEC.

           EXEC SQL
               OPEN TRI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRI1-WBEG-BRNO 
                          QTRI1-WEND-BRNO
                          QTRI1-WBEG-SEQ
                          QTRI1-WEND-SEQ.
 
           MOVE SPACES TO QTRI1-WBEG-DATE
                          QTRI1-WEND-DATE.

           MOVE STAT---GOOD TO TRI1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRI1-FILE.
           PERFORM READ-IT.
           MOVE TRI-PATH-OWNBR  TO TI-BRNO. 
           MOVE TRI-PATH-SQL    TO E-FILE.
           MOVE TRI1-KEY        TO E-KEYX.
           INITIALIZE QTRI-REC.

           MOVE TI-BRNO             TO QTI-BRNO.
           MOVE TI-KEY-DATE         TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTI-KEY-DATE.
           MOVE TI-SEQ              TO QTI-SEQ.

           EXEC SQL
            SELECT TRIFILE.TI_BRNO,
                   CAST(TRIFILE.TI_KEY_DATE
                                                  AS VARCHAR(10)),
                   TRIFILE.TI_SEQ,
                   CAST(TRIFILE.TI_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRIFILE.TI_WAIS,
                   TRIFILE.TI_FILLER,
                   TRIFILE.TI_TYPE,
                   TRIFILE.TI_AMOUNT,
                   TRIFILE.TI_CLAIM_NUMBER,
                   TRIFILE.TI_FILLER2,
                   TRIFILE.TI_LNAME,
                   TRIFILE.TI_FNAME,
                   CAST(TRIFILE.TI_DATE AS VARCHAR(10)),
                   TRIFILE.TI_DRAFT,
                   TRIFILE.TI_FILLER3,
                   TRIFILE.TI_PRODUCER,
                   TRIFILE.TI_USERID,
                   TRIFILE.TI_POSTTIME,
                   CAST(TRIFILE.TI_POSTDATE
                                                  AS VARCHAR(10)),
                   TRIFILE.TI_INSCOMP,
                   TRIFILE.TI_DAYS_PAID,
                   TRIFILE.TI_PMT_TYPE,
                   CAST(TRIFILE.TI_FROM_DATE
                                                  AS VARCHAR(10)),
                   CAST(TRIFILE.TI_TO_DATE AS VARCHAR(10))
            INTO :QTRI-REC
            FROM DBO.TRIFILE
            WHERE TRIFILE.TI_BRNO = :QTI-BRNO
              AND TRIFILE.TI_KEY_DATE = :QTI-KEY-DATE
              AND TRIFILE.TI_SEQ = :QTI-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRI1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRI-FIELDS.

      *-----------------------------------------------------------------
       READ-TRI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRI-PATH-OWNBR  TO TI-BRNO.
           MOVE TRI-PATH-SQL    TO E-FILE.
           MOVE TRI1-KEY        TO E-KEYX.
           INITIALIZE QTRI-REC.

           IF ( TRI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRI1_CURSOR INTO :QTRI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRI-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRI1-FILE.

           MOVE TRI-PATH-OWNBR  TO TI-BRNO.
           ACCEPT TI-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRI-PATH-SQL    TO E-FILE.
           MOVE TRI1-KEY        TO E-KEYX.
           PERFORM SET-TRI-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRIFILE
             (TRIFILE.TI_BRNO,
              TRIFILE.TI_KEY_DATE,
              TRIFILE.TI_SEQ,
              TRIFILE.TI_LTOUCH_DATE,
              TRIFILE.TI_WAIS,
              TRIFILE.TI_FILLER,
              TRIFILE.TI_TYPE,
              TRIFILE.TI_AMOUNT,
              TRIFILE.TI_CLAIM_NUMBER,
              TRIFILE.TI_FILLER2,
              TRIFILE.TI_LNAME,
              TRIFILE.TI_FNAME,
              TRIFILE.TI_DATE,
              TRIFILE.TI_DRAFT,
              TRIFILE.TI_FILLER3,
              TRIFILE.TI_PRODUCER,
              TRIFILE.TI_USERID,
              TRIFILE.TI_POSTTIME,
              TRIFILE.TI_POSTDATE,
              TRIFILE.TI_INSCOMP,
              TRIFILE.TI_DAYS_PAID,
              TRIFILE.TI_PMT_TYPE,
              TRIFILE.TI_FROM_DATE,
              TRIFILE.TI_TO_DATE)
            VALUES
             (:QTI-BRNO,
              :QTI-KEY-DATE,
              :QTI-SEQ,
              :QTI-LTOUCH-DATE,
              :QTI-WAIS,
              :QTI-FILLER,
              :QTI-TYPE,
              :QTI-AMOUNT,
              :QTI-CLAIM-NUMBER,
              :QTI-FILLER2,
              :QTI-LNAME,
              :QTI-FNAME,
              :QTI-DATE,
              :QTI-DRAFT,
              :QTI-FILLER3,
              :QTI-PRODUCER,
              :QTI-USERID,
              :QTI-POSTTIME,
              :QTI-POSTDATE,
              :QTI-INSCOMP,
              :QTI-DAYS-PAID,
              :QTI-PMT-TYPE,
              :QTI-FROM-DATE,
              :QTI-TO-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRI1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRI1-FILE.

           MOVE TRI-PATH-OWNBR  TO TI-BRNO.
           ACCEPT TI-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRI-PATH-SQL    TO E-FILE.
           MOVE TRI1-KEY        TO E-KEYX.
           PERFORM SET-TRI-FIELDS.

           EXEC SQL
            UPDATE DBO.TRIFILE
             SET
              TRIFILE.TI_BRNO = :QTI-BRNO,
              TRIFILE.TI_KEY_DATE = :QTI-KEY-DATE,
              TRIFILE.TI_SEQ = :QTI-SEQ,
              TRIFILE.TI_LTOUCH_DATE = :QTI-LTOUCH-DATE,
              TRIFILE.TI_WAIS = :QTI-WAIS,
              TRIFILE.TI_FILLER = :QTI-FILLER,
              TRIFILE.TI_TYPE = :QTI-TYPE,
              TRIFILE.TI_AMOUNT = :QTI-AMOUNT,
              TRIFILE.TI_CLAIM_NUMBER = :QTI-CLAIM-NUMBER,
              TRIFILE.TI_FILLER2 = :QTI-FILLER2,
              TRIFILE.TI_LNAME = :QTI-LNAME,
              TRIFILE.TI_FNAME = :QTI-FNAME,
              TRIFILE.TI_DATE = :QTI-DATE,
              TRIFILE.TI_DRAFT = :QTI-DRAFT,
              TRIFILE.TI_FILLER3 = :QTI-FILLER3,
              TRIFILE.TI_PRODUCER = :QTI-PRODUCER,
              TRIFILE.TI_USERID = :QTI-USERID,
              TRIFILE.TI_POSTTIME = :QTI-POSTTIME,
              TRIFILE.TI_POSTDATE = :QTI-POSTDATE,
              TRIFILE.TI_INSCOMP = :QTI-INSCOMP,
              TRIFILE.TI_DAYS_PAID = :QTI-DAYS-PAID,
              TRIFILE.TI_PMT_TYPE = :QTI-PMT-TYPE,
              TRIFILE.TI_FROM_DATE = :QTI-FROM-DATE,
              TRIFILE.TI_TO_DATE = :QTI-TO-DATE
            WHERE TRIFILE.TI_BRNO = :QTI-BRNO
              AND TRIFILE.TI_KEY_DATE = :QTI-KEY-DATE
              AND TRIFILE.TI_SEQ = :QTI-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRI1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRI1-FILE.

           MOVE TRI-PATH-OWNBR  TO TI-BRNO.
           PERFORM DELETE-IT.
           MOVE TRI-PATH-SQL    TO E-FILE.
           MOVE TRI1-KEY        TO E-KEYX.
           PERFORM SET-TRI-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRIFILE
            WHERE TRIFILE.TI_BRNO = :QTI-BRNO
              AND TRIFILE.TI_KEY_DATE = :QTI-KEY-DATE
              AND TRIFILE.TI_SEQ = :QTI-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRI1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRI1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRI1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRI1-CURSOR-STAT.

