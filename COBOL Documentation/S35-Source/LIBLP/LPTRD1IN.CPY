      * COPYMEMBER: LIBLP/LPTRD1IN
       LOAD-TRD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRD-ALL-DATA
              MOVE TRD-ALL-PATH             TO TRD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRD-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRD-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRD-ALL-DATA
              MOVE TRD-ALL-PATH             TO TRD-PATH.

      *-----------------------------------------------------------------
       OPEN-TRD1-FILE.
           PERFORM LOAD-TRD1-FILE.
           PERFORM OPEN-IT.
           MOVE TRD-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.
      *-----------------------------------------------------------------
       START-TRD1-FILE.
           PERFORM START-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.

           IF ( TRD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRD1-FILE.

           IF ( QTRD1-WEND-BRNO = ZEROES    ) OR
              ( QTRD1-WEND-BRNO NOT NUMERIC )
              MOVE TRD-PATH-OWNBR       TO QTRD1-WBEG-BRNO
              MOVE TRD-PATH-OWNBR       TO QTRD1-WEND-BRNO
              MOVE TD-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WEND-DATE.


           EXEC SQL
            DECLARE TRD1_CURSOR CURSOR FOR
            SELECT TRDFILE.TD_BRNO,
                   CAST(TRDFILE.TD_DATE AS VARCHAR(10)),
                   CAST(TRDFILE.TD_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRDFILE.TD_DEPOSIT,
                   TRDFILE.TD_CHECK_DEPOSIT,
                   TRDFILE.TD_BANK,
                   TRDFILE.TD_CASH,
                   TRDFILE.TD_GLFG,
                   TRDFILE.TD_GLOVERSHORT,
                   TRDFILE.TD_OVERSHORT,
                   TRDFILE.TD_USERID,
                   CAST(TRDFILE.TD_POSTDATE
                                                  AS VARCHAR(10)),
                   TRDFILE.TD_POSTTIME
            FROM DBO.TRDFILE
            WHERE TRDFILE.TD_BRNO  = :QTRD1-WBEG-BRNO
              AND TRDFILE.TD_DATE >= :QTRD1-WBEG-DATE
              AND TRDFILE.TD_DATE <= :QTRD1-WEND-DATE
            ORDER BY TRDFILE.TD_BRNO,
                     TRDFILE.TD_DATE
           END-EXEC.

           EXEC SQL
               OPEN TRD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRD1-WBEG-BRNO 
                          QTRD1-WEND-BRNO.
 
           MOVE SPACES TO QTRD1-WBEG-DATE
                          QTRD1-WEND-DATE.

           MOVE STAT---GOOD TO TRD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRD1-FILE.
           PERFORM READ-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO. 
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           INITIALIZE QTRD-REC.

           MOVE TD-BRNO   TO QTD-BRNO.
           MOVE TD-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTD-DATE.

           EXEC SQL
            SELECT TRDFILE.TD_BRNO,
                   CAST(TRDFILE.TD_DATE AS VARCHAR(10)),
                   CAST(TRDFILE.TD_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRDFILE.TD_DEPOSIT,
                   TRDFILE.TD_CHECK_DEPOSIT,
                   TRDFILE.TD_BANK,
                   TRDFILE.TD_CASH,
                   TRDFILE.TD_GLFG,
                   TRDFILE.TD_GLOVERSHORT,
                   TRDFILE.TD_OVERSHORT,
                   TRDFILE.TD_USERID,
                   CAST(TRDFILE.TD_POSTDATE
                                                  AS VARCHAR(10)),
                   TRDFILE.TD_POSTTIME
            INTO :QTRD-REC
            FROM DBO.TRDFILE
            WHERE TRDFILE.TD_BRNO = :QTD-BRNO
              AND TRDFILE.TD_DATE = :QTD-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRD-FIELDS.

      *-----------------------------------------------------------------
       READ-TRD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           INITIALIZE QTRD-REC.

           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRD1_CURSOR INTO :QTRD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRD-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRD1-FILE.

           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           ACCEPT TD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           PERFORM SET-TRD-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRDFILE
             (TRDFILE.TD_BRNO,
              TRDFILE.TD_DATE,
              TRDFILE.TD_LTOUCH_DATE,
              TRDFILE.TD_DEPOSIT,
              TRDFILE.TD_CHECK_DEPOSIT,
              TRDFILE.TD_BANK,
              TRDFILE.TD_CASH,
              TRDFILE.TD_GLFG,
              TRDFILE.TD_GLOVERSHORT,
              TRDFILE.TD_OVERSHORT,
              TRDFILE.TD_USERID,
              TRDFILE.TD_POSTDATE,
              TRDFILE.TD_POSTTIME)
            VALUES
             (:QTD-BRNO,
              :QTD-DATE,
              :QTD-LTOUCH-DATE,
              :QTD-DEPOSIT,
              :QTD-CHECK-DEPOSIT,
              :QTD-BANK,
              :QTD-CASH,
              :QTD-GLFG,
              :QTD-GLOVERSHORT,
              :QTD-OVERSHORT,
              :QTD-USERID,
              :QTD-POSTDATE,
              :QTD-POSTTIME)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRD1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRD1-FILE.

           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           ACCEPT TD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           PERFORM SET-TRD-FIELDS.

           EXEC SQL
            UPDATE DBO.TRDFILE
             SET
              TRDFILE.TD_BRNO = :QTD-BRNO,
              TRDFILE.TD_DATE =
                 :QTD-DATE,
              TRDFILE.TD_LTOUCH_DATE =
                 :QTD-LTOUCH-DATE,
              TRDFILE.TD_DEPOSIT = :QTD-DEPOSIT,
              TRDFILE.TD_CHECK_DEPOSIT = :QTD-CHECK-DEPOSIT,
              TRDFILE.TD_BANK = :QTD-BANK,
              TRDFILE.TD_CASH = :QTD-CASH,
              TRDFILE.TD_GLFG = :QTD-GLFG,
              TRDFILE.TD_GLOVERSHORT = :QTD-GLOVERSHORT,
              TRDFILE.TD_OVERSHORT = :QTD-OVERSHORT,
              TRDFILE.TD_USERID = :QTD-USERID,
              TRDFILE.TD_POSTDATE = :QTD-POSTDATE,
              TRDFILE.TD_POSTTIME = :QTD-POSTTIME
            WHERE TRDFILE.TD_BRNO = :QTD-BRNO
              AND TRDFILE.TD_DATE = :QTD-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRD1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRD1-FILE.

           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           PERFORM DELETE-IT.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           PERFORM SET-TRD-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRDFILE
            WHERE TRDFILE.TD_BRNO = :QTD-BRNO
              AND TRDFILE.TD_DATE = :QTD-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRD1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRD1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRD1-CURSOR-STAT.

