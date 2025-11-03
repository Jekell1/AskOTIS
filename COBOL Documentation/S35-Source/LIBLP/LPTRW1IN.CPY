      * COPYMEMBER: LIBLP/LPTRW1IN
       LOAD-TRW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRW-ALL-DATA
              MOVE TRW-ALL-PATH             TO TRW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRW-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRW-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRW-ALL-DATA
              MOVE TRW-ALL-PATH             TO TRW-PATH.

      *-----------------------------------------------------------------
       OPEN-TRW1-FILE.
           PERFORM LOAD-TRW1-FILE.
           PERFORM OPEN-IT.
           MOVE TRW-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRW1-FILE.
           PERFORM START-IT.
           MOVE TRW-PATH-OWNBR  TO TW-BRNO.
           MOVE TRW-PATH-SQL    TO E-FILE.
           MOVE TRW1-KEY        TO E-KEYX.

           IF ( TRW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRW1-FILE.

           IF ( QTRW1-WEND-TILLNO = ZEROES    ) OR
              ( QTRW1-WEND-TILLNO NOT NUMERIC )
              MOVE TRW-PATH-OWNBR        TO QTRW1-WBEG-BRNO
              MOVE TRW-PATH-OWNBR        TO QTRW1-WEND-BRNO
              MOVE TW-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRW1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRW1-WEND-DATE
              MOVE TW-TILLNO            TO QTRW1-WBEG-TILLNO
              MOVE ALL "9"              TO QTRW1-WEND-TILLNO.


           EXEC SQL
            DECLARE TRW1_CURSOR CURSOR FOR
            SELECT TRWFILE.TW_BRNO,
                   CAST(TRWFILE.TW_DATE AS VARCHAR(10)),
                   TRWFILE.TW_TILLNO,
                   CAST(TRWFILE.TW_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRWFILE.TW_POSTNAME_1,
                   TRWFILE.TW_TIME_1,
                   TRWFILE.TW_AMOUNT_1,
                   TRWFILE.TW_OVERSHORT_1,
                   TRWFILE.TW_POSTNAME_2,
                   TRWFILE.TW_TIME_2,
                   TRWFILE.TW_AMOUNT_2,
                   TRWFILE.TW_OVERSHORT_2,
                   TRWFILE.TW_POSTNAME_3,
                   TRWFILE.TW_TIME_3,
                   TRWFILE.TW_AMOUNT_3,
                   TRWFILE.TW_OVERSHORT_3,
                   TRWFILE.TW_POSTNAME_4,
                   TRWFILE.TW_TIME_4,
                   TRWFILE.TW_AMOUNT_4,
                   TRWFILE.TW_OVERSHORT_4,
                   TRWFILE.TW_POSTNAME_5,
                   TRWFILE.TW_TIME_5,
                   TRWFILE.TW_AMOUNT_5,
                   TRWFILE.TW_OVERSHORT_5,
                   TRWFILE.TW_POSTNAME_6,
                   TRWFILE.TW_TIME_6,
                   TRWFILE.TW_AMOUNT_6,
                   TRWFILE.TW_OVERSHORT_6,
                   TRWFILE.TW_POSTNAME_7,
                   TRWFILE.TW_TIME_7,
                   TRWFILE.TW_AMOUNT_7,
                   TRWFILE.TW_OVERSHORT_7,
                   TRWFILE.TW_POSTNAME_8,
                   TRWFILE.TW_TIME_8,
                   TRWFILE.TW_AMOUNT_8,
                   TRWFILE.TW_OVERSHORT_8,
                   TRWFILE.TW_POSTNAME_9,
                   TRWFILE.TW_TIME_9,
                   TRWFILE.TW_AMOUNT_9,
                   TRWFILE.TW_OVERSHORT_9,
                   TRWFILE.TW_POSTNAME_10,
                   TRWFILE.TW_TIME_10,
                   TRWFILE.TW_AMOUNT_10,
                   TRWFILE.TW_OVERSHORT_10,
                   TRWFILE.TW_POSTNAME_11,
                   TRWFILE.TW_TIME_11,
                   TRWFILE.TW_AMOUNT_11,
                   TRWFILE.TW_OVERSHORT_11,
                   TRWFILE.TW_POSTNAME_12,
                   TRWFILE.TW_TIME_12,
                   TRWFILE.TW_AMOUNT_12,
                   TRWFILE.TW_OVERSHORT_12,
                   TRWFILE.TW_POSTNAME_13,
                   TRWFILE.TW_TIME_13,
                   TRWFILE.TW_AMOUNT_13,
                   TRWFILE.TW_OVERSHORT_13,
                   TRWFILE.TW_POSTNAME_14,
                   TRWFILE.TW_TIME_14,
                   TRWFILE.TW_AMOUNT_14,
                   TRWFILE.TW_OVERSHORT_14,
                   TRWFILE.TW_POSTNAME_15,
                   TRWFILE.TW_TIME_15,
                   TRWFILE.TW_AMOUNT_15,
                   TRWFILE.TW_OVERSHORT_15,
                   TRWFILE.TW_POSTNAME_16,
                   TRWFILE.TW_TIME_16,
                   TRWFILE.TW_AMOUNT_16,
                   TRWFILE.TW_OVERSHORT_16,
                   TRWFILE.TW_POSTNAME_17,
                   TRWFILE.TW_TIME_17,
                   TRWFILE.TW_AMOUNT_17,
                   TRWFILE.TW_OVERSHORT_17,
                   TRWFILE.TW_POSTNAME_18,
                   TRWFILE.TW_TIME_18,
                   TRWFILE.TW_AMOUNT_18,
                   TRWFILE.TW_OVERSHORT_18,
                   TRWFILE.TW_POSTNAME_19,
                   TRWFILE.TW_TIME_19,
                   TRWFILE.TW_AMOUNT_19,
                   TRWFILE.TW_OVERSHORT_19,
                   TRWFILE.TW_POSTNAME_20,
                   TRWFILE.TW_TIME_20,
                   TRWFILE.TW_AMOUNT_20,
                   TRWFILE.TW_OVERSHORT_20
            FROM DBO.TRWFILE
            WHERE TRWFILE.TW_BRNO  = :QTRW1-WBEG-BRNO
              AND TRWFILE.TW_DATE >= :QTRW1-WBEG-DATE
              AND TRWFILE.TW_DATE <= :QTRW1-WEND-DATE
              AND TRWFILE.TW_TILLNO >= :QTRW1-WBEG-TILLNO
              AND TRWFILE.TW_TILLNO <= :QTRW1-WEND-TILLNO
            ORDER BY TRWFILE.TW_BRNO,
                     TRWFILE.TW_DATE,
                     TRWFILE.TW_TILLNO
           END-EXEC.

           EXEC SQL
               OPEN TRW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRW1-WBEG-BRNO
                          QTRW1-WEND-BRNO
                          QTRW1-WBEG-TILLNO
                          QTRW1-WEND-TILLNO.

           MOVE SPACES TO QTRW1-WBEG-DATE
                          QTRW1-WEND-DATE.

           MOVE STAT---GOOD TO TRW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRW1-FILE.
           PERFORM READ-IT.
           MOVE TRW-PATH-OWNBR  TO TW-BRNO. 
           MOVE TRW-PATH-SQL    TO E-FILE.
           MOVE TRW1-KEY        TO E-KEYX.
           INITIALIZE QTRW-REC.

           MOVE TW-BRNO             TO QTW-BRNO.
           MOVE TW-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTW-DATE.
           MOVE TW-TILLNO           TO QTW-TILLNO.

           EXEC SQL
            SELECT TRWFILE.TW_BRNO,
                   CAST(TRWFILE.TW_DATE AS VARCHAR(10)),
                   TRWFILE.TW_TILLNO,
                   CAST(TRWFILE.TW_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRWFILE.TW_POSTNAME_1,
                   TRWFILE.TW_TIME_1,
                   TRWFILE.TW_AMOUNT_1,
                   TRWFILE.TW_OVERSHORT_1,
                   TRWFILE.TW_POSTNAME_2,
                   TRWFILE.TW_TIME_2,
                   TRWFILE.TW_AMOUNT_2,
                   TRWFILE.TW_OVERSHORT_2,
                   TRWFILE.TW_POSTNAME_3,
                   TRWFILE.TW_TIME_3,
                   TRWFILE.TW_AMOUNT_3,
                   TRWFILE.TW_OVERSHORT_3,
                   TRWFILE.TW_POSTNAME_4,
                   TRWFILE.TW_TIME_4,
                   TRWFILE.TW_AMOUNT_4,
                   TRWFILE.TW_OVERSHORT_4,
                   TRWFILE.TW_POSTNAME_5,
                   TRWFILE.TW_TIME_5,
                   TRWFILE.TW_AMOUNT_5,
                   TRWFILE.TW_OVERSHORT_5,
                   TRWFILE.TW_POSTNAME_6,
                   TRWFILE.TW_TIME_6,
                   TRWFILE.TW_AMOUNT_6,
                   TRWFILE.TW_OVERSHORT_6,
                   TRWFILE.TW_POSTNAME_7,
                   TRWFILE.TW_TIME_7,
                   TRWFILE.TW_AMOUNT_7,
                   TRWFILE.TW_OVERSHORT_7,
                   TRWFILE.TW_POSTNAME_8,
                   TRWFILE.TW_TIME_8,
                   TRWFILE.TW_AMOUNT_8,
                   TRWFILE.TW_OVERSHORT_8,
                   TRWFILE.TW_POSTNAME_9,
                   TRWFILE.TW_TIME_9,
                   TRWFILE.TW_AMOUNT_9,
                   TRWFILE.TW_OVERSHORT_9,
                   TRWFILE.TW_POSTNAME_10,
                   TRWFILE.TW_TIME_10,
                   TRWFILE.TW_AMOUNT_10,
                   TRWFILE.TW_OVERSHORT_10,
                   TRWFILE.TW_POSTNAME_11,
                   TRWFILE.TW_TIME_11,
                   TRWFILE.TW_AMOUNT_11,
                   TRWFILE.TW_OVERSHORT_11,
                   TRWFILE.TW_POSTNAME_12,
                   TRWFILE.TW_TIME_12,
                   TRWFILE.TW_AMOUNT_12,
                   TRWFILE.TW_OVERSHORT_12,
                   TRWFILE.TW_POSTNAME_13,
                   TRWFILE.TW_TIME_13,
                   TRWFILE.TW_AMOUNT_13,
                   TRWFILE.TW_OVERSHORT_13,
                   TRWFILE.TW_POSTNAME_14,
                   TRWFILE.TW_TIME_14,
                   TRWFILE.TW_AMOUNT_14,
                   TRWFILE.TW_OVERSHORT_14,
                   TRWFILE.TW_POSTNAME_15,
                   TRWFILE.TW_TIME_15,
                   TRWFILE.TW_AMOUNT_15,
                   TRWFILE.TW_OVERSHORT_15,
                   TRWFILE.TW_POSTNAME_16,
                   TRWFILE.TW_TIME_16,
                   TRWFILE.TW_AMOUNT_16,
                   TRWFILE.TW_OVERSHORT_16,
                   TRWFILE.TW_POSTNAME_17,
                   TRWFILE.TW_TIME_17,
                   TRWFILE.TW_AMOUNT_17,
                   TRWFILE.TW_OVERSHORT_17,
                   TRWFILE.TW_POSTNAME_18,
                   TRWFILE.TW_TIME_18,
                   TRWFILE.TW_AMOUNT_18,
                   TRWFILE.TW_OVERSHORT_18,
                   TRWFILE.TW_POSTNAME_19,
                   TRWFILE.TW_TIME_19,
                   TRWFILE.TW_AMOUNT_19,
                   TRWFILE.TW_OVERSHORT_19,
                   TRWFILE.TW_POSTNAME_20,
                   TRWFILE.TW_TIME_20,
                   TRWFILE.TW_AMOUNT_20,
                   TRWFILE.TW_OVERSHORT_20
            INTO :QTRW-REC
            FROM DBO.TRWFILE
            WHERE TRWFILE.TW_BRNO = :QTW-BRNO
              AND TRWFILE.TW_DATE = :QTW-DATE
              AND TRWFILE.TW_TILLNO = :QTW-TILLNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRW-FIELDS.

      *-----------------------------------------------------------------
       READ-TRW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRW-PATH-OWNBR  TO TW-BRNO.
           MOVE TRW-PATH-SQL    TO E-FILE.
           MOVE TRW1-KEY        TO E-KEYX.
           INITIALIZE QTRW-REC.

           IF ( TRW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRW1_CURSOR INTO :QTRW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRW-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRW1-FILE.

           MOVE TRW-PATH-OWNBR  TO TW-BRNO.
           ACCEPT TW-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRW-PATH-SQL    TO E-FILE.
           MOVE TRW1-KEY        TO E-KEYX.
           PERFORM SET-TRW-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRWFILE
             (TRWFILE.TW_BRNO,
              TRWFILE.TW_DATE,
              TRWFILE.TW_TILLNO,
              TRWFILE.TW_LTOUCH_DATE,
              TRWFILE.TW_POSTNAME_1,
              TRWFILE.TW_TIME_1,
              TRWFILE.TW_AMOUNT_1,
              TRWFILE.TW_OVERSHORT_1,
              TRWFILE.TW_POSTNAME_2,
              TRWFILE.TW_TIME_2,
              TRWFILE.TW_AMOUNT_2,
              TRWFILE.TW_OVERSHORT_2,
              TRWFILE.TW_POSTNAME_3,
              TRWFILE.TW_TIME_3,
              TRWFILE.TW_AMOUNT_3,
              TRWFILE.TW_OVERSHORT_3,
              TRWFILE.TW_POSTNAME_4,
              TRWFILE.TW_TIME_4,
              TRWFILE.TW_AMOUNT_4,
              TRWFILE.TW_OVERSHORT_4,
              TRWFILE.TW_POSTNAME_5,
              TRWFILE.TW_TIME_5,
              TRWFILE.TW_AMOUNT_5,
              TRWFILE.TW_OVERSHORT_5,
              TRWFILE.TW_POSTNAME_6,
              TRWFILE.TW_TIME_6,
              TRWFILE.TW_AMOUNT_6,
              TRWFILE.TW_OVERSHORT_6,
              TRWFILE.TW_POSTNAME_7,
              TRWFILE.TW_TIME_7,
              TRWFILE.TW_AMOUNT_7,
              TRWFILE.TW_OVERSHORT_7,
              TRWFILE.TW_POSTNAME_8,
              TRWFILE.TW_TIME_8,
              TRWFILE.TW_AMOUNT_8,
              TRWFILE.TW_OVERSHORT_8,
              TRWFILE.TW_POSTNAME_9,
              TRWFILE.TW_TIME_9,
              TRWFILE.TW_AMOUNT_9,
              TRWFILE.TW_OVERSHORT_9,
              TRWFILE.TW_POSTNAME_10,
              TRWFILE.TW_TIME_10,
              TRWFILE.TW_AMOUNT_10,
              TRWFILE.TW_OVERSHORT_10,
              TRWFILE.TW_POSTNAME_11,
              TRWFILE.TW_TIME_11,
              TRWFILE.TW_AMOUNT_11,
              TRWFILE.TW_OVERSHORT_11,
              TRWFILE.TW_POSTNAME_12,
              TRWFILE.TW_TIME_12,
              TRWFILE.TW_AMOUNT_12,
              TRWFILE.TW_OVERSHORT_12,
              TRWFILE.TW_POSTNAME_13,
              TRWFILE.TW_TIME_13,
              TRWFILE.TW_AMOUNT_13,
              TRWFILE.TW_OVERSHORT_13,
              TRWFILE.TW_POSTNAME_14,
              TRWFILE.TW_TIME_14,
              TRWFILE.TW_AMOUNT_14,
              TRWFILE.TW_OVERSHORT_14,
              TRWFILE.TW_POSTNAME_15,
              TRWFILE.TW_TIME_15,
              TRWFILE.TW_AMOUNT_15,
              TRWFILE.TW_OVERSHORT_15,
              TRWFILE.TW_POSTNAME_16,
              TRWFILE.TW_TIME_16,
              TRWFILE.TW_AMOUNT_16,
              TRWFILE.TW_OVERSHORT_16,
              TRWFILE.TW_POSTNAME_17,
              TRWFILE.TW_TIME_17,
              TRWFILE.TW_AMOUNT_17,
              TRWFILE.TW_OVERSHORT_17,
              TRWFILE.TW_POSTNAME_18,
              TRWFILE.TW_TIME_18,
              TRWFILE.TW_AMOUNT_18,
              TRWFILE.TW_OVERSHORT_18,
              TRWFILE.TW_POSTNAME_19,
              TRWFILE.TW_TIME_19,
              TRWFILE.TW_AMOUNT_19,
              TRWFILE.TW_OVERSHORT_19,
              TRWFILE.TW_POSTNAME_20,
              TRWFILE.TW_TIME_20,
              TRWFILE.TW_AMOUNT_20,
              TRWFILE.TW_OVERSHORT_20)
            VALUES
             (:QTW-BRNO,
              :QTW-DATE,
              :QTW-TILLNO,
              :QTW-LTOUCH-DATE,
              :QTW-POSTNAME-1,
              :QTW-TIME-1,
              :QTW-AMOUNT-1,
              :QTW-OVERSHORT-1,
              :QTW-POSTNAME-2,
              :QTW-TIME-2,
              :QTW-AMOUNT-2,
              :QTW-OVERSHORT-2,
              :QTW-POSTNAME-3,
              :QTW-TIME-3,
              :QTW-AMOUNT-3,
              :QTW-OVERSHORT-3,
              :QTW-POSTNAME-4,
              :QTW-TIME-4,
              :QTW-AMOUNT-4,
              :QTW-OVERSHORT-4,
              :QTW-POSTNAME-5,
              :QTW-TIME-5,
              :QTW-AMOUNT-5,
              :QTW-OVERSHORT-5,
              :QTW-POSTNAME-6,
              :QTW-TIME-6,
              :QTW-AMOUNT-6,
              :QTW-OVERSHORT-6,
              :QTW-POSTNAME-7,
              :QTW-TIME-7,
              :QTW-AMOUNT-7,
              :QTW-OVERSHORT-7,
              :QTW-POSTNAME-8,
              :QTW-TIME-8,
              :QTW-AMOUNT-8,
              :QTW-OVERSHORT-8,
              :QTW-POSTNAME-9,
              :QTW-TIME-9,
              :QTW-AMOUNT-9,
              :QTW-OVERSHORT-9,
              :QTW-POSTNAME-10,
              :QTW-TIME-10,
              :QTW-AMOUNT-10,
              :QTW-OVERSHORT-10,
              :QTW-POSTNAME-11,
              :QTW-TIME-11,
              :QTW-AMOUNT-11,
              :QTW-OVERSHORT-11,
              :QTW-POSTNAME-12,
              :QTW-TIME-12,
              :QTW-AMOUNT-12,
              :QTW-OVERSHORT-12,
              :QTW-POSTNAME-13,
              :QTW-TIME-13,
              :QTW-AMOUNT-13,
              :QTW-OVERSHORT-13,
              :QTW-POSTNAME-14,
              :QTW-TIME-14,
              :QTW-AMOUNT-14,
              :QTW-OVERSHORT-14,
              :QTW-POSTNAME-15,
              :QTW-TIME-15,
              :QTW-AMOUNT-15,
              :QTW-OVERSHORT-15,
              :QTW-POSTNAME-16,
              :QTW-TIME-16,
              :QTW-AMOUNT-16,
              :QTW-OVERSHORT-16,
              :QTW-POSTNAME-17,
              :QTW-TIME-17,
              :QTW-AMOUNT-17,
              :QTW-OVERSHORT-17,
              :QTW-POSTNAME-18,
              :QTW-TIME-18,
              :QTW-AMOUNT-18,
              :QTW-OVERSHORT-18,
              :QTW-POSTNAME-19,
              :QTW-TIME-19,
              :QTW-AMOUNT-19,
              :QTW-OVERSHORT-19,
              :QTW-POSTNAME-20,
              :QTW-TIME-20,
              :QTW-AMOUNT-20,
              :QTW-OVERSHORT-20)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRW1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRW1-FILE.

           MOVE TRW-PATH-OWNBR  TO TW-BRNO.
           ACCEPT TW-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRW-PATH-SQL    TO E-FILE.
           MOVE TRW1-KEY        TO E-KEYX.
           PERFORM SET-TRW-FIELDS.

           EXEC SQL
            UPDATE DBO.TRWFILE
             SET
              TRWFILE.TW_BRNO = :QTW-BRNO,
              TRWFILE.TW_DATE = :QTW-DATE,
              TRWFILE.TW_TILLNO = :QTW-TILLNO,
              TRWFILE.TW_LTOUCH_DATE = :QTW-LTOUCH-DATE,
              TRWFILE.TW_POSTNAME_1 = :QTW-POSTNAME-1,
              TRWFILE.TW_TIME_1 = :QTW-TIME-1,
              TRWFILE.TW_AMOUNT_1 = :QTW-AMOUNT-1,
              TRWFILE.TW_OVERSHORT_1 = :QTW-OVERSHORT-1,
              TRWFILE.TW_POSTNAME_2 = :QTW-POSTNAME-2,
              TRWFILE.TW_TIME_2 = :QTW-TIME-2,
              TRWFILE.TW_AMOUNT_2 = :QTW-AMOUNT-2,
              TRWFILE.TW_OVERSHORT_2 = :QTW-OVERSHORT-2,
              TRWFILE.TW_POSTNAME_3 = :QTW-POSTNAME-3,
              TRWFILE.TW_TIME_3 = :QTW-TIME-3,
              TRWFILE.TW_AMOUNT_3 = :QTW-AMOUNT-3,
              TRWFILE.TW_OVERSHORT_3 = :QTW-OVERSHORT-3,
              TRWFILE.TW_POSTNAME_4 = :QTW-POSTNAME-4,
              TRWFILE.TW_TIME_4 = :QTW-TIME-4,
              TRWFILE.TW_AMOUNT_4 = :QTW-AMOUNT-4,
              TRWFILE.TW_OVERSHORT_4 = :QTW-OVERSHORT-4,
              TRWFILE.TW_POSTNAME_5 = :QTW-POSTNAME-5,
              TRWFILE.TW_TIME_5 = :QTW-TIME-5,
              TRWFILE.TW_AMOUNT_5 = :QTW-AMOUNT-5,
              TRWFILE.TW_OVERSHORT_5 = :QTW-OVERSHORT-5,
              TRWFILE.TW_POSTNAME_6 = :QTW-POSTNAME-6,
              TRWFILE.TW_TIME_6 = :QTW-TIME-6,
              TRWFILE.TW_AMOUNT_6 = :QTW-AMOUNT-6,
              TRWFILE.TW_OVERSHORT_6 = :QTW-OVERSHORT-6,
              TRWFILE.TW_POSTNAME_7 = :QTW-POSTNAME-7,
              TRWFILE.TW_TIME_7 = :QTW-TIME-7,
              TRWFILE.TW_AMOUNT_7 = :QTW-AMOUNT-7,
              TRWFILE.TW_OVERSHORT_7 = :QTW-OVERSHORT-7,
              TRWFILE.TW_POSTNAME_8 = :QTW-POSTNAME-8,
              TRWFILE.TW_TIME_8 = :QTW-TIME-8,
              TRWFILE.TW_AMOUNT_8 = :QTW-AMOUNT-8,
              TRWFILE.TW_OVERSHORT_8 = :QTW-OVERSHORT-8,
              TRWFILE.TW_POSTNAME_9 = :QTW-POSTNAME-9,
              TRWFILE.TW_TIME_9 = :QTW-TIME-9,
              TRWFILE.TW_AMOUNT_9 = :QTW-AMOUNT-9,
              TRWFILE.TW_OVERSHORT_9 = :QTW-OVERSHORT-9,
              TRWFILE.TW_POSTNAME_10 = :QTW-POSTNAME-10,
              TRWFILE.TW_TIME_10 = :QTW-TIME-10,
              TRWFILE.TW_AMOUNT_10 = :QTW-AMOUNT-10,
              TRWFILE.TW_OVERSHORT_10 = :QTW-OVERSHORT-10,
              TRWFILE.TW_POSTNAME_11 = :QTW-POSTNAME-11,
              TRWFILE.TW_TIME_11 = :QTW-TIME-11,
              TRWFILE.TW_AMOUNT_11 = :QTW-AMOUNT-11,
              TRWFILE.TW_OVERSHORT_11 = :QTW-OVERSHORT-11,
              TRWFILE.TW_POSTNAME_12 = :QTW-POSTNAME-12,
              TRWFILE.TW_TIME_12 = :QTW-TIME-12,
              TRWFILE.TW_AMOUNT_12 = :QTW-AMOUNT-12,
              TRWFILE.TW_OVERSHORT_12 = :QTW-OVERSHORT-12,
              TRWFILE.TW_POSTNAME_13 = :QTW-POSTNAME-13,
              TRWFILE.TW_TIME_13 = :QTW-TIME-13,
              TRWFILE.TW_AMOUNT_13 = :QTW-AMOUNT-13,
              TRWFILE.TW_OVERSHORT_13 = :QTW-OVERSHORT-13,
              TRWFILE.TW_POSTNAME_14 = :QTW-POSTNAME-14,
              TRWFILE.TW_TIME_14 = :QTW-TIME-14,
              TRWFILE.TW_AMOUNT_14 = :QTW-AMOUNT-14,
              TRWFILE.TW_OVERSHORT_14 = :QTW-OVERSHORT-14,
              TRWFILE.TW_POSTNAME_15 = :QTW-POSTNAME-15,
              TRWFILE.TW_TIME_15 = :QTW-TIME-15,
              TRWFILE.TW_AMOUNT_15 = :QTW-AMOUNT-15,
              TRWFILE.TW_OVERSHORT_15 = :QTW-OVERSHORT-15,
              TRWFILE.TW_POSTNAME_16 = :QTW-POSTNAME-16,
              TRWFILE.TW_TIME_16 = :QTW-TIME-16,
              TRWFILE.TW_AMOUNT_16 = :QTW-AMOUNT-16,
              TRWFILE.TW_OVERSHORT_16 = :QTW-OVERSHORT-16,
              TRWFILE.TW_POSTNAME_17 = :QTW-POSTNAME-17,
              TRWFILE.TW_TIME_17 = :QTW-TIME-17,
              TRWFILE.TW_AMOUNT_17 = :QTW-AMOUNT-17,
              TRWFILE.TW_OVERSHORT_17 = :QTW-OVERSHORT-17,
              TRWFILE.TW_POSTNAME_18 = :QTW-POSTNAME-18,
              TRWFILE.TW_TIME_18 = :QTW-TIME-18,
              TRWFILE.TW_AMOUNT_18 = :QTW-AMOUNT-18,
              TRWFILE.TW_OVERSHORT_18 = :QTW-OVERSHORT-18,
              TRWFILE.TW_POSTNAME_19 = :QTW-POSTNAME-19,
              TRWFILE.TW_TIME_19 = :QTW-TIME-19,
              TRWFILE.TW_AMOUNT_19 = :QTW-AMOUNT-19,
              TRWFILE.TW_OVERSHORT_19 = :QTW-OVERSHORT-19,
              TRWFILE.TW_POSTNAME_20 = :QTW-POSTNAME-20,
              TRWFILE.TW_TIME_20 = :QTW-TIME-20,
              TRWFILE.TW_AMOUNT_20 = :QTW-AMOUNT-20,
              TRWFILE.TW_OVERSHORT_20 = :QTW-OVERSHORT-20
            WHERE TRWFILE.TW_BRNO = :QTW-BRNO
              AND TRWFILE.TW_DATE = :QTW-DATE
              AND TRWFILE.TW_TILLNO = :QTW-TILLNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRW1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRW1-FILE.

           MOVE TRW-PATH-OWNBR  TO TW-BRNO.
           PERFORM DELETE-IT.
           MOVE TRW-PATH-SQL TO E-FILE.
           MOVE TRW1-KEY        TO E-KEYX.
           PERFORM SET-TRW-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRWFILE
            WHERE TRWFILE.TW_BRNO = :QTW-BRNO
              AND TRWFILE.TW_DATE = :QTW-DATE
              AND TRWFILE.TW_TILLNO = :QTW-TILLNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRW1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRW1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRW1-CURSOR-STAT.

