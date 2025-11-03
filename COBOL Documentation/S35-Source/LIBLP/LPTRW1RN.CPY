      * COPYMEMBER: LIBLP/LPTRW1RN
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
       START-TRW1-FILE-SCAN.
           PERFORM START-IT.
           MOVE TRW-PATH-OWNBR  TO TW-BRNO.
           MOVE TRW-PATH-SQL    TO E-FILE.
           MOVE TRW1-KEY        TO E-KEYX.

           IF ( TRW1-CURSOR-SC-STAT-GOOD )
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
              MOVE ALL "9"              TO QTRW1-WEND-TILLNO
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }        

              MOVE TW-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRW1-WSBEG-DATE. 

           EXEC SQL
            DECLARE TRW1_CURSOR_SC CURSOR FOR
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
            WHERE (  TRWFILE.TW_BRNO    = :QTRW1-WBEG-BRNO    )
              AND (( TRWFILE.TW_DATE   >= :QTRW1-WBEG-DATE
              AND    TRWFILE.TW_TILLNO >= :QTRW1-WBEG-TILLNO )
               OR  ( TRWFILE.TW_DATE   >= :QTRW1-WSBEG-DATE
              AND    TRWFILE.TW_TILLNO >= 0                  ))
              AND (  TRWFILE.TW_DATE   <= :QTRW1-WEND-DATE
              AND    TRWFILE.TW_TILLNO >= :QTRW1-WBEG-TILLNO  )
            ORDER BY TRWFILE.TW_BRNO,
                     TRWFILE.TW_DATE,
                     TRWFILE.TW_TILLNO
           END-EXEC.

           EXEC SQL
               OPEN TRW1_CURSOR_SC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRW1-WBEG-BRNO
                          QTRW1-WEND-BRNO
                          QTRW1-WBEG-TILLNO
                          QTRW1-WEND-TILLNO.

           MOVE SPACES TO QTRW1-WBEG-DATE
                          QTRW1-WEND-DATE.
           MOVE STAT---GOOD TO TRW1-CURSOR-SC-STAT.

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
              IF ( TRW1-CURSOR-SC-STAT-GOOD )
                 EXEC SQL
                      FETCH TRW1_CURSOR_SC INTO :QTRW-REC
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
       CLOSE-TRW1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRW1-CURSOR-STAT.
              
           IF ( TRW1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE TRW1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO TRW1-CURSOR-SC-STAT.


