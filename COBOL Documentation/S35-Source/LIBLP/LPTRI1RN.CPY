      * COPYMEMBER: LIBLP/LPTRI1RN
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

           IF ( QTRI1-WEND-SEQ = ZEROES    ) OR
              ( QTRI1-WEND-SEQ NOT NUMERIC )
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
       START-TRI1-FILE-SCAN.
           PERFORM START-IT.
           MOVE TRI-PATH-OWNBR  TO TI-BRNO.
           MOVE TRI-PATH-SQL    TO E-FILE.
           MOVE TRI1-KEY        TO E-KEYX.

           IF ( TRI1-CURSOR-SC-STAT-GOOD )
              PERFORM CLOSE-TRI1-FILE.

           IF ( QTRI1-WEND-SEQ = ZEROES    ) OR
              ( QTRI1-WEND-SEQ NOT NUMERIC )
              MOVE TRI-PATH-OWNBR       TO QTRI1-WBEG-BRNO
              MOVE TRI-PATH-OWNBR       TO QTRI1-WEND-BRNO
              MOVE TI-KEY-DATE          TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRI1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRI1-WEND-DATE
              MOVE TI-SEQ               TO QTRI1-WBEG-SEQ
              MOVE ALL "9"              TO QTRI1-WEND-SEQ
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }        

              MOVE TI-KEY-DATE          TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRI1-WSBEG-DATE.  

           EXEC SQL
            DECLARE TRI1_CURSOR_SC CURSOR FOR
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
            WHERE (  TRIFILE.TI_BRNO      = :QTRI1-WBEG-BRNO  )
              AND (( TRIFILE.TI_KEY_DATE >= :QTRI1-WBEG-DATE
              AND    TRIFILE.TI_SEQ      >= :QTRI1-WBEG-SEQ   )
               OR  ( TRIFILE.TI_KEY_DATE >= :QTRI1-WSBEG-DATE
              AND    TRIFILE.TI_SEQ      >= 0                 ))
              AND (  TRIFILE.TI_KEY_DATE <= :QTRI1-WEND-DATE
              AND    TRIFILE.TI_SEQ      <= :QTRI1-WEND-SEQ    )
            ORDER BY TRIFILE.TI_BRNO,
                     TRIFILE.TI_KEY_DATE,
                     TRIFILE.TI_SEQ
           END-EXEC.

           EXEC SQL
               OPEN TRI1_CURSOR_SC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRI1-WBEG-BRNO
                          QTRI1-WEND-BRNO
                          QTRI1-WBEG-SEQ   
                          QTRI1-WEND-SEQ.    

           MOVE SPACES TO QTRI1-WBEG-DATE
                          QTRI1-WEND-DATE.
           MOVE STAT---GOOD TO TRI1-CURSOR-SC-STAT.

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
              IF ( TRI1-CURSOR-SC-STAT-GOOD )
                 EXEC SQL
                      FETCH TRI1_CURSOR_SC INTO :QTRI-REC
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
       CLOSE-TRI1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRI1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRI1-CURSOR-STAT.

           IF ( TRI1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE TRI1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO TRI1-CURSOR-SC-STAT.

