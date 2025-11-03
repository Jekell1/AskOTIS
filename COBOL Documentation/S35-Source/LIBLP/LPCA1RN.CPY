      * COPYMEMBER: LIBLP/LPCA1RN
       LOAD-CA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CA-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CA-ALL-DATA
              MOVE CA-ALL-PATH             TO CA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CA-ALL-BASE
              MOVE FILPATH-MACHINE         TO CA-ALL-MACHINE
              MOVE FILPATH-DATA            TO CA-ALL-DATA
              MOVE CA-ALL-PATH             TO CA-PATH.

      *-----------------------------------------------------------------
       OPEN-CA1-FILE.
           PERFORM LOAD-CA1-FILE.
           PERFORM OPEN-IT.
           MOVE CA-PATH-SQL    TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CA1-FILE.
           PERFORM START-IT.
           MOVE CA-PATH-OWNBR  TO CA-BRNO.
           MOVE CA-PATH-SQL    TO E-FILE.
           MOVE CA1-KEY        TO E-KEYX.

           IF ( CA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CA1-FILE.

           IF ( QCA1-WEND-BRNO = ZEROES    ) OR
              ( QCA1-WEND-BRNO NOT NUMERIC )
              MOVE CA-PATH-OWNBR        TO QCA1-WBEG-BRNO
              MOVE CA-PATH-OWNBR        TO QCA1-WEND-BRNO

              MOVE CA-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCA1-WBEG-DATE

              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCA1-WEND-DATE 

              MOVE CA-TILLNO            TO QCA1-WBEG-TILLNO
              MOVE ALL "9"              TO QCA1-WEND-TILLNO.


           EXEC SQL
            DECLARE CA1_CURSOR CURSOR FOR
            SELECT CAFILE.CA_BRNO,
                   CAST(CAFILE.CA_DATE AS VARCHAR(10)),
                   CAFILE.CA_TILLNO,
                   CAST(CAFILE.CA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAFILE.CA_DESC,
                   CAFILE.CA_BEGBAL,
                   CAFILE.CA_OPENED_HHMM,
                   CAFILE.CA_CLOSED_HHMM,
                   CAFILE.CA_ENDBAL,
                   CAFILE.CA_BALBY,
                   CAFILE.CA_RUNNING,
                   CAFILE.CA_DEPOSIT,
                   CAFILE.CA_OPEN_USERID,
                   CAFILE.CA_NET_CASH_NOTES,
                   CAFILE.CA_PASSWORD,
                   CAFILE.CA_CHECK_DEPOSIT
            FROM DBO.CAFILE
            WHERE CAFILE.CA_BRNO = :QCA1-WBEG-BRNO
              AND CAFILE.CA_DATE >= :QCA1-WBEG-DATE 
              AND CAFILE.CA_DATE <= :QCA1-WEND-DATE
              AND CAFILE.CA_TILLNO >= :QCA1-WBEG-TILLNO
              AND CAFILE.CA_TILLNO <= :QCA1-WEND-TILLNO
              ORDER BY CAFILE.CA_BRNO,
                       CAFILE.CA_DATE,
                       CAFILE.CA_TILLNO
           END-EXEC.

           EXEC SQL
               OPEN CA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCA1-WBEG-BRNO 
                          QCA1-WEND-BRNO
                          QCA1-WBEG-TILLNO
                          QCA1-WEND-TILLNO.
 
           MOVE SPACES TO QCA1-WBEG-DATE
                          QCA1-WEND-DATE.

           MOVE STAT---GOOD TO CA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CA1-FILE.
           PERFORM READ-IT.
           MOVE CA-PATH-OWNBR  TO CA-BRNO. 
           MOVE CA-PATH-SQL    TO E-FILE.
           MOVE CA1-KEY        TO E-KEYX.
           INITIALIZE QCA-REC.

           MOVE CA-BRNO   TO QCA-BRNO.

           MOVE CA-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCA-DATE.

           MOVE CA-TILLNO TO QCA-TILLNO.

           EXEC SQL
            SELECT CAFILE.CA_BRNO,
                   CAST(CAFILE.CA_DATE AS VARCHAR(10)),
                   CAFILE.CA_TILLNO,
                   CAST(CAFILE.CA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAFILE.CA_DESC,
                   CAFILE.CA_BEGBAL,
                   CAFILE.CA_OPENED_HHMM,
                   CAFILE.CA_CLOSED_HHMM,
                   CAFILE.CA_ENDBAL,
                   CAFILE.CA_BALBY,
                   CAFILE.CA_RUNNING,
                   CAFILE.CA_DEPOSIT,
                   CAFILE.CA_OPEN_USERID,
                   CAFILE.CA_NET_CASH_NOTES,
                   CAFILE.CA_PASSWORD,
                   CAFILE.CA_CHECK_DEPOSIT
            INTO :QCA-REC
            FROM DBO.CAFILE
            WHERE CAFILE.CA_BRNO = :QCA-BRNO            
              AND CAFILE.CA_DATE = :QCA-DATE
              AND CAFILE.CA_TILLNO = :QCA-TILLNO 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CA-FIELDS.

      *-----------------------------------------------------------------
       READ-CA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CA-PATH-OWNBR  TO CA-BRNO.
           MOVE CA-PATH-SQL    TO E-FILE.
           MOVE CA1-KEY        TO E-KEYX.
           INITIALIZE QCA-REC.

           IF ( CA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CA1_CURSOR INTO :QCA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CA-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CA1-FILE.

           PERFORM CLOSE-IT.
           IF ( CA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CA1-CURSOR-STAT.

