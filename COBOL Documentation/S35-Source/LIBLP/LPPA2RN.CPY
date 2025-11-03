      * COPYMEMBER: LIBLP/LPPA2RN
       LOAD-PA2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( PA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( PA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO PA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO PA-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO PA-ALL-DATA
              MOVE PA-ALL-PATH             TO PA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( PA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO PA-ALL-BASE
              MOVE FILPATH-MACHINE         TO PA-ALL-MACHINE
              MOVE FILPATH-DATA            TO PA-ALL-DATA
              MOVE PA-ALL-PATH             TO PA-PATH.

      *-----------------------------------------------------------------
       OPEN-PA2-FILE.
           PERFORM LOAD-PA2-FILE.
           PERFORM OPEN-IT.
           MOVE PA-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-PA2-FILE.
           PERFORM START-IT.
           MOVE PA-PATH-OWNBR  TO PA-BRNO.
           MOVE PA-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING PA-BRNO, PA-SORTKEY DELIMITED BY SIZE INTO E-KEYX.

           IF ( PA2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-PA2-FILE.

           IF ( QPA2-WEND-BRNO = ZEROES    ) OR
              ( QPA2-WEND-BRNO NOT NUMERIC )
              MOVE PA-PATH-OWNBR        TO QPA2-WBEG-BRNO
              MOVE PA-PATH-OWNBR        TO QPA2-WEND-BRNO
              MOVE PA-SORTKEY           TO QPA2-WBEG-SORTKEY
              MOVE ALL "Z"              TO QPA2-WEND-SORTKEY.


           EXEC SQL
            DECLARE PA2_CURSOR CURSOR FOR
            SELECT PAFILE.PA_BRNO,
                   PAFILE.PA_NUMBER,
                   PAFILE.PA_NAME,
                   PAFILE.PA_ADR1,
                   PAFILE.PA_ADR2,
                   PAFILE.PA_CITY,
                   PAFILE.PA_STATE,
                   PAFILE.PA_ZIP,
                   PAFILE.PA_LCHECK,
                   CAST(PAFILE.PA_LCDATE AS VARCHAR(10)),
                   PAFILE.PA_LAMT,
                   PAFILE.PA_YTD_PAID,
                   PAFILE.PA_LYTD_PAID,
                   PAFILE.PA_SORTKEY,
                   PAFILE.PA_CUSNO,
                   PAFILE.PA_CONTACT,
                   PAFILE.PA_PHONE,
                   PAFILE.PA_FAX,
                   PAFILE.PA_1099_FLAG,
                   PAFILE.PA_1099_ID,
                   PAFILE.PA_LEGAL_NAME1,
                   PAFILE.PA_LEGAL_NAME2,
                   PAFILE.PA_VAT_PERC,
                   PAFILE.PA_RFC,
                   PAFILE.PA_CURP,
                   CAST(PAFILE.PA_LTOUCH_DATE
                                                  AS VARCHAR(10))
            FROM DBO.PAFILE
            WHERE PAFILE.PA_BRNO  = :QPA2-WBEG-BRNO
              AND PAFILE.PA_SORTKEY >= :QPA2-WBEG-SORTKEY
              AND PAFILE.PA_SORTKEY <= :QPA2-WEND-SORTKEY
            ORDER BY PAFILE.PA_BRNO,
                     PAFILE.PA_SORTKEY,
                     PAFILE.PA_NUMBER
           END-EXEC.

           EXEC SQL
               OPEN PA2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QPA2-WBEG-BRNO 
                          QPA2-WEND-BRNO.
           MOVE SPACES TO QPA2-WBEG-SORTKEY
                          QPA2-WEND-SORTKEY.

           MOVE STAT---GOOD TO PA2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-PA2-FILE.
           PERFORM READ-IT.
           MOVE PA-PATH-OWNBR  TO PA-BRNO. 
           MOVE PA-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING PA-BRNO, PA-SORTKEY DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QPA-REC.

           MOVE PA-BRNO    TO QPA-BRNO.
           MOVE PA-SORTKEY TO QPA-SORTKEY.

           EXEC SQL
            SELECT PAFILE.PA_BRNO,
                   PAFILE.PA_NUMBER,
                   PAFILE.PA_NAME,
                   PAFILE.PA_ADR1,
                   PAFILE.PA_ADR2,
                   PAFILE.PA_CITY,
                   PAFILE.PA_STATE,
                   PAFILE.PA_ZIP,
                   PAFILE.PA_LCHECK,
                   CAST(PAFILE.PA_LCDATE AS VARCHAR(10)),
                   PAFILE.PA_LAMT,
                   PAFILE.PA_YTD_PAID,
                   PAFILE.PA_LYTD_PAID,
                   PAFILE.PA_SORTKEY,
                   PAFILE.PA_CUSNO,
                   PAFILE.PA_CONTACT,
                   PAFILE.PA_PHONE,
                   PAFILE.PA_FAX,
                   PAFILE.PA_1099_FLAG,
                   PAFILE.PA_1099_ID,
                   PAFILE.PA_LEGAL_NAME1,
                   PAFILE.PA_LEGAL_NAME2,
                   PAFILE.PA_VAT_PERC,
                   PAFILE.PA_RFC,
                   PAFILE.PA_CURP,
                   CAST(PAFILE.PA_LTOUCH_DATE
                                                  AS VARCHAR(10))
            INTO :QPA-REC
            FROM DBO.PAFILE
            WHERE PAFILE.PA_BRNO = :QPA-BRNO
              AND PAFILE.PA_SORTKEY = :QPA-SORTKEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PA2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-PA-FIELDS.

      *-----------------------------------------------------------------
       READ-PA2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE PA-PATH-OWNBR  TO PA-BRNO.
           MOVE PA-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING PA-BRNO, PA-SORTKEY DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QPA-REC.

           IF ( PA2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH PA2_CURSOR INTO :QPA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PA2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-PA-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-PA2-FILE.

           PERFORM CLOSE-IT.
           IF ( PA2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE PA2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO PA2-CURSOR-STAT.

