      * COPYMEMBER: LIBLP/LPPA1RN
       LOAD-PA1-FILE.
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
       OPEN-PA1-FILE.
           PERFORM LOAD-PA1-FILE.
           PERFORM OPEN-IT.
           MOVE PA-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-PA1-FILE.
           PERFORM START-IT.
           MOVE PA-PATH-OWNBR  TO PA-BRNO.
           MOVE PA-PATH-SQL    TO E-FILE.
           MOVE PA1-KEY        TO E-KEYX.

           IF ( PA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-PA1-FILE.

           IF ( QPA1-WEND-NUMBER = ZEROES    ) OR
              ( QPA1-WEND-NUMBER NOT NUMERIC )
              MOVE PA-PATH-OWNBR        TO QPA1-WBEG-BRNO
              MOVE PA-PATH-OWNBR        TO QPA1-WEND-BRNO
              MOVE PA-NUMBER            TO QPA1-WBEG-NUMBER
              MOVE ALL "9"              TO QPA1-WEND-NUMBER.


           EXEC SQL
            DECLARE PA1_CURSOR CURSOR FOR
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
            WHERE PAFILE.PA_BRNO  = :QPA1-WBEG-BRNO
              AND PAFILE.PA_NUMBER >= :QPA1-WBEG-NUMBER
              AND PAFILE.PA_NUMBER <= :QPA1-WEND-NUMBER
            ORDER BY PAFILE.PA_BRNO,
                     PAFILE.PA_NUMBER
           END-EXEC.

           EXEC SQL
               OPEN PA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QPA1-WBEG-BRNO 
                          QPA1-WEND-BRNO
                          QPA1-WBEG-NUMBER
                          QPA1-WEND-NUMBER.
 

           MOVE STAT---GOOD TO PA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-PA1-FILE.
           PERFORM READ-IT.
           MOVE PA-PATH-OWNBR  TO PA-BRNO. 
           MOVE PA-PATH-SQL    TO E-FILE.
           MOVE PA1-KEY        TO E-KEYX.
           INITIALIZE QPA-REC. 

           MOVE PA-BRNO   TO QPA-BRNO.
           MOVE PA-NUMBER TO QPA-NUMBER.

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
              AND PAFILE.PA_NUMBER = :QPA-NUMBER
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-PA-FIELDS.

      *-----------------------------------------------------------------
       READ-PA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE PA-PATH-OWNBR  TO PA-BRNO.
           MOVE PA-PATH-SQL    TO E-FILE.
           MOVE PA1-KEY        TO E-KEYX.
           INITIALIZE QPA-REC.

           IF ( PA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH PA1_CURSOR INTO :QPA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-PA-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-PA1-FILE.

           PERFORM CLOSE-IT.
           IF ( PA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE PA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO PA1-CURSOR-STAT.

