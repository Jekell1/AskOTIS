      * COPYMEMBER: LIBLP/LPCI1RN
       LOAD-CI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CI-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CI-ALL-DATA
              MOVE CI-ALL-PATH             TO CI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CI-ALL-BASE
              MOVE FILPATH-MACHINE         TO CI-ALL-MACHINE
              MOVE FILPATH-DATA            TO CI-ALL-DATA
              MOVE CI-ALL-PATH             TO CI-PATH.

      *-----------------------------------------------------------------
       OPEN-CI1-FILE.
           PERFORM LOAD-CI1-FILE.
           PERFORM OPEN-IT.
           MOVE CI-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CI1-FILE.
           PERFORM START-IT.
           MOVE CI-PATH-OWNBR  TO CI-BRNO.
           MOVE CI-PATH-SQL    TO E-FILE.
           MOVE CI1-KEY        TO E-KEYX.

           IF ( CI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CI1-FILE.

           IF ( QCI1-WEND-ACCTNO = ZEROES    ) OR
              ( QCI1-WEND-ACCTNO NOT NUMERIC )
              MOVE CI-BRNO              TO QCI1-WBEG-BRNO
              MOVE CI-BRNO              TO QCI1-WEND-BRNO
              MOVE CI-ACCTNO            TO QCI1-WBEG-ACCTNO
              MOVE ALL "9"              TO QCI1-WEND-ACCTNO.

           EXEC SQL
            DECLARE CI1_CURSOR CURSOR FOR
            SELECT CIFILE.CI_BRNO,
                   CIFILE.CI_ACCTNO,
                   CIFILE.CI_SSNO,
                   CIFILE.CI_SSNO_2,
                   CAST(CIFILE.CI_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(CIFILE.CI_DATE_UPDATED
                                                  AS VARCHAR(10)),
                   CIFILE.CI_AMT_REQUESTED,
                   CIFILE.CI_MARITAL_STAT,
                   CIFILE.CI_YEARS_IN_AREA,
                   CAST(CIFILE.CI_DATE_AT_PRESENT_ADR
                                                  AS VARCHAR(10)),
                   CIFILE.CI_MONTHLY_RENT_MTG,
                   CIFILE.CI_FAIR_MARKET_VALUE,
                   CIFILE.CI_MTG_BALANCE,
                   CIFILE.CI_CHECKING_ACCOUNT,
                   CIFILE.CI_SAVINGS_ACCOUNT,
                   CIFILE.CI_TOTAL_MO_INCOME,
                   CIFILE.CI_LESS_MO_PAY,
                   CAST(CIFILE.CI_DATE_HIRE_1
                                                  AS VARCHAR(10)),
                   CIFILE.CI_POSITION_CODE_1,
                   CIFILE.CI_GROSS_MO_INCOME_1,
                   CAST(CIFILE.CI_DATE_HIRE_2
                                                  AS VARCHAR(10)),
                   CIFILE.CI_POSITION_CODE_2,
                   CIFILE.CI_GROSS_MO_INCOME_2
            FROM DBO.CIFILE
            WHERE CIFILE.CI_BRNO = :QCI1-WBEG-BRNO
              AND CIFILE.CI_ACCTNO >= :QCI1-WBEG-ACCTNO
              AND CIFILE.CI_ACCTNO <= :QCI1-WEND-ACCTNO
              ORDER BY CIFILE.CI_BRNO,
                       CIFILE.CI_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN CI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCI1-WBEG-BRNO 
                          QCI1-WEND-BRNO
                          QCI1-WBEG-ACCTNO
                          QCI1-WEND-ACCTNO.

           MOVE STAT---GOOD TO CI1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CI1-FILE.
           PERFORM READ-IT.
           MOVE CI-PATH-OWNBR  TO CI-BRNO. 
           MOVE CI-PATH-SQL    TO E-FILE.
           MOVE CI1-KEY        TO E-KEYX.
           INITIALIZE QCI-REC.

           MOVE CI-BRNO   TO QCI-BRNO.
           MOVE CI-ACCTNO TO QCI-ACCTNO.

           EXEC SQL
            SELECT CIFILE.CI_BRNO,
                   CIFILE.CI_ACCTNO,
                   CIFILE.CI_SSNO,
                   CIFILE.CI_SSNO_2,
                   CAST(CIFILE.CI_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(CIFILE.CI_DATE_UPDATED
                                                  AS VARCHAR(10)),
                   CIFILE.CI_AMT_REQUESTED,
                   CIFILE.CI_MARITAL_STAT,
                   CIFILE.CI_YEARS_IN_AREA,
                   CAST(CIFILE.CI_DATE_AT_PRESENT_ADR
                                                  AS VARCHAR(10)),
                   CIFILE.CI_MONTHLY_RENT_MTG,
                   CIFILE.CI_FAIR_MARKET_VALUE,
                   CIFILE.CI_MTG_BALANCE,
                   CIFILE.CI_CHECKING_ACCOUNT,
                   CIFILE.CI_SAVINGS_ACCOUNT,
                   CIFILE.CI_TOTAL_MO_INCOME,
                   CIFILE.CI_LESS_MO_PAY,
                   CAST(CIFILE.CI_DATE_HIRE_1
                                                  AS VARCHAR(10)),
                   CIFILE.CI_POSITION_CODE_1,
                   CIFILE.CI_GROSS_MO_INCOME_1,
                   CAST(CIFILE.CI_DATE_HIRE_2
                                                  AS VARCHAR(10)),
                   CIFILE.CI_POSITION_CODE_2,
                   CIFILE.CI_GROSS_MO_INCOME_2
            INTO :QCI-REC
            FROM DBO.CIFILE
            WHERE CIFILE.CI_BRNO = :QCI-BRNO
              AND CIFILE.CI_ACCTNO = :QCI-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CI1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CI-FIELDS.

      *-----------------------------------------------------------------
       READ-CI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CI-PATH-OWNBR  TO CI-BRNO.
           MOVE CI-PATH-SQL    TO E-FILE.
           MOVE CI1-KEY        TO E-KEYX.
           INITIALIZE QCI-REC.

           IF ( CI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CI1_CURSOR INTO :QCI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CI-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CI1-FILE.

           PERFORM CLOSE-IT.
           IF ( CI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CI1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CI1-CURSOR-STAT.

