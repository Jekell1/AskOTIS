      * COPYMEMBER: LIBLP/LPCI3IN
       LOAD-CI3-FILE.
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
       OPEN-CI3-FILE.
           PERFORM LOAD-CI3-FILE.
           PERFORM OPEN-IT.
           MOVE CI-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CI3-FILE.
           PERFORM START-IT.
           MOVE CI-PATH-OWNBR  TO CI-BRNO.
           MOVE CI-PATH-SQL    TO E-FILE.
           MOVE SPACES TO E-KEYX.
           STRING CI-BRNO, CI-SSNO-2 DELIMITED BY SIZE INTO E-KEYX.

           IF ( CI3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CI3-FILE.

           IF ( QCI3-WEND-BRNO = ZEROES    ) OR
              ( QCI3-WEND-BRNO NOT NUMERIC )
              MOVE CI-BRNO              TO QCI3-WBEG-BRNO
              MOVE CI-BRNO              TO QCI3-WEND-BRNO
              MOVE CI-SSNO-2            TO QCI3-WBEG-SSNO2
              MOVE ALL "9"              TO QCI3-WEND-SSNO2.

           EXEC SQL
            DECLARE CI3_CURSOR CURSOR FOR
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
            WHERE CIFILE.CI_BRNO = :QCI3-WBEG-BRNO
              AND CIFILE.CI_SSNO_2 >= :QCI3-WBEG-SSNO2
              AND CIFILE.CI_SSNO_2 <= :QCI3-WEND-SSNO2
              ORDER BY CIFILE.CI_BRNO,
                       CIFILE.CI_SSNO_2,
                       CIFILE.CI_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN CI3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCI3-WBEG-BRNO 
                          QCI3-WEND-BRNO
                          QCI3-WBEG-SSNO2
                          QCI3-WEND-SSNO2.

           MOVE STAT---GOOD TO CI3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CI3-FILE.
           PERFORM READ-IT.
           MOVE CI-PATH-OWNBR  TO CI-BRNO. 
           MOVE CI-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CI-BRNO, CI-SSNO-2 DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QCI-REC.

           MOVE CI-BRNO   TO QCI-BRNO.
           MOVE CI-SSNO-2 TO QCI-SSNO-2.

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
              AND CIFILE.CI_SSNO_2 = :QCI-SSNO-2
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CI3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CI-FIELDS.

      *-----------------------------------------------------------------
       READ-CI3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CI-PATH-OWNBR  TO CI-BRNO.
           MOVE CI-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CI-BRNO, CI-SSNO-2 DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QCI-REC.

           IF ( CI3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CI3_CURSOR INTO :QCI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CI3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CI-FIELDS.

      *-----------------------------------------------------------------
       REWRITE-CI3-FILE.

           MOVE CI-PATH-OWNBR  TO CI-BRNO.
           PERFORM REWRITE-IT.
           MOVE CI-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CI-BRNO, CI-SSNO-2 DELIMITED BY SIZE INTO E-KEYX.
           PERFORM SET-CI-FIELDS.

           EXEC SQL
            UPDATE DBO.CIFILE
             SET
              CIFILE.CI_BRNO = :QCI-BRNO,
              CIFILE.CI_ACCTNO = :QCI-ACCTNO,
              CIFILE.CI_SSNO = :QCI-SSNO,
              CIFILE.CI_SSNO_2 = :QCI-SSNO-2,
              CIFILE.CI_LTOUCH_DATE =
                 :QCI-LTOUCH-DATE,
              CIFILE.CI_DATE_UPDATED =
                 :QCI-DATE-UPDATED,
              CIFILE.CI_AMT_REQUESTED = :QCI-AMT-REQUESTED,
              CIFILE.CI_MARITAL_STAT = :QCI-MARITAL-STAT,
              CIFILE.CI_YEARS_IN_AREA = :QCI-YEARS-IN-AREA,
              CIFILE.CI_DATE_AT_PRESENT_ADR =
                 :QCI-DATE-AT-PRESENT-ADR,
              CIFILE.CI_MONTHLY_RENT_MTG = :QCI-MONTHLY-RENT-MTG,
              CIFILE.CI_FAIR_MARKET_VALUE =
                   :QCI-FAIR-MARKET-VALUE,
              CIFILE.CI_MTG_BALANCE = :QCI-MTG-BALANCE,
              CIFILE.CI_CHECKING_ACCOUNT = :QCI-CHECKING-ACCOUNT,
              CIFILE.CI_SAVINGS_ACCOUNT = :QCI-SAVINGS-ACCOUNT,
              CIFILE.CI_TOTAL_MO_INCOME = :QCI-TOTAL-MO-INCOME,
              CIFILE.CI_LESS_MO_PAY = :QCI-LESS-MO-PAY,
              CIFILE.CI_DATE_HIRE_1 =
                 :QCI-DATE-HIRE-1,
              CIFILE.CI_POSITION_CODE_1 = :QCI-POSITION-CODE-1,
              CIFILE.CI_GROSS_MO_INCOME_1 =
                   :QCI-GROSS-MO-INCOME-1,
              CIFILE.CI_DATE_HIRE_2 = :QCI-DATE-HIRE-2,
              CIFILE.CI_POSITION_CODE_2 = :QCI-POSITION-CODE-2,
              CIFILE.CI_GROSS_MO_INCOME_2 =
                   :QCI-GROSS-MO-INCOME-2
             WHERE CIFILE.CI_BRNO = :QCI-BRNO
               AND CIFILE.CI_ACCTNO = :QCI-ACCTNO
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
              GO TO REWRITE-CI3-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       CLOSE-CI3-FILE.

           PERFORM CLOSE-IT.

           IF ( CI3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CI3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CI3-CURSOR-STAT.

