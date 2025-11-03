      * COPYMEMBER: LIBSP/SPAQ2RN
      * 2024.0724 BAH START WAS TESTING QAQ2-WEND-BRNO AND SHOULD NOT,
      *               WAS NOT GETTING ANY RECORDS, CHANGED TO TEST FOR
      *               QAQ2-WEND-ACCTNO = 0 , WAS ALSO MOVE AQ-SEQNO TO
      *               QAQ2-WEND-SEQNO WHEN IT SHOULD MOVE ALL "9" S35Q-97
      *-----------------------------------------------------------------
       LOAD-AQ2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( AQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( AQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO AQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO AQ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO AQ-ALL-DATA
              MOVE AQ-ALL-PATH             TO AQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( AQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO AQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO AQ-ALL-MACHINE
              MOVE FILPATH-DATA            TO AQ-ALL-DATA
              MOVE AQ-ALL-PATH             TO AQ-PATH.

      *-----------------------------------------------------------------
       OPEN-AQ2-FILE.
           PERFORM LOAD-AQ2-FILE.
           PERFORM OPEN-IT.
           MOVE AQ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-AQ2-FILE.
           PERFORM START-IT.
           MOVE AQ-PATH-OWNBR  TO AQ-BRNO.
           MOVE AQ-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING AQ-BRNO, AQ-SEQNO, AQ-LOANDATE, AQ-ACCTNO
                  DELIMITED BY SIZE INTO E-KEYX.

           IF ( AQ2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-AQ2-FILE.

           IF ( QAQ2-WEND-ACCTNO = ZEROES    ) OR
              ( QAQ2-WEND-ACCTNO NOT NUMERIC )
              MOVE AQ-BRNO              TO QAQ2-WBEG-BRNO
              MOVE AQ-BRNO              TO QAQ2-WEND-BRNO
              MOVE AQ-SEQNO             TO QAQ2-WBEG-SEQNO
              MOVE ALL "9"              TO QAQ2-WEND-SEQNO
              MOVE AQ-LOANDATE          TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QAQ2-WBEG-LOANDATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QAQ2-WEND-LOANDATE
              MOVE AQ-ACCTNO            TO QAQ2-WBEG-ACCTNO
              MOVE ALL "9"              TO QAQ2-WEND-ACCTNO.

           EXEC SQL
            DECLARE AQ2_CURSOR CURSOR FOR
            SELECT AQFILE.AQ_BRNO,
                   AQFILE.AQ_ACCTNO,
                   CAST(AQFILE.AQ_LOANDATE AS VARCHAR(10)),
                   AQFILE.AQ_SEQNO,
                   AQFILE.AQ_WORKED_FG,
                   CAST(AQFILE.AQ_LTOUCH_DATE AS VARCHAR(10))
            FROM DBO.AQFILE
            WHERE AQFILE.AQ_BRNO    = :QAQ2-WBEG-BRNO
              AND AQFILE.AQ_SEQNO  >= :QAQ2-WBEG-SEQNO
              AND AQFILE.AQ_SEQNO  <= :QAQ2-WEND-SEQNO
              AND AQFILE.AQ_LOANDATE >= :QAQ2-WBEG-LOANDATE
              AND AQFILE.AQ_LOANDATE <= :QAQ2-WEND-LOANDATE
              AND AQFILE.AQ_ACCTNO >= :QAQ2-WBEG-ACCTNO
              AND AQFILE.AQ_ACCTNO <= :QAQ2-WEND-ACCTNO
            ORDER BY AQFILE.AQ_BRNO,
                     AQFILE.AQ_SEQNO,
                     AQFILE.AQ_LOANDATE,
                     AQFILE.AQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN AQ2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QAQ2-WBEG-BRNO 
                          QAQ2-WEND-BRNO
                          QAQ2-WBEG-SEQNO
                          QAQ2-WEND-SEQNO
                          QAQ2-WBEG-ACCTNO
                          QAQ2-WEND-ACCTNO.
 
           MOVE SPACES TO QAQ2-WBEG-LOANDATE
                          QAQ2-WEND-LOANDATE.

           MOVE STAT---GOOD TO AQ2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-AQ2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE AQ-PATH-OWNBR  TO AQ-BRNO.
           MOVE AQ-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING AQ-BRNO, AQ-SEQNO, AQ-LOANDATE, AQ-ACCTNO
              DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QAQ-REC.

           IF ( AQ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH AQ2_CURSOR INTO :QAQ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-AQ2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-AQ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-AQ2-FILE.

           PERFORM CLOSE-IT.
           IF ( AQ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE AQ2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO AQ2-CURSOR-STAT.

