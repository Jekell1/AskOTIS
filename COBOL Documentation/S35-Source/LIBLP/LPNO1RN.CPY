      * COPYMEMBER: LIBLP/LPNO1RN
      * BAH 2024.0201 REMOVED NO-FMNO, NO-PRINTED, NO-WAGE-ASSIGN-DELINQ
      *               ADDED FIELDS AT THE END OF RECORD
       LOAD-NO1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( NO-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( NO-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO NO-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO NO-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO NO-ALL-DATA
              MOVE NO-ALL-PATH             TO NO-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( NO-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO NO-ALL-BASE
              MOVE FILPATH-MACHINE         TO NO-ALL-MACHINE
              MOVE FILPATH-DATA            TO NO-ALL-DATA
              MOVE NO-ALL-PATH             TO NO-PATH.

      *-----------------------------------------------------------------
       OPEN-NO1-FILE.
           PERFORM LOAD-NO1-FILE.
           PERFORM OPEN-IT.
           MOVE NO-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-NO1-FILE.
           PERFORM START-IT.
           MOVE NO-PATH-OWNBR  TO NO-BRNO.
           MOVE NO-PATH-SQL    TO E-FILE.
           MOVE NO1-KEY        TO E-KEYX.

           IF ( NO1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-NO1-FILE.

           IF ( QNO1-WEND-ACCTNO = ZEROES    ) OR
              ( QNO1-WEND-ACCTNO NOT NUMERIC )
              MOVE NO-PATH-OWNBR        TO QNO1-WBEG-BRNO
              MOVE NO-PATH-OWNBR        TO QNO1-WEND-BRNO
              MOVE NO-ACCTNO            TO QNO1-WBEG-ACCTNO
              MOVE ALL "9"              TO QNO1-WEND-ACCTNO.

           EXEC SQL
            DECLARE NO1_CURSOR CURSOR FOR
            SELECT NOFILE.NO_BRNO,
                   NOFILE.NO_ACCTNO,
                   CAST(NOFILE.NO_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(NOFILE.NO_TDUE_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_TDUELC,
                   NOFILE.NO_TDUEAMT,
                   NOFILE.NO_TDUEPRIN,
                   NOFILE.NO_TDUETBAL,
                   NOFILE.NO_TDUEPOFF,
                   CAST(NOFILE.NO_LDUE_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_LDUELC,
                   NOFILE.NO_LDUEAMT,
                   CAST(NOFILE.NO_DATE_PAID_LAST
                                                  AS VARCHAR(10)),
                   NOFILE.NO_AMT_PAID_LAST,
                   NOFILE.NO_LSTLPKEY,
                   CAST(NOFILE.NO_NXDUE_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_NXDUEAMT,
                   NOFILE.NO_NXDUELC,
                   NOFILE.NO_TRUCONTR,
                   NOFILE.NO_RECENCY,
                   CAST(NOFILE.NO_PDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(NOFILE.NO_LCPDTH_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_LCHG_INTDUE,
                   NOFILE.NO_AVAILCREDIT,
                   NOFILE.NO_TDUEDELINQ,
                   NOFILE.NO_DEFERMENT,
                   CAST(NOFILE.NO_TRUE_DUE_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_SCHED_PAYMENT,
                   NOFILE.NO_TDUECONTR,
                   NOFILE.NO_LDUECONTR,
                   NOFILE.NO_NXDUECONTR,
                   NOFILE.NO_CURRENT_AVAIL,
                   NOFILE.NO_LATE_CHG,
                   NOFILE.NO_PAST_DUE_DAYS,
                   NOFILE.NO_RECDEL,
                   NOFILE.NO_AVCR_RESTRICT_FG,
                   NOFILE.NO_INDU_PENALTY,
                   NOFILE.NO_CPOT30,
                   NOFILE.NO_RDPOT30,
                   NOFILE.NO_CPOTCHGOFF,
                   NOFILE.NO_RDPOTCHGOFF,
                   NOFILE.NO_CSUB,
                   NOFILE.NO_RSUB
            FROM DBO.NOFILE
            WHERE NOFILE.NO_BRNO  = :QNO1-WBEG-BRNO
              AND NOFILE.NO_ACCTNO >= :QNO1-WBEG-ACCTNO
              AND NOFILE.NO_ACCTNO <= :QNO1-WEND-ACCTNO
            ORDER BY NOFILE.NO_BRNO,
                     NOFILE.NO_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN NO1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QNO1-WBEG-BRNO 
                          QNO1-WEND-BRNO
                          QNO1-WBEG-ACCTNO
                          QNO1-WEND-ACCTNO.

           MOVE STAT---GOOD TO NO1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-NO1-FILE.
           PERFORM READ-IT.
           MOVE NO-PATH-OWNBR  TO NO-BRNO. 
           MOVE NO-PATH-SQL    TO E-FILE.
           MOVE NO1-KEY        TO E-KEYX.
           INITIALIZE QNO-REC.

           MOVE NO-BRNO   TO QNO-BRNO.
           MOVE NO-ACCTNO TO QNO-ACCTNO.

           EXEC SQL
            SELECT NOFILE.NO_BRNO,
                   NOFILE.NO_ACCTNO,
                   CAST(NOFILE.NO_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(NOFILE.NO_TDUE_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_TDUELC,
                   NOFILE.NO_TDUEAMT,
                   NOFILE.NO_TDUEPRIN,
                   NOFILE.NO_TDUETBAL,
                   NOFILE.NO_TDUEPOFF,
                   CAST(NOFILE.NO_LDUE_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_LDUELC,
                   NOFILE.NO_LDUEAMT,
                   CAST(NOFILE.NO_DATE_PAID_LAST
                                                  AS VARCHAR(10)),
                   NOFILE.NO_AMT_PAID_LAST,
                   NOFILE.NO_LSTLPKEY,
                   CAST(NOFILE.NO_NXDUE_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_NXDUEAMT,
                   NOFILE.NO_NXDUELC,
                   NOFILE.NO_TRUCONTR,
                   NOFILE.NO_RECENCY,
                   CAST(NOFILE.NO_PDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(NOFILE.NO_LCPDTH_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_LCHG_INTDUE,
                   NOFILE.NO_AVAILCREDIT,
                   NOFILE.NO_TDUEDELINQ,
                   NOFILE.NO_DEFERMENT,
                   CAST(NOFILE.NO_TRUE_DUE_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_SCHED_PAYMENT,
                   NOFILE.NO_TDUECONTR,
                   NOFILE.NO_LDUECONTR,
                   NOFILE.NO_NXDUECONTR,
                   NOFILE.NO_CURRENT_AVAIL,
                   NOFILE.NO_LATE_CHG,
                   NOFILE.NO_PAST_DUE_DAYS,
                   NOFILE.NO_RECDEL,
                   NOFILE.NO_AVCR_RESTRICT_FG,
                   NOFILE.NO_INDU_PENALTY,
                   NOFILE.NO_CPOT30,
                   NOFILE.NO_RDPOT30,
                   NOFILE.NO_CPOTCHGOFF,
                   NOFILE.NO_RDPOTCHGOFF,
                   NOFILE.NO_CSUB,
                   NOFILE.NO_RSUB
            INTO :QNO-REC
            FROM DBO.NOFILE
            WHERE NOFILE.NO_BRNO = :QNO-BRNO
              AND NOFILE.NO_ACCTNO = :QNO-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-NO1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-NO-FIELDS.

      *-----------------------------------------------------------------
       READ-NO1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE NO-PATH-OWNBR  TO NO-BRNO.
           MOVE NO-PATH-SQL    TO E-FILE.
           MOVE NO1-KEY        TO E-KEYX.
           INITIALIZE QNO-REC.

           IF ( NO1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH NO1_CURSOR INTO :QNO-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-NO1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-NO-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-NO1-FILE.

           PERFORM CLOSE-IT.
           IF ( NO1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE NO1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO NO1-CURSOR-STAT.

