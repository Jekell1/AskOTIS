      * COPYMEMBER: LIBLP/LPLE1IN
       LOAD-LE1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LE-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LE-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LE-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LE-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LE-ALL-DATA
              MOVE LE-ALL-PATH             TO LE-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LE-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LE-ALL-BASE
              MOVE FILPATH-MACHINE         TO LE-ALL-MACHINE
              MOVE FILPATH-DATA            TO LE-ALL-DATA
              MOVE LE-ALL-PATH             TO LE-PATH.

      *-----------------------------------------------------------------
       OPEN-LE1-FILE.
           PERFORM LOAD-LE1-FILE.
           PERFORM OPEN-IT.
           MOVE LE-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LE1-FILE.
           PERFORM START-IT.
           MOVE LE-PATH-OWNBR  TO LE-BRNO.
           MOVE LE-PATH-SQL    TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.

           IF ( LE1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LE1-FILE.

           IF ( QLE1-WEND-OSACCTNO = ZEROES    ) OR
              ( QLE1-WEND-OSACCTNO NOT NUMERIC )
              MOVE LE-PATH-OWNBR        TO QLE1-WBEG-BRNO
              MOVE LE-PATH-OWNBR        TO QLE1-WEND-BRNO
              MOVE LE-ACCTNO            TO QLE1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLE1-WEND-ACCTNO
              MOVE LE-TYPE              TO QLE1-WBEG-TYPE
              MOVE "Z"                  TO QLE1-WEND-TYPE
              MOVE LE-FLAG              TO QLE1-WBEG-FLAG
              MOVE "Z"                  TO QLE1-WEND-FLAG
              MOVE LE-SSNO              TO QLE1-WBEG-SSNO
              MOVE ALL "9"              TO QLE1-WEND-SSNO
              MOVE LE-OSACCTNO          TO QLE1-WBEG-OSACCTNO
              MOVE ALL "9"              TO QLE1-WEND-OSACCTNO.

           EXEC SQL
            DECLARE LE1_CURSOR CURSOR FOR
            SELECT LEFILE.LE_BRNO,
                   LEFILE.LE_ACCTNO,
                   LEFILE.LE_TYPE,
                   LEFILE.LE_FLAG,
                   LEFILE.LE_SSNO,
                   LEFILE.LE_OSACCTNO,
                   CAST(LEFILE.LE_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LEFILE.LE_E_OSRATE,
                   LEFILE.LE_E_OSAMT,
                   CAST(LEFILE.LE_E_OSDATE AS VARCHAR(10)),
                   LEFILE.LE_E_AMT,
                   CAST(LEFILE.LE_E_DATE AS VARCHAR(10)),
                   LEFILE.LE_E_OSMAKERCD,
                   CAST(LEFILE.LE_R_LOANDATE
                                                  AS VARCHAR(10)),
                   LEFILE.LE_R_LNAMT,
                   CAST(LEFILE.LE_R_POFFDATE
                                                  AS VARCHAR(10)),
                   LEFILE.LE_R_RESTRICTED_MONEY,
                   LEFILE.LE_R_BALANCE,
                   LEFILE.LE_R_SALEFIN
            FROM DBO.LEFILE
            WHERE LEFILE.LE_BRNO = :QLE1-WBEG-BRNO
              AND LEFILE.LE_ACCTNO >= :QLE1-WBEG-ACCTNO
              AND LEFILE.LE_ACCTNO <= :QLE1-WEND-ACCTNO
              AND LEFILE.LE_TYPE >= :QLE1-WBEG-TYPE
              AND LEFILE.LE_TYPE <= :QLE1-WEND-TYPE
              AND LEFILE.LE_FLAG >= :QLE1-WBEG-FLAG
              AND LEFILE.LE_FLAG <= :QLE1-WEND-FLAG
              AND LEFILE.LE_SSNO >= :QLE1-WBEG-SSNO
              AND LEFILE.LE_SSNO <= :QLE1-WEND-SSNO
              AND LEFILE.LE_OSACCTNO >= :QLE1-WBEG-OSACCTNO
              AND LEFILE.LE_OSACCTNO <= :QLE1-WEND-OSACCTNO
            ORDER BY LEFILE.LE_BRNO,
                     LEFILE.LE_ACCTNO,
                     LEFILE.LE_TYPE,
                     LEFILE.LE_FLAG,
                     LEFILE.LE_SSNO,
                     LEFILE.LE_OSACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LE1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLE1-WBEG-BRNO 
                          QLE1-WEND-BRNO
                          QLE1-WBEG-ACCTNO
                          QLE1-WEND-ACCTNO
                          QLE1-WBEG-SSNO
                          QLE1-WEND-SSNO
                          QLE1-WBEG-OSACCTNO
                          QLE1-WEND-OSACCTNO.
           MOVE SPACES TO QLE1-WBEG-TYPE
                          QLE1-WEND-TYPE
                          QLE1-WBEG-FLAG
                          QLE1-WEND-FLAG

           MOVE STAT---GOOD TO LE1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LE1-FILE.
           PERFORM READ-IT.
           MOVE LE-PATH-OWNBR  TO LE-BRNO. 
           MOVE LE-PATH-SQL    TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.
           INITIALIZE QLE-REC.

           MOVE LE-BRNO     TO QLE-BRNO.
           MOVE LE-ACCTNO   TO QLE-ACCTNO.
           MOVE LE-TYPE     TO QLE-TYPE.
           MOVE LE-FLAG     TO QLE-FLAG.
           MOVE LE-SSNO     TO QLE-SSNO.
           MOVE LE-OSACCTNO TO QLE-OSACCTNO.

           EXEC SQL
            SELECT LEFILE.LE_BRNO,
                   LEFILE.LE_ACCTNO,
                   LEFILE.LE_TYPE,
                   LEFILE.LE_FLAG,
                   LEFILE.LE_SSNO,
                   LEFILE.LE_OSACCTNO,
                   CAST(LEFILE.LE_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LEFILE.LE_E_OSRATE,
                   LEFILE.LE_E_OSAMT,
                   CAST(LEFILE.LE_E_OSDATE AS VARCHAR(10)),
                   LEFILE.LE_E_AMT,
                   CAST(LEFILE.LE_E_DATE AS VARCHAR(10)),
                   LEFILE.LE_E_OSMAKERCD,
                   CAST(LEFILE.LE_R_LOANDATE
                                                  AS VARCHAR(10)),
                   LEFILE.LE_R_LNAMT,
                   CAST(LEFILE.LE_R_POFFDATE
                                                  AS VARCHAR(10)),
                   LEFILE.LE_R_RESTRICTED_MONEY,
                   LEFILE.LE_R_BALANCE,
                   LEFILE.LE_R_SALEFIN
            INTO :QLE-REC
            FROM DBO.LEFILE
            WHERE LEFILE.LE_BRNO = :QLE-BRNO
              AND LEFILE.LE_ACCTNO = :QLE-ACCTNO
              AND LEFILE.LE_TYPE = :QLE-TYPE
              AND LEFILE.LE_FLAG = :QLE-FLAG
              AND LEFILE.LE_SSNO = :QLE-SSNO
              AND LEFILE.LE_OSACCTNO = :QLE-OSACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LE1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LE-FIELDS.

      *-----------------------------------------------------------------
       READ-LE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LE-PATH-OWNBR  TO LE-BRNO.
           MOVE LE-PATH-SQL    TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.
           INITIALIZE QLE-REC.

           IF ( LE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LE1_CURSOR INTO :QLE-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LE1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LE-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LE1-FILE.

           MOVE LE-PATH-OWNBR  TO LE-BRNO.
           ACCEPT LE-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LE-PATH-SQL    TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.
           PERFORM SET-LE-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LEFILE
             (LEFILE.LE_BRNO,
              LEFILE.LE_ACCTNO,
              LEFILE.LE_TYPE,
              LEFILE.LE_FLAG,
              LEFILE.LE_SSNO,
              LEFILE.LE_OSACCTNO,
              LEFILE.LE_LTOUCH_DATE,
              LEFILE.LE_E_OSRATE,
              LEFILE.LE_E_OSAMT,
              LEFILE.LE_E_OSDATE,
              LEFILE.LE_E_AMT,
              LEFILE.LE_E_DATE,
              LEFILE.LE_E_OSMAKERCD,
              LEFILE.LE_R_LOANDATE,
              LEFILE.LE_R_LNAMT,
              LEFILE.LE_R_POFFDATE,
              LEFILE.LE_R_RESTRICTED_MONEY,
              LEFILE.LE_R_BALANCE,
              LEFILE.LE_R_SALEFIN)
            VALUES
             (:QLE-BRNO,
              :QLE-ACCTNO,
              :QLE-TYPE,
              :QLE-FLAG,
              :QLE-SSNO,
              :QLE-OSACCTNO,
              :QLE-LTOUCH-DATE,
              :QLE-E-OSRATE,
              :QLE-E-OSAMT,
              :QLE-E-OSDATE,
              :QLE-E-AMT,
              :QLE-E-DATE,
              :QLE-E-OSMAKERCD,
              :QLE-R-LOANDATE,
              :QLE-R-LNAMT,
              :QLE-R-POFFDATE,
              :QLE-R-RESTRICTED-MONEY,
              :QLE-R-BALANCE,
              :QLE-R-SALEFIN)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LE1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LE1-FILE.

           MOVE LE-PATH-OWNBR  TO LE-BRNO.
           ACCEPT LE-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LE-PATH-SQL    TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.
           PERFORM SET-LE-FIELDS.

           EXEC SQL
            UPDATE DBO.LEFILE
             SET
              LEFILE.LE_BRNO = :QLE-BRNO,
              LEFILE.LE_ACCTNO = :QLE-ACCTNO,
              LEFILE.LE_TYPE = :QLE-TYPE,
              LEFILE.LE_FLAG = :QLE-FLAG,
              LEFILE.LE_SSNO = :QLE-SSNO,
              LEFILE.LE_OSACCTNO = :QLE-OSACCTNO,
              LEFILE.LE_LTOUCH_DATE =
                 :QLE-LTOUCH-DATE,
              LEFILE.LE_E_OSRATE = :QLE-E-OSRATE,
              LEFILE.LE_E_OSAMT = :QLE-E-OSAMT,
              LEFILE.LE_E_OSDATE =
                 :QLE-E-OSDATE,
              LEFILE.LE_E_AMT = :QLE-E-AMT,
              LEFILE.LE_E_DATE =
                 :QLE-E-DATE,
              LEFILE.LE_E_OSMAKERCD = :QLE-E-OSMAKERCD,
              LEFILE.LE_R_LOANDATE =
                 :QLE-R-LOANDATE,
              LEFILE.LE_R_LNAMT = :QLE-R-LNAMT,
              LEFILE.LE_R_POFFDATE = :QLE-R-POFFDATE,
              LEFILE.LE_R_RESTRICTED_MONEY =
                   :QLE-R-RESTRICTED-MONEY,
              LEFILE.LE_R_BALANCE = :QLE-R-BALANCE,
              LEFILE.LE_R_SALEFIN = :QLE-R-SALEFIN
             WHERE LEFILE.LE_BRNO = :QLE-BRNO
               AND LEFILE.LE_ACCTNO = :QLE-ACCTNO
               AND LEFILE.LE_TYPE = :QLE-TYPE
               AND LEFILE.LE_FLAG = :QLE-FLAG
               AND LEFILE.LE_SSNO = :QLE-SSNO
               AND LEFILE.LE_OSACCTNO = :QLE-OSACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LE1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LE1-FILE.

           MOVE LE-PATH-OWNBR  TO LE-BRNO.
           PERFORM DELETE-IT.
           MOVE LE-PATH-SQL    TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.
           PERFORM SET-LE-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LEFILE
             WHERE LEFILE.LE_BRNO = :QLE-BRNO
               AND LEFILE.LE_ACCTNO = :QLE-ACCTNO
               AND LEFILE.LE_TYPE = :QLE-TYPE
               AND LEFILE.LE_FLAG = :QLE-FLAG
               AND LEFILE.LE_SSNO = :QLE-SSNO
               AND LEFILE.LE_OSACCTNO = :QLE-OSACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LE1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LE1-FILE.

           PERFORM CLOSE-IT.

           IF ( LE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LE1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LE1-CURSOR-STAT.

