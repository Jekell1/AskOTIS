      * COPYMEMBER: LIBLP/LPLXE1IN
       LOAD-LXE1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LXE-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LXE-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LXE-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LXE-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LXE-ALL-DATA
              MOVE LXE-ALL-PATH             TO LXE-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LXE-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LXE-ALL-BASE
              MOVE FILPATH-MACHINE         TO LXE-ALL-MACHINE
              MOVE FILPATH-DATA            TO LXE-ALL-DATA
              MOVE LXE-ALL-PATH             TO LXE-PATH.

      *-----------------------------------------------------------------
       OPEN-LXE1-FILE.
           PERFORM LOAD-LXE1-FILE.
           PERFORM OPEN-IT.
           MOVE LXE-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LXE1-FILE.
           PERFORM START-IT.
           MOVE LXE-PATH-OWNBR  TO LXE-BRNO.
           MOVE LXE-PATH-SQL    TO E-FILE.
           MOVE LXE1-KEY        TO E-KEYX.

           IF ( LXE1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LXE1-FILE.

           IF ( QLXE1-WEND-SEQNO = ZEROES    ) OR
              ( QLXE1-WEND-SEQNO NOT NUMERIC )
              MOVE LXE-PATH-OWNBR        TO QLXE1-WBEG-BRNO
              MOVE LXE-PATH-OWNBR        TO QLXE1-WEND-BRNO
              MOVE LXE-ACCTNO            TO QLXE1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLXE1-WEND-ACCTNO
              MOVE LXE-SEQNO             TO QLXE1-WBEG-SEQNO
              MOVE ALL "9"               TO QLXE1-WEND-SEQNO.

           EXEC SQL
            DECLARE LXE1_CURSOR CURSOR FOR
            SELECT LXEFILE.LXE_BRNO,
                   LXEFILE.LXE_ACCTNO,
                   LXEFILE.LXE_SEQNO,
                   CAST(LXEFILE.LXE_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LXEFILE.LXE_EARN_1,
                   LXEFILE.LXE_EARN_2,
                   LXEFILE.LXE_EARN_3,
                   LXEFILE.LXE_EARN_4,
                   LXEFILE.LXE_EARN_5,
                   LXEFILE.LXE_EARN_6,
                   LXEFILE.LXE_EARN_7,
                   LXEFILE.LXE_WORKER,
                   LXEFILE.LXE_WORKER2
            FROM DBO.LXEFILE
            WHERE LXEFILE.LXE_BRNO  = :QLXE1-WBEG-BRNO
              AND LXEFILE.LXE_ACCTNO >= :QLXE1-WBEG-ACCTNO
              AND LXEFILE.LXE_ACCTNO <= :QLXE1-WEND-ACCTNO
              AND LXEFILE.LXE_SEQNO >= :QLXE1-WBEG-SEQNO
              AND LXEFILE.LXE_SEQNO <= :QLXE1-WEND-SEQNO
            ORDER BY LXEFILE.LXE_BRNO,
                     LXEFILE.LXE_ACCTNO,
                     LXEFILE.LXE_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LXE1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLXE1-WBEG-BRNO
                          QLXE1-WEND-BRNO
                          QLXE1-WBEG-ACCTNO
                          QLXE1-WEND-ACCTNO
                          QLXE1-WBEG-SEQNO
                          QLXE1-WEND-SEQNO.


           MOVE STAT---GOOD TO LXE1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LXE1-FILE.
           PERFORM READ-IT.
           MOVE LXE-PATH-OWNBR  TO LXE-BRNO. 
           MOVE LXE-PATH-SQL    TO E-FILE.
           MOVE LXE1-KEY        TO E-KEYX.
           INITIALIZE QLXE-REC.

           MOVE LXE-BRNO   TO QLXE-BRNO.
           MOVE LXE-ACCTNO TO QLXE-ACCTNO.
           MOVE LXE-SEQNO TO QLXE-SEQNO.

           EXEC SQL
            SELECT LXEFILE.LXE_BRNO,
                   LXEFILE.LXE_ACCTNO,
                   LXEFILE.LXE_SEQNO,
                   CAST(LXEFILE.LXE_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LXEFILE.LXE_EARN_1,
                   LXEFILE.LXE_EARN_2,
                   LXEFILE.LXE_EARN_3,
                   LXEFILE.LXE_EARN_4,
                   LXEFILE.LXE_EARN_5,
                   LXEFILE.LXE_EARN_6,
                   LXEFILE.LXE_EARN_7,
                   LXEFILE.LXE_WORKER,
                   LXEFILE.LXE_WORKER2
            INTO :QLXE-REC
            FROM DBO.LXEFILE
            WHERE LXEFILE.LXE_BRNO = :QLXE-BRNO
              AND LXEFILE.LXE_ACCTNO = :QLXE-ACCTNO
              AND LXEFILE.LXE_SEQNO = :QLXE-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LXE1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LXE-FIELDS.

      *-----------------------------------------------------------------
       READ-LXE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LXE-PATH-OWNBR  TO LXE-BRNO.
           MOVE LXE-PATH-SQL    TO E-FILE.
           MOVE LXE1-KEY        TO E-KEYX.
           INITIALIZE QLXE-REC.

           IF ( LXE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LXE1_CURSOR INTO :QLXE-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LXE1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LXE-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LXE1-FILE.

           MOVE LXE-PATH-OWNBR  TO LXE-BRNO.
           ACCEPT LXE-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LXE-PATH-SQL    TO E-FILE.
           MOVE LXE1-KEY        TO E-KEYX.
           PERFORM SET-LXE-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LXEFILE
             (LXEFILE.LXE_BRNO,
              LXEFILE.LXE_ACCTNO,
              LXEFILE.LXE_SEQNO,
              LXEFILE.LXE_LTOUCH_DATE,
              LXEFILE.LXE_EARN_1,
              LXEFILE.LXE_EARN_2,
              LXEFILE.LXE_EARN_3,
              LXEFILE.LXE_EARN_4,
              LXEFILE.LXE_EARN_5,
              LXEFILE.LXE_EARN_6,
              LXEFILE.LXE_EARN_7,
              LXEFILE.LXE_WORKER,
              LXEFILE.LXE_WORKER2)
            VALUES
             (:QLXE-BRNO,
              :QLXE-ACCTNO,
              :QLXE-SEQNO,
              :QLXE-LTOUCH-DATE,
              :QLXE-EARN-1,
              :QLXE-EARN-2,
              :QLXE-EARN-3,
              :QLXE-EARN-4,
              :QLXE-EARN-5,
              :QLXE-EARN-6,
              :QLXE-EARN-7,
              :QLXE-WORKER,
              :QLXE-WORKER2)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LXE1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LXE1-FILE.

           MOVE LXE-PATH-OWNBR  TO LXE-BRNO.
           ACCEPT LXE-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LXE-PATH-SQL    TO E-FILE.
           MOVE LXE1-KEY        TO E-KEYX.
           PERFORM SET-LXE-FIELDS.

           EXEC SQL
            UPDATE DBO.LXEFILE
             SET
              LXEFILE.LXE_BRNO = :QLXE-BRNO,
              LXEFILE.LXE_ACCTNO = :QLXE-ACCTNO,
              LXEFILE.LXE_SEQNO = :QLXE-SEQNO,
              LXEFILE.LXE_LTOUCH_DATE = :QLXE-LTOUCH-DATE,
              LXEFILE.LXE_EARN_1 = :QLXE-EARN-1,
              LXEFILE.LXE_EARN_2 = :QLXE-EARN-2,
              LXEFILE.LXE_EARN_3 = :QLXE-EARN-3,
              LXEFILE.LXE_EARN_4 = :QLXE-EARN-4,
              LXEFILE.LXE_EARN_5 = :QLXE-EARN-5,
              LXEFILE.LXE_EARN_6 = :QLXE-EARN-6,
              LXEFILE.LXE_EARN_7 = :QLXE-EARN-7,
              LXEFILE.LXE_WORKER = :QLXE-WORKER,
              LXEFILE.LXE_WORKER2 = :QLXE-WORKER2
             WHERE LXEFILE.LXE_BRNO = :QLXE-BRNO
               AND LXEFILE.LXE_ACCTNO = :QLXE-ACCTNO
               AND LXEFILE.LXE_SEQNO = :QLXE-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LXE1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LXE1-FILE.

           MOVE LXE-PATH-OWNBR  TO LXE-BRNO.
           PERFORM DELETE-IT.
           MOVE LXE-PATH-SQL    TO E-FILE.
           MOVE LXE1-KEY        TO E-KEYX.
           PERFORM SET-LXE-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LXEFILE
             WHERE LXEFILE.LXE_BRNO = :QLXE-BRNO
               AND LXEFILE.LXE_ACCTNO = :QLXE-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LXE1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LXE1-FILE.

           PERFORM CLOSE-IT.

           IF ( LXE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LXE1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LXE1-CURSOR-STAT.

