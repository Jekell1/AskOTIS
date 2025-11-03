      * COPYMEMBER: LIBLP/LPLXE1RN
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
       CLOSE-LXE1-FILE.

           PERFORM CLOSE-IT.
           IF ( LXE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LXE1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LXE1-CURSOR-STAT.

