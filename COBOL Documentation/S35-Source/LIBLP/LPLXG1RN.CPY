      * COPYMEMBER: LIBLP/LPLXGE1RN
       LOAD-LXG1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LXG-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LXG-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LXG-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LXG-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LXG-ALL-DATA
              MOVE LXG-ALL-PATH             TO LXG-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LXG-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LXG-ALL-BASE
              MOVE FILPATH-MACHINE         TO LXG-ALL-MACHINE
              MOVE FILPATH-DATA            TO LXG-ALL-DATA
              MOVE LXG-ALL-PATH             TO LXG-PATH.

      *-----------------------------------------------------------------
       OPEN-LXG1-FILE.
           PERFORM LOAD-LXG1-FILE.
           PERFORM OPEN-IT.
           MOVE LXG-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LXG1-FILE.
           PERFORM START-IT.
           MOVE LXG-PATH-OWNBR  TO LXG-BRNO.
           MOVE LXG-PATH-SQL    TO E-FILE.
           MOVE LXG1-KEY        TO E-KEYX.

           IF ( LXG1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LXG1-FILE.

           IF ( QLXG1-WEND-SEQNO = ZEROES    ) OR
              ( QLXG1-WEND-SEQNO NOT NUMERIC )
              MOVE LXG-PATH-OWNBR        TO QLXG1-WBEG-BRNO
              MOVE LXG-PATH-OWNBR        TO QLXG1-WEND-BRNO
              MOVE LXG-ACCTNO            TO QLXG1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLXG1-WEND-ACCTNO
              MOVE LXG-SEQNO             TO QLXG1-WBEG-SEQNO
              MOVE ALL "9"               TO QLXG1-WEND-SEQNO.

           EXEC SQL
            DECLARE LXG1_CURSOR CURSOR FOR
            SELECT LXGFILE.LXG_BRNO,
                   LXGFILE.LXG_ACCTNO,
                   LXGFILE.LXG_SEQNO,
                   CAST(LXGFILE.LXG_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LXGFILE.LXG_GLNO_1,
                   LXGFILE.LXG_GLAMT_1,
                   LXGFILE.LXG_GLNO_2,
                   LXGFILE.LXG_GLAMT_2,
                   LXGFILE.LXG_GLNO_3,
                   LXGFILE.LXG_GLAMT_3
            FROM DBO.LXGFILE
            WHERE LXGFILE.LXG_BRNO  = :QLXG1-WBEG-BRNO
              AND LXGFILE.LXG_ACCTNO >= :QLXG1-WBEG-ACCTNO
              AND LXGFILE.LXG_ACCTNO <= :QLXG1-WEND-ACCTNO
              AND LXGFILE.LXG_SEQNO >= :QLXG1-WBEG-SEQNO
              AND LXGFILE.LXG_SEQNO <= :QLXG1-WEND-SEQNO
            ORDER BY LXGFILE.LXG_BRNO,
                     LXGFILE.LXG_ACCTNO,
                     LXGFILE.LXG_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LXG1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLXG1-WBEG-BRNO
                          QLXG1-WEND-BRNO
                          QLXG1-WBEG-ACCTNO
                          QLXG1-WEND-ACCTNO
                          QLXG1-WBEG-SEQNO
                          QLXG1-WEND-SEQNO.

           MOVE STAT---GOOD TO LXG1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LXG1-FILE.
           PERFORM READ-IT.
           MOVE LXG-PATH-OWNBR  TO LXG-BRNO. 
           MOVE LXG-PATH-SQL    TO E-FILE.
           MOVE LXG1-KEY        TO E-KEYX.
           INITIALIZE QLXG-REC.

           MOVE LXG-BRNO   TO QLXG-BRNO.
           MOVE LXG-ACCTNO TO QLXG-ACCTNO.
           MOVE LXG-SEQNO TO QLXG-SEQNO.

           EXEC SQL
            SELECT LXGFILE.LXG_BRNO,
                   LXGFILE.LXG_ACCTNO,
                   LXGFILE.LXG_SEQNO,
                   CAST(LXGFILE.LXG_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LXGFILE.LXG_GLNO_1,
                   LXGFILE.LXG_GLAMT_1,
                   LXGFILE.LXG_GLNO_2,
                   LXGFILE.LXG_GLAMT_2,
                   LXGFILE.LXG_GLNO_3,
                   LXGFILE.LXG_GLAMT_3
            INTO :QLXG-REC
            FROM DBO.LXGFILE
            WHERE LXGFILE.LXG_BRNO = :QLXG-BRNO
              AND LXGFILE.LXG_ACCTNO = :QLXG-ACCTNO
              AND LXGFILE.LXG_SEQNO = :QLXG-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LXG1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LXG-FIELDS.

      *-----------------------------------------------------------------
       READ-LXG1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LXG-PATH-OWNBR  TO LXG-BRNO.
           MOVE LXG-PATH-SQL TO E-FILE.
           MOVE LXG1-KEY        TO E-KEYX.
           INITIALIZE QLXG-REC.

           IF ( LXG1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LXG1_CURSOR INTO :QLXG-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LXG1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LXG-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LXG1-FILE.

           PERFORM CLOSE-IT.
           IF ( LXG1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LXG1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LXG1-CURSOR-STAT.

