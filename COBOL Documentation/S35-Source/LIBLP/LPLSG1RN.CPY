      * COPYMEMBER: LIBLP/LPLSG1RN
       LOAD-LSG1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSG-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSG-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSG-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSG-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSG-ALL-DATA
              MOVE LSG-ALL-PATH             TO LSG-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSG-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSG-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSG-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSG-ALL-DATA
              MOVE LSG-ALL-PATH             TO LSG-PATH.

      *-----------------------------------------------------------------
       OPEN-LSG1-FILE.
           PERFORM LOAD-LSG1-FILE.
           PERFORM OPEN-IT.
           MOVE LSG-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSG1-FILE.
           PERFORM START-IT.
           MOVE LSG-PATH-OWNBR  TO LSG-BRNO.
           MOVE LSG-PATH-SQL    TO E-FILE.
           MOVE LSG1-KEY        TO E-KEYX.

           IF ( LSG1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSG1-FILE.

           IF ( QLSG1-WEND-BRNO = ZEROES    ) OR
              ( QLSG1-WEND-BRNO NOT NUMERIC )
              MOVE LSG-PATH-OWNBR        TO QLSG1-WBEG-BRNO
              MOVE LSG-PATH-OWNBR        TO QLSG1-WEND-BRNO
              MOVE LSG-ACCTNO            TO QLSG1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSG1-WEND-ACCTNO
              MOVE LSG-SEQNO             TO QLSG1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSG1-WEND-SEQNO.

           EXEC SQL
            DECLARE LSG1_CURSOR CURSOR FOR
            SELECT LSGFILE.LSG_BRNO,
                   LSGFILE.LSG_ACCTNO,
                   LSGFILE.LSG_SEQNO,
                   CAST(LSGFILE.LSG_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LSGFILE.LSG_SERIAL_NO,
                   LSGFILE.LSG_STARTER_STAT,
                   LSGFILE.LSG_WARNING_STAT,
                   LSGFILE.LSG_RECOVERY_STAT,
                   CAST(LSGFILE.LSG_LAST_COMMAND_DATE
                                                  AS VARCHAR(10)),
                   LSGFILE.LSG_LAST_ADDRESS,
                   CAST(LSGFILE.LSG_LAST_AUTO_REPORT_DATE
                                                  AS VARCHAR(10)),
                   LSGFILE.LSG_GPS_INSTALLER
            FROM DBO.LSGFILE
            WHERE LSGFILE.LSG_BRNO  = :QLSG1-WBEG-BRNO
              AND LSGFILE.LSG_ACCTNO >= :QLSG1-WBEG-ACCTNO
              AND LSGFILE.LSG_ACCTNO <= :QLSG1-WEND-ACCTNO
              AND LSGFILE.LSG_SEQNO >= :QLSG1-WBEG-SEQNO
              AND LSGFILE.LSG_SEQNO <= :QLSG1-WEND-SEQNO
            ORDER BY LSGFILE.LSG_BRNO,
                     LSGFILE.LSG_ACCTNO,
                     LSGFILE.LSG_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSG1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSG1-WBEG-BRNO
                          QLSG1-WEND-BRNO
                          QLSG1-WBEG-ACCTNO
                          QLSG1-WEND-ACCTNO
                          QLSG1-WBEG-SEQNO
                          QLSG1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSG1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSG1-FILE.
           PERFORM READ-IT.
           MOVE LSG-PATH-OWNBR  TO LSG-BRNO. 
           MOVE LSG-PATH-SQL    TO E-FILE.
           MOVE LSG1-KEY        TO E-KEYX.
           INITIALIZE QLSG-REC.

           MOVE LSG-BRNO   TO QLSG-BRNO.
           MOVE LSG-ACCTNO TO QLSG-ACCTNO.
           MOVE LSG-SEQNO  TO QLSG-SEQNO.

           EXEC SQL
            SELECT LSGFILE.LSG_BRNO,
                   LSGFILE.LSG_ACCTNO,
                   LSGFILE.LSG_SEQNO,
                   CAST(LSGFILE.LSG_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LSGFILE.LSG_SERIAL_NO,
                   LSGFILE.LSG_STARTER_STAT,
                   LSGFILE.LSG_WARNING_STAT,
                   LSGFILE.LSG_RECOVERY_STAT,
                   CAST(LSGFILE.LSG_LAST_COMMAND_DATE
                                                  AS VARCHAR(10)),
                   LSGFILE.LSG_LAST_ADDRESS,
                   CAST(LSGFILE.LSG_LAST_AUTO_REPORT_DATE
                                                  AS VARCHAR(10)),
                   LSGFILE.LSG_GPS_INSTALLER
            INTO :QLSG-REC
            FROM DBO.LSGFILE
            WHERE LSGFILE.LSG_BRNO = :QLSG-BRNO
              AND LSGFILE.LSG_ACCTNO = :QLSG-ACCTNO
              AND LSGFILE.LSG_SEQNO = :QLSG-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSG1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSG-FIELDS.

      *-----------------------------------------------------------------
       READ-LSG1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSG-PATH-OWNBR  TO LSG-BRNO.
           MOVE LSG-PATH-SQL    TO E-FILE.
           MOVE LSG1-KEY        TO E-KEYX.
           INITIALIZE QLSG-REC.

           IF ( LSG1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSG1_CURSOR INTO :QLSG-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSG1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSG-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LSG1-FILE.

           PERFORM CLOSE-IT.
           IF ( LSG1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSG1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSG1-CURSOR-STAT.

