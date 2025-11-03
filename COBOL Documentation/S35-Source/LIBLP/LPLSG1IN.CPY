      * COPYMEMBER: LIBLP/LPLSG1IN
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
       WRITE-LSG1-FILE.

           MOVE LSG-PATH-OWNBR  TO LSG-BRNO.
           ACCEPT LSG-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LSG-PATH-SQL    TO E-FILE.
           MOVE LSG1-KEY        TO E-KEYX.
           PERFORM SET-LSG-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LSGFILE
             (LSGFILE.LSG_BRNO,
              LSGFILE.LSG_ACCTNO,
              LSGFILE.LSG_SEQNO,
              LSGFILE.LSG_LTOUCH_DATE,
              LSGFILE.LSG_SERIAL_NO,
              LSGFILE.LSG_STARTER_STAT,
              LSGFILE.LSG_WARNING_STAT,
              LSGFILE.LSG_RECOVERY_STAT,
              LSGFILE.LSG_LAST_COMMAND_DATE,
              LSGFILE.LSG_LAST_ADDRESS,
              LSGFILE.LSG_LAST_AUTO_REPORT_DATE,
              LSGFILE.LSG_GPS_INSTALLER)
            VALUES
             (:QLSG-BRNO,
              :QLSG-ACCTNO,
              :QLSG-SEQNO,
              :QLSG-LTOUCH-DATE,
              :QLSG-SERIAL-NO,
              :QLSG-STARTER-STAT,
              :QLSG-WARNING-STAT,
              :QLSG-RECOVERY-STAT,
              :QLSG-LAST-COMMAND-DATE,
              :QLSG-LAST-ADDRESS,
              :QLSG-LAST-AUTO-REPORT-DATE,
              :QLSG-GPS-INSTALLER)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LSG1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LSG1-FILE.

           MOVE LSG-PATH-OWNBR  TO LSG-BRNO.
           ACCEPT LSG-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LSG-PATH-SQL    TO E-FILE.
           MOVE LSG1-KEY        TO E-KEYX.
           PERFORM SET-LSG-FIELDS.

           EXEC SQL
            UPDATE DBO.LSGFILE
             SET
              LSGFILE.LSG_BRNO = :QLSG-BRNO,
              LSGFILE.LSG_ACCTNO = :QLSG-ACCTNO,
              LSGFILE.LSG_SEQNO = :QLSG-SEQNO,
              LSGFILE.LSG_LTOUCH_DATE =
                 :QLSG-LTOUCH-DATE,
              LSGFILE.LSG_SERIAL_NO = :QLSG-SERIAL-NO,
              LSGFILE.LSG_STARTER_STAT = :QLSG-STARTER-STAT,
              LSGFILE.LSG_WARNING_STAT = :QLSG-WARNING-STAT,
              LSGFILE.LSG_RECOVERY_STAT = :QLSG-RECOVERY-STAT,
              LSGFILE.LSG_LAST_COMMAND_DATE =
                 :QLSG-LAST-COMMAND-DATE,
              LSGFILE.LSG_LAST_ADDRESS = :QLSG-LAST-ADDRESS,
              LSGFILE.LSG_LAST_AUTO_REPORT_DATE =
                 :QLSG-LAST-AUTO-REPORT-DATE,
              LSGFILE.LSG_GPS_INSTALLER = :QLSG-GPS-INSTALLER
             WHERE LSGFILE.LSG_BRNO = :QLSG-BRNO
               AND LSGFILE.LSG_ACCTNO = :QLSG-ACCTNO
               AND LSGFILE.LSG_SEQNO = :QLSG-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LSG1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LSG1-FILE.

           MOVE LSG-PATH-OWNBR  TO LSG-BRNO.
           PERFORM DELETE-IT.
           MOVE LSG-PATH-SQL    TO E-FILE.
           MOVE LSG1-KEY        TO E-KEYX.
           PERFORM SET-LSG-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LSGFILE
             WHERE LSGFILE.LSG_BRNO = :QLSG-BRNO
               AND LSGFILE.LSG_ACCTNO = :QLSG-ACCTNO
               AND LSGFILE.LSG_SEQNO = :QLSG-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LSG1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LSG1-FILE.

           PERFORM CLOSE-IT.

           IF ( LSG1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSG1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSG1-CURSOR-STAT.

