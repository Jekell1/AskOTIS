      * COPYMEMBER: LIBLP/LPLH1IN
       LOAD-LH1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LH-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LH-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LH-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LH-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LH-ALL-DATA
              MOVE LH-ALL-PATH             TO LH-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LH-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LH-ALL-BASE
              MOVE FILPATH-MACHINE         TO LH-ALL-MACHINE
              MOVE FILPATH-DATA            TO LH-ALL-DATA
              MOVE LH-ALL-PATH             TO LH-PATH.

      *-----------------------------------------------------------------
       OPEN-LH1-FILE.
           PERFORM LOAD-LH1-FILE.
           PERFORM OPEN-IT.
           MOVE LH-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LH1-FILE.
           PERFORM START-IT.
           MOVE LH-PATH-OWNBR  TO LH-BRNO.
           MOVE LH-PATH-SQL    TO E-FILE.
           MOVE LH1-KEY        TO E-KEYX.

           IF ( LH1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LH1-FILE.

           IF ( QLH1-WEND-ACCTNO = ZEROES    ) OR
              ( QLH1-WEND-ACCTNO NOT NUMERIC )
              MOVE LH-PATH-OWNBR        TO QLH1-WBEG-BRNO
              MOVE LH-PATH-OWNBR        TO QLH1-WEND-BRNO
              MOVE LH-SSNO              TO QLH1-WBEG-SSNO
              MOVE ALL "9"              TO QLH1-WEND-SSNO
              MOVE LH-ACCTNO            TO QLH1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLH1-WEND-ACCTNO.

           EXEC SQL
            DECLARE LH1_CURSOR CURSOR FOR
            SELECT LHFILE.LH_BRNO,
                   LHFILE.LH_SSNO,
                   LHFILE.LH_ACCTNO,
                   CAST(LHFILE.LH_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LHFILE.LH_CLASS,
                   LHFILE.LH_LAWCD,
                   LHFILE.LH_COLLCD,
                   LHFILE.LH_MAKERCD,
                   LHFILE.LH_POCD,
                   CAST(LHFILE.LH_LOANDATE AS VARCHAR(10)),
                   CAST(LHFILE.LH_PLDATE AS VARCHAR(10)),
                   CAST(LHFILE.LH_JDDATE AS VARCHAR(10)),
                   CAST(LHFILE.LH_POFFDATE AS VARCHAR(10)),
                   LHFILE.LH_LNAMT,
                   LHFILE.LH_ORGTERM,
                   LHFILE.LH_POTERMS,
                   LHFILE.LH_PYPROFILE,
                   LHFILE.LH_DLNO,
                   LHFILE.LH_STATUS,
                   LHFILE.LH_ARCHIVE_FG
            FROM DBO.LHFILE
            WHERE LHFILE.LH_BRNO  = :QLH1-WBEG-BRNO
              AND LHFILE.LH_SSNO >= :QLH1-WBEG-SSNO
              AND LHFILE.LH_SSNO <= :QLH1-WEND-SSNO
              AND LHFILE.LH_ACCTNO >= :QLH1-WBEG-ACCTNO
              AND LHFILE.LH_ACCTNO <= :QLH1-WEND-ACCTNO
            ORDER BY LHFILE.LH_BRNO,
                     LHFILE.LH_SSNO,
                     LHFILE.LH_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LH1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLH1-WBEG-BRNO 
                          QLH1-WEND-BRNO
                          QLH1-WBEG-SSNO
                          QLH1-WEND-SSNO
                          QLH1-WBEG-ACCTNO
                          QLH1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LH1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LH1-FILE.
           PERFORM READ-IT.
           MOVE LH-PATH-OWNBR  TO LH-BRNO. 
           MOVE LH-PATH-SQL    TO E-FILE.
           MOVE LH1-KEY        TO E-KEYX.
           INITIALIZE QLH-REC.

           MOVE LH-BRNO   TO QLH-BRNO.
           MOVE LH-SSNO   TO QLH-SSNO.
           MOVE LH-ACCTNO TO QLH-ACCTNO.

           EXEC SQL
            SELECT LHFILE.LH_BRNO,
                   LHFILE.LH_SSNO,
                   LHFILE.LH_ACCTNO,
                   CAST(LHFILE.LH_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LHFILE.LH_CLASS,
                   LHFILE.LH_LAWCD,
                   LHFILE.LH_COLLCD,
                   LHFILE.LH_MAKERCD,
                   LHFILE.LH_POCD,
                   CAST(LHFILE.LH_LOANDATE AS VARCHAR(10)),
                   CAST(LHFILE.LH_PLDATE AS VARCHAR(10)),
                   CAST(LHFILE.LH_JDDATE AS VARCHAR(10)),
                   CAST(LHFILE.LH_POFFDATE AS VARCHAR(10)),
                   LHFILE.LH_LNAMT,
                   LHFILE.LH_ORGTERM,
                   LHFILE.LH_POTERMS,
                   LHFILE.LH_PYPROFILE,
                   LHFILE.LH_DLNO,
                   LHFILE.LH_STATUS,
                   LHFILE.LH_ARCHIVE_FG
            INTO :QLH-REC
            FROM DBO.LHFILE
            WHERE LHFILE.LH_BRNO = :QLH-BRNO
              AND LHFILE.LH_SSNO = :QLH-SSNO
              AND LHFILE.LH_ACCTNO = :QLH-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LH1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LH-FIELDS.

      *-----------------------------------------------------------------
       READ-LH1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LH-PATH-OWNBR  TO LH-BRNO.
           MOVE LH-PATH-SQL    TO E-FILE.
           MOVE LH1-KEY        TO E-KEYX.
           INITIALIZE QLH-REC.

           IF ( LH1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LH1_CURSOR INTO :QLH-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LH1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LH-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LH1-FILE.

           MOVE LH-PATH-OWNBR  TO LH-BRNO.
           ACCEPT LH-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LH-PATH-SQL    TO E-FILE.
           MOVE LH1-KEY        TO E-KEYX.
           PERFORM SET-LH-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LHFILE
             (LHFILE.LH_BRNO,
              LHFILE.LH_SSNO,
              LHFILE.LH_ACCTNO,
              LHFILE.LH_LTOUCH_DATE,
              LHFILE.LH_CLASS,
              LHFILE.LH_LAWCD,
              LHFILE.LH_COLLCD,
              LHFILE.LH_MAKERCD,
              LHFILE.LH_POCD,
              LHFILE.LH_LOANDATE,
              LHFILE.LH_PLDATE,
              LHFILE.LH_JDDATE,
              LHFILE.LH_POFFDATE,
              LHFILE.LH_LNAMT,
              LHFILE.LH_ORGTERM,
              LHFILE.LH_POTERMS,
              LHFILE.LH_PYPROFILE,
              LHFILE.LH_DLNO,
              LHFILE.LH_STATUS,
              LHFILE.LH_ARCHIVE_FG)
            VALUES
             (:QLH-BRNO,
              :QLH-SSNO,
              :QLH-ACCTNO,
              :QLH-LTOUCH-DATE,
              :QLH-CLASS,
              :QLH-LAWCD,
              :QLH-COLLCD,
              :QLH-MAKERCD,
              :QLH-POCD,
              :QLH-LOANDATE,
              :QLH-PLDATE,
              :QLH-JDDATE,
              :QLH-POFFDATE,
              :QLH-LNAMT,
              :QLH-ORGTERM,
              :QLH-POTERMS,
              :QLH-PYPROFILE,
              :QLH-DLNO,
              :QLH-STATUS,
              :QLH-ARCHIVE-FG)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LH1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LH1-FILE.

           MOVE LH-PATH-OWNBR  TO LH-BRNO.
           ACCEPT LH-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LH-PATH-SQL    TO E-FILE.
           MOVE LH1-KEY        TO E-KEYX.
           PERFORM SET-LH-FIELDS.

           EXEC SQL
            UPDATE DBO.LHFILE
             SET
              LHFILE.LH_BRNO = :QLH-BRNO,
              LHFILE.LH_SSNO = :QLH-SSNO,
              LHFILE.LH_ACCTNO = :QLH-ACCTNO,
              LHFILE.LH_LTOUCH_DATE =
                 :QLH-LTOUCH-DATE,
              LHFILE.LH_CLASS = :QLH-CLASS,
              LHFILE.LH_LAWCD = :QLH-LAWCD,
              LHFILE.LH_COLLCD = :QLH-COLLCD,
              LHFILE.LH_MAKERCD = :QLH-MAKERCD,
              LHFILE.LH_POCD = :QLH-POCD,
              LHFILE.LH_LOANDATE =
                 :QLH-LOANDATE,
              LHFILE.LH_PLDATE =
                 :QLH-PLDATE,
              LHFILE.LH_JDDATE =
                 :QLH-JDDATE,
              LHFILE.LH_POFFDATE = :QLH-POFFDATE,
              LHFILE.LH_LNAMT = :QLH-LNAMT,
              LHFILE.LH_ORGTERM = :QLH-ORGTERM,
              LHFILE.LH_POTERMS = :QLH-POTERMS,
              LHFILE.LH_PYPROFILE = :QLH-PYPROFILE,
              LHFILE.LH_DLNO = :QLH-DLNO,
              LHFILE.LH_STATUS = :QLH-STATUS,
              LHFILE.LH_ARCHIVE_FG = :QLH-ARCHIVE-FG
             WHERE LHFILE.LH_BRNO = :QLH-BRNO
               AND LHFILE.LH_SSNO = :QLH-SSNO
               AND LHFILE.LH_ACCTNO = :QLH-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LH1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LH1-FILE.

           MOVE LH-PATH-OWNBR  TO LH-BRNO.
           PERFORM DELETE-IT.
           MOVE LH-PATH-SQL    TO E-FILE.
           MOVE LH1-KEY        TO E-KEYX.
           PERFORM SET-LH-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LHFILE
             WHERE LHFILE.LH_BRNO = :QLH-BRNO
               AND LHFILE.LH_SSNO = :QLH-SSNO
               AND LHFILE.LH_ACCTNO = :QLH-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LH1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LH1-FILE.

           PERFORM CLOSE-IT.

           IF ( LH1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LH1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LH1-CURSOR-STAT.

