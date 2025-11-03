      * COPYMEMBER: LIBLP/LPLH1RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
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
       START-LH1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE LH-PATH-OWNBR  TO LH-BRNO.
           MOVE LH-PATH-SQL    TO E-FILE.
           MOVE LH1-KEY        TO E-KEYX.

           IF ( LH1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-LH1-FILE.

           IF ( QLH1-WEND-ACCTNO = ZEROES    ) OR
              ( QLH1-WEND-ACCTNO NOT NUMERIC )
              MOVE LH-PATH-OWNBR        TO QLH1-WBEG-BRNO
              MOVE LH-PATH-OWNBR        TO QLH1-WEND-BRNO
              MOVE ALL "0"              TO QLH1-WBEG-SSNO
              MOVE LH-SSNO              TO QLH1-WEND-SSNO
              MOVE ALL "0"              TO QLH1-WBEG-ACCTNO
              MOVE LH-ACCTNO            TO QLH1-WEND-ACCTNO.

           EXEC SQL
            DECLARE LH1_CURSOR_NG CURSOR FOR
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
           ORDER BY LHFILE.LH_BRNO DESC,
                    LHFILE.LH_SSNO DESC,
                    LHFILE.LH_ACCTNO DESC
           END-EXEC.

           EXEC SQL
               OPEN LH1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLH1-WBEG-BRNO 
                          QLH1-WEND-BRNO
                          QLH1-WBEG-SSNO
                          QLH1-WEND-SSNO
                          QLH1-WBEG-ACCTNO
                          QLH1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LH1-CURSOR-NG-STAT.

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
       READ-LH1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE LH-PATH-OWNBR  TO LH-BRNO.
           MOVE LH-PATH-SQL    TO E-FILE.
           MOVE LH1-KEY        TO E-KEYX.
           INITIALIZE QLH-REC.

           IF ( LH1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH LH1_CURSOR_NG INTO :QLH-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LH1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-LH-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LH1-FILE.

           PERFORM CLOSE-IT.
           IF ( LH1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LH1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LH1-CURSOR-STAT.

           IF ( LH1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE LH1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO LH1-CURSOR-NG-STAT.

