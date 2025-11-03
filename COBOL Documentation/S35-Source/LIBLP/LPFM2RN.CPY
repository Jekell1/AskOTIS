      * COPYMEMBER: LIBLP/LPFM2RN
       LOAD-FM2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( FM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( FM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO FM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO FM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO FM-ALL-DATA
              MOVE FM-ALL-PATH             TO FM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( FM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO FM-ALL-BASE
              MOVE FILPATH-MACHINE         TO FM-ALL-MACHINE
              MOVE FILPATH-DATA            TO FM-ALL-DATA
              MOVE FM-ALL-PATH             TO FM-PATH.

      *-----------------------------------------------------------------
       OPEN-FM2-FILE.
           PERFORM LOAD-FM2-FILE.
           PERFORM OPEN-IT.
           MOVE FM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-FM2-FILE.
           PERFORM START-IT.
           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING FM-BRNO, FM-FMNO, FM-ZIP DELIMITED BY SIZE
                               INTO E-KEYX.

           IF ( FM2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-FM2-FILE.

           IF ( QFM2-WEND-FMNO = ZEROES    ) OR
              ( QFM2-WEND-FMNO NOT NUMERIC )
              MOVE FM-PATH-OWNBR        TO QFM2-WBEG-BRNO
              MOVE FM-PATH-OWNBR        TO QFM2-WEND-BRNO
              MOVE FM-FMNO              TO QFM2-WBEG-FMNO
              MOVE ALL "9"              TO QFM2-WEND-FMNO
              MOVE FM-ZIP               TO QFM2-WBEG-ZIP
              MOVE ALL "Z"              TO QFM2-WEND-ZIP.

           EXEC SQL
            DECLARE FM2_CURSOR CURSOR FOR
            SELECT FMFILE.FM_BRNO,
                   FMFILE.FM_INITKEY,
                   FMFILE.FM_FMNO,
                   FMFILE.FM_ZIP,
                   FMFILE.FM_PRINT,
                   FMFILE.FM_STMT_COUNTER,
                   CAST(FMFILE.FM_TRDATE AS VARCHAR(10)),
                   FMFILE.FM_USERID,
                   CAST(FMFILE.FM_LTOUCH_DATE
                                                  AS VARCHAR(10))
            FROM DBO.FMFILE
            WHERE FMFILE.FM_BRNO  = :QFM2-WBEG-BRNO
              AND FMFILE.FM_FMNO >= :QFM2-WBEG-FMNO
              AND FMFILE.FM_FMNO <= :QFM2-WEND-FMNO
              AND FMFILE.FM_ZIP >= :QFM2-WBEG-ZIP
              AND FMFILE.FM_ZIP <= :QFM2-WEND-ZIP
            ORDER BY FMFILE.FM_BRNO,
                     FMFILE.FM_FMNO,
                     FMFILE.FM_ZIP,
                     FMFILE.FM_INITKEY
           END-EXEC.

           EXEC SQL
               OPEN FM2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QFM2-WBEG-BRNO 
                          QFM2-WEND-BRNO
                          QFM2-WBEG-FMNO
                          QFM2-WEND-FMNO.
           MOVE SPACES TO QFM2-WBEG-ZIP
                          QFM2-WEND-ZIP.
                            

           MOVE STAT---GOOD TO FM2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-FM2-FILE.
           PERFORM READ-IT.
           MOVE FM-PATH-OWNBR  TO FM-BRNO. 
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING FM-BRNO, FM-FMNO, FM-ZIP DELIMITED BY SIZE
                               INTO E-KEYX.
           INITIALIZE QFM-REC.

           MOVE FM-BRNO    TO QFM-BRNO.
           MOVE FM-FMNO    TO QFM-FMNO.
           MOVE FM-ZIP     TO QFM-ZIP.

           EXEC SQL
            SELECT FMFILE.FM_BRNO,
                   FMFILE.FM_INITKEY,
                   FMFILE.FM_FMNO,
                   FMFILE.FM_ZIP,
                   FMFILE.FM_PRINT,
                   FMFILE.FM_STMT_COUNTER,
                   CAST(FMFILE.FM_TRDATE AS VARCHAR(10)),
                   FMFILE.FM_USERID,
                   CAST(FMFILE.FM_LTOUCH_DATE
                                                  AS VARCHAR(10))
            INTO :QFM-REC
            FROM DBO.FMFILE
            WHERE FMFILE.FM_BRNO = :QFM-BRNO
              AND FMFILE.FM_FMNO = :QFM-FMNO
              AND FMFILE.FM_ZIP = :QFM-ZIP
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-FM2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-FM-FIELDS.

      *-----------------------------------------------------------------
       READ-FM2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING FM-BRNO, FM-FMNO, FM-ZIP DELIMITED BY SIZE
                               INTO E-KEYX.
           INITIALIZE QFM-REC.

           IF ( FM2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH FM2_CURSOR INTO :QFM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-FM2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-FM-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-FM2-FILE.

           PERFORM CLOSE-IT.
           IF ( FM2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE FM2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO FM2-CURSOR-STAT.

