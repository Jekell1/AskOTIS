      * COPYMEMBER: LIBLP/LPLT1RN
       LOAD-LTF1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTF-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTF-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTF-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTF-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTF-ALL-DATA
              MOVE LTF-ALL-PATH             TO LTF-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTF-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTF-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTF-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTF-ALL-DATA
              MOVE LTF-ALL-PATH             TO LTF-PATH.

      *-----------------------------------------------------------------
       OPEN-LTF1-FILE.
           PERFORM LOAD-LTF1-FILE.
           PERFORM OPEN-IT.
           MOVE LTF-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTF1-FILE.
           PERFORM START-IT.
           MOVE LTF-PATH-OWNBR  TO LTF-BRNO.
           MOVE LTF-PATH-SQL    TO E-FILE.
           MOVE LTF1-KEY        TO E-KEYX.

           IF ( LTF1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTF1-FILE.

           IF ( QLTF1-WEND-BRNO = ZEROES    ) OR
              ( QLTF1-WEND-BRNO NOT NUMERIC )
              MOVE LTF-PATH-OWNBR        TO QLTF1-WBEG-BRNO
              MOVE LTF-PATH-OWNBR        TO QLTF1-WEND-BRNO
              MOVE LTF-ACCTNO            TO QLTF1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTF1-WEND-ACCTNO
              MOVE LTF-SEQNO             TO QLTF1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTF1-WEND-SEQNO.

           EXEC SQL
            DECLARE LTF1_CURSOR CURSOR FOR
            SELECT LTFFILE.LTF_BRNO,
                   LTFFILE.LTF_ACCTNO,
                   LTFFILE.LTF_SEQNO,
                   CAST(LTFFILE.LTF_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LTFFILE.LTF_FEECODE_1,
                   LTFFILE.LTF_FEECODE_PAYABLE_1,
                   LTFFILE.LTF_FEECODE_2,
                   LTFFILE.LTF_FEECODE_PAYABLE_2,
                   LTFFILE.LTF_FEECODE_3,
                   LTFFILE.LTF_FEECODE_PAYABLE_3,
                   LTFFILE.LTF_FEECODE_4,
                   LTFFILE.LTF_FEECODE_PAYABLE_4,
                   LTFFILE.LTF_FEECODE_5,
                   LTFFILE.LTF_FEECODE_PAYABLE_5,
                   LTFFILE.LTF_FEECODE_6,
                   LTFFILE.LTF_FEECODE_PAYABLE_6,
                   LTFFILE.LTF_FEECODE_7,
                   LTFFILE.LTF_FEECODE_PAYABLE_7,
                   LTFFILE.LTF_FEECODE_8,
                   LTFFILE.LTF_FEECODE_PAYABLE_8,
                   LTFFILE.LTF_FEECODE_9,
                   LTFFILE.LTF_FEECODE_PAYABLE_9,
                   LTFFILE.LTF_FEECODE_10,
                   LTFFILE.LTF_FEECODE_PAYABLE_10
            FROM DBO.LTFFILE
            WHERE LTFFILE.LTF_BRNO  = :QLTF1-WBEG-BRNO
              AND LTFFILE.LTF_ACCTNO >= :QLTF1-WBEG-ACCTNO
              AND LTFFILE.LTF_ACCTNO <= :QLTF1-WEND-ACCTNO
              AND LTFFILE.LTF_SEQNO >= :QLTF1-WBEG-SEQNO
              AND LTFFILE.LTF_SEQNO <= :QLTF1-WEND-SEQNO
            ORDER BY LTFFILE.LTF_BRNO,
                     LTFFILE.LTF_ACCTNO,
                     LTFFILE.LTF_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTF1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTF1-WBEG-BRNO
                          QLTF1-WEND-BRNO
                          QLTF1-WBEG-ACCTNO
                          QLTF1-WEND-ACCTNO
                          QLTF1-WBEG-SEQNO
                          QLTF1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTF1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTF1-FILE.
           PERFORM READ-IT.
           MOVE LTF-PATH-OWNBR  TO LTF-BRNO. 
           MOVE LTF-PATH-SQL    TO E-FILE.
           MOVE LTF1-KEY        TO E-KEYX.
           INITIALIZE QLTF-REC.

           MOVE LTF-BRNO   TO QLTF-BRNO.
           MOVE LTF-ACCTNO TO QLTF-ACCTNO.
           MOVE LTF-SEQNO  TO QLTF-SEQNO.

           EXEC SQL
            SELECT LTFFILE.LTF_BRNO,
                   LTFFILE.LTF_ACCTNO,
                   LTFFILE.LTF_SEQNO,
                   CAST(LTFFILE.LTF_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LTFFILE.LTF_FEECODE_1,
                   LTFFILE.LTF_FEECODE_PAYABLE_1,
                   LTFFILE.LTF_FEECODE_2,
                   LTFFILE.LTF_FEECODE_PAYABLE_2,
                   LTFFILE.LTF_FEECODE_3,
                   LTFFILE.LTF_FEECODE_PAYABLE_3,
                   LTFFILE.LTF_FEECODE_4,
                   LTFFILE.LTF_FEECODE_PAYABLE_4,
                   LTFFILE.LTF_FEECODE_5,
                   LTFFILE.LTF_FEECODE_PAYABLE_5,
                   LTFFILE.LTF_FEECODE_6,
                   LTFFILE.LTF_FEECODE_PAYABLE_6,
                   LTFFILE.LTF_FEECODE_7,
                   LTFFILE.LTF_FEECODE_PAYABLE_7,
                   LTFFILE.LTF_FEECODE_8,
                   LTFFILE.LTF_FEECODE_PAYABLE_8,
                   LTFFILE.LTF_FEECODE_9,
                   LTFFILE.LTF_FEECODE_PAYABLE_9,
                   LTFFILE.LTF_FEECODE_10,
                   LTFFILE.LTF_FEECODE_PAYABLE_10
            INTO :QLTF-REC
            FROM DBO.LTFFILE
            WHERE LTFFILE.LTF_BRNO = :QLTF-BRNO
              AND LTFFILE.LTF_ACCTNO = :QLTF-ACCTNO
              AND LTFFILE.LTF_SEQNO = :QLTF-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTF1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTF-FIELDS.

      *-----------------------------------------------------------------
       READ-LTF1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTF-PATH-OWNBR  TO LTF-BRNO.
           MOVE LTF-PATH-SQL    TO E-FILE.
           MOVE LTF1-KEY        TO E-KEYX.
           INITIALIZE QLTF-REC.

           IF ( LTF1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTF1_CURSOR INTO :QLTF-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTF1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTF-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTF1-FILE.

           PERFORM CLOSE-IT.
           IF ( LTF1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTF1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTF1-CURSOR-STAT.

