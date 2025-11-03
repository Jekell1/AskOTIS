      * COPYMEMBER: LIBLP/LPLTF1IN FEE CODE PAYABLE TRAILER
      * REV:
      *
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
            WHERE LTFFILE.LTF_BRNO = :QLTF1-WBEG-BRNO
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
       WRITE-LTF1-FILE.

           MOVE LTF-PATH-OWNBR  TO LTF-BRNO.
           ACCEPT LTF-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTF-PATH-SQL    TO E-FILE.
           MOVE LTF1-KEY        TO E-KEYX.
           PERFORM SET-LTF-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LTFFILE
             (LTFFILE.LTF_BRNO,
              LTFFILE.LTF_ACCTNO,
              LTFFILE.LTF_SEQNO,
              LTFFILE.LTF_LTOUCH_DATE,
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
              LTFFILE.LTF_FEECODE_PAYABLE_10)
            VALUES
             (:QLTF-BRNO,
              :QLTF-ACCTNO,
              :QLTF-SEQNO,
              :QLTF-LTOUCH-DATE,
              :QLTF-FEECODE-1,
              :QLTF-FEECODE-PAYABLE-1,
              :QLTF-FEECODE-2,
              :QLTF-FEECODE-PAYABLE-2,
              :QLTF-FEECODE-3,
              :QLTF-FEECODE-PAYABLE-3,
              :QLTF-FEECODE-4,
              :QLTF-FEECODE-PAYABLE-4,
              :QLTF-FEECODE-5,
              :QLTF-FEECODE-PAYABLE-5,
              :QLTF-FEECODE-6,
              :QLTF-FEECODE-PAYABLE-6,
              :QLTF-FEECODE-7,
              :QLTF-FEECODE-PAYABLE-7,
              :QLTF-FEECODE-8,
              :QLTF-FEECODE-PAYABLE-8,
              :QLTF-FEECODE-9,
              :QLTF-FEECODE-PAYABLE-9,
              :QLTF-FEECODE-10,
              :QLTF-FEECODE-PAYABLE-10)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTF1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LTF1-FILE.

           MOVE LTF-PATH-OWNBR  TO LTF-BRNO.
           ACCEPT LTF-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTF-PATH-SQL    TO E-FILE.
           MOVE LTF1-KEY        TO E-KEYX.
           PERFORM SET-LTF-FIELDS.

           EXEC SQL
            UPDATE DBO.LTFFILE
             SET
              LTFFILE.LTF_BRNO = :QLTF-BRNO,
              LTFFILE.LTF_ACCTNO = :QLTF-ACCTNO,
              LTFFILE.LTF_SEQNO = :QLTF-SEQNO,
              LTFFILE.LTF_LTOUCH_DATE = :QLTF-LTOUCH-DATE,
              LTFFILE.LTF_FEECODE_1 = :QLTF-FEECODE-1,
              LTFFILE.LTF_FEECODE_PAYABLE_1 =
                   :QLTF-FEECODE-PAYABLE-1,
              LTFFILE.LTF_FEECODE_2 = :QLTF-FEECODE-2,
              LTFFILE.LTF_FEECODE_PAYABLE_2 =
                   :QLTF-FEECODE-PAYABLE-2,
              LTFFILE.LTF_FEECODE_3 = :QLTF-FEECODE-3,
              LTFFILE.LTF_FEECODE_PAYABLE_3 =
                   :QLTF-FEECODE-PAYABLE-3,
              LTFFILE.LTF_FEECODE_4 = :QLTF-FEECODE-4,
              LTFFILE.LTF_FEECODE_PAYABLE_4 =
                   :QLTF-FEECODE-PAYABLE-4,
              LTFFILE.LTF_FEECODE_5 = :QLTF-FEECODE-5,
              LTFFILE.LTF_FEECODE_PAYABLE_5 =
                   :QLTF-FEECODE-PAYABLE-5,
              LTFFILE.LTF_FEECODE_6 = :QLTF-FEECODE-6,
              LTFFILE.LTF_FEECODE_PAYABLE_6 =
                   :QLTF-FEECODE-PAYABLE-6,
              LTFFILE.LTF_FEECODE_7 = :QLTF-FEECODE-7,
              LTFFILE.LTF_FEECODE_PAYABLE_7 =
                   :QLTF-FEECODE-PAYABLE-7,
              LTFFILE.LTF_FEECODE_8 = :QLTF-FEECODE-8,
              LTFFILE.LTF_FEECODE_PAYABLE_8 =
                   :QLTF-FEECODE-PAYABLE-8,
              LTFFILE.LTF_FEECODE_9 = :QLTF-FEECODE-9,
              LTFFILE.LTF_FEECODE_PAYABLE_9 =
                   :QLTF-FEECODE-PAYABLE-9,
              LTFFILE.LTF_FEECODE_10 = :QLTF-FEECODE-10,
              LTFFILE.LTF_FEECODE_PAYABLE_10 =
                   :QLTF-FEECODE-PAYABLE-10
             WHERE LTFFILE.LTF_BRNO = :QLTF-BRNO
               AND LTFFILE.LTF_ACCTNO = :QLTF-ACCTNO
               AND LTFFILE.LTF_SEQNO = :QLTF-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTF1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LTF1-FILE.

           MOVE LTF-PATH-OWNBR  TO LTF-BRNO.
           PERFORM DELETE-IT.
           MOVE LTF-PATH-SQL    TO E-FILE.
           MOVE LTF1-KEY        TO E-KEYX.
           PERFORM SET-LTF-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LTFFILE
             WHERE LTFFILE.LTF_BRNO = :QLTF-BRNO
               AND LTFFILE.LTF_ACCTNO = :QLTF-ACCTNO
               AND LTFFILE.LTF_SEQNO = :QLTF-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LTF1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LTF1-FILE.

           PERFORM CLOSE-IT.

           IF ( LTF1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTF1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTF1-CURSOR-STAT.

