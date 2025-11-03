      * COPYMEMBER: LIBLP/LPCDB1RN
       LOAD-CDB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CDB-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CDB-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CDB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CDB-ALL-MACHINE
              MOVE CDB-ALL-PATH             TO CDB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CDB-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CDB-ALL-BASE
              MOVE FILPATH-MACHINE         TO CDB-ALL-MACHINE
              MOVE CDB-ALL-PATH             TO CDB-PATH.

      *-----------------------------------------------------------------
       OPEN-CDB1-FILE.
           PERFORM LOAD-CDB1-FILE.
           PERFORM OPEN-IT.
           MOVE CDB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CDB1-FILE.
           PERFORM START-IT.
           MOVE CDB-PATH-SQL TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.

           IF ( CDB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CDB1-FILE.

           IF ( QCDB1-WEND-BRANCH = ZEROES    )  OR
              ( QCDB1-WEND-TYPE = SPACES )
              MOVE CDB-TYPE             TO QCDB1-WBEG-TYPE
              MOVE ALL "Z"              TO QCDB1-WEND-TYPE
              MOVE CDB-BRANCH           TO QCDB1-WBEG-BRANCH
              MOVE ALL "9"              TO QCDB1-WEND-BRANCH
              MOVE CDB-CODE             TO QCDB1-WBEG-CODE
              MOVE ALL "Z"              TO QCDB1-WEND-CODE.

           EXEC SQL
            DECLARE CDB1_CURSOR CURSOR FOR
            SELECT CDBFILE.CDB_TYPE,
                   CDBFILE.CDB_BRANCH,
                   CDBFILE.CDB_CODE,
                   CAST(CDBFILE.CDB_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CDBFILE.CDB_DESC,
                   CDBFILE.CDB_CN_GROUP_ID,
                   CDBFILE.CDB_CN_PHONE,
                   CDBFILE.CDB_CN_PHONE_EXT,
                   CDBFILE.CDB_GC_GLFEENO,
                   CDBFILE.CDB_GC_GLFEEFINCHG,
                   CDBFILE.CDB_GC_GLFEE_INT_CHARGABLE,
                   CDBFILE.CDB_GC_FEE_GIFTNET,
                   CDBFILE.CDB_GC_FEE_PRODNO,
                   CDBFILE.CDB_GC_FEE_PRICE,
                   CDBFILE.CDB_GC_NEW_PRODNO,
                   CDBFILE.CDB_GC_FEE_MAXAMT,
                   CDBFILE.CDB_GC_ALT_GLFEENO,
                   CDBFILE.CDB_GC_COMM_GLFEENO,
                   CDBFILE.CDB_GC_COMM_PERCENT,
                   CDBFILE.CDB_GC_FEE_EARNINGS_FG,
                   CDBFILE.CDB_GC_BRANCH_COMM_AMOUNT
            FROM DBO.CDBFILE
            WHERE CDBFILE.CDB_TYPE >= :QCDB1-WBEG-TYPE
              AND CDBFILE.CDB_TYPE <= :QCDB1-WEND-TYPE
              AND CDBFILE.CDB_BRANCH >= :QCDB1-WBEG-BRANCH
              AND CDBFILE.CDB_BRANCH <= :QCDB1-WEND-BRANCH
              AND CDBFILE.CDB_CODE >= :QCDB1-WBEG-CODE
              AND CDBFILE.CDB_CODE <= :QCDB1-WEND-CODE
              ORDER BY CDBFILE.CDB_TYPE,
                       CDBFILE.CDB_BRANCH,
                       CDBFILE.CDB_CODE
           END-EXEC.

           EXEC SQL
               OPEN CDB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCDB1-WBEG-TYPE
                          QCDB1-WEND-TYPE
                          QCDB1-WBEG-CODE
                          QCDB1-WEND-CODE.

           MOVE ZEROES TO QCDB1-WBEG-BRANCH
                          QCDB1-WEND-BRANCH.

           MOVE STAT---GOOD TO CDB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CDB1-FILE.
           PERFORM READ-IT.
           MOVE CDB-PATH-SQL TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.
           INITIALIZE QCDB-REC.

           MOVE CDB-TYPE    TO QCDB-TYPE. 
           MOVE CDB-BRANCH  TO QCDB-BRANCH.
           MOVE CDB-CODE    TO QCDB-CODE.

           EXEC SQL
            SELECT CDBFILE.CDB_TYPE,
                   CDBFILE.CDB_BRANCH,
                   CDBFILE.CDB_CODE,
                   CAST(CDBFILE.CDB_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CDBFILE.CDB_DESC,
                   CDBFILE.CDB_CN_GROUP_ID,
                   CDBFILE.CDB_CN_PHONE,
                   CDBFILE.CDB_CN_PHONE_EXT,
                   CDBFILE.CDB_GC_GLFEENO,
                   CDBFILE.CDB_GC_GLFEEFINCHG,
                   CDBFILE.CDB_GC_GLFEE_INT_CHARGABLE,
                   CDBFILE.CDB_GC_FEE_GIFTNET,
                   CDBFILE.CDB_GC_FEE_PRODNO,
                   CDBFILE.CDB_GC_FEE_PRICE,
                   CDBFILE.CDB_GC_NEW_PRODNO,
                   CDBFILE.CDB_GC_FEE_MAXAMT,
                   CDBFILE.CDB_GC_ALT_GLFEENO,
                   CDBFILE.CDB_GC_COMM_GLFEENO,
                   CDBFILE.CDB_GC_COMM_PERCENT,
                   CDBFILE.CDB_GC_FEE_EARNINGS_FG,
                   CDBFILE.CDB_GC_BRANCH_COMM_AMOUNT
            INTO :QCDB-REC
            FROM DBO.CDBFILE
            WHERE CDBFILE.CDB_TYPE = :QCDB-TYPE
              AND CDBFILE.CDB_BRANCH = :QCDB-BRANCH
              AND CDBFILE.CDB_CODE = :QCDB-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CDB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CDB-FIELDS.

      *-----------------------------------------------------------------
       READ-CDB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CDB-PATH-SQL TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.
           INITIALIZE QCDB-REC.

           IF ( CDB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CDB1_CURSOR INTO :QCDB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CDB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CDB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CDB1-FILE.
           PERFORM CLOSE-IT.
           IF ( CDB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CDB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CDB1-CURSOR-STAT.

