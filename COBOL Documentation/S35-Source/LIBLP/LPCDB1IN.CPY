      * COPYMEMBER: LIBLP/LPCDB1IN
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
       START-CDB1-FILE-GREATER.
           PERFORM START-IT.
           MOVE CDB-PATH-SQL TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.

           IF ( CDB1-CURSOR-GR-STAT-GOOD )
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
            DECLARE CDB1_CURSOR_GR CURSOR FOR
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
            WHERE CDBFILE.CDB_TYPE > :QCDB1-WBEG-TYPE
              AND CDBFILE.CDB_TYPE <= :QCDB1-WEND-TYPE
              AND CDBFILE.CDB_BRANCH > :QCDB1-WBEG-BRANCH
              AND CDBFILE.CDB_BRANCH <= :QCDB1-WEND-BRANCH
              AND CDBFILE.CDB_CODE > :QCDB1-WBEG-CODE
              AND CDBFILE.CDB_CODE <= :QCDB1-WEND-CODE
              ORDER BY CDBFILE.CDB_TYPE,
                       CDBFILE.CDB_BRANCH,
                       CDBFILE.CDB_CODE
           END-EXEC.

           EXEC SQL
               OPEN CDB1_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCDB1-WBEG-TYPE
                          QCDB1-WEND-TYPE
                          QCDB1-WBEG-CODE
                          QCDB1-WEND-CODE.

           MOVE ZEROES TO QCDB1-WBEG-BRANCH
                          QCDB1-WEND-BRANCH.

           MOVE STAT---GOOD TO CDB1-CURSOR-GR-STAT.

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
       WRITE-CDB1-FILE.

           ACCEPT CDB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE CDB-PATH-SQL TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.
           PERFORM SET-CDB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.CDBFILE
             (CDBFILE.CDB_TYPE,
              CDBFILE.CDB_BRANCH,
              CDBFILE.CDB_CODE,
              CDBFILE.CDB_LTOUCH_DATE,
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
              CDBFILE.CDB_GC_BRANCH_COMM_AMOUNT)
            VALUES
             (:QCDB-TYPE,
              :QCDB-BRANCH,
              :QCDB-CODE,
              :QCDB-LTOUCH-DATE,
              :QCDB-DESC,
              :QCDB-CN-GROUP-ID,
              :QCDB-CN-PHONE,
              :QCDB-CN-PHONE-EXT,
              :QCDB-GC-GLFEENO,
              :QCDB-GC-GLFEEFINCHG,
              :QCDB-GC-GLFEE-INT-CHARGABLE,
              :QCDB-GC-FEE-GIFTNET,
              :QCDB-GC-FEE-PRODNO,
              :QCDB-GC-FEE-PRICE,
              :QCDB-GC-NEW-PRODNO,
              :QCDB-GC-FEE-MAXAMT,
              :QCDB-GC-ALT-GLFEENO,
              :QCDB-GC-COMM-GLFEENO,
              :QCDB-GC-COMM-PERCENT,
              :QCDB-GC-FEE-EARNINGS-FG,
              :QCDB-GC-BRANCH-COMM-AMOUNT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-CDB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-CDB1-FILE.

           ACCEPT CDB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE CDB-PATH-SQL TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.
           PERFORM SET-CDB-FIELDS.

           EXEC SQL
            UPDATE DBO.CDBFILE
             SET
              CDBFILE.CDB_TYPE = :QCDB-TYPE,
              CDBFILE.CDB_BRANCH = :QCDB-BRANCH,
              CDBFILE.CDB_CODE = :QCDB-CODE,
              CDBFILE.CDB_LTOUCH_DATE = :QCDB-LTOUCH-DATE,
              CDBFILE.CDB_DESC = :QCDB-DESC,
              CDBFILE.CDB_CN_GROUP_ID = :QCDB-CN-GROUP-ID,
              CDBFILE.CDB_CN_PHONE = :QCDB-CN-PHONE,
              CDBFILE.CDB_CN_PHONE_EXT = :QCDB-CN-PHONE-EXT,
              CDBFILE.CDB_GC_GLFEENO = :QCDB-GC-GLFEENO,
              CDBFILE.CDB_GC_GLFEEFINCHG = :QCDB-GC-GLFEEFINCHG,
              CDBFILE.CDB_GC_GLFEE_INT_CHARGABLE =
                   :QCDB-GC-GLFEE-INT-CHARGABLE,
              CDBFILE.CDB_GC_FEE_GIFTNET = :QCDB-GC-FEE-GIFTNET,
              CDBFILE.CDB_GC_FEE_PRODNO = :QCDB-GC-FEE-PRODNO,
              CDBFILE.CDB_GC_FEE_PRICE = :QCDB-GC-FEE-PRICE,
              CDBFILE.CDB_GC_NEW_PRODNO = :QCDB-GC-NEW-PRODNO,
              CDBFILE.CDB_GC_FEE_MAXAMT = :QCDB-GC-FEE-MAXAMT,
              CDBFILE.CDB_GC_ALT_GLFEENO = :QCDB-GC-ALT-GLFEENO,
              CDBFILE.CDB_GC_COMM_GLFEENO = :QCDB-GC-COMM-GLFEENO,
              CDBFILE.CDB_GC_COMM_PERCENT = :QCDB-GC-COMM-PERCENT,
              CDBFILE.CDB_GC_FEE_EARNINGS_FG =
                   :QCDB-GC-FEE-EARNINGS-FG,
              CDBFILE.CDB_GC_BRANCH_COMM_AMOUNT =
                   :QCDB-GC-BRANCH-COMM-AMOUNT
             WHERE CDBFILE.CDB_TYPE = :QCDB-TYPE
               AND CDBFILE.CDB_BRANCH = :QCDB-BRANCH
               AND CDBFILE.CDB_CODE = :QCDB-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CDB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-CDB1-FILE.

           PERFORM DELETE-IT.
           MOVE CDB-PATH-SQL TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.
           PERFORM SET-CDB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.CDBFILE
             WHERE CDBFILE.CDB_TYPE = :QCDB-TYPE
               AND CDBFILE.CDB_BRANCH = :QCDB-BRANCH
               AND CDBFILE.CDB_CODE = :QCDB-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-CDB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-CDB1-FILE.

           PERFORM CLOSE-IT.

           IF ( CDB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CDB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CDB1-CURSOR-STAT.

