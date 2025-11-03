      * COPYMEMBER: LIBLP/LPST1RN
       LOAD-ST1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ST-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ST-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ST-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ST-ALL-MACHINE
              MOVE ST-ALL-PATH             TO ST-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ST-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ST-ALL-BASE
              MOVE FILPATH-MACHINE         TO ST-ALL-MACHINE
              MOVE ST-ALL-PATH             TO ST-PATH.

      *-----------------------------------------------------------------
       OPEN-ST1-FILE.
           PERFORM LOAD-ST1-FILE.
           PERFORM OPEN-IT.
           MOVE ST-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-ST1-FILE.
           PERFORM START-IT.
           MOVE ST-PATH-SQL TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.

           IF ( ST1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-ST1-FILE.

           IF ( QST1-WEND-CODE = SPACES )
              MOVE ST-CODE        TO QST1-WBEG-CODE
              MOVE ALL "Z"        TO QST1-WEND-CODE.

           EXEC SQL
            DECLARE ST1_CURSOR CURSOR FOR
            SELECT STFILE.ST_CODE,
                   CAST(STFILE.ST_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   STFILE.ST_DESC,
                   STFILE.ST_REINSTATE_WINDOW,
                   STFILE.ST_REPO_RENOTICE,
                   STFILE.ST_TITLE_RELEASE_TYPE,
                   STFILE.ST_AUCTION_DATE_REQ,
                   STFILE.ST_GEOGRAPHIC_CODE,
                   STFILE.ST_MAX_LOAN_AVAIL,
                   STFILE.ST_MAX_TAX_LOAN_AMT,
                   STFILE.ST_NSF_FEE_AMT
            FROM DBO.STFILE
            WHERE STFILE.ST_CODE >= :QST1-WBEG-CODE
              AND STFILE.ST_CODE <= :QST1-WEND-CODE
            ORDER BY STFILE.ST_CODE
           END-EXEC.

           EXEC SQL
               OPEN ST1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QST1-WBEG-CODE
                          QST1-WEND-CODE.

           MOVE STAT---GOOD TO ST1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-ST1-FILE.
           PERFORM READ-IT.
           MOVE ST-PATH-SQL TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           INITIALIZE QST-REC.      
 
           MOVE ST-CODE   TO QST-CODE.
       
           EXEC SQL
            SELECT STFILE.ST_CODE,
                   CAST(STFILE.ST_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   STFILE.ST_DESC,
                   STFILE.ST_REINSTATE_WINDOW,
                   STFILE.ST_REPO_RENOTICE,
                   STFILE.ST_TITLE_RELEASE_TYPE,
                   STFILE.ST_AUCTION_DATE_REQ,
                   STFILE.ST_GEOGRAPHIC_CODE,
                   STFILE.ST_MAX_LOAN_AVAIL,
                   STFILE.ST_MAX_TAX_LOAN_AMT,
                   STFILE.ST_NSF_FEE_AMT
            INTO :QST-REC
            FROM DBO.STFILE
            WHERE STFILE.ST_CODE = :QST-CODE
           END-EXEC.
       
           PERFORM SQL-IO-VALIDATION.
       
           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ST1-FILE.
       
           IF ( IO-FG = 0 )
              PERFORM GET-ST-FIELDS.

      *-----------------------------------------------------------------
       READ-ST1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ST-PATH-SQL TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           INITIALIZE QST-REC.

           IF ( ST1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH ST1_CURSOR INTO :QST-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ST1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-ST-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-ST1-FILE.
           PERFORM CLOSE-IT.
           IF ( ST1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE ST1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO ST1-CURSOR-STAT.

