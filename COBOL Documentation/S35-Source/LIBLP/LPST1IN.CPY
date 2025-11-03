      * COPYMEMBER: LIBLP/LPST1IN
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
       WRITE-ST1-FILE.

           ACCEPT ST-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE ST-PATH-SQL TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           PERFORM SET-ST-FIELDS.

           EXEC SQL
            INSERT INTO DBO.STFILE
             (STFILE.ST_CODE,
              STFILE.ST_LTOUCH_DATE,
              STFILE.ST_DESC,
              STFILE.ST_REINSTATE_WINDOW,
              STFILE.ST_REPO_RENOTICE,
              STFILE.ST_TITLE_RELEASE_TYPE,
              STFILE.ST_AUCTION_DATE_REQ,
              STFILE.ST_GEOGRAPHIC_CODE,
              STFILE.ST_MAX_LOAN_AVAIL,
              STFILE.ST_MAX_TAX_LOAN_AMT,
              STFILE.ST_NSF_FEE_AMT)
            VALUES
             (:QST-CODE,
              :QST-LTOUCH-DATE,
              :QST-DESC,
              :QST-REINSTATE-WINDOW,
              :QST-REPO-RENOTICE,
              :QST-TITLE-RELEASE-TYPE,
              :QST-AUCTION-DATE-REQ,
              :QST-GEOGRAPHIC-CODE,
              :QST-MAX-LOAN-AVAIL,
              :QST-MAX-TAX-LOAN-AMT,
              :QST-NSF-FEE-AMT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-ST1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-ST1-FILE.

           ACCEPT ST-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE ST-PATH-SQL TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           PERFORM SET-ST-FIELDS.

           EXEC SQL
            UPDATE DBO.STFILE
             SET
              STFILE.ST_CODE = :QST-CODE,
              STFILE.ST_LTOUCH_DATE =
                 :QST-LTOUCH-DATE,
              STFILE.ST_DESC = :QST-DESC,
              STFILE.ST_REINSTATE_WINDOW = :QST-REINSTATE-WINDOW,
              STFILE.ST_REPO_RENOTICE = :QST-REPO-RENOTICE,
              STFILE.ST_TITLE_RELEASE_TYPE =
                   :QST-TITLE-RELEASE-TYPE,
              STFILE.ST_AUCTION_DATE_REQ = :QST-AUCTION-DATE-REQ,
              STFILE.ST_GEOGRAPHIC_CODE = :QST-GEOGRAPHIC-CODE,
              STFILE.ST_MAX_LOAN_AVAIL = :QST-MAX-LOAN-AVAIL,
              STFILE.ST_MAX_TAX_LOAN_AMT = :QST-MAX-TAX-LOAN-AMT,
              STFILE.ST_NSF_FEE_AMT = :QST-NSF-FEE-AMT
            WHERE STFILE.ST_CODE = :QST-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-ST1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-ST1-FILE.

           PERFORM DELETE-IT.
           MOVE ST-PATH-SQL TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           PERFORM SET-ST-FIELDS.

           EXEC SQL
            DELETE FROM DBO.STFILE
            WHERE STFILE.ST_CODE = :QST-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-ST1-FILE.

      *-----------------------------------------------------------------
       CLOSE-ST1-FILE.

           PERFORM CLOSE-IT.

           IF ( ST1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE ST1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO ST1-CURSOR-STAT.

