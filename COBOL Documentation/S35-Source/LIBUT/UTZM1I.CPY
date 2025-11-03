      * COPYMEMBER: LIBUT/UTZM1I
       LOAD-ZMB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ZM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
               IF ( ZM-PATH-OVERRIDE = SPACES ) 
                  MOVE EXT-FILPATH-BASE        TO ZM-ALL-BASE
                  MOVE EXT-FILPATH-MACHINE     TO ZM-ALL-MACHINE
                  MOVE ZM-ALL-PATH             TO ZM-PATH
               ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
                   IF ( ZM-PATH-OVERRIDE = "B" ) 
                      MOVE FILPATH-BASE            TO ZM-ALL-BASE
                      MOVE FILPATH-MACHINE         TO ZM-ALL-MACHINE
                      MOVE ZM-ALL-PATH             TO ZM-PATH.

      *-----------------------------------------------------------------
       OPEN-ZMB1-FILE.
           PERFORM LOAD-ZMB1-FILE.
           PERFORM OPEN-IT.
           MOVE ZM-PATH-SQL TO E-FILE.

      *-----------------------------------------------------------------
       START-ZMB1-FILE.
           PERFORM START-IT.
           MOVE ZM-PATH-SQL TO E-FILE.
           MOVE ZMB1-KEY TO E-KEYX.

           IF ( ZMB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-ZMB1-FILE.

           IF ( QZMB1-WEND-CODE = SPACES )
              MOVE ZMB-CODE       TO QZMB1-WBEG-CODE
              MOVE ALL "Z"        TO QZMB1-WEND-CODE.

           EXEC SQL
            DECLARE ZMB1_CURSOR CURSOR FOR
            SELECT ZMFILE.ZMB_CODE,
                   ZMFILE.ZMB_DESC, 
                   ZMFILE.ZMB_REINSTATE_WINDOW, 
                   ZMFILE.ZMB_REPO_RENOTICE, 
                   ZMFILE.ZMB_TITLE_RELEASE_TYPE, 
                   ZMFILE.ZMB_AUCTION_DATE_REQ, 
                   ZMFILE.ZMB_GEOGRAPHIC_CODE,
                   ZMFILE.ZMB_MAX_LOAN_AVAIL,
                   ZMFILE.ZMB_MAX_TAX_LOAN_AMT,
                   ZMFILE.ZMB_NSF_FEE_AMT,
                   ZMFILE.ZMB_TEST_AMOUNT,
                   ZMFILE.ZMB_TEST_CODE,
                   ZMFILE.ZMB_TEST_DATE,
                   ZMFILE.ZMB_TEST_NUM,
                   ZMFILE.ZMB_TEST_RATE,
                   ZMFILE.ZMB_TEST_SSNO
            FROM DBO.ZMFILE
            WHERE ZMFILE.ZM_CODE >= :QZMB1-WBEG-CODE
              AND ZMFILE.ZM_CODE <= :QZMB1-WEND-CODE
            ORDER BY ZMFILE.ZM-CODE
           END-EXEC.

           EXEC SQL
               OPEN ZMB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QZMB1-WBEG-CODE
                          QZMB1-WEND-CODE.

           MOVE STAT---GOOD TO ZMB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-ZMB1-FILE.
           PERFORM READ-IT.
           MOVE ZM-PATH-SQL TO E-FILE.
           MOVE ZMB1-KEY TO E-KEYX.

           MOVE ZMB-CODE   TO QZMB-CODE.

           EXEC SQL
            SELECT ZMFILE.ZMB_CODE,
                   ZMFILE.ZMB_DESC, 
                   ZMFILE.ZMB_REINSTATE_WINDOW, 
                   ZMFILE.ZMB_REPO_RENOTICE, 
                   ZMFILE.ZMB_TITLE_RELEASE_TYPE, 
                   ZMFILE.ZMB_AUCTION_DATE_REQ, 
                   ZMFILE.ZMB_GEOGRAPHIC_CODE,
                   ZMFILE.ZMB_MAX_LOAN_AVAIL,
                   ZMFILE.ZMB_MAX_TAX_LOAN_AMT,
                   ZMFILE.ZMB_NSF_FEE_AMT,
                   ZMFILE.ZMB_TEST_AMOUNT,
                   ZMFILE.ZMB_TEST_CODE,
                   ZMFILE.ZMB_TEST_DATE,
                   ZMFILE.ZMB_TEST_NUM,
                   ZMFILE.ZMB_TEST_RATE,
                   ZMFILE.ZMB_TEST_SSNO
            INTO :QZMB-REC
            FROM DBO.ZMFILE
            WHERE ZMFILE.ZMB_CODE = :QZMB-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ZMB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-ZMB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-ZMB1-FILE.
           PERFORM WRITE-IT.
           MOVE ZM-PATH-SQL TO E-FILE.
           MOVE ZMB1-KEY TO E-KEYX.
           PERFORM SET-ZMB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.ZMFILE
             (ZMFILE.ZMB_CODE,
              ZMFILE.ZMB_DESC,
              ZMFILE.ZMB_REINSTATE_WINDOW,
              ZMFILE.ZMB_REPO_RENOTICE,
              ZMFILE.ZMB_TITLE_RELEASE_TYPE,
              ZMFILE.ZMB_AUCTION_DATE_REQ,
              ZMFILE.ZMB_GEOGRAPHIC_CODE,
              ZMFILE.ZMB_MAX_LOAN_AVAIL,
              ZMFILE.ZMB_MAX_TAX_LOAN_AMT,
              ZMFILE.ZMB_NSF_FEE_AMT,
              ZMFILE.ZMB_TEST_CODE,
              ZMFILE.ZMB_TEST_DATE,
              ZMFILE.ZMB_TEST_NUM,
              ZMFILE.ZMB_TEST_RATE,
              ZMFILE.ZMB_TEST_SSNO)
            VALUES
             (:QZMB-CODE,
              :QZMB-DESC,
              :QZMB-REINSTATE-WINDOW,
              :QZMB-REPO-RENOTICE,
              :QZMB-TITLE-RELEASE-TYPE,
              :QZMB-AUCTION-DATE-REQ,
              :QZMB-GEOGRAPHIC-CODE,
              :QZMB-MAX-LOAN-AVAIL,
              :QZMB-MAX-TAX-LOAN-AMT,
              :QZMB-NSF-FEE-AMT,
              :QZMB-TEST-CODE,
              :QZMB-TEST-DATE,
              :QZMB-TEST-NUM,
              :QZMB-TEST-RATE,
              :QZMB-TEST-SSNO)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-ZMB1-FILE.

      *-----------------------------------------------------------------
       REWRITE-ZMB1-FILE.
           PERFORM REWRITE-IT.
           MOVE ZM-PATH-SQL TO E-FILE.
           MOVE ZMB1-KEY TO E-KEYX.
           PERFORM SET-ZMB-FIELDS.

           EXEC SQL
            UPDATE DBO.ZMFILE
             SET
              ZMFILE.ZMB_CODE             = :QZMB-CODE,
              ZMFILE.ZMB_DESC             = :QZMB-DESC,
              ZMFILE.ZMB_REINSTATE_WINDOW = :QZMB-REINSTATE-WINDOW,
              ZMFILE.ZMB_REPO_RENOTICE    = :QZMB-REPO-RENOTICE,
              ZMFILE.ZMB_TITLE_RELEASE_TYPE =
                                           :QZMB-TITLE-RELEASE-TYPE,
              ZMFILE.ZMB_AUCTION_DATE_REQ = :QZMB-AUCTION-DATE-REQ,
              ZMFILE.ZMB_GEOGRAPHIC_CODE  = :QZMB-GEOGRAPHIC-CODE,
              ZMFILE.ZMB_MAX_LOAN_AVAIL   = :QZMB-MAX-LOAN-AVAIL,
              ZMFILE.ZMB_MAX_TAX_LOAN_AMT = :QZMB-MAX-TAX-LOAN-AMT,
              ZMFILE.ZMB_NSF_FEE_AMT      = :QZMB-NSF-FEE-AMT,
              ZMFILE.ZMB_TEST_CODE        = :QZMB-TEST-CODE,
              ZMFILE.ZMB_TEST_DATE        = :QZMB-TEST-DATE,
              ZMFILE.ZMB_TEST_NUM         = :QZMB-TEST-NUM,
              ZMFILE.ZMB_TEST_RATE        = :QZMB-TEST-RATE,
              ZMFILE.ZMB_TEST_SSNO        = :QZMB-TEST-SSNO
            WHERE ZMFILE.ZMB_CODE         = :QZMB-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-ZMB1-FILE.

      *-----------------------------------------------------------------
       DELETE-ZMB1-FILE.
           PERFORM DELETE-IT.
           MOVE ZM-PATH-SQL TO E-FILE.
           MOVE ZMB1-KEY TO E-KEYX.
           PERFORM SET-ZMB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.ZMFILE
            WHERE ZMFILE.ZMB_CODE = :QZMB-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-ZMB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-ZMB1-FILE.
           PERFORM CLOSE-IT.

           IF ( ZMB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE ZMB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO ZMB1-CURSOR-STAT.
