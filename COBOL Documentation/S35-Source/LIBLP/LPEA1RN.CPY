      * COPYMEMBER: LIBLP/LPEA1RN
       LOAD-EA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( EA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( EA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO EA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO EA-ALL-MACHINE
              MOVE EA-ALL-PATH             TO EA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( EA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO EA-ALL-BASE
              MOVE FILPATH-MACHINE         TO EA-ALL-MACHINE
              MOVE EA-ALL-PATH             TO EA-PATH.

      *-----------------------------------------------------------------
       OPEN-EA1-FILE.
           PERFORM LOAD-EA1-FILE.
           PERFORM OPEN-IT.
           MOVE EA-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-EA1-FILE.
           PERFORM START-IT.
           MOVE EA-PATH-SQL TO E-FILE.
           MOVE EA1-KEY TO E-KEYX.

           IF ( EA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-EA1-FILE.

           IF ( QEA1-WEND-SEQNO = ZEROES    ) OR
              ( QEA1-WEND-SEQNO NOT NUMERIC )
              MOVE EA-BRANCH      TO QEA1-WBEG-BRANCH
              MOVE ALL "9"        TO QEA1-WEND-BRANCH
              MOVE EA-CLASS       TO QEA1-WBEG-CLASS
              MOVE ALL "9"        TO QEA1-WEND-CLASS
              MOVE EA-GROUP-ID    TO QEA1-WBEG-GROUP-ID
              MOVE ALL "Z"        TO QEA1-WEND-GROUP-ID
              MOVE EA-SEQNO       TO QEA1-WBEG-SEQNO
              MOVE ALL "9"        TO QEA1-WEND-SEQNO.

           EXEC SQL
            DECLARE EA1_CURSOR CURSOR FOR
            SELECT EAFILE.EA_BRANCH,
                   EAFILE.EA_CLASS,
                   EAFILE.EA_GROUP_ID,
                   EAFILE.EA_SEQNO,
                   CAST(EAFILE.EA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   EAFILE.EA_DESC,
                   EAFILE.EA_PASTDUE_DAY1,
                   EAFILE.EA_PASTDUE_DAY2,
                   EAFILE.EA_PASTDUE_AMT1,
                   EAFILE.EA_PASTDUE_AMT2,
                   EAFILE.EA_ACCT_BAL1,
                   EAFILE.EA_ACCT_BAL2,
                   EAFILE.EA_MATURED,
                   EAFILE.EA_PROMISED,
                   EAFILE.EA_ACTION,
                   CAST(EAFILE.EA_LSTFM_DATE1
                                                  AS VARCHAR(10)),
                   CAST(EAFILE.EA_LSTFM_DATE2
                                                  AS VARCHAR(10)),
                   EAFILE.EA_LSTFM_NO1,
                   EAFILE.EA_LSTFM_NO2,
                   EAFILE.EA_PASTPROM_DAY1,
                   EAFILE.EA_PASTPROM_DAY2,
                   EAFILE.EA_RECENCY_DAY1,
                   EAFILE.EA_RECENCY_DAY2,
                   EAFILE.EA_ALPNAME1,
                   EAFILE.EA_ALPNAME2,
                   EAFILE.EA_RECENCYDATE,
                   EAFILE.EA_DLNO1,
                   EAFILE.EA_DLNO2,
                   EAFILE.EA_CONTRACTUAL_DAY1,
                   EAFILE.EA_CONTRACTUAL_DAY2,
                   EAFILE.EA_SETFM_NO,
                   EAFILE.EA_SETACT_CD,
                   EAFILE.EA_INC_CNT,
                   EAFILE.EA_INC_PROMISED,
                   EAFILE.EA_SETFM_1,
                   EAFILE.EA_SETFM_2,
                   EAFILE.EA_SETFM_3,
                   EAFILE.EA_SET_LN_COL,
                   EAFILE.EA_LN_COL_ID,
                   EAFILE.EA_CQ_FG,
                   EAFILE.EA_COL_POOL,
                   EAFILE.EA_CPOTS,
                   EAFILE.EA_REPO,
                   EAFILE.EA_PL,
                   EAFILE.EA_BANKRUPT,
                   EAFILE.EA_PL_IN_ACTIVE_BOTH,
                   EAFILE.EA_CONFIDENTIAL_FG,
                   EAFILE.EA_1ST_PMT_MISSES,
                   EAFILE.EA_LEGAL,
                   EAFILE.EA_DEBTAID,
                   EAFILE.EA_JUDGEMENT,
                   EAFILE.EA_BROKEN_PROM_FG,
                   EAFILE.EA_PROM_BREAK1,
                   EAFILE.EA_PROM_BREAK2,
                   EAFILE.EA_ORIGIN,
                   CAST(EAFILE.EA_PL_BEG_DATE
                                                  AS VARCHAR(10)),
                   CAST(EAFILE.EA_PL_END_DATE
                                                  AS VARCHAR(10)),
                   EAFILE.EA_RPOTS,
                   EAFILE.EA_MISPAYS,
                   EAFILE.EA_ALLOTMENTS,
                   EAFILE.EA_LNDATE_DAYS1,
                   EAFILE.EA_LNDATE_DAYS2,
                   EAFILE.EA_MATURE_DAYS1,
                   EAFILE.EA_MATURE_DAYS2,
                   EAFILE.EA_DAY_OF_MO_DUE1,
                   EAFILE.EA_DAY_OF_MO_DUE2,
                   EAFILE.EA_INCL_COLLECTOR,
                   EAFILE.EA_BORR_STATE
            FROM DBO.EAFILE
            WHERE EAFILE.EA_BRANCH >= :QEA1-WBEG-BRANCH
              AND EAFILE.EA_BRANCH <= :QEA1-WEND-BRANCH
              AND EAFILE.EA_CLASS >= :QEA1-WBEG-CLASS
              AND EAFILE.EA_CLASS <= :QEA1-WEND-CLASS
              AND EAFILE.EA_GROUP_ID >= :QEA1-WBEG-GROUP-ID
              AND EAFILE.EA_GROUP_ID <= :QEA1-WEND-GROUP-ID
              AND EAFILE.EA_SEQNO >= :QEA1-WBEG-SEQNO
              AND EAFILE.EA_SEQNO <= :QEA1-WEND-SEQNO
            ORDER BY EAFILE.EA_BRANCH,
                     EAFILE.EA_CLASS,
                     EAFILE.EA_GROUP_ID,
                     EAFILE.EA_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN EA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QEA1-WBEG-GROUP-ID
                          QEA1-WEND-GROUP-ID.

           MOVE ZEROES TO QEA1-WBEG-BRANCH
                          QEA1-WEND-BRANCH
                          QEA1-WEND-CLASS
                          QEA1-WEND-CLASS
                          QEA1-WEND-SEQNO
                          QEA1-WEND-SEQNO.

           MOVE STAT---GOOD TO EA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-EA1-FILE.
           PERFORM READ-IT.
           MOVE EA-PATH-SQL TO E-FILE.
           MOVE EA1-KEY TO E-KEYX.
           INITIALIZE QEA-REC.

           MOVE EA-BRANCH     TO QEA-BRANCH.
           MOVE EA-CLASS      TO QEA-CLASS.
           MOVE EA-GROUP-ID   TO QEA-GROUP-ID.
           MOVE EA-SEQNO      TO QEA-SEQNO.

           EXEC SQL
            SELECT EAFILE.EA_BRANCH,
                   EAFILE.EA_CLASS,
                   EAFILE.EA_GROUP_ID,
                   EAFILE.EA_SEQNO,
                   CAST(EAFILE.EA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   EAFILE.EA_DESC,
                   EAFILE.EA_PASTDUE_DAY1,
                   EAFILE.EA_PASTDUE_DAY2,
                   EAFILE.EA_PASTDUE_AMT1,
                   EAFILE.EA_PASTDUE_AMT2,
                   EAFILE.EA_ACCT_BAL1,
                   EAFILE.EA_ACCT_BAL2,
                   EAFILE.EA_MATURED,
                   EAFILE.EA_PROMISED,
                   EAFILE.EA_ACTION,
                   CAST(EAFILE.EA_LSTFM_DATE1
                                                  AS VARCHAR(10)),
                   CAST(EAFILE.EA_LSTFM_DATE2
                                                  AS VARCHAR(10)),
                   EAFILE.EA_LSTFM_NO1,
                   EAFILE.EA_LSTFM_NO2,
                   EAFILE.EA_PASTPROM_DAY1,
                   EAFILE.EA_PASTPROM_DAY2,
                   EAFILE.EA_RECENCY_DAY1,
                   EAFILE.EA_RECENCY_DAY2,
                   EAFILE.EA_ALPNAME1,
                   EAFILE.EA_ALPNAME2,
                   EAFILE.EA_RECENCYDATE,
                   EAFILE.EA_DLNO1,
                   EAFILE.EA_DLNO2,
                   EAFILE.EA_CONTRACTUAL_DAY1,
                   EAFILE.EA_CONTRACTUAL_DAY2,
                   EAFILE.EA_SETFM_NO,
                   EAFILE.EA_SETACT_CD,
                   EAFILE.EA_INC_CNT,
                   EAFILE.EA_INC_PROMISED,
                   EAFILE.EA_SETFM_1,
                   EAFILE.EA_SETFM_2,
                   EAFILE.EA_SETFM_3,
                   EAFILE.EA_SET_LN_COL,
                   EAFILE.EA_LN_COL_ID,
                   EAFILE.EA_CQ_FG,
                   EAFILE.EA_COL_POOL,
                   EAFILE.EA_CPOTS,
                   EAFILE.EA_REPO,
                   EAFILE.EA_PL,
                   EAFILE.EA_BANKRUPT,
                   EAFILE.EA_PL_IN_ACTIVE_BOTH,
                   EAFILE.EA_CONFIDENTIAL_FG,
                   EAFILE.EA_1ST_PMT_MISSES,
                   EAFILE.EA_LEGAL,
                   EAFILE.EA_DEBTAID,
                   EAFILE.EA_JUDGEMENT,
                   EAFILE.EA_BROKEN_PROM_FG,
                   EAFILE.EA_PROM_BREAK1,
                   EAFILE.EA_PROM_BREAK2,
                   EAFILE.EA_ORIGIN,
                   CAST(EAFILE.EA_PL_BEG_DATE
                                                  AS VARCHAR(10)),
                   CAST(EAFILE.EA_PL_END_DATE
                                                  AS VARCHAR(10)),
                   EAFILE.EA_RPOTS,
                   EAFILE.EA_MISPAYS,
                   EAFILE.EA_ALLOTMENTS,
                   EAFILE.EA_LNDATE_DAYS1,
                   EAFILE.EA_LNDATE_DAYS2,
                   EAFILE.EA_MATURE_DAYS1,
                   EAFILE.EA_MATURE_DAYS2,
                   EAFILE.EA_DAY_OF_MO_DUE1,
                   EAFILE.EA_DAY_OF_MO_DUE2,
                   EAFILE.EA_INCL_COLLECTOR,
                   EAFILE.EA_BORR_STATE
            INTO :QEA-REC
            FROM DBO.EAFILE
            WHERE EAFILE.EA_BRANCH = :QEA-BRANCH
              AND EAFILE.EA_CLASS = :QEA-CLASS
              AND EAFILE.EA_GROUP_ID = :QEA-GROUP-ID
              AND EAFILE.EA_SEQNO = :QEA-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-EA-FIELDS.

      *-----------------------------------------------------------------
       READ-EA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE EA-PATH-SQL TO E-FILE.
           MOVE EA1-KEY TO E-KEYX.
           INITIALIZE QEA-REC.

           IF ( EA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH EA1_CURSOR INTO :QEA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-EA-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-EA1-FILE.
           PERFORM CLOSE-IT.
           IF ( EA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE EA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO EA1-CURSOR-STAT.

