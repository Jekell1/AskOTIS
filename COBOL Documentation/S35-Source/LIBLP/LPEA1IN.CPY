      * COPYMEMBER: LIBLP/LPEA1IN
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
           MOVE EA-PATH-SQL    TO E-FILE.
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
       WRITE-EA1-FILE.

           ACCEPT EA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE EA-PATH-SQL TO E-FILE.
           MOVE EA1-KEY TO E-KEYX.
           PERFORM SET-EA-FIELDS.

           EXEC SQL
            INSERT INTO DBO.EAFILE
             (EAFILE.EA_BRANCH,
              EAFILE.EA_CLASS,
              EAFILE.EA_GROUP_ID,
              EAFILE.EA_SEQNO,
              EAFILE.EA_LTOUCH_DATE,
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
              EAFILE.EA_LSTFM_DATE1,
              EAFILE.EA_LSTFM_DATE2,
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
              EAFILE.EA_PL_BEG_DATE,
              EAFILE.EA_PL_END_DATE,
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
              EAFILE.EA_BORR_STATE)
            VALUES
             (:QEA-BRANCH,
              :QEA-CLASS,
              :QEA-GROUP-ID,
              :QEA-SEQNO,
              :QEA-LTOUCH-DATE,
              :QEA-DESC,
              :QEA-PASTDUE-DAY1,
              :QEA-PASTDUE-DAY2,
              :QEA-PASTDUE-AMT1,
              :QEA-PASTDUE-AMT2,
              :QEA-ACCT-BAL1,
              :QEA-ACCT-BAL2,
              :QEA-MATURED,
              :QEA-PROMISED,
              :QEA-ACTION,
              :QEA-LSTFM-DATE1,
              :QEA-LSTFM-DATE2,
              :QEA-LSTFM-NO1,
              :QEA-LSTFM-NO2,
              :QEA-PASTPROM-DAY1,
              :QEA-PASTPROM-DAY2,
              :QEA-RECENCY-DAY1,
              :QEA-RECENCY-DAY2,
              :QEA-ALPNAME1,
              :QEA-ALPNAME2,
              :QEA-RECENCYDATE,
              :QEA-DLNO1,
              :QEA-DLNO2,
              :QEA-CONTRACTUAL-DAY1,
              :QEA-CONTRACTUAL-DAY2,
              :QEA-SETFM-NO,
              :QEA-SETACT-CD,
              :QEA-INC-CNT,
              :QEA-INC-PROMISED,
              :QEA-SETFM-1,
              :QEA-SETFM-2,
              :QEA-SETFM-3,
              :QEA-SET-LN-COL,
              :QEA-LN-COL-ID,
              :QEA-CQ-FG,
              :QEA-COL-POOL,
              :QEA-CPOTS,
              :QEA-REPO,
              :QEA-PL,
              :QEA-BANKRUPT,
              :QEA-PL-IN-ACTIVE-BOTH,
              :QEA-CONFIDENTIAL-FG,
              :QEA-1ST-PMT-MISSES,
              :QEA-LEGAL,
              :QEA-DEBTAID,
              :QEA-JUDGEMENT,
              :QEA-BROKEN-PROM-FG,
              :QEA-PROM-BREAK1,
              :QEA-PROM-BREAK2,
              :QEA-ORIGIN,
              :QEA-PL-BEG-DATE,
              :QEA-PL-END-DATE,
              :QEA-RPOTS,
              :QEA-MISPAYS,
              :QEA-ALLOTMENTS,
              :QEA-LNDATE-DAYS1,
              :QEA-LNDATE-DAYS2,
              :QEA-MATURE-DAYS1,
              :QEA-MATURE-DAYS2,
              :QEA-DAY-OF-MO-DUE1,
              :QEA-DAY-OF-MO-DUE2,
              :QEA-INCL-COLLECTOR,
              :QEA-BORR-STATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-EA1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-EA1-FILE.

           ACCEPT EA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE EA-PATH-SQL TO E-FILE.
           MOVE EA1-KEY TO E-KEYX.
           PERFORM SET-EA-FIELDS.

           EXEC SQL
            UPDATE DBO.EAFILE
             SET
              EAFILE.EA_BRANCH = :QEA-BRANCH,
              EAFILE.EA_CLASS = :QEA-CLASS,
              EAFILE.EA_GROUP_ID = :QEA-GROUP-ID,
              EAFILE.EA_SEQNO = :QEA-SEQNO,
              EAFILE.EA_LTOUCH_DATE =
                 :QEA-LTOUCH-DATE,
              EAFILE.EA_DESC = :QEA-DESC,
              EAFILE.EA_PASTDUE_DAY1 = :QEA-PASTDUE-DAY1,
              EAFILE.EA_PASTDUE_DAY2 = :QEA-PASTDUE-DAY2,
              EAFILE.EA_PASTDUE_AMT1 = :QEA-PASTDUE-AMT1,
              EAFILE.EA_PASTDUE_AMT2 = :QEA-PASTDUE-AMT2,
              EAFILE.EA_ACCT_BAL1 = :QEA-ACCT-BAL1,
              EAFILE.EA_ACCT_BAL2 = :QEA-ACCT-BAL2,
              EAFILE.EA_MATURED = :QEA-MATURED,
              EAFILE.EA_PROMISED = :QEA-PROMISED,
              EAFILE.EA_ACTION = :QEA-ACTION,
              EAFILE.EA_LSTFM_DATE1 =
                 :QEA-LSTFM-DATE1,
              EAFILE.EA_LSTFM_DATE2 =
                 :QEA-LSTFM-DATE2,
              EAFILE.EA_LSTFM_NO1 = :QEA-LSTFM-NO1,
              EAFILE.EA_LSTFM_NO2 = :QEA-LSTFM-NO2,
              EAFILE.EA_PASTPROM_DAY1 = :QEA-PASTPROM-DAY1,
              EAFILE.EA_PASTPROM_DAY2 = :QEA-PASTPROM-DAY2,
              EAFILE.EA_RECENCY_DAY1 = :QEA-RECENCY-DAY1,
              EAFILE.EA_RECENCY_DAY2 = :QEA-RECENCY-DAY2,
              EAFILE.EA_ALPNAME1 = :QEA-ALPNAME1,
              EAFILE.EA_ALPNAME2 = :QEA-ALPNAME2,
              EAFILE.EA_RECENCYDATE = :QEA-RECENCYDATE,
              EAFILE.EA_DLNO1 = :QEA-DLNO1,
              EAFILE.EA_DLNO2 = :QEA-DLNO2,
              EAFILE.EA_CONTRACTUAL_DAY1 = :QEA-CONTRACTUAL-DAY1,
              EAFILE.EA_CONTRACTUAL_DAY2 = :QEA-CONTRACTUAL-DAY2,
              EAFILE.EA_SETFM_NO = :QEA-SETFM-NO,
              EAFILE.EA_SETACT_CD = :QEA-SETACT-CD,
              EAFILE.EA_INC_CNT = :QEA-INC-CNT,
              EAFILE.EA_INC_PROMISED = :QEA-INC-PROMISED,
              EAFILE.EA_SETFM_1 = :QEA-SETFM-1,
              EAFILE.EA_SETFM_2 = :QEA-SETFM-2,
              EAFILE.EA_SETFM_3 = :QEA-SETFM-3,
              EAFILE.EA_SET_LN_COL = :QEA-SET-LN-COL,
              EAFILE.EA_LN_COL_ID = :QEA-LN-COL-ID,
              EAFILE.EA_CQ_FG = :QEA-CQ-FG,
              EAFILE.EA_COL_POOL = :QEA-COL-POOL,
              EAFILE.EA_CPOTS = :QEA-CPOTS,
              EAFILE.EA_REPO = :QEA-REPO,
              EAFILE.EA_PL = :QEA-PL,
              EAFILE.EA_BANKRUPT = :QEA-BANKRUPT,
              EAFILE.EA_PL_IN_ACTIVE_BOTH =
                   :QEA-PL-IN-ACTIVE-BOTH,
              EAFILE.EA_CONFIDENTIAL_FG = :QEA-CONFIDENTIAL-FG,
              EAFILE.EA_1ST_PMT_MISSES = :QEA-1ST-PMT-MISSES,
              EAFILE.EA_LEGAL = :QEA-LEGAL,
              EAFILE.EA_DEBTAID = :QEA-DEBTAID,
              EAFILE.EA_JUDGEMENT = :QEA-JUDGEMENT,
              EAFILE.EA_BROKEN_PROM_FG = :QEA-BROKEN-PROM-FG,
              EAFILE.EA_PROM_BREAK1 = :QEA-PROM-BREAK1,
              EAFILE.EA_PROM_BREAK2 = :QEA-PROM-BREAK2,
              EAFILE.EA_ORIGIN = :QEA-ORIGIN,
              EAFILE.EA_PL_BEG_DATE =
                 :QEA-PL-BEG-DATE,
              EAFILE.EA_PL_END_DATE = :QEA-PL-END-DATE,
              EAFILE.EA_RPOTS = :QEA-RPOTS,
              EAFILE.EA_MISPAYS = :QEA-MISPAYS,
              EAFILE.EA_ALLOTMENTS = :QEA-ALLOTMENTS,
              EAFILE.EA_LNDATE_DAYS1 = :QEA-LNDATE-DAYS1,
              EAFILE.EA_LNDATE_DAYS2 = :QEA-LNDATE-DAYS2,
              EAFILE.EA_MATURE_DAYS1 = :QEA-MATURE-DAYS1,
              EAFILE.EA_MATURE_DAYS2 = :QEA-MATURE-DAYS2,
              EAFILE.EA_DAY_OF_MO_DUE1 = :QEA-DAY-OF-MO-DUE1,
              EAFILE.EA_DAY_OF_MO_DUE2 = :QEA-DAY-OF-MO-DUE2,
              EAFILE.EA_INCL_COLLECTOR = :QEA-INCL-COLLECTOR,
              EAFILE.EA_BORR_STATE = :QEA-BORR-STATE
             WHERE EAFILE.EA_BRANCH = :QEA-BRANCH
               AND EAFILE.EA_CLASS = :QEA-CLASS
               AND EAFILE.EA_GROUP_ID = :QEA-GROUP-ID
               AND EAFILE.EA_SEQNO = :QEA-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-EA1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-EA1-FILE.

           PERFORM DELETE-IT.
           MOVE EA-PATH-SQL TO E-FILE.
           MOVE EA1-KEY TO E-KEYX.
           PERFORM SET-EA-FIELDS.

           EXEC SQL
            DELETE FROM DBO.EAFILE
             WHERE EAFILE.EA_BRANCH = :QEA-BRANCH
               AND EAFILE.EA_CLASS = :QEA-CLASS
               AND EAFILE.EA_GROUP_ID = :QEA-GROUP-ID
               AND EAFILE.EA_SEQNO = :QEA-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-EA1-FILE.

      *-----------------------------------------------------------------
       CLOSE-EA1-FILE.

           PERFORM CLOSE-IT.

           IF ( EA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE EA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO EA1-CURSOR-STAT.

