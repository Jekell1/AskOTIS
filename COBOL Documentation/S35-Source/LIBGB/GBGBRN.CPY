      * COPYMEMBER: LIBGB/GBGBRN
       LOAD-GB-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO GB-ALL-DATA
              MOVE GB-ALL-PATH             TO GB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GB-ALL-BASE
              MOVE FILPATH-MACHINE         TO GB-ALL-MACHINE
              MOVE FILPATH-DATA            TO GB-ALL-DATA
              MOVE GB-ALL-PATH             TO GB-PATH.

      *-----------------------------------------------------------------
       OPEN-GB-FILE.
           PERFORM LOAD-GB-FILE.
           PERFORM OPEN-IT.
           MOVE GB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GB-FILE.
           PERFORM START-IT.
           MOVE GB-PATH-OWNBR  TO GB-BRNO.
           MOVE GB-PATH-SQL    TO E-FILE.
           MOVE GB-KEY         TO E-KEYX.

           IF ( GB-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GB-FILE.

           IF ( QGB-WEND-NO = ZEROES    ) OR
              ( QGB-WEND-NO NOT NUMERIC )
              MOVE GB-PATH-OWNBR    TO QGB-WBEG-BRNO
              MOVE GB-PATH-OWNBR    TO QGB-WEND-BRNO
              MOVE GB-NO            TO QGB-WBEG-NO
              MOVE 9                TO QGB-WEND-NO.

           EXEC SQL
            DECLARE GB_CURSOR CURSOR FOR
            SELECT GBFILE.GB_BRNO,
                   GBFILE.GB_NO,
                   CAST(GBFILE.GB_LTOUCH_DATE AS VARCHAR(10)),
                   GBFILE.GB_COMPANY_NAME,
                   GBFILE.GB_ADDR_1,
                   GBFILE.GB_ADDR_2,
                   GBFILE.GB_CITY,
                   GBFILE.GB_STATE,
                   GBFILE.GB_ZIP,
                   GBFILE.GB_PHONE,
                   GBFILE.GB_NEXT_SF,
                   GBFILE.GB_NEXT_SMALL,
                   GBFILE.GB_NEXT_OTHER,
                   GBFILE.GB_HOLIDAY_1,
                   GBFILE.GB_HOLIDAY_2,
                   GBFILE.GB_HOLIDAY_3,
                   GBFILE.GB_HOLIDAY_4,
                   GBFILE.GB_HOLIDAY_5,
                   GBFILE.GB_HOLIDAY_6,
                   GBFILE.GB_HOLIDAY_7,
                   GBFILE.GB_HOLIDAY_8,
                   GBFILE.GB_HOLIDAY_9,
                   GBFILE.GB_HOLIDAY_10,
                   GBFILE.GB_HOLIDAY_11,
                   GBFILE.GB_HOLIDAY_12,
                   GBFILE.GB_HOLIDAY_13,
                   GBFILE.GB_HOLIDAY_14,
                   GBFILE.GB_HOLIDAY_15,
                   GBFILE.GB_HOLIDAY_16,
                   GBFILE.GB_RPT_LINES,
                   GBFILE.GB_CHECK_PROG,
                   GBFILE.GB_OTHER_BRANCH_POST,
                   GBFILE.GB_LAST_ACCRUE,
                   GBFILE.GB_AUTO_DEPOSIT,
                   GBFILE.GB_UUCP_HOME,
                   GBFILE.GB_EOM_TAG_CCYYMM,
                   CAST(GBFILE.GB_EOM_UPDATE_DATE AS VARCHAR(10)),
                   GBFILE.GB_EOM_UPDATE_TIME,
                   GBFILE.GB_EOM_UPDATE_USERID,
                   GBFILE.GB_FISCAL_EOY_TAG_CCYYMM,
                   CAST(GBFILE.GB_EOYF_UPDATE_DATE AS VARCHAR(10)),
                   GBFILE.GB_EOYF_UPDATE_TIME,
                   GBFILE.GB_EOYF_UPDATE_USERID,
                   GBFILE.GB_CALENDAR_EOY_TAG_CCYYMM,
                   CAST(GBFILE.GB_EOYC_UPDATE_DATE AS VARCHAR(10)),
                   GBFILE.GB_EOYC_UPDATE_TIME,
                   GBFILE.GB_EOYC_UPDATE_USERID
            FROM DBO.GBFILE
            WHERE GBFILE.GB_BRNO = :QGB-WBEG-BRNO
              AND GBFILE.GB_NO >= :QGB-WBEG-NO
              AND GBFILE.GB_NO <= :QGB-WEND-NO
            ORDER BY GBFILE.GB_BRNO,
                     GBFILE.GB_NO
           END-EXEC.

           EXEC SQL
               OPEN GB_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGB-WBEG-BRNO 
                          QGB-WEND-BRNO
                          QGB-WBEG-NO
                          QGB-WEND-NO.

           MOVE STAT---GOOD TO GB-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GB-FILE.
           PERFORM READ-IT.
           MOVE GB-PATH-OWNBR  TO GB-BRNO. 
           MOVE GB-PATH-SQL    TO E-FILE.
           MOVE GB-KEY         TO E-KEYX.
           INITIALIZE QGB-REC.

           MOVE GB-BRNO   TO QGB-BRNO.
           MOVE GB-NO     TO QGB-NO.

           EXEC SQL
            SELECT GBFILE.GB_BRNO,
                   GBFILE.GB_NO,
                   CAST(GBFILE.GB_LTOUCH_DATE AS VARCHAR(10)),
                   GBFILE.GB_COMPANY_NAME,
                   GBFILE.GB_ADDR_1,
                   GBFILE.GB_ADDR_2,
                   GBFILE.GB_CITY,
                   GBFILE.GB_STATE,
                   GBFILE.GB_ZIP,
                   GBFILE.GB_PHONE,
                   GBFILE.GB_NEXT_SF,
                   GBFILE.GB_NEXT_SMALL,
                   GBFILE.GB_NEXT_OTHER,
                   GBFILE.GB_HOLIDAY_1,
                   GBFILE.GB_HOLIDAY_2,
                   GBFILE.GB_HOLIDAY_3,
                   GBFILE.GB_HOLIDAY_4,
                   GBFILE.GB_HOLIDAY_5,
                   GBFILE.GB_HOLIDAY_6,
                   GBFILE.GB_HOLIDAY_7,
                   GBFILE.GB_HOLIDAY_8,
                   GBFILE.GB_HOLIDAY_9,
                   GBFILE.GB_HOLIDAY_10,
                   GBFILE.GB_HOLIDAY_11,
                   GBFILE.GB_HOLIDAY_12,
                   GBFILE.GB_HOLIDAY_13,
                   GBFILE.GB_HOLIDAY_14,
                   GBFILE.GB_HOLIDAY_15,
                   GBFILE.GB_HOLIDAY_16,
                   GBFILE.GB_RPT_LINES,
                   GBFILE.GB_CHECK_PROG,
                   GBFILE.GB_OTHER_BRANCH_POST,
                   GBFILE.GB_LAST_ACCRUE,
                   GBFILE.GB_AUTO_DEPOSIT,
                   GBFILE.GB_UUCP_HOME,
                   GBFILE.GB_EOM_TAG_CCYYMM,
                   CAST(GBFILE.GB_EOM_UPDATE_DATE AS VARCHAR(10)),
                   GBFILE.GB_EOM_UPDATE_TIME,
                   GBFILE.GB_EOM_UPDATE_USERID,
                   GBFILE.GB_FISCAL_EOY_TAG_CCYYMM,
                   CAST(GBFILE.GB_EOYF_UPDATE_DATE AS VARCHAR(10)),
                   GBFILE.GB_EOYF_UPDATE_TIME,
                   GBFILE.GB_EOYF_UPDATE_USERID,
                   GBFILE.GB_CALENDAR_EOY_TAG_CCYYMM,
                   CAST(GBFILE.GB_EOYC_UPDATE_DATE AS VARCHAR(10)),
                   GBFILE.GB_EOYC_UPDATE_TIME,
                   GBFILE.GB_EOYC_UPDATE_USERID
            INTO :QGB-REC
            FROM DBO.GBFILE
            WHERE GBFILE.GB_BRNO = :QGB-BRNO
              AND GBFILE.GB_NO = :QGB-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GB-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GB-FIELDS.

      *-----------------------------------------------------------------
       READ-GB-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GB-PATH-OWNBR  TO GB-BRNO.
           MOVE GB-PATH-SQL    TO E-FILE.
           MOVE GB-KEY         TO E-KEYX.
           INITIALIZE QGB-REC.

           IF ( GB-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GB_CURSOR INTO :QGB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GB-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GB-FILE.

           PERFORM CLOSE-IT.
           IF ( GB-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GB_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GB-CURSOR-STAT.

