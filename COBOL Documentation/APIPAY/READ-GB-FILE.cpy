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
