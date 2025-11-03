      * COPYMEMBER: LIBGB/GBGBIN
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
       WRITE-GB-FILE.

           MOVE GB-PATH-OWNBR  TO GB-BRNO.
           ACCEPT GB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GB-PATH-SQL    TO E-FILE.
           MOVE GB-KEY         TO E-KEYX.
           PERFORM SET-GB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GBFILE
             (GBFILE.GB_BRNO,
              GBFILE.GB_NO,
              GBFILE.GB_LTOUCH_DATE,
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
              GBFILE.GB_EOM_UPDATE_DATE,
              GBFILE.GB_EOM_UPDATE_TIME,
              GBFILE.GB_EOM_UPDATE_USERID,
              GBFILE.GB_FISCAL_EOY_TAG_CCYYMM,
              GBFILE.GB_EOYF_UPDATE_DATE,
              GBFILE.GB_EOYF_UPDATE_TIME,
              GBFILE.GB_EOYF_UPDATE_USERID,
              GBFILE.GB_CALENDAR_EOY_TAG_CCYYMM,
              GBFILE.GB_EOYC_UPDATE_DATE,
              GBFILE.GB_EOYC_UPDATE_TIME,
              GBFILE.GB_EOYC_UPDATE_USERID)
            VALUES
             (:QGB-BRNO,
              :QGB-NO,
              :QGB-LTOUCH-DATE,
              :QGB-COMPANY-NAME,
              :QGB-ADDR-1,
              :QGB-ADDR-2,
              :QGB-CITY,
              :QGB-STATE,
              :QGB-ZIP,
              :QGB-PHONE,
              :QGB-NEXT-SF,
              :QGB-NEXT-SMALL,
              :QGB-NEXT-OTHER,
              :QGB-HOLIDAY-1,
              :QGB-HOLIDAY-2,
              :QGB-HOLIDAY-3,
              :QGB-HOLIDAY-4,
              :QGB-HOLIDAY-5,
              :QGB-HOLIDAY-6,
              :QGB-HOLIDAY-7,
              :QGB-HOLIDAY-8,
              :QGB-HOLIDAY-9,
              :QGB-HOLIDAY-10,
              :QGB-HOLIDAY-11,
              :QGB-HOLIDAY-12,
              :QGB-HOLIDAY-13,
              :QGB-HOLIDAY-14,
              :QGB-HOLIDAY-15,
              :QGB-HOLIDAY-16,
              :QGB-RPT-LINES,
              :QGB-CHECK-PROG,
              :QGB-OTHER-BRANCH-POST,
              :QGB-LAST-ACCRUE,
              :QGB-AUTO-DEPOSIT,
              :QGB-UUCP-HOME,
              :QGB-EOM-TAG-CCYYMM,
              :QGB-EOM-UPDATE-DATE,
              :QGB-EOM-UPDATE-TIME,
              :QGB-EOM-UPDATE-USERID,
              :QGB-FISCAL-EOY-TAG-CCYYMM,
              :QGB-EOYF-UPDATE-DATE,
              :QGB-EOYF-UPDATE-TIME,
              :QGB-EOYF-UPDATE-USERID,
              :QGB-CALENDAR-EOY-TAG-CCYYMM,
              :QGB-EOYC-UPDATE-DATE,
              :QGB-EOYC-UPDATE-TIME,
              :QGB-EOYC-UPDATE-USERID)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GB-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GB-FILE.

           MOVE GB-PATH-OWNBR  TO GB-BRNO.
           ACCEPT GB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GB-PATH-SQL    TO E-FILE.
           MOVE GB-KEY         TO E-KEYX.
           PERFORM SET-GB-FIELDS.

           EXEC SQL
            UPDATE DBO.GBFILE
             SET
              GBFILE.GB_BRNO = :QGB-BRNO,
              GBFILE.GB_NO = :QGB-NO,
              GBFILE.GB_LTOUCH_DATE =
                 :QGB-LTOUCH-DATE,
              GBFILE.GB_COMPANY_NAME = :QGB-COMPANY-NAME,
              GBFILE.GB_ADDR_1 = :QGB-ADDR-1,
              GBFILE.GB_ADDR_2 = :QGB-ADDR-2,
              GBFILE.GB_CITY = :QGB-CITY,
              GBFILE.GB_STATE = :QGB-STATE,
              GBFILE.GB_ZIP = :QGB-ZIP,
              GBFILE.GB_PHONE = :QGB-PHONE,
              GBFILE.GB_NEXT_SF = :QGB-NEXT-SF,
              GBFILE.GB_NEXT_SMALL = :QGB-NEXT-SMALL,
              GBFILE.GB_NEXT_OTHER = :QGB-NEXT-OTHER,
              GBFILE.GB_HOLIDAY_1 = :QGB-HOLIDAY-1,
              GBFILE.GB_HOLIDAY_2 = :QGB-HOLIDAY-2,
              GBFILE.GB_HOLIDAY_3 = :QGB-HOLIDAY-3,
              GBFILE.GB_HOLIDAY_4 = :QGB-HOLIDAY-4,
              GBFILE.GB_HOLIDAY_5 = :QGB-HOLIDAY-5,
              GBFILE.GB_HOLIDAY_6 = :QGB-HOLIDAY-6,
              GBFILE.GB_HOLIDAY_7 = :QGB-HOLIDAY-7,
              GBFILE.GB_HOLIDAY_8 = :QGB-HOLIDAY-8,
              GBFILE.GB_HOLIDAY_9 = :QGB-HOLIDAY-9,
              GBFILE.GB_HOLIDAY_10 = :QGB-HOLIDAY-10,
              GBFILE.GB_HOLIDAY_11 = :QGB-HOLIDAY-11,
              GBFILE.GB_HOLIDAY_12 = :QGB-HOLIDAY-12,
              GBFILE.GB_HOLIDAY_13 = :QGB-HOLIDAY-13,
              GBFILE.GB_HOLIDAY_14 = :QGB-HOLIDAY-14,
              GBFILE.GB_HOLIDAY_15 = :QGB-HOLIDAY-15,
              GBFILE.GB_HOLIDAY_16 = :QGB-HOLIDAY-16,
              GBFILE.GB_RPT_LINES = :QGB-RPT-LINES,
              GBFILE.GB_CHECK_PROG = :QGB-CHECK-PROG,
              GBFILE.GB_OTHER_BRANCH_POST =
                   :QGB-OTHER-BRANCH-POST,
              GBFILE.GB_LAST_ACCRUE = :QGB-LAST-ACCRUE,
              GBFILE.GB_AUTO_DEPOSIT = :QGB-AUTO-DEPOSIT,
              GBFILE.GB_UUCP_HOME = :QGB-UUCP-HOME,
              GBFILE.GB_EOM_TAG_CCYYMM = :QGB-EOM-TAG-CCYYMM,
              GBFILE.GB_EOM_UPDATE_DATE =
                 :QGB-EOM-UPDATE-DATE,
              GBFILE.GB_EOM_UPDATE_TIME = :QGB-EOM-UPDATE-TIME,
              GBFILE.GB_EOM_UPDATE_USERID = :QGB-EOM-UPDATE-USERID,
              GBFILE.GB_FISCAL_EOY_TAG_CCYYMM =
                   :QGB-FISCAL-EOY-TAG-CCYYMM,
              GBFILE.GB_EOYF_UPDATE_DATE =
                 :QGB-EOYF-UPDATE-DATE,
              GBFILE.GB_EOYF_UPDATE_TIME = :QGB-EOYF-UPDATE-TIME,
              GBFILE.GB_EOYF_UPDATE_USERID = :QGB-EOYF-UPDATE-USERID,
              GBFILE.GB_CALENDAR_EOY_TAG_CCYYMM =
                   :QGB-CALENDAR-EOY-TAG-CCYYMM,
              GBFILE.GB_EOYC_UPDATE_DATE =
                 :QGB-EOYC-UPDATE-DATE,
              GBFILE.GB_EOYC_UPDATE_TIME = :QGB-EOYC-UPDATE-TIME,
              GBFILE.GB_EOYC_UPDATE_USERID = :QGB-EOYC-UPDATE-USERID
             WHERE GBFILE.GB_BRNO = :QGB-BRNO
               AND GBFILE.GB_NO = :QGB-NO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GB-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GB-FILE.

           MOVE GB-PATH-OWNBR  TO GB-BRNO.
           PERFORM DELETE-IT.
           MOVE GB-PATH-SQL    TO E-FILE.
           MOVE GB-KEY         TO E-KEYX.
           PERFORM SET-GB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GBFILE
             WHERE GBFILE.GB_BRNO = :QGB-BRNO
               AND GBFILE.GB_NO = :QGB-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GB-FILE.

      *-----------------------------------------------------------------
       CLOSE-GB-FILE.

           PERFORM CLOSE-IT.

           IF ( GB-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GB_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GB-CURSOR-STAT.

