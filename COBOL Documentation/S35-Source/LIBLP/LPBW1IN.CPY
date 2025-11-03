      * COPYMEMBER: LIBLP/LPBW1IN
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW1-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-SSNO = ZEROES    ) OR
              ( QBW1-WEND-SSNO NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   BWFILE.BW_LNAME,
                   BWFILE.BW_FNAME,
                   BWFILE.BW_MNAME,
                   BWFILE.BW_NICKNAME,
                   BWFILE.BW_GENERATION,
                   BWFILE.BW_ADR1,
                   BWFILE.BW_ADR2,
                   BWFILE.BW_CITY,
                   BWFILE.BW_STATE,
                   BWFILE.BW_ZIP,
                   CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),
                   CAST(BWFILE.BW_BDATE AS VARCHAR(10)),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   BWFILE.BW_NATIONALITY,
                   BWFILE.BW_GENDER,
                   BWFILE.BW_DEPEND,
                   BWFILE.BW_RENTCD,
                   BWFILE.BW_TIMEADDR,
                   BWFILE.BW_CREDITCD,
                   BWFILE.BW_STRTGCD,
                   BWFILE.BW_DWELLCD,
                   BWFILE.BW_COUNTY,
                   BWFILE.BW_ORIGIN,
                   BWFILE.BW_CENSUSCD,
                   BWFILE.BW_ORGSLSMN,
                   BWFILE.BW_REPCODE,
                   BWFILE.BW_SPSSNO,
                   BWFILE.BW_SPLNAME,
                   BWFILE.BW_SPFNAME,
                   CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),
                   BWFILE.BW_EMPLOYER,
                   BWFILE.BW_EMPLPOS,
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   BWFILE.BW_EMPPAYSR,
                   BWFILE.BW_EMPADDR1,
                   BWFILE.BW_EMPADDR2,
                   BWFILE.BW_EMPCITY,
                   BWFILE.BW_EMPSTATE,
                   BWFILE.BW_EMPZIP,
                   BWFILE.BW_WORKHRS,
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_BAD_ADR_FG,
                   BWFILE.BW_BAD_PHONE_FG,
                   BWFILE.BW_BAD_EMPPHONE_FG,
                   BWFILE.BW_BAD_MISC_PHONE1_FG,
                   BWFILE.BW_BAD_MISC_PHONE2_FG,
                   BWFILE.BW_BAD_CELL_FG,
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   BWFILE.BW_MISC_PHONE_CD_1,
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   BWFILE.BW_MISC_PHONE_CD_2,
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   BWFILE.BW_MISC_PHONE_CD_3,
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   BWFILE.BW_MISC_PHONE_CD_4,
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   BWFILE.BW_MISC_PHONE_CD_5,
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   BWFILE.BW_MISC_PHONE_CD_6,
                   BWFILE.BW_CONTACT_DESC_1,
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   BWFILE.BW_CONTACT_RELATION_1,
                   BWFILE.BW_CONTACT_DESC_2,
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   BWFILE.BW_CONTACT_RELATION_2,
                   BWFILE.BW_CONTACT_DESC_3,
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   BWFILE.BW_CONTACT_RELATION_3,
                   BWFILE.BW_CONTACT_DESC_4,
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   BWFILE.BW_CONTACT_RELATION_4,
                   BWFILE.BW_CONTACT_DESC_5,
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   BWFILE.BW_CONTACT_RELATION_5,
                   BWFILE.BW_CONTACT_DESC_6,
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   BWFILE.BW_CONTACT_RELATION_6,
                   BWFILE.BW_CONTACT_DESC_7,
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   BWFILE.BW_CONTACT_RELATION_7,
                   BWFILE.BW_CONTACT_DESC_8,
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   BWFILE.BW_CONTACT_RELATION_8,
                   BWFILE.BW_CONTACT_DESC_9,
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   BWFILE.BW_CONTACT_RELATION_9,
                   BWFILE.BW_CONTACT_DESC_10,
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   BWFILE.BW_CONTACT_RELATION_10,
                   BWFILE.BW_SRCD,
                   BWFILE.BW_CALL_ON_SUNDAY,
                   BWFILE.BW_SOLICIT,
                   BWFILE.BW_CONFIDENTIAL,
                   BWFILE.BW_PRIVACY_OPT_OUT,
                   BWFILE.BW_OPT_IN_EMAIL,
                   BWFILE.BW_OPT_IN_TEXT,
                   BWFILE.BW_CITY_TAX_CD,
                   CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_DECEASED,
                   CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_EMAIL,
                   BWFILE.BW_IDENT_NO,
                   BWFILE.BW_IDENT_TYPE,
                   BWFILE.BW_IDENT_STATE,
                   BWFILE.BW_MILITARY_FLAG,
                   BWFILE.BW_ORGBR,
                   CAST(BWFILE.BW_CEASE_DESIST_DATE AS VARCHAR(10)),
                   BWFILE.BW_CEASE_DESIST_TYPE,
                   CAST(BWFILE.BW_UMC_EXPIRE_DATE AS VARCHAR(10)),
                   CAST(BWFILE.BW_UMC_PR_EXPIRE_DATE AS VARCHAR(10))
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR = :QBW1-WBEG-OWNBR
              AND BWFILE.BW_SSNO >= :QBW1-WBEG-SSNO
              AND BWFILE.BW_SSNO <= :QBW1-WEND-SSNO
              ORDER BY BWFILE.BW_OWNBR,
                       BWFILE.BW_SSNO
           END-EXEC.

           EXEC SQL
               OPEN BW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW1-WBEG-OWNBR 
                          QBW1-WEND-OWNBR
                          QBW1-WBEG-SSNO
                          QBW1-WEND-SSNO.

           MOVE STAT---GOOD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BW1-FILE.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   BWFILE.BW_LNAME,
                   BWFILE.BW_FNAME,
                   BWFILE.BW_MNAME,
                   BWFILE.BW_NICKNAME,
                   BWFILE.BW_GENERATION,
                   BWFILE.BW_ADR1,
                   BWFILE.BW_ADR2,
                   BWFILE.BW_CITY,
                   BWFILE.BW_STATE,
                   BWFILE.BW_ZIP,
                   CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),
                   CAST(BWFILE.BW_BDATE AS VARCHAR(10)),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   BWFILE.BW_NATIONALITY,
                   BWFILE.BW_GENDER,
                   BWFILE.BW_DEPEND,
                   BWFILE.BW_RENTCD,
                   BWFILE.BW_TIMEADDR,
                   BWFILE.BW_CREDITCD,
                   BWFILE.BW_STRTGCD,
                   BWFILE.BW_DWELLCD,
                   BWFILE.BW_COUNTY,
                   BWFILE.BW_ORIGIN,
                   BWFILE.BW_CENSUSCD,
                   BWFILE.BW_ORGSLSMN,
                   BWFILE.BW_REPCODE,
                   BWFILE.BW_SPSSNO,
                   BWFILE.BW_SPLNAME,
                   BWFILE.BW_SPFNAME,
                   CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),
                   BWFILE.BW_EMPLOYER,
                   BWFILE.BW_EMPLPOS,
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   BWFILE.BW_EMPPAYSR,
                   BWFILE.BW_EMPADDR1,
                   BWFILE.BW_EMPADDR2,
                   BWFILE.BW_EMPCITY,
                   BWFILE.BW_EMPSTATE,
                   BWFILE.BW_EMPZIP,
                   BWFILE.BW_WORKHRS,
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_BAD_ADR_FG,
                   BWFILE.BW_BAD_PHONE_FG,
                   BWFILE.BW_BAD_EMPPHONE_FG,
                   BWFILE.BW_BAD_MISC_PHONE1_FG,
                   BWFILE.BW_BAD_MISC_PHONE2_FG,
                   BWFILE.BW_BAD_CELL_FG,
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   BWFILE.BW_MISC_PHONE_CD_1,
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   BWFILE.BW_MISC_PHONE_CD_2,
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   BWFILE.BW_MISC_PHONE_CD_3,
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   BWFILE.BW_MISC_PHONE_CD_4,
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   BWFILE.BW_MISC_PHONE_CD_5,
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   BWFILE.BW_MISC_PHONE_CD_6,
                   BWFILE.BW_CONTACT_DESC_1,
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   BWFILE.BW_CONTACT_RELATION_1,
                   BWFILE.BW_CONTACT_DESC_2,
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   BWFILE.BW_CONTACT_RELATION_2,
                   BWFILE.BW_CONTACT_DESC_3,
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   BWFILE.BW_CONTACT_RELATION_3,
                   BWFILE.BW_CONTACT_DESC_4,
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   BWFILE.BW_CONTACT_RELATION_4,
                   BWFILE.BW_CONTACT_DESC_5,
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   BWFILE.BW_CONTACT_RELATION_5,
                   BWFILE.BW_CONTACT_DESC_6,
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   BWFILE.BW_CONTACT_RELATION_6,
                   BWFILE.BW_CONTACT_DESC_7,
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   BWFILE.BW_CONTACT_RELATION_7,
                   BWFILE.BW_CONTACT_DESC_8,
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   BWFILE.BW_CONTACT_RELATION_8,
                   BWFILE.BW_CONTACT_DESC_9,
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   BWFILE.BW_CONTACT_RELATION_9,
                   BWFILE.BW_CONTACT_DESC_10,
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   BWFILE.BW_CONTACT_RELATION_10,
                   BWFILE.BW_SRCD,
                   BWFILE.BW_CALL_ON_SUNDAY,
                   BWFILE.BW_SOLICIT,
                   BWFILE.BW_CONFIDENTIAL,
                   BWFILE.BW_PRIVACY_OPT_OUT,
                   BWFILE.BW_OPT_IN_EMAIL,
                   BWFILE.BW_OPT_IN_TEXT,
                   BWFILE.BW_CITY_TAX_CD,
                   CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_DECEASED,
                   CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_EMAIL,
                   BWFILE.BW_IDENT_NO,
                   BWFILE.BW_IDENT_TYPE,
                   BWFILE.BW_IDENT_STATE,
                   BWFILE.BW_MILITARY_FLAG,
                   BWFILE.BW_ORGBR,
                   CAST(BWFILE.BW_CEASE_DESIST_DATE AS VARCHAR(10)),
                   BWFILE.BW_CEASE_DESIST_TYPE,
                   CAST(BWFILE.BW_UMC_EXPIRE_DATE AS VARCHAR(10)),
                   CAST(BWFILE.BW_UMC_PR_EXPIRE_DATE AS VARCHAR(10))
            INTO :QBW-REC
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
              AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       READ-BW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW1_CURSOR INTO :QBW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       WRITE-BW1-FILE.
           MOVE BW-PATH-OWNBR TO BW-OWNBR. 
           ACCEPT BW-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE BW-PATH-SQL    TO E-FILE.

      *    MOVE SPACES         TO BW-VISION-KEY2
      *                           BW-VISION-KEY3
      *                           BW-VISION-KEY4.
      *    STRING BW-OWNBR, BW-LNAME, BW-FNAME
      *           DELIMITED BY SIZE INTO BW-VISION-KEY2.
      *    STRING BW-OWNBR, BW-SPSSNO
      *           DELIMITED BY SIZE INTO BW-VISION-KEY3.
      *    STRING BW-OWNBR, BW-SPLNAME, BW-SPFNAME
      *           DELIMITED BY SIZE INTO BW-VISION-KEY4.
      *    MOVE BW1-KEY        TO E-KEYX BW-VISION-KEY1.

           MOVE BW1-KEY        TO E-KEYX.
           PERFORM SET-BW-FIELDS.

           EXEC SQL
            INSERT INTO DBO.BWFILE
             (BWFILE.BW_OWNBR,
              BWFILE.BW_SSNO,
              BWFILE.BW_LNAME,
              BWFILE.BW_FNAME,
              BWFILE.BW_MNAME,
              BWFILE.BW_NICKNAME,
              BWFILE.BW_GENERATION,
              BWFILE.BW_ADR1,
              BWFILE.BW_ADR2,
              BWFILE.BW_CITY,
              BWFILE.BW_STATE,
              BWFILE.BW_ZIP,
              BWFILE.BW_LTOUCH_DATE,
              BWFILE.BW_REFDATE,
              BWFILE.BW_BDATE,
              BWFILE.BW_PHONE,
              BWFILE.BW_CELL_PHONE,
              BWFILE.BW_NATIONALITY,
              BWFILE.BW_GENDER,
              BWFILE.BW_DEPEND,
              BWFILE.BW_RENTCD,
              BWFILE.BW_TIMEADDR,
              BWFILE.BW_CREDITCD,
              BWFILE.BW_STRTGCD,
              BWFILE.BW_DWELLCD,
              BWFILE.BW_COUNTY,
              BWFILE.BW_ORIGIN,
              BWFILE.BW_CENSUSCD,
              BWFILE.BW_ORGSLSMN,
              BWFILE.BW_REPCODE,
              BWFILE.BW_SPSSNO,
              BWFILE.BW_SPLNAME,
              BWFILE.BW_SPFNAME,
              BWFILE.BW_SP_BDATE,
              BWFILE.BW_EMPLOYER,
              BWFILE.BW_EMPLPOS,
              BWFILE.BW_EMPPHONE,
              BWFILE.BW_EMPEXT,
              BWFILE.BW_EMPYEARS,
              BWFILE.BW_EMPPAY,
              BWFILE.BW_EMPPAYOT,
              BWFILE.BW_EMPPAYSR,
              BWFILE.BW_EMPADDR1,
              BWFILE.BW_EMPADDR2,
              BWFILE.BW_EMPCITY,
              BWFILE.BW_EMPSTATE,
              BWFILE.BW_EMPZIP,
              BWFILE.BW_WORKHRS,
              BWFILE.BW_LOANS,
              BWFILE.BW_INTYTD,
              BWFILE.BW_INTLYR,
              BWFILE.BW_LIASMLN,
              BWFILE.BW_LIAOTH,
              BWFILE.BW_LASTPODATE,
              BWFILE.BW_PRLOANS,
              BWFILE.BW_RE_INTYTD,
              BWFILE.BW_RE_INTLYR,
              BWFILE.BW_CREDIT_LIMIT,
              BWFILE.BW_CREDIT_SCORE,
              BWFILE.BW_CRED_SCORE_1,
              BWFILE.BW_CRED_SCORE_2,
              BWFILE.BW_CRED_SCORE_3,
              BWFILE.BW_CRED_SCORE_4,
              BWFILE.BW_BNKRPT_SCORE_1,
              BWFILE.BW_BNKRPT_SCORE_2,
              BWFILE.BW_BNKRPT_SCORE_3,
              BWFILE.BW_BNKRPT_SCORE_4,
              BWFILE.BW_LAST_FORM_NO,
              BWFILE.BW_LAST_FORM_DATE,
              BWFILE.BW_PRIOR_RN_FOR10_DATE,
              BWFILE.BW_LAST_RN_FOR10_DATE,
              BWFILE.BW_ADR_PHONE_CHG_DATE,
              BWFILE.BW_BAD_ADR_FG,
              BWFILE.BW_BAD_PHONE_FG,
              BWFILE.BW_BAD_EMPPHONE_FG,
              BWFILE.BW_BAD_MISC_PHONE1_FG,
              BWFILE.BW_BAD_MISC_PHONE2_FG,
              BWFILE.BW_BAD_CELL_FG,
              BWFILE.BW_MISC_PHONE_1,
              BWFILE.BW_MISC_EXT_1,
              BWFILE.BW_MISC_PHONE_CD_1,
              BWFILE.BW_MISC_PHONE_2,
              BWFILE.BW_MISC_EXT_2,
              BWFILE.BW_MISC_PHONE_CD_2,
              BWFILE.BW_MISC_PHONE_3,
              BWFILE.BW_MISC_EXT_3,
              BWFILE.BW_MISC_PHONE_CD_3,
              BWFILE.BW_MISC_PHONE_4,
              BWFILE.BW_MISC_EXT_4,
              BWFILE.BW_MISC_PHONE_CD_4,
              BWFILE.BW_MISC_PHONE_5,
              BWFILE.BW_MISC_EXT_5,
              BWFILE.BW_MISC_PHONE_CD_5,
              BWFILE.BW_MISC_PHONE_6,
              BWFILE.BW_MISC_EXT_6,
              BWFILE.BW_MISC_PHONE_CD_6,
              BWFILE.BW_CONTACT_DESC_1,
              BWFILE.BW_CONTACT_PHONE_1,
              BWFILE.BW_CONTACT_EXT_1,
              BWFILE.BW_CONTACT_RELATION_1,
              BWFILE.BW_CONTACT_DESC_2,
              BWFILE.BW_CONTACT_PHONE_2,
              BWFILE.BW_CONTACT_EXT_2,
              BWFILE.BW_CONTACT_RELATION_2,
              BWFILE.BW_CONTACT_DESC_3,
              BWFILE.BW_CONTACT_PHONE_3,
              BWFILE.BW_CONTACT_EXT_3,
              BWFILE.BW_CONTACT_RELATION_3,
              BWFILE.BW_CONTACT_DESC_4,
              BWFILE.BW_CONTACT_PHONE_4,
              BWFILE.BW_CONTACT_EXT_4,
              BWFILE.BW_CONTACT_RELATION_4,
              BWFILE.BW_CONTACT_DESC_5,
              BWFILE.BW_CONTACT_PHONE_5,
              BWFILE.BW_CONTACT_EXT_5,
              BWFILE.BW_CONTACT_RELATION_5,
              BWFILE.BW_CONTACT_DESC_6,
              BWFILE.BW_CONTACT_PHONE_6,
              BWFILE.BW_CONTACT_EXT_6,
              BWFILE.BW_CONTACT_RELATION_6,
              BWFILE.BW_CONTACT_DESC_7,
              BWFILE.BW_CONTACT_PHONE_7,
              BWFILE.BW_CONTACT_EXT_7,
              BWFILE.BW_CONTACT_RELATION_7,
              BWFILE.BW_CONTACT_DESC_8,
              BWFILE.BW_CONTACT_PHONE_8,
              BWFILE.BW_CONTACT_EXT_8,
              BWFILE.BW_CONTACT_RELATION_8,
              BWFILE.BW_CONTACT_DESC_9,
              BWFILE.BW_CONTACT_PHONE_9,
              BWFILE.BW_CONTACT_EXT_9,
              BWFILE.BW_CONTACT_RELATION_9,
              BWFILE.BW_CONTACT_DESC_10,
              BWFILE.BW_CONTACT_PHONE_10,
              BWFILE.BW_CONTACT_EXT_10,
              BWFILE.BW_CONTACT_RELATION_10,
              BWFILE.BW_SRCD,
              BWFILE.BW_CALL_ON_SUNDAY,
              BWFILE.BW_SOLICIT,
              BWFILE.BW_CONFIDENTIAL,
              BWFILE.BW_PRIVACY_OPT_OUT,
              BWFILE.BW_OPT_IN_EMAIL,
              BWFILE.BW_OPT_IN_TEXT,
              BWFILE.BW_CITY_TAX_CD,
              BWFILE.BW_DEALTRACK_CHG_DATE,
              BWFILE.BW_DECEASED,
              BWFILE.BW_DECEASED_DATE,
              BWFILE.BW_EMAIL,
              BWFILE.BW_IDENT_NO,
              BWFILE.BW_IDENT_TYPE,
              BWFILE.BW_IDENT_STATE,
              BWFILE.BW_MILITARY_FLAG,
              BWFILE.BW_ORGBR,
              BWFILE.BW_CEASE_DESIST_DATE,
              BWFILE.BW_CEASE_DESIST_TYPE,
              BWFILE.BW_UMC_EXPIRE_DATE,
              BWFILE.BW_UMC_PR_EXPIRE_DATE)
            VALUES
             (:QBW-OWNBR,
              :QBW-SSNO,
              :QBW-LNAME,
              :QBW-FNAME,
              :QBW-MNAME,
              :QBW-NICKNAME,
              :QBW-GENERATION,
              :QBW-ADR1,
              :QBW-ADR2,
              :QBW-CITY,
              :QBW-STATE,
              :QBW-ZIP,
              :QBW-LTOUCH-DATE,
              :QBW-REFDATE,
              :QBW-BDATE,
              :QBW-PHONE,
              :QBW-CELL-PHONE,
              :QBW-NATIONALITY,
              :QBW-GENDER,
              :QBW-DEPEND,
              :QBW-RENTCD,
              :QBW-TIMEADDR,
              :QBW-CREDITCD,
              :QBW-STRTGCD,
              :QBW-DWELLCD,
              :QBW-COUNTY,
              :QBW-ORIGIN,
              :QBW-CENSUSCD,
              :QBW-ORGSLSMN,
              :QBW-REPCODE,
              :QBW-SPSSNO,
              :QBW-SPLNAME,
              :QBW-SPFNAME,
              :QBW-SP-BDATE,
              :QBW-EMPLOYER,
              :QBW-EMPLPOS,
              :QBW-EMPPHONE,
              :QBW-EMPEXT,
              :QBW-EMPYEARS,
              :QBW-EMPPAY,
              :QBW-EMPPAYOT,
              :QBW-EMPPAYSR,
              :QBW-EMPADDR1,
              :QBW-EMPADDR2,
              :QBW-EMPCITY,
              :QBW-EMPSTATE,
              :QBW-EMPZIP,
              :QBW-WORKHRS,
              :QBW-LOANS,
              :QBW-INTYTD,
              :QBW-INTLYR,
              :QBW-LIASMLN,
              :QBW-LIAOTH,
              :QBW-LASTPODATE,
              :QBW-PRLOANS,
              :QBW-RE-INTYTD,
              :QBW-RE-INTLYR,
              :QBW-CREDIT-LIMIT,
              :QBW-CREDIT-SCORE,
              :QBW-CRED-SCORE-1,
              :QBW-CRED-SCORE-2,
              :QBW-CRED-SCORE-3,
              :QBW-CRED-SCORE-4,
              :QBW-BNKRPT-SCORE-1,
              :QBW-BNKRPT-SCORE-2,
              :QBW-BNKRPT-SCORE-3,
              :QBW-BNKRPT-SCORE-4,
              :QBW-LAST-FORM-NO,
              :QBW-LAST-FORM-DATE,
              :QBW-PRIOR-RN-FOR10-DATE,
              :QBW-LAST-RN-FOR10-DATE,
              :QBW-ADR-PHONE-CHG-DATE,
              :QBW-BAD-ADR-FG,
              :QBW-BAD-PHONE-FG,
              :QBW-BAD-EMPPHONE-FG,
              :QBW-BAD-MISC-PHONE1-FG,
              :QBW-BAD-MISC-PHONE2-FG,
              :QBW-BAD-CELL-FG,
              :QBW-MISC-PHONE-1,
              :QBW-MISC-EXT-1,
              :QBW-MISC-PHONE-CD-1,
              :QBW-MISC-PHONE-2,
              :QBW-MISC-EXT-2,
              :QBW-MISC-PHONE-CD-2,
              :QBW-MISC-PHONE-3,
              :QBW-MISC-EXT-3,
              :QBW-MISC-PHONE-CD-3,
              :QBW-MISC-PHONE-4,
              :QBW-MISC-EXT-4,
              :QBW-MISC-PHONE-CD-4,
              :QBW-MISC-PHONE-5,
              :QBW-MISC-EXT-5,
              :QBW-MISC-PHONE-CD-5,
              :QBW-MISC-PHONE-6,
              :QBW-MISC-EXT-6,
              :QBW-MISC-PHONE-CD-6,
              :QBW-CONTACT-DESC-1,
              :QBW-CONTACT-PHONE-1,
              :QBW-CONTACT-EXT-1,
              :QBW-CONTACT-RELATION-1,
              :QBW-CONTACT-DESC-2,
              :QBW-CONTACT-PHONE-2,
              :QBW-CONTACT-EXT-2,
              :QBW-CONTACT-RELATION-2,
              :QBW-CONTACT-DESC-3,
              :QBW-CONTACT-PHONE-3,
              :QBW-CONTACT-EXT-3,
              :QBW-CONTACT-RELATION-3,
              :QBW-CONTACT-DESC-4,
              :QBW-CONTACT-PHONE-4,
              :QBW-CONTACT-EXT-4,
              :QBW-CONTACT-RELATION-4,
              :QBW-CONTACT-DESC-5,
              :QBW-CONTACT-PHONE-5,
              :QBW-CONTACT-EXT-5,
              :QBW-CONTACT-RELATION-5,
              :QBW-CONTACT-DESC-6,
              :QBW-CONTACT-PHONE-6,
              :QBW-CONTACT-EXT-6,
              :QBW-CONTACT-RELATION-6,
              :QBW-CONTACT-DESC-7,
              :QBW-CONTACT-PHONE-7,
              :QBW-CONTACT-EXT-7,
              :QBW-CONTACT-RELATION-7,
              :QBW-CONTACT-DESC-8,
              :QBW-CONTACT-PHONE-8,
              :QBW-CONTACT-EXT-8,
              :QBW-CONTACT-RELATION-8,
              :QBW-CONTACT-DESC-9,
              :QBW-CONTACT-PHONE-9,
              :QBW-CONTACT-EXT-9,
              :QBW-CONTACT-RELATION-9,
              :QBW-CONTACT-DESC-10,
              :QBW-CONTACT-PHONE-10,
              :QBW-CONTACT-EXT-10,
              :QBW-CONTACT-RELATION-10,
              :QBW-SRCD,
              :QBW-CALL-ON-SUNDAY,
              :QBW-SOLICIT,
              :QBW-CONFIDENTIAL,
              :QBW-PRIVACY-OPT-OUT,
              :QBW-OPT-IN-EMAIL,
              :QBW-OPT-IN-TEXT,
              :QBW-CITY-TAX-CD,
              :QBW-DEALTRACK-CHG-DATE,
              :QBW-DECEASED,
              :QBW-DECEASED-DATE,
              :QBW-EMAIL,
              :QBW-IDENT-NO,
              :QBW-IDENT-TYPE,
              :QBW-IDENT-STATE,
              :QBW-MILITARY-FLAG,
              :QBW-ORGBR,
              :QBW-CEASE-DESIST-DATE,
              :QBW-CEASE-DESIST-TYPE,
              :QBW-UMC-EXPIRE-DATE,
              :QBW-UMC-PR-EXPIRE-DATE)
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BW1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR. 
           ACCEPT BW-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE BW-PATH-SQL TO E-FILE.

      *    MOVE SPACES         TO BW-VISION-KEY2
      *                           BW-VISION-KEY3
      *                           BW-VISION-KEY4.
      *    STRING BW-OWNBR, BW-LNAME, BW-FNAME
      *           DELIMITED BY SIZE INTO BW-VISION-KEY2.
      *    STRING BW-OWNBR, BW-SPSSNO
      *           DELIMITED BY SIZE INTO BW-VISION-KEY3.
      *    STRING BW-OWNBR, BW-SPLNAME, BW-SPFNAME
      *           DELIMITED BY SIZE INTO BW-VISION-KEY4.
      *    MOVE BW1-KEY        TO E-KEYX BW-VISION-KEY1.

           MOVE BW1-KEY        TO E-KEYX.
           PERFORM SET-BW-FIELDS.

           EXEC SQL
            UPDATE DBO.BWFILE
             SET
              BWFILE.BW_OWNBR = :QBW-OWNBR,
              BWFILE.BW_SSNO = :QBW-SSNO,
              BWFILE.BW_LNAME = :QBW-LNAME,
              BWFILE.BW_FNAME = :QBW-FNAME,
              BWFILE.BW_MNAME = :QBW-MNAME,
              BWFILE.BW_NICKNAME = :QBW-NICKNAME,
              BWFILE.BW_GENERATION = :QBW-GENERATION,
              BWFILE.BW_ADR1 = :QBW-ADR1,
              BWFILE.BW_ADR2 = :QBW-ADR2,
              BWFILE.BW_CITY = :QBW-CITY,
              BWFILE.BW_STATE = :QBW-STATE,
              BWFILE.BW_ZIP = :QBW-ZIP,
              BWFILE.BW_LTOUCH_DATE =
                 :QBW-LTOUCH-DATE,
              BWFILE.BW_REFDATE =
                 :QBW-REFDATE,
              BWFILE.BW_BDATE =
                 :QBW-BDATE,
              BWFILE.BW_PHONE = :QBW-PHONE,
              BWFILE.BW_CELL_PHONE = :QBW-CELL-PHONE,
              BWFILE.BW_NATIONALITY = :QBW-NATIONALITY,
              BWFILE.BW_GENDER = :QBW-GENDER,
              BWFILE.BW_DEPEND = :QBW-DEPEND,
              BWFILE.BW_RENTCD = :QBW-RENTCD,
              BWFILE.BW_TIMEADDR = :QBW-TIMEADDR,
              BWFILE.BW_CREDITCD = :QBW-CREDITCD,
              BWFILE.BW_STRTGCD = :QBW-STRTGCD,
              BWFILE.BW_DWELLCD = :QBW-DWELLCD,
              BWFILE.BW_COUNTY = :QBW-COUNTY,
              BWFILE.BW_ORIGIN = :QBW-ORIGIN,
              BWFILE.BW_CENSUSCD = :QBW-CENSUSCD,
              BWFILE.BW_ORGSLSMN = :QBW-ORGSLSMN,
              BWFILE.BW_REPCODE = :QBW-REPCODE,
              BWFILE.BW_SPSSNO = :QBW-SPSSNO,
              BWFILE.BW_SPLNAME = :QBW-SPLNAME,
              BWFILE.BW_SPFNAME = :QBW-SPFNAME,
              BWFILE.BW_SP_BDATE =
                 :QBW-SP-BDATE,
              BWFILE.BW_EMPLOYER = :QBW-EMPLOYER,
              BWFILE.BW_EMPLPOS = :QBW-EMPLPOS,
              BWFILE.BW_EMPPHONE = :QBW-EMPPHONE,
              BWFILE.BW_EMPEXT = :QBW-EMPEXT,
              BWFILE.BW_EMPYEARS = :QBW-EMPYEARS,
              BWFILE.BW_EMPPAY = :QBW-EMPPAY,
              BWFILE.BW_EMPPAYOT = :QBW-EMPPAYOT,
              BWFILE.BW_EMPPAYSR = :QBW-EMPPAYSR,
              BWFILE.BW_EMPADDR1 = :QBW-EMPADDR1,
              BWFILE.BW_EMPADDR2 = :QBW-EMPADDR2,
              BWFILE.BW_EMPCITY = :QBW-EMPCITY,
              BWFILE.BW_EMPSTATE = :QBW-EMPSTATE,
              BWFILE.BW_EMPZIP = :QBW-EMPZIP,
              BWFILE.BW_WORKHRS = :QBW-WORKHRS,
              BWFILE.BW_LOANS = :QBW-LOANS,
              BWFILE.BW_INTYTD = :QBW-INTYTD,
              BWFILE.BW_INTLYR = :QBW-INTLYR,
              BWFILE.BW_LIASMLN = :QBW-LIASMLN,
              BWFILE.BW_LIAOTH = :QBW-LIAOTH,
              BWFILE.BW_LASTPODATE =
                 :QBW-LASTPODATE,
              BWFILE.BW_PRLOANS = :QBW-PRLOANS,
              BWFILE.BW_RE_INTYTD = :QBW-RE-INTYTD,
              BWFILE.BW_RE_INTLYR = :QBW-RE-INTLYR,
              BWFILE.BW_CREDIT_LIMIT = :QBW-CREDIT-LIMIT,
              BWFILE.BW_CREDIT_SCORE = :QBW-CREDIT-SCORE,
              BWFILE.BW_CRED_SCORE_1 = :QBW-CRED-SCORE-1,
              BWFILE.BW_CRED_SCORE_2 = :QBW-CRED-SCORE-2,
              BWFILE.BW_CRED_SCORE_3 = :QBW-CRED-SCORE-3,
              BWFILE.BW_CRED_SCORE_4 = :QBW-CRED-SCORE-4,
              BWFILE.BW_BNKRPT_SCORE_1 = :QBW-BNKRPT-SCORE-1,
              BWFILE.BW_BNKRPT_SCORE_2 = :QBW-BNKRPT-SCORE-2,
              BWFILE.BW_BNKRPT_SCORE_3 = :QBW-BNKRPT-SCORE-3,
              BWFILE.BW_BNKRPT_SCORE_4 = :QBW-BNKRPT-SCORE-4,
              BWFILE.BW_LAST_FORM_NO = :QBW-LAST-FORM-NO,
              BWFILE.BW_LAST_FORM_DATE =
                 :QBW-LAST-FORM-DATE,
              BWFILE.BW_PRIOR_RN_FOR10_DATE =
                 :QBW-PRIOR-RN-FOR10-DATE,
              BWFILE.BW_LAST_RN_FOR10_DATE =
                 :QBW-LAST-RN-FOR10-DATE,
              BWFILE.BW_ADR_PHONE_CHG_DATE =
                 :QBW-ADR-PHONE-CHG-DATE,
              BWFILE.BW_BAD_ADR_FG = :QBW-BAD-ADR-FG,
              BWFILE.BW_BAD_PHONE_FG = :QBW-BAD-PHONE-FG,
              BWFILE.BW_BAD_EMPPHONE_FG = :QBW-BAD-EMPPHONE-FG,
              BWFILE.BW_BAD_MISC_PHONE1_FG =
                   :QBW-BAD-MISC-PHONE1-FG,
              BWFILE.BW_BAD_MISC_PHONE2_FG =
                   :QBW-BAD-MISC-PHONE2-FG,
              BWFILE.BW_BAD_CELL_FG = :QBW-BAD-CELL-FG,
              BWFILE.BW_MISC_PHONE_1 = :QBW-MISC-PHONE-1,
              BWFILE.BW_MISC_EXT_1 = :QBW-MISC-EXT-1,
              BWFILE.BW_MISC_PHONE_CD_1 = :QBW-MISC-PHONE-CD-1,
              BWFILE.BW_MISC_PHONE_2 = :QBW-MISC-PHONE-2,
              BWFILE.BW_MISC_EXT_2 = :QBW-MISC-EXT-2,
              BWFILE.BW_MISC_PHONE_CD_2 = :QBW-MISC-PHONE-CD-2,
              BWFILE.BW_MISC_PHONE_3 = :QBW-MISC-PHONE-3,
              BWFILE.BW_MISC_EXT_3 = :QBW-MISC-EXT-3,
              BWFILE.BW_MISC_PHONE_CD_3 = :QBW-MISC-PHONE-CD-3,
              BWFILE.BW_MISC_PHONE_4 = :QBW-MISC-PHONE-4,
              BWFILE.BW_MISC_EXT_4 = :QBW-MISC-EXT-4,
              BWFILE.BW_MISC_PHONE_CD_4 = :QBW-MISC-PHONE-CD-4,
              BWFILE.BW_MISC_PHONE_5 = :QBW-MISC-PHONE-5,
              BWFILE.BW_MISC_EXT_5 = :QBW-MISC-EXT-5,
              BWFILE.BW_MISC_PHONE_CD_5 = :QBW-MISC-PHONE-CD-5,
              BWFILE.BW_MISC_PHONE_6 = :QBW-MISC-PHONE-6,
              BWFILE.BW_MISC_EXT_6 = :QBW-MISC-EXT-6,
              BWFILE.BW_MISC_PHONE_CD_6 = :QBW-MISC-PHONE-CD-6,
              BWFILE.BW_CONTACT_DESC_1 = :QBW-CONTACT-DESC-1,
              BWFILE.BW_CONTACT_PHONE_1 = :QBW-CONTACT-PHONE-1,
              BWFILE.BW_CONTACT_EXT_1 = :QBW-CONTACT-EXT-1,
              BWFILE.BW_CONTACT_RELATION_1 =
                   :QBW-CONTACT-RELATION-1,
              BWFILE.BW_CONTACT_DESC_2 = :QBW-CONTACT-DESC-2,
              BWFILE.BW_CONTACT_PHONE_2 = :QBW-CONTACT-PHONE-2,
              BWFILE.BW_CONTACT_EXT_2 = :QBW-CONTACT-EXT-2,
              BWFILE.BW_CONTACT_RELATION_2 =
                   :QBW-CONTACT-RELATION-2,
              BWFILE.BW_CONTACT_DESC_3 = :QBW-CONTACT-DESC-3,
              BWFILE.BW_CONTACT_PHONE_3 = :QBW-CONTACT-PHONE-3,
              BWFILE.BW_CONTACT_EXT_3 = :QBW-CONTACT-EXT-3,
              BWFILE.BW_CONTACT_RELATION_3 =
                   :QBW-CONTACT-RELATION-3,
              BWFILE.BW_CONTACT_DESC_4 = :QBW-CONTACT-DESC-4,
              BWFILE.BW_CONTACT_PHONE_4 = :QBW-CONTACT-PHONE-4,
              BWFILE.BW_CONTACT_EXT_4 = :QBW-CONTACT-EXT-4,
              BWFILE.BW_CONTACT_RELATION_4 =
                   :QBW-CONTACT-RELATION-4,
              BWFILE.BW_CONTACT_DESC_5 = :QBW-CONTACT-DESC-5,
              BWFILE.BW_CONTACT_PHONE_5 = :QBW-CONTACT-PHONE-5,
              BWFILE.BW_CONTACT_EXT_5 = :QBW-CONTACT-EXT-5,
              BWFILE.BW_CONTACT_RELATION_5 =
                   :QBW-CONTACT-RELATION-5,
              BWFILE.BW_CONTACT_DESC_6 = :QBW-CONTACT-DESC-6,
              BWFILE.BW_CONTACT_PHONE_6 = :QBW-CONTACT-PHONE-6,
              BWFILE.BW_CONTACT_EXT_6 = :QBW-CONTACT-EXT-6,
              BWFILE.BW_CONTACT_RELATION_6 =
                   :QBW-CONTACT-RELATION-6,
              BWFILE.BW_CONTACT_DESC_7 = :QBW-CONTACT-DESC-7,
              BWFILE.BW_CONTACT_PHONE_7 = :QBW-CONTACT-PHONE-7,
              BWFILE.BW_CONTACT_EXT_7 = :QBW-CONTACT-EXT-7,
              BWFILE.BW_CONTACT_RELATION_7 =
                   :QBW-CONTACT-RELATION-7,
              BWFILE.BW_CONTACT_DESC_8 = :QBW-CONTACT-DESC-8,
              BWFILE.BW_CONTACT_PHONE_8 = :QBW-CONTACT-PHONE-8,
              BWFILE.BW_CONTACT_EXT_8 = :QBW-CONTACT-EXT-8,
              BWFILE.BW_CONTACT_RELATION_8 =
                   :QBW-CONTACT-RELATION-8,
              BWFILE.BW_CONTACT_DESC_9 = :QBW-CONTACT-DESC-9,
              BWFILE.BW_CONTACT_PHONE_9 = :QBW-CONTACT-PHONE-9,
              BWFILE.BW_CONTACT_EXT_9 = :QBW-CONTACT-EXT-9,
              BWFILE.BW_CONTACT_RELATION_9 =
                   :QBW-CONTACT-RELATION-9,
              BWFILE.BW_CONTACT_DESC_10 = :QBW-CONTACT-DESC-10,
              BWFILE.BW_CONTACT_PHONE_10 = :QBW-CONTACT-PHONE-10,
              BWFILE.BW_CONTACT_EXT_10 = :QBW-CONTACT-EXT-10,
              BWFILE.BW_CONTACT_RELATION_10 =
                   :QBW-CONTACT-RELATION-10,
              BWFILE.BW_SRCD = :QBW-SRCD,
              BWFILE.BW_CALL_ON_SUNDAY = :QBW-CALL-ON-SUNDAY,
              BWFILE.BW_SOLICIT = :QBW-SOLICIT,
              BWFILE.BW_CONFIDENTIAL = :QBW-CONFIDENTIAL,
              BWFILE.BW_PRIVACY_OPT_OUT = :QBW-PRIVACY-OPT-OUT,
              BWFILE.BW_OPT_IN_EMAIL = :QBW-OPT-IN-EMAIL,
              BWFILE.BW_OPT_IN_TEXT = :QBW-OPT-IN-TEXT,
              BWFILE.BW_CITY_TAX_CD = :QBW-CITY-TAX-CD,
              BWFILE.BW_DEALTRACK_CHG_DATE =
                 :QBW-DEALTRACK-CHG-DATE,
              BWFILE.BW_DECEASED = :QBW-DECEASED,
              BWFILE.BW_DECEASED_DATE = :QBW-DECEASED-DATE,
              BWFILE.BW_EMAIL = :QBW-EMAIL,
              BWFILE.BW_IDENT_NO = :QBW-IDENT-NO,
              BWFILE.BW_IDENT_TYPE = :QBW-IDENT-TYPE,
              BWFILE.BW_IDENT_STATE = :QBW-IDENT-STATE,
              BWFILE.BW_MILITARY_FLAG = :QBW-MILITARY-FLAG,
              BWFILE.BW_ORGBR = :QBW-ORGBR,
              BWFILE.BW_CEASE_DESIST_DATE = :QBW-CEASE-DESIST-DATE,
              BWFILE.BW_CEASE_DESIST_TYPE = :QBW-CEASE-DESIST-TYPE,
              BWFILE.BW_UMC_EXPIRE_DATE = :QBW-UMC-EXPIRE-DATE,
              BWFILE.BW_UMC_PR_EXPIRE_DATE = :QBW-UMC-PR-EXPIRE-DATE
            WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
               AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BW1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR. 
           PERFORM DELETE-IT.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           PERFORM SET-BW-FIELDS.

           EXEC SQL
            DELETE FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
               AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BW1-FILE.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW1-CURSOR-STAT.

