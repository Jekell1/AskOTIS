      * COPYMEMBER: LIBLP/LPBW4RN
       LOAD-BW4-FILE.
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
       OPEN-BW4-FILE.
           PERFORM LOAD-BW4-FILE.
           PERFORM OPEN-IT.
           MOVE BW-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW4-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING BW-OWNBR, BW-SPLNAME, BW-SPFNAME
                  DELIMITED BY SIZE INTO E-KEYX.

           IF ( BW4-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW4-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QBW4-WEND-OWNBR = ZEROES    ) OR
              ( QBW4-WEND-OWNBR NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW4-WBEG-OWNBR
                                           QBW4-WEND-OWNBR
              MOVE BW-SPLNAME           TO QBW4-WBEG-SPLNAME
              MOVE ALL "Z"              TO QBW4-WEND-SPLNAME
              CALL "C$TOLOWER" USING QBW4-WEND-SPLNAME, VALUE 35
              MOVE BW-SPFNAME           TO QBW4-WBEG-SPFNAME
              MOVE ALL "Z"              TO QBW4-WEND-SPFNAME
              CALL "C$TOLOWER" USING QBW4-WEND-SPFNAME, VALUE 35
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
              MOVE 35                   TO SQL-TEXT-SIZE
              MOVE BW-SPLNAME           TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QBW4-WSBEG-SPLNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE BW4_CURSOR CURSOR FOR
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
            WHERE (  BWFILE.BW_OWNBR    = :QBW4-WBEG-OWNBR    )
              AND (( BWFILE.BW_SPLNAME >= :QBW4-WBEG-SPLNAME
              AND    BWFILE.BW_SPFNAME >= :QBW4-WBEG-SPFNAME )
               OR  ( BWFILE.BW_SPLNAME >= :QBW4-WSBEG-SPLNAME
              AND    BWFILE.BW_SPFNAME >= ''                 ))
              AND (  BWFILE.BW_SPLNAME <= :QBW4-WEND-SPLNAME
              AND    BWFILE.BW_SPFNAME <= :QBW4-WEND-SPFNAME  )
            ORDER BY BWFILE.BW_OWNBR,
                     BWFILE.BW_SPLNAME,
                     BWFILE.BW_SPFNAME,
                     BWFILE.BW_SSNO
           END-EXEC.

           EXEC SQL
               OPEN BW4_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW4-WBEG-OWNBR 
                          QBW4-WEND-OWNBR.
           MOVE SPACES TO QBW4-WBEG-SPLNAME
                          QBW4-WBEG-SPFNAME
                          QBW4-WEND-SPLNAME
                          QBW4-WEND-SPFNAME.

           MOVE STAT---GOOD TO BW4-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BW4-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING BW-OWNBR, BW-SPLNAME, BW-SPFNAME
                  DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QBW-REC.

           IF ( BW4-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW4_CURSOR INTO :QBW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW4-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW4-FILE.

           PERFORM CLOSE-IT.
           IF ( BW4-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW4_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW4-CURSOR-STAT.

