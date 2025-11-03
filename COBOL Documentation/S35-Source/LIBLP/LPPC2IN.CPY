      * COPYMEMBER: LIBLP/LPPC2IN
       LOAD-PC2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( PC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( PC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO PC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO PC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO PC-ALL-DATA
              MOVE PC-ALL-PATH             TO PC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( PC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO PC-ALL-BASE
              MOVE FILPATH-MACHINE         TO PC-ALL-MACHINE
              MOVE FILPATH-DATA            TO PC-ALL-DATA
              MOVE PC-ALL-PATH             TO PC-PATH.

      *-----------------------------------------------------------------
       OPEN-PC2-FILE.
           PERFORM LOAD-PC2-FILE.
           PERFORM OPEN-IT.
           MOVE PC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-PC2-FILE.
           PERFORM START-IT.
           MOVE PC-PATH-OWNBR  TO PC2-BRANCH.
           MOVE PC-PATH-SQL    TO E-FILE.
           MOVE PC2-KEY        TO E-KEYX.

           IF ( PC2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-PC2-FILE.

           IF ( QPC2-WEND-ID = SPACES )
              MOVE PC-PATH-OWNBR        TO QPC2-WBEG-BRANCH
              MOVE PC-PATH-OWNBR        TO QPC2-WEND-BRANCH
              MOVE PC2-ID               TO QPC2-WBEG-ID
              MOVE ALL "Z"              TO QPC2-WEND-ID
              MOVE PC2-BWNAME           TO QPC2-WBEG-BWNAME
              MOVE ALL "Z"              TO QPC2-WEND-BWNAME.


           EXEC SQL
            DECLARE PC2_CURSOR CURSOR FOR
            SELECT PCFILE.PC_ID,
                   PCFILE.PC_BRANCH,
                   PCFILE.PC_SOC_SEC,
                   PCFILE.PC_LTC_BRNO,
                   PCFILE.PC_LTC_ACCTNO,
                   PCFILE.PC_LTC_SEQNO,
                   CAST(PCFILE.PC_LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   PCFILE.PC_LTC_DESC_1,
                   PCFILE.PC_LTC_MAKE_1,
                   PCFILE.PC_LTC_MODEL_1,
                   PCFILE.PC_LTC_YEAR_1,
                   PCFILE.PC_LTC_STYLE_1,
                   PCFILE.PC_LTC_CLASS_1,
                   PCFILE.PC_LTC_VIN_1,
                   PCFILE.PC_LTC_TITLE_NO_1,
                   PCFILE.PC_LTC_MILEAGE_1,
                   PCFILE.PC_LTC_VALUE_1,
                   PCFILE.PC_LTC_SELLPR_1,
                   PCFILE.PC_LTC_MISC_AMT_1,
                   PCFILE.PC_LTC_SENT_FLAG_1,
                   CAST(PCFILE.PC_LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),
                   PCFILE.PC_LTC_DESC_2,
                   PCFILE.PC_LTC_MAKE_2,
                   PCFILE.PC_LTC_MODEL_2,
                   PCFILE.PC_LTC_YEAR_2,
                   PCFILE.PC_LTC_STYLE_2,
                   PCFILE.PC_LTC_CLASS_2,
                   PCFILE.PC_LTC_VIN_2,
                   PCFILE.PC_LTC_TITLE_NO_2,
                   PCFILE.PC_LTC_MILEAGE_2,
                   PCFILE.PC_LTC_VALUE_2,
                   PCFILE.PC_LTC_SELLPR_2,
                   PCFILE.PC_LTC_MISC_AMT_2,
                   PCFILE.PC_LTC_SENT_FLAG_2,
                   CAST(PCFILE.PC_LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),
                   PCFILE.PC_LTC_DESC_3,
                   PCFILE.PC_LTC_MAKE_3,
                   PCFILE.PC_LTC_MODEL_3,
                   PCFILE.PC_LTC_YEAR_3,
                   PCFILE.PC_LTC_STYLE_3,
                   PCFILE.PC_LTC_CLASS_3,
                   PCFILE.PC_LTC_VIN_3,
                   PCFILE.PC_LTC_TITLE_NO_3,
                   PCFILE.PC_LTC_MILEAGE_3,
                   PCFILE.PC_LTC_VALUE_3,
                   PCFILE.PC_LTC_SELLPR_3,
                   PCFILE.PC_LTC_MISC_AMT_3,
                   PCFILE.PC_LTC_SENT_FLAG_3,
                   CAST(PCFILE.PC_LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),
                   PCFILE.PC_LTC_HHGS_1,
                   PCFILE.PC_LTC_HHGS_2,
                   PCFILE.PC_LTC_HHGS_3,
                   PCFILE.PC_LTA_BRNO,
                   PCFILE.PC_LTA_ACCTNO,
                   PCFILE.PC_LTA_SEQNO,
                   CAST(PCFILE.PC_LTA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   PCFILE.PC_LTA_DLR_REQ,
                   PCFILE.PC_LTA_INS_TYPE,
                   PCFILE.PC_LTA_INS_CO,
                   PCFILE.PC_LTA_POLICY_NO,
                   PCFILE.PC_LTA_AGENCY,
                   PCFILE.PC_LTA_AGENT,
                   PCFILE.PC_LTA_ADR1,
                   PCFILE.PC_LTA_ADR2,
                   PCFILE.PC_LTA_CITY,
                   PCFILE.PC_LTA_STATE,
                   PCFILE.PC_LTA_ZIP,
                   PCFILE.PC_LTA_PHONE,
                   CAST(PCFILE.PC_LTA_EFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(PCFILE.PC_LTA_EXP_DATE
                                                  AS VARCHAR(10)),
                   CAST(PCFILE.PC_LTA_CAN_DATE
                                                  AS VARCHAR(10)),
                   CAST(PCFILE.PC_LTA_CONF_DATE
                                                  AS VARCHAR(10)),
                   PCFILE.PC_LTA_LAST_FORM,
                   PCFILE.PC_LTA_COV_TYPE,
                   PCFILE.PC_LTA_BIL_COV,
                   PCFILE.PC_LTA_PDL_COV,
                   PCFILE.PC_LTA_PIP_COV,
                   PCFILE.PC_LTA_UMB_COV,
                   PCFILE.PC_LTA_COL_COV,
                   PCFILE.PC_LTA_LOSS_PAYEE,
                   PCFILE.PC_LTA_COM_COV,
                   PCFILE.PC_LTA_TRACK_ONLY,
                   PCFILE.PC_LTA_INSURANCE,
                   PCFILE.PC_LTA_TRACK_MONTHS,
                   PCFILE.PC_LTA_INS_MONTHS,
                   PCFILE.PC_LTA_CPP_MONTHS,
                   CAST(PCFILE.PC_LOANDATE AS VARCHAR(10)),
                   PCFILE.PC_ORGTERM,
                   PCFILE.PC_REGPYAMT,
                   PCFILE.PC2_ID,
                   PCFILE.PC2_BRANCH,
                   PCFILE.PC2_BWNAME,
                   CAST(PCFILE.PC_LTOUCH_DATE
                                                  AS VARCHAR(10))
            FROM DBO.PCFILE
            WHERE PCFILE.PC2_ID >= :QPC2-WBEG-ID
              AND PCFILE.PC2_ID <= :QPC2-WEND-ID
              AND PCFILE.PC2_BRANCH  = :QPC2-WBEG-BRANCH
              AND PCFILE.PC2_BWNAME >= :QPC2-WBEG-BWNAME
              AND PCFILE.PC2_BWNAME <= :QPC2-WEND-BWNAME
            ORDER BY PCFILE.PC2_ID,
                     PCFILE.PC2_BRANCH,
                     PCFILE.PC2_BWNAME
           END-EXEC.

           EXEC SQL
               OPEN PC2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QPC2-WBEG-BRANCH 
                          QPC2-WEND-BRANCH.
 
           MOVE SPACES TO QPC2-WBEG-ID
                          QPC2-WEND-ID
                          QPC2-WBEG-BWNAME
                          QPC2-WEND-BWNAME.

           MOVE STAT---GOOD TO PC2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-PC2-FILE.
           PERFORM READ-IT.
           MOVE PC-PATH-OWNBR  TO PC2-BRANCH. 
           MOVE PC-PATH-SQL    TO E-FILE.
           MOVE PC2-KEY        TO E-KEYX.
           INITIALIZE QPC-REC.

           MOVE PC2-ID     TO QPC2-ID.
           MOVE PC2-BRANCH TO QPC2-BRANCH.
           MOVE PC2-BWNAME TO QPC2-BWNAME.

           EXEC SQL
            SELECT PCFILE.PC_ID,
                   PCFILE.PC_BRANCH,
                   PCFILE.PC_SOC_SEC,
                   PCFILE.PC_LTC_BRNO,
                   PCFILE.PC_LTC_ACCTNO,
                   PCFILE.PC_LTC_SEQNO,
                   CAST(PCFILE.PC_LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   PCFILE.PC_LTC_DESC_1,
                   PCFILE.PC_LTC_MAKE_1,
                   PCFILE.PC_LTC_MODEL_1,
                   PCFILE.PC_LTC_YEAR_1,
                   PCFILE.PC_LTC_STYLE_1,
                   PCFILE.PC_LTC_CLASS_1,
                   PCFILE.PC_LTC_VIN_1,
                   PCFILE.PC_LTC_TITLE_NO_1,
                   PCFILE.PC_LTC_MILEAGE_1,
                   PCFILE.PC_LTC_VALUE_1,
                   PCFILE.PC_LTC_SELLPR_1,
                   PCFILE.PC_LTC_MISC_AMT_1,
                   PCFILE.PC_LTC_SENT_FLAG_1,
                   CAST(PCFILE.PC_LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),
                   PCFILE.PC_LTC_DESC_2,
                   PCFILE.PC_LTC_MAKE_2,
                   PCFILE.PC_LTC_MODEL_2,
                   PCFILE.PC_LTC_YEAR_2,
                   PCFILE.PC_LTC_STYLE_2,
                   PCFILE.PC_LTC_CLASS_2,
                   PCFILE.PC_LTC_VIN_2,
                   PCFILE.PC_LTC_TITLE_NO_2,
                   PCFILE.PC_LTC_MILEAGE_2,
                   PCFILE.PC_LTC_VALUE_2,
                   PCFILE.PC_LTC_SELLPR_2,
                   PCFILE.PC_LTC_MISC_AMT_2,
                   PCFILE.PC_LTC_SENT_FLAG_2,
                   CAST(PCFILE.PC_LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),
                   PCFILE.PC_LTC_DESC_3,
                   PCFILE.PC_LTC_MAKE_3,
                   PCFILE.PC_LTC_MODEL_3,
                   PCFILE.PC_LTC_YEAR_3,
                   PCFILE.PC_LTC_STYLE_3,
                   PCFILE.PC_LTC_CLASS_3,
                   PCFILE.PC_LTC_VIN_3,
                   PCFILE.PC_LTC_TITLE_NO_3,
                   PCFILE.PC_LTC_MILEAGE_3,
                   PCFILE.PC_LTC_VALUE_3,
                   PCFILE.PC_LTC_SELLPR_3,
                   PCFILE.PC_LTC_MISC_AMT_3,
                   PCFILE.PC_LTC_SENT_FLAG_3,
                   CAST(PCFILE.PC_LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),
                   PCFILE.PC_LTC_HHGS_1,
                   PCFILE.PC_LTC_HHGS_2,
                   PCFILE.PC_LTC_HHGS_3,
                   PCFILE.PC_LTA_BRNO,
                   PCFILE.PC_LTA_ACCTNO,
                   PCFILE.PC_LTA_SEQNO,
                   CAST(PCFILE.PC_LTA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   PCFILE.PC_LTA_DLR_REQ,
                   PCFILE.PC_LTA_INS_TYPE,
                   PCFILE.PC_LTA_INS_CO,
                   PCFILE.PC_LTA_POLICY_NO,
                   PCFILE.PC_LTA_AGENCY,
                   PCFILE.PC_LTA_AGENT,
                   PCFILE.PC_LTA_ADR1,
                   PCFILE.PC_LTA_ADR2,
                   PCFILE.PC_LTA_CITY,
                   PCFILE.PC_LTA_STATE,
                   PCFILE.PC_LTA_ZIP,
                   PCFILE.PC_LTA_PHONE,
                   CAST(PCFILE.PC_LTA_EFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(PCFILE.PC_LTA_EXP_DATE
                                                  AS VARCHAR(10)),
                   CAST(PCFILE.PC_LTA_CAN_DATE
                                                  AS VARCHAR(10)),
                   CAST(PCFILE.PC_LTA_CONF_DATE
                                                  AS VARCHAR(10)),
                   PCFILE.PC_LTA_LAST_FORM,
                   PCFILE.PC_LTA_COV_TYPE,
                   PCFILE.PC_LTA_BIL_COV,
                   PCFILE.PC_LTA_PDL_COV,
                   PCFILE.PC_LTA_PIP_COV,
                   PCFILE.PC_LTA_UMB_COV,
                   PCFILE.PC_LTA_COL_COV,
                   PCFILE.PC_LTA_LOSS_PAYEE,
                   PCFILE.PC_LTA_COM_COV,
                   PCFILE.PC_LTA_TRACK_ONLY,
                   PCFILE.PC_LTA_INSURANCE,
                   PCFILE.PC_LTA_TRACK_MONTHS,
                   PCFILE.PC_LTA_INS_MONTHS,
                   PCFILE.PC_LTA_CPP_MONTHS,
                   CAST(PCFILE.PC_LOANDATE AS VARCHAR(10)),
                   PCFILE.PC_ORGTERM,
                   PCFILE.PC_REGPYAMT,
                   PCFILE.PC2_ID,
                   PCFILE.PC2_BRANCH,
                   PCFILE.PC2_BWNAME,
                   CAST(PCFILE.PC_LTOUCH_DATE
                                                  AS VARCHAR(10))
            INTO :QPC-REC
            FROM DBO.PCFILE
            WHERE PCFILE.PC2_ID = :QPC2-ID
              AND PCFILE.PC2_BRANCH = :QPC2-BRANCH
              AND PCFILE.PC2_BWNAME = :QPC2-BWNAME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PC2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-PC-FIELDS.

      *-----------------------------------------------------------------
       READ-PC2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE PC-PATH-OWNBR  TO PC2-BRANCH.
           MOVE PC-PATH-SQL    TO E-FILE.
           MOVE PC2-KEY        TO E-KEYX.
           INITIALIZE QPC-REC.

           IF ( PC2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH PC2_CURSOR INTO :QPC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PC2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-PC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-PC2-FILE.

           PERFORM CLOSE-IT.
           IF ( PC2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE PC2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO PC2-CURSOR-STAT.

