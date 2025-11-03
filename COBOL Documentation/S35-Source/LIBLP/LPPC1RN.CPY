      * COPYMEMBER: LIBLP/LPPC1RN
       LOAD-PC1-FILE.
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
       OPEN-PC1-FILE.
           PERFORM LOAD-PC1-FILE.
           PERFORM OPEN-IT.
           MOVE PC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-PC1-FILE.
           PERFORM START-IT.
           MOVE PC-PATH-OWNBR  TO PC-BRANCH.
           MOVE PC-PATH-SQL    TO E-FILE.
           MOVE PC1-KEY        TO E-KEYX.

           IF ( PC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-PC1-FILE.

           IF ( QPC1-WEND-ID = SPACES )
              MOVE PC-PATH-OWNBR        TO QPC1-WBEG-BRANCH
              MOVE PC-PATH-OWNBR        TO QPC1-WEND-BRANCH
              MOVE PC-ID                TO QPC1-WBEG-ID
              MOVE ALL "Z"              TO QPC1-WEND-ID
              MOVE PC-SOC-SEC           TO QPC1-WBEG-SSNO
              MOVE ALL "9"              TO QPC1-WEND-SSNO.


           EXEC SQL
            DECLARE PC1_CURSOR CURSOR FOR
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
            WHERE PCFILE.PC_ID >= :QPC1-WBEG-ID
              AND PCFILE.PC_ID <= :QPC1-WEND-ID
              AND PCFILE.PC_BRANCH  = :QPC1-WBEG-BRANCH
              AND PCFILE.PC_SOC_SEC >= :QPC1-WBEG-SSNO
              AND PCFILE.PC_SOC_SEC <= :QPC1-WEND-SSNO
            ORDER BY PCFILE.PC_ID,
                     PCFILE.PC_BRANCH,
                     PCFILE.PC_SOC_SEC
           END-EXEC.

           EXEC SQL
               OPEN PC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QPC1-WBEG-BRANCH 
                          QPC1-WEND-BRANCH
                          QPC1-WBEG-SSNO
                          QPC1-WEND-SSNO.
 
           MOVE SPACES TO QPC1-WBEG-ID
                          QPC1-WEND-ID.

           MOVE STAT---GOOD TO PC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-PC1-FILE.
           PERFORM READ-IT.
           MOVE PC-PATH-OWNBR  TO PC-BRANCH. 
           MOVE PC-PATH-SQL    TO E-FILE.
           MOVE PC1-KEY        TO E-KEYX.
           INITIALIZE QPC-REC.

           MOVE PC-ID     TO QPC-ID.
           MOVE PC-BRANCH TO QPC-BRANCH.
           MOVE PC-SOC-SEC   TO QPC-SOC-SEC.

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
            WHERE PCFILE.PC_ID = :QPC-ID
              AND PCFILE.PC_BRANCH = :QPC-BRANCH
              AND PCFILE.PC_SOC_SEC = :QPC-SOC-SEC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-PC-FIELDS.

      *-----------------------------------------------------------------
       READ-PC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE PC-PATH-OWNBR  TO PC-BRANCH.
           MOVE PC-PATH-SQL    TO E-FILE.
           MOVE PC1-KEY        TO E-KEYX.
           INITIALIZE QPC-REC.

           IF ( PC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH PC1_CURSOR INTO :QPC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-PC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-PC1-FILE.

           PERFORM CLOSE-IT.
           IF ( PC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE PC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO PC1-CURSOR-STAT.

