      * COPYMEMBER: LIBLP/LPPC1IN
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
       WRITE-PC1-FILE.

           MOVE PC-PATH-OWNBR  TO PC-BRANCH.
           ACCEPT PC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE PC-PATH-SQL    TO E-FILE.
           MOVE PC1-KEY        TO E-KEYX.
           PERFORM SET-PC-FIELDS.

           EXEC SQL
            INSERT INTO DBO.PCFILE
             (PCFILE.PC_ID,
              PCFILE.PC_BRANCH,
              PCFILE.PC_SOC_SEC,
              PCFILE.PC_LTC_BRNO,
              PCFILE.PC_LTC_ACCTNO,
              PCFILE.PC_LTC_SEQNO,
              PCFILE.PC_LTC_LTOUCH_DATE,
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
              PCFILE.PC_LTC_VIN_CHG_DATE_1,
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
              PCFILE.PC_LTC_VIN_CHG_DATE_2,
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
              PCFILE.PC_LTC_VIN_CHG_DATE_3,
              PCFILE.PC_LTC_HHGS_1,
              PCFILE.PC_LTC_HHGS_2,
              PCFILE.PC_LTC_HHGS_3,
              PCFILE.PC_LTA_BRNO,
              PCFILE.PC_LTA_ACCTNO,
              PCFILE.PC_LTA_SEQNO,
              PCFILE.PC_LTA_LTOUCH_DATE,
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
              PCFILE.PC_LTA_EFF_DATE,
              PCFILE.PC_LTA_EXP_DATE,
              PCFILE.PC_LTA_CAN_DATE,
              PCFILE.PC_LTA_CONF_DATE,
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
              PCFILE.PC_LOANDATE,
              PCFILE.PC_ORGTERM,
              PCFILE.PC_REGPYAMT,
              PCFILE.PC2_ID,
              PCFILE.PC2_BRANCH,
              PCFILE.PC2_BWNAME,
              PCFILE.PC_LTOUCH_DATE)
            VALUES
             (:QPC-ID,
              :QPC-BRANCH,
              :QPC-SOC-SEC,
              :QPC-LTC-BRNO,
              :QPC-LTC-ACCTNO,
              :QPC-LTC-SEQNO,
              :QPC-LTC-LTOUCH-DATE,
              :QPC-LTC-DESC-1,
              :QPC-LTC-MAKE-1,
              :QPC-LTC-MODEL-1,
              :QPC-LTC-YEAR-1,
              :QPC-LTC-STYLE-1,
              :QPC-LTC-CLASS-1,
              :QPC-LTC-VIN-1,
              :QPC-LTC-TITLE-NO-1,
              :QPC-LTC-MILEAGE-1,
              :QPC-LTC-VALUE-1,
              :QPC-LTC-SELLPR-1,
              :QPC-LTC-MISC-AMT-1,
              :QPC-LTC-SENT-FLAG-1,
              :QPC-LTC-VIN-CHG-DATE-1,
              :QPC-LTC-DESC-2,
              :QPC-LTC-MAKE-2,
              :QPC-LTC-MODEL-2,
              :QPC-LTC-YEAR-2,
              :QPC-LTC-STYLE-2,
              :QPC-LTC-CLASS-2,
              :QPC-LTC-VIN-2,
              :QPC-LTC-TITLE-NO-2,
              :QPC-LTC-MILEAGE-2,
              :QPC-LTC-VALUE-2,
              :QPC-LTC-SELLPR-2,
              :QPC-LTC-MISC-AMT-2,
              :QPC-LTC-SENT-FLAG-2,
              :QPC-LTC-VIN-CHG-DATE-2,
              :QPC-LTC-DESC-3,
              :QPC-LTC-MAKE-3,
              :QPC-LTC-MODEL-3,
              :QPC-LTC-YEAR-3,
              :QPC-LTC-STYLE-3,
              :QPC-LTC-CLASS-3,
              :QPC-LTC-VIN-3,
              :QPC-LTC-TITLE-NO-3,
              :QPC-LTC-MILEAGE-3,
              :QPC-LTC-VALUE-3,
              :QPC-LTC-SELLPR-3,
              :QPC-LTC-MISC-AMT-3,
              :QPC-LTC-SENT-FLAG-3,
              :QPC-LTC-VIN-CHG-DATE-3,
              :QPC-LTC-HHGS-1,
              :QPC-LTC-HHGS-2,
              :QPC-LTC-HHGS-3,
              :QPC-LTA-BRNO,
              :QPC-LTA-ACCTNO,
              :QPC-LTA-SEQNO,
              :QPC-LTA-LTOUCH-DATE,
              :QPC-LTA-DLR-REQ,
              :QPC-LTA-INS-TYPE,
              :QPC-LTA-INS-CO,
              :QPC-LTA-POLICY-NO,
              :QPC-LTA-AGENCY,
              :QPC-LTA-AGENT,
              :QPC-LTA-ADR1,
              :QPC-LTA-ADR2,
              :QPC-LTA-CITY,
              :QPC-LTA-STATE,
              :QPC-LTA-ZIP,
              :QPC-LTA-PHONE,
              :QPC-LTA-EFF-DATE,
              :QPC-LTA-EXP-DATE,
              :QPC-LTA-CAN-DATE,
              :QPC-LTA-CONF-DATE,
              :QPC-LTA-LAST-FORM,
              :QPC-LTA-COV-TYPE,
              :QPC-LTA-BIL-COV,
              :QPC-LTA-PDL-COV,
              :QPC-LTA-PIP-COV,
              :QPC-LTA-UMB-COV,
              :QPC-LTA-COL-COV,
              :QPC-LTA-LOSS-PAYEE,
              :QPC-LTA-COM-COV,
              :QPC-LTA-TRACK-ONLY,
              :QPC-LTA-INSURANCE,
              :QPC-LTA-TRACK-MONTHS,
              :QPC-LTA-INS-MONTHS,
              :QPC-LTA-CPP-MONTHS,
              :QPC-LOANDATE,
              :QPC-ORGTERM,
              :QPC-REGPYAMT,
              :QPC2-ID,
              :QPC2-BRANCH,
              :QPC2-BWNAME,
              :QPC-LTOUCH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-PC1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-PC1-FILE.

           MOVE PC-PATH-OWNBR  TO PC-BRANCH.
           ACCEPT PC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE PC-PATH-SQL    TO E-FILE.
           MOVE PC1-KEY        TO E-KEYX.
           PERFORM SET-PC-FIELDS.

           EXEC SQL
            UPDATE DBO.PCFILE
             SET
              PCFILE.PC_ID = :QPC-ID,
              PCFILE.PC_BRANCH = :QPC-BRANCH,
              PCFILE.PC_SOC_SEC = :QPC-SOC-SEC,
              PCFILE.PC_LTC_BRNO = :QPC-LTC-BRNO,
              PCFILE.PC_LTC_ACCTNO = :QPC-LTC-ACCTNO,
              PCFILE.PC_LTC_SEQNO = :QPC-LTC-SEQNO,
              PCFILE.PC_LTC_LTOUCH_DATE = :QPC-LTC-LTOUCH-DATE,
              PCFILE.PC_LTC_DESC_1 = :QPC-LTC-DESC-1,
              PCFILE.PC_LTC_MAKE_1 = :QPC-LTC-MAKE-1,
              PCFILE.PC_LTC_MODEL_1 = :QPC-LTC-MODEL-1,
              PCFILE.PC_LTC_YEAR_1 = :QPC-LTC-YEAR-1,
              PCFILE.PC_LTC_STYLE_1 = :QPC-LTC-STYLE-1,
              PCFILE.PC_LTC_CLASS_1 = :QPC-LTC-CLASS-1,
              PCFILE.PC_LTC_VIN_1 = :QPC-LTC-VIN-1,
              PCFILE.PC_LTC_TITLE_NO_1 = :QPC-LTC-TITLE-NO-1,
              PCFILE.PC_LTC_MILEAGE_1 = :QPC-LTC-MILEAGE-1,
              PCFILE.PC_LTC_VALUE_1 = :QPC-LTC-VALUE-1,
              PCFILE.PC_LTC_SELLPR_1 = :QPC-LTC-SELLPR-1,
              PCFILE.PC_LTC_MISC_AMT_1 = :QPC-LTC-MISC-AMT-1,
              PCFILE.PC_LTC_SENT_FLAG_1 = :QPC-LTC-SENT-FLAG-1,
              PCFILE.PC_LTC_VIN_CHG_DATE_1 =
                 :QPC-LTC-VIN-CHG-DATE-1,
              PCFILE.PC_LTC_DESC_2 = :QPC-LTC-DESC-2,
              PCFILE.PC_LTC_MAKE_2 = :QPC-LTC-MAKE-2,
              PCFILE.PC_LTC_MODEL_2 = :QPC-LTC-MODEL-2,
              PCFILE.PC_LTC_YEAR_2 = :QPC-LTC-YEAR-2,
              PCFILE.PC_LTC_STYLE_2 = :QPC-LTC-STYLE-2,
              PCFILE.PC_LTC_CLASS_2 = :QPC-LTC-CLASS-2,
              PCFILE.PC_LTC_VIN_2 = :QPC-LTC-VIN-2,
              PCFILE.PC_LTC_TITLE_NO_2 = :QPC-LTC-TITLE-NO-2,
              PCFILE.PC_LTC_MILEAGE_2 = :QPC-LTC-MILEAGE-2,
              PCFILE.PC_LTC_VALUE_2 = :QPC-LTC-VALUE-2,
              PCFILE.PC_LTC_SELLPR_2 = :QPC-LTC-SELLPR-2,
              PCFILE.PC_LTC_MISC_AMT_2 = :QPC-LTC-MISC-AMT-2,
              PCFILE.PC_LTC_SENT_FLAG_2 = :QPC-LTC-SENT-FLAG-2,
              PCFILE.PC_LTC_VIN_CHG_DATE_2 =
                 :QPC-LTC-VIN-CHG-DATE-2,
              PCFILE.PC_LTC_DESC_3 = :QPC-LTC-DESC-3,
              PCFILE.PC_LTC_MAKE_3 = :QPC-LTC-MAKE-3,
              PCFILE.PC_LTC_MODEL_3 = :QPC-LTC-MODEL-3,
              PCFILE.PC_LTC_YEAR_3 = :QPC-LTC-YEAR-3,
              PCFILE.PC_LTC_STYLE_3 = :QPC-LTC-STYLE-3,
              PCFILE.PC_LTC_CLASS_3 = :QPC-LTC-CLASS-3,
              PCFILE.PC_LTC_VIN_3 = :QPC-LTC-VIN-3,
              PCFILE.PC_LTC_TITLE_NO_3 = :QPC-LTC-TITLE-NO-3,
              PCFILE.PC_LTC_MILEAGE_3 = :QPC-LTC-MILEAGE-3,
              PCFILE.PC_LTC_VALUE_3 = :QPC-LTC-VALUE-3,
              PCFILE.PC_LTC_SELLPR_3 = :QPC-LTC-SELLPR-3,
              PCFILE.PC_LTC_MISC_AMT_3 = :QPC-LTC-MISC-AMT-3,
              PCFILE.PC_LTC_SENT_FLAG_3 = :QPC-LTC-SENT-FLAG-3,
              PCFILE.PC_LTC_VIN_CHG_DATE_3 =
                 :QPC-LTC-VIN-CHG-DATE-3,
              PCFILE.PC_LTC_HHGS_1 = :QPC-LTC-HHGS-1,
              PCFILE.PC_LTC_HHGS_2 = :QPC-LTC-HHGS-2,
              PCFILE.PC_LTC_HHGS_3 = :QPC-LTC-HHGS-3,
              PCFILE.PC_LTA_BRNO = :QPC-LTA-BRNO,
              PCFILE.PC_LTA_ACCTNO = :QPC-LTA-ACCTNO,
              PCFILE.PC_LTA_SEQNO = :QPC-LTA-SEQNO,
              PCFILE.PC_LTA_LTOUCH_DATE = :QPC-LTA-LTOUCH-DATE,
              PCFILE.PC_LTA_DLR_REQ = :QPC-LTA-DLR-REQ,
              PCFILE.PC_LTA_INS_TYPE = :QPC-LTA-INS-TYPE,
              PCFILE.PC_LTA_INS_CO = :QPC-LTA-INS-CO,
              PCFILE.PC_LTA_POLICY_NO = :QPC-LTA-POLICY-NO,
              PCFILE.PC_LTA_AGENCY = :QPC-LTA-AGENCY,
              PCFILE.PC_LTA_AGENT = :QPC-LTA-AGENT,
              PCFILE.PC_LTA_ADR1 = :QPC-LTA-ADR1,
              PCFILE.PC_LTA_ADR2 = :QPC-LTA-ADR2,
              PCFILE.PC_LTA_CITY = :QPC-LTA-CITY,
              PCFILE.PC_LTA_STATE = :QPC-LTA-STATE,
              PCFILE.PC_LTA_ZIP = :QPC-LTA-ZIP,
              PCFILE.PC_LTA_PHONE = :QPC-LTA-PHONE,
              PCFILE.PC_LTA_EFF_DATE =
                 :QPC-LTA-EFF-DATE,
              PCFILE.PC_LTA_EXP_DATE =
                 :QPC-LTA-EXP-DATE,
              PCFILE.PC_LTA_CAN_DATE =
                 :QPC-LTA-CAN-DATE,
              PCFILE.PC_LTA_CONF_DATE =
                 :QPC-LTA-CONF-DATE,
              PCFILE.PC_LTA_LAST_FORM = :QPC-LTA-LAST-FORM,
              PCFILE.PC_LTA_COV_TYPE = :QPC-LTA-COV-TYPE,
              PCFILE.PC_LTA_BIL_COV = :QPC-LTA-BIL-COV,
              PCFILE.PC_LTA_PDL_COV = :QPC-LTA-PDL-COV,
              PCFILE.PC_LTA_PIP_COV = :QPC-LTA-PIP-COV,
              PCFILE.PC_LTA_UMB_COV = :QPC-LTA-UMB-COV,
              PCFILE.PC_LTA_COL_COV = :QPC-LTA-COL-COV,
              PCFILE.PC_LTA_LOSS_PAYEE = :QPC-LTA-LOSS-PAYEE,
              PCFILE.PC_LTA_COM_COV = :QPC-LTA-COM-COV,
              PCFILE.PC_LTA_TRACK_ONLY = :QPC-LTA-TRACK-ONLY,
              PCFILE.PC_LTA_INSURANCE = :QPC-LTA-INSURANCE,
              PCFILE.PC_LTA_TRACK_MONTHS = :QPC-LTA-TRACK-MONTHS,
              PCFILE.PC_LTA_INS_MONTHS = :QPC-LTA-INS-MONTHS,
              PCFILE.PC_LTA_CPP_MONTHS = :QPC-LTA-CPP-MONTHS,
              PCFILE.PC_LOANDATE =
                 :QPC-LOANDATE,
              PCFILE.PC_ORGTERM = :QPC-ORGTERM,
              PCFILE.PC_REGPYAMT = :QPC-REGPYAMT,
              PCFILE.PC2_ID = :QPC2-ID,
              PCFILE.PC2_BRANCH = :QPC2-BRANCH,
              PCFILE.PC2_BWNAME = :QPC2-BWNAME,
              PCFILE.PC_LTOUCH_DATE = :QPC-LTOUCH-DATE
            WHERE PCFILE.PC_ID = :QPC-ID
              AND PCFILE.PC_BRANCH = :QPC-BRANCH
              AND PCFILE.PC_SOC_SEC = :QPC-SOC-SEC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-PC1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-PC1-FILE.

           MOVE PC-PATH-OWNBR  TO PC-BRANCH.
           PERFORM DELETE-IT.
           MOVE PC-PATH-SQL    TO E-FILE.
           MOVE PC1-KEY        TO E-KEYX.
           PERFORM SET-PC-FIELDS.

           EXEC SQL
            DELETE FROM DBO.PCFILE
            WHERE PCFILE.PC_ID = :QPC-ID
              AND PCFILE.PC_BRANCH = :QPC-BRANCH
              AND PCFILE.PC_SOC_SEC = :QPC-SOC-SEC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-PC1-FILE.

      *-----------------------------------------------------------------
       CLOSE-PC1-FILE.

           PERFORM CLOSE-IT.

           IF ( PC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE PC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO PC1-CURSOR-STAT.

