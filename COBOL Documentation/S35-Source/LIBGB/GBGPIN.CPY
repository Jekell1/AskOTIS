      * COPYMEMBER: LIBGB/GBGPIN
      * REV:
      * 2023.1023 ADDED GP-CREATE-PAY-XTRP-XNO #1629
       LOAD-GP-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GP-ALL-MACHINE
              MOVE GP-ALL-PATH             TO GP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GP-ALL-BASE
              MOVE FILPATH-MACHINE         TO GP-ALL-MACHINE
              MOVE GP-ALL-PATH             TO GP-PATH.

      *-----------------------------------------------------------------
       OPEN-GP-FILE.
           PERFORM LOAD-GP-FILE.
           PERFORM OPEN-IT.
           MOVE GP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GP-FILE.
           PERFORM START-IT.
           MOVE GP-PATH-SQL  TO E-FILE.
           MOVE GP-KEY       TO E-KEYX.

           IF ( GP-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GP-FILE.

           IF ( QGP-WEND-KEY = ZEROES    ) OR
              ( QGP-WEND-KEY NOT NUMERIC )
              MOVE GP-KEY              TO QGP-WBEG-KEY
              MOVE 99                  TO QGP-WEND-KEY.

           EXEC SQL
            DECLARE GP_CURSOR CURSOR FOR
            SELECT GPFILE.GP_KEY,
                   CAST(GPFILE.GP_LTOUCH_DATE AS VARCHAR(10)),
                   CAST(GPFILE.GP_BEG_FISCAL_DATE AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_1 AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_2 AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_3 AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_4 AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_5 AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_6 AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_7 AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_8 AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_9 AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_10 AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_11 AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_12 AS VARCHAR(10)),
                   GPFILE.GP_BANK_GL,
                   GPFILE.GP_BANK_CLEARING_GL,
                   GPFILE.GP_BANK_ROUTING_NO,
                   GPFILE.GP_BANK_ACCTNO,
                   GPFILE.GP_TR_RETAIN_MONTHS,
                   GPFILE.GP_FED_TAX_ID,
                   GPFILE.GP_CORP_NAME,
                   GPFILE.GP_SHORT_NAME,
                   GPFILE.GP_BANK_NAME,
                   GPFILE.GP_BANK_DEPOSIT_GL,
                   GPFILE.GP_AGE_BUCKET_CD,
                   GPFILE.GP_THE_ONLY_SF_SRCD,
                   GPFILE.GP_ALLOW_BATCH_CHECKS,
                   GPFILE.GP_DLRVPRIN_3_FG,
                   GPFILE.GP_DLRVCHGS_3_FG,
                   GPFILE.GP_CASH_METHOD_ONLY,
                   GPFILE.GP_IB_ACCRUALS_DONE,
                   GPFILE.GP_EOD_RETAIN_MONTHS,
                   GPFILE.GP_NO_LC_IN_ARREARAGE_FG,
                   GPFILE.GP_DEFAULT_O3_INS_COMPANY,
                   GPFILE.GP_STOP_ON_PRIOR_FG,
                   GPFILE.GP_APPLICATION_FG,
                   GPFILE.GP_DEFAULT_ACCRATE,
                   GPFILE.GP_DLR_DISP_TYPE,
                   GPFILE.GP_ALLOW_RLOGINS,
                   GPFILE.GP_AUTO_REPO_WINDOW,
                   GPFILE.GP_ALLOW_PI_WITH_SE_BAL,
                   GPFILE.GP_ALLOW_MORE_COLLECT,
                   GPFILE.GP_BATCH_MORE_THAN_DAY,
                   GPFILE.GP_PRINT_GL_RECAP,
                   GPFILE.GP_LOAN_CONVERSION_UPD_FG,
                   GPFILE.GP_FORM_CHG_CD,
                   GPFILE.GP_PROGRP_PRINT_REDEEM,
                   GPFILE.GP_BW_HOUSING_CODE,
                   GPFILE.GP_BW_CREDIT_SCORE,
                   GPFILE.GP_DISPLAY_BANK_BAL,
                   GPFILE.GP_PROGRP_DL_CONT,
                   GPFILE.GP_DLRVPRIN_2_FG,
                   GPFILE.GP_DLRVPRIN_1_FG,
                   GPFILE.GP_ADV_TO_1ST_NO_CORR,
                   GPFILE.GP_COMPANY_FLAG,
                   GPFILE.GP_INTERBR_AR_AP_FULLGL,
                   GPFILE.GP_SKIP_BI_UPDATE,
                   GPFILE.GP_ASK_HARD_COPY_MEMOS,
                   GPFILE.GP_RLOGIN_LOCAL_PROFILE,
                   GPFILE.GP_RLOGIN_REMOTE_PROFILE,
                   GPFILE.GP_REPODATE_PAYDATE,
                   GPFILE.GP_POST_INTO_FUTURE,
                   GPFILE.GP_RENEWAL_COLLATERAL,
                   GPFILE.GP_PROGRP_LN_CLASS_1,
                   GPFILE.GP_PROGRP_LN_CLASS_2,
                   GPFILE.GP_PROGRP_LN_CLASS_3,
                   GPFILE.GP_PROGRP_SF_CLASS_1,
                   GPFILE.GP_PROGRP_SF_CLASS_2,
                   GPFILE.GP_PROGRP_SF_CLASS_3,
                   GPFILE.GP_PROGRP_SF_CLASS_4,
                   GPFILE.GP_RENEW_DISC_UNERN,
                   GPFILE.GP_NOTES_EOD_BATCH_ID,
                   GPFILE.GP_NEXT_NOTE,
                   GPFILE.GP_NEXT_BATCH_ID,
                   GPFILE.GP_REFNO_VERIFY,
                   GPFILE.GP_ACTIONCD_VERIFY,
                   GPFILE.GP_BP_NEXT_BATCH,
                   GPFILE.GP_IB_ACCRUE_EARN_ON_REPO,
                   GPFILE.GP_DELINQ_WITH_O2,
                   GPFILE.GP_ALLOW_REPO_ON_PL,
                   GPFILE.GP_BK_PRIOR_TO_POSTING,
                   GPFILE.GP_AMT_CASH_PROCEEDS,
                   GPFILE.GP_INCL_OTH_IN_EARNED,
                   GPFILE.GP_VALID_BDATE,
                   GPFILE.GP_ARCHIVE_DIRECTORY,
                   GPFILE.GP_DL_MEMO_TOGGLE,
                   GPFILE.GP_RN_FOR10_LN,
                   GPFILE.GP_LI_APPLICATION_FG,
                   GPFILE.GP_OT_INS_TERM,
                   GPFILE.GP_UNEARNED_STATS,
                   GPFILE.GP_PDTH_GT_SCHED_PAY,
                   GPFILE.GP_LB_NEXT_BATCH,
                   GPFILE.GP_RECDEL_REVERSAL_FG,
                   GPFILE.GP_REF_PHONE,
                   GPFILE.GP_ALLOW_PL_ON_SX,
                   GPFILE.GP_USE_CONRATE,
                   GPFILE.GP_CPCORP_CURBAL,
                   GPFILE.GP_TIME_ZONE_Q,
                   GPFILE.GP_TIME_ZONE_FACTOR,
                   GPFILE.GP_TIME_ZONE_DLS,
                   GPFILE.GP_CALL_START_TIME,
                   GPFILE.GP_CALL_END_TIME,
                   GPFILE.GP_RV_MEMO,
                   GPFILE.GP_PENALTY_INT,
                   GPFILE.GP_ALLOW_SET_FOR_NONCASH,
                   GPFILE.GP_IGNORE_LCHG_NXDUE,
                   GPFILE.GP_ALLOW_ENTRY_TRADEIN,
                   GPFILE.GP_TRUE_NEXT_BATCH,
                   GPFILE.GP_REPO_CODE,
                   GPFILE.GP_ALLOW_MORTGAGE_INT_Z,
                   GPFILE.GP_PROM_AMT_MIN_PCT,
                   GPFILE.GP_BW_PRIVACY_OPT_OUT,
                   GPFILE.GP_NO_GL_ON_O2_WHEN_PL,
                   GPFILE.GP_INTERIM_COUPONS,
                   GPFILE.GP_MGRAM_NEXT_BATCH,
                   GPFILE.GP_RESCHEDULE_MEMO,
                   GPFILE.GP_DONT_COLL_INT_ON_SS,
                   GPFILE.GP_DL_ACH_FG,
                   GPFILE.GP_GL_NEW_JE,
                   GPFILE.GP_CLEAR_PROM_PDTH,
                   GPFILE.GP_CREATE_TP_FORMS,
                   GPFILE.GP_CHECK_RECONCILIATION,
                   GPFILE.GP_CORP_BRNO,
                   GPFILE.GP_PL_REPO_REASON,
                   GPFILE.GP_ALLOW_MIP_INSURANCE,
                   GPFILE.GP_ENTER_LPO_FG,
                   GPFILE.GP_DLCOMM_CMSI_FG,
                   GPFILE.GP_REFCD_NEXT_BATCH,
                   GPFILE.GP_EARLIEST_CLOSE_TIME,
                   GPFILE.GP_GROUP_APPROVAL_FG,
                   GPFILE.GP_APPROVAL_MEMO,
                   GPFILE.GP_FORCE_INPUT_PP_COVERAGE,
                   GPFILE.GP_NO_SC_USE_DM,
                   GPFILE.GP_DEFAULT_O3_INSOURS,
                   GPFILE.GP_USE_BW_BAD_FLAGS,
                   GPFILE.GP_NON_FILE_FEECODE,
                   GPFILE.GP_CHBORR_PAST_DUE_DAYS,
                   GPFILE.GP_CLOSED_FISCAL_FG_1,
                   GPFILE.GP_CLOSED_FISCAL_FG_2,
                   GPFILE.GP_CLOSED_FISCAL_FG_3,
                   GPFILE.GP_CLOSED_FISCAL_FG_4,
                   GPFILE.GP_CLOSED_FISCAL_FG_5,
                   GPFILE.GP_CLOSED_FISCAL_FG_6,
                   GPFILE.GP_CLOSED_FISCAL_FG_7,
                   GPFILE.GP_CLOSED_FISCAL_FG_8,
                   GPFILE.GP_CLOSED_FISCAL_FG_9,
                   GPFILE.GP_CLOSED_FISCAL_FG_10,
                   GPFILE.GP_CLOSED_FISCAL_FG_11,
                   GPFILE.GP_CLOSED_FISCAL_FG_12,
                   GPFILE.GP_BACKDATE_MEMO,
                   GPFILE.GP_OTREF_NEXT_BATCH,
                   GPFILE.GP_NEW_REPO_UNEARNED_ACCTS,
                   GPFILE.GP_BK_FLAG_CONFIDENTIAL,
                   GPFILE.GP_AMORTIZE_ACQUIST_FEE,
                   GPFILE.GP_DENY_FORM_NO_ADDRESS,
                   GPFILE.GP_HIDE_FIRST_5_SSNO,
                   GPFILE.GP_DEFERMENT_APPROVAL,
                   GPFILE.GP_INCLUDE_O2_IN_BAL,
                   GPFILE.GP_LOAN_ACH_PROCESSING,
                   GPFILE.GP_PAYEE_PROCESSING_FG,
                   GPFILE.GP_AUTO_BANKRUPT_WINDOW,
                   GPFILE.GP_BW_SCORES_QUARTER_SUB,
                   GPFILE.GP_POST_XX_BF_PAYMENTS,
                   GPFILE.GP_VALIDATE_ENDORSER_AGE,
                   GPFILE.GP_ALLOW_MANUAL_CHECKS,
                   GPFILE.GP_EXPANDED_FOLLOW_UP,
                   GPFILE.GP_NEXT_EMPLOYEE_NO,
                   GPFILE.GP_USE_RESULT_WORKED_FG,
                   GPFILE.GP_MAX_PROMISE_DAYS,
                   GPFILE.GP_PURGE_TO_ARCHIVE_FG,
                   GPFILE.GP_EDIT_BORROWER_NAME_FG,
                   GPFILE.GP_RECAP_CLASS_TOTALS,
                   GPFILE.GP_BRING_AUTO_INS_ON_RENEWAL,
                   GPFILE.GP_ALLOW_MF_ON_IB,
                   GPFILE.GP_USE_MIN_FORM_SENT_DAYS,
                   GPFILE.GP_DISPLAY_BAD_ADR_FLAG,
                   GPFILE.GP_DAYS_IDENT_NEW_PAPER,
                   GPFILE.GP_ESCROW_FG,
                   GPFILE.GP_SECURITY_FG,
                   GPFILE.GP_GPS_INFORMATION,
                   GPFILE.GP_CUST_INFO_FG,
                   GPFILE.GP_USE_ADDRESS_BW_SCAN,
                   GPFILE.GP_EXPANDED_LEGAL_WINDOW,
                   GPFILE.GP_USE_SCRA,
                   GPFILE.GP_USE_ACCRUAL_EARN_FILE,
                   GPFILE.GP_MESSAGE_ATTR,
                   GPFILE.GP_JUDGMENT_WINDOW,
                   GPFILE.GP_DUE_29_30_31,
                   GPFILE.GP_DEFAULT_SKIP_TRW,
                   GPFILE.GP_EARLIEST_OPEN_TIME,
                   GPFILE.GP_TRANSFER_CONPROFILE,
                   GPFILE.GP_BATCH_REFCD_DUP_DAYS,
                   GPFILE.GP_CREATE_PAY_XTRP_XNO
            FROM DBO.GPFILE
            WHERE GPFILE.GP_KEY >= :QGP-WBEG-KEY
             AND GPFILE.GP_KEY <= :QGP-WEND-KEY
            ORDER BY GPFILE.GP_KEY
           END-EXEC.

           EXEC SQL
               OPEN GP_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE 0 TO QGP-WBEG-KEY
                     QGP-WEND-KEY.

           MOVE STAT---GOOD TO GP-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GP-FILE.
           PERFORM READ-IT.
           MOVE GP-PATH-SQL TO E-FILE.
           MOVE GP-KEY      TO E-KEYX.
           INITIALIZE QGP-REC.

           MOVE GP-KEY      TO QGP-KEY. 

           EXEC SQL
            SELECT GPFILE.GP_KEY,
                   CAST(GPFILE.GP_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_BEG_FISCAL_DATE
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_1
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_2
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_3
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_4
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_5
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_6
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_7
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_8
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_9
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_10
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_11
                                                  AS VARCHAR(10)),
                   CAST(GPFILE.GP_END_FISCAL_DATE_12
                                                  AS VARCHAR(10)),
                   GPFILE.GP_BANK_GL,
                   GPFILE.GP_BANK_CLEARING_GL,
                   GPFILE.GP_BANK_ROUTING_NO,
                   GPFILE.GP_BANK_ACCTNO,
                   GPFILE.GP_TR_RETAIN_MONTHS,
                   GPFILE.GP_FED_TAX_ID,
                   GPFILE.GP_CORP_NAME,
                   GPFILE.GP_SHORT_NAME,
                   GPFILE.GP_BANK_NAME,
                   GPFILE.GP_BANK_DEPOSIT_GL,
                   GPFILE.GP_AGE_BUCKET_CD,
                   GPFILE.GP_THE_ONLY_SF_SRCD,
                   GPFILE.GP_ALLOW_BATCH_CHECKS,
                   GPFILE.GP_DLRVPRIN_3_FG,
                   GPFILE.GP_DLRVCHGS_3_FG,
                   GPFILE.GP_CASH_METHOD_ONLY,
                   GPFILE.GP_IB_ACCRUALS_DONE,
                   GPFILE.GP_EOD_RETAIN_MONTHS,
                   GPFILE.GP_NO_LC_IN_ARREARAGE_FG,
                   GPFILE.GP_DEFAULT_O3_INS_COMPANY,
                   GPFILE.GP_STOP_ON_PRIOR_FG,
                   GPFILE.GP_APPLICATION_FG,
                   GPFILE.GP_DEFAULT_ACCRATE,
                   GPFILE.GP_DLR_DISP_TYPE,
                   GPFILE.GP_ALLOW_RLOGINS,
                   GPFILE.GP_AUTO_REPO_WINDOW,
                   GPFILE.GP_ALLOW_PI_WITH_SE_BAL,
                   GPFILE.GP_ALLOW_MORE_COLLECT,
                   GPFILE.GP_BATCH_MORE_THAN_DAY,
                   GPFILE.GP_PRINT_GL_RECAP,
                   GPFILE.GP_LOAN_CONVERSION_UPD_FG,
                   GPFILE.GP_FORM_CHG_CD,
                   GPFILE.GP_PROGRP_PRINT_REDEEM,
                   GPFILE.GP_BW_HOUSING_CODE,
                   GPFILE.GP_BW_CREDIT_SCORE,
                   GPFILE.GP_DISPLAY_BANK_BAL,
                   GPFILE.GP_PROGRP_DL_CONT,
                   GPFILE.GP_DLRVPRIN_2_FG,
                   GPFILE.GP_DLRVPRIN_1_FG,
                   GPFILE.GP_ADV_TO_1ST_NO_CORR,
                   GPFILE.GP_COMPANY_FLAG,
                   GPFILE.GP_INTERBR_AR_AP_FULLGL,
                   GPFILE.GP_SKIP_BI_UPDATE,
                   GPFILE.GP_ASK_HARD_COPY_MEMOS,
                   GPFILE.GP_RLOGIN_LOCAL_PROFILE,
                   GPFILE.GP_RLOGIN_REMOTE_PROFILE,
                   GPFILE.GP_REPODATE_PAYDATE,
                   GPFILE.GP_POST_INTO_FUTURE,
                   GPFILE.GP_RENEWAL_COLLATERAL,
                   GPFILE.GP_PROGRP_LN_CLASS_1,
                   GPFILE.GP_PROGRP_LN_CLASS_2,
                   GPFILE.GP_PROGRP_LN_CLASS_3,
                   GPFILE.GP_PROGRP_SF_CLASS_1,
                   GPFILE.GP_PROGRP_SF_CLASS_2,
                   GPFILE.GP_PROGRP_SF_CLASS_3,
                   GPFILE.GP_PROGRP_SF_CLASS_4,
                   GPFILE.GP_RENEW_DISC_UNERN,
                   GPFILE.GP_NOTES_EOD_BATCH_ID,
                   GPFILE.GP_NEXT_NOTE,
                   GPFILE.GP_NEXT_BATCH_ID,
                   GPFILE.GP_REFNO_VERIFY,
                   GPFILE.GP_ACTIONCD_VERIFY,
                   GPFILE.GP_BP_NEXT_BATCH,
                   GPFILE.GP_IB_ACCRUE_EARN_ON_REPO,
                   GPFILE.GP_DELINQ_WITH_O2,
                   GPFILE.GP_ALLOW_REPO_ON_PL,
                   GPFILE.GP_BK_PRIOR_TO_POSTING,
                   GPFILE.GP_AMT_CASH_PROCEEDS,
                   GPFILE.GP_INCL_OTH_IN_EARNED,
                   GPFILE.GP_VALID_BDATE,
                   GPFILE.GP_ARCHIVE_DIRECTORY,
                   GPFILE.GP_DL_MEMO_TOGGLE,
                   GPFILE.GP_RN_FOR10_LN,
                   GPFILE.GP_LI_APPLICATION_FG,
                   GPFILE.GP_OT_INS_TERM,
                   GPFILE.GP_UNEARNED_STATS,
                   GPFILE.GP_PDTH_GT_SCHED_PAY,
                   GPFILE.GP_LB_NEXT_BATCH,
                   GPFILE.GP_RECDEL_REVERSAL_FG,
                   GPFILE.GP_REF_PHONE,
                   GPFILE.GP_ALLOW_PL_ON_SX,
                   GPFILE.GP_USE_CONRATE,
                   GPFILE.GP_CPCORP_CURBAL,
                   GPFILE.GP_TIME_ZONE_Q,
                   GPFILE.GP_TIME_ZONE_FACTOR,
                   GPFILE.GP_TIME_ZONE_DLS,
                   GPFILE.GP_CALL_START_TIME,
                   GPFILE.GP_CALL_END_TIME,
                   GPFILE.GP_RV_MEMO,
                   GPFILE.GP_PENALTY_INT,
                   GPFILE.GP_ALLOW_SET_FOR_NONCASH,
                   GPFILE.GP_IGNORE_LCHG_NXDUE,
                   GPFILE.GP_ALLOW_ENTRY_TRADEIN,
                   GPFILE.GP_TRUE_NEXT_BATCH,
                   GPFILE.GP_REPO_CODE,
                   GPFILE.GP_ALLOW_MORTGAGE_INT_Z,
                   GPFILE.GP_PROM_AMT_MIN_PCT,
                   GPFILE.GP_BW_PRIVACY_OPT_OUT,
                   GPFILE.GP_NO_GL_ON_O2_WHEN_PL,
                   GPFILE.GP_INTERIM_COUPONS,
                   GPFILE.GP_MGRAM_NEXT_BATCH,
                   GPFILE.GP_RESCHEDULE_MEMO,
                   GPFILE.GP_DONT_COLL_INT_ON_SS,
                   GPFILE.GP_DL_ACH_FG,
                   GPFILE.GP_GL_NEW_JE,
                   GPFILE.GP_CLEAR_PROM_PDTH,
                   GPFILE.GP_CREATE_TP_FORMS,
                   GPFILE.GP_CHECK_RECONCILIATION,
                   GPFILE.GP_CORP_BRNO,
                   GPFILE.GP_PL_REPO_REASON,
                   GPFILE.GP_ALLOW_MIP_INSURANCE,
                   GPFILE.GP_ENTER_LPO_FG,
                   GPFILE.GP_DLCOMM_CMSI_FG,
                   GPFILE.GP_REFCD_NEXT_BATCH,
                   GPFILE.GP_EARLIEST_CLOSE_TIME,
                   GPFILE.GP_GROUP_APPROVAL_FG,
                   GPFILE.GP_APPROVAL_MEMO,
                   GPFILE.GP_FORCE_INPUT_PP_COVERAGE,
                   GPFILE.GP_NO_SC_USE_DM,
                   GPFILE.GP_DEFAULT_O3_INSOURS,
                   GPFILE.GP_USE_BW_BAD_FLAGS,
                   GPFILE.GP_NON_FILE_FEECODE,
                   GPFILE.GP_CHBORR_PAST_DUE_DAYS,
                   GPFILE.GP_CLOSED_FISCAL_FG_1,
                   GPFILE.GP_CLOSED_FISCAL_FG_2,
                   GPFILE.GP_CLOSED_FISCAL_FG_3,
                   GPFILE.GP_CLOSED_FISCAL_FG_4,
                   GPFILE.GP_CLOSED_FISCAL_FG_5,
                   GPFILE.GP_CLOSED_FISCAL_FG_6,
                   GPFILE.GP_CLOSED_FISCAL_FG_7,
                   GPFILE.GP_CLOSED_FISCAL_FG_8,
                   GPFILE.GP_CLOSED_FISCAL_FG_9,
                   GPFILE.GP_CLOSED_FISCAL_FG_10,
                   GPFILE.GP_CLOSED_FISCAL_FG_11,
                   GPFILE.GP_CLOSED_FISCAL_FG_12,
                   GPFILE.GP_BACKDATE_MEMO,
                   GPFILE.GP_OTREF_NEXT_BATCH,
                   GPFILE.GP_NEW_REPO_UNEARNED_ACCTS,
                   GPFILE.GP_BK_FLAG_CONFIDENTIAL,
                   GPFILE.GP_AMORTIZE_ACQUIST_FEE,
                   GPFILE.GP_DENY_FORM_NO_ADDRESS,
                   GPFILE.GP_HIDE_FIRST_5_SSNO,
                   GPFILE.GP_DEFERMENT_APPROVAL,
                   GPFILE.GP_INCLUDE_O2_IN_BAL,
                   GPFILE.GP_LOAN_ACH_PROCESSING,
                   GPFILE.GP_PAYEE_PROCESSING_FG,
                   GPFILE.GP_AUTO_BANKRUPT_WINDOW,
                   GPFILE.GP_BW_SCORES_QUARTER_SUB,
                   GPFILE.GP_POST_XX_BF_PAYMENTS,
                   GPFILE.GP_VALIDATE_ENDORSER_AGE,
                   GPFILE.GP_ALLOW_MANUAL_CHECKS,
                   GPFILE.GP_EXPANDED_FOLLOW_UP,
                   GPFILE.GP_NEXT_EMPLOYEE_NO,
                   GPFILE.GP_USE_RESULT_WORKED_FG,
                   GPFILE.GP_MAX_PROMISE_DAYS,
                   GPFILE.GP_PURGE_TO_ARCHIVE_FG,
                   GPFILE.GP_EDIT_BORROWER_NAME_FG,
                   GPFILE.GP_RECAP_CLASS_TOTALS,
                   GPFILE.GP_BRING_AUTO_INS_ON_RENEWAL,
                   GPFILE.GP_ALLOW_MF_ON_IB,
                   GPFILE.GP_USE_MIN_FORM_SENT_DAYS,
                   GPFILE.GP_DISPLAY_BAD_ADR_FLAG,
                   GPFILE.GP_DAYS_IDENT_NEW_PAPER,
                   GPFILE.GP_ESCROW_FG,
                   GPFILE.GP_SECURITY_FG,
                   GPFILE.GP_GPS_INFORMATION,
                   GPFILE.GP_CUST_INFO_FG,
                   GPFILE.GP_USE_ADDRESS_BW_SCAN,
                   GPFILE.GP_EXPANDED_LEGAL_WINDOW,
                   GPFILE.GP_USE_SCRA,
                   GPFILE.GP_USE_ACCRUAL_EARN_FILE,
                   GPFILE.GP_MESSAGE_ATTR,
                   GPFILE.GP_JUDGMENT_WINDOW,
                   GPFILE.GP_DUE_29_30_31,
                   GPFILE.GP_DEFAULT_SKIP_TRW,
                   GPFILE.GP_EARLIEST_OPEN_TIME,
                   GPFILE.GP_TRANSFER_CONPROFILE,
                   GPFILE.GP_BATCH_REFCD_DUP_DAYS,
                   GPFILE.GP_CREATE_PAY_XTRP_XNO
            INTO :QGP-REC
            FROM DBO.GPFILE
            WHERE GPFILE.GP_KEY = :QGP-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GP-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GP-FIELDS.

      *-----------------------------------------------------------------
       WRITE-GP-FILE.

           ACCEPT GP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GP-PATH-SQL TO E-FILE.
           MOVE GP-KEY TO E-KEYX.
           PERFORM SET-GP-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GPFILE
             (GPFILE.GP_KEY,
              GPFILE.GP_LTOUCH_DATE,
              GPFILE.GP_BEG_FISCAL_DATE,
              GPFILE.GP_END_FISCAL_DATE_1,
              GPFILE.GP_END_FISCAL_DATE_2,
              GPFILE.GP_END_FISCAL_DATE_3,
              GPFILE.GP_END_FISCAL_DATE_4,
              GPFILE.GP_END_FISCAL_DATE_5,
              GPFILE.GP_END_FISCAL_DATE_6,
              GPFILE.GP_END_FISCAL_DATE_7,
              GPFILE.GP_END_FISCAL_DATE_8,
              GPFILE.GP_END_FISCAL_DATE_9,
              GPFILE.GP_END_FISCAL_DATE_10,
              GPFILE.GP_END_FISCAL_DATE_11,
              GPFILE.GP_END_FISCAL_DATE_12,
              GPFILE.GP_BANK_GL,
              GPFILE.GP_BANK_CLEARING_GL,
              GPFILE.GP_BANK_ROUTING_NO,
              GPFILE.GP_BANK_ACCTNO,
              GPFILE.GP_TR_RETAIN_MONTHS,
              GPFILE.GP_FED_TAX_ID,
              GPFILE.GP_CORP_NAME,
              GPFILE.GP_SHORT_NAME,
              GPFILE.GP_BANK_NAME,
              GPFILE.GP_BANK_DEPOSIT_GL,
              GPFILE.GP_AGE_BUCKET_CD,
              GPFILE.GP_THE_ONLY_SF_SRCD,
              GPFILE.GP_ALLOW_BATCH_CHECKS,
              GPFILE.GP_DLRVPRIN_3_FG,
              GPFILE.GP_DLRVCHGS_3_FG,
              GPFILE.GP_CASH_METHOD_ONLY,
              GPFILE.GP_IB_ACCRUALS_DONE,
              GPFILE.GP_EOD_RETAIN_MONTHS,
              GPFILE.GP_NO_LC_IN_ARREARAGE_FG,
              GPFILE.GP_DEFAULT_O3_INS_COMPANY,
              GPFILE.GP_STOP_ON_PRIOR_FG,
              GPFILE.GP_APPLICATION_FG,
              GPFILE.GP_DEFAULT_ACCRATE,
              GPFILE.GP_DLR_DISP_TYPE,
              GPFILE.GP_ALLOW_RLOGINS,
              GPFILE.GP_AUTO_REPO_WINDOW,
              GPFILE.GP_ALLOW_PI_WITH_SE_BAL,
              GPFILE.GP_ALLOW_MORE_COLLECT,
              GPFILE.GP_BATCH_MORE_THAN_DAY,
              GPFILE.GP_PRINT_GL_RECAP,
              GPFILE.GP_LOAN_CONVERSION_UPD_FG,
              GPFILE.GP_FORM_CHG_CD,
              GPFILE.GP_PROGRP_PRINT_REDEEM,
              GPFILE.GP_BW_HOUSING_CODE,
              GPFILE.GP_BW_CREDIT_SCORE,
              GPFILE.GP_DISPLAY_BANK_BAL,
              GPFILE.GP_PROGRP_DL_CONT,
              GPFILE.GP_DLRVPRIN_2_FG,
              GPFILE.GP_DLRVPRIN_1_FG,
              GPFILE.GP_ADV_TO_1ST_NO_CORR,
              GPFILE.GP_COMPANY_FLAG,
              GPFILE.GP_INTERBR_AR_AP_FULLGL,
              GPFILE.GP_SKIP_BI_UPDATE,
              GPFILE.GP_ASK_HARD_COPY_MEMOS,
              GPFILE.GP_RLOGIN_LOCAL_PROFILE,
              GPFILE.GP_RLOGIN_REMOTE_PROFILE,
              GPFILE.GP_REPODATE_PAYDATE,
              GPFILE.GP_POST_INTO_FUTURE,
              GPFILE.GP_RENEWAL_COLLATERAL,
              GPFILE.GP_PROGRP_LN_CLASS_1,
              GPFILE.GP_PROGRP_LN_CLASS_2,
              GPFILE.GP_PROGRP_LN_CLASS_3,
              GPFILE.GP_PROGRP_SF_CLASS_1,
              GPFILE.GP_PROGRP_SF_CLASS_2,
              GPFILE.GP_PROGRP_SF_CLASS_3,
              GPFILE.GP_PROGRP_SF_CLASS_4,
              GPFILE.GP_RENEW_DISC_UNERN,
              GPFILE.GP_NOTES_EOD_BATCH_ID,
              GPFILE.GP_NEXT_NOTE,
              GPFILE.GP_NEXT_BATCH_ID,
              GPFILE.GP_REFNO_VERIFY,
              GPFILE.GP_ACTIONCD_VERIFY,
              GPFILE.GP_BP_NEXT_BATCH,
              GPFILE.GP_IB_ACCRUE_EARN_ON_REPO,
              GPFILE.GP_DELINQ_WITH_O2,
              GPFILE.GP_ALLOW_REPO_ON_PL,
              GPFILE.GP_BK_PRIOR_TO_POSTING,
              GPFILE.GP_AMT_CASH_PROCEEDS,
              GPFILE.GP_INCL_OTH_IN_EARNED,
              GPFILE.GP_VALID_BDATE,
              GPFILE.GP_ARCHIVE_DIRECTORY,
              GPFILE.GP_DL_MEMO_TOGGLE,
              GPFILE.GP_RN_FOR10_LN,
              GPFILE.GP_LI_APPLICATION_FG,
              GPFILE.GP_OT_INS_TERM,
              GPFILE.GP_UNEARNED_STATS,
              GPFILE.GP_PDTH_GT_SCHED_PAY,
              GPFILE.GP_LB_NEXT_BATCH,
              GPFILE.GP_RECDEL_REVERSAL_FG,
              GPFILE.GP_REF_PHONE,
              GPFILE.GP_ALLOW_PL_ON_SX,
              GPFILE.GP_USE_CONRATE,
              GPFILE.GP_CPCORP_CURBAL,
              GPFILE.GP_TIME_ZONE_Q,
              GPFILE.GP_TIME_ZONE_FACTOR,
              GPFILE.GP_TIME_ZONE_DLS,
              GPFILE.GP_CALL_START_TIME,
              GPFILE.GP_CALL_END_TIME,
              GPFILE.GP_RV_MEMO,
              GPFILE.GP_PENALTY_INT,
              GPFILE.GP_ALLOW_SET_FOR_NONCASH,
              GPFILE.GP_IGNORE_LCHG_NXDUE,
              GPFILE.GP_ALLOW_ENTRY_TRADEIN,
              GPFILE.GP_TRUE_NEXT_BATCH,
              GPFILE.GP_REPO_CODE,
              GPFILE.GP_ALLOW_MORTGAGE_INT_Z,
              GPFILE.GP_PROM_AMT_MIN_PCT,
              GPFILE.GP_BW_PRIVACY_OPT_OUT,
              GPFILE.GP_NO_GL_ON_O2_WHEN_PL,
              GPFILE.GP_INTERIM_COUPONS,
              GPFILE.GP_MGRAM_NEXT_BATCH,
              GPFILE.GP_RESCHEDULE_MEMO,
              GPFILE.GP_DONT_COLL_INT_ON_SS,
              GPFILE.GP_DL_ACH_FG,
              GPFILE.GP_GL_NEW_JE,
              GPFILE.GP_CLEAR_PROM_PDTH,
              GPFILE.GP_CREATE_TP_FORMS,
              GPFILE.GP_CHECK_RECONCILIATION,
              GPFILE.GP_CORP_BRNO,
              GPFILE.GP_PL_REPO_REASON,
              GPFILE.GP_ALLOW_MIP_INSURANCE,
              GPFILE.GP_ENTER_LPO_FG,
              GPFILE.GP_DLCOMM_CMSI_FG,
              GPFILE.GP_REFCD_NEXT_BATCH,
              GPFILE.GP_EARLIEST_CLOSE_TIME,
              GPFILE.GP_GROUP_APPROVAL_FG,
              GPFILE.GP_APPROVAL_MEMO,
              GPFILE.GP_FORCE_INPUT_PP_COVERAGE,
              GPFILE.GP_NO_SC_USE_DM,
              GPFILE.GP_DEFAULT_O3_INSOURS,
              GPFILE.GP_USE_BW_BAD_FLAGS,
              GPFILE.GP_NON_FILE_FEECODE,
              GPFILE.GP_CHBORR_PAST_DUE_DAYS,
              GPFILE.GP_CLOSED_FISCAL_FG_1,
              GPFILE.GP_CLOSED_FISCAL_FG_2,
              GPFILE.GP_CLOSED_FISCAL_FG_3,
              GPFILE.GP_CLOSED_FISCAL_FG_4,
              GPFILE.GP_CLOSED_FISCAL_FG_5,
              GPFILE.GP_CLOSED_FISCAL_FG_6,
              GPFILE.GP_CLOSED_FISCAL_FG_7,
              GPFILE.GP_CLOSED_FISCAL_FG_8,
              GPFILE.GP_CLOSED_FISCAL_FG_9,
              GPFILE.GP_CLOSED_FISCAL_FG_10,
              GPFILE.GP_CLOSED_FISCAL_FG_11,
              GPFILE.GP_CLOSED_FISCAL_FG_12,
              GPFILE.GP_BACKDATE_MEMO,
              GPFILE.GP_OTREF_NEXT_BATCH,
              GPFILE.GP_NEW_REPO_UNEARNED_ACCTS,
              GPFILE.GP_BK_FLAG_CONFIDENTIAL,
              GPFILE.GP_AMORTIZE_ACQUIST_FEE,
              GPFILE.GP_DENY_FORM_NO_ADDRESS,
              GPFILE.GP_HIDE_FIRST_5_SSNO,
              GPFILE.GP_DEFERMENT_APPROVAL,
              GPFILE.GP_INCLUDE_O2_IN_BAL,
              GPFILE.GP_LOAN_ACH_PROCESSING,
              GPFILE.GP_PAYEE_PROCESSING_FG,
              GPFILE.GP_AUTO_BANKRUPT_WINDOW,
              GPFILE.GP_BW_SCORES_QUARTER_SUB,
              GPFILE.GP_POST_XX_BF_PAYMENTS,
              GPFILE.GP_VALIDATE_ENDORSER_AGE,
              GPFILE.GP_ALLOW_MANUAL_CHECKS,
              GPFILE.GP_EXPANDED_FOLLOW_UP,
              GPFILE.GP_NEXT_EMPLOYEE_NO,
              GPFILE.GP_USE_RESULT_WORKED_FG,
              GPFILE.GP_MAX_PROMISE_DAYS,
              GPFILE.GP_PURGE_TO_ARCHIVE_FG,
              GPFILE.GP_EDIT_BORROWER_NAME_FG,
              GPFILE.GP_RECAP_CLASS_TOTALS,
              GPFILE.GP_BRING_AUTO_INS_ON_RENEWAL,
              GPFILE.GP_ALLOW_MF_ON_IB,
              GPFILE.GP_USE_MIN_FORM_SENT_DAYS,
              GPFILE.GP_DISPLAY_BAD_ADR_FLAG,
              GPFILE.GP_DAYS_IDENT_NEW_PAPER,
              GPFILE.GP_ESCROW_FG,
              GPFILE.GP_SECURITY_FG,
              GPFILE.GP_GPS_INFORMATION,
              GPFILE.GP_CUST_INFO_FG,
              GPFILE.GP_USE_ADDRESS_BW_SCAN,
              GPFILE.GP_EXPANDED_LEGAL_WINDOW,
              GPFILE.GP_USE_SCRA,
              GPFILE.GP_USE_ACCRUAL_EARN_FILE,
              GPFILE.GP_MESSAGE_ATTR,
              GPFILE.GP_JUDGMENT_WINDOW,
              GPFILE.GP_DUE_29_30_31,
              GPFILE.GP_DEFAULT_SKIP_TRW,
              GPFILE.GP_EARLIEST_OPEN_TIME,
              GPFILE.GP_TRANSFER_CONPROFILE,
              GPFILE.GP_BATCH_REFCD_DUP_DAYS,
              GPFILE.GP_CREATE_PAY_XTRP_XNO)
            VALUES
             (:QGP-KEY,
              :QGP-LTOUCH-DATE,
              :QGP-BEG-FISCAL-DATE,
              :QGP-END-FISCAL-DATE-1,
              :QGP-END-FISCAL-DATE-2,
              :QGP-END-FISCAL-DATE-3,
              :QGP-END-FISCAL-DATE-4,
              :QGP-END-FISCAL-DATE-5,
              :QGP-END-FISCAL-DATE-6,
              :QGP-END-FISCAL-DATE-7,
              :QGP-END-FISCAL-DATE-8,
              :QGP-END-FISCAL-DATE-9,
              :QGP-END-FISCAL-DATE-10,
              :QGP-END-FISCAL-DATE-11,
              :QGP-END-FISCAL-DATE-12,
              :QGP-BANK-GL,
              :QGP-BANK-CLEARING-GL,
              :QGP-BANK-ROUTING-NO,
              :QGP-BANK-ACCTNO,
              :QGP-TR-RETAIN-MONTHS,
              :QGP-FED-TAX-ID,
              :QGP-CORP-NAME,
              :QGP-SHORT-NAME,
              :QGP-BANK-NAME,
              :QGP-BANK-DEPOSIT-GL,
              :QGP-AGE-BUCKET-CD,
              :QGP-THE-ONLY-SF-SRCD,
              :QGP-ALLOW-BATCH-CHECKS,
              :QGP-DLRVPRIN-3-FG,
              :QGP-DLRVCHGS-3-FG,
              :QGP-CASH-METHOD-ONLY,
              :QGP-IB-ACCRUALS-DONE,
              :QGP-EOD-RETAIN-MONTHS,
              :QGP-NO-LC-IN-ARREARAGE-FG,
              :QGP-DEFAULT-O3-INS-COMPANY,
              :QGP-STOP-ON-PRIOR-FG,
              :QGP-APPLICATION-FG,
              :QGP-DEFAULT-ACCRATE,
              :QGP-DLR-DISP-TYPE,
              :QGP-ALLOW-RLOGINS,
              :QGP-AUTO-REPO-WINDOW,
              :QGP-ALLOW-PI-WITH-SE-BAL,
              :QGP-ALLOW-MORE-COLLECT,
              :QGP-BATCH-MORE-THAN-DAY,
              :QGP-PRINT-GL-RECAP,
              :QGP-LOAN-CONVERSION-UPD-FG,
              :QGP-FORM-CHG-CD,
              :QGP-PROGRP-PRINT-REDEEM,
              :QGP-BW-HOUSING-CODE,
              :QGP-BW-CREDIT-SCORE,
              :QGP-DISPLAY-BANK-BAL,
              :QGP-PROGRP-DL-CONT,
              :QGP-DLRVPRIN-2-FG,
              :QGP-DLRVPRIN-1-FG,
              :QGP-ADV-TO-1ST-NO-CORR,
              :QGP-COMPANY-FLAG,
              :QGP-INTERBR-AR-AP-FULLGL,
              :QGP-SKIP-BI-UPDATE,
              :QGP-ASK-HARD-COPY-MEMOS,
              :QGP-RLOGIN-LOCAL-PROFILE,
              :QGP-RLOGIN-REMOTE-PROFILE,
              :QGP-REPODATE-PAYDATE,
              :QGP-POST-INTO-FUTURE,
              :QGP-RENEWAL-COLLATERAL,
              :QGP-PROGRP-LN-CLASS-1,
              :QGP-PROGRP-LN-CLASS-2,
              :QGP-PROGRP-LN-CLASS-3,
              :QGP-PROGRP-SF-CLASS-1,
              :QGP-PROGRP-SF-CLASS-2,
              :QGP-PROGRP-SF-CLASS-3,
              :QGP-PROGRP-SF-CLASS-4,
              :QGP-RENEW-DISC-UNERN,
              :QGP-NOTES-EOD-BATCH-ID,
              :QGP-NEXT-NOTE,
              :QGP-NEXT-BATCH-ID,
              :QGP-REFNO-VERIFY,
              :QGP-ACTIONCD-VERIFY,
              :QGP-BP-NEXT-BATCH,
              :QGP-IB-ACCRUE-EARN-ON-REPO,
              :QGP-DELINQ-WITH-O2,
              :QGP-ALLOW-REPO-ON-PL,
              :QGP-BK-PRIOR-TO-POSTING,
              :QGP-AMT-CASH-PROCEEDS,
              :QGP-INCL-OTH-IN-EARNED,
              :QGP-VALID-BDATE,
              :QGP-ARCHIVE-DIRECTORY,
              :QGP-DL-MEMO-TOGGLE,
              :QGP-RN-FOR10-LN,
              :QGP-LI-APPLICATION-FG,
              :QGP-OT-INS-TERM,
              :QGP-UNEARNED-STATS,
              :QGP-PDTH-GT-SCHED-PAY,
              :QGP-LB-NEXT-BATCH,
              :QGP-RECDEL-REVERSAL-FG,
              :QGP-REF-PHONE,
              :QGP-ALLOW-PL-ON-SX,
              :QGP-USE-CONRATE,
              :QGP-CPCORP-CURBAL,
              :QGP-TIME-ZONE-Q,
              :QGP-TIME-ZONE-FACTOR,
              :QGP-TIME-ZONE-DLS,
              :QGP-CALL-START-TIME,
              :QGP-CALL-END-TIME,
              :QGP-RV-MEMO,
              :QGP-PENALTY-INT,
              :QGP-ALLOW-SET-FOR-NONCASH,
              :QGP-IGNORE-LCHG-NXDUE,
              :QGP-ALLOW-ENTRY-TRADEIN,
              :QGP-TRUE-NEXT-BATCH,
              :QGP-REPO-CODE,
              :QGP-ALLOW-MORTGAGE-INT-Z,
              :QGP-PROM-AMT-MIN-PCT,
              :QGP-BW-PRIVACY-OPT-OUT,
              :QGP-NO-GL-ON-O2-WHEN-PL,
              :QGP-INTERIM-COUPONS,
              :QGP-MGRAM-NEXT-BATCH,
              :QGP-RESCHEDULE-MEMO,
              :QGP-DONT-COLL-INT-ON-SS,
              :QGP-DL-ACH-FG,
              :QGP-GL-NEW-JE,
              :QGP-CLEAR-PROM-PDTH,
              :QGP-CREATE-TP-FORMS,
              :QGP-CHECK-RECONCILIATION,
              :QGP-CORP-BRNO,
              :QGP-PL-REPO-REASON,
              :QGP-ALLOW-MIP-INSURANCE,
              :QGP-ENTER-LPO-FG,
              :QGP-DLCOMM-CMSI-FG,
              :QGP-REFCD-NEXT-BATCH,
              :QGP-EARLIEST-CLOSE-TIME,
              :QGP-GROUP-APPROVAL-FG,
              :QGP-APPROVAL-MEMO,
              :QGP-FORCE-INPUT-PP-COVERAGE,
              :QGP-NO-SC-USE-DM,
              :QGP-DEFAULT-O3-INSOURS,
              :QGP-USE-BW-BAD-FLAGS,
              :QGP-NON-FILE-FEECODE,
              :QGP-CHBORR-PAST-DUE-DAYS,
              :QGP-CLOSED-FISCAL-FG-1,
              :QGP-CLOSED-FISCAL-FG-2,
              :QGP-CLOSED-FISCAL-FG-3,
              :QGP-CLOSED-FISCAL-FG-4,
              :QGP-CLOSED-FISCAL-FG-5,
              :QGP-CLOSED-FISCAL-FG-6,
              :QGP-CLOSED-FISCAL-FG-7,
              :QGP-CLOSED-FISCAL-FG-8,
              :QGP-CLOSED-FISCAL-FG-9,
              :QGP-CLOSED-FISCAL-FG-10,
              :QGP-CLOSED-FISCAL-FG-11,
              :QGP-CLOSED-FISCAL-FG-12,
              :QGP-BACKDATE-MEMO,
              :QGP-OTREF-NEXT-BATCH,
              :QGP-NEW-REPO-UNEARNED-ACCTS,
              :QGP-BK-FLAG-CONFIDENTIAL,
              :QGP-AMORTIZE-ACQUIST-FEE,
              :QGP-DENY-FORM-NO-ADDRESS,
              :QGP-HIDE-FIRST-5-SSNO,
              :QGP-DEFERMENT-APPROVAL,
              :QGP-INCLUDE-O2-IN-BAL,
              :QGP-LOAN-ACH-PROCESSING,
              :QGP-PAYEE-PROCESSING-FG,
              :QGP-AUTO-BANKRUPT-WINDOW,
              :QGP-BW-SCORES-QUARTER-SUB,
              :QGP-POST-XX-BF-PAYMENTS,
              :QGP-VALIDATE-ENDORSER-AGE,
              :QGP-ALLOW-MANUAL-CHECKS,
              :QGP-EXPANDED-FOLLOW-UP,
              :QGP-NEXT-EMPLOYEE-NO,
              :QGP-USE-RESULT-WORKED-FG,
              :QGP-MAX-PROMISE-DAYS,
              :QGP-PURGE-TO-ARCHIVE-FG,
              :QGP-EDIT-BORROWER-NAME-FG,
              :QGP-RECAP-CLASS-TOTALS,
              :QGP-BRING-AUTO-INS-ON-RENEWAL,
              :QGP-ALLOW-MF-ON-IB,
              :QGP-USE-MIN-FORM-SENT-DAYS,
              :QGP-DISPLAY-BAD-ADR-FLAG,
              :QGP-DAYS-IDENT-NEW-PAPER,
              :QGP-ESCROW-FG,
              :QGP-SECURITY-FG,
              :QGP-GPS-INFORMATION,
              :QGP-CUST-INFO-FG,
              :QGP-USE-ADDRESS-BW-SCAN,
              :QGP-EXPANDED-LEGAL-WINDOW,
              :QGP-USE-SCRA,
              :QGP-USE-ACCRUAL-EARN-FILE,
              :QGP-MESSAGE-ATTR,
              :QGP-JUDGMENT-WINDOW,
              :QGP-DUE-29-30-31,
              :QGP-DEFAULT-SKIP-TRW,
              :QGP-EARLIEST-OPEN-TIME,
              :QGP-TRANSFER-CONPROFILE,
              :QGP-BATCH-REFCD-DUP-DAYS,
              :QGP-CREATE-PAY-XTRP-XNO)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GP-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GP-FILE.

           ACCEPT GP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GP-PATH-SQL TO E-FILE.
           MOVE GP-KEY TO E-KEYX.
           PERFORM SET-GP-FIELDS.

           EXEC SQL
            UPDATE DBO.GPFILE
             SET
              GPFILE.GP_KEY = :QGP-KEY,
              GPFILE.GP_LTOUCH_DATE =
                 :QGP-LTOUCH-DATE,
              GPFILE.GP_BEG_FISCAL_DATE =
                 :QGP-BEG-FISCAL-DATE,
              GPFILE.GP_END_FISCAL_DATE_1 =
                 :QGP-END-FISCAL-DATE-1,
              GPFILE.GP_END_FISCAL_DATE_2 =
                 :QGP-END-FISCAL-DATE-2,
              GPFILE.GP_END_FISCAL_DATE_3 =
                 :QGP-END-FISCAL-DATE-3,
              GPFILE.GP_END_FISCAL_DATE_4 =
                 :QGP-END-FISCAL-DATE-4,
              GPFILE.GP_END_FISCAL_DATE_5 =
                 :QGP-END-FISCAL-DATE-5,
              GPFILE.GP_END_FISCAL_DATE_6 =
                 :QGP-END-FISCAL-DATE-6,
              GPFILE.GP_END_FISCAL_DATE_7 =
                 :QGP-END-FISCAL-DATE-7,
              GPFILE.GP_END_FISCAL_DATE_8 =
                 :QGP-END-FISCAL-DATE-8,
              GPFILE.GP_END_FISCAL_DATE_9 =
                 :QGP-END-FISCAL-DATE-9,
              GPFILE.GP_END_FISCAL_DATE_10 =
                 :QGP-END-FISCAL-DATE-10,
              GPFILE.GP_END_FISCAL_DATE_11 =
                 :QGP-END-FISCAL-DATE-11,
              GPFILE.GP_END_FISCAL_DATE_12 =
                 :QGP-END-FISCAL-DATE-12,
              GPFILE.GP_BANK_GL = :QGP-BANK-GL,
              GPFILE.GP_BANK_CLEARING_GL = :QGP-BANK-CLEARING-GL,
              GPFILE.GP_BANK_ROUTING_NO = :QGP-BANK-ROUTING-NO,
              GPFILE.GP_BANK_ACCTNO = :QGP-BANK-ACCTNO,
              GPFILE.GP_TR_RETAIN_MONTHS = :QGP-TR-RETAIN-MONTHS,
              GPFILE.GP_FED_TAX_ID = :QGP-FED-TAX-ID,
              GPFILE.GP_CORP_NAME = :QGP-CORP-NAME,
              GPFILE.GP_SHORT_NAME = :QGP-SHORT-NAME,
              GPFILE.GP_BANK_NAME = :QGP-BANK-NAME,
              GPFILE.GP_BANK_DEPOSIT_GL = :QGP-BANK-DEPOSIT-GL,
              GPFILE.GP_AGE_BUCKET_CD = :QGP-AGE-BUCKET-CD,
              GPFILE.GP_THE_ONLY_SF_SRCD = :QGP-THE-ONLY-SF-SRCD,
              GPFILE.GP_ALLOW_BATCH_CHECKS =
                   :QGP-ALLOW-BATCH-CHECKS,
              GPFILE.GP_DLRVPRIN_3_FG = :QGP-DLRVPRIN-3-FG,
              GPFILE.GP_DLRVCHGS_3_FG = :QGP-DLRVCHGS-3-FG,
              GPFILE.GP_CASH_METHOD_ONLY = :QGP-CASH-METHOD-ONLY,
              GPFILE.GP_IB_ACCRUALS_DONE = :QGP-IB-ACCRUALS-DONE,
              GPFILE.GP_EOD_RETAIN_MONTHS =
                   :QGP-EOD-RETAIN-MONTHS,
              GPFILE.GP_NO_LC_IN_ARREARAGE_FG =
                   :QGP-NO-LC-IN-ARREARAGE-FG,
              GPFILE.GP_DEFAULT_O3_INS_COMPANY =
                   :QGP-DEFAULT-O3-INS-COMPANY,
              GPFILE.GP_STOP_ON_PRIOR_FG = :QGP-STOP-ON-PRIOR-FG,
              GPFILE.GP_APPLICATION_FG = :QGP-APPLICATION-FG,
              GPFILE.GP_DEFAULT_ACCRATE = :QGP-DEFAULT-ACCRATE,
              GPFILE.GP_DLR_DISP_TYPE = :QGP-DLR-DISP-TYPE,
              GPFILE.GP_ALLOW_RLOGINS = :QGP-ALLOW-RLOGINS,
              GPFILE.GP_AUTO_REPO_WINDOW = :QGP-AUTO-REPO-WINDOW,
              GPFILE.GP_ALLOW_PI_WITH_SE_BAL =
                   :QGP-ALLOW-PI-WITH-SE-BAL,
              GPFILE.GP_ALLOW_MORE_COLLECT =
                   :QGP-ALLOW-MORE-COLLECT,
              GPFILE.GP_BATCH_MORE_THAN_DAY =
                   :QGP-BATCH-MORE-THAN-DAY,
              GPFILE.GP_PRINT_GL_RECAP = :QGP-PRINT-GL-RECAP,
              GPFILE.GP_LOAN_CONVERSION_UPD_FG =
                   :QGP-LOAN-CONVERSION-UPD-FG,
              GPFILE.GP_FORM_CHG_CD = :QGP-FORM-CHG-CD,
              GPFILE.GP_PROGRP_PRINT_REDEEM =
                   :QGP-PROGRP-PRINT-REDEEM,
              GPFILE.GP_BW_HOUSING_CODE = :QGP-BW-HOUSING-CODE,
              GPFILE.GP_BW_CREDIT_SCORE = :QGP-BW-CREDIT-SCORE,
              GPFILE.GP_DISPLAY_BANK_BAL = :QGP-DISPLAY-BANK-BAL,
              GPFILE.GP_PROGRP_DL_CONT = :QGP-PROGRP-DL-CONT,
              GPFILE.GP_DLRVPRIN_2_FG = :QGP-DLRVPRIN-2-FG,
              GPFILE.GP_DLRVPRIN_1_FG = :QGP-DLRVPRIN-1-FG,
              GPFILE.GP_ADV_TO_1ST_NO_CORR =
                   :QGP-ADV-TO-1ST-NO-CORR,
              GPFILE.GP_COMPANY_FLAG = :QGP-COMPANY-FLAG,
              GPFILE.GP_INTERBR_AR_AP_FULLGL =
                   :QGP-INTERBR-AR-AP-FULLGL,
              GPFILE.GP_SKIP_BI_UPDATE = :QGP-SKIP-BI-UPDATE,
              GPFILE.GP_ASK_HARD_COPY_MEMOS =
                   :QGP-ASK-HARD-COPY-MEMOS,
              GPFILE.GP_RLOGIN_LOCAL_PROFILE =
                   :QGP-RLOGIN-LOCAL-PROFILE,
              GPFILE.GP_RLOGIN_REMOTE_PROFILE =
                   :QGP-RLOGIN-REMOTE-PROFILE,
              GPFILE.GP_REPODATE_PAYDATE = :QGP-REPODATE-PAYDATE,
              GPFILE.GP_POST_INTO_FUTURE = :QGP-POST-INTO-FUTURE,
              GPFILE.GP_RENEWAL_COLLATERAL =
                   :QGP-RENEWAL-COLLATERAL,
              GPFILE.GP_PROGRP_LN_CLASS_1 =
                   :QGP-PROGRP-LN-CLASS-1,
              GPFILE.GP_PROGRP_LN_CLASS_2 =
                   :QGP-PROGRP-LN-CLASS-2,
              GPFILE.GP_PROGRP_LN_CLASS_3 =
                   :QGP-PROGRP-LN-CLASS-3,
              GPFILE.GP_PROGRP_SF_CLASS_1 =
                   :QGP-PROGRP-SF-CLASS-1,
              GPFILE.GP_PROGRP_SF_CLASS_2 =
                   :QGP-PROGRP-SF-CLASS-2,
              GPFILE.GP_PROGRP_SF_CLASS_3 =
                   :QGP-PROGRP-SF-CLASS-3,
              GPFILE.GP_PROGRP_SF_CLASS_4 =
                   :QGP-PROGRP-SF-CLASS-4,
              GPFILE.GP_RENEW_DISC_UNERN = :QGP-RENEW-DISC-UNERN,
              GPFILE.GP_NOTES_EOD_BATCH_ID =
                   :QGP-NOTES-EOD-BATCH-ID,
              GPFILE.GP_NEXT_NOTE = :QGP-NEXT-NOTE,
              GPFILE.GP_NEXT_BATCH_ID = :QGP-NEXT-BATCH-ID,
              GPFILE.GP_REFNO_VERIFY = :QGP-REFNO-VERIFY,
              GPFILE.GP_ACTIONCD_VERIFY = :QGP-ACTIONCD-VERIFY,
              GPFILE.GP_BP_NEXT_BATCH = :QGP-BP-NEXT-BATCH,
              GPFILE.GP_IB_ACCRUE_EARN_ON_REPO =
                   :QGP-IB-ACCRUE-EARN-ON-REPO,
              GPFILE.GP_DELINQ_WITH_O2 = :QGP-DELINQ-WITH-O2,
              GPFILE.GP_ALLOW_REPO_ON_PL = :QGP-ALLOW-REPO-ON-PL,
              GPFILE.GP_BK_PRIOR_TO_POSTING =
                   :QGP-BK-PRIOR-TO-POSTING,
              GPFILE.GP_AMT_CASH_PROCEEDS =
                   :QGP-AMT-CASH-PROCEEDS,
              GPFILE.GP_INCL_OTH_IN_EARNED =
                   :QGP-INCL-OTH-IN-EARNED,
              GPFILE.GP_VALID_BDATE = :QGP-VALID-BDATE,
              GPFILE.GP_ARCHIVE_DIRECTORY =
                   :QGP-ARCHIVE-DIRECTORY,
              GPFILE.GP_DL_MEMO_TOGGLE = :QGP-DL-MEMO-TOGGLE,
              GPFILE.GP_RN_FOR10_LN = :QGP-RN-FOR10-LN,
              GPFILE.GP_LI_APPLICATION_FG =
                   :QGP-LI-APPLICATION-FG,
              GPFILE.GP_OT_INS_TERM = :QGP-OT-INS-TERM,
              GPFILE.GP_UNEARNED_STATS = :QGP-UNEARNED-STATS,
              GPFILE.GP_PDTH_GT_SCHED_PAY =
                   :QGP-PDTH-GT-SCHED-PAY,
              GPFILE.GP_LB_NEXT_BATCH = :QGP-LB-NEXT-BATCH,
              GPFILE.GP_RECDEL_REVERSAL_FG =
                   :QGP-RECDEL-REVERSAL-FG,
              GPFILE.GP_REF_PHONE = :QGP-REF-PHONE,
              GPFILE.GP_ALLOW_PL_ON_SX = :QGP-ALLOW-PL-ON-SX,
              GPFILE.GP_USE_CONRATE = :QGP-USE-CONRATE,
              GPFILE.GP_CPCORP_CURBAL = :QGP-CPCORP-CURBAL,
              GPFILE.GP_TIME_ZONE_Q = :QGP-TIME-ZONE-Q,
              GPFILE.GP_TIME_ZONE_FACTOR = :QGP-TIME-ZONE-FACTOR,
              GPFILE.GP_TIME_ZONE_DLS = :QGP-TIME-ZONE-DLS,
              GPFILE.GP_CALL_START_TIME = :QGP-CALL-START-TIME,
              GPFILE.GP_CALL_END_TIME = :QGP-CALL-END-TIME,
              GPFILE.GP_RV_MEMO = :QGP-RV-MEMO,
              GPFILE.GP_PENALTY_INT = :QGP-PENALTY-INT,
              GPFILE.GP_ALLOW_SET_FOR_NONCASH =
                   :QGP-ALLOW-SET-FOR-NONCASH,
              GPFILE.GP_IGNORE_LCHG_NXDUE =
                   :QGP-IGNORE-LCHG-NXDUE,
              GPFILE.GP_ALLOW_ENTRY_TRADEIN =
                   :QGP-ALLOW-ENTRY-TRADEIN,
              GPFILE.GP_TRUE_NEXT_BATCH = :QGP-TRUE-NEXT-BATCH,
              GPFILE.GP_REPO_CODE = :QGP-REPO-CODE,
              GPFILE.GP_ALLOW_MORTGAGE_INT_Z =
                   :QGP-ALLOW-MORTGAGE-INT-Z,
              GPFILE.GP_PROM_AMT_MIN_PCT = :QGP-PROM-AMT-MIN-PCT,
              GPFILE.GP_BW_PRIVACY_OPT_OUT =
                   :QGP-BW-PRIVACY-OPT-OUT,
              GPFILE.GP_NO_GL_ON_O2_WHEN_PL =
                   :QGP-NO-GL-ON-O2-WHEN-PL,
              GPFILE.GP_INTERIM_COUPONS = :QGP-INTERIM-COUPONS,
              GPFILE.GP_MGRAM_NEXT_BATCH = :QGP-MGRAM-NEXT-BATCH,
              GPFILE.GP_RESCHEDULE_MEMO = :QGP-RESCHEDULE-MEMO,
              GPFILE.GP_DONT_COLL_INT_ON_SS =
                   :QGP-DONT-COLL-INT-ON-SS,
              GPFILE.GP_DL_ACH_FG = :QGP-DL-ACH-FG,
              GPFILE.GP_GL_NEW_JE = :QGP-GL-NEW-JE,
              GPFILE.GP_CLEAR_PROM_PDTH = :QGP-CLEAR-PROM-PDTH,
              GPFILE.GP_CREATE_TP_FORMS = :QGP-CREATE-TP-FORMS,
              GPFILE.GP_CHECK_RECONCILIATION =
                   :QGP-CHECK-RECONCILIATION,
              GPFILE.GP_CORP_BRNO = :QGP-CORP-BRNO,
              GPFILE.GP_PL_REPO_REASON = :QGP-PL-REPO-REASON,
              GPFILE.GP_ALLOW_MIP_INSURANCE =
                   :QGP-ALLOW-MIP-INSURANCE,
              GPFILE.GP_ENTER_LPO_FG = :QGP-ENTER-LPO-FG,
              GPFILE.GP_DLCOMM_CMSI_FG = :QGP-DLCOMM-CMSI-FG,
              GPFILE.GP_REFCD_NEXT_BATCH = :QGP-REFCD-NEXT-BATCH,
              GPFILE.GP_EARLIEST_CLOSE_TIME =
                   :QGP-EARLIEST-CLOSE-TIME,
              GPFILE.GP_GROUP_APPROVAL_FG =
                   :QGP-GROUP-APPROVAL-FG,
              GPFILE.GP_APPROVAL_MEMO = :QGP-APPROVAL-MEMO,
              GPFILE.GP_FORCE_INPUT_PP_COVERAGE =
                   :QGP-FORCE-INPUT-PP-COVERAGE,
              GPFILE.GP_NO_SC_USE_DM = :QGP-NO-SC-USE-DM,
              GPFILE.GP_DEFAULT_O3_INSOURS =
                   :QGP-DEFAULT-O3-INSOURS,
              GPFILE.GP_USE_BW_BAD_FLAGS = :QGP-USE-BW-BAD-FLAGS,
              GPFILE.GP_NON_FILE_FEECODE = :QGP-NON-FILE-FEECODE,
              GPFILE.GP_CHBORR_PAST_DUE_DAYS =
                   :QGP-CHBORR-PAST-DUE-DAYS,
              GPFILE.GP_CLOSED_FISCAL_FG_1 =
                   :QGP-CLOSED-FISCAL-FG-1,
              GPFILE.GP_CLOSED_FISCAL_FG_2 =
                   :QGP-CLOSED-FISCAL-FG-2,
              GPFILE.GP_CLOSED_FISCAL_FG_3 =
                   :QGP-CLOSED-FISCAL-FG-3,
              GPFILE.GP_CLOSED_FISCAL_FG_4 =
                   :QGP-CLOSED-FISCAL-FG-4,
              GPFILE.GP_CLOSED_FISCAL_FG_5 =
                   :QGP-CLOSED-FISCAL-FG-5,
              GPFILE.GP_CLOSED_FISCAL_FG_6 =
                   :QGP-CLOSED-FISCAL-FG-6,
              GPFILE.GP_CLOSED_FISCAL_FG_7 =
                   :QGP-CLOSED-FISCAL-FG-7,
              GPFILE.GP_CLOSED_FISCAL_FG_8 =
                   :QGP-CLOSED-FISCAL-FG-8,
              GPFILE.GP_CLOSED_FISCAL_FG_9 =
                   :QGP-CLOSED-FISCAL-FG-9,
              GPFILE.GP_CLOSED_FISCAL_FG_10 =
                   :QGP-CLOSED-FISCAL-FG-10,
              GPFILE.GP_CLOSED_FISCAL_FG_11 =
                   :QGP-CLOSED-FISCAL-FG-11,
              GPFILE.GP_CLOSED_FISCAL_FG_12 =
                   :QGP-CLOSED-FISCAL-FG-12,
              GPFILE.GP_BACKDATE_MEMO = :QGP-BACKDATE-MEMO,
              GPFILE.GP_OTREF_NEXT_BATCH = :QGP-OTREF-NEXT-BATCH,
              GPFILE.GP_NEW_REPO_UNEARNED_ACCTS =
                   :QGP-NEW-REPO-UNEARNED-ACCTS,
              GPFILE.GP_BK_FLAG_CONFIDENTIAL =
                   :QGP-BK-FLAG-CONFIDENTIAL,
              GPFILE.GP_AMORTIZE_ACQUIST_FEE =
                   :QGP-AMORTIZE-ACQUIST-FEE,
              GPFILE.GP_DENY_FORM_NO_ADDRESS =
                   :QGP-DENY-FORM-NO-ADDRESS,
              GPFILE.GP_HIDE_FIRST_5_SSNO =
                   :QGP-HIDE-FIRST-5-SSNO,
              GPFILE.GP_DEFERMENT_APPROVAL =
                   :QGP-DEFERMENT-APPROVAL,
              GPFILE.GP_INCLUDE_O2_IN_BAL =
                   :QGP-INCLUDE-O2-IN-BAL,
              GPFILE.GP_LOAN_ACH_PROCESSING =
                   :QGP-LOAN-ACH-PROCESSING,
              GPFILE.GP_PAYEE_PROCESSING_FG =
                   :QGP-PAYEE-PROCESSING-FG,
              GPFILE.GP_AUTO_BANKRUPT_WINDOW =
                   :QGP-AUTO-BANKRUPT-WINDOW,
              GPFILE.GP_BW_SCORES_QUARTER_SUB =
                   :QGP-BW-SCORES-QUARTER-SUB,
              GPFILE.GP_POST_XX_BF_PAYMENTS =
                   :QGP-POST-XX-BF-PAYMENTS,
              GPFILE.GP_VALIDATE_ENDORSER_AGE =
                   :QGP-VALIDATE-ENDORSER-AGE,
              GPFILE.GP_ALLOW_MANUAL_CHECKS =
                   :QGP-ALLOW-MANUAL-CHECKS,
              GPFILE.GP_EXPANDED_FOLLOW_UP =
                   :QGP-EXPANDED-FOLLOW-UP,
              GPFILE.GP_NEXT_EMPLOYEE_NO = :QGP-NEXT-EMPLOYEE-NO,
              GPFILE.GP_USE_RESULT_WORKED_FG =
                   :QGP-USE-RESULT-WORKED-FG,
              GPFILE.GP_MAX_PROMISE_DAYS = :QGP-MAX-PROMISE-DAYS,
              GPFILE.GP_PURGE_TO_ARCHIVE_FG =
                   :QGP-PURGE-TO-ARCHIVE-FG,
              GPFILE.GP_EDIT_BORROWER_NAME_FG =
                   :QGP-EDIT-BORROWER-NAME-FG,
              GPFILE.GP_RECAP_CLASS_TOTALS =
                   :QGP-RECAP-CLASS-TOTALS,
              GPFILE.GP_BRING_AUTO_INS_ON_RENEWAL =
                   :QGP-BRING-AUTO-INS-ON-RENEWAL,
              GPFILE.GP_ALLOW_MF_ON_IB = :QGP-ALLOW-MF-ON-IB,
              GPFILE.GP_USE_MIN_FORM_SENT_DAYS =
                   :QGP-USE-MIN-FORM-SENT-DAYS,
              GPFILE.GP_DISPLAY_BAD_ADR_FLAG =
                   :QGP-DISPLAY-BAD-ADR-FLAG,
              GPFILE.GP_DAYS_IDENT_NEW_PAPER =
                   :QGP-DAYS-IDENT-NEW-PAPER,
              GPFILE.GP_ESCROW_FG = :QGP-ESCROW-FG,
              GPFILE.GP_SECURITY_FG = :QGP-SECURITY-FG,
              GPFILE.GP_GPS_INFORMATION = :QGP-GPS-INFORMATION,
              GPFILE.GP_CUST_INFO_FG = :QGP-CUST-INFO-FG,
              GPFILE.GP_USE_ADDRESS_BW_SCAN =
                   :QGP-USE-ADDRESS-BW-SCAN,
              GPFILE.GP_EXPANDED_LEGAL_WINDOW =
                   :QGP-EXPANDED-LEGAL-WINDOW,
              GPFILE.GP_USE_SCRA = :QGP-USE-SCRA,
              GPFILE.GP_USE_ACCRUAL_EARN_FILE =
                   :QGP-USE-ACCRUAL-EARN-FILE,
              GPFILE.GP_MESSAGE_ATTR = :QGP-MESSAGE-ATTR,
              GPFILE.GP_JUDGMENT_WINDOW = :QGP-JUDGMENT-WINDOW,
              GPFILE.GP_DUE_29_30_31 = :QGP-DUE-29-30-31,
              GPFILE.GP_DEFAULT_SKIP_TRW = :QGP-DEFAULT-SKIP-TRW,
              GPFILE.GP_EARLIEST_OPEN_TIME =
                   :QGP-EARLIEST-OPEN-TIME,
              GPFILE.GP_TRANSFER_CONPROFILE =
                   :QGP-TRANSFER-CONPROFILE,
              GPFILE.GP_BATCH_REFCD_DUP_DAYS =
                   :QGP-BATCH-REFCD-DUP-DAYS,
              GPFILE.GP_CREATE_PAY_XTRP_XNO =
                   :QGP-CREATE-PAY-XTRP-XNO
             WHERE GPFILE.GP_KEY = :QGP-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GP-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GP-FILE.

           PERFORM DELETE-IT.
           MOVE GP-PATH-SQL TO E-FILE.
           MOVE GP-KEY TO E-KEYX.
           PERFORM SET-GP-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GPFILE
             WHERE GPFILE.GP_KEY = :QGP-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GP-FILE.

      *-----------------------------------------------------------------
       CLOSE-GP-FILE.

           PERFORM CLOSE-IT.

           IF ( GP-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GP_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GP-CURSOR-STAT.
 
