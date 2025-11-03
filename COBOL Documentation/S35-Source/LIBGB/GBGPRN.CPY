      * COPYMEMBER: LIBGB/GBGPRN
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
           MOVE GP-KEY TO E-KEYX.
           INITIALIZE QGP-REC.

           MOVE GP-KEY    TO QGP-KEY. 

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
       CLOSE-GP-FILE.
           PERFORM CLOSE-IT.
           IF ( GP-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GP_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GP-CURSOR-STAT.
 
