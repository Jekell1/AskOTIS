      * COPYMEMBER: LIBLP/LPLSX1RN
       LOAD-LSX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSX-ALL-DATA
              MOVE LSX-ALL-PATH             TO LSX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSX-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSX-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSX-ALL-DATA
              MOVE LSX-ALL-PATH             TO LSX-PATH.

      *-----------------------------------------------------------------
       OPEN-LSX1-FILE.
           PERFORM LOAD-LSX1-FILE.
           PERFORM OPEN-IT.
           MOVE LSX-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSX1-FILE.
           PERFORM START-IT.
           MOVE LSX-PATH-OWNBR  TO LSX-BRNO.
           MOVE LSX-PATH-SQL    TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.

           IF ( LSX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSX1-FILE.

           IF ( QLSX1-WEND-BRNO = ZEROES    ) OR
              ( QLSX1-WEND-BRNO NOT NUMERIC )
              MOVE LSX-PATH-OWNBR        TO QLSX1-WBEG-BRNO
              MOVE LSX-PATH-OWNBR        TO QLSX1-WEND-BRNO
              MOVE LSX-ACCTNO            TO QLSX1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSX1-WEND-ACCTNO
              MOVE LSX-SEQNO             TO QLSX1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSX1-WEND-SEQNO.


           EXEC SQL
            DECLARE LSX1_CURSOR CURSOR FOR
            SELECT LSXFILE.LSX_BRNO,
                   LSXFILE.LSX_ACCTNO,
                   LSXFILE.LSX_SEQNO,
                   CAST(LSXFILE.LSX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_REPODATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_TITLE_MILEAGE,
                   LSXFILE.LSX_VOLUNTARY_FG,
                   LSXFILE.LSX_PERFECT,
                   CAST(LSXFILE.LSX_AUTHORIZED_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_AUTHORIZED_BY,
                   LSXFILE.LSX_REASON,
                   LSXFILE.LSX_X_ASSIGNED_TO,
                   CAST(LSXFILE.LSX_ASSIGNED_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_REPO_CKNO,
                   CAST(LSXFILE.LSX_REPO_CKDATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_REPO_CKAMT,
                   LSXFILE.LSX_STORED_ADR1,
                   CAST(LSXFILE.LSX_STORED_CKDATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_STORED_CKNO,
                   LSXFILE.LSX_STORED_CKAMT,
                   CAST(LSXFILE.LSX_REDEEM_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_BUREAU_NOTIFIED,
                   CAST(LSXFILE.LSX_BUREAU_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_BUREAU_BY,
                   CAST(LSXFILE.LSX_10DAY_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_10DAY_BY,
                   LSXFILE.LSX_10DAY_COST,
                   CAST(LSXFILE.LSX_TITLE_APPL_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_TITLE_APPL_BY,
                   LSXFILE.LSX_TITLE_APPL_COST,
                   LSXFILE.LSX_COND_MILEAGE,
                   LSXFILE.LSX_COND_REPORT_Y_N,
                   LSXFILE.LSX_REPAIRS_DESC_1,
                   LSXFILE.LSX_REPAIRS_DESC_2,
                   LSXFILE.LSX_REPAIRS_COST,
                   LSXFILE.LSX_ASSIGN_TO_1,
                   CAST(LSXFILE.LSX_ASSIGN_COMPLETE_DATE_1
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_ASSIGN_CKDATE_1
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_ASSIGN_CKNO_1,
                   LSXFILE.LSX_ASSIGN_CKAMT_1,
                   LSXFILE.LSX_ASSIGN_TO_2,
                   CAST(LSXFILE.LSX_ASSIGN_COMPLETE_DATE_2
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_ASSIGN_CKDATE_2
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_ASSIGN_CKNO_2,
                   LSXFILE.LSX_ASSIGN_CKAMT_2,
                   LSXFILE.LSX_OTH_EXP_1,
                   CAST(LSXFILE.LSX_OTH_EXP_CKDATE_1
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_OTH_EXP_CKNO_1,
                   LSXFILE.LSX_OTH_EXP_CKAMT_1,
                   LSXFILE.LSX_OTH_EXP_2,
                   CAST(LSXFILE.LSX_OTH_EXP_CKDATE_2
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_OTH_EXP_CKNO_2,
                   LSXFILE.LSX_OTH_EXP_CKAMT_2,
                   LSXFILE.LSX_OTH_EXP_3,
                   CAST(LSXFILE.LSX_OTH_EXP_CKDATE_3
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_OTH_EXP_CKNO_3,
                   LSXFILE.LSX_OTH_EXP_CKAMT_3,
                   CAST(LSXFILE.LSX_INS_RB_REQ_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_INS_RB_REC_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_INS_RB_AMT,
                   CAST(LSXFILE.LSX_RB_REQ_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_RB_REC_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_RB_AMT,
                   CAST(LSXFILE.LSX_SIC_INS_FILE_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_SIC_INS_PAID_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_SIC_INS_PAID_AMT,
                   LSXFILE.LSX_BOOK_VALUE,
                   LSXFILE.LSX_SOLD_TO_1,
                   LSXFILE.LSX_SOLD_TO_2,
                   LSXFILE.LSX_SOLD_TO_3,
                   CAST(LSXFILE.LSX_SOLD_TO_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_SOLD_TO_AMT,
                   LSXFILE.LSX_DLNO,
                   LSXFILE.LSX_DLR_RSV_CKNO,
                   LSXFILE.LSX_DLR_RSV_CKAMT,
                   LSXFILE.LSX_PL_EXPENSE,
                   LSXFILE.LSX_DEFICIENCY_Y_N,
                   LSXFILE.LSX_NET_BAL_DUE,
                   LSXFILE.LSX_DEFICIENCY_BAL,
                   LSXFILE.LSX_AUCTION_EXPENSE,
                   LSXFILE.LSX_REASON_CODE,
                   LSXFILE.LSX_TRIM,
                   LSXFILE.LSX_DOORS,
                   LSXFILE.LSX_COLOR,
                   LSXFILE.LSX_CYLINDERS,
                   LSXFILE.LSX_AUTO_TRANS,
                   LSXFILE.LSX_AC,
                   LSXFILE.LSX_SROOF,
                   LSXFILE.LSX_LEATHER,
                   LSXFILE.LSX_4WD,
                   LSXFILE.LSX_DIESEL,
                   LSXFILE.LSX_DUAL_AIR,
                   LSXFILE.LSX_EXT_CAB,
                   LSXFILE.LSX_AGENT_PHONE,
                   LSXFILE.LSX_AGENT_FAX,
                   LSXFILE.LSX_STORED_ADR2,
                   LSXFILE.LSX_X_STORED_CITY,
                   LSXFILE.LSX_X_STORED_STATE,
                   LSXFILE.LSX_X_STORED_ZIP,
                   LSXFILE.LSX_INS_CLAIM_PENDING,
                   LSXFILE.LSX_WARRANTY_AVAILABLE,
                   LSXFILE.LSX_INS_REBATE_REQUESTED,
                   LSXFILE.LSX_EXTRACTED,
                   CAST(LSXFILE.LSX_TITLE_SENT_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_DISPOSAL_HOLD,
                   CAST(LSXFILE.LSX_NEXT_HLD_RVW_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_CASE_ID,
                   LSXFILE.LSX_COLLATERAL_LOCATION,
                   LSXFILE.LSX_VIN_MATCH,
                   LSXFILE.LSX_PERSONALS,
                   LSXFILE.LSX_KEYS,
                   LSXFILE.LSX_DRIVES,
                   LSXFILE.LSX_DMG,
                   LSXFILE.LSX_Y_ASSIGNED_TO,
                   LSXFILE.LSX_SUPPLIER_ID,
                   LSXFILE.LSX_TRANSPORT_EMAIL,
                   LSXFILE.LSX_STORED_LOCATION,
                   LSXFILE.LSX_STORED_ADDRESS,
                   LSXFILE.LSX_Y_STORED_CITY,
                   LSXFILE.LSX_Y_STORED_STATE,
                   LSXFILE.LSX_Y_STORED_ZIP,
                   LSXFILE.LSX_AGENCY_NAME,
                   LSXFILE.LSX_AGENCY_ADDRESS,
                   LSXFILE.LSX_AGENCY_CITY,
                   LSXFILE.LSX_AGENCY_STATE,
                   LSXFILE.LSX_AGENCY_ZIP,
                   LSXFILE.LSX_AGENCY_PHONE
            FROM DBO.LSXFILE
            WHERE LSXFILE.LSX_BRNO  = :QLSX1-WBEG-BRNO
              AND LSXFILE.LSX_ACCTNO >= :QLSX1-WBEG-ACCTNO
              AND LSXFILE.LSX_ACCTNO <= :QLSX1-WEND-ACCTNO
              AND LSXFILE.LSX_SEQNO >= :QLSX1-WBEG-SEQNO
              AND LSXFILE.LSX_SEQNO <= :QLSX1-WEND-SEQNO
            ORDER BY LSXFILE.LSX_BRNO,
                     LSXFILE.LSX_ACCTNO,
                     LSXFILE.LSX_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSX1-WBEG-BRNO
                          QLSX1-WEND-BRNO
                          QLSX1-WBEG-ACCTNO
                          QLSX1-WEND-ACCTNO
                          QLSX1-WBEG-SEQNO
                          QLSX1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-LSX1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE LSX-PATH-OWNBR  TO LSX-BRNO.
           MOVE LSX-PATH-SQL    TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.

           IF ( LSX1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-LSX1-FILE.

           IF ( QLSX1-WEND-BRNO = ZEROES    ) OR
              ( QLSX1-WEND-BRNO NOT NUMERIC )
              MOVE LSX-PATH-OWNBR        TO QLSX1-WBEG-BRNO
              MOVE LSX-PATH-OWNBR        TO QLSX1-WEND-BRNO
              MOVE ALL "0"               TO QLSX1-WBEG-ACCTNO
              MOVE LSX-ACCTNO            TO QLSX1-WEND-ACCTNO
              MOVE ALL "0"               TO QLSX1-WBEG-SEQNO
              MOVE LSX-SEQNO             TO QLSX1-WEND-SEQNO.


           EXEC SQL
            DECLARE LSX1_CURSOR_NG CURSOR FOR
            SELECT LSXFILE.LSX_BRNO,
                   LSXFILE.LSX_ACCTNO,
                   LSXFILE.LSX_SEQNO,
                   CAST(LSXFILE.LSX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_REPODATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_TITLE_MILEAGE,
                   LSXFILE.LSX_VOLUNTARY_FG,
                   LSXFILE.LSX_PERFECT,
                   CAST(LSXFILE.LSX_AUTHORIZED_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_AUTHORIZED_BY,
                   LSXFILE.LSX_REASON,
                   LSXFILE.LSX_X_ASSIGNED_TO,
                   CAST(LSXFILE.LSX_ASSIGNED_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_REPO_CKNO,
                   CAST(LSXFILE.LSX_REPO_CKDATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_REPO_CKAMT,
                   LSXFILE.LSX_STORED_ADR1,
                   CAST(LSXFILE.LSX_STORED_CKDATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_STORED_CKNO,
                   LSXFILE.LSX_STORED_CKAMT,
                   CAST(LSXFILE.LSX_REDEEM_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_BUREAU_NOTIFIED,
                   CAST(LSXFILE.LSX_BUREAU_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_BUREAU_BY,
                   CAST(LSXFILE.LSX_10DAY_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_10DAY_BY,
                   LSXFILE.LSX_10DAY_COST,
                   CAST(LSXFILE.LSX_TITLE_APPL_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_TITLE_APPL_BY,
                   LSXFILE.LSX_TITLE_APPL_COST,
                   LSXFILE.LSX_COND_MILEAGE,
                   LSXFILE.LSX_COND_REPORT_Y_N,
                   LSXFILE.LSX_REPAIRS_DESC_1,
                   LSXFILE.LSX_REPAIRS_DESC_2,
                   LSXFILE.LSX_REPAIRS_COST,
                   LSXFILE.LSX_ASSIGN_TO_1,
                   CAST(LSXFILE.LSX_ASSIGN_COMPLETE_DATE_1
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_ASSIGN_CKDATE_1
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_ASSIGN_CKNO_1,
                   LSXFILE.LSX_ASSIGN_CKAMT_1,
                   LSXFILE.LSX_ASSIGN_TO_2,
                   CAST(LSXFILE.LSX_ASSIGN_COMPLETE_DATE_2
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_ASSIGN_CKDATE_2
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_ASSIGN_CKNO_2,
                   LSXFILE.LSX_ASSIGN_CKAMT_2,
                   LSXFILE.LSX_OTH_EXP_1,
                   CAST(LSXFILE.LSX_OTH_EXP_CKDATE_1
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_OTH_EXP_CKNO_1,
                   LSXFILE.LSX_OTH_EXP_CKAMT_1,
                   LSXFILE.LSX_OTH_EXP_2,
                   CAST(LSXFILE.LSX_OTH_EXP_CKDATE_2
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_OTH_EXP_CKNO_2,
                   LSXFILE.LSX_OTH_EXP_CKAMT_2,
                   LSXFILE.LSX_OTH_EXP_3,
                   CAST(LSXFILE.LSX_OTH_EXP_CKDATE_3
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_OTH_EXP_CKNO_3,
                   LSXFILE.LSX_OTH_EXP_CKAMT_3,
                   CAST(LSXFILE.LSX_INS_RB_REQ_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_INS_RB_REC_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_INS_RB_AMT,
                   CAST(LSXFILE.LSX_RB_REQ_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_RB_REC_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_RB_AMT,
                   CAST(LSXFILE.LSX_SIC_INS_FILE_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_SIC_INS_PAID_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_SIC_INS_PAID_AMT,
                   LSXFILE.LSX_BOOK_VALUE,
                   LSXFILE.LSX_SOLD_TO_1,
                   LSXFILE.LSX_SOLD_TO_2,
                   LSXFILE.LSX_SOLD_TO_3,
                   CAST(LSXFILE.LSX_SOLD_TO_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_SOLD_TO_AMT,
                   LSXFILE.LSX_DLNO,
                   LSXFILE.LSX_DLR_RSV_CKNO,
                   LSXFILE.LSX_DLR_RSV_CKAMT,
                   LSXFILE.LSX_PL_EXPENSE,
                   LSXFILE.LSX_DEFICIENCY_Y_N,
                   LSXFILE.LSX_NET_BAL_DUE,
                   LSXFILE.LSX_DEFICIENCY_BAL,
                   LSXFILE.LSX_AUCTION_EXPENSE,
                   LSXFILE.LSX_REASON_CODE,
                   LSXFILE.LSX_TRIM,
                   LSXFILE.LSX_DOORS,
                   LSXFILE.LSX_COLOR,
                   LSXFILE.LSX_CYLINDERS,
                   LSXFILE.LSX_AUTO_TRANS,
                   LSXFILE.LSX_AC,
                   LSXFILE.LSX_SROOF,
                   LSXFILE.LSX_LEATHER,
                   LSXFILE.LSX_4WD,
                   LSXFILE.LSX_DIESEL,
                   LSXFILE.LSX_DUAL_AIR,
                   LSXFILE.LSX_EXT_CAB,
                   LSXFILE.LSX_AGENT_PHONE,
                   LSXFILE.LSX_AGENT_FAX,
                   LSXFILE.LSX_STORED_ADR2,
                   LSXFILE.LSX_X_STORED_CITY,
                   LSXFILE.LSX_X_STORED_STATE,
                   LSXFILE.LSX_X_STORED_ZIP,
                   LSXFILE.LSX_INS_CLAIM_PENDING,
                   LSXFILE.LSX_WARRANTY_AVAILABLE,
                   LSXFILE.LSX_INS_REBATE_REQUESTED,
                   LSXFILE.LSX_EXTRACTED,
                   CAST(LSXFILE.LSX_TITLE_SENT_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_DISPOSAL_HOLD,
                   CAST(LSXFILE.LSX_NEXT_HLD_RVW_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_CASE_ID,
                   LSXFILE.LSX_COLLATERAL_LOCATION,
                   LSXFILE.LSX_VIN_MATCH,
                   LSXFILE.LSX_PERSONALS,
                   LSXFILE.LSX_KEYS,
                   LSXFILE.LSX_DRIVES,
                   LSXFILE.LSX_DMG,
                   LSXFILE.LSX_Y_ASSIGNED_TO,
                   LSXFILE.LSX_SUPPLIER_ID,
                   LSXFILE.LSX_TRANSPORT_EMAIL,
                   LSXFILE.LSX_STORED_LOCATION,
                   LSXFILE.LSX_STORED_ADDRESS,
                   LSXFILE.LSX_Y_STORED_CITY,
                   LSXFILE.LSX_Y_STORED_STATE,
                   LSXFILE.LSX_Y_STORED_ZIP,
                   LSXFILE.LSX_AGENCY_NAME,
                   LSXFILE.LSX_AGENCY_ADDRESS,
                   LSXFILE.LSX_AGENCY_CITY,
                   LSXFILE.LSX_AGENCY_STATE,
                   LSXFILE.LSX_AGENCY_ZIP,
                   LSXFILE.LSX_AGENCY_PHONE
            FROM DBO.LSXFILE
            WHERE LSXFILE.LSX_BRNO  = :QLSX1-WBEG-BRNO
              AND LSXFILE.LSX_ACCTNO >= :QLSX1-WBEG-ACCTNO
              AND LSXFILE.LSX_ACCTNO <= :QLSX1-WEND-ACCTNO
              AND LSXFILE.LSX_SEQNO >= :QLSX1-WBEG-SEQNO
              AND LSXFILE.LSX_SEQNO <= :QLSX1-WEND-SEQNO
            ORDER BY LSXFILE.LSX_BRNO DESC,
                     LSXFILE.LSX_ACCTNO DESC,
                     LSXFILE.LSX_SEQNO DESC
           END-EXEC.

           EXEC SQL
               OPEN LSX1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSX1-WBEG-BRNO
                          QLSX1-WEND-BRNO
                          QLSX1-WBEG-ACCTNO
                          QLSX1-WEND-ACCTNO
                          QLSX1-WBEG-SEQNO
                          QLSX1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSX1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-LSX1-FILE.
           PERFORM READ-IT.
           MOVE LSX-PATH-OWNBR  TO LSX-BRNO. 
           MOVE LSX-PATH-SQL    TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.
           INITIALIZE QLSX-REC.

           MOVE LSX-BRNO   TO QLSX-BRNO.
           MOVE LSX-ACCTNO TO QLSX-ACCTNO.
           MOVE LSX-SEQNO TO QLSX-SEQNO.

           EXEC SQL
            SELECT LSXFILE.LSX_BRNO,
                   LSXFILE.LSX_ACCTNO,
                   LSXFILE.LSX_SEQNO,
                   CAST(LSXFILE.LSX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_REPODATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_TITLE_MILEAGE,
                   LSXFILE.LSX_VOLUNTARY_FG,
                   LSXFILE.LSX_PERFECT,
                   CAST(LSXFILE.LSX_AUTHORIZED_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_AUTHORIZED_BY,
                   LSXFILE.LSX_REASON,
                   LSXFILE.LSX_X_ASSIGNED_TO,
                   CAST(LSXFILE.LSX_ASSIGNED_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_REPO_CKNO,
                   CAST(LSXFILE.LSX_REPO_CKDATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_REPO_CKAMT,
                   LSXFILE.LSX_STORED_ADR1,
                   CAST(LSXFILE.LSX_STORED_CKDATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_STORED_CKNO,
                   LSXFILE.LSX_STORED_CKAMT,
                   CAST(LSXFILE.LSX_REDEEM_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_BUREAU_NOTIFIED,
                   CAST(LSXFILE.LSX_BUREAU_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_BUREAU_BY,
                   CAST(LSXFILE.LSX_10DAY_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_10DAY_BY,
                   LSXFILE.LSX_10DAY_COST,
                   CAST(LSXFILE.LSX_TITLE_APPL_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_TITLE_APPL_BY,
                   LSXFILE.LSX_TITLE_APPL_COST,
                   LSXFILE.LSX_COND_MILEAGE,
                   LSXFILE.LSX_COND_REPORT_Y_N,
                   LSXFILE.LSX_REPAIRS_DESC_1,
                   LSXFILE.LSX_REPAIRS_DESC_2,
                   LSXFILE.LSX_REPAIRS_COST,
                   LSXFILE.LSX_ASSIGN_TO_1,
                   CAST(LSXFILE.LSX_ASSIGN_COMPLETE_DATE_1
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_ASSIGN_CKDATE_1
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_ASSIGN_CKNO_1,
                   LSXFILE.LSX_ASSIGN_CKAMT_1,
                   LSXFILE.LSX_ASSIGN_TO_2,
                   CAST(LSXFILE.LSX_ASSIGN_COMPLETE_DATE_2
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_ASSIGN_CKDATE_2
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_ASSIGN_CKNO_2,
                   LSXFILE.LSX_ASSIGN_CKAMT_2,
                   LSXFILE.LSX_OTH_EXP_1,
                   CAST(LSXFILE.LSX_OTH_EXP_CKDATE_1
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_OTH_EXP_CKNO_1,
                   LSXFILE.LSX_OTH_EXP_CKAMT_1,
                   LSXFILE.LSX_OTH_EXP_2,
                   CAST(LSXFILE.LSX_OTH_EXP_CKDATE_2
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_OTH_EXP_CKNO_2,
                   LSXFILE.LSX_OTH_EXP_CKAMT_2,
                   LSXFILE.LSX_OTH_EXP_3,
                   CAST(LSXFILE.LSX_OTH_EXP_CKDATE_3
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_OTH_EXP_CKNO_3,
                   LSXFILE.LSX_OTH_EXP_CKAMT_3,
                   CAST(LSXFILE.LSX_INS_RB_REQ_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_INS_RB_REC_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_INS_RB_AMT,
                   CAST(LSXFILE.LSX_RB_REQ_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_RB_REC_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_RB_AMT,
                   CAST(LSXFILE.LSX_SIC_INS_FILE_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSXFILE.LSX_SIC_INS_PAID_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_SIC_INS_PAID_AMT,
                   LSXFILE.LSX_BOOK_VALUE,
                   LSXFILE.LSX_SOLD_TO_1,
                   LSXFILE.LSX_SOLD_TO_2,
                   LSXFILE.LSX_SOLD_TO_3,
                   CAST(LSXFILE.LSX_SOLD_TO_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_SOLD_TO_AMT,
                   LSXFILE.LSX_DLNO,
                   LSXFILE.LSX_DLR_RSV_CKNO,
                   LSXFILE.LSX_DLR_RSV_CKAMT,
                   LSXFILE.LSX_PL_EXPENSE,
                   LSXFILE.LSX_DEFICIENCY_Y_N,
                   LSXFILE.LSX_NET_BAL_DUE,
                   LSXFILE.LSX_DEFICIENCY_BAL,
                   LSXFILE.LSX_AUCTION_EXPENSE,
                   LSXFILE.LSX_REASON_CODE,
                   LSXFILE.LSX_TRIM,
                   LSXFILE.LSX_DOORS,
                   LSXFILE.LSX_COLOR,
                   LSXFILE.LSX_CYLINDERS,
                   LSXFILE.LSX_AUTO_TRANS,
                   LSXFILE.LSX_AC,
                   LSXFILE.LSX_SROOF,
                   LSXFILE.LSX_LEATHER,
                   LSXFILE.LSX_4WD,
                   LSXFILE.LSX_DIESEL,
                   LSXFILE.LSX_DUAL_AIR,
                   LSXFILE.LSX_EXT_CAB,
                   LSXFILE.LSX_AGENT_PHONE,
                   LSXFILE.LSX_AGENT_FAX,
                   LSXFILE.LSX_STORED_ADR2,
                   LSXFILE.LSX_X_STORED_CITY,
                   LSXFILE.LSX_X_STORED_STATE,
                   LSXFILE.LSX_X_STORED_ZIP,
                   LSXFILE.LSX_INS_CLAIM_PENDING,
                   LSXFILE.LSX_WARRANTY_AVAILABLE,
                   LSXFILE.LSX_INS_REBATE_REQUESTED,
                   LSXFILE.LSX_EXTRACTED,
                   CAST(LSXFILE.LSX_TITLE_SENT_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_DISPOSAL_HOLD,
                   CAST(LSXFILE.LSX_NEXT_HLD_RVW_DATE
                                                  AS VARCHAR(10)),
                   LSXFILE.LSX_CASE_ID,
                   LSXFILE.LSX_COLLATERAL_LOCATION,
                   LSXFILE.LSX_VIN_MATCH,
                   LSXFILE.LSX_PERSONALS,
                   LSXFILE.LSX_KEYS,
                   LSXFILE.LSX_DRIVES,
                   LSXFILE.LSX_DMG,
                   LSXFILE.LSX_Y_ASSIGNED_TO,
                   LSXFILE.LSX_SUPPLIER_ID,
                   LSXFILE.LSX_TRANSPORT_EMAIL,
                   LSXFILE.LSX_STORED_LOCATION,
                   LSXFILE.LSX_STORED_ADDRESS,
                   LSXFILE.LSX_Y_STORED_CITY,
                   LSXFILE.LSX_Y_STORED_STATE,
                   LSXFILE.LSX_Y_STORED_ZIP,
                   LSXFILE.LSX_AGENCY_NAME,
                   LSXFILE.LSX_AGENCY_ADDRESS,
                   LSXFILE.LSX_AGENCY_CITY,
                   LSXFILE.LSX_AGENCY_STATE,
                   LSXFILE.LSX_AGENCY_ZIP,
                   LSXFILE.LSX_AGENCY_PHONE
            INTO :QLSX-REC
            FROM DBO.LSXFILE
            WHERE LSXFILE.LSX_BRNO = :QLSX-BRNO
              AND LSXFILE.LSX_ACCTNO = :QLSX-ACCTNO
              AND LSXFILE.LSX_SEQNO = :QLSX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSX-FIELDS.

      *-----------------------------------------------------------------
       READ-LSX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSX-PATH-OWNBR  TO LSX-BRNO.
           MOVE LSX-PATH-SQL    TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.
           INITIALIZE QLSX-REC.

           IF ( LSX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSX1_CURSOR INTO :QLSX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSX-FIELDS.

      *-----------------------------------------------------------------
       READ-LSX1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE LSX-PATH-OWNBR  TO LSX-BRNO.
           MOVE LSX-PATH-SQL    TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.
           INITIALIZE QLSX-REC.

           IF ( LSX1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH LSX1_CURSOR_NG INTO :QLSX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSX1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-LSX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LSX1-FILE.

           PERFORM CLOSE-IT.
           IF ( LSX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSX1-CURSOR-STAT.

           IF ( LSX1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE LSX1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO LSX1-CURSOR-NG-STAT.

