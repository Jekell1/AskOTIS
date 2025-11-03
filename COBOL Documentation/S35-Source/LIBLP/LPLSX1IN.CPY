      * COPYMEMBER: LIBLP/LPLSX1IN
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
       WRITE-LSX1-FILE.

           MOVE LSX-PATH-OWNBR  TO LSX-BRNO.
           ACCEPT LSX-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LSX-PATH-SQL    TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.
           PERFORM SET-LSX-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LSXFILE
             (LSXFILE.LSX_BRNO,
              LSXFILE.LSX_ACCTNO,
              LSXFILE.LSX_SEQNO,
              LSXFILE.LSX_LTOUCH_DATE,
              LSXFILE.LSX_REPODATE,
              LSXFILE.LSX_TITLE_MILEAGE,
              LSXFILE.LSX_VOLUNTARY_FG,
              LSXFILE.LSX_PERFECT,
              LSXFILE.LSX_AUTHORIZED_DATE,
              LSXFILE.LSX_AUTHORIZED_BY,
              LSXFILE.LSX_REASON,
              LSXFILE.LSX_X_ASSIGNED_TO,
              LSXFILE.LSX_ASSIGNED_DATE,
              LSXFILE.LSX_REPO_CKNO,
              LSXFILE.LSX_REPO_CKDATE,
              LSXFILE.LSX_REPO_CKAMT,
              LSXFILE.LSX_STORED_ADR1,
              LSXFILE.LSX_STORED_CKDATE,
              LSXFILE.LSX_STORED_CKNO,
              LSXFILE.LSX_STORED_CKAMT,
              LSXFILE.LSX_REDEEM_DATE,
              LSXFILE.LSX_BUREAU_NOTIFIED,
              LSXFILE.LSX_BUREAU_DATE,
              LSXFILE.LSX_BUREAU_BY,
              LSXFILE.LSX_10DAY_DATE,
              LSXFILE.LSX_10DAY_BY,
              LSXFILE.LSX_10DAY_COST,
              LSXFILE.LSX_TITLE_APPL_DATE,
              LSXFILE.LSX_TITLE_APPL_BY,
              LSXFILE.LSX_TITLE_APPL_COST,
              LSXFILE.LSX_COND_MILEAGE,
              LSXFILE.LSX_COND_REPORT_Y_N,
              LSXFILE.LSX_REPAIRS_DESC_1,
              LSXFILE.LSX_REPAIRS_DESC_2,
              LSXFILE.LSX_REPAIRS_COST,
              LSXFILE.LSX_ASSIGN_TO_1,
              LSXFILE.LSX_ASSIGN_COMPLETE_DATE_1,
              LSXFILE.LSX_ASSIGN_CKDATE_1,
              LSXFILE.LSX_ASSIGN_CKNO_1,
              LSXFILE.LSX_ASSIGN_CKAMT_1,
              LSXFILE.LSX_ASSIGN_TO_2,
              LSXFILE.LSX_ASSIGN_COMPLETE_DATE_2,
              LSXFILE.LSX_ASSIGN_CKDATE_2,
              LSXFILE.LSX_ASSIGN_CKNO_2,
              LSXFILE.LSX_ASSIGN_CKAMT_2,
              LSXFILE.LSX_OTH_EXP_1,
              LSXFILE.LSX_OTH_EXP_CKDATE_1,
              LSXFILE.LSX_OTH_EXP_CKNO_1,
              LSXFILE.LSX_OTH_EXP_CKAMT_1,
              LSXFILE.LSX_OTH_EXP_2,
              LSXFILE.LSX_OTH_EXP_CKDATE_2,
              LSXFILE.LSX_OTH_EXP_CKNO_2,
              LSXFILE.LSX_OTH_EXP_CKAMT_2,
              LSXFILE.LSX_OTH_EXP_3,
              LSXFILE.LSX_OTH_EXP_CKDATE_3,
              LSXFILE.LSX_OTH_EXP_CKNO_3,
              LSXFILE.LSX_OTH_EXP_CKAMT_3,
              LSXFILE.LSX_INS_RB_REQ_DATE,
              LSXFILE.LSX_INS_RB_REC_DATE,
              LSXFILE.LSX_INS_RB_AMT,
              LSXFILE.LSX_RB_REQ_DATE,
              LSXFILE.LSX_RB_REC_DATE,
              LSXFILE.LSX_RB_AMT,
              LSXFILE.LSX_SIC_INS_FILE_DATE,
              LSXFILE.LSX_SIC_INS_PAID_DATE,
              LSXFILE.LSX_SIC_INS_PAID_AMT,
              LSXFILE.LSX_BOOK_VALUE,
              LSXFILE.LSX_SOLD_TO_1,
              LSXFILE.LSX_SOLD_TO_2,
              LSXFILE.LSX_SOLD_TO_3,
              LSXFILE.LSX_SOLD_TO_DATE,
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
              LSXFILE.LSX_TITLE_SENT_DATE,
              LSXFILE.LSX_DISPOSAL_HOLD,
              LSXFILE.LSX_NEXT_HLD_RVW_DATE,
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
              LSXFILE.LSX_AGENCY_PHONE)
            VALUES
             (:QLSX-BRNO,
              :QLSX-ACCTNO,
              :QLSX-SEQNO,
              :QLSX-LTOUCH-DATE,
              :QLSX-REPODATE,
              :QLSX-TITLE-MILEAGE,
              :QLSX-VOLUNTARY-FG,
              :QLSX-PERFECT,
              :QLSX-AUTHORIZED-DATE,
              :QLSX-AUTHORIZED-BY,
              :QLSX-REASON,
              :QLSX-X-ASSIGNED-TO,
              :QLSX-ASSIGNED-DATE,
              :QLSX-REPO-CKNO,
              :QLSX-REPO-CKDATE,
              :QLSX-REPO-CKAMT,
              :QLSX-STORED-ADR1,
              :QLSX-STORED-CKDATE,
              :QLSX-STORED-CKNO,
              :QLSX-STORED-CKAMT,
              :QLSX-REDEEM-DATE,
              :QLSX-BUREAU-NOTIFIED,
              :QLSX-BUREAU-DATE,
              :QLSX-BUREAU-BY,
              :QLSX-10DAY-DATE,
              :QLSX-10DAY-BY,
              :QLSX-10DAY-COST,
              :QLSX-TITLE-APPL-DATE,
              :QLSX-TITLE-APPL-BY,
              :QLSX-TITLE-APPL-COST,
              :QLSX-COND-MILEAGE,
              :QLSX-COND-REPORT-Y-N,
              :QLSX-REPAIRS-DESC-1,
              :QLSX-REPAIRS-DESC-2,
              :QLSX-REPAIRS-COST,
              :QLSX-ASSIGN-TO-1,
              :QLSX-ASSIGN-COMPLETE-DATE-1,
              :QLSX-ASSIGN-CKDATE-1,
              :QLSX-ASSIGN-CKNO-1,
              :QLSX-ASSIGN-CKAMT-1,
              :QLSX-ASSIGN-TO-2,
              :QLSX-ASSIGN-COMPLETE-DATE-2,
              :QLSX-ASSIGN-CKDATE-2,
              :QLSX-ASSIGN-CKNO-2,
              :QLSX-ASSIGN-CKAMT-2,
              :QLSX-OTH-EXP-1,
              :QLSX-OTH-EXP-CKDATE-1,
              :QLSX-OTH-EXP-CKNO-1,
              :QLSX-OTH-EXP-CKAMT-1,
              :QLSX-OTH-EXP-2,
              :QLSX-OTH-EXP-CKDATE-2,
              :QLSX-OTH-EXP-CKNO-2,
              :QLSX-OTH-EXP-CKAMT-2,
              :QLSX-OTH-EXP-3,
              :QLSX-OTH-EXP-CKDATE-3,
              :QLSX-OTH-EXP-CKNO-3,
              :QLSX-OTH-EXP-CKAMT-3,
              :QLSX-INS-RB-REQ-DATE,
              :QLSX-INS-RB-REC-DATE,
              :QLSX-INS-RB-AMT,
              :QLSX-RB-REQ-DATE,
              :QLSX-RB-REC-DATE,
              :QLSX-RB-AMT,
              :QLSX-SIC-INS-FILE-DATE,
              :QLSX-SIC-INS-PAID-DATE,
              :QLSX-SIC-INS-PAID-AMT,
              :QLSX-BOOK-VALUE,
              :QLSX-SOLD-TO-1,
              :QLSX-SOLD-TO-2,
              :QLSX-SOLD-TO-3,
              :QLSX-SOLD-TO-DATE,
              :QLSX-SOLD-TO-AMT,
              :QLSX-DLNO,
              :QLSX-DLR-RSV-CKNO,
              :QLSX-DLR-RSV-CKAMT,
              :QLSX-PL-EXPENSE,
              :QLSX-DEFICIENCY-Y-N,
              :QLSX-NET-BAL-DUE,
              :QLSX-DEFICIENCY-BAL,
              :QLSX-AUCTION-EXPENSE,
              :QLSX-REASON-CODE,
              :QLSX-TRIM,
              :QLSX-DOORS,
              :QLSX-COLOR,
              :QLSX-CYLINDERS,
              :QLSX-AUTO-TRANS,
              :QLSX-AC,
              :QLSX-SROOF,
              :QLSX-LEATHER,
              :QLSX-4WD,
              :QLSX-DIESEL,
              :QLSX-DUAL-AIR,
              :QLSX-EXT-CAB,
              :QLSX-AGENT-PHONE,
              :QLSX-AGENT-FAX,
              :QLSX-STORED-ADR2,
              :QLSX-X-STORED-CITY,
              :QLSX-X-STORED-STATE,
              :QLSX-X-STORED-ZIP,
              :QLSX-INS-CLAIM-PENDING,
              :QLSX-WARRANTY-AVAILABLE,
              :QLSX-INS-REBATE-REQUESTED,
              :QLSX-EXTRACTED,
              :QLSX-TITLE-SENT-DATE,
              :QLSX-DISPOSAL-HOLD,
              :QLSX-NEXT-HLD-RVW-DATE,
              :QLSX-CASE-ID,
              :QLSX-COLLATERAL-LOCATION,
              :QLSX-VIN-MATCH,
              :QLSX-PERSONALS,
              :QLSX-KEYS,
              :QLSX-DRIVES,
              :QLSX-DMG,
              :QLSX-Y-ASSIGNED-TO,
              :QLSX-SUPPLIER-ID,
              :QLSX-TRANSPORT-EMAIL,
              :QLSX-STORED-LOCATION,
              :QLSX-STORED-ADDRESS,
              :QLSX-Y-STORED-CITY,
              :QLSX-Y-STORED-STATE,
              :QLSX-Y-STORED-ZIP,
              :QLSX-AGENCY-NAME,
              :QLSX-AGENCY-ADDRESS,
              :QLSX-AGENCY-CITY,
              :QLSX-AGENCY-STATE,
              :QLSX-AGENCY-ZIP,
              :QLSX-AGENCY-PHONE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LSX1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LSX1-FILE.

           MOVE LSX-PATH-OWNBR  TO LSX-BRNO.
           ACCEPT LSX-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LSX-PATH-SQL    TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.
           PERFORM SET-LSX-FIELDS.

           EXEC SQL
            UPDATE DBO.LSXFILE
             SET
              LSXFILE.LSX_BRNO = :QLSX-BRNO,
              LSXFILE.LSX_ACCTNO = :QLSX-ACCTNO,
              LSXFILE.LSX_SEQNO = :QLSX-SEQNO,
              LSXFILE.LSX_LTOUCH_DATE =
                 :QLSX-LTOUCH-DATE,
              LSXFILE.LSX_REPODATE =
                 :QLSX-REPODATE,
              LSXFILE.LSX_TITLE_MILEAGE = :QLSX-TITLE-MILEAGE,
              LSXFILE.LSX_VOLUNTARY_FG = :QLSX-VOLUNTARY-FG,
              LSXFILE.LSX_PERFECT = :QLSX-PERFECT,
              LSXFILE.LSX_AUTHORIZED_DATE =
                 :QLSX-AUTHORIZED-DATE,
              LSXFILE.LSX_AUTHORIZED_BY = :QLSX-AUTHORIZED-BY,
              LSXFILE.LSX_REASON = :QLSX-REASON,
              LSXFILE.LSX_X_ASSIGNED_TO = :QLSX-X-ASSIGNED-TO,
              LSXFILE.LSX_ASSIGNED_DATE =
                 :QLSX-ASSIGNED-DATE,
              LSXFILE.LSX_REPO_CKNO = :QLSX-REPO-CKNO,
              LSXFILE.LSX_REPO_CKDATE =
                 :QLSX-REPO-CKDATE,
              LSXFILE.LSX_REPO_CKAMT = :QLSX-REPO-CKAMT,
              LSXFILE.LSX_STORED_ADR1 = :QLSX-STORED-ADR1,
              LSXFILE.LSX_STORED_CKDATE =
                 :QLSX-STORED-CKDATE,
              LSXFILE.LSX_STORED_CKNO = :QLSX-STORED-CKNO,
              LSXFILE.LSX_STORED_CKAMT = :QLSX-STORED-CKAMT,
              LSXFILE.LSX_REDEEM_DATE =
                 :QLSX-REDEEM-DATE,
              LSXFILE.LSX_BUREAU_NOTIFIED = :QLSX-BUREAU-NOTIFIED,
              LSXFILE.LSX_BUREAU_DATE =
                 :QLSX-BUREAU-DATE,
              LSXFILE.LSX_BUREAU_BY = :QLSX-BUREAU-BY,
              LSXFILE.LSX_10DAY_DATE =
                 :QLSX-10DAY-DATE,
              LSXFILE.LSX_10DAY_BY = :QLSX-10DAY-BY,
              LSXFILE.LSX_10DAY_COST = :QLSX-10DAY-COST,
              LSXFILE.LSX_TITLE_APPL_DATE =
                 :QLSX-TITLE-APPL-DATE,
              LSXFILE.LSX_TITLE_APPL_BY = :QLSX-TITLE-APPL-BY,
              LSXFILE.LSX_TITLE_APPL_COST = :QLSX-TITLE-APPL-COST,
              LSXFILE.LSX_COND_MILEAGE = :QLSX-COND-MILEAGE,
              LSXFILE.LSX_COND_REPORT_Y_N = :QLSX-COND-REPORT-Y-N,
              LSXFILE.LSX_REPAIRS_DESC_1 = :QLSX-REPAIRS-DESC-1,
              LSXFILE.LSX_REPAIRS_DESC_2 = :QLSX-REPAIRS-DESC-2,
              LSXFILE.LSX_REPAIRS_COST = :QLSX-REPAIRS-COST,
              LSXFILE.LSX_ASSIGN_TO_1 = :QLSX-ASSIGN-TO-1,
              LSXFILE.LSX_ASSIGN_COMPLETE_DATE_1 =
                 :QLSX-ASSIGN-COMPLETE-DATE-1,
              LSXFILE.LSX_ASSIGN_CKDATE_1 =
                 :QLSX-ASSIGN-CKDATE-1,
              LSXFILE.LSX_ASSIGN_CKNO_1 = :QLSX-ASSIGN-CKNO-1,
              LSXFILE.LSX_ASSIGN_CKAMT_1 = :QLSX-ASSIGN-CKAMT-1,
              LSXFILE.LSX_ASSIGN_TO_2 = :QLSX-ASSIGN-TO-2,
              LSXFILE.LSX_ASSIGN_COMPLETE_DATE_2 =
                 :QLSX-ASSIGN-COMPLETE-DATE-2,
              LSXFILE.LSX_ASSIGN_CKDATE_2 =
                 :QLSX-ASSIGN-CKDATE-2,
              LSXFILE.LSX_ASSIGN_CKNO_2 = :QLSX-ASSIGN-CKNO-2,
              LSXFILE.LSX_ASSIGN_CKAMT_2 = :QLSX-ASSIGN-CKAMT-2,
              LSXFILE.LSX_OTH_EXP_1 = :QLSX-OTH-EXP-1,
              LSXFILE.LSX_OTH_EXP_CKDATE_1 =
                 :QLSX-OTH-EXP-CKDATE-1,
              LSXFILE.LSX_OTH_EXP_CKNO_1 = :QLSX-OTH-EXP-CKNO-1,
              LSXFILE.LSX_OTH_EXP_CKAMT_1 = :QLSX-OTH-EXP-CKAMT-1,
              LSXFILE.LSX_OTH_EXP_2 = :QLSX-OTH-EXP-2,
              LSXFILE.LSX_OTH_EXP_CKDATE_2 =
                 :QLSX-OTH-EXP-CKDATE-2,
              LSXFILE.LSX_OTH_EXP_CKNO_2 = :QLSX-OTH-EXP-CKNO-2,
              LSXFILE.LSX_OTH_EXP_CKAMT_2 = :QLSX-OTH-EXP-CKAMT-2,
              LSXFILE.LSX_OTH_EXP_3 = :QLSX-OTH-EXP-3,
              LSXFILE.LSX_OTH_EXP_CKDATE_3 =
                 :QLSX-OTH-EXP-CKDATE-3,
              LSXFILE.LSX_OTH_EXP_CKNO_3 = :QLSX-OTH-EXP-CKNO-3,
              LSXFILE.LSX_OTH_EXP_CKAMT_3 = :QLSX-OTH-EXP-CKAMT-3,
              LSXFILE.LSX_INS_RB_REQ_DATE =
                 :QLSX-INS-RB-REQ-DATE,
              LSXFILE.LSX_INS_RB_REC_DATE =
                 :QLSX-INS-RB-REC-DATE,
              LSXFILE.LSX_INS_RB_AMT = :QLSX-INS-RB-AMT,
              LSXFILE.LSX_RB_REQ_DATE =
                 :QLSX-RB-REQ-DATE,
              LSXFILE.LSX_RB_REC_DATE =
                 :QLSX-RB-REC-DATE,
              LSXFILE.LSX_RB_AMT = :QLSX-RB-AMT,
              LSXFILE.LSX_SIC_INS_FILE_DATE =
                 :QLSX-SIC-INS-FILE-DATE,
              LSXFILE.LSX_SIC_INS_PAID_DATE =
                 :QLSX-SIC-INS-PAID-DATE,
              LSXFILE.LSX_SIC_INS_PAID_AMT =
                   :QLSX-SIC-INS-PAID-AMT,
              LSXFILE.LSX_BOOK_VALUE = :QLSX-BOOK-VALUE,
              LSXFILE.LSX_SOLD_TO_1 = :QLSX-SOLD-TO-1,
              LSXFILE.LSX_SOLD_TO_2 = :QLSX-SOLD-TO-2,
              LSXFILE.LSX_SOLD_TO_3 = :QLSX-SOLD-TO-3,
              LSXFILE.LSX_SOLD_TO_DATE =
                 :QLSX-SOLD-TO-DATE,
              LSXFILE.LSX_SOLD_TO_AMT = :QLSX-SOLD-TO-AMT,
              LSXFILE.LSX_DLNO = :QLSX-DLNO,
              LSXFILE.LSX_DLR_RSV_CKNO = :QLSX-DLR-RSV-CKNO,
              LSXFILE.LSX_DLR_RSV_CKAMT = :QLSX-DLR-RSV-CKAMT,
              LSXFILE.LSX_PL_EXPENSE = :QLSX-PL-EXPENSE,
              LSXFILE.LSX_DEFICIENCY_Y_N = :QLSX-DEFICIENCY-Y-N,
              LSXFILE.LSX_NET_BAL_DUE = :QLSX-NET-BAL-DUE,
              LSXFILE.LSX_DEFICIENCY_BAL = :QLSX-DEFICIENCY-BAL,
              LSXFILE.LSX_AUCTION_EXPENSE = :QLSX-AUCTION-EXPENSE,
              LSXFILE.LSX_REASON_CODE = :QLSX-REASON-CODE,
              LSXFILE.LSX_TRIM = :QLSX-TRIM,
              LSXFILE.LSX_DOORS = :QLSX-DOORS,
              LSXFILE.LSX_COLOR = :QLSX-COLOR,
              LSXFILE.LSX_CYLINDERS = :QLSX-CYLINDERS,
              LSXFILE.LSX_AUTO_TRANS = :QLSX-AUTO-TRANS,
              LSXFILE.LSX_AC = :QLSX-AC,
              LSXFILE.LSX_SROOF = :QLSX-SROOF,
              LSXFILE.LSX_LEATHER = :QLSX-LEATHER,
              LSXFILE.LSX_4WD = :QLSX-4WD,
              LSXFILE.LSX_DIESEL = :QLSX-DIESEL,
              LSXFILE.LSX_DUAL_AIR = :QLSX-DUAL-AIR,
              LSXFILE.LSX_EXT_CAB = :QLSX-EXT-CAB,
              LSXFILE.LSX_AGENT_PHONE = :QLSX-AGENT-PHONE,
              LSXFILE.LSX_AGENT_FAX = :QLSX-AGENT-FAX,
              LSXFILE.LSX_STORED_ADR2 = :QLSX-STORED-ADR2,
              LSXFILE.LSX_X_STORED_CITY = :QLSX-X-STORED-CITY,
              LSXFILE.LSX_X_STORED_STATE = :QLSX-X-STORED-STATE,
              LSXFILE.LSX_X_STORED_ZIP = :QLSX-X-STORED-ZIP,
              LSXFILE.LSX_INS_CLAIM_PENDING =
                   :QLSX-INS-CLAIM-PENDING,
              LSXFILE.LSX_WARRANTY_AVAILABLE =
                   :QLSX-WARRANTY-AVAILABLE,
              LSXFILE.LSX_INS_REBATE_REQUESTED =
                   :QLSX-INS-REBATE-REQUESTED,
              LSXFILE.LSX_EXTRACTED = :QLSX-EXTRACTED,
              LSXFILE.LSX_TITLE_SENT_DATE =
                 :QLSX-TITLE-SENT-DATE,
              LSXFILE.LSX_DISPOSAL_HOLD = :QLSX-DISPOSAL-HOLD,
              LSXFILE.LSX_NEXT_HLD_RVW_DATE = :QLSX-NEXT-HLD-RVW-DATE,
              LSXFILE.LSX_CASE_ID = :QLSX-CASE-ID,
              LSXFILE.LSX_COLLATERAL_LOCATION =
                   :QLSX-COLLATERAL-LOCATION,
              LSXFILE.LSX_VIN_MATCH = :QLSX-VIN-MATCH,
              LSXFILE.LSX_PERSONALS = :QLSX-PERSONALS,
              LSXFILE.LSX_KEYS = :QLSX-KEYS,
              LSXFILE.LSX_DRIVES = :QLSX-DRIVES,
              LSXFILE.LSX_DMG = :QLSX-DMG,
              LSXFILE.LSX_Y_ASSIGNED_TO = :QLSX-Y-ASSIGNED-TO,
              LSXFILE.LSX_SUPPLIER_ID = :QLSX-SUPPLIER-ID,
              LSXFILE.LSX_TRANSPORT_EMAIL = :QLSX-TRANSPORT-EMAIL,
              LSXFILE.LSX_STORED_LOCATION = :QLSX-STORED-LOCATION,
              LSXFILE.LSX_STORED_ADDRESS = :QLSX-STORED-ADDRESS,
              LSXFILE.LSX_Y_STORED_CITY = :QLSX-Y-STORED-CITY,
              LSXFILE.LSX_Y_STORED_STATE = :QLSX-Y-STORED-STATE,
              LSXFILE.LSX_Y_STORED_ZIP = :QLSX-Y-STORED-ZIP,
              LSXFILE.LSX_AGENCY_NAME = :QLSX-AGENCY-NAME,
              LSXFILE.LSX_AGENCY_ADDRESS = :QLSX-AGENCY-ADDRESS,
              LSXFILE.LSX_AGENCY_CITY = :QLSX-AGENCY-CITY,
              LSXFILE.LSX_AGENCY_STATE = :QLSX-AGENCY-STATE,
              LSXFILE.LSX_AGENCY_ZIP = :QLSX-AGENCY-ZIP,
              LSXFILE.LSX_AGENCY_PHONE = :QLSX-AGENCY-PHONE
             WHERE LSXFILE.LSX_BRNO = :QLSX-BRNO
               AND LSXFILE.LSX_ACCTNO = :QLSX-ACCTNO
               AND LSXFILE.LSX_SEQNO = :QLSX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LSX1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LSX1-FILE.

           MOVE LSX-PATH-OWNBR  TO LSX-BRNO.
           PERFORM DELETE-IT.
           MOVE LSX-PATH-SQL    TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.
           PERFORM SET-LSX-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LSXFILE
             WHERE LSXFILE.LSX_BRNO = :QLSX-BRNO
               AND LSXFILE.LSX_ACCTNO = :QLSX-ACCTNO
               AND LSXFILE.LSX_SEQNO = :QLSX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LSX1-FILE.

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

