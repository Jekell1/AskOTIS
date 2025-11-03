      * COPYMEMBER: LIBLP/LPDL2RN
       LOAD-DL2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( DL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( DL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO DL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO DL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO DL-ALL-DATA
              MOVE DL-ALL-PATH             TO DL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( DL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO DL-ALL-BASE
              MOVE FILPATH-MACHINE         TO DL-ALL-MACHINE
              MOVE FILPATH-DATA            TO DL-ALL-DATA
              MOVE DL-ALL-PATH             TO DL-PATH.

      *-----------------------------------------------------------------
       OPEN-DL2-FILE.
           PERFORM LOAD-DL2-FILE.
           PERFORM OPEN-IT.
           MOVE DL-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DL2-FILE.
           PERFORM START-IT.
           MOVE DL-PATH-OWNBR  TO DL-BRNO.
           MOVE DL-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DL-BRNO, DL-SHORT DELIMITED BY SIZE INTO E-KEYX.

           IF ( DL2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DL2-FILE.

           IF ( QDL2-WEND-SHORT = SPACES )
              MOVE DL-PATH-OWNBR        TO QDL2-WBEG-BRNO
              MOVE DL-PATH-OWNBR        TO QDL2-WEND-BRNO
              MOVE DL-SHORT             TO QDL2-WBEG-SHORT
              MOVE ALL "Z"              TO QDL2-WEND-SHORT.

           EXEC SQL
            DECLARE DL2_CURSOR CURSOR FOR
            SELECT DLFILE.DL_BRNO,
                   DLFILE.DL_NUM,
                   CAST(DLFILE.DL_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   DLFILE.DL_NAME,
                   DLFILE.DL_ADR1,
                   DLFILE.DL_ADR2,
                   DLFILE.DL_CITY,
                   DLFILE.DL_STATE,
                   DLFILE.DL_ZIP,
                   DLFILE.DL_SHORT,
                   DLFILE.DL_PHONE,
                   DLFILE.DL_PURPOSE,
                   DLFILE.DL_MFGCODE,
                   CAST(DLFILE.DL_ORIGDATE AS VARCHAR(10)),
                   CAST(DLFILE.DL_TERMDATE AS VARCHAR(10)),
                   DLFILE.DL_LC_CODE,
                   DLFILE.DL_FULLCEIL,
                   DLFILE.DL_PARTCEIL,
                   DLFILE.DL_NONCEIL,
                   DLFILE.DL_SIGN1,
                   DLFILE.DL_SIGN2,
                   DLFILE.DL_SIGN3,
                   DLFILE.DL_MAXPURTRM,
                   DLFILE.DL_MAXPURPER,
                   DLFILE.DL_EARNCD,
                   DLFILE.DL_EARNFAC,
                   DLFILE.DL_DISC_FRMLA,
                   DLFILE.DL_DISC_CODE,
                   DLFILE.DL_DISC_TERM_1,
                   DLFILE.DL_DISC_RATE_1,
                   DLFILE.DL_DISC_TERM_2,
                   DLFILE.DL_DISC_RATE_2,
                   DLFILE.DL_DISC_TERM_3,
                   DLFILE.DL_DISC_RATE_3,
                   DLFILE.DL_DISC_TERM_4,
                   DLFILE.DL_DISC_RATE_4,
                   DLFILE.DL_DISC_TERM_5,
                   DLFILE.DL_DISC_RATE_5,
                   DLFILE.DL_DISC_TERM_6,
                   DLFILE.DL_DISC_RATE_6,
                   DLFILE.DL_RVPRIN_FRMLA_1,
                   DLFILE.DL_RVPRIN_CODE_1,
                   DLFILE.DL_RVPRIN_ADDON_1,
                   DLFILE.DL_RVPRIN_TERM_1_1,
                   DLFILE.DL_RVPRIN_RATE_1_1,
                   DLFILE.DL_RVPRIN_TERM_1_2,
                   DLFILE.DL_RVPRIN_RATE_1_2,
                   DLFILE.DL_RVPRIN_TERM_1_3,
                   DLFILE.DL_RVPRIN_RATE_1_3,
                   DLFILE.DL_RVPRIN_TERM_1_4,
                   DLFILE.DL_RVPRIN_RATE_1_4,
                   DLFILE.DL_RVPRIN_TERM_1_5,
                   DLFILE.DL_RVPRIN_RATE_1_5,
                   DLFILE.DL_RVPRIN_TERM_1_6,
                   DLFILE.DL_RVPRIN_RATE_1_6,
                   DLFILE.DL_RVPRIN_FRMLA_2,
                   DLFILE.DL_RVPRIN_CODE_2,
                   DLFILE.DL_RVPRIN_ADDON_2,
                   DLFILE.DL_RVPRIN_TERM_2_1,
                   DLFILE.DL_RVPRIN_RATE_2_1,
                   DLFILE.DL_RVPRIN_TERM_2_2,
                   DLFILE.DL_RVPRIN_RATE_2_2,
                   DLFILE.DL_RVPRIN_TERM_2_3,
                   DLFILE.DL_RVPRIN_RATE_2_3,
                   DLFILE.DL_RVPRIN_TERM_2_4,
                   DLFILE.DL_RVPRIN_RATE_2_4,
                   DLFILE.DL_RVPRIN_TERM_2_5,
                   DLFILE.DL_RVPRIN_RATE_2_5,
                   DLFILE.DL_RVPRIN_TERM_2_6,
                   DLFILE.DL_RVPRIN_RATE_2_6,
                   DLFILE.DL_RVPRIN_FRMLA_3,
                   DLFILE.DL_RVPRIN_CODE_3,
                   DLFILE.DL_RVPRIN_ADDON_3,
                   DLFILE.DL_RVPRIN_TERM_3_1,
                   DLFILE.DL_RVPRIN_RATE_3_1,
                   DLFILE.DL_RVPRIN_TERM_3_2,
                   DLFILE.DL_RVPRIN_RATE_3_2,
                   DLFILE.DL_RVPRIN_TERM_3_3,
                   DLFILE.DL_RVPRIN_RATE_3_3,
                   DLFILE.DL_RVPRIN_TERM_3_4,
                   DLFILE.DL_RVPRIN_RATE_3_4,
                   DLFILE.DL_RVPRIN_TERM_3_5,
                   DLFILE.DL_RVPRIN_RATE_3_5,
                   DLFILE.DL_RVPRIN_TERM_3_6,
                   DLFILE.DL_RVPRIN_RATE_3_6,
                   DLFILE.DL_RVCHGS_FRMLA_1,
                   DLFILE.DL_RVCHGS_CODE_1,
                   DLFILE.DL_RVCHGS_ADDON_1,
                   DLFILE.DL_RVCHGS_TERM_1_1,
                   DLFILE.DL_RVCHGS_RATE_1_1,
                   DLFILE.DL_RVCHGS_TERM_1_2,
                   DLFILE.DL_RVCHGS_RATE_1_2,
                   DLFILE.DL_RVCHGS_TERM_1_3,
                   DLFILE.DL_RVCHGS_RATE_1_3,
                   DLFILE.DL_RVCHGS_TERM_1_4,
                   DLFILE.DL_RVCHGS_RATE_1_4,
                   DLFILE.DL_RVCHGS_TERM_1_5,
                   DLFILE.DL_RVCHGS_RATE_1_5,
                   DLFILE.DL_RVCHGS_TERM_1_6,
                   DLFILE.DL_RVCHGS_RATE_1_6,
                   DLFILE.DL_RVCHGS_FRMLA_2,
                   DLFILE.DL_RVCHGS_CODE_2,
                   DLFILE.DL_RVCHGS_ADDON_2,
                   DLFILE.DL_RVCHGS_TERM_2_1,
                   DLFILE.DL_RVCHGS_RATE_2_1,
                   DLFILE.DL_RVCHGS_TERM_2_2,
                   DLFILE.DL_RVCHGS_RATE_2_2,
                   DLFILE.DL_RVCHGS_TERM_2_3,
                   DLFILE.DL_RVCHGS_RATE_2_3,
                   DLFILE.DL_RVCHGS_TERM_2_4,
                   DLFILE.DL_RVCHGS_RATE_2_4,
                   DLFILE.DL_RVCHGS_TERM_2_5,
                   DLFILE.DL_RVCHGS_RATE_2_5,
                   DLFILE.DL_RVCHGS_TERM_2_6,
                   DLFILE.DL_RVCHGS_RATE_2_6,
                   DLFILE.DL_RVCHGS_FRMLA_3,
                   DLFILE.DL_RVCHGS_CODE_3,
                   DLFILE.DL_RVCHGS_ADDON_3,
                   DLFILE.DL_RVCHGS_TERM_3_1,
                   DLFILE.DL_RVCHGS_RATE_3_1,
                   DLFILE.DL_RVCHGS_TERM_3_2,
                   DLFILE.DL_RVCHGS_RATE_3_2,
                   DLFILE.DL_RVCHGS_TERM_3_3,
                   DLFILE.DL_RVCHGS_RATE_3_3,
                   DLFILE.DL_RVCHGS_TERM_3_4,
                   DLFILE.DL_RVCHGS_RATE_3_4,
                   DLFILE.DL_RVCHGS_TERM_3_5,
                   DLFILE.DL_RVCHGS_RATE_3_5,
                   DLFILE.DL_RVCHGS_TERM_3_6,
                   DLFILE.DL_RVCHGS_RATE_3_6,
                   DLFILE.DL_PARVHELD_FRMLA,
                   DLFILE.DL_PARVHELD_CODE,
                   DLFILE.DL_PARVHELD_ADDON,
                   DLFILE.DL_PARVHELD_TERM_1,
                   DLFILE.DL_PARVHELD_RATE_1,
                   DLFILE.DL_PARVHELD_TERM_2,
                   DLFILE.DL_PARVHELD_RATE_2,
                   DLFILE.DL_PARVHELD_TERM_3,
                   DLFILE.DL_PARVHELD_RATE_3,
                   DLFILE.DL_PARVHELD_TERM_4,
                   DLFILE.DL_PARVHELD_RATE_4,
                   DLFILE.DL_PARVHELD_TERM_5,
                   DLFILE.DL_PARVHELD_RATE_5,
                   DLFILE.DL_PARVHELD_TERM_6,
                   DLFILE.DL_PARVHELD_RATE_6,
                   DLFILE.DL_PARVPAID_FRMLA,
                   DLFILE.DL_PARVPAID_CODE,
                   DLFILE.DL_PARVPAID_ADDON,
                   DLFILE.DL_PARVPAID_TERM_1,
                   DLFILE.DL_PARVPAID_RATE_1,
                   DLFILE.DL_PARVPAID_TERM_2,
                   DLFILE.DL_PARVPAID_RATE_2,
                   DLFILE.DL_PARVPAID_TERM_3,
                   DLFILE.DL_PARVPAID_RATE_3,
                   DLFILE.DL_PARVPAID_TERM_4,
                   DLFILE.DL_PARVPAID_RATE_4,
                   DLFILE.DL_PARVPAID_TERM_5,
                   DLFILE.DL_PARVPAID_RATE_5,
                   DLFILE.DL_PARVPAID_TERM_6,
                   DLFILE.DL_PARVPAID_RATE_6,
                   DLFILE.DL_SVACCT_FRMLA,
                   DLFILE.DL_SVCODE,
                   DLFILE.DL_SVACCT_AMT,
                   DLFILE.DL_SVACCT_RATE,
                   DLFILE.DL_SVACCT_LCHGPER,
                   DLFILE.DL_SVACCT_INTPER,
                   DLFILE.DL_SVACCT_DEFPER,
                   DLFILE.DL_FULLCEIL_BAL,
                   DLFILE.DL_PARTCEIL_BAL,
                   DLFILE.DL_NONCEIL_BAL,
                   DLFILE.DL_AFP_DEALER,
                   DLFILE.DL_RECOURSE_FRMLA,
                   DLFILE.DL_FAX_NUMBER,
                   DLFILE.DL_PARVPAID_FRMLA_IB,
                   DLFILE.DL_PARVPAID_CODE_IB,
                   DLFILE.DL_LPO,
                   DLFILE.DL_ACH_FG,
                   DLFILE.DL_BANK_ROUTING_NO,
                   DLFILE.DL_BANK_ACCTNO,
                   DLFILE.DL_ACH_PRENOTE_FG,
                   DLFILE.DL_SAL_MISC_PCT,
                   DLFILE.DL_SAL_MISC_PCT2,
                   DLFILE.DL_WAIVE_ACH_FEE,
                   DLFILE.DL_TRACK_CHANGE_FLAG,
                   DLFILE.DL_DBA_NAME
            FROM DBO.DLFILE
            WHERE DLFILE.DL_BRNO   = :QDL2-WBEG-BRNO
              AND DLFILE.DL_SHORT >= :QDL2-WBEG-SHORT
              AND DLFILE.DL_SHORT <= :QDL2-WEND-SHORT
            ORDER BY DLFILE.DL_BRNO,
                     DLFILE.DL_SHORT,
                     DLFILE.DL_NUM
           END-EXEC.

           EXEC SQL
               OPEN DL2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDL2-WBEG-BRNO 
                          QDL2-WEND-BRNO.
           MOVE SPACES TO QDL2-WBEG-SHORT
                          QDL2-WEND-SHORT.

           MOVE STAT---GOOD TO DL2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DL2-FILE.
           PERFORM READ-IT.
           MOVE DL-PATH-OWNBR  TO DL-BRNO. 
           MOVE DL-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DL-BRNO, DL-SHORT DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QDL-REC.

           MOVE DL-BRNO   TO QDL-BRNO.
           MOVE DL-SHORT   TO QDL-SHORT.

           EXEC SQL
            SELECT DLFILE.DL_BRNO,
                   DLFILE.DL_NUM,
                   CAST(DLFILE.DL_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   DLFILE.DL_NAME,
                   DLFILE.DL_ADR1,
                   DLFILE.DL_ADR2,
                   DLFILE.DL_CITY,
                   DLFILE.DL_STATE,
                   DLFILE.DL_ZIP,
                   DLFILE.DL_SHORT,
                   DLFILE.DL_PHONE,
                   DLFILE.DL_PURPOSE,
                   DLFILE.DL_MFGCODE,
                   CAST(DLFILE.DL_ORIGDATE AS VARCHAR(10)),
                   CAST(DLFILE.DL_TERMDATE AS VARCHAR(10)),
                   DLFILE.DL_LC_CODE,
                   DLFILE.DL_FULLCEIL,
                   DLFILE.DL_PARTCEIL,
                   DLFILE.DL_NONCEIL,
                   DLFILE.DL_SIGN1,
                   DLFILE.DL_SIGN2,
                   DLFILE.DL_SIGN3,
                   DLFILE.DL_MAXPURTRM,
                   DLFILE.DL_MAXPURPER,
                   DLFILE.DL_EARNCD,
                   DLFILE.DL_EARNFAC,
                   DLFILE.DL_DISC_FRMLA,
                   DLFILE.DL_DISC_CODE,
                   DLFILE.DL_DISC_TERM_1,
                   DLFILE.DL_DISC_RATE_1,
                   DLFILE.DL_DISC_TERM_2,
                   DLFILE.DL_DISC_RATE_2,
                   DLFILE.DL_DISC_TERM_3,
                   DLFILE.DL_DISC_RATE_3,
                   DLFILE.DL_DISC_TERM_4,
                   DLFILE.DL_DISC_RATE_4,
                   DLFILE.DL_DISC_TERM_5,
                   DLFILE.DL_DISC_RATE_5,
                   DLFILE.DL_DISC_TERM_6,
                   DLFILE.DL_DISC_RATE_6,
                   DLFILE.DL_RVPRIN_FRMLA_1,
                   DLFILE.DL_RVPRIN_CODE_1,
                   DLFILE.DL_RVPRIN_ADDON_1,
                   DLFILE.DL_RVPRIN_TERM_1_1,
                   DLFILE.DL_RVPRIN_RATE_1_1,
                   DLFILE.DL_RVPRIN_TERM_1_2,
                   DLFILE.DL_RVPRIN_RATE_1_2,
                   DLFILE.DL_RVPRIN_TERM_1_3,
                   DLFILE.DL_RVPRIN_RATE_1_3,
                   DLFILE.DL_RVPRIN_TERM_1_4,
                   DLFILE.DL_RVPRIN_RATE_1_4,
                   DLFILE.DL_RVPRIN_TERM_1_5,
                   DLFILE.DL_RVPRIN_RATE_1_5,
                   DLFILE.DL_RVPRIN_TERM_1_6,
                   DLFILE.DL_RVPRIN_RATE_1_6,
                   DLFILE.DL_RVPRIN_FRMLA_2,
                   DLFILE.DL_RVPRIN_CODE_2,
                   DLFILE.DL_RVPRIN_ADDON_2,
                   DLFILE.DL_RVPRIN_TERM_2_1,
                   DLFILE.DL_RVPRIN_RATE_2_1,
                   DLFILE.DL_RVPRIN_TERM_2_2,
                   DLFILE.DL_RVPRIN_RATE_2_2,
                   DLFILE.DL_RVPRIN_TERM_2_3,
                   DLFILE.DL_RVPRIN_RATE_2_3,
                   DLFILE.DL_RVPRIN_TERM_2_4,
                   DLFILE.DL_RVPRIN_RATE_2_4,
                   DLFILE.DL_RVPRIN_TERM_2_5,
                   DLFILE.DL_RVPRIN_RATE_2_5,
                   DLFILE.DL_RVPRIN_TERM_2_6,
                   DLFILE.DL_RVPRIN_RATE_2_6,
                   DLFILE.DL_RVPRIN_FRMLA_3,
                   DLFILE.DL_RVPRIN_CODE_3,
                   DLFILE.DL_RVPRIN_ADDON_3,
                   DLFILE.DL_RVPRIN_TERM_3_1,
                   DLFILE.DL_RVPRIN_RATE_3_1,
                   DLFILE.DL_RVPRIN_TERM_3_2,
                   DLFILE.DL_RVPRIN_RATE_3_2,
                   DLFILE.DL_RVPRIN_TERM_3_3,
                   DLFILE.DL_RVPRIN_RATE_3_3,
                   DLFILE.DL_RVPRIN_TERM_3_4,
                   DLFILE.DL_RVPRIN_RATE_3_4,
                   DLFILE.DL_RVPRIN_TERM_3_5,
                   DLFILE.DL_RVPRIN_RATE_3_5,
                   DLFILE.DL_RVPRIN_TERM_3_6,
                   DLFILE.DL_RVPRIN_RATE_3_6,
                   DLFILE.DL_RVCHGS_FRMLA_1,
                   DLFILE.DL_RVCHGS_CODE_1,
                   DLFILE.DL_RVCHGS_ADDON_1,
                   DLFILE.DL_RVCHGS_TERM_1_1,
                   DLFILE.DL_RVCHGS_RATE_1_1,
                   DLFILE.DL_RVCHGS_TERM_1_2,
                   DLFILE.DL_RVCHGS_RATE_1_2,
                   DLFILE.DL_RVCHGS_TERM_1_3,
                   DLFILE.DL_RVCHGS_RATE_1_3,
                   DLFILE.DL_RVCHGS_TERM_1_4,
                   DLFILE.DL_RVCHGS_RATE_1_4,
                   DLFILE.DL_RVCHGS_TERM_1_5,
                   DLFILE.DL_RVCHGS_RATE_1_5,
                   DLFILE.DL_RVCHGS_TERM_1_6,
                   DLFILE.DL_RVCHGS_RATE_1_6,
                   DLFILE.DL_RVCHGS_FRMLA_2,
                   DLFILE.DL_RVCHGS_CODE_2,
                   DLFILE.DL_RVCHGS_ADDON_2,
                   DLFILE.DL_RVCHGS_TERM_2_1,
                   DLFILE.DL_RVCHGS_RATE_2_1,
                   DLFILE.DL_RVCHGS_TERM_2_2,
                   DLFILE.DL_RVCHGS_RATE_2_2,
                   DLFILE.DL_RVCHGS_TERM_2_3,
                   DLFILE.DL_RVCHGS_RATE_2_3,
                   DLFILE.DL_RVCHGS_TERM_2_4,
                   DLFILE.DL_RVCHGS_RATE_2_4,
                   DLFILE.DL_RVCHGS_TERM_2_5,
                   DLFILE.DL_RVCHGS_RATE_2_5,
                   DLFILE.DL_RVCHGS_TERM_2_6,
                   DLFILE.DL_RVCHGS_RATE_2_6,
                   DLFILE.DL_RVCHGS_FRMLA_3,
                   DLFILE.DL_RVCHGS_CODE_3,
                   DLFILE.DL_RVCHGS_ADDON_3,
                   DLFILE.DL_RVCHGS_TERM_3_1,
                   DLFILE.DL_RVCHGS_RATE_3_1,
                   DLFILE.DL_RVCHGS_TERM_3_2,
                   DLFILE.DL_RVCHGS_RATE_3_2,
                   DLFILE.DL_RVCHGS_TERM_3_3,
                   DLFILE.DL_RVCHGS_RATE_3_3,
                   DLFILE.DL_RVCHGS_TERM_3_4,
                   DLFILE.DL_RVCHGS_RATE_3_4,
                   DLFILE.DL_RVCHGS_TERM_3_5,
                   DLFILE.DL_RVCHGS_RATE_3_5,
                   DLFILE.DL_RVCHGS_TERM_3_6,
                   DLFILE.DL_RVCHGS_RATE_3_6,
                   DLFILE.DL_PARVHELD_FRMLA,
                   DLFILE.DL_PARVHELD_CODE,
                   DLFILE.DL_PARVHELD_ADDON,
                   DLFILE.DL_PARVHELD_TERM_1,
                   DLFILE.DL_PARVHELD_RATE_1,
                   DLFILE.DL_PARVHELD_TERM_2,
                   DLFILE.DL_PARVHELD_RATE_2,
                   DLFILE.DL_PARVHELD_TERM_3,
                   DLFILE.DL_PARVHELD_RATE_3,
                   DLFILE.DL_PARVHELD_TERM_4,
                   DLFILE.DL_PARVHELD_RATE_4,
                   DLFILE.DL_PARVHELD_TERM_5,
                   DLFILE.DL_PARVHELD_RATE_5,
                   DLFILE.DL_PARVHELD_TERM_6,
                   DLFILE.DL_PARVHELD_RATE_6,
                   DLFILE.DL_PARVPAID_FRMLA,
                   DLFILE.DL_PARVPAID_CODE,
                   DLFILE.DL_PARVPAID_ADDON,
                   DLFILE.DL_PARVPAID_TERM_1,
                   DLFILE.DL_PARVPAID_RATE_1,
                   DLFILE.DL_PARVPAID_TERM_2,
                   DLFILE.DL_PARVPAID_RATE_2,
                   DLFILE.DL_PARVPAID_TERM_3,
                   DLFILE.DL_PARVPAID_RATE_3,
                   DLFILE.DL_PARVPAID_TERM_4,
                   DLFILE.DL_PARVPAID_RATE_4,
                   DLFILE.DL_PARVPAID_TERM_5,
                   DLFILE.DL_PARVPAID_RATE_5,
                   DLFILE.DL_PARVPAID_TERM_6,
                   DLFILE.DL_PARVPAID_RATE_6,
                   DLFILE.DL_SVACCT_FRMLA,
                   DLFILE.DL_SVCODE,
                   DLFILE.DL_SVACCT_AMT,
                   DLFILE.DL_SVACCT_RATE,
                   DLFILE.DL_SVACCT_LCHGPER,
                   DLFILE.DL_SVACCT_INTPER,
                   DLFILE.DL_SVACCT_DEFPER,
                   DLFILE.DL_FULLCEIL_BAL,
                   DLFILE.DL_PARTCEIL_BAL,
                   DLFILE.DL_NONCEIL_BAL,
                   DLFILE.DL_AFP_DEALER,
                   DLFILE.DL_RECOURSE_FRMLA,
                   DLFILE.DL_FAX_NUMBER,
                   DLFILE.DL_PARVPAID_FRMLA_IB,
                   DLFILE.DL_PARVPAID_CODE_IB,
                   DLFILE.DL_LPO,
                   DLFILE.DL_ACH_FG,
                   DLFILE.DL_BANK_ROUTING_NO,
                   DLFILE.DL_BANK_ACCTNO,
                   DLFILE.DL_ACH_PRENOTE_FG,
                   DLFILE.DL_SAL_MISC_PCT,
                   DLFILE.DL_SAL_MISC_PCT2,
                   DLFILE.DL_WAIVE_ACH_FEE,
                   DLFILE.DL_TRACK_CHANGE_FLAG,
                   DLFILE.DL_DBA_NAME
            INTO :QDL-REC
            FROM DBO.DLFILE
            WHERE DLFILE.DL_BRNO = :QDL-BRNO
              AND DLFILE.DL_SHORT = :QDL-SHORT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DL2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DL-FIELDS.

      *-----------------------------------------------------------------
       READ-DL2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DL-PATH-OWNBR  TO DL-BRNO.
           MOVE DL-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DL-BRNO, DL-SHORT DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QDL-REC.

           IF ( DL2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DL2_CURSOR INTO :QDL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DL2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-DL2-FILE.

           PERFORM CLOSE-IT.
           IF ( DL2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DL2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DL2-CURSOR-STAT.

