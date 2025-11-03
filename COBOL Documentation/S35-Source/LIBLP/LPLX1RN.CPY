      * COPYMEMBER: LIBLP/LPLX1RN
       LOAD-LX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LX-ALL-DATA
              MOVE LX-ALL-PATH             TO LX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LX-ALL-BASE
              MOVE FILPATH-MACHINE         TO LX-ALL-MACHINE
              MOVE FILPATH-DATA            TO LX-ALL-DATA
              MOVE LX-ALL-PATH             TO LX-PATH.

      *-----------------------------------------------------------------
       OPEN-LX1-FILE.
           PERFORM LOAD-LX1-FILE.
           PERFORM OPEN-IT.
           MOVE LX-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LX1-FILE.
           PERFORM START-IT.
           MOVE LX-PATH-OWNBR  TO LX-BRNO.
           MOVE LX-PATH-SQL    TO E-FILE.
           MOVE LX1-KEY        TO E-KEYX.

           IF ( LX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LX1-FILE.

           IF ( QLX1-WEND-SEQNO = ZEROES    ) OR
              ( QLX1-WEND-SEQNO NOT NUMERIC )
              MOVE LX-PATH-OWNBR        TO QLX1-WBEG-BRNO
              MOVE LX-PATH-OWNBR        TO QLX1-WEND-BRNO
              MOVE LX-ACCTNO            TO QLX1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLX1-WEND-ACCTNO
              MOVE LX-SEQNO             TO QLX1-WBEG-SEQNO
              MOVE ALL "9"              TO QLX1-WEND-SEQNO.

           EXEC SQL
            DECLARE LX1_CURSOR CURSOR FOR
            SELECT LXFILE.LX_BRNO,
                   LXFILE.LX_ACCTNO,
                   LXFILE.LX_SEQNO,
                   CAST(LXFILE.LX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LXFILE.LXT_CODE,
                   LXFILE.LXT_EARN_1,
                   LXFILE.LXT_EARN_2,
                   LXFILE.LXT_EARN_3,
                   LXFILE.LXT_EARN_4,
                   LXFILE.LXT_EARN_5,
                   LXFILE.LXT_EARN_6,
                   LXFILE.LXT_EARN_7,
                   LXFILE.LXT_EARN_8,
                   LXFILE.LXT_EARN_9,
                   LXFILE.LXT_EARN_10,
                   LXFILE.LXT_EARN_11,
                   LXFILE.LXT_EARN_12,
                   LXFILE.LXT_EARN_13,
                   LXFILE.LXT_EARN_14,
                   LXFILE.LXT_EARN_15,
                   LXFILE.LXT_EARN_16,
                   LXFILE.LXT_EARN_17,
                   LXFILE.LXT_EARN_18,
                   LXFILE.LXT_EARN_19,
                   LXFILE.LXT_EARN_20,
                   LXFILE.LXP_CODE,
                   LXFILE.LXP_EARN_1,
                   LXFILE.LXP_EARN_2,
                   LXFILE.LXP_EARN_3,
                   LXFILE.LXP_EARN_4,
                   LXFILE.LXP_EARN_5,
                   LXFILE.LXP_EARN_6,
                   LXFILE.LXP_EARN_7,
                   LXFILE.LXP_EARN_8,
                   LXFILE.LXP_EARN_9,
                   LXFILE.LXP_EARN_10,
                   LXFILE.LXP_EARN_11,
                   LXFILE.LXP_EARN_12,
                   LXFILE.LXP_EARN_13,
                   LXFILE.LXP_EARN_14,
                   LXFILE.LXP_EARN_15,
                   LXFILE.LXP_EARN_16,
                   LXFILE.LXP_EARN_17,
                   LXFILE.LXP_EARN_18,
                   LXFILE.LXP_EARN_19,
                   LXFILE.LXP_EARN_20,
                   LXFILE.LXA_CODE,
                   LXFILE.LXA_SRCD,
                   LXFILE.LXA_START_PMT_1,
                   LXFILE.LXA_END_PMT_1,
                   LXFILE.LXA_NET_CHANGE_1,
                   LXFILE.LXA_START_PMT_2,
                   LXFILE.LXA_END_PMT_2,
                   LXFILE.LXA_NET_CHANGE_2,
                   LXFILE.LXA_START_PMT_3,
                   LXFILE.LXA_END_PMT_3,
                   LXFILE.LXA_NET_CHANGE_3,
                   LXFILE.LXA_START_PMT_4,
                   LXFILE.LXA_END_PMT_4,
                   LXFILE.LXA_NET_CHANGE_4,
                   LXFILE.LXA_START_PMT_5,
                   LXFILE.LXA_END_PMT_5,
                   LXFILE.LXA_NET_CHANGE_5,
                   LXFILE.LXA_START_PMT_6,
                   LXFILE.LXA_END_PMT_6,
                   LXFILE.LXA_NET_CHANGE_6,
                   LXFILE.LXA_START_PMT_7,
                   LXFILE.LXA_END_PMT_7,
                   LXFILE.LXA_NET_CHANGE_7,
                   LXFILE.LXA_START_PMT_8,
                   LXFILE.LXA_END_PMT_8,
                   LXFILE.LXA_NET_CHANGE_8,
                   LXFILE.LXA_START_PMT_9,
                   LXFILE.LXA_END_PMT_9,
                   LXFILE.LXA_NET_CHANGE_9,
                   LXFILE.LXA_LN_INSCOMP,
                   LXFILE.LXA_LN_INSOURS,
                   CAST(LXFILE.LXA_LN_INSEFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(LXFILE.LXA_LN_INSEXP_DATE
                                                  AS VARCHAR(10)),
                   LXFILE.LXA_LN_INSPREM,
                   LXFILE.LXA_LN_INSCOVR,
                   LXFILE.LXA_LN_INSCOMM,
                   LXFILE.LXA_LN_INS_REBATE,
                   LXFILE.LXA_LN_INSCODES,
                   LXFILE.LXZ_CODE,
                   LXFILE.LXZ_POST_REGPY,
                   LXFILE.LXZ_POST_EXTRA_PRIN,
                   LXFILE.LXZ_POST_OTH,
                   LXFILE.LXZ_POST_LCHG,
                   LXFILE.LXZ_PDUE_APINT,
                   LXFILE.LXZ_PDUE_APPRIN,
                   LXFILE.LXZ_CUR_APINT,
                   LXFILE.LXZ_CUR_APPRIN,
                   LXFILE.LXR_CODE,
                   LXFILE.LXR_REB_AMT,
                   LXFILE.LXX_CODE,
                   LXFILE.LXX_INS_TAX_COUNTY_CD,
                   LXFILE.LXX_INS_TAX_CITY_CD,
                   LXFILE.LXM_CODE,
                   LXFILE.LXM_MIP_CLPAID,
                   CAST(LXFILE.LXM_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),
                   LXFILE.LXM_MIP_AHPAID,
                   CAST(LXFILE.LXM_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10))
            FROM DBO.LXFILE
            WHERE LXFILE.LX_BRNO  = :QLX1-WBEG-BRNO
              AND LXFILE.LX_ACCTNO >= :QLX1-WBEG-ACCTNO
              AND LXFILE.LX_ACCTNO <= :QLX1-WEND-ACCTNO
              AND LXFILE.LX_SEQNO >= :QLX1-WBEG-SEQNO
              AND LXFILE.LX_SEQNO <= :QLX1-WEND-SEQNO
            ORDER BY LXFILE.LX_BRNO,
                     LXFILE.LX_ACCTNO,
                     LXFILE.LX_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLX1-WBEG-BRNO
                          QLX1-WEND-BRNO
                          QLX1-WBEG-ACCTNO
                          QLX1-WEND-ACCTNO
                          QLX1-WBEG-SEQNO
                          QLX1-WEND-SEQNO.

           MOVE STAT---GOOD TO LX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LX1-FILE.
           PERFORM READ-IT.
           MOVE LX-PATH-OWNBR  TO LX-BRNO. 
           MOVE LX-PATH-SQL    TO E-FILE.
           MOVE LX1-KEY        TO E-KEYX.
           INITIALIZE QLX-REC.

           MOVE LX-BRNO   TO QLX-BRNO.
           MOVE LX-ACCTNO TO QLX-ACCTNO.
           MOVE LX-SEQNO TO QLX-SEQNO.

           EXEC SQL
            SELECT LXFILE.LX_BRNO,
                   LXFILE.LX_ACCTNO,
                   LXFILE.LX_SEQNO,
                   CAST(LXFILE.LX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LXFILE.LXT_CODE,
                   LXFILE.LXT_EARN_1,
                   LXFILE.LXT_EARN_2,
                   LXFILE.LXT_EARN_3,
                   LXFILE.LXT_EARN_4,
                   LXFILE.LXT_EARN_5,
                   LXFILE.LXT_EARN_6,
                   LXFILE.LXT_EARN_7,
                   LXFILE.LXT_EARN_8,
                   LXFILE.LXT_EARN_9,
                   LXFILE.LXT_EARN_10,
                   LXFILE.LXT_EARN_11,
                   LXFILE.LXT_EARN_12,
                   LXFILE.LXT_EARN_13,
                   LXFILE.LXT_EARN_14,
                   LXFILE.LXT_EARN_15,
                   LXFILE.LXT_EARN_16,
                   LXFILE.LXT_EARN_17,
                   LXFILE.LXT_EARN_18,
                   LXFILE.LXT_EARN_19,
                   LXFILE.LXT_EARN_20,
                   LXFILE.LXP_CODE,
                   LXFILE.LXP_EARN_1,
                   LXFILE.LXP_EARN_2,
                   LXFILE.LXP_EARN_3,
                   LXFILE.LXP_EARN_4,
                   LXFILE.LXP_EARN_5,
                   LXFILE.LXP_EARN_6,
                   LXFILE.LXP_EARN_7,
                   LXFILE.LXP_EARN_8,
                   LXFILE.LXP_EARN_9,
                   LXFILE.LXP_EARN_10,
                   LXFILE.LXP_EARN_11,
                   LXFILE.LXP_EARN_12,
                   LXFILE.LXP_EARN_13,
                   LXFILE.LXP_EARN_14,
                   LXFILE.LXP_EARN_15,
                   LXFILE.LXP_EARN_16,
                   LXFILE.LXP_EARN_17,
                   LXFILE.LXP_EARN_18,
                   LXFILE.LXP_EARN_19,
                   LXFILE.LXP_EARN_20,
                   LXFILE.LXA_CODE,
                   LXFILE.LXA_SRCD,
                   LXFILE.LXA_START_PMT_1,
                   LXFILE.LXA_END_PMT_1,
                   LXFILE.LXA_NET_CHANGE_1,
                   LXFILE.LXA_START_PMT_2,
                   LXFILE.LXA_END_PMT_2,
                   LXFILE.LXA_NET_CHANGE_2,
                   LXFILE.LXA_START_PMT_3,
                   LXFILE.LXA_END_PMT_3,
                   LXFILE.LXA_NET_CHANGE_3,
                   LXFILE.LXA_START_PMT_4,
                   LXFILE.LXA_END_PMT_4,
                   LXFILE.LXA_NET_CHANGE_4,
                   LXFILE.LXA_START_PMT_5,
                   LXFILE.LXA_END_PMT_5,
                   LXFILE.LXA_NET_CHANGE_5,
                   LXFILE.LXA_START_PMT_6,
                   LXFILE.LXA_END_PMT_6,
                   LXFILE.LXA_NET_CHANGE_6,
                   LXFILE.LXA_START_PMT_7,
                   LXFILE.LXA_END_PMT_7,
                   LXFILE.LXA_NET_CHANGE_7,
                   LXFILE.LXA_START_PMT_8,
                   LXFILE.LXA_END_PMT_8,
                   LXFILE.LXA_NET_CHANGE_8,
                   LXFILE.LXA_START_PMT_9,
                   LXFILE.LXA_END_PMT_9,
                   LXFILE.LXA_NET_CHANGE_9,
                   LXFILE.LXA_LN_INSCOMP,
                   LXFILE.LXA_LN_INSOURS,
                   CAST(LXFILE.LXA_LN_INSEFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(LXFILE.LXA_LN_INSEXP_DATE
                                                  AS VARCHAR(10)),
                   LXFILE.LXA_LN_INSPREM,
                   LXFILE.LXA_LN_INSCOVR,
                   LXFILE.LXA_LN_INSCOMM,
                   LXFILE.LXA_LN_INS_REBATE,
                   LXFILE.LXA_LN_INSCODES,
                   LXFILE.LXZ_CODE,
                   LXFILE.LXZ_POST_REGPY,
                   LXFILE.LXZ_POST_EXTRA_PRIN,
                   LXFILE.LXZ_POST_OTH,
                   LXFILE.LXZ_POST_LCHG,
                   LXFILE.LXZ_PDUE_APINT,
                   LXFILE.LXZ_PDUE_APPRIN,
                   LXFILE.LXZ_CUR_APINT,
                   LXFILE.LXZ_CUR_APPRIN,
                   LXFILE.LXR_CODE,
                   LXFILE.LXR_REB_AMT,
                   LXFILE.LXX_CODE,
                   LXFILE.LXX_INS_TAX_COUNTY_CD,
                   LXFILE.LXX_INS_TAX_CITY_CD,
                   LXFILE.LXM_CODE,
                   LXFILE.LXM_MIP_CLPAID,
                   CAST(LXFILE.LXM_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),
                   LXFILE.LXM_MIP_AHPAID,
                   CAST(LXFILE.LXM_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10))
            INTO :QLX-REC
            FROM DBO.LXFILE
            WHERE LXFILE.LX_BRNO = :QLX-BRNO
              AND LXFILE.LX_ACCTNO = :QLX-ACCTNO
              AND LXFILE.LX_SEQNO = :QLX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LX-FIELDS.

      *-----------------------------------------------------------------
       READ-LX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LX-PATH-OWNBR  TO LX-BRNO.
           MOVE LX-PATH-SQL    TO E-FILE.
           MOVE LX1-KEY        TO E-KEYX.
           INITIALIZE QLX-REC.

           IF ( LX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LX1_CURSOR INTO :QLX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LX1-FILE.

           PERFORM CLOSE-IT.
           IF ( LX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LX1-CURSOR-STAT.

