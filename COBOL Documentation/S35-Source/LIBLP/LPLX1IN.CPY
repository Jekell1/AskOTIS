      * COPYMEMBER: LIBLP/LPLX1IN
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
       WRITE-LX1-FILE.

           MOVE LX-PATH-OWNBR  TO LX-BRNO.
           ACCEPT LX-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LX-PATH-SQL    TO E-FILE.
           MOVE LX1-KEY        TO E-KEYX.
           PERFORM SET-LX-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LXFILE
             (LXFILE.LX_BRNO,
              LXFILE.LX_ACCTNO,
              LXFILE.LX_SEQNO,
              LXFILE.LX_LTOUCH_DATE,
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
              LXFILE.LXA_LN_INSEFF_DATE,
              LXFILE.LXA_LN_INSEXP_DATE,
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
              LXFILE.LXM_MIP_CLPDTH_DATE,
              LXFILE.LXM_MIP_AHPAID,
              LXFILE.LXM_MIP_AHPDTH_DATE)
            VALUES
             (:QLX-BRNO,
              :QLX-ACCTNO,
              :QLX-SEQNO,
              :QLX-LTOUCH-DATE,
              :QLXT-CODE,
              :QLXT-EARN-1,
              :QLXT-EARN-2,
              :QLXT-EARN-3,
              :QLXT-EARN-4,
              :QLXT-EARN-5,
              :QLXT-EARN-6,
              :QLXT-EARN-7,
              :QLXT-EARN-8,
              :QLXT-EARN-9,
              :QLXT-EARN-10,
              :QLXT-EARN-11,
              :QLXT-EARN-12,
              :QLXT-EARN-13,
              :QLXT-EARN-14,
              :QLXT-EARN-15,
              :QLXT-EARN-16,
              :QLXT-EARN-17,
              :QLXT-EARN-18,
              :QLXT-EARN-19,
              :QLXT-EARN-20,
              :QLXP-CODE,
              :QLXP-EARN-1,
              :QLXP-EARN-2,
              :QLXP-EARN-3,
              :QLXP-EARN-4,
              :QLXP-EARN-5,
              :QLXP-EARN-6,
              :QLXP-EARN-7,
              :QLXP-EARN-8,
              :QLXP-EARN-9,
              :QLXP-EARN-10,
              :QLXP-EARN-11,
              :QLXP-EARN-12,
              :QLXP-EARN-13,
              :QLXP-EARN-14,
              :QLXP-EARN-15,
              :QLXP-EARN-16,
              :QLXP-EARN-17,
              :QLXP-EARN-18,
              :QLXP-EARN-19,
              :QLXP-EARN-20,
              :QLXA-CODE,
              :QLXA-SRCD,
              :QLXA-START-PMT-1,
              :QLXA-END-PMT-1,
              :QLXA-NET-CHANGE-1,
              :QLXA-START-PMT-2,
              :QLXA-END-PMT-2,
              :QLXA-NET-CHANGE-2,
              :QLXA-START-PMT-3,
              :QLXA-END-PMT-3,
              :QLXA-NET-CHANGE-3,
              :QLXA-START-PMT-4,
              :QLXA-END-PMT-4,
              :QLXA-NET-CHANGE-4,
              :QLXA-START-PMT-5,
              :QLXA-END-PMT-5,
              :QLXA-NET-CHANGE-5,
              :QLXA-START-PMT-6,
              :QLXA-END-PMT-6,
              :QLXA-NET-CHANGE-6,
              :QLXA-START-PMT-7,
              :QLXA-END-PMT-7,
              :QLXA-NET-CHANGE-7,
              :QLXA-START-PMT-8,
              :QLXA-END-PMT-8,
              :QLXA-NET-CHANGE-8,
              :QLXA-START-PMT-9,
              :QLXA-END-PMT-9,
              :QLXA-NET-CHANGE-9,
              :QLXA-LN-INSCOMP,
              :QLXA-LN-INSOURS,
              :QLXA-LN-INSEFF-DATE,
              :QLXA-LN-INSEXP-DATE,
              :QLXA-LN-INSPREM,
              :QLXA-LN-INSCOVR,
              :QLXA-LN-INSCOMM,
              :QLXA-LN-INS-REBATE,
              :QLXA-LN-INSCODES,
              :QLXZ-CODE,
              :QLXZ-POST-REGPY,
              :QLXZ-POST-EXTRA-PRIN,
              :QLXZ-POST-OTH,
              :QLXZ-POST-LCHG,
              :QLXZ-PDUE-APINT,
              :QLXZ-PDUE-APPRIN,
              :QLXZ-CUR-APINT,
              :QLXZ-CUR-APPRIN,
              :QLXR-CODE,
              :QLXR-REB-AMT,
              :QLXX-CODE,
              :QLXX-INS-TAX-COUNTY-CD,
              :QLXX-INS-TAX-CITY-CD,
              :QLXM-CODE,
              :QLXM-MIP-CLPAID,
              :QLXM-MIP-CLPDTH-DATE,
              :QLXM-MIP-AHPAID,
              :QLXM-MIP-AHPDTH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LX1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LX1-FILE.

           MOVE LX-PATH-OWNBR  TO LX-BRNO.
           ACCEPT LX-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LX-PATH-SQL    TO E-FILE.
           MOVE LX1-KEY        TO E-KEYX.
           PERFORM SET-LX-FIELDS.

           EXEC SQL
            UPDATE DBO.LXFILE
             SET
              LXFILE.LX_BRNO = :QLX-BRNO,
              LXFILE.LX_ACCTNO = :QLX-ACCTNO,
              LXFILE.LX_SEQNO = :QLX-SEQNO,
              LXFILE.LX_LTOUCH_DATE =
                 :QLX-LTOUCH-DATE,
              LXFILE.LXT_CODE = :QLXT-CODE,
              LXFILE.LXT_EARN_1 = :QLXT-EARN-1,
              LXFILE.LXT_EARN_2 = :QLXT-EARN-2,
              LXFILE.LXT_EARN_3 = :QLXT-EARN-3,
              LXFILE.LXT_EARN_4 = :QLXT-EARN-4,
              LXFILE.LXT_EARN_5 = :QLXT-EARN-5,
              LXFILE.LXT_EARN_6 = :QLXT-EARN-6,
              LXFILE.LXT_EARN_7 = :QLXT-EARN-7,
              LXFILE.LXT_EARN_8 = :QLXT-EARN-8,
              LXFILE.LXT_EARN_9 = :QLXT-EARN-9,
              LXFILE.LXT_EARN_10 = :QLXT-EARN-10,
              LXFILE.LXT_EARN_11 = :QLXT-EARN-11,
              LXFILE.LXT_EARN_12 = :QLXT-EARN-12,
              LXFILE.LXT_EARN_13 = :QLXT-EARN-13,
              LXFILE.LXT_EARN_14 = :QLXT-EARN-14,
              LXFILE.LXT_EARN_15 = :QLXT-EARN-15,
              LXFILE.LXT_EARN_16 = :QLXT-EARN-16,
              LXFILE.LXT_EARN_17 = :QLXT-EARN-17,
              LXFILE.LXT_EARN_18 = :QLXT-EARN-18,
              LXFILE.LXT_EARN_19 = :QLXT-EARN-19,
              LXFILE.LXT_EARN_20 = :QLXT-EARN-20,
              LXFILE.LXP_CODE = :QLXP-CODE,
              LXFILE.LXP_EARN_1 = :QLXP-EARN-1,
              LXFILE.LXP_EARN_2 = :QLXP-EARN-2,
              LXFILE.LXP_EARN_3 = :QLXP-EARN-3,
              LXFILE.LXP_EARN_4 = :QLXP-EARN-4,
              LXFILE.LXP_EARN_5 = :QLXP-EARN-5,
              LXFILE.LXP_EARN_6 = :QLXP-EARN-6,
              LXFILE.LXP_EARN_7 = :QLXP-EARN-7,
              LXFILE.LXP_EARN_8 = :QLXP-EARN-8,
              LXFILE.LXP_EARN_9 = :QLXP-EARN-9,
              LXFILE.LXP_EARN_10 = :QLXP-EARN-10,
              LXFILE.LXP_EARN_11 = :QLXP-EARN-11,
              LXFILE.LXP_EARN_12 = :QLXP-EARN-12,
              LXFILE.LXP_EARN_13 = :QLXP-EARN-13,
              LXFILE.LXP_EARN_14 = :QLXP-EARN-14,
              LXFILE.LXP_EARN_15 = :QLXP-EARN-15,
              LXFILE.LXP_EARN_16 = :QLXP-EARN-16,
              LXFILE.LXP_EARN_17 = :QLXP-EARN-17,
              LXFILE.LXP_EARN_18 = :QLXP-EARN-18,
              LXFILE.LXP_EARN_19 = :QLXP-EARN-19,
              LXFILE.LXP_EARN_20 = :QLXP-EARN-20,
              LXFILE.LXA_CODE = :QLXA-CODE,
              LXFILE.LXA_SRCD = :QLXA-SRCD,
              LXFILE.LXA_START_PMT_1 = :QLXA-START-PMT-1,
              LXFILE.LXA_END_PMT_1 = :QLXA-END-PMT-1,
              LXFILE.LXA_NET_CHANGE_1 = :QLXA-NET-CHANGE-1,
              LXFILE.LXA_START_PMT_2 = :QLXA-START-PMT-2,
              LXFILE.LXA_END_PMT_2 = :QLXA-END-PMT-2,
              LXFILE.LXA_NET_CHANGE_2 = :QLXA-NET-CHANGE-2,
              LXFILE.LXA_START_PMT_3 = :QLXA-START-PMT-3,
              LXFILE.LXA_END_PMT_3 = :QLXA-END-PMT-3,
              LXFILE.LXA_NET_CHANGE_3 = :QLXA-NET-CHANGE-3,
              LXFILE.LXA_START_PMT_4 = :QLXA-START-PMT-4,
              LXFILE.LXA_END_PMT_4 = :QLXA-END-PMT-4,
              LXFILE.LXA_NET_CHANGE_4 = :QLXA-NET-CHANGE-4,
              LXFILE.LXA_START_PMT_5 = :QLXA-START-PMT-5,
              LXFILE.LXA_END_PMT_5 = :QLXA-END-PMT-5,
              LXFILE.LXA_NET_CHANGE_5 = :QLXA-NET-CHANGE-5,
              LXFILE.LXA_START_PMT_6 = :QLXA-START-PMT-6,
              LXFILE.LXA_END_PMT_6 = :QLXA-END-PMT-6,
              LXFILE.LXA_NET_CHANGE_6 = :QLXA-NET-CHANGE-6,
              LXFILE.LXA_START_PMT_7 = :QLXA-START-PMT-7,
              LXFILE.LXA_END_PMT_7 = :QLXA-END-PMT-7,
              LXFILE.LXA_NET_CHANGE_7 = :QLXA-NET-CHANGE-7,
              LXFILE.LXA_START_PMT_8 = :QLXA-START-PMT-8,
              LXFILE.LXA_END_PMT_8 = :QLXA-END-PMT-8,
              LXFILE.LXA_NET_CHANGE_8 = :QLXA-NET-CHANGE-8,
              LXFILE.LXA_START_PMT_9 = :QLXA-START-PMT-9,
              LXFILE.LXA_END_PMT_9 = :QLXA-END-PMT-9,
              LXFILE.LXA_NET_CHANGE_9 = :QLXA-NET-CHANGE-9,
              LXFILE.LXA_LN_INSCOMP = :QLXA-LN-INSCOMP,
              LXFILE.LXA_LN_INSOURS = :QLXA-LN-INSOURS,
              LXFILE.LXA_LN_INSEFF_DATE =
                 :QLXA-LN-INSEFF-DATE,
              LXFILE.LXA_LN_INSEXP_DATE =
                 :QLXA-LN-INSEXP-DATE,
              LXFILE.LXA_LN_INSPREM = :QLXA-LN-INSPREM,
              LXFILE.LXA_LN_INSCOVR = :QLXA-LN-INSCOVR,
              LXFILE.LXA_LN_INSCOMM = :QLXA-LN-INSCOMM,
              LXFILE.LXA_LN_INS_REBATE = :QLXA-LN-INS-REBATE,
              LXFILE.LXA_LN_INSCODES = :QLXA-LN-INSCODES,
              LXFILE.LXZ_CODE = :QLXZ-CODE,
              LXFILE.LXZ_POST_REGPY = :QLXZ-POST-REGPY,
              LXFILE.LXZ_POST_EXTRA_PRIN = :QLXZ-POST-EXTRA-PRIN,
              LXFILE.LXZ_POST_OTH = :QLXZ-POST-OTH,
              LXFILE.LXZ_POST_LCHG = :QLXZ-POST-LCHG,
              LXFILE.LXZ_PDUE_APINT = :QLXZ-PDUE-APINT,
              LXFILE.LXZ_PDUE_APPRIN = :QLXZ-PDUE-APPRIN,
              LXFILE.LXZ_CUR_APINT = :QLXZ-CUR-APINT,
              LXFILE.LXZ_CUR_APPRIN = :QLXZ-CUR-APPRIN,
              LXFILE.LXR_CODE = :QLXR-CODE,
              LXFILE.LXR_REB_AMT = :QLXR-REB-AMT,
              LXFILE.LXX_CODE = :QLXX-CODE,
              LXFILE.LXX_INS_TAX_COUNTY_CD =
                   :QLXX-INS-TAX-COUNTY-CD,
              LXFILE.LXX_INS_TAX_CITY_CD = :QLXX-INS-TAX-CITY-CD,
              LXFILE.LXM_CODE = :QLXM-CODE,
              LXFILE.LXM_MIP_CLPAID = :QLXM-MIP-CLPAID,
              LXFILE.LXM_MIP_CLPDTH_DATE =
                 :QLXM-MIP-CLPDTH-DATE,
              LXFILE.LXM_MIP_AHPAID = :QLXM-MIP-AHPAID,
              LXFILE.LXM_MIP_AHPDTH_DATE = :QLXM-MIP-AHPDTH-DATE
             WHERE LXFILE.LX_BRNO = :QLX-BRNO
               AND LXFILE.LX_ACCTNO = :QLX-ACCTNO
               AND LXFILE.LX_SEQNO = :QLX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LX1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LX1-FILE.

           MOVE LX-PATH-OWNBR  TO LX-BRNO.
           PERFORM DELETE-IT.
           MOVE LX-PATH-SQL    TO E-FILE.
           MOVE LX1-KEY        TO E-KEYX.
           PERFORM SET-LX-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LXFILE
             WHERE LXFILE.LX_BRNO = :QLX-BRNO
               AND LXFILE.LX_ACCTNO = :QLX-ACCTNO
               AND LXFILE.LX_SEQNO = :QLX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LX1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LX1-FILE.

           PERFORM CLOSE-IT.

           IF ( LX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LX1-CURSOR-STAT.

