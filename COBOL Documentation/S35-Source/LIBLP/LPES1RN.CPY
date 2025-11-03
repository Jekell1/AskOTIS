      * COPYMEMBER: LIBLP/LPES1RN
       LOAD-ES1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ES-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ES-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ES-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ES-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO ES-ALL-DATA
              MOVE ES-ALL-PATH             TO ES-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ES-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ES-ALL-BASE
              MOVE FILPATH-MACHINE         TO ES-ALL-MACHINE
              MOVE FILPATH-DATA            TO ES-ALL-DATA
              MOVE ES-ALL-PATH             TO ES-PATH.

      *-----------------------------------------------------------------
       OPEN-ES1-FILE.
           PERFORM LOAD-ES1-FILE.
           PERFORM OPEN-IT.
           MOVE ES-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-ES1-FILE.
           PERFORM START-IT.
           MOVE ES-PATH-OWNBR  TO ES-BRNO.
           MOVE ES-PATH-SQL    TO E-FILE.
           MOVE ES1-KEY        TO E-KEYX.

           IF ( ES1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-ES1-FILE.

           IF ( QES1-WEND-ACCTNO = ZEROES    ) OR
              ( QES1-WEND-ACCTNO NOT NUMERIC )
              MOVE ES-PATH-OWNBR        TO QES1-WBEG-BRNO
              MOVE ES-PATH-OWNBR        TO QES1-WEND-BRNO
              MOVE ES-ACCTNO            TO QES1-WBEG-ACCTNO
              MOVE ALL "9"              TO QES1-WEND-ACCTNO.

           EXEC SQL
            DECLARE ES1_CURSOR CURSOR FOR
            SELECT ESFILE.ES_BRNO,
                   ESFILE.ES_ACCTNO,
                   CAST(ESFILE.ES_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_EFF_DATE_1
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_1
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_1,
                   ESFILE.ES_SCHD_TERM_1,
                   ESFILE.ES_SCHD_COLLECTED_1,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_2
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_2
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_2,
                   ESFILE.ES_SCHD_TERM_2,
                   ESFILE.ES_SCHD_COLLECTED_2,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_3
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_3
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_3,
                   ESFILE.ES_SCHD_TERM_3,
                   ESFILE.ES_SCHD_COLLECTED_3,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_4
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_4
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_4,
                   ESFILE.ES_SCHD_TERM_4,
                   ESFILE.ES_SCHD_COLLECTED_4,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_5
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_5
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_5,
                   ESFILE.ES_SCHD_TERM_5,
                   ESFILE.ES_SCHD_COLLECTED_5,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_6
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_6
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_6,
                   ESFILE.ES_SCHD_TERM_6,
                   ESFILE.ES_SCHD_COLLECTED_6,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_7
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_7
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_7,
                   ESFILE.ES_SCHD_TERM_7,
                   ESFILE.ES_SCHD_COLLECTED_7,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_8
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_8
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_8,
                   ESFILE.ES_SCHD_TERM_8,
                   ESFILE.ES_SCHD_COLLECTED_8,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_9
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_9
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_9,
                   ESFILE.ES_SCHD_TERM_9,
                   ESFILE.ES_SCHD_COLLECTED_9,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_10
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_10
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_10,
                   ESFILE.ES_SCHD_TERM_10,
                   ESFILE.ES_SCHD_COLLECTED_10,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_11
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_11
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_11,
                   ESFILE.ES_SCHD_TERM_11,
                   ESFILE.ES_SCHD_COLLECTED_11,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_12
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_12
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_12,
                   ESFILE.ES_SCHD_TERM_12,
                   ESFILE.ES_SCHD_COLLECTED_12,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_13
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_13
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_13,
                   ESFILE.ES_SCHD_TERM_13,
                   ESFILE.ES_SCHD_COLLECTED_13,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_14
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_14
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_14,
                   ESFILE.ES_SCHD_TERM_14,
                   ESFILE.ES_SCHD_COLLECTED_14,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_15
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_15
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_15,
                   ESFILE.ES_SCHD_TERM_15,
                   ESFILE.ES_SCHD_COLLECTED_15,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_16
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_16
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_16,
                   ESFILE.ES_SCHD_TERM_16,
                   ESFILE.ES_SCHD_COLLECTED_16,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_17
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_17
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_17,
                   ESFILE.ES_SCHD_TERM_17,
                   ESFILE.ES_SCHD_COLLECTED_17,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_18
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_18
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_18,
                   ESFILE.ES_SCHD_TERM_18,
                   ESFILE.ES_SCHD_COLLECTED_18,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_19
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_19
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_19,
                   ESFILE.ES_SCHD_TERM_19,
                   ESFILE.ES_SCHD_COLLECTED_19,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_20
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_20
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_20,
                   ESFILE.ES_SCHD_TERM_20,
                   ESFILE.ES_SCHD_COLLECTED_20,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_21
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_21
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_21,
                   ESFILE.ES_SCHD_TERM_21,
                   ESFILE.ES_SCHD_COLLECTED_21,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_22
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_22
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_22,
                   ESFILE.ES_SCHD_TERM_22,
                   ESFILE.ES_SCHD_COLLECTED_22,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_23
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_23
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_23,
                   ESFILE.ES_SCHD_TERM_23,
                   ESFILE.ES_SCHD_COLLECTED_23,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_24
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_24
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_24,
                   ESFILE.ES_SCHD_TERM_24,
                   ESFILE.ES_SCHD_COLLECTED_24,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_25
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_25
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_25,
                   ESFILE.ES_SCHD_TERM_25,
                   ESFILE.ES_SCHD_COLLECTED_25,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_26
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_26
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_26,
                   ESFILE.ES_SCHD_TERM_26,
                   ESFILE.ES_SCHD_COLLECTED_26,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_27
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_27
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_27,
                   ESFILE.ES_SCHD_TERM_27,
                   ESFILE.ES_SCHD_COLLECTED_27,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_28
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_28
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_28,
                   ESFILE.ES_SCHD_TERM_28,
                   ESFILE.ES_SCHD_COLLECTED_28,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_29
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_29
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_29,
                   ESFILE.ES_SCHD_TERM_29,
                   ESFILE.ES_SCHD_COLLECTED_29,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_30
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_30
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_30,
                   ESFILE.ES_SCHD_TERM_30,
                   ESFILE.ES_SCHD_COLLECTED_30,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_31
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_31
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_31,
                   ESFILE.ES_SCHD_TERM_31,
                   ESFILE.ES_SCHD_COLLECTED_31,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_32
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_32
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_32,
                   ESFILE.ES_SCHD_TERM_32,
                   ESFILE.ES_SCHD_COLLECTED_32,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_33
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_33
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_33,
                   ESFILE.ES_SCHD_TERM_33,
                   ESFILE.ES_SCHD_COLLECTED_33,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_34
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_34
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_34,
                   ESFILE.ES_SCHD_TERM_34,
                   ESFILE.ES_SCHD_COLLECTED_34,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_35
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_35
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_35,
                   ESFILE.ES_SCHD_TERM_35,
                   ESFILE.ES_SCHD_COLLECTED_35,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_36
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_36
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_36,
                   ESFILE.ES_SCHD_TERM_36,
                   ESFILE.ES_SCHD_COLLECTED_36,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_37
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_37
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_37,
                   ESFILE.ES_SCHD_TERM_37,
                   ESFILE.ES_SCHD_COLLECTED_37,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_38
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_38
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_38,
                   ESFILE.ES_SCHD_TERM_38,
                   ESFILE.ES_SCHD_COLLECTED_38,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_39
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_39
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_39,
                   ESFILE.ES_SCHD_TERM_39,
                   ESFILE.ES_SCHD_COLLECTED_39,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_40
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_40
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_40,
                   ESFILE.ES_SCHD_TERM_40,
                   ESFILE.ES_SCHD_COLLECTED_40,
                   ESFILE.ES_BALANCE,
                   ESFILE.ES_PREPAID_1,
                   ESFILE.ES_COLLECTED_1,
                   ESFILE.ES_DISBURSED_TAXES_1,
                   ESFILE.ES_DISBURSED_INS_1,
                   ESFILE.ES_DISBURSED_PMI_1,
                   ESFILE.ES_DISBURSED_OTHER_1,
                   ESFILE.ES_DISBURSED_REF_1,
                   ESFILE.ES_DISBURSED_CNT_TAXES_1,
                   ESFILE.ES_DISBURSED_MISC1_1,
                   ESFILE.ES_DISBURSED_MISC2_1,
                   ESFILE.ES_PREPAID_2,
                   ESFILE.ES_COLLECTED_2,
                   ESFILE.ES_DISBURSED_TAXES_2,
                   ESFILE.ES_DISBURSED_INS_2,
                   ESFILE.ES_DISBURSED_PMI_2,
                   ESFILE.ES_DISBURSED_OTHER_2,
                   ESFILE.ES_DISBURSED_REF_2,
                   ESFILE.ES_DISBURSED_CNT_TAXES_2,
                   ESFILE.ES_DISBURSED_MISC1_2,
                   ESFILE.ES_DISBURSED_MISC2_2,
                   ESFILE.ES_PREPAID_3,
                   ESFILE.ES_COLLECTED_3,
                   ESFILE.ES_DISBURSED_TAXES_3,
                   ESFILE.ES_DISBURSED_INS_3,
                   ESFILE.ES_DISBURSED_PMI_3,
                   ESFILE.ES_DISBURSED_OTHER_3,
                   ESFILE.ES_DISBURSED_REF_3,
                   ESFILE.ES_DISBURSED_CNT_TAXES_3,
                   ESFILE.ES_DISBURSED_MISC1_3,
                   ESFILE.ES_DISBURSED_MISC2_3,
                   ESFILE.ES_MISC_AMT_1,
                   CAST(ESFILE.ES_EXP_DATE_1
                                                  AS VARCHAR(10)),
                   ESFILE.ES_MISC_AMT_2,
                   CAST(ESFILE.ES_EXP_DATE_2
                                                  AS VARCHAR(10)),
                   ESFILE.ES_MISC_AMT_3,
                   CAST(ESFILE.ES_EXP_DATE_3
                                                  AS VARCHAR(10)),
                   ESFILE.ES_MISC_AMT_4,
                   CAST(ESFILE.ES_EXP_DATE_4
                                                  AS VARCHAR(10)),
                   ESFILE.ES_MISC_AMT_5,
                   CAST(ESFILE.ES_EXP_DATE_5
                                                  AS VARCHAR(10)),
                   ESFILE.ES_MISC_AMT_6,
                   CAST(ESFILE.ES_EXP_DATE_6
                                                  AS VARCHAR(10))
            FROM DBO.ESFILE
            WHERE ESFILE.ES_BRNO = :QES1-WBEG-BRNO
              AND ESFILE.ES_ACCTNO >= :QES1-WBEG-ACCTNO
              AND ESFILE.ES_ACCTNO <= :QES1-WEND-ACCTNO
            ORDER BY ESFILE.ES_BRNO,
                     ESFILE.ES_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN ES1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QES1-WBEG-BRNO 
                          QES1-WEND-BRNO
                          QES1-WBEG-ACCTNO
                          QES1-WEND-ACCTNO.

           MOVE STAT---GOOD TO ES1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-ES1-FILE.
           PERFORM READ-IT.
           MOVE ES-PATH-OWNBR  TO ES-BRNO. 
           MOVE ES-PATH-SQL    TO E-FILE.
           MOVE ES1-KEY        TO E-KEYX.
           INITIALIZE QES-REC.

           MOVE ES-BRNO   TO QES-BRNO.
           MOVE ES-ACCTNO TO QES-ACCTNO.

           EXEC SQL
            SELECT ESFILE.ES_BRNO,
                   ESFILE.ES_ACCTNO,
                   CAST(ESFILE.ES_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_EFF_DATE_1
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_1
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_1,
                   ESFILE.ES_SCHD_TERM_1,
                   ESFILE.ES_SCHD_COLLECTED_1,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_2
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_2
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_2,
                   ESFILE.ES_SCHD_TERM_2,
                   ESFILE.ES_SCHD_COLLECTED_2,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_3
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_3
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_3,
                   ESFILE.ES_SCHD_TERM_3,
                   ESFILE.ES_SCHD_COLLECTED_3,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_4
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_4
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_4,
                   ESFILE.ES_SCHD_TERM_4,
                   ESFILE.ES_SCHD_COLLECTED_4,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_5
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_5
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_5,
                   ESFILE.ES_SCHD_TERM_5,
                   ESFILE.ES_SCHD_COLLECTED_5,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_6
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_6
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_6,
                   ESFILE.ES_SCHD_TERM_6,
                   ESFILE.ES_SCHD_COLLECTED_6,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_7
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_7
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_7,
                   ESFILE.ES_SCHD_TERM_7,
                   ESFILE.ES_SCHD_COLLECTED_7,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_8
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_8
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_8,
                   ESFILE.ES_SCHD_TERM_8,
                   ESFILE.ES_SCHD_COLLECTED_8,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_9
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_9
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_9,
                   ESFILE.ES_SCHD_TERM_9,
                   ESFILE.ES_SCHD_COLLECTED_9,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_10
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_10
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_10,
                   ESFILE.ES_SCHD_TERM_10,
                   ESFILE.ES_SCHD_COLLECTED_10,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_11
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_11
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_11,
                   ESFILE.ES_SCHD_TERM_11,
                   ESFILE.ES_SCHD_COLLECTED_11,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_12
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_12
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_12,
                   ESFILE.ES_SCHD_TERM_12,
                   ESFILE.ES_SCHD_COLLECTED_12,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_13
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_13
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_13,
                   ESFILE.ES_SCHD_TERM_13,
                   ESFILE.ES_SCHD_COLLECTED_13,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_14
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_14
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_14,
                   ESFILE.ES_SCHD_TERM_14,
                   ESFILE.ES_SCHD_COLLECTED_14,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_15
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_15
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_15,
                   ESFILE.ES_SCHD_TERM_15,
                   ESFILE.ES_SCHD_COLLECTED_15,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_16
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_16
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_16,
                   ESFILE.ES_SCHD_TERM_16,
                   ESFILE.ES_SCHD_COLLECTED_16,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_17
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_17
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_17,
                   ESFILE.ES_SCHD_TERM_17,
                   ESFILE.ES_SCHD_COLLECTED_17,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_18
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_18
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_18,
                   ESFILE.ES_SCHD_TERM_18,
                   ESFILE.ES_SCHD_COLLECTED_18,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_19
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_19
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_19,
                   ESFILE.ES_SCHD_TERM_19,
                   ESFILE.ES_SCHD_COLLECTED_19,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_20
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_20
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_20,
                   ESFILE.ES_SCHD_TERM_20,
                   ESFILE.ES_SCHD_COLLECTED_20,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_21
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_21
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_21,
                   ESFILE.ES_SCHD_TERM_21,
                   ESFILE.ES_SCHD_COLLECTED_21,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_22
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_22
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_22,
                   ESFILE.ES_SCHD_TERM_22,
                   ESFILE.ES_SCHD_COLLECTED_22,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_23
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_23
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_23,
                   ESFILE.ES_SCHD_TERM_23,
                   ESFILE.ES_SCHD_COLLECTED_23,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_24
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_24
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_24,
                   ESFILE.ES_SCHD_TERM_24,
                   ESFILE.ES_SCHD_COLLECTED_24,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_25
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_25
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_25,
                   ESFILE.ES_SCHD_TERM_25,
                   ESFILE.ES_SCHD_COLLECTED_25,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_26
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_26
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_26,
                   ESFILE.ES_SCHD_TERM_26,
                   ESFILE.ES_SCHD_COLLECTED_26,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_27
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_27
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_27,
                   ESFILE.ES_SCHD_TERM_27,
                   ESFILE.ES_SCHD_COLLECTED_27,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_28
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_28
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_28,
                   ESFILE.ES_SCHD_TERM_28,
                   ESFILE.ES_SCHD_COLLECTED_28,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_29
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_29
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_29,
                   ESFILE.ES_SCHD_TERM_29,
                   ESFILE.ES_SCHD_COLLECTED_29,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_30
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_30
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_30,
                   ESFILE.ES_SCHD_TERM_30,
                   ESFILE.ES_SCHD_COLLECTED_30,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_31
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_31
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_31,
                   ESFILE.ES_SCHD_TERM_31,
                   ESFILE.ES_SCHD_COLLECTED_31,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_32
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_32
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_32,
                   ESFILE.ES_SCHD_TERM_32,
                   ESFILE.ES_SCHD_COLLECTED_32,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_33
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_33
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_33,
                   ESFILE.ES_SCHD_TERM_33,
                   ESFILE.ES_SCHD_COLLECTED_33,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_34
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_34
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_34,
                   ESFILE.ES_SCHD_TERM_34,
                   ESFILE.ES_SCHD_COLLECTED_34,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_35
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_35
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_35,
                   ESFILE.ES_SCHD_TERM_35,
                   ESFILE.ES_SCHD_COLLECTED_35,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_36
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_36
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_36,
                   ESFILE.ES_SCHD_TERM_36,
                   ESFILE.ES_SCHD_COLLECTED_36,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_37
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_37
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_37,
                   ESFILE.ES_SCHD_TERM_37,
                   ESFILE.ES_SCHD_COLLECTED_37,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_38
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_38
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_38,
                   ESFILE.ES_SCHD_TERM_38,
                   ESFILE.ES_SCHD_COLLECTED_38,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_39
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_39
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_39,
                   ESFILE.ES_SCHD_TERM_39,
                   ESFILE.ES_SCHD_COLLECTED_39,
                   CAST(ESFILE.ES_SCHD_EFF_DATE_40
                                                  AS VARCHAR(10)),
                   CAST(ESFILE.ES_SCHD_1STPYDATE_40
                                                  AS VARCHAR(10)),
                   ESFILE.ES_SCHD_ESCROW_40,
                   ESFILE.ES_SCHD_TERM_40,
                   ESFILE.ES_SCHD_COLLECTED_40,
                   ESFILE.ES_BALANCE,
                   ESFILE.ES_PREPAID_1,
                   ESFILE.ES_COLLECTED_1,
                   ESFILE.ES_DISBURSED_TAXES_1,
                   ESFILE.ES_DISBURSED_INS_1,
                   ESFILE.ES_DISBURSED_PMI_1,
                   ESFILE.ES_DISBURSED_OTHER_1,
                   ESFILE.ES_DISBURSED_REF_1,
                   ESFILE.ES_DISBURSED_CNT_TAXES_1,
                   ESFILE.ES_DISBURSED_MISC1_1,
                   ESFILE.ES_DISBURSED_MISC2_1,
                   ESFILE.ES_PREPAID_2,
                   ESFILE.ES_COLLECTED_2,
                   ESFILE.ES_DISBURSED_TAXES_2,
                   ESFILE.ES_DISBURSED_INS_2,
                   ESFILE.ES_DISBURSED_PMI_2,
                   ESFILE.ES_DISBURSED_OTHER_2,
                   ESFILE.ES_DISBURSED_REF_2,
                   ESFILE.ES_DISBURSED_CNT_TAXES_2,
                   ESFILE.ES_DISBURSED_MISC1_2,
                   ESFILE.ES_DISBURSED_MISC2_2,
                   ESFILE.ES_PREPAID_3,
                   ESFILE.ES_COLLECTED_3,
                   ESFILE.ES_DISBURSED_TAXES_3,
                   ESFILE.ES_DISBURSED_INS_3,
                   ESFILE.ES_DISBURSED_PMI_3,
                   ESFILE.ES_DISBURSED_OTHER_3,
                   ESFILE.ES_DISBURSED_REF_3,
                   ESFILE.ES_DISBURSED_CNT_TAXES_3,
                   ESFILE.ES_DISBURSED_MISC1_3,
                   ESFILE.ES_DISBURSED_MISC2_3,
                   ESFILE.ES_MISC_AMT_1,
                   CAST(ESFILE.ES_EXP_DATE_1
                                                  AS VARCHAR(10)),
                   ESFILE.ES_MISC_AMT_2,
                   CAST(ESFILE.ES_EXP_DATE_2
                                                  AS VARCHAR(10)),
                   ESFILE.ES_MISC_AMT_3,
                   CAST(ESFILE.ES_EXP_DATE_3
                                                  AS VARCHAR(10)),
                   ESFILE.ES_MISC_AMT_4,
                   CAST(ESFILE.ES_EXP_DATE_4
                                                  AS VARCHAR(10)),
                   ESFILE.ES_MISC_AMT_5,
                   CAST(ESFILE.ES_EXP_DATE_5
                                                  AS VARCHAR(10)),
                   ESFILE.ES_MISC_AMT_6,
                   CAST(ESFILE.ES_EXP_DATE_6
                                                  AS VARCHAR(10))
            INTO :QES-REC
            FROM DBO.ESFILE
            WHERE ESFILE.ES_BRNO = :QES-BRNO
              AND ESFILE.ES_ACCTNO = :QES-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ES1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-ES-FIELDS.

      *-----------------------------------------------------------------
       READ-ES1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ES-PATH-OWNBR  TO ES-BRNO.
           MOVE ES-PATH-SQL    TO E-FILE.
           MOVE ES1-KEY        TO E-KEYX.
           INITIALIZE QES-REC.

           IF ( ES1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH ES1_CURSOR INTO :QES-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ES1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-ES-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-ES1-FILE.

           PERFORM CLOSE-IT.
           IF ( ES1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE ES1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO ES1-CURSOR-STAT.

