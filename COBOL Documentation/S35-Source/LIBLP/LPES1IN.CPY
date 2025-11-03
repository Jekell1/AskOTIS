      * COPYMEMBER: LIBLP/LPES1IN
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
       WRITE-ES1-FILE.

           MOVE ES-PATH-OWNBR  TO ES-BRNO.
           ACCEPT ES-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE ES-PATH-SQL    TO E-FILE.
           MOVE ES1-KEY        TO E-KEYX.
           PERFORM SET-ES-FIELDS.

           EXEC SQL
            INSERT INTO DBO.ESFILE
             (ESFILE.ES_BRNO,
              ESFILE.ES_ACCTNO,
              ESFILE.ES_LTOUCH_DATE,
              ESFILE.ES_SCHD_EFF_DATE_1,
              ESFILE.ES_SCHD_1STPYDATE_1,
              ESFILE.ES_SCHD_ESCROW_1,
              ESFILE.ES_SCHD_TERM_1,
              ESFILE.ES_SCHD_COLLECTED_1,
              ESFILE.ES_SCHD_EFF_DATE_2,
              ESFILE.ES_SCHD_1STPYDATE_2,
              ESFILE.ES_SCHD_ESCROW_2,
              ESFILE.ES_SCHD_TERM_2,
              ESFILE.ES_SCHD_COLLECTED_2,
              ESFILE.ES_SCHD_EFF_DATE_3,
              ESFILE.ES_SCHD_1STPYDATE_3,
              ESFILE.ES_SCHD_ESCROW_3,
              ESFILE.ES_SCHD_TERM_3,
              ESFILE.ES_SCHD_COLLECTED_3,
              ESFILE.ES_SCHD_EFF_DATE_4,
              ESFILE.ES_SCHD_1STPYDATE_4,
              ESFILE.ES_SCHD_ESCROW_4,
              ESFILE.ES_SCHD_TERM_4,
              ESFILE.ES_SCHD_COLLECTED_4,
              ESFILE.ES_SCHD_EFF_DATE_5,
              ESFILE.ES_SCHD_1STPYDATE_5,
              ESFILE.ES_SCHD_ESCROW_5,
              ESFILE.ES_SCHD_TERM_5,
              ESFILE.ES_SCHD_COLLECTED_5,
              ESFILE.ES_SCHD_EFF_DATE_6,
              ESFILE.ES_SCHD_1STPYDATE_6,
              ESFILE.ES_SCHD_ESCROW_6,
              ESFILE.ES_SCHD_TERM_6,
              ESFILE.ES_SCHD_COLLECTED_6,
              ESFILE.ES_SCHD_EFF_DATE_7,
              ESFILE.ES_SCHD_1STPYDATE_7,
              ESFILE.ES_SCHD_ESCROW_7,
              ESFILE.ES_SCHD_TERM_7,
              ESFILE.ES_SCHD_COLLECTED_7,
              ESFILE.ES_SCHD_EFF_DATE_8,
              ESFILE.ES_SCHD_1STPYDATE_8,
              ESFILE.ES_SCHD_ESCROW_8,
              ESFILE.ES_SCHD_TERM_8,
              ESFILE.ES_SCHD_COLLECTED_8,
              ESFILE.ES_SCHD_EFF_DATE_9,
              ESFILE.ES_SCHD_1STPYDATE_9,
              ESFILE.ES_SCHD_ESCROW_9,
              ESFILE.ES_SCHD_TERM_9,
              ESFILE.ES_SCHD_COLLECTED_9,
              ESFILE.ES_SCHD_EFF_DATE_10,
              ESFILE.ES_SCHD_1STPYDATE_10,
              ESFILE.ES_SCHD_ESCROW_10,
              ESFILE.ES_SCHD_TERM_10,
              ESFILE.ES_SCHD_COLLECTED_10,
              ESFILE.ES_SCHD_EFF_DATE_11,
              ESFILE.ES_SCHD_1STPYDATE_11,
              ESFILE.ES_SCHD_ESCROW_11,
              ESFILE.ES_SCHD_TERM_11,
              ESFILE.ES_SCHD_COLLECTED_11,
              ESFILE.ES_SCHD_EFF_DATE_12,
              ESFILE.ES_SCHD_1STPYDATE_12,
              ESFILE.ES_SCHD_ESCROW_12,
              ESFILE.ES_SCHD_TERM_12,
              ESFILE.ES_SCHD_COLLECTED_12,
              ESFILE.ES_SCHD_EFF_DATE_13,
              ESFILE.ES_SCHD_1STPYDATE_13,
              ESFILE.ES_SCHD_ESCROW_13,
              ESFILE.ES_SCHD_TERM_13,
              ESFILE.ES_SCHD_COLLECTED_13,
              ESFILE.ES_SCHD_EFF_DATE_14,
              ESFILE.ES_SCHD_1STPYDATE_14,
              ESFILE.ES_SCHD_ESCROW_14,
              ESFILE.ES_SCHD_TERM_14,
              ESFILE.ES_SCHD_COLLECTED_14,
              ESFILE.ES_SCHD_EFF_DATE_15,
              ESFILE.ES_SCHD_1STPYDATE_15,
              ESFILE.ES_SCHD_ESCROW_15,
              ESFILE.ES_SCHD_TERM_15,
              ESFILE.ES_SCHD_COLLECTED_15,
              ESFILE.ES_SCHD_EFF_DATE_16,
              ESFILE.ES_SCHD_1STPYDATE_16,
              ESFILE.ES_SCHD_ESCROW_16,
              ESFILE.ES_SCHD_TERM_16,
              ESFILE.ES_SCHD_COLLECTED_16,
              ESFILE.ES_SCHD_EFF_DATE_17,
              ESFILE.ES_SCHD_1STPYDATE_17,
              ESFILE.ES_SCHD_ESCROW_17,
              ESFILE.ES_SCHD_TERM_17,
              ESFILE.ES_SCHD_COLLECTED_17,
              ESFILE.ES_SCHD_EFF_DATE_18,
              ESFILE.ES_SCHD_1STPYDATE_18,
              ESFILE.ES_SCHD_ESCROW_18,
              ESFILE.ES_SCHD_TERM_18,
              ESFILE.ES_SCHD_COLLECTED_18,
              ESFILE.ES_SCHD_EFF_DATE_19,
              ESFILE.ES_SCHD_1STPYDATE_19,
              ESFILE.ES_SCHD_ESCROW_19,
              ESFILE.ES_SCHD_TERM_19,
              ESFILE.ES_SCHD_COLLECTED_19,
              ESFILE.ES_SCHD_EFF_DATE_20,
              ESFILE.ES_SCHD_1STPYDATE_20,
              ESFILE.ES_SCHD_ESCROW_20,
              ESFILE.ES_SCHD_TERM_20,
              ESFILE.ES_SCHD_COLLECTED_20,
              ESFILE.ES_SCHD_EFF_DATE_21,
              ESFILE.ES_SCHD_1STPYDATE_21,
              ESFILE.ES_SCHD_ESCROW_21,
              ESFILE.ES_SCHD_TERM_21,
              ESFILE.ES_SCHD_COLLECTED_21,
              ESFILE.ES_SCHD_EFF_DATE_22,
              ESFILE.ES_SCHD_1STPYDATE_22,
              ESFILE.ES_SCHD_ESCROW_22,
              ESFILE.ES_SCHD_TERM_22,
              ESFILE.ES_SCHD_COLLECTED_22,
              ESFILE.ES_SCHD_EFF_DATE_23,
              ESFILE.ES_SCHD_1STPYDATE_23,
              ESFILE.ES_SCHD_ESCROW_23,
              ESFILE.ES_SCHD_TERM_23,
              ESFILE.ES_SCHD_COLLECTED_23,
              ESFILE.ES_SCHD_EFF_DATE_24,
              ESFILE.ES_SCHD_1STPYDATE_24,
              ESFILE.ES_SCHD_ESCROW_24,
              ESFILE.ES_SCHD_TERM_24,
              ESFILE.ES_SCHD_COLLECTED_24,
              ESFILE.ES_SCHD_EFF_DATE_25,
              ESFILE.ES_SCHD_1STPYDATE_25,
              ESFILE.ES_SCHD_ESCROW_25,
              ESFILE.ES_SCHD_TERM_25,
              ESFILE.ES_SCHD_COLLECTED_25,
              ESFILE.ES_SCHD_EFF_DATE_26,
              ESFILE.ES_SCHD_1STPYDATE_26,
              ESFILE.ES_SCHD_ESCROW_26,
              ESFILE.ES_SCHD_TERM_26,
              ESFILE.ES_SCHD_COLLECTED_26,
              ESFILE.ES_SCHD_EFF_DATE_27,
              ESFILE.ES_SCHD_1STPYDATE_27,
              ESFILE.ES_SCHD_ESCROW_27,
              ESFILE.ES_SCHD_TERM_27,
              ESFILE.ES_SCHD_COLLECTED_27,
              ESFILE.ES_SCHD_EFF_DATE_28,
              ESFILE.ES_SCHD_1STPYDATE_28,
              ESFILE.ES_SCHD_ESCROW_28,
              ESFILE.ES_SCHD_TERM_28,
              ESFILE.ES_SCHD_COLLECTED_28,
              ESFILE.ES_SCHD_EFF_DATE_29,
              ESFILE.ES_SCHD_1STPYDATE_29,
              ESFILE.ES_SCHD_ESCROW_29,
              ESFILE.ES_SCHD_TERM_29,
              ESFILE.ES_SCHD_COLLECTED_29,
              ESFILE.ES_SCHD_EFF_DATE_30,
              ESFILE.ES_SCHD_1STPYDATE_30,
              ESFILE.ES_SCHD_ESCROW_30,
              ESFILE.ES_SCHD_TERM_30,
              ESFILE.ES_SCHD_COLLECTED_30,
              ESFILE.ES_SCHD_EFF_DATE_31,
              ESFILE.ES_SCHD_1STPYDATE_31,
              ESFILE.ES_SCHD_ESCROW_31,
              ESFILE.ES_SCHD_TERM_31,
              ESFILE.ES_SCHD_COLLECTED_31,
              ESFILE.ES_SCHD_EFF_DATE_32,
              ESFILE.ES_SCHD_1STPYDATE_32,
              ESFILE.ES_SCHD_ESCROW_32,
              ESFILE.ES_SCHD_TERM_32,
              ESFILE.ES_SCHD_COLLECTED_32,
              ESFILE.ES_SCHD_EFF_DATE_33,
              ESFILE.ES_SCHD_1STPYDATE_33,
              ESFILE.ES_SCHD_ESCROW_33,
              ESFILE.ES_SCHD_TERM_33,
              ESFILE.ES_SCHD_COLLECTED_33,
              ESFILE.ES_SCHD_EFF_DATE_34,
              ESFILE.ES_SCHD_1STPYDATE_34,
              ESFILE.ES_SCHD_ESCROW_34,
              ESFILE.ES_SCHD_TERM_34,
              ESFILE.ES_SCHD_COLLECTED_34,
              ESFILE.ES_SCHD_EFF_DATE_35,
              ESFILE.ES_SCHD_1STPYDATE_35,
              ESFILE.ES_SCHD_ESCROW_35,
              ESFILE.ES_SCHD_TERM_35,
              ESFILE.ES_SCHD_COLLECTED_35,
              ESFILE.ES_SCHD_EFF_DATE_36,
              ESFILE.ES_SCHD_1STPYDATE_36,
              ESFILE.ES_SCHD_ESCROW_36,
              ESFILE.ES_SCHD_TERM_36,
              ESFILE.ES_SCHD_COLLECTED_36,
              ESFILE.ES_SCHD_EFF_DATE_37,
              ESFILE.ES_SCHD_1STPYDATE_37,
              ESFILE.ES_SCHD_ESCROW_37,
              ESFILE.ES_SCHD_TERM_37,
              ESFILE.ES_SCHD_COLLECTED_37,
              ESFILE.ES_SCHD_EFF_DATE_38,
              ESFILE.ES_SCHD_1STPYDATE_38,
              ESFILE.ES_SCHD_ESCROW_38,
              ESFILE.ES_SCHD_TERM_38,
              ESFILE.ES_SCHD_COLLECTED_38,
              ESFILE.ES_SCHD_EFF_DATE_39,
              ESFILE.ES_SCHD_1STPYDATE_39,
              ESFILE.ES_SCHD_ESCROW_39,
              ESFILE.ES_SCHD_TERM_39,
              ESFILE.ES_SCHD_COLLECTED_39,
              ESFILE.ES_SCHD_EFF_DATE_40,
              ESFILE.ES_SCHD_1STPYDATE_40,
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
              ESFILE.ES_EXP_DATE_1,
              ESFILE.ES_MISC_AMT_2,
              ESFILE.ES_EXP_DATE_2,
              ESFILE.ES_MISC_AMT_3,
              ESFILE.ES_EXP_DATE_3,
              ESFILE.ES_MISC_AMT_4,
              ESFILE.ES_EXP_DATE_4,
              ESFILE.ES_MISC_AMT_5,
              ESFILE.ES_EXP_DATE_5,
              ESFILE.ES_MISC_AMT_6,
              ESFILE.ES_EXP_DATE_6)
            VALUES
             (:QES-BRNO,
              :QES-ACCTNO,
              :QES-LTOUCH-DATE,
              :QES-SCHD-EFF-DATE-1,
              :QES-SCHD-1STPYDATE-1,
              :QES-SCHD-ESCROW-1,
              :QES-SCHD-TERM-1,
              :QES-SCHD-COLLECTED-1,
              :QES-SCHD-EFF-DATE-2,
              :QES-SCHD-1STPYDATE-2,
              :QES-SCHD-ESCROW-2,
              :QES-SCHD-TERM-2,
              :QES-SCHD-COLLECTED-2,
              :QES-SCHD-EFF-DATE-3,
              :QES-SCHD-1STPYDATE-3,
              :QES-SCHD-ESCROW-3,
              :QES-SCHD-TERM-3,
              :QES-SCHD-COLLECTED-3,
              :QES-SCHD-EFF-DATE-4,
              :QES-SCHD-1STPYDATE-4,
              :QES-SCHD-ESCROW-4,
              :QES-SCHD-TERM-4,
              :QES-SCHD-COLLECTED-4,
              :QES-SCHD-EFF-DATE-5,
              :QES-SCHD-1STPYDATE-5,
              :QES-SCHD-ESCROW-5,
              :QES-SCHD-TERM-5,
              :QES-SCHD-COLLECTED-5,
              :QES-SCHD-EFF-DATE-6,
              :QES-SCHD-1STPYDATE-6,
              :QES-SCHD-ESCROW-6,
              :QES-SCHD-TERM-6,
              :QES-SCHD-COLLECTED-6,
              :QES-SCHD-EFF-DATE-7,
              :QES-SCHD-1STPYDATE-7,
              :QES-SCHD-ESCROW-7,
              :QES-SCHD-TERM-7,
              :QES-SCHD-COLLECTED-7,
              :QES-SCHD-EFF-DATE-8,
              :QES-SCHD-1STPYDATE-8,
              :QES-SCHD-ESCROW-8,
              :QES-SCHD-TERM-8,
              :QES-SCHD-COLLECTED-8,
              :QES-SCHD-EFF-DATE-9,
              :QES-SCHD-1STPYDATE-9,
              :QES-SCHD-ESCROW-9,
              :QES-SCHD-TERM-9,
              :QES-SCHD-COLLECTED-9,
              :QES-SCHD-EFF-DATE-10,
              :QES-SCHD-1STPYDATE-10,
              :QES-SCHD-ESCROW-10,
              :QES-SCHD-TERM-10,
              :QES-SCHD-COLLECTED-10,
              :QES-SCHD-EFF-DATE-11,
              :QES-SCHD-1STPYDATE-11,
              :QES-SCHD-ESCROW-11,
              :QES-SCHD-TERM-11,
              :QES-SCHD-COLLECTED-11,
              :QES-SCHD-EFF-DATE-12,
              :QES-SCHD-1STPYDATE-12,
              :QES-SCHD-ESCROW-12,
              :QES-SCHD-TERM-12,
              :QES-SCHD-COLLECTED-12,
              :QES-SCHD-EFF-DATE-13,
              :QES-SCHD-1STPYDATE-13,
              :QES-SCHD-ESCROW-13,
              :QES-SCHD-TERM-13,
              :QES-SCHD-COLLECTED-13,
              :QES-SCHD-EFF-DATE-14,
              :QES-SCHD-1STPYDATE-14,
              :QES-SCHD-ESCROW-14,
              :QES-SCHD-TERM-14,
              :QES-SCHD-COLLECTED-14,
              :QES-SCHD-EFF-DATE-15,
              :QES-SCHD-1STPYDATE-15,
              :QES-SCHD-ESCROW-15,
              :QES-SCHD-TERM-15,
              :QES-SCHD-COLLECTED-15,
              :QES-SCHD-EFF-DATE-16,
              :QES-SCHD-1STPYDATE-16,
              :QES-SCHD-ESCROW-16,
              :QES-SCHD-TERM-16,
              :QES-SCHD-COLLECTED-16,
              :QES-SCHD-EFF-DATE-17,
              :QES-SCHD-1STPYDATE-17,
              :QES-SCHD-ESCROW-17,
              :QES-SCHD-TERM-17,
              :QES-SCHD-COLLECTED-17,
              :QES-SCHD-EFF-DATE-18,
              :QES-SCHD-1STPYDATE-18,
              :QES-SCHD-ESCROW-18,
              :QES-SCHD-TERM-18,
              :QES-SCHD-COLLECTED-18,
              :QES-SCHD-EFF-DATE-19,
              :QES-SCHD-1STPYDATE-19,
              :QES-SCHD-ESCROW-19,
              :QES-SCHD-TERM-19,
              :QES-SCHD-COLLECTED-19,
              :QES-SCHD-EFF-DATE-20,
              :QES-SCHD-1STPYDATE-20,
              :QES-SCHD-ESCROW-20,
              :QES-SCHD-TERM-20,
              :QES-SCHD-COLLECTED-20,
              :QES-SCHD-EFF-DATE-21,
              :QES-SCHD-1STPYDATE-21,
              :QES-SCHD-ESCROW-21,
              :QES-SCHD-TERM-21,
              :QES-SCHD-COLLECTED-21,
              :QES-SCHD-EFF-DATE-22,
              :QES-SCHD-1STPYDATE-22,
              :QES-SCHD-ESCROW-22,
              :QES-SCHD-TERM-22,
              :QES-SCHD-COLLECTED-22,
              :QES-SCHD-EFF-DATE-23,
              :QES-SCHD-1STPYDATE-23,
              :QES-SCHD-ESCROW-23,
              :QES-SCHD-TERM-23,
              :QES-SCHD-COLLECTED-23,
              :QES-SCHD-EFF-DATE-24,
              :QES-SCHD-1STPYDATE-24,
              :QES-SCHD-ESCROW-24,
              :QES-SCHD-TERM-24,
              :QES-SCHD-COLLECTED-24,
              :QES-SCHD-EFF-DATE-25,
              :QES-SCHD-1STPYDATE-25,
              :QES-SCHD-ESCROW-25,
              :QES-SCHD-TERM-25,
              :QES-SCHD-COLLECTED-25,
              :QES-SCHD-EFF-DATE-26,
              :QES-SCHD-1STPYDATE-26,
              :QES-SCHD-ESCROW-26,
              :QES-SCHD-TERM-26,
              :QES-SCHD-COLLECTED-26,
              :QES-SCHD-EFF-DATE-27,
              :QES-SCHD-1STPYDATE-27,
              :QES-SCHD-ESCROW-27,
              :QES-SCHD-TERM-27,
              :QES-SCHD-COLLECTED-27,
              :QES-SCHD-EFF-DATE-28,
              :QES-SCHD-1STPYDATE-28,
              :QES-SCHD-ESCROW-28,
              :QES-SCHD-TERM-28,
              :QES-SCHD-COLLECTED-28,
              :QES-SCHD-EFF-DATE-29,
              :QES-SCHD-1STPYDATE-29,
              :QES-SCHD-ESCROW-29,
              :QES-SCHD-TERM-29,
              :QES-SCHD-COLLECTED-29,
              :QES-SCHD-EFF-DATE-30,
              :QES-SCHD-1STPYDATE-30,
              :QES-SCHD-ESCROW-30,
              :QES-SCHD-TERM-30,
              :QES-SCHD-COLLECTED-30,
              :QES-SCHD-EFF-DATE-31,
              :QES-SCHD-1STPYDATE-31,
              :QES-SCHD-ESCROW-31,
              :QES-SCHD-TERM-31,
              :QES-SCHD-COLLECTED-31,
              :QES-SCHD-EFF-DATE-32,
              :QES-SCHD-1STPYDATE-32,
              :QES-SCHD-ESCROW-32,
              :QES-SCHD-TERM-32,
              :QES-SCHD-COLLECTED-32,
              :QES-SCHD-EFF-DATE-33,
              :QES-SCHD-1STPYDATE-33,
              :QES-SCHD-ESCROW-33,
              :QES-SCHD-TERM-33,
              :QES-SCHD-COLLECTED-33,
              :QES-SCHD-EFF-DATE-34,
              :QES-SCHD-1STPYDATE-34,
              :QES-SCHD-ESCROW-34,
              :QES-SCHD-TERM-34,
              :QES-SCHD-COLLECTED-34,
              :QES-SCHD-EFF-DATE-35,
              :QES-SCHD-1STPYDATE-35,
              :QES-SCHD-ESCROW-35,
              :QES-SCHD-TERM-35,
              :QES-SCHD-COLLECTED-35,
              :QES-SCHD-EFF-DATE-36,
              :QES-SCHD-1STPYDATE-36,
              :QES-SCHD-ESCROW-36,
              :QES-SCHD-TERM-36,
              :QES-SCHD-COLLECTED-36,
              :QES-SCHD-EFF-DATE-37,
              :QES-SCHD-1STPYDATE-37,
              :QES-SCHD-ESCROW-37,
              :QES-SCHD-TERM-37,
              :QES-SCHD-COLLECTED-37,
              :QES-SCHD-EFF-DATE-38,
              :QES-SCHD-1STPYDATE-38,
              :QES-SCHD-ESCROW-38,
              :QES-SCHD-TERM-38,
              :QES-SCHD-COLLECTED-38,
              :QES-SCHD-EFF-DATE-39,
              :QES-SCHD-1STPYDATE-39,
              :QES-SCHD-ESCROW-39,
              :QES-SCHD-TERM-39,
              :QES-SCHD-COLLECTED-39,
              :QES-SCHD-EFF-DATE-40,
              :QES-SCHD-1STPYDATE-40,
              :QES-SCHD-ESCROW-40,
              :QES-SCHD-TERM-40,
              :QES-SCHD-COLLECTED-40,
              :QES-BALANCE,
              :QES-PREPAID-1,
              :QES-COLLECTED-1,
              :QES-DISBURSED-TAXES-1,
              :QES-DISBURSED-INS-1,
              :QES-DISBURSED-PMI-1,
              :QES-DISBURSED-OTHER-1,
              :QES-DISBURSED-REF-1,
              :QES-DISBURSED-CNT-TAXES-1,
              :QES-DISBURSED-MISC1-1,
              :QES-DISBURSED-MISC2-1,
              :QES-PREPAID-2,
              :QES-COLLECTED-2,
              :QES-DISBURSED-TAXES-2,
              :QES-DISBURSED-INS-2,
              :QES-DISBURSED-PMI-2,
              :QES-DISBURSED-OTHER-2,
              :QES-DISBURSED-REF-2,
              :QES-DISBURSED-CNT-TAXES-2,
              :QES-DISBURSED-MISC1-2,
              :QES-DISBURSED-MISC2-2,
              :QES-PREPAID-3,
              :QES-COLLECTED-3,
              :QES-DISBURSED-TAXES-3,
              :QES-DISBURSED-INS-3,
              :QES-DISBURSED-PMI-3,
              :QES-DISBURSED-OTHER-3,
              :QES-DISBURSED-REF-3,
              :QES-DISBURSED-CNT-TAXES-3,
              :QES-DISBURSED-MISC1-3,
              :QES-DISBURSED-MISC2-3,
              :QES-MISC-AMT-1,
              :QES-EXP-DATE-1,
              :QES-MISC-AMT-2,
              :QES-EXP-DATE-2,
              :QES-MISC-AMT-3,
              :QES-EXP-DATE-3,
              :QES-MISC-AMT-4,
              :QES-EXP-DATE-4,
              :QES-MISC-AMT-5,
              :QES-EXP-DATE-5,
              :QES-MISC-AMT-6,
              :QES-EXP-DATE-6)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-ES1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-ES1-FILE.

           MOVE ES-PATH-OWNBR  TO ES-BRNO.
           ACCEPT ES-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE ES-PATH-SQL    TO E-FILE.
           MOVE ES1-KEY        TO E-KEYX.
           PERFORM SET-ES-FIELDS.

           EXEC SQL
            UPDATE DBO.ESFILE
             SET
              ESFILE.ES_BRNO = :QES-BRNO,
              ESFILE.ES_ACCTNO = :QES-ACCTNO,
              ESFILE.ES_LTOUCH_DATE =
                 :QES-LTOUCH-DATE,
              ESFILE.ES_SCHD_EFF_DATE_1 =
                 :QES-SCHD-EFF-DATE-1,
              ESFILE.ES_SCHD_1STPYDATE_1 =
                 :QES-SCHD-1STPYDATE-1,
              ESFILE.ES_SCHD_ESCROW_1 = :QES-SCHD-ESCROW-1,
              ESFILE.ES_SCHD_TERM_1 = :QES-SCHD-TERM-1,
              ESFILE.ES_SCHD_COLLECTED_1 = :QES-SCHD-COLLECTED-1,
              ESFILE.ES_SCHD_EFF_DATE_2 =
                 :QES-SCHD-EFF-DATE-2,
              ESFILE.ES_SCHD_1STPYDATE_2 =
                 :QES-SCHD-1STPYDATE-2,
              ESFILE.ES_SCHD_ESCROW_2 = :QES-SCHD-ESCROW-2,
              ESFILE.ES_SCHD_TERM_2 = :QES-SCHD-TERM-2,
              ESFILE.ES_SCHD_COLLECTED_2 = :QES-SCHD-COLLECTED-2,
              ESFILE.ES_SCHD_EFF_DATE_3 =
                 :QES-SCHD-EFF-DATE-3,
              ESFILE.ES_SCHD_1STPYDATE_3 =
                 :QES-SCHD-1STPYDATE-3,
              ESFILE.ES_SCHD_ESCROW_3 = :QES-SCHD-ESCROW-3,
              ESFILE.ES_SCHD_TERM_3 = :QES-SCHD-TERM-3,
              ESFILE.ES_SCHD_COLLECTED_3 = :QES-SCHD-COLLECTED-3,
              ESFILE.ES_SCHD_EFF_DATE_4 =
                 :QES-SCHD-EFF-DATE-4,
              ESFILE.ES_SCHD_1STPYDATE_4 =
                 :QES-SCHD-1STPYDATE-4,
              ESFILE.ES_SCHD_ESCROW_4 = :QES-SCHD-ESCROW-4,
              ESFILE.ES_SCHD_TERM_4 = :QES-SCHD-TERM-4,
              ESFILE.ES_SCHD_COLLECTED_4 = :QES-SCHD-COLLECTED-4,
              ESFILE.ES_SCHD_EFF_DATE_5 =
                 :QES-SCHD-EFF-DATE-5,
              ESFILE.ES_SCHD_1STPYDATE_5 =
                 :QES-SCHD-1STPYDATE-5,
              ESFILE.ES_SCHD_ESCROW_5 = :QES-SCHD-ESCROW-5,
              ESFILE.ES_SCHD_TERM_5 = :QES-SCHD-TERM-5,
              ESFILE.ES_SCHD_COLLECTED_5 = :QES-SCHD-COLLECTED-5,
              ESFILE.ES_SCHD_EFF_DATE_6 =
                 :QES-SCHD-EFF-DATE-6,
              ESFILE.ES_SCHD_1STPYDATE_6 =
                 :QES-SCHD-1STPYDATE-6,
              ESFILE.ES_SCHD_ESCROW_6 = :QES-SCHD-ESCROW-6,
              ESFILE.ES_SCHD_TERM_6 = :QES-SCHD-TERM-6,
              ESFILE.ES_SCHD_COLLECTED_6 = :QES-SCHD-COLLECTED-6,
              ESFILE.ES_SCHD_EFF_DATE_7 =
                 :QES-SCHD-EFF-DATE-7,
              ESFILE.ES_SCHD_1STPYDATE_7 =
                 :QES-SCHD-1STPYDATE-7,
              ESFILE.ES_SCHD_ESCROW_7 = :QES-SCHD-ESCROW-7,
              ESFILE.ES_SCHD_TERM_7 = :QES-SCHD-TERM-7,
              ESFILE.ES_SCHD_COLLECTED_7 = :QES-SCHD-COLLECTED-7,
              ESFILE.ES_SCHD_EFF_DATE_8 =
                 :QES-SCHD-EFF-DATE-8,
              ESFILE.ES_SCHD_1STPYDATE_8 =
                 :QES-SCHD-1STPYDATE-8,
              ESFILE.ES_SCHD_ESCROW_8 = :QES-SCHD-ESCROW-8,
              ESFILE.ES_SCHD_TERM_8 = :QES-SCHD-TERM-8,
              ESFILE.ES_SCHD_COLLECTED_8 = :QES-SCHD-COLLECTED-8,
              ESFILE.ES_SCHD_EFF_DATE_9 =
                 :QES-SCHD-EFF-DATE-9,
              ESFILE.ES_SCHD_1STPYDATE_9 =
                 :QES-SCHD-1STPYDATE-9,
              ESFILE.ES_SCHD_ESCROW_9 = :QES-SCHD-ESCROW-9,
              ESFILE.ES_SCHD_TERM_9 = :QES-SCHD-TERM-9,
              ESFILE.ES_SCHD_COLLECTED_9 = :QES-SCHD-COLLECTED-9,
              ESFILE.ES_SCHD_EFF_DATE_10 =
                 :QES-SCHD-EFF-DATE-10,
              ESFILE.ES_SCHD_1STPYDATE_10 =
                 :QES-SCHD-1STPYDATE-10,
              ESFILE.ES_SCHD_ESCROW_10 = :QES-SCHD-ESCROW-10,
              ESFILE.ES_SCHD_TERM_10 = :QES-SCHD-TERM-10,
              ESFILE.ES_SCHD_COLLECTED_10 =
                   :QES-SCHD-COLLECTED-10,
              ESFILE.ES_SCHD_EFF_DATE_11 =
                 :QES-SCHD-EFF-DATE-11,
              ESFILE.ES_SCHD_1STPYDATE_11 =
                 :QES-SCHD-1STPYDATE-11,
              ESFILE.ES_SCHD_ESCROW_11 = :QES-SCHD-ESCROW-11,
              ESFILE.ES_SCHD_TERM_11 = :QES-SCHD-TERM-11,
              ESFILE.ES_SCHD_COLLECTED_11 =
                   :QES-SCHD-COLLECTED-11,
              ESFILE.ES_SCHD_EFF_DATE_12 =
                 :QES-SCHD-EFF-DATE-12,
              ESFILE.ES_SCHD_1STPYDATE_12 =
                 :QES-SCHD-1STPYDATE-12,
              ESFILE.ES_SCHD_ESCROW_12 = :QES-SCHD-ESCROW-12,
              ESFILE.ES_SCHD_TERM_12 = :QES-SCHD-TERM-12,
              ESFILE.ES_SCHD_COLLECTED_12 =
                   :QES-SCHD-COLLECTED-12,
              ESFILE.ES_SCHD_EFF_DATE_13 =
                 :QES-SCHD-EFF-DATE-13,
              ESFILE.ES_SCHD_1STPYDATE_13 =
                 :QES-SCHD-1STPYDATE-13,
              ESFILE.ES_SCHD_ESCROW_13 = :QES-SCHD-ESCROW-13,
              ESFILE.ES_SCHD_TERM_13 = :QES-SCHD-TERM-13,
              ESFILE.ES_SCHD_COLLECTED_13 =
                   :QES-SCHD-COLLECTED-13,
              ESFILE.ES_SCHD_EFF_DATE_14 =
                 :QES-SCHD-EFF-DATE-14,
              ESFILE.ES_SCHD_1STPYDATE_14 =
                 :QES-SCHD-1STPYDATE-14,
              ESFILE.ES_SCHD_ESCROW_14 = :QES-SCHD-ESCROW-14,
              ESFILE.ES_SCHD_TERM_14 = :QES-SCHD-TERM-14,
              ESFILE.ES_SCHD_COLLECTED_14 =
                   :QES-SCHD-COLLECTED-14,
              ESFILE.ES_SCHD_EFF_DATE_15 =
                 :QES-SCHD-EFF-DATE-15,
              ESFILE.ES_SCHD_1STPYDATE_15 =
                 :QES-SCHD-1STPYDATE-15,
              ESFILE.ES_SCHD_ESCROW_15 = :QES-SCHD-ESCROW-15,
              ESFILE.ES_SCHD_TERM_15 = :QES-SCHD-TERM-15,
              ESFILE.ES_SCHD_COLLECTED_15 =
                   :QES-SCHD-COLLECTED-15,
              ESFILE.ES_SCHD_EFF_DATE_16 =
                 :QES-SCHD-EFF-DATE-16,
              ESFILE.ES_SCHD_1STPYDATE_16 =
                 :QES-SCHD-1STPYDATE-16,
              ESFILE.ES_SCHD_ESCROW_16 = :QES-SCHD-ESCROW-16,
              ESFILE.ES_SCHD_TERM_16 = :QES-SCHD-TERM-16,
              ESFILE.ES_SCHD_COLLECTED_16 =
                   :QES-SCHD-COLLECTED-16,
              ESFILE.ES_SCHD_EFF_DATE_17 =
                 :QES-SCHD-EFF-DATE-17,
              ESFILE.ES_SCHD_1STPYDATE_17 =
                 :QES-SCHD-1STPYDATE-17,
              ESFILE.ES_SCHD_ESCROW_17 = :QES-SCHD-ESCROW-17,
              ESFILE.ES_SCHD_TERM_17 = :QES-SCHD-TERM-17,
              ESFILE.ES_SCHD_COLLECTED_17 =
                   :QES-SCHD-COLLECTED-17,
              ESFILE.ES_SCHD_EFF_DATE_18 =
                 :QES-SCHD-EFF-DATE-18,
              ESFILE.ES_SCHD_1STPYDATE_18 =
                 :QES-SCHD-1STPYDATE-18,
              ESFILE.ES_SCHD_ESCROW_18 = :QES-SCHD-ESCROW-18,
              ESFILE.ES_SCHD_TERM_18 = :QES-SCHD-TERM-18,
              ESFILE.ES_SCHD_COLLECTED_18 =
                   :QES-SCHD-COLLECTED-18,
              ESFILE.ES_SCHD_EFF_DATE_19 =
                 :QES-SCHD-EFF-DATE-19,
              ESFILE.ES_SCHD_1STPYDATE_19 =
                 :QES-SCHD-1STPYDATE-19,
              ESFILE.ES_SCHD_ESCROW_19 = :QES-SCHD-ESCROW-19,
              ESFILE.ES_SCHD_TERM_19 = :QES-SCHD-TERM-19,
              ESFILE.ES_SCHD_COLLECTED_19 =
                   :QES-SCHD-COLLECTED-19,
              ESFILE.ES_SCHD_EFF_DATE_20 =
                 :QES-SCHD-EFF-DATE-20,
              ESFILE.ES_SCHD_1STPYDATE_20 =
                 :QES-SCHD-1STPYDATE-20,
              ESFILE.ES_SCHD_ESCROW_20 = :QES-SCHD-ESCROW-20,
              ESFILE.ES_SCHD_TERM_20 = :QES-SCHD-TERM-20,
              ESFILE.ES_SCHD_COLLECTED_20 =
                   :QES-SCHD-COLLECTED-20,
              ESFILE.ES_SCHD_EFF_DATE_21 =
                 :QES-SCHD-EFF-DATE-21,
              ESFILE.ES_SCHD_1STPYDATE_21 =
                 :QES-SCHD-1STPYDATE-21,
              ESFILE.ES_SCHD_ESCROW_21 = :QES-SCHD-ESCROW-21,
              ESFILE.ES_SCHD_TERM_21 = :QES-SCHD-TERM-21,
              ESFILE.ES_SCHD_COLLECTED_21 =
                   :QES-SCHD-COLLECTED-21,
              ESFILE.ES_SCHD_EFF_DATE_22 =
                 :QES-SCHD-EFF-DATE-22,
              ESFILE.ES_SCHD_1STPYDATE_22 =
                 :QES-SCHD-1STPYDATE-22,
              ESFILE.ES_SCHD_ESCROW_22 = :QES-SCHD-ESCROW-22,
              ESFILE.ES_SCHD_TERM_22 = :QES-SCHD-TERM-22,
              ESFILE.ES_SCHD_COLLECTED_22 =
                   :QES-SCHD-COLLECTED-22,
              ESFILE.ES_SCHD_EFF_DATE_23 =
                 :QES-SCHD-EFF-DATE-23,
              ESFILE.ES_SCHD_1STPYDATE_23 =
                 :QES-SCHD-1STPYDATE-23,
              ESFILE.ES_SCHD_ESCROW_23 = :QES-SCHD-ESCROW-23,
              ESFILE.ES_SCHD_TERM_23 = :QES-SCHD-TERM-23,
              ESFILE.ES_SCHD_COLLECTED_23 =
                   :QES-SCHD-COLLECTED-23,
              ESFILE.ES_SCHD_EFF_DATE_24 =
                 :QES-SCHD-EFF-DATE-24,
              ESFILE.ES_SCHD_1STPYDATE_24 =
                 :QES-SCHD-1STPYDATE-24,
              ESFILE.ES_SCHD_ESCROW_24 = :QES-SCHD-ESCROW-24,
              ESFILE.ES_SCHD_TERM_24 = :QES-SCHD-TERM-24,
              ESFILE.ES_SCHD_COLLECTED_24 =
                   :QES-SCHD-COLLECTED-24,
              ESFILE.ES_SCHD_EFF_DATE_25 =
                 :QES-SCHD-EFF-DATE-25,
              ESFILE.ES_SCHD_1STPYDATE_25 =
                 :QES-SCHD-1STPYDATE-25,
              ESFILE.ES_SCHD_ESCROW_25 = :QES-SCHD-ESCROW-25,
              ESFILE.ES_SCHD_TERM_25 = :QES-SCHD-TERM-25,
              ESFILE.ES_SCHD_COLLECTED_25 =
                   :QES-SCHD-COLLECTED-25,
              ESFILE.ES_SCHD_EFF_DATE_26 =
                 :QES-SCHD-EFF-DATE-26,
              ESFILE.ES_SCHD_1STPYDATE_26 =
                 :QES-SCHD-1STPYDATE-26,
              ESFILE.ES_SCHD_ESCROW_26 = :QES-SCHD-ESCROW-26,
              ESFILE.ES_SCHD_TERM_26 = :QES-SCHD-TERM-26,
              ESFILE.ES_SCHD_COLLECTED_26 =
                   :QES-SCHD-COLLECTED-26,
              ESFILE.ES_SCHD_EFF_DATE_27 =
                 :QES-SCHD-EFF-DATE-27,
              ESFILE.ES_SCHD_1STPYDATE_27 =
                 :QES-SCHD-1STPYDATE-27,
              ESFILE.ES_SCHD_ESCROW_27 = :QES-SCHD-ESCROW-27,
              ESFILE.ES_SCHD_TERM_27 = :QES-SCHD-TERM-27,
              ESFILE.ES_SCHD_COLLECTED_27 =
                   :QES-SCHD-COLLECTED-27,
              ESFILE.ES_SCHD_EFF_DATE_28 =
                 :QES-SCHD-EFF-DATE-28,
              ESFILE.ES_SCHD_1STPYDATE_28 =
                 :QES-SCHD-1STPYDATE-28,
              ESFILE.ES_SCHD_ESCROW_28 = :QES-SCHD-ESCROW-28,
              ESFILE.ES_SCHD_TERM_28 = :QES-SCHD-TERM-28,
              ESFILE.ES_SCHD_COLLECTED_28 =
                   :QES-SCHD-COLLECTED-28,
              ESFILE.ES_SCHD_EFF_DATE_29 =
                 :QES-SCHD-EFF-DATE-29,
              ESFILE.ES_SCHD_1STPYDATE_29 =
                 :QES-SCHD-1STPYDATE-29,
              ESFILE.ES_SCHD_ESCROW_29 = :QES-SCHD-ESCROW-29,
              ESFILE.ES_SCHD_TERM_29 = :QES-SCHD-TERM-29,
              ESFILE.ES_SCHD_COLLECTED_29 =
                   :QES-SCHD-COLLECTED-29,
              ESFILE.ES_SCHD_EFF_DATE_30 =
                 :QES-SCHD-EFF-DATE-30,
              ESFILE.ES_SCHD_1STPYDATE_30 =
                 :QES-SCHD-1STPYDATE-30,
              ESFILE.ES_SCHD_ESCROW_30 = :QES-SCHD-ESCROW-30,
              ESFILE.ES_SCHD_TERM_30 = :QES-SCHD-TERM-30,
              ESFILE.ES_SCHD_COLLECTED_30 =
                   :QES-SCHD-COLLECTED-30,
              ESFILE.ES_SCHD_EFF_DATE_31 =
                 :QES-SCHD-EFF-DATE-31,
              ESFILE.ES_SCHD_1STPYDATE_31 =
                 :QES-SCHD-1STPYDATE-31,
              ESFILE.ES_SCHD_ESCROW_31 = :QES-SCHD-ESCROW-31,
              ESFILE.ES_SCHD_TERM_31 = :QES-SCHD-TERM-31,
              ESFILE.ES_SCHD_COLLECTED_31 =
                   :QES-SCHD-COLLECTED-31,
              ESFILE.ES_SCHD_EFF_DATE_32 =
                 :QES-SCHD-EFF-DATE-32,
              ESFILE.ES_SCHD_1STPYDATE_32 =
                 :QES-SCHD-1STPYDATE-32,
              ESFILE.ES_SCHD_ESCROW_32 = :QES-SCHD-ESCROW-32,
              ESFILE.ES_SCHD_TERM_32 = :QES-SCHD-TERM-32,
              ESFILE.ES_SCHD_COLLECTED_32 =
                   :QES-SCHD-COLLECTED-32,
              ESFILE.ES_SCHD_EFF_DATE_33 =
                 :QES-SCHD-EFF-DATE-33,
              ESFILE.ES_SCHD_1STPYDATE_33 =
                 :QES-SCHD-1STPYDATE-33,
              ESFILE.ES_SCHD_ESCROW_33 = :QES-SCHD-ESCROW-33,
              ESFILE.ES_SCHD_TERM_33 = :QES-SCHD-TERM-33,
              ESFILE.ES_SCHD_COLLECTED_33 =
                   :QES-SCHD-COLLECTED-33,
              ESFILE.ES_SCHD_EFF_DATE_34 =
                 :QES-SCHD-EFF-DATE-34,
              ESFILE.ES_SCHD_1STPYDATE_34 =
                 :QES-SCHD-1STPYDATE-34,
              ESFILE.ES_SCHD_ESCROW_34 = :QES-SCHD-ESCROW-34,
              ESFILE.ES_SCHD_TERM_34 = :QES-SCHD-TERM-34,
              ESFILE.ES_SCHD_COLLECTED_34 =
                   :QES-SCHD-COLLECTED-34,
              ESFILE.ES_SCHD_EFF_DATE_35 =
                 :QES-SCHD-EFF-DATE-35,
              ESFILE.ES_SCHD_1STPYDATE_35 =
                 :QES-SCHD-1STPYDATE-35,
              ESFILE.ES_SCHD_ESCROW_35 = :QES-SCHD-ESCROW-35,
              ESFILE.ES_SCHD_TERM_35 = :QES-SCHD-TERM-35,
              ESFILE.ES_SCHD_COLLECTED_35 =
                   :QES-SCHD-COLLECTED-35,
              ESFILE.ES_SCHD_EFF_DATE_36 =
                 :QES-SCHD-EFF-DATE-36,
              ESFILE.ES_SCHD_1STPYDATE_36 =
                 :QES-SCHD-1STPYDATE-36,
              ESFILE.ES_SCHD_ESCROW_36 = :QES-SCHD-ESCROW-36,
              ESFILE.ES_SCHD_TERM_36 = :QES-SCHD-TERM-36,
              ESFILE.ES_SCHD_COLLECTED_36 =
                   :QES-SCHD-COLLECTED-36,
              ESFILE.ES_SCHD_EFF_DATE_37 =
                 :QES-SCHD-EFF-DATE-37,
              ESFILE.ES_SCHD_1STPYDATE_37 =
                 :QES-SCHD-1STPYDATE-37,
              ESFILE.ES_SCHD_ESCROW_37 = :QES-SCHD-ESCROW-37,
              ESFILE.ES_SCHD_TERM_37 = :QES-SCHD-TERM-37,
              ESFILE.ES_SCHD_COLLECTED_37 =
                   :QES-SCHD-COLLECTED-37,
              ESFILE.ES_SCHD_EFF_DATE_38 =
                 :QES-SCHD-EFF-DATE-38,
              ESFILE.ES_SCHD_1STPYDATE_38 =
                 :QES-SCHD-1STPYDATE-38,
              ESFILE.ES_SCHD_ESCROW_38 = :QES-SCHD-ESCROW-38,
              ESFILE.ES_SCHD_TERM_38 = :QES-SCHD-TERM-38,
              ESFILE.ES_SCHD_COLLECTED_38 =
                   :QES-SCHD-COLLECTED-38,
              ESFILE.ES_SCHD_EFF_DATE_39 =
                 :QES-SCHD-EFF-DATE-39,
              ESFILE.ES_SCHD_1STPYDATE_39 =
                 :QES-SCHD-1STPYDATE-39,
              ESFILE.ES_SCHD_ESCROW_39 = :QES-SCHD-ESCROW-39,
              ESFILE.ES_SCHD_TERM_39 = :QES-SCHD-TERM-39,
              ESFILE.ES_SCHD_COLLECTED_39 =
                   :QES-SCHD-COLLECTED-39,
              ESFILE.ES_SCHD_EFF_DATE_40 =
                 :QES-SCHD-EFF-DATE-40,
              ESFILE.ES_SCHD_1STPYDATE_40 =
                 :QES-SCHD-1STPYDATE-40,
              ESFILE.ES_SCHD_ESCROW_40 = :QES-SCHD-ESCROW-40,
              ESFILE.ES_SCHD_TERM_40 = :QES-SCHD-TERM-40,
              ESFILE.ES_SCHD_COLLECTED_40 =
                   :QES-SCHD-COLLECTED-40,
              ESFILE.ES_BALANCE = :QES-BALANCE,
              ESFILE.ES_PREPAID_1 = :QES-PREPAID-1,
              ESFILE.ES_COLLECTED_1 = :QES-COLLECTED-1,
              ESFILE.ES_DISBURSED_TAXES_1 =
                   :QES-DISBURSED-TAXES-1,
              ESFILE.ES_DISBURSED_INS_1 = :QES-DISBURSED-INS-1,
              ESFILE.ES_DISBURSED_PMI_1 = :QES-DISBURSED-PMI-1,
              ESFILE.ES_DISBURSED_OTHER_1 =
                   :QES-DISBURSED-OTHER-1,
              ESFILE.ES_DISBURSED_REF_1 = :QES-DISBURSED-REF-1,
              ESFILE.ES_DISBURSED_CNT_TAXES_1 =
                   :QES-DISBURSED-CNT-TAXES-1,
              ESFILE.ES_DISBURSED_MISC1_1 =
                   :QES-DISBURSED-MISC1-1,
              ESFILE.ES_DISBURSED_MISC2_1 =
                   :QES-DISBURSED-MISC2-1,
              ESFILE.ES_PREPAID_2 = :QES-PREPAID-2,
              ESFILE.ES_COLLECTED_2 = :QES-COLLECTED-2,
              ESFILE.ES_DISBURSED_TAXES_2 =
                   :QES-DISBURSED-TAXES-2,
              ESFILE.ES_DISBURSED_INS_2 = :QES-DISBURSED-INS-2,
              ESFILE.ES_DISBURSED_PMI_2 = :QES-DISBURSED-PMI-2,
              ESFILE.ES_DISBURSED_OTHER_2 =
                   :QES-DISBURSED-OTHER-2,
              ESFILE.ES_DISBURSED_REF_2 = :QES-DISBURSED-REF-2,
              ESFILE.ES_DISBURSED_CNT_TAXES_2 =
                   :QES-DISBURSED-CNT-TAXES-2,
              ESFILE.ES_DISBURSED_MISC1_2 =
                   :QES-DISBURSED-MISC1-2,
              ESFILE.ES_DISBURSED_MISC2_2 =
                   :QES-DISBURSED-MISC2-2,
              ESFILE.ES_PREPAID_3 = :QES-PREPAID-3,
              ESFILE.ES_COLLECTED_3 = :QES-COLLECTED-3,
              ESFILE.ES_DISBURSED_TAXES_3 =
                   :QES-DISBURSED-TAXES-3,
              ESFILE.ES_DISBURSED_INS_3 = :QES-DISBURSED-INS-3,
              ESFILE.ES_DISBURSED_PMI_3 = :QES-DISBURSED-PMI-3,
              ESFILE.ES_DISBURSED_OTHER_3 =
                   :QES-DISBURSED-OTHER-3,
              ESFILE.ES_DISBURSED_REF_3 = :QES-DISBURSED-REF-3,
              ESFILE.ES_DISBURSED_CNT_TAXES_3 =
                   :QES-DISBURSED-CNT-TAXES-3,
              ESFILE.ES_DISBURSED_MISC1_3 =
                   :QES-DISBURSED-MISC1-3,
              ESFILE.ES_DISBURSED_MISC2_3 =
                   :QES-DISBURSED-MISC2-3,
              ESFILE.ES_MISC_AMT_1 = :QES-MISC-AMT-1,
              ESFILE.ES_EXP_DATE_1 =
                 :QES-EXP-DATE-1,
              ESFILE.ES_MISC_AMT_2 = :QES-MISC-AMT-2,
              ESFILE.ES_EXP_DATE_2 =
                 :QES-EXP-DATE-2,
              ESFILE.ES_MISC_AMT_3 = :QES-MISC-AMT-3,
              ESFILE.ES_EXP_DATE_3 =
                 :QES-EXP-DATE-3,
              ESFILE.ES_MISC_AMT_4 = :QES-MISC-AMT-4,
              ESFILE.ES_EXP_DATE_4 =
                 :QES-EXP-DATE-4,
              ESFILE.ES_MISC_AMT_5 = :QES-MISC-AMT-5,
              ESFILE.ES_EXP_DATE_5 =
                 :QES-EXP-DATE-5,
              ESFILE.ES_MISC_AMT_6 = :QES-MISC-AMT-6,
              ESFILE.ES_EXP_DATE_6 = :QES-EXP-DATE-6
             WHERE ESFILE.ES_BRNO = :QES-BRNO
               AND ESFILE.ES_ACCTNO = :QES-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-ES1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-ES1-FILE.

           MOVE ES-PATH-OWNBR  TO ES-BRNO.
           PERFORM DELETE-IT.
           MOVE ES-PATH-SQL    TO E-FILE.
           MOVE ES1-KEY        TO E-KEYX.
           PERFORM SET-ES-FIELDS.

           EXEC SQL
            DELETE FROM DBO.ESFILE
             WHERE ESFILE.ES_BRNO = :QES-BRNO
               AND ESFILE.ES_ACCTNO = :QES-ACCTNO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-ES1-FILE.

      *-----------------------------------------------------------------
       CLOSE-ES1-FILE.

           PERFORM CLOSE-IT.

           IF ( ES1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE ES1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO ES1-CURSOR-STAT.

