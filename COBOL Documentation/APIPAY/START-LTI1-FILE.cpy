      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.
      *-----------------------------------------------------------------
       START-LTI1-FILE.
           PERFORM START-IT.
           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           MOVE LTI-PATH-SQL    TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.

           IF ( LTI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTI1-FILE.

           IF ( QLTI1-WEND-BRNO = ZEROES    ) OR
              ( QLTI1-WEND-BRNO NOT NUMERIC )
              MOVE LTI-PATH-OWNBR        TO QLTI1-WBEG-BRNO
              MOVE LTI-PATH-OWNBR        TO QLTI1-WEND-BRNO
              MOVE LTI-ACCTNO            TO QLTI1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTI1-WEND-ACCTNO
              MOVE LTI-SEQNO             TO QLTI1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTI1-WEND-SEQNO.


           EXEC SQL
            DECLARE LTI1_CURSOR CURSOR FOR
            SELECT LTIFILE.LTI_BRNO,
                   LTIFILE.LTI_ACCTNO,
                   LTIFILE.LTI_SEQNO,
                   CAST(LTIFILE.LTI_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LTIFILE.LTI_INS_TYPE,
                   CAST(LTIFILE.LTI_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),
                   LTIFILE.LTI_PRIOR_NODEF,
                   LTIFILE.LTI_INSFEECD_1,
                   LTIFILE.LTI_INSFEE_1,
                   LTIFILE.LTI_INSFEECD_2,
                   LTIFILE.LTI_INSFEE_2,
                   LTIFILE.LTI_TRCD,
                   LTIFILE.LTI_INSCOMP,
                   LTIFILE.LTI_INSOURS,
                   CAST(LTIFILE.LTI_INSEFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTIFILE.LTI_INSEXP_DATE
                                                  AS VARCHAR(10)),
                   LTIFILE.LTI_INSPREM,
                   LTIFILE.LTI_INSCOVR,
                   LTIFILE.LTI_INSCOMM,
                   LTIFILE.LTI_ANTICTERM,
                   LTIFILE.LTI_ANTICERN_1,
                   LTIFILE.LTI_ANTICADJ_1,
                   LTIFILE.LTI_UFERN_1,
                   LTIFILE.LTI_ACCERN_1,
                   LTIFILE.LTI_ANTICERN_2,
                   LTIFILE.LTI_ANTICADJ_2,
                   LTIFILE.LTI_UFERN_2,
                   LTIFILE.LTI_ACCERN_2,
                   LTIFILE.LTI_ANTICERN_3,
                   LTIFILE.LTI_ANTICADJ_3,
                   LTIFILE.LTI_UFERN_3,
                   LTIFILE.LTI_ACCERN_3,
                   LTIFILE.LTI_INT_REBATE,
                   LTIFILE.LTI_INS_PAYMNT_INCR,
                   LTIFILE.LTI_INS_TOT_ADDON,
                   LTIFILE.LTI_INT_LAST_EARNED,
                   LTIFILE.LTI_INS_REBATE,
                   LTIFILE.LTI_RB_WORKER,
                   CAST(LTIFILE.LTI_ANTIC_START_DATE
                                                  AS VARCHAR(10)),
                   LTIFILE.LTI_CANCEL_REASON_CD
            FROM DBO.LTIFILE
            WHERE LTIFILE.LTI_BRNO  = :QLTI1-WBEG-BRNO
              AND LTIFILE.LTI_ACCTNO >= :QLTI1-WBEG-ACCTNO
              AND LTIFILE.LTI_ACCTNO <= :QLTI1-WEND-ACCTNO
              AND LTIFILE.LTI_SEQNO >= :QLTI1-WBEG-SEQNO
              AND LTIFILE.LTI_SEQNO <= :QLTI1-WEND-SEQNO
            ORDER BY LTIFILE.LTI_BRNO,
                     LTIFILE.LTI_ACCTNO,
                     LTIFILE.LTI_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTI1-WBEG-BRNO
                          QLTI1-WEND-BRNO
                          QLTI1-WBEG-ACCTNO
                          QLTI1-WEND-ACCTNO
                          QLTI1-WBEG-SEQNO
                          QLTI1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTI1-CURSOR-STAT.

      *-----------------------------------------------------------------
