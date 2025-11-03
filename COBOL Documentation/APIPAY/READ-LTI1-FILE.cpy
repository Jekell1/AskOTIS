      *-----------------------------------------------------------------
       READ-LTI1-FILE.
           PERFORM READ-IT.
           MOVE LTI-PATH-OWNBR  TO LTI-BRNO. 
           MOVE LTI-PATH-SQL    TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.
           INITIALIZE QLTI-REC.

           MOVE LTI-BRNO   TO QLTI-BRNO.
           MOVE LTI-ACCTNO TO QLTI-ACCTNO.
           MOVE LTI-SEQNO  TO QLTI-SEQNO.

           EXEC SQL
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
            INTO :QLTI-REC
            FROM DBO.LTIFILE
            WHERE LTIFILE.LTI_BRNO = :QLTI-BRNO
              AND LTIFILE.LTI_ACCTNO = :QLTI-ACCTNO
              AND LTIFILE.LTI_SEQNO = :QLTI-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTI1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTI-FIELDS.

      *-----------------------------------------------------------------
