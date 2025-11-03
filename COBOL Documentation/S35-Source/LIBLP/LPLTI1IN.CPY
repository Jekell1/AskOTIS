      * COPYMEMBER: LIBLP/LPLTI1IN INSURANCE ADDON TRAILER
      * REV:
      *
       LOAD-LTI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTI-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTI-ALL-DATA
              MOVE LTI-ALL-PATH             TO LTI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTI-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTI-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTI-ALL-DATA
              MOVE LTI-ALL-PATH             TO LTI-PATH.

      *-----------------------------------------------------------------
       OPEN-LTI1-FILE.
           PERFORM LOAD-LTI1-FILE.
           PERFORM OPEN-IT.
           MOVE LTI-PATH-SQL TO E-FILE.

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
       READ-LTI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           MOVE LTI-PATH-SQL    TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.
           INITIALIZE QLTI-REC.

           IF ( LTI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTI1_CURSOR INTO :QLTI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTI-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LTI1-FILE.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           ACCEPT LTI-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTI-PATH-SQL    TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.
           PERFORM SET-LTI-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LTIFILE
             (LTIFILE.LTI_BRNO,
              LTIFILE.LTI_ACCTNO,
              LTIFILE.LTI_SEQNO,
              LTIFILE.LTI_LTOUCH_DATE,
              LTIFILE.LTI_INS_TYPE,
              LTIFILE.LTI_ORIG_1STPYDATE,
              LTIFILE.LTI_PRIOR_NODEF,
              LTIFILE.LTI_INSFEECD_1,
              LTIFILE.LTI_INSFEE_1,
              LTIFILE.LTI_INSFEECD_2,
              LTIFILE.LTI_INSFEE_2,
              LTIFILE.LTI_TRCD,
              LTIFILE.LTI_INSCOMP,
              LTIFILE.LTI_INSOURS,
              LTIFILE.LTI_INSEFF_DATE,
              LTIFILE.LTI_INSEXP_DATE,
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
              LTIFILE.LTI_ANTIC_START_DATE,
              LTIFILE.LTI_CANCEL_REASON_CD)
            VALUES
             (:QLTI-BRNO,
              :QLTI-ACCTNO,
              :QLTI-SEQNO,
              :QLTI-LTOUCH-DATE,
              :QLTI-INS-TYPE,
              :QLTI-ORIG-1STPYDATE,
              :QLTI-PRIOR-NODEF,
              :QLTI-INSFEECD-1,
              :QLTI-INSFEE-1,
              :QLTI-INSFEECD-2,
              :QLTI-INSFEE-2,
              :QLTI-TRCD,
              :QLTI-INSCOMP,
              :QLTI-INSOURS,
              :QLTI-INSEFF-DATE,
              :QLTI-INSEXP-DATE,
              :QLTI-INSPREM,
              :QLTI-INSCOVR,
              :QLTI-INSCOMM,
              :QLTI-ANTICTERM,
              :QLTI-ANTICERN-1,
              :QLTI-ANTICADJ-1,
              :QLTI-UFERN-1,
              :QLTI-ACCERN-1,
              :QLTI-ANTICERN-2,
              :QLTI-ANTICADJ-2,
              :QLTI-UFERN-2,
              :QLTI-ACCERN-2,
              :QLTI-ANTICERN-3,
              :QLTI-ANTICADJ-3,
              :QLTI-UFERN-3,
              :QLTI-ACCERN-3,
              :QLTI-INT-REBATE,
              :QLTI-INS-PAYMNT-INCR,
              :QLTI-INS-TOT-ADDON,
              :QLTI-INT-LAST-EARNED,
              :QLTI-INS-REBATE,
              :QLTI-RB-WORKER,
              :QLTI-ANTIC-START-DATE,
              :QLTI-CANCEL-REASON-CD)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTI1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LTI1-FILE.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           ACCEPT LTI-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTI-PATH-SQL    TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.
           PERFORM SET-LTI-FIELDS.

           EXEC SQL
            UPDATE DBO.LTIFILE
             SET
              LTIFILE.LTI_BRNO = :QLTI-BRNO,
              LTIFILE.LTI_ACCTNO = :QLTI-ACCTNO,
              LTIFILE.LTI_SEQNO = :QLTI-SEQNO,
              LTIFILE.LTI_LTOUCH_DATE =
                 :QLTI-LTOUCH-DATE,
              LTIFILE.LTI_INS_TYPE = :QLTI-INS-TYPE,
              LTIFILE.LTI_ORIG_1STPYDATE =
                 :QLTI-ORIG-1STPYDATE,
              LTIFILE.LTI_PRIOR_NODEF = :QLTI-PRIOR-NODEF,
              LTIFILE.LTI_INSFEECD_1 = :QLTI-INSFEECD-1,
              LTIFILE.LTI_INSFEE_1 = :QLTI-INSFEE-1,
              LTIFILE.LTI_INSFEECD_2 = :QLTI-INSFEECD-2,
              LTIFILE.LTI_INSFEE_2 = :QLTI-INSFEE-2,
              LTIFILE.LTI_TRCD = :QLTI-TRCD,
              LTIFILE.LTI_INSCOMP = :QLTI-INSCOMP,
              LTIFILE.LTI_INSOURS = :QLTI-INSOURS,
              LTIFILE.LTI_INSEFF_DATE =
                 :QLTI-INSEFF-DATE,
              LTIFILE.LTI_INSEXP_DATE =
                 :QLTI-INSEXP-DATE,
              LTIFILE.LTI_INSPREM = :QLTI-INSPREM,
              LTIFILE.LTI_INSCOVR = :QLTI-INSCOVR,
              LTIFILE.LTI_INSCOMM = :QLTI-INSCOMM,
              LTIFILE.LTI_ANTICTERM = :QLTI-ANTICTERM,
              LTIFILE.LTI_ANTICERN_1 = :QLTI-ANTICERN-1,
              LTIFILE.LTI_ANTICADJ_1 = :QLTI-ANTICADJ-1,
              LTIFILE.LTI_UFERN_1 = :QLTI-UFERN-1,
              LTIFILE.LTI_ACCERN_1 = :QLTI-ACCERN-1,
              LTIFILE.LTI_ANTICERN_2 = :QLTI-ANTICERN-2,
              LTIFILE.LTI_ANTICADJ_2 = :QLTI-ANTICADJ-2,
              LTIFILE.LTI_UFERN_2 = :QLTI-UFERN-2,
              LTIFILE.LTI_ACCERN_2 = :QLTI-ACCERN-2,
              LTIFILE.LTI_ANTICERN_3 = :QLTI-ANTICERN-3,
              LTIFILE.LTI_ANTICADJ_3 = :QLTI-ANTICADJ-3,
              LTIFILE.LTI_UFERN_3 = :QLTI-UFERN-3,
              LTIFILE.LTI_ACCERN_3 = :QLTI-ACCERN-3,
              LTIFILE.LTI_INT_REBATE = :QLTI-INT-REBATE,
              LTIFILE.LTI_INS_PAYMNT_INCR = :QLTI-INS-PAYMNT-INCR,
              LTIFILE.LTI_INS_TOT_ADDON = :QLTI-INS-TOT-ADDON,
              LTIFILE.LTI_INT_LAST_EARNED = :QLTI-INT-LAST-EARNED,
              LTIFILE.LTI_INS_REBATE = :QLTI-INS-REBATE,
              LTIFILE.LTI_RB_WORKER = :QLTI-RB-WORKER,
              LTIFILE.LTI_ANTIC_START_DATE = :QLTI-ANTIC-START-DATE,
              LTIFILE.LTI_CANCEL_REASON_CD =
                   :QLTI-CANCEL-REASON-CD
             WHERE LTIFILE.LTI_BRNO = :QLTI-BRNO
               AND LTIFILE.LTI_ACCTNO = :QLTI-ACCTNO
               AND LTIFILE.LTI_SEQNO = :QLTI-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTI1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LTI1-FILE.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           PERFORM DELETE-IT.
           MOVE LTI-PATH-SQL    TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.
           PERFORM SET-LTI-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LTIFILE
             WHERE LTIFILE.LTI_BRNO = :QLTI-BRNO
               AND LTIFILE.LTI_ACCTNO = :QLTI-ACCTNO
               AND LTIFILE.LTI_SEQNO = :QLTI-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LTI1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LTI1-FILE.

           PERFORM CLOSE-IT.

           IF ( LTI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTI1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTI1-CURSOR-STAT.

