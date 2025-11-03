      * COPYMEMBER: LIBLP/LPLTB1IN
      * REV:
       LOAD-LTB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTB-ALL-DATA
              MOVE LTB-ALL-PATH             TO LTB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTB-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTB-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTB-ALL-DATA
              MOVE LTB-ALL-PATH             TO LTB-PATH.

      *-----------------------------------------------------------------
       OPEN-LTB1-FILE.
           PERFORM LOAD-LTB1-FILE.
           PERFORM OPEN-IT.
           MOVE LTB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTB1-FILE.
           PERFORM START-IT.
           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.

           IF ( LTB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTB1-FILE.

           IF ( QLTB1-WEND-BRNO = ZEROES    ) OR
              ( QLTB1-WEND-BRNO NOT NUMERIC )
              MOVE LTB-PATH-OWNBR        TO QLTB1-WBEG-BRNO
              MOVE LTB-PATH-OWNBR        TO QLTB1-WEND-BRNO
              MOVE LTB-ACCTNO            TO QLTB1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTB1-WEND-ACCTNO
              MOVE LTB-SEQNO             TO QLTB1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTB1-WEND-SEQNO.


           EXEC SQL
            DECLARE LTB1_CURSOR CURSOR FOR
            SELECT LTBFILE.LTB_BRNO,
                   LTBFILE.LTB_ACCTNO,
                   LTBFILE.LTB_SEQNO,
                   CAST(LTBFILE.LTB_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_FILEDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_CASE_NO,
                   LTBFILE.LTB_CHAPTER,
                   LTBFILE.LTB_MISC1,
                   LTBFILE.LTB_MISC2,
                   LTBFILE.LTB_MISC3,
                   CAST(LTBFILE.LTB_BDDATE AS VARCHAR(10)),
                   LTBFILE.LTB_WHO_FILED,
                   LTBFILE.LTB_ENDORSER_FILED,
                   LTBFILE.LTB_CLAIM_AMT,
                   LTBFILE.LTB_ACTIVE_INACTIVE,
                   LTBFILE.LTB_INT_LC_DUE,
                   LTBFILE.LTB_OTH_DUE,
                   LTBFILE.LTB_TDUEAMT,
                   LTBFILE.LTB_NAME_1,
                   LTBFILE.LTB_ADR1_1,
                   LTBFILE.LTB_ADR2_1,
                   LTBFILE.LTB_CITY_1,
                   LTBFILE.LTB_STATE_1,
                   LTBFILE.LTB_ZIP_1,
                   LTBFILE.LTB_PHONE_1,
                   LTBFILE.LTB_NAME_2,
                   LTBFILE.LTB_ADR1_2,
                   LTBFILE.LTB_ADR2_2,
                   LTBFILE.LTB_CITY_2,
                   LTBFILE.LTB_STATE_2,
                   LTBFILE.LTB_ZIP_2,
                   LTBFILE.LTB_PHONE_2,
                   LTBFILE.LTB_NAME_3,
                   LTBFILE.LTB_ADR1_3,
                   LTBFILE.LTB_ADR2_3,
                   LTBFILE.LTB_CITY_3,
                   LTBFILE.LTB_STATE_3,
                   LTBFILE.LTB_ZIP_3,
                   LTBFILE.LTB_PHONE_3,
                   CAST(LTBFILE.LTB_REAFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_REAFF_SYSDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_REAFF_BALANCE,
                   LTBFILE.LTB_REAFF_PAYAMT,
                   LTBFILE.LTB_REAFF_NO_PAYS,
                   LTBFILE.LTB_INTRATE,
                   CAST(LTBFILE.LTB_RESCIND_DATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_PARTIAL_FULL,
                   CAST(LTBFILE.LTB_DISCHARGE_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_DISCHARGE_SYSDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_GROSS_BAL
            FROM DBO.LTBFILE
            WHERE LTBFILE.LTB_BRNO  = :QLTB1-WBEG-BRNO
              AND LTBFILE.LTB_ACCTNO >= :QLTB1-WBEG-ACCTNO
              AND LTBFILE.LTB_ACCTNO <= :QLTB1-WEND-ACCTNO
              AND LTBFILE.LTB_SEQNO >= :QLTB1-WBEG-SEQNO
              AND LTBFILE.LTB_SEQNO <= :QLTB1-WEND-SEQNO
            ORDER BY LTBFILE.LTB_BRNO,
                     LTBFILE.LTB_ACCTNO,
                     LTBFILE.LTB_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTB1-WBEG-BRNO
                          QLTB1-WEND-BRNO
                          QLTB1-WBEG-ACCTNO
                          QLTB1-WEND-ACCTNO
                          QLTB1-WBEG-SEQNO
                          QLTB1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTB1-FILE.
           PERFORM READ-IT.
           MOVE LTB-PATH-OWNBR  TO LTB-BRNO. 
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           INITIALIZE QLTB-REC.

           MOVE LTB-BRNO   TO QLTB-BRNO.
           MOVE LTB-ACCTNO TO QLTB-ACCTNO.
           MOVE LTB-SEQNO  TO QLTB-SEQNO.

           EXEC SQL
            SELECT LTBFILE.LTB_BRNO,
                   LTBFILE.LTB_ACCTNO,
                   LTBFILE.LTB_SEQNO,
                   CAST(LTBFILE.LTB_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_FILEDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_CASE_NO,
                   LTBFILE.LTB_CHAPTER,
                   LTBFILE.LTB_MISC1,
                   LTBFILE.LTB_MISC2,
                   LTBFILE.LTB_MISC3,
                   CAST(LTBFILE.LTB_BDDATE AS VARCHAR(10)),
                   LTBFILE.LTB_WHO_FILED,
                   LTBFILE.LTB_ENDORSER_FILED,
                   LTBFILE.LTB_CLAIM_AMT,
                   LTBFILE.LTB_ACTIVE_INACTIVE,
                   LTBFILE.LTB_INT_LC_DUE,
                   LTBFILE.LTB_OTH_DUE,
                   LTBFILE.LTB_TDUEAMT,
                   LTBFILE.LTB_NAME_1,
                   LTBFILE.LTB_ADR1_1,
                   LTBFILE.LTB_ADR2_1,
                   LTBFILE.LTB_CITY_1,
                   LTBFILE.LTB_STATE_1,
                   LTBFILE.LTB_ZIP_1,
                   LTBFILE.LTB_PHONE_1,
                   LTBFILE.LTB_NAME_2,
                   LTBFILE.LTB_ADR1_2,
                   LTBFILE.LTB_ADR2_2,
                   LTBFILE.LTB_CITY_2,
                   LTBFILE.LTB_STATE_2,
                   LTBFILE.LTB_ZIP_2,
                   LTBFILE.LTB_PHONE_2,
                   LTBFILE.LTB_NAME_3,
                   LTBFILE.LTB_ADR1_3,
                   LTBFILE.LTB_ADR2_3,
                   LTBFILE.LTB_CITY_3,
                   LTBFILE.LTB_STATE_3,
                   LTBFILE.LTB_ZIP_3,
                   LTBFILE.LTB_PHONE_3,
                   CAST(LTBFILE.LTB_REAFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_REAFF_SYSDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_REAFF_BALANCE,
                   LTBFILE.LTB_REAFF_PAYAMT,
                   LTBFILE.LTB_REAFF_NO_PAYS,
                   LTBFILE.LTB_INTRATE,
                   CAST(LTBFILE.LTB_RESCIND_DATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_PARTIAL_FULL,
                   CAST(LTBFILE.LTB_DISCHARGE_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_DISCHARGE_SYSDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_GROSS_BAL
            INTO :QLTB-REC
            FROM DBO.LTBFILE
            WHERE LTBFILE.LTB_BRNO = :QLTB-BRNO
              AND LTBFILE.LTB_ACCTNO = :QLTB-ACCTNO
              AND LTBFILE.LTB_SEQNO = :QLTB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTB-FIELDS.

      *-----------------------------------------------------------------
       READ-LTB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           INITIALIZE QLTB-REC.

           IF ( LTB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTB1_CURSOR INTO :QLTB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LTB1-FILE.

           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           ACCEPT LTB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           PERFORM SET-LTB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LTBFILE
             (LTBFILE.LTB_BRNO,
              LTBFILE.LTB_ACCTNO,
              LTBFILE.LTB_SEQNO,
              LTBFILE.LTB_LTOUCH_DATE,
              LTBFILE.LTB_FILEDATE,
              LTBFILE.LTB_CASE_NO,
              LTBFILE.LTB_CHAPTER,
              LTBFILE.LTB_MISC1,
              LTBFILE.LTB_MISC2,
              LTBFILE.LTB_MISC3,
              LTBFILE.LTB_BDDATE,
              LTBFILE.LTB_WHO_FILED,
              LTBFILE.LTB_ENDORSER_FILED,
              LTBFILE.LTB_CLAIM_AMT,
              LTBFILE.LTB_ACTIVE_INACTIVE,
              LTBFILE.LTB_INT_LC_DUE,
              LTBFILE.LTB_OTH_DUE,
              LTBFILE.LTB_TDUEAMT,
              LTBFILE.LTB_NAME_1,
              LTBFILE.LTB_ADR1_1,
              LTBFILE.LTB_ADR2_1,
              LTBFILE.LTB_CITY_1,
              LTBFILE.LTB_STATE_1,
              LTBFILE.LTB_ZIP_1,
              LTBFILE.LTB_PHONE_1,
              LTBFILE.LTB_NAME_2,
              LTBFILE.LTB_ADR1_2,
              LTBFILE.LTB_ADR2_2,
              LTBFILE.LTB_CITY_2,
              LTBFILE.LTB_STATE_2,
              LTBFILE.LTB_ZIP_2,
              LTBFILE.LTB_PHONE_2,
              LTBFILE.LTB_NAME_3,
              LTBFILE.LTB_ADR1_3,
              LTBFILE.LTB_ADR2_3,
              LTBFILE.LTB_CITY_3,
              LTBFILE.LTB_STATE_3,
              LTBFILE.LTB_ZIP_3,
              LTBFILE.LTB_PHONE_3,
              LTBFILE.LTB_REAFF_DATE,
              LTBFILE.LTB_REAFF_SYSDATE,
              LTBFILE.LTB_REAFF_BALANCE,
              LTBFILE.LTB_REAFF_PAYAMT,
              LTBFILE.LTB_REAFF_NO_PAYS,
              LTBFILE.LTB_INTRATE,
              LTBFILE.LTB_RESCIND_DATE,
              LTBFILE.LTB_PARTIAL_FULL,
              LTBFILE.LTB_DISCHARGE_DATE,
              LTBFILE.LTB_DISCHARGE_SYSDATE,
              LTBFILE.LTB_GROSS_BAL)
            VALUES
             (:QLTB-BRNO,
              :QLTB-ACCTNO,
              :QLTB-SEQNO,
              :QLTB-LTOUCH-DATE,
              :QLTB-FILEDATE,
              :QLTB-CASE-NO,
              :QLTB-CHAPTER,
              :QLTB-MISC1,
              :QLTB-MISC2,
              :QLTB-MISC3,
              :QLTB-BDDATE,
              :QLTB-WHO-FILED,
              :QLTB-ENDORSER-FILED,
              :QLTB-CLAIM-AMT,
              :QLTB-ACTIVE-INACTIVE,
              :QLTB-INT-LC-DUE,
              :QLTB-OTH-DUE,
              :QLTB-TDUEAMT,
              :QLTB-NAME-1,
              :QLTB-ADR1-1,
              :QLTB-ADR2-1,
              :QLTB-CITY-1,
              :QLTB-STATE-1,
              :QLTB-ZIP-1,
              :QLTB-PHONE-1,
              :QLTB-NAME-2,
              :QLTB-ADR1-2,
              :QLTB-ADR2-2,
              :QLTB-CITY-2,
              :QLTB-STATE-2,
              :QLTB-ZIP-2,
              :QLTB-PHONE-2,
              :QLTB-NAME-3,
              :QLTB-ADR1-3,
              :QLTB-ADR2-3,
              :QLTB-CITY-3,
              :QLTB-STATE-3,
              :QLTB-ZIP-3,
              :QLTB-PHONE-3,
              :QLTB-REAFF-DATE,
              :QLTB-REAFF-SYSDATE,
              :QLTB-REAFF-BALANCE,
              :QLTB-REAFF-PAYAMT,
              :QLTB-REAFF-NO-PAYS,
              :QLTB-INTRATE,
              :QLTB-RESCIND-DATE,
              :QLTB-PARTIAL-FULL,
              :QLTB-DISCHARGE-DATE,
              :QLTB-DISCHARGE-SYSDATE,
              :QLTB-GROSS-BAL)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LTB1-FILE.

           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           ACCEPT LTB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           PERFORM SET-LTB-FIELDS.

           EXEC SQL
            UPDATE DBO.LTBFILE
             SET
              LTBFILE.LTB_BRNO = :QLTB-BRNO,
              LTBFILE.LTB_ACCTNO = :QLTB-ACCTNO,
              LTBFILE.LTB_SEQNO = :QLTB-SEQNO,
              LTBFILE.LTB_LTOUCH_DATE =
                 :QLTB-LTOUCH-DATE,
              LTBFILE.LTB_FILEDATE =
                 :QLTB-FILEDATE,
              LTBFILE.LTB_CASE_NO = :QLTB-CASE-NO,
              LTBFILE.LTB_CHAPTER = :QLTB-CHAPTER,
              LTBFILE.LTB_MISC1 = :QLTB-MISC1,
              LTBFILE.LTB_MISC2 = :QLTB-MISC2,
              LTBFILE.LTB_MISC3 = :QLTB-MISC3,
              LTBFILE.LTB_BDDATE =
                 :QLTB-BDDATE,
              LTBFILE.LTB_WHO_FILED = :QLTB-WHO-FILED,
              LTBFILE.LTB_ENDORSER_FILED = :QLTB-ENDORSER-FILED,
              LTBFILE.LTB_CLAIM_AMT = :QLTB-CLAIM-AMT,
              LTBFILE.LTB_ACTIVE_INACTIVE = :QLTB-ACTIVE-INACTIVE,
              LTBFILE.LTB_INT_LC_DUE = :QLTB-INT-LC-DUE,
              LTBFILE.LTB_OTH_DUE = :QLTB-OTH-DUE,
              LTBFILE.LTB_TDUEAMT = :QLTB-TDUEAMT,
              LTBFILE.LTB_NAME_1 = :QLTB-NAME-1,
              LTBFILE.LTB_ADR1_1 = :QLTB-ADR1-1,
              LTBFILE.LTB_ADR2_1 = :QLTB-ADR2-1,
              LTBFILE.LTB_CITY_1 = :QLTB-CITY-1,
              LTBFILE.LTB_STATE_1 = :QLTB-STATE-1,
              LTBFILE.LTB_ZIP_1 = :QLTB-ZIP-1,
              LTBFILE.LTB_PHONE_1 = :QLTB-PHONE-1,
              LTBFILE.LTB_NAME_2 = :QLTB-NAME-2,
              LTBFILE.LTB_ADR1_2 = :QLTB-ADR1-2,
              LTBFILE.LTB_ADR2_2 = :QLTB-ADR2-2,
              LTBFILE.LTB_CITY_2 = :QLTB-CITY-2,
              LTBFILE.LTB_STATE_2 = :QLTB-STATE-2,
              LTBFILE.LTB_ZIP_2 = :QLTB-ZIP-2,
              LTBFILE.LTB_PHONE_2 = :QLTB-PHONE-2,
              LTBFILE.LTB_NAME_3 = :QLTB-NAME-3,
              LTBFILE.LTB_ADR1_3 = :QLTB-ADR1-3,
              LTBFILE.LTB_ADR2_3 = :QLTB-ADR2-3,
              LTBFILE.LTB_CITY_3 = :QLTB-CITY-3,
              LTBFILE.LTB_STATE_3 = :QLTB-STATE-3,
              LTBFILE.LTB_ZIP_3 = :QLTB-ZIP-3,
              LTBFILE.LTB_PHONE_3 = :QLTB-PHONE-3,
              LTBFILE.LTB_REAFF_DATE =
                 :QLTB-REAFF-DATE,
              LTBFILE.LTB_REAFF_SYSDATE =
                 :QLTB-REAFF-SYSDATE,
              LTBFILE.LTB_REAFF_BALANCE = :QLTB-REAFF-BALANCE,
              LTBFILE.LTB_REAFF_PAYAMT = :QLTB-REAFF-PAYAMT,
              LTBFILE.LTB_REAFF_NO_PAYS = :QLTB-REAFF-NO-PAYS,
              LTBFILE.LTB_INTRATE = :QLTB-INTRATE,
              LTBFILE.LTB_RESCIND_DATE =
                 :QLTB-RESCIND-DATE,
              LTBFILE.LTB_PARTIAL_FULL = :QLTB-PARTIAL-FULL,
              LTBFILE.LTB_DISCHARGE_DATE =
                 :QLTB-DISCHARGE-DATE,
              LTBFILE.LTB_DISCHARGE_SYSDATE = :QLTB-DISCHARGE-SYSDATE,
              LTBFILE.LTB_GROSS_BAL = :QLTB-GROSS-BAL
             WHERE LTBFILE.LTB_BRNO = :QLTB-BRNO
               AND LTBFILE.LTB_ACCTNO = :QLTB-ACCTNO
               AND LTBFILE.LTB_SEQNO = :QLTB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LTB1-FILE.

           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           PERFORM DELETE-IT.
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           PERFORM SET-LTB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LTBFILE
             WHERE LTBFILE.LTB_BRNO = :QLTB-BRNO
               AND LTBFILE.LTB_ACCTNO = :QLTB-ACCTNO
               AND LTBFILE.LTB_SEQNO = :QLTB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LTB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LTB1-FILE.

           PERFORM CLOSE-IT.

           IF ( LTB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTB1-CURSOR-STAT.

