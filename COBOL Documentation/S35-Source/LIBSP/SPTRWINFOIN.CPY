      * COPYMEMBER: LIBSP/SPTRWINFOI
      * REV:
      *  2025.0304 BAH ADDED TRW-INFO-INNOVIS-ID #1702 S35Q-169
       LOAD-TRW-INFO-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRW-INFO-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRW-INFO-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO TRW-INFO-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRW-INFO-PATH-MACHINE
              MOVE TRW-INFO-PATH-FORMAT    TO TRW-INFO-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRW-INFO-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO TRW-INFO-PATH-BASE
              MOVE FILPATH-MACHINE         TO TRW-INFO-PATH-MACHINE
              MOVE TRW-INFO-PATH-FORMAT    TO TRW-INFO-PATH.

      *-----------------------------------------------------------------
       OPEN-TRW-INFO-FILE.
           PERFORM LOAD-TRW-INFO-FILE.
           PERFORM OPEN-IT.
           MOVE TRW-INFO-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRW-INFO-FILE.
           PERFORM START-IT.
           MOVE TRW-INFO-PATH-SQL TO E-FILE.
           MOVE TRW-INFO-KEY TO E-KEYX.

           IF ( TRW-INFO-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRW-INFO-FILE.

           IF ( QTRW-INFO-WEND-INFO-ID = ZEROES    ) OR
              ( QTRW-INFO-WEND-INFO-ID NOT NUMERIC )
              MOVE TRW-INFO-ID         TO QTRW-INFO-WBEG-INFO-ID
              MOVE 9999                TO QTRW-INFO-WEND-INFO-ID.

           EXEC SQL
            DECLARE TRWINFO_CURSOR CURSOR FOR
            SELECT TRWINFO.TRW_INFO_ID,
                   TRWINFO.TRW_INFO_EQUIFAX_ID,
                   TRWINFO.TRW_INFO_EXPERIAN_ID,
                   TRWINFO.TRW_INFO_TUC_ID,
                   TRWINFO.TRW_INFO_REPORTER_NAME,
                   TRWINFO.TRW_INFO_REPORTER_ADR1,
                   TRWINFO.TRW_INFO_REPORTER_ADR2,
                   TRWINFO.TRW_INFO_REPORTER_CSZ,
                   TRWINFO.TRW_INFO_REPORTER_PHONE,
                   TRWINFO.TRW_INFO_GRANTOR_CODE,
                   TRWINFO.TRW_INFO_CR_LF,
                   TRWINFO.TRW_INFO_BRNO_ACCTNO,
                   TRWINFO.TRW_INFO_IDENT_BRNO,
                   TRWINFO.TRW_YEARS_STOP_REPORTING,
                   TRWINFO.TRW_INVALID_SSNO,
                   TRWINFO.TRW_INFO_SCHED_PAY,
                   CAST(TRWINFO.TRW_INFO_LTOUCH_DATE AS VARCHAR(10)),
                   TRWINFO.TRW_INFO_INNOVIS_ID
            FROM DBO.TRWINFO
            WHERE TRWINFO.TRW_INFO_ID >= :QTRW-INFO-WBEG-INFO-ID
              AND TRWINFO.TRW_INFO_ID <= :QTRW-INFO-WEND-INFO-ID
            ORDER BY TRWINFO.TRW_INFO_ID
           END-EXEC.

           EXEC SQL
               OPEN TRWINFO_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRW-INFO-WBEG-INFO-ID
                          QTRW-INFO-WEND-INFO-ID.

           MOVE STAT---GOOD TO TRW-INFO-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRW-INFO-FILE.
           PERFORM READ-IT.
           MOVE TRW-INFO-PATH-SQL TO E-FILE.
           MOVE TRW-INFO-KEY TO E-KEYX.
           INITIALIZE QTRWINFO-REC.

           MOVE TRW-INFO-ID  TO QTRW-INFO-ID. 

           EXEC SQL
            SELECT TRWINFO.TRW_INFO_ID,
                   TRWINFO.TRW_INFO_EQUIFAX_ID,
                   TRWINFO.TRW_INFO_EXPERIAN_ID,
                   TRWINFO.TRW_INFO_TUC_ID,
                   TRWINFO.TRW_INFO_REPORTER_NAME,
                   TRWINFO.TRW_INFO_REPORTER_ADR1,
                   TRWINFO.TRW_INFO_REPORTER_ADR2,
                   TRWINFO.TRW_INFO_REPORTER_CSZ,
                   TRWINFO.TRW_INFO_REPORTER_PHONE,
                   TRWINFO.TRW_INFO_GRANTOR_CODE,
                   TRWINFO.TRW_INFO_CR_LF,
                   TRWINFO.TRW_INFO_BRNO_ACCTNO,
                   TRWINFO.TRW_INFO_IDENT_BRNO,
                   TRWINFO.TRW_YEARS_STOP_REPORTING,
                   TRWINFO.TRW_INVALID_SSNO,
                   TRWINFO.TRW_INFO_SCHED_PAY,
                   CAST(TRWINFO.TRW_INFO_LTOUCH_DATE AS VARCHAR(10)),
                   TRWINFO.TRW_INFO_INNOVIS_ID
            INTO :QTRWINFO-REC
            FROM DBO.TRWINFO
            WHERE TRWINFO.TRW_INFO_ID = :QTRW-INFO-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRW-INFO-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRWINFO-FIELDS.

      *-----------------------------------------------------------------
       READ-TRW-INFO-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRW-INFO-PATH-SQL TO E-FILE.
           MOVE TRW-INFO-KEY TO E-KEYX.
           INITIALIZE QTRWINFO-REC.

           IF ( TRW-INFO-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRWINFO_CURSOR INTO :QTRWINFO-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRW-INFO-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRWINFO-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRW-INFO-FILE.

           ACCEPT TRW-INFO-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRW-INFO-PATH-SQL TO E-FILE.
           MOVE TRW-INFO-KEY TO E-KEYX.
           PERFORM SET-TRWINFO-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRWINFO
             (TRWINFO.TRW_INFO_ID,
              TRWINFO.TRW_INFO_EQUIFAX_ID,
              TRWINFO.TRW_INFO_EXPERIAN_ID,
              TRWINFO.TRW_INFO_TUC_ID,
              TRWINFO.TRW_INFO_REPORTER_NAME,
              TRWINFO.TRW_INFO_REPORTER_ADR1,
              TRWINFO.TRW_INFO_REPORTER_ADR2,
              TRWINFO.TRW_INFO_REPORTER_CSZ,
              TRWINFO.TRW_INFO_REPORTER_PHONE,
              TRWINFO.TRW_INFO_GRANTOR_CODE,
              TRWINFO.TRW_INFO_CR_LF,
              TRWINFO.TRW_INFO_BRNO_ACCTNO,
              TRWINFO.TRW_INFO_IDENT_BRNO,
              TRWINFO.TRW_YEARS_STOP_REPORTING,
              TRWINFO.TRW_INVALID_SSNO,
              TRWINFO.TRW_INFO_SCHED_PAY,
              TRWINFO.TRW_INFO_LTOUCH_DATE,
              TRWINFO.TRW_INFO_INNOVIS_ID)
            VALUES
             (:QTRW-INFO-ID,
              :QTRW-INFO-EQUIFAX-ID,
              :QTRW-INFO-EXPERIAN-ID,
              :QTRW-INFO-TUC-ID,
              :QTRW-INFO-REPORTER-NAME,
              :QTRW-INFO-REPORTER-ADR1,
              :QTRW-INFO-REPORTER-ADR2,
              :QTRW-INFO-REPORTER-CSZ,
              :QTRW-INFO-REPORTER-PHONE,
              :QTRW-INFO-GRANTOR-CODE,
              :QTRW-INFO-CR-LF,
              :QTRW-INFO-BRNO-ACCTNO,
              :QTRW-INFO-IDENT-BRNO,
              :QTRW-YEARS-STOP-REPORTING,
              :QTRW-INVALID-SSNO,
              :QTRW-INFO-SCHED-PAY,
              :QTRW-INFO-LTOUCH-DATE,
              :QTRW-INFO-INNOVIS-ID)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRW-INFO-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRW-INFO-FILE.

           ACCEPT TRW-INFO-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRW-INFO-PATH-SQL TO E-FILE.
           MOVE TRW-INFO-KEY TO E-KEYX.
           PERFORM SET-TRWINFO-FIELDS.

           EXEC SQL
            UPDATE DBO.TRWINFO
             SET
              TRWINFO.TRW_INFO_ID = :QTRW-INFO-ID,
              TRWINFO.TRW_INFO_EQUIFAX_ID = :QTRW-INFO-EQUIFAX-ID,
              TRWINFO.TRW_INFO_EXPERIAN_ID =
                   :QTRW-INFO-EXPERIAN-ID,
              TRWINFO.TRW_INFO_TUC_ID = :QTRW-INFO-TUC-ID,
              TRWINFO.TRW_INFO_REPORTER_NAME =
                   :QTRW-INFO-REPORTER-NAME,
              TRWINFO.TRW_INFO_REPORTER_ADR1 =
                   :QTRW-INFO-REPORTER-ADR1,
              TRWINFO.TRW_INFO_REPORTER_ADR2 =
                   :QTRW-INFO-REPORTER-ADR2,
              TRWINFO.TRW_INFO_REPORTER_CSZ =
                   :QTRW-INFO-REPORTER-CSZ,
              TRWINFO.TRW_INFO_REPORTER_PHONE =
                   :QTRW-INFO-REPORTER-PHONE,
              TRWINFO.TRW_INFO_GRANTOR_CODE =
                   :QTRW-INFO-GRANTOR-CODE,
              TRWINFO.TRW_INFO_CR_LF = :QTRW-INFO-CR-LF,
              TRWINFO.TRW_INFO_BRNO_ACCTNO =
                   :QTRW-INFO-BRNO-ACCTNO,
              TRWINFO.TRW_INFO_IDENT_BRNO = :QTRW-INFO-IDENT-BRNO,
              TRWINFO.TRW_YEARS_STOP_REPORTING =
                   :QTRW-YEARS-STOP-REPORTING,
              TRWINFO.TRW_INVALID_SSNO = :QTRW-INVALID-SSNO,
              TRWINFO.TRW_INFO_SCHED_PAY = :QTRW-INFO-SCHED-PAY,
              TRWINFO.TRW_INFO_LTOUCH_DATE = :QTRW-INFO-LTOUCH-DATE,
              TRWINFO.TRW_INFO_INNOVIS_ID = :QTRW-INFO-INNOVIS-ID
            WHERE TRWINFO.TRW_INFO_ID = :QTRW-INFO-ID
              AND TRWINFO.TRW_INFO_ID = :QTRW-INFO-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRW-INFO-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRW-INFO-FILE.

           PERFORM DELETE-IT.
           MOVE TRW-INFO-PATH-SQL TO E-FILE.
           MOVE TRW-INFO-KEY TO E-KEYX.
           PERFORM SET-TRWINFO-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRWINFO
            WHERE TRWINFO.TRW_INFO_ID = :QTRW-INFO-ID
              AND TRWINFO.TRW_INFO_ID = :QTRW-INFO-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRW-INFO-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRW-INFO-FILE.

           PERFORM CLOSE-IT.

           IF ( TRW-INFO-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRWINFO_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRW-INFO-CURSOR-STAT.

