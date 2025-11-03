      ******************************************************************
      *
      * COPYMEMBER: LIBLP/LPLTC1IN_NOIX - DOES NOT CREATE IX
      *
      ******************************************************************
       LOAD-LTC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTC-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH.

      *-----------------------------------------------------------------
       OPEN-LTC1-FILE.
           PERFORM LOAD-LTC1-FILE.
           PERFORM OPEN-IT.
           MOVE LTC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTC1-FILE.
           PERFORM START-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-PATH-SQL    TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.

           IF ( LTC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTC1-FILE.

           IF ( QLTC1-WEND-BRNO = ZEROES    ) OR
              ( QLTC1-WEND-BRNO NOT NUMERIC )
              MOVE LTC-PATH-OWNBR        TO QLTC1-WBEG-BRNO
              MOVE LTC-PATH-OWNBR        TO QLTC1-WEND-BRNO
              MOVE LTC-ACCTNO            TO QLTC1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTC1-WEND-ACCTNO
              MOVE LTC-SEQNO             TO QLTC1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTC1-WEND-SEQNO.

           EXEC SQL
            DECLARE LTC1_CURSOR CURSOR FOR
            SELECT LTCFILE.LTC_BRNO,
                   LTCFILE.LTC_ACCTNO,
                   LTCFILE.LTC_SEQNO,
                   CAST(LTCFILE.LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LTCFILE.LTC_DESC_1,
                   LTCFILE.LTC_MAKE_1,
                   LTCFILE.LTC_MODEL_1,
                   LTCFILE.LTC_YEAR_1,
                   LTCFILE.LTC_STYLE_1,
                   LTCFILE.LTC_CLASS_1,
                   LTCFILE.LTC_VIN_1,
                   LTCFILE.LTC_TITLE_NO_1,
                   LTCFILE.LTC_MILEAGE_1,
                   LTCFILE.LTC_VALUE_1,
                   LTCFILE.LTC_SELLPR_1,
                   LTCFILE.LTC_MISC_AMT_1,
                   LTCFILE.LTC_SENT_FLAG_1,
                   CAST(LTCFILE.LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),
                   LTCFILE.LTC_DESC_2,
                   LTCFILE.LTC_MAKE_2,
                   LTCFILE.LTC_MODEL_2,
                   LTCFILE.LTC_YEAR_2,
                   LTCFILE.LTC_STYLE_2,
                   LTCFILE.LTC_CLASS_2,
                   LTCFILE.LTC_VIN_2,
                   LTCFILE.LTC_TITLE_NO_2,
                   LTCFILE.LTC_MILEAGE_2,
                   LTCFILE.LTC_VALUE_2,
                   LTCFILE.LTC_SELLPR_2,
                   LTCFILE.LTC_MISC_AMT_2,
                   LTCFILE.LTC_SENT_FLAG_2,
                   CAST(LTCFILE.LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),
                   LTCFILE.LTC_DESC_3,
                   LTCFILE.LTC_MAKE_3,
                   LTCFILE.LTC_MODEL_3,
                   LTCFILE.LTC_YEAR_3,
                   LTCFILE.LTC_STYLE_3,
                   LTCFILE.LTC_CLASS_3,
                   LTCFILE.LTC_VIN_3,
                   LTCFILE.LTC_TITLE_NO_3,
                   LTCFILE.LTC_MILEAGE_3,
                   LTCFILE.LTC_VALUE_3,
                   LTCFILE.LTC_SELLPR_3,
                   LTCFILE.LTC_MISC_AMT_3,
                   LTCFILE.LTC_SENT_FLAG_3,
                   CAST(LTCFILE.LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),
                   LTCFILE.LTC_HHGS_1,
                   LTCFILE.LTC_HHGS_2,
                   LTCFILE.LTC_HHGS_3
            FROM DBO.LTCFILE
            WHERE LTCFILE.LTC_BRNO >= :QLTC1-WBEG-BRNO
              AND LTCFILE.LTC_BRNO <= :QLTC1-WEND-BRNO
              AND LTCFILE.LTC_ACCTNO >= :QLTC1-WBEG-ACCTNO
              AND LTCFILE.LTC_ACCTNO <= :QLTC1-WEND-ACCTNO
              AND LTCFILE.LTC_SEQNO >= :QLTC1-WBEG-SEQNO
              AND LTCFILE.LTC_SEQNO <= :QLTC1-WEND-SEQNO
            ORDER BY LTCFILE.LTC_BRNO,
                     LTCFILE.LTC_ACCTNO,
                     LTCFILE.LTC_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTC1-WBEG-BRNO
                          QLTC1-WEND-BRNO
                          QLTC1-WBEG-ACCTNO
                          QLTC1-WEND-ACCTNO
                          QLTC1-WBEG-SEQNO
                          QLTC1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTC1-FILE.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO. 
           MOVE LTC-PATH-SQL    TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.
           INITIALIZE QLTC-REC.

           MOVE LTC-BRNO   TO QLTC-BRNO.
           MOVE LTC-ACCTNO TO QLTC-ACCTNO.
           MOVE LTC-SEQNO  TO QLTC-SEQNO.

           EXEC SQL
            SELECT LTCFILE.LTC_BRNO,
                   LTCFILE.LTC_ACCTNO,
                   LTCFILE.LTC_SEQNO,
                   CAST(LTCFILE.LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LTCFILE.LTC_DESC_1,
                   LTCFILE.LTC_MAKE_1,
                   LTCFILE.LTC_MODEL_1,
                   LTCFILE.LTC_YEAR_1,
                   LTCFILE.LTC_STYLE_1,
                   LTCFILE.LTC_CLASS_1,
                   LTCFILE.LTC_VIN_1,
                   LTCFILE.LTC_TITLE_NO_1,
                   LTCFILE.LTC_MILEAGE_1,
                   LTCFILE.LTC_VALUE_1,
                   LTCFILE.LTC_SELLPR_1,
                   LTCFILE.LTC_MISC_AMT_1,
                   LTCFILE.LTC_SENT_FLAG_1,
                   CAST(LTCFILE.LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),
                   LTCFILE.LTC_DESC_2,
                   LTCFILE.LTC_MAKE_2,
                   LTCFILE.LTC_MODEL_2,
                   LTCFILE.LTC_YEAR_2,
                   LTCFILE.LTC_STYLE_2,
                   LTCFILE.LTC_CLASS_2,
                   LTCFILE.LTC_VIN_2,
                   LTCFILE.LTC_TITLE_NO_2,
                   LTCFILE.LTC_MILEAGE_2,
                   LTCFILE.LTC_VALUE_2,
                   LTCFILE.LTC_SELLPR_2,
                   LTCFILE.LTC_MISC_AMT_2,
                   LTCFILE.LTC_SENT_FLAG_2,
                   CAST(LTCFILE.LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),
                   LTCFILE.LTC_DESC_3,
                   LTCFILE.LTC_MAKE_3,
                   LTCFILE.LTC_MODEL_3,
                   LTCFILE.LTC_YEAR_3,
                   LTCFILE.LTC_STYLE_3,
                   LTCFILE.LTC_CLASS_3,
                   LTCFILE.LTC_VIN_3,
                   LTCFILE.LTC_TITLE_NO_3,
                   LTCFILE.LTC_MILEAGE_3,
                   LTCFILE.LTC_VALUE_3,
                   LTCFILE.LTC_SELLPR_3,
                   LTCFILE.LTC_MISC_AMT_3,
                   LTCFILE.LTC_SENT_FLAG_3,
                   CAST(LTCFILE.LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),
                   LTCFILE.LTC_HHGS_1,
                   LTCFILE.LTC_HHGS_2,
                   LTCFILE.LTC_HHGS_3
            INTO :QLTC-REC
            FROM DBO.LTCFILE
            WHERE LTCFILE.LTC_BRNO = :QLTC-BRNO
              AND LTCFILE.LTC_ACCTNO = :QLTC-ACCTNO
              AND LTCFILE.LTC_SEQNO = :QLTC-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.

      *-----------------------------------------------------------------
       READ-LTC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-PATH-SQL    TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.
           INITIALIZE QLTC-REC.

           IF ( LTC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTC1_CURSOR INTO :QLTC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LTC1-FILE.

           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           ACCEPT LTC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTC-PATH-SQL    TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.
           PERFORM SET-LTC-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LTCFILE
             (LTCFILE.LTC_BRNO,
              LTCFILE.LTC_ACCTNO,
              LTCFILE.LTC_SEQNO,
              LTCFILE.LTC_LTOUCH_DATE,
              LTCFILE.LTC_DESC_1,
              LTCFILE.LTC_MAKE_1,
              LTCFILE.LTC_MODEL_1,
              LTCFILE.LTC_YEAR_1,
              LTCFILE.LTC_STYLE_1,
              LTCFILE.LTC_CLASS_1,
              LTCFILE.LTC_VIN_1,
              LTCFILE.LTC_TITLE_NO_1,
              LTCFILE.LTC_MILEAGE_1,
              LTCFILE.LTC_VALUE_1,
              LTCFILE.LTC_SELLPR_1,
              LTCFILE.LTC_MISC_AMT_1,
              LTCFILE.LTC_SENT_FLAG_1,
              LTCFILE.LTC_VIN_CHG_DATE_1,
              LTCFILE.LTC_DESC_2,
              LTCFILE.LTC_MAKE_2,
              LTCFILE.LTC_MODEL_2,
              LTCFILE.LTC_YEAR_2,
              LTCFILE.LTC_STYLE_2,
              LTCFILE.LTC_CLASS_2,
              LTCFILE.LTC_VIN_2,
              LTCFILE.LTC_TITLE_NO_2,
              LTCFILE.LTC_MILEAGE_2,
              LTCFILE.LTC_VALUE_2,
              LTCFILE.LTC_SELLPR_2,
              LTCFILE.LTC_MISC_AMT_2,
              LTCFILE.LTC_SENT_FLAG_2,
              LTCFILE.LTC_VIN_CHG_DATE_2,
              LTCFILE.LTC_DESC_3,
              LTCFILE.LTC_MAKE_3,
              LTCFILE.LTC_MODEL_3,
              LTCFILE.LTC_YEAR_3,
              LTCFILE.LTC_STYLE_3,
              LTCFILE.LTC_CLASS_3,
              LTCFILE.LTC_VIN_3,
              LTCFILE.LTC_TITLE_NO_3,
              LTCFILE.LTC_MILEAGE_3,
              LTCFILE.LTC_VALUE_3,
              LTCFILE.LTC_SELLPR_3,
              LTCFILE.LTC_MISC_AMT_3,
              LTCFILE.LTC_SENT_FLAG_3,
              LTCFILE.LTC_VIN_CHG_DATE_3,
              LTCFILE.LTC_HHGS_1,
              LTCFILE.LTC_HHGS_2,
              LTCFILE.LTC_HHGS_3)
            VALUES
             (:QLTC-BRNO,
              :QLTC-ACCTNO,
              :QLTC-SEQNO,
              :QLTC-LTOUCH-DATE,
              :QLTC-DESC-1,
              :QLTC-MAKE-1,
              :QLTC-MODEL-1,
              :QLTC-YEAR-1,
              :QLTC-STYLE-1,
              :QLTC-CLASS-1,
              :QLTC-VIN-1,
              :QLTC-TITLE-NO-1,
              :QLTC-MILEAGE-1,
              :QLTC-VALUE-1,
              :QLTC-SELLPR-1,
              :QLTC-MISC-AMT-1,
              :QLTC-SENT-FLAG-1,
              :QLTC-VIN-CHG-DATE-1,
              :QLTC-DESC-2,
              :QLTC-MAKE-2,
              :QLTC-MODEL-2,
              :QLTC-YEAR-2,
              :QLTC-STYLE-2,
              :QLTC-CLASS-2,
              :QLTC-VIN-2,
              :QLTC-TITLE-NO-2,
              :QLTC-MILEAGE-2,
              :QLTC-VALUE-2,
              :QLTC-SELLPR-2,
              :QLTC-MISC-AMT-2,
              :QLTC-SENT-FLAG-2,
              :QLTC-VIN-CHG-DATE-2,
              :QLTC-DESC-3,
              :QLTC-MAKE-3,
              :QLTC-MODEL-3,
              :QLTC-YEAR-3,
              :QLTC-STYLE-3,
              :QLTC-CLASS-3,
              :QLTC-VIN-3,
              :QLTC-TITLE-NO-3,
              :QLTC-MILEAGE-3,
              :QLTC-VALUE-3,
              :QLTC-SELLPR-3,
              :QLTC-MISC-AMT-3,
              :QLTC-SENT-FLAG-3,
              :QLTC-VIN-CHG-DATE-3,
              :QLTC-HHGS-1,
              :QLTC-HHGS-2,
              :QLTC-HHGS-3)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTC1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LTC1-FILE.

           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           ACCEPT LTC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTC-PATH-SQL    TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.
           PERFORM SET-LTC-FIELDS.

           EXEC SQL
            UPDATE DBO.LTCFILE
             SET
              LTCFILE.LTC_BRNO = :QLTC-BRNO,
              LTCFILE.LTC_ACCTNO = :QLTC-ACCTNO,
              LTCFILE.LTC_SEQNO = :QLTC-SEQNO,
              LTCFILE.LTC_LTOUCH_DATE =
                 :QLTC-LTOUCH-DATE,
              LTCFILE.LTC_DESC_1 = :QLTC-DESC-1,
              LTCFILE.LTC_MAKE_1 = :QLTC-MAKE-1,
              LTCFILE.LTC_MODEL_1 = :QLTC-MODEL-1,
              LTCFILE.LTC_YEAR_1 = :QLTC-YEAR-1,
              LTCFILE.LTC_STYLE_1 = :QLTC-STYLE-1,
              LTCFILE.LTC_CLASS_1 = :QLTC-CLASS-1,
              LTCFILE.LTC_VIN_1 = :QLTC-VIN-1,
              LTCFILE.LTC_TITLE_NO_1 = :QLTC-TITLE-NO-1,
              LTCFILE.LTC_MILEAGE_1 = :QLTC-MILEAGE-1,
              LTCFILE.LTC_VALUE_1 = :QLTC-VALUE-1,
              LTCFILE.LTC_SELLPR_1 = :QLTC-SELLPR-1,
              LTCFILE.LTC_MISC_AMT_1 = :QLTC-MISC-AMT-1,
              LTCFILE.LTC_SENT_FLAG_1 = :QLTC-SENT-FLAG-1,
              LTCFILE.LTC_VIN_CHG_DATE_1 =
                 :QLTC-VIN-CHG-DATE-1,
              LTCFILE.LTC_DESC_2 = :QLTC-DESC-2,
              LTCFILE.LTC_MAKE_2 = :QLTC-MAKE-2,
              LTCFILE.LTC_MODEL_2 = :QLTC-MODEL-2,
              LTCFILE.LTC_YEAR_2 = :QLTC-YEAR-2,
              LTCFILE.LTC_STYLE_2 = :QLTC-STYLE-2,
              LTCFILE.LTC_CLASS_2 = :QLTC-CLASS-2,
              LTCFILE.LTC_VIN_2 = :QLTC-VIN-2,
              LTCFILE.LTC_TITLE_NO_2 = :QLTC-TITLE-NO-2,
              LTCFILE.LTC_MILEAGE_2 = :QLTC-MILEAGE-2,
              LTCFILE.LTC_VALUE_2 = :QLTC-VALUE-2,
              LTCFILE.LTC_SELLPR_2 = :QLTC-SELLPR-2,
              LTCFILE.LTC_MISC_AMT_2 = :QLTC-MISC-AMT-2,
              LTCFILE.LTC_SENT_FLAG_2 = :QLTC-SENT-FLAG-2,
              LTCFILE.LTC_VIN_CHG_DATE_2 =
                 :QLTC-VIN-CHG-DATE-2,
              LTCFILE.LTC_DESC_3 = :QLTC-DESC-3,
              LTCFILE.LTC_MAKE_3 = :QLTC-MAKE-3,
              LTCFILE.LTC_MODEL_3 = :QLTC-MODEL-3,
              LTCFILE.LTC_YEAR_3 = :QLTC-YEAR-3,
              LTCFILE.LTC_STYLE_3 = :QLTC-STYLE-3,
              LTCFILE.LTC_CLASS_3 = :QLTC-CLASS-3,
              LTCFILE.LTC_VIN_3 = :QLTC-VIN-3,
              LTCFILE.LTC_TITLE_NO_3 = :QLTC-TITLE-NO-3,
              LTCFILE.LTC_MILEAGE_3 = :QLTC-MILEAGE-3,
              LTCFILE.LTC_VALUE_3 = :QLTC-VALUE-3,
              LTCFILE.LTC_SELLPR_3 = :QLTC-SELLPR-3,
              LTCFILE.LTC_MISC_AMT_3 = :QLTC-MISC-AMT-3,
              LTCFILE.LTC_SENT_FLAG_3 = :QLTC-SENT-FLAG-3,
              LTCFILE.LTC_VIN_CHG_DATE_3 = :QLTC-VIN-CHG-DATE-3,
              LTCFILE.LTC_HHGS_1 = :QLTC-HHGS-1,
              LTCFILE.LTC_HHGS_2 = :QLTC-HHGS-2,
              LTCFILE.LTC_HHGS_3 = :QLTC-HHGS-3
             WHERE LTCFILE.LTC_BRNO = :QLTC-BRNO
               AND LTCFILE.LTC_ACCTNO = :QLTC-ACCTNO
               AND LTCFILE.LTC_SEQNO = :QLTC-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTC1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LTC1-FILE.

           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           PERFORM DELETE-IT.
           MOVE LTC-PATH-SQL    TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.
           PERFORM SET-LTC-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LTCFILE
             WHERE LTCFILE.LTC_BRNO = :QLTC-BRNO
               AND LTCFILE.LTC_ACCTNO = :QLTC-ACCTNO
               AND LTCFILE.LTC_SEQNO = :QLTC-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LTC1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LTC1-FILE.

           PERFORM CLOSE-IT.

           IF ( LTC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTC1-CURSOR-STAT.

      * NOTE: A VERSION OF THIS EXISTS IN FX/MAKEIX.C
