      * COPYMEMBER: LIBLP/LPLTL1IN LIENHOLDER TRAILER
      * REV:
      *
       LOAD-LTL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTL-ALL-DATA
              MOVE LTL-ALL-PATH             TO LTL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTL-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTL-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTL-ALL-DATA
              MOVE LTL-ALL-PATH             TO LTL-PATH.

      *-----------------------------------------------------------------
       OPEN-LTL1-FILE.
           PERFORM LOAD-LTL1-FILE.
           PERFORM OPEN-IT.
           MOVE LTL-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTL1-FILE.
           PERFORM START-IT.
           MOVE LTL-PATH-OWNBR  TO LTL-BRNO.
           MOVE LTL-PATH-SQL    TO E-FILE.
           MOVE LTL1-KEY        TO E-KEYX.

           IF ( LTL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTL1-FILE.

           IF ( QLTL1-WEND-BRNO = ZEROES    ) OR
              ( QLTL1-WEND-BRNO NOT NUMERIC )
              MOVE LTL-PATH-OWNBR        TO QLTL1-WBEG-BRNO
              MOVE LTL-PATH-OWNBR        TO QLTL1-WEND-BRNO
              MOVE LTL-ACCTNO            TO QLTL1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTL1-WEND-ACCTNO
              MOVE LTL-SEQNO             TO QLTL1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTL1-WEND-SEQNO.

           EXEC SQL
            DECLARE LTL1_CURSOR CURSOR FOR
            SELECT LTLFILE.LTL_BRNO,
                   LTLFILE.LTL_ACCTNO,
                   LTLFILE.LTL_SEQNO,
                   CAST(LTLFILE.LTL_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LTLFILE.LTL_LIEN_TYPE_1,
                   LTLFILE.LTL_LIEN_NAME_1,
                   LTLFILE.LTL_LIEN_ADR_1,
                   LTLFILE.LTL_LIEN_CITY_1,
                   LTLFILE.LTL_LIEN_STATE_1,
                   LTLFILE.LTL_LIEN_ZIP_1,
                   CAST(LTLFILE.LTL_LIEN_DATE_1
                                                  AS VARCHAR(10)),
                   LTLFILE.LTL_LIEN_TYPE_2,
                   LTLFILE.LTL_LIEN_NAME_2,
                   LTLFILE.LTL_LIEN_ADR_2,
                   LTLFILE.LTL_LIEN_CITY_2,
                   LTLFILE.LTL_LIEN_STATE_2,
                   LTLFILE.LTL_LIEN_ZIP_2,
                   CAST(LTLFILE.LTL_LIEN_DATE_2
                                                  AS VARCHAR(10)),
                   LTLFILE.LTL_LIEN_TYPE_3,
                   LTLFILE.LTL_LIEN_NAME_3,
                   LTLFILE.LTL_LIEN_ADR_3,
                   LTLFILE.LTL_LIEN_CITY_3,
                   LTLFILE.LTL_LIEN_STATE_3,
                   LTLFILE.LTL_LIEN_ZIP_3,
                   CAST(LTLFILE.LTL_LIEN_DATE_3
                                                  AS VARCHAR(10)),
                   CAST(LTLFILE.LTL_TITLE_RCV_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTLFILE.LTL_TITLE_LIEN_DATE
                                                  AS VARCHAR(10)),
                   LTLFILE.LTL_TITLE_NO,
                   LTLFILE.LTL_LIEN_LETTER_SENT,
                   LTLFILE.LTL_LIEN_COMMENT1,
                   LTLFILE.LTL_LIEN_COMMENT2,
                   LTLFILE.LTL_LIEN_TITLE_STATE,
                   LTLFILE.LTL_TITLE_STATUS_CODE,
                   LTLFILE.LTL_TITLE_SEND_FLAG,
                   LTLFILE.LTL_TITLE_NAME,
                   LTLFILE.LTL_TITLE_ADR,
                   LTLFILE.LTL_TITLE_CITY,
                   LTLFILE.LTL_TITLE_STATE,
                   LTLFILE.LTL_TITLE_ZIP,
                   CAST(LTLFILE.LTL_TITLE_DATE
                                                  AS VARCHAR(10)),
                   LTLFILE.LTL_TITLE_SENT_BY,
                   LTLFILE.LTL_SEND_ASAP,
                   LTLFILE.LTL_TITLE_SEND_FMNO,
                   LTLFILE.LTL_TITLE_ADR2,
                   LTLFILE.LTL_TITLE_COMMENT1,
                   LTLFILE.LTL_TITLE_COMMENT2,
                   LTLFILE.LTL_RELEASE_TYPE,
                   LTLFILE.LTL_CARRIER_TYPE,
                   LTLFILE.LTL_CARRIER_ACCTNO
            FROM DBO.LTLFILE
            WHERE LTLFILE.LTL_BRNO  = :QLTL1-WBEG-BRNO
              AND LTLFILE.LTL_ACCTNO >= :QLTL1-WBEG-ACCTNO
              AND LTLFILE.LTL_ACCTNO <= :QLTL1-WEND-ACCTNO
              AND LTLFILE.LTL_SEQNO >= :QLTL1-WBEG-SEQNO
              AND LTLFILE.LTL_SEQNO <= :QLTL1-WEND-SEQNO
            ORDER BY LTLFILE.LTL_BRNO,
                     LTLFILE.LTL_ACCTNO,
                     LTLFILE.LTL_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTL1-WBEG-BRNO 
                          QLTL1-WEND-BRNO
                          QLTL1-WBEG-ACCTNO
                          QLTL1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LTL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTL1-FILE.
           PERFORM READ-IT.
           MOVE LTL-PATH-OWNBR  TO LTL-BRNO. 
           MOVE LTL-PATH-SQL    TO E-FILE.
           MOVE LTL1-KEY        TO E-KEYX.
           INITIALIZE QLTL-REC.

           MOVE LTL-BRNO   TO QLTL-BRNO.
           MOVE LTL-ACCTNO TO QLTL-ACCTNO.
           MOVE LTL-SEQNO  TO QLTL-SEQNO.

           EXEC SQL
            SELECT LTLFILE.LTL_BRNO,
                   LTLFILE.LTL_ACCTNO,
                   LTLFILE.LTL_SEQNO,
                   CAST(LTLFILE.LTL_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LTLFILE.LTL_LIEN_TYPE_1,
                   LTLFILE.LTL_LIEN_NAME_1,
                   LTLFILE.LTL_LIEN_ADR_1,
                   LTLFILE.LTL_LIEN_CITY_1,
                   LTLFILE.LTL_LIEN_STATE_1,
                   LTLFILE.LTL_LIEN_ZIP_1,
                   CAST(LTLFILE.LTL_LIEN_DATE_1
                                                  AS VARCHAR(10)),
                   LTLFILE.LTL_LIEN_TYPE_2,
                   LTLFILE.LTL_LIEN_NAME_2,
                   LTLFILE.LTL_LIEN_ADR_2,
                   LTLFILE.LTL_LIEN_CITY_2,
                   LTLFILE.LTL_LIEN_STATE_2,
                   LTLFILE.LTL_LIEN_ZIP_2,
                   CAST(LTLFILE.LTL_LIEN_DATE_2
                                                  AS VARCHAR(10)),
                   LTLFILE.LTL_LIEN_TYPE_3,
                   LTLFILE.LTL_LIEN_NAME_3,
                   LTLFILE.LTL_LIEN_ADR_3,
                   LTLFILE.LTL_LIEN_CITY_3,
                   LTLFILE.LTL_LIEN_STATE_3,
                   LTLFILE.LTL_LIEN_ZIP_3,
                   CAST(LTLFILE.LTL_LIEN_DATE_3
                                                  AS VARCHAR(10)),
                   CAST(LTLFILE.LTL_TITLE_RCV_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTLFILE.LTL_TITLE_LIEN_DATE
                                                  AS VARCHAR(10)),
                   LTLFILE.LTL_TITLE_NO,
                   LTLFILE.LTL_LIEN_LETTER_SENT,
                   LTLFILE.LTL_LIEN_COMMENT1,
                   LTLFILE.LTL_LIEN_COMMENT2,
                   LTLFILE.LTL_LIEN_TITLE_STATE,
                   LTLFILE.LTL_TITLE_STATUS_CODE,
                   LTLFILE.LTL_TITLE_SEND_FLAG,
                   LTLFILE.LTL_TITLE_NAME,
                   LTLFILE.LTL_TITLE_ADR,
                   LTLFILE.LTL_TITLE_CITY,
                   LTLFILE.LTL_TITLE_STATE,
                   LTLFILE.LTL_TITLE_ZIP,
                   CAST(LTLFILE.LTL_TITLE_DATE
                                                  AS VARCHAR(10)),
                   LTLFILE.LTL_TITLE_SENT_BY,
                   LTLFILE.LTL_SEND_ASAP,
                   LTLFILE.LTL_TITLE_SEND_FMNO,
                   LTLFILE.LTL_TITLE_ADR2,
                   LTLFILE.LTL_TITLE_COMMENT1,
                   LTLFILE.LTL_TITLE_COMMENT2,
                   LTLFILE.LTL_RELEASE_TYPE,
                   LTLFILE.LTL_CARRIER_TYPE,
                   LTLFILE.LTL_CARRIER_ACCTNO
            INTO :QLTL-REC
            FROM DBO.LTLFILE
            WHERE LTLFILE.LTL_BRNO = :QLTL-BRNO
              AND LTLFILE.LTL_ACCTNO = :QLTL-ACCTNO
              AND LTLFILE.LTL_SEQNO = :QLTL-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTL-FIELDS.

      *-----------------------------------------------------------------
       READ-LTL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTL-PATH-OWNBR  TO LTL-BRNO.
           MOVE LTL-PATH-SQL    TO E-FILE.
           MOVE LTL1-KEY        TO E-KEYX.
           INITIALIZE QLTL-REC.

           IF ( LTL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTL1_CURSOR INTO :QLTL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTL-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LTL1-FILE.

           MOVE LTL-PATH-OWNBR  TO LTL-BRNO.
           ACCEPT LTL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTL-PATH-SQL    TO E-FILE.
           MOVE LTL1-KEY        TO E-KEYX.
           PERFORM SET-LTL-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LTLFILE
             (LTLFILE.LTL_BRNO,
              LTLFILE.LTL_ACCTNO,
              LTLFILE.LTL_SEQNO,
              LTLFILE.LTL_LTOUCH_DATE,
              LTLFILE.LTL_LIEN_TYPE_1,
              LTLFILE.LTL_LIEN_NAME_1,
              LTLFILE.LTL_LIEN_ADR_1,
              LTLFILE.LTL_LIEN_CITY_1,
              LTLFILE.LTL_LIEN_STATE_1,
              LTLFILE.LTL_LIEN_ZIP_1,
              LTLFILE.LTL_LIEN_DATE_1,
              LTLFILE.LTL_LIEN_TYPE_2,
              LTLFILE.LTL_LIEN_NAME_2,
              LTLFILE.LTL_LIEN_ADR_2,
              LTLFILE.LTL_LIEN_CITY_2,
              LTLFILE.LTL_LIEN_STATE_2,
              LTLFILE.LTL_LIEN_ZIP_2,
              LTLFILE.LTL_LIEN_DATE_2,
              LTLFILE.LTL_LIEN_TYPE_3,
              LTLFILE.LTL_LIEN_NAME_3,
              LTLFILE.LTL_LIEN_ADR_3,
              LTLFILE.LTL_LIEN_CITY_3,
              LTLFILE.LTL_LIEN_STATE_3,
              LTLFILE.LTL_LIEN_ZIP_3,
              LTLFILE.LTL_LIEN_DATE_3,
              LTLFILE.LTL_TITLE_RCV_DATE,
              LTLFILE.LTL_TITLE_LIEN_DATE,
              LTLFILE.LTL_TITLE_NO,
              LTLFILE.LTL_LIEN_LETTER_SENT,
              LTLFILE.LTL_LIEN_COMMENT1,
              LTLFILE.LTL_LIEN_COMMENT2,
              LTLFILE.LTL_LIEN_TITLE_STATE,
              LTLFILE.LTL_TITLE_STATUS_CODE,
              LTLFILE.LTL_TITLE_SEND_FLAG,
              LTLFILE.LTL_TITLE_NAME,
              LTLFILE.LTL_TITLE_ADR,
              LTLFILE.LTL_TITLE_CITY,
              LTLFILE.LTL_TITLE_STATE,
              LTLFILE.LTL_TITLE_ZIP,
              LTLFILE.LTL_TITLE_DATE,
              LTLFILE.LTL_TITLE_SENT_BY,
              LTLFILE.LTL_SEND_ASAP,
              LTLFILE.LTL_TITLE_SEND_FMNO,
              LTLFILE.LTL_TITLE_ADR2,
              LTLFILE.LTL_TITLE_COMMENT1,
              LTLFILE.LTL_TITLE_COMMENT2,
              LTLFILE.LTL_RELEASE_TYPE,
              LTLFILE.LTL_CARRIER_TYPE,
              LTLFILE.LTL_CARRIER_ACCTNO)
            VALUES
             (:QLTL-BRNO,
              :QLTL-ACCTNO,
              :QLTL-SEQNO,
              :QLTL-LTOUCH-DATE,
              :QLTL-LIEN-TYPE-1,
              :QLTL-LIEN-NAME-1,
              :QLTL-LIEN-ADR-1,
              :QLTL-LIEN-CITY-1,
              :QLTL-LIEN-STATE-1,
              :QLTL-LIEN-ZIP-1,
              :QLTL-LIEN-DATE-1,
              :QLTL-LIEN-TYPE-2,
              :QLTL-LIEN-NAME-2,
              :QLTL-LIEN-ADR-2,
              :QLTL-LIEN-CITY-2,
              :QLTL-LIEN-STATE-2,
              :QLTL-LIEN-ZIP-2,
              :QLTL-LIEN-DATE-2,
              :QLTL-LIEN-TYPE-3,
              :QLTL-LIEN-NAME-3,
              :QLTL-LIEN-ADR-3,
              :QLTL-LIEN-CITY-3,
              :QLTL-LIEN-STATE-3,
              :QLTL-LIEN-ZIP-3,
              :QLTL-LIEN-DATE-3,
              :QLTL-TITLE-RCV-DATE,
              :QLTL-TITLE-LIEN-DATE,
              :QLTL-TITLE-NO,
              :QLTL-LIEN-LETTER-SENT,
              :QLTL-LIEN-COMMENT1,
              :QLTL-LIEN-COMMENT2,
              :QLTL-LIEN-TITLE-STATE,
              :QLTL-TITLE-STATUS-CODE,
              :QLTL-TITLE-SEND-FLAG,
              :QLTL-TITLE-NAME,
              :QLTL-TITLE-ADR,
              :QLTL-TITLE-CITY,
              :QLTL-TITLE-STATE,
              :QLTL-TITLE-ZIP,
              :QLTL-TITLE-DATE,
              :QLTL-TITLE-SENT-BY,
              :QLTL-SEND-ASAP,
              :QLTL-TITLE-SEND-FMNO,
              :QLTL-TITLE-ADR2,
              :QLTL-TITLE-COMMENT1,
              :QLTL-TITLE-COMMENT2,
              :QLTL-RELEASE-TYPE,
              :QLTL-CARRIER-TYPE,
              :QLTL-CARRIER-ACCTNO)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTL1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LTL1-FILE.

           MOVE LTL-PATH-OWNBR  TO LTL-BRNO.
           ACCEPT LTL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTL-PATH-SQL    TO E-FILE.
           MOVE LTL1-KEY        TO E-KEYX.
           PERFORM SET-LTL-FIELDS.

           EXEC SQL
            UPDATE DBO.LTLFILE
             SET
              LTLFILE.LTL_BRNO = :QLTL-BRNO,
              LTLFILE.LTL_ACCTNO = :QLTL-ACCTNO,
              LTLFILE.LTL_SEQNO = :QLTL-SEQNO,
              LTLFILE.LTL_LTOUCH_DATE =
                 :QLTL-LTOUCH-DATE,
              LTLFILE.LTL_LIEN_TYPE_1 = :QLTL-LIEN-TYPE-1,
              LTLFILE.LTL_LIEN_NAME_1 = :QLTL-LIEN-NAME-1,
              LTLFILE.LTL_LIEN_ADR_1 = :QLTL-LIEN-ADR-1,
              LTLFILE.LTL_LIEN_CITY_1 = :QLTL-LIEN-CITY-1,
              LTLFILE.LTL_LIEN_STATE_1 = :QLTL-LIEN-STATE-1,
              LTLFILE.LTL_LIEN_ZIP_1 = :QLTL-LIEN-ZIP-1,
              LTLFILE.LTL_LIEN_DATE_1 =
                 :QLTL-LIEN-DATE-1,
              LTLFILE.LTL_LIEN_TYPE_2 = :QLTL-LIEN-TYPE-2,
              LTLFILE.LTL_LIEN_NAME_2 = :QLTL-LIEN-NAME-2,
              LTLFILE.LTL_LIEN_ADR_2 = :QLTL-LIEN-ADR-2,
              LTLFILE.LTL_LIEN_CITY_2 = :QLTL-LIEN-CITY-2,
              LTLFILE.LTL_LIEN_STATE_2 = :QLTL-LIEN-STATE-2,
              LTLFILE.LTL_LIEN_ZIP_2 = :QLTL-LIEN-ZIP-2,
              LTLFILE.LTL_LIEN_DATE_2 =
                 :QLTL-LIEN-DATE-2,
              LTLFILE.LTL_LIEN_TYPE_3 = :QLTL-LIEN-TYPE-3,
              LTLFILE.LTL_LIEN_NAME_3 = :QLTL-LIEN-NAME-3,
              LTLFILE.LTL_LIEN_ADR_3 = :QLTL-LIEN-ADR-3,
              LTLFILE.LTL_LIEN_CITY_3 = :QLTL-LIEN-CITY-3,
              LTLFILE.LTL_LIEN_STATE_3 = :QLTL-LIEN-STATE-3,
              LTLFILE.LTL_LIEN_ZIP_3 = :QLTL-LIEN-ZIP-3,
              LTLFILE.LTL_LIEN_DATE_3 =
                 :QLTL-LIEN-DATE-3,
              LTLFILE.LTL_TITLE_RCV_DATE =
                 :QLTL-TITLE-RCV-DATE,
              LTLFILE.LTL_TITLE_LIEN_DATE =
                 :QLTL-TITLE-LIEN-DATE,
              LTLFILE.LTL_TITLE_NO = :QLTL-TITLE-NO,
              LTLFILE.LTL_LIEN_LETTER_SENT =
                   :QLTL-LIEN-LETTER-SENT,
              LTLFILE.LTL_LIEN_COMMENT1 = :QLTL-LIEN-COMMENT1,
              LTLFILE.LTL_LIEN_COMMENT2 = :QLTL-LIEN-COMMENT2,
              LTLFILE.LTL_LIEN_TITLE_STATE =
                   :QLTL-LIEN-TITLE-STATE,
              LTLFILE.LTL_TITLE_STATUS_CODE =
                   :QLTL-TITLE-STATUS-CODE,
              LTLFILE.LTL_TITLE_SEND_FLAG = :QLTL-TITLE-SEND-FLAG,
              LTLFILE.LTL_TITLE_NAME = :QLTL-TITLE-NAME,
              LTLFILE.LTL_TITLE_ADR = :QLTL-TITLE-ADR,
              LTLFILE.LTL_TITLE_CITY = :QLTL-TITLE-CITY,
              LTLFILE.LTL_TITLE_STATE = :QLTL-TITLE-STATE,
              LTLFILE.LTL_TITLE_ZIP = :QLTL-TITLE-ZIP,
              LTLFILE.LTL_TITLE_DATE = :QLTL-TITLE-DATE,
              LTLFILE.LTL_TITLE_SENT_BY = :QLTL-TITLE-SENT-BY,
              LTLFILE.LTL_SEND_ASAP = :QLTL-SEND-ASAP,
              LTLFILE.LTL_TITLE_SEND_FMNO = :QLTL-TITLE-SEND-FMNO,
              LTLFILE.LTL_TITLE_ADR2 = :QLTL-TITLE-ADR2,
              LTLFILE.LTL_TITLE_COMMENT1 = :QLTL-TITLE-COMMENT1,
              LTLFILE.LTL_TITLE_COMMENT2 = :QLTL-TITLE-COMMENT2,
              LTLFILE.LTL_RELEASE_TYPE = :QLTL-RELEASE-TYPE,
              LTLFILE.LTL_CARRIER_TYPE = :QLTL-CARRIER-TYPE,
              LTLFILE.LTL_CARRIER_ACCTNO = :QLTL-CARRIER-ACCTNO
             WHERE LTLFILE.LTL_BRNO = :QLTL-BRNO
               AND LTLFILE.LTL_ACCTNO = :QLTL-ACCTNO
               AND LTLFILE.LTL_SEQNO = :QLTL-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTL1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LTL1-FILE.

           MOVE LTL-PATH-OWNBR  TO LTL-BRNO.
           PERFORM DELETE-IT.
           MOVE LTL-PATH-SQL    TO E-FILE.
           MOVE LTL1-KEY        TO E-KEYX.
           PERFORM SET-LTL-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LTLFILE
             WHERE LTLFILE.LTL_BRNO = :QLTL-BRNO
               AND LTLFILE.LTL_ACCTNO = :QLTL-ACCTNO
               AND LTLFILE.LTL_SEQNO = :QLTL-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LTL1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LTL1-FILE.

           PERFORM CLOSE-IT.

           IF ( LTL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTL1-CURSOR-STAT.

