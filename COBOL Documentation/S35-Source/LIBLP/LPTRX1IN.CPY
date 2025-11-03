      * COPYMEMBER: LIBLP/LPTRX1IN
       LOAD-TRX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRX-ALL-DATA
              MOVE TRX-ALL-PATH             TO TRX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRX-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRX-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRX-ALL-DATA
              MOVE TRX-ALL-PATH             TO TRX-PATH.

      *-----------------------------------------------------------------
       OPEN-TRX1-FILE.
           PERFORM LOAD-TRX1-FILE.
           PERFORM OPEN-IT.
           MOVE TRX-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRX1-FILE.
           PERFORM START-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           MOVE TRX-PATH-SQL    TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.

           IF ( TRX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRX1-FILE.

           IF ( QTRX1-WEND-LINE = ZEROES    ) OR
              ( QTRX1-WEND-LINE NOT NUMERIC )
              MOVE TRX-PATH-OWNBR        TO QTRX1-WBEG-BRNO
              MOVE TRX-PATH-OWNBR        TO QTRX1-WEND-BRNO
              MOVE TX-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRX1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRX1-WEND-DATE
              MOVE TX-CLASS             TO QTRX1-WBEG-CLASS
              MOVE TX-ACCTNO            TO QTRX1-WBEG-ACCTNO
              MOVE TX-LAWCODE           TO QTRX1-WBEG-LAWCODE
              MOVE TX-LINE              TO QTRX1-WBEG-LINE
              MOVE ALL "9"              TO QTRX1-WEND-CLASS
                                           QTRX1-WEND-ACCTNO
                                           QTRX1-WEND-LAWCODE
                                           QTRX1-WEND-LINE.


           EXEC SQL
            DECLARE TRX1_CURSOR CURSOR FOR
            SELECT TRXFILE.TX_BRNO,
                   CAST(TRXFILE.TX_DATE AS VARCHAR(10)),
                   TRXFILE.TX_CLASS,
                   TRXFILE.TX_ACCTNO,
                   TRXFILE.TX_LAWCODE,
                   TRXFILE.TX_LINE,
                   CAST(TRXFILE.TX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRXFILE.TX_SSNO_1,
                   TRXFILE.TX_MAKERCD_1,
                   TRXFILE.TX_SSNO_2,
                   TRXFILE.TX_MAKERCD_2,
                   TRXFILE.TX_SSNO_3,
                   TRXFILE.TX_MAKERCD_3,
                   TRXFILE.TX_SSNO_4,
                   TRXFILE.TX_MAKERCD_4,
                   TRXFILE.TX_PURCD,
                   TRXFILE.TX_SRCD,
                   TRXFILE.TX_COLLCD,
                   TRXFILE.TX_SERVACCT,
                   TRXFILE.TX_LOANTYPE,
                   TRXFILE.TX_SUBCLASS,
                   CAST(TRXFILE.TX_LOANDATE
                                                  AS VARCHAR(10)),
                   TRXFILE.TX_PROCEEDS,
                   TRXFILE.TX_LNAMT,
                   TRXFILE.TX_FINCHG,
                   TRXFILE.TX_INTCHG,
                   TRXFILE.TX_SERCHG,
                   TRXFILE.TX_EXTCHG,
                   TRXFILE.TX_MAINTFEE,
                   TRXFILE.TX_ORGTERM,
                   TRXFILE.TX_INSURED,
                   TRXFILE.TX_INSANTICERN_1,
                   TRXFILE.TX_INSANTICERN_2,
                   TRXFILE.TX_INSANTICERN_3,
                   TRXFILE.TX_INSANTICERN_4,
                   TRXFILE.TX_INSANTICERN_5,
                   TRXFILE.TX_INSANTICERN_6,
                   TRXFILE.TX_INSANTICERN_7,
                   TRXFILE.TX_INSANTICERN_8,
                   TRXFILE.TX_INSANTICERN_9,
                   TRXFILE.TX_INSANTICERN_10,
                   TRXFILE.TX_INSEARNINGS_1,
                   TRXFILE.TX_INSEARNINGS_2,
                   TRXFILE.TX_INSEARNINGS_3,
                   TRXFILE.TX_INSEARNINGS_4,
                   TRXFILE.TX_INSEARNINGS_5,
                   TRXFILE.TX_INSEARNINGS_6,
                   TRXFILE.TX_INSEARNINGS_7,
                   TRXFILE.TX_INSEARNINGS_8,
                   TRXFILE.TX_INSEARNINGS_9,
                   TRXFILE.TX_INSEARNINGS_10,
                   TRXFILE.TX_DLNO,
                   TRXFILE.TX_DLRECOURSE,
                   TRXFILE.TX_DLPARTRCAMT,
                   TRXFILE.TX_PURBAL,
                   TRXFILE.TX_DLRVPRIN_1,
                   TRXFILE.TX_DLRVPRIN_2,
                   TRXFILE.TX_DLRVPRIN_3,
                   TRXFILE.TX_DLRVCHGS_1,
                   TRXFILE.TX_DLRVCHGS_2,
                   TRXFILE.TX_DLRVCHGS_3,
                   TRXFILE.TX_DLPARVHELD,
                   TRXFILE.TX_DLPARVPAID,
                   TRXFILE.TX_DLDISC,
                   TRXFILE.TX_ANTICTERM,
                   TRXFILE.TX_ANTICERN_1,
                   TRXFILE.TX_ANTICERN_2,
                   TRXFILE.TX_ANTICERN_3,
                   TRXFILE.TX_ANTICERN_4,
                   TRXFILE.TX_ANTICERN_5,
                   TRXFILE.TX_ANTICERN_6,
                   TRXFILE.TX_ANTICERN_7,
                   TRXFILE.TX_ANTICERN_8,
                   TRXFILE.TX_ANTICERN_9,
                   TRXFILE.TX_ANTICERN_10,
                   TRXFILE.TX_EARNINGS_1,
                   TRXFILE.TX_EARNINGS_2,
                   TRXFILE.TX_EARNINGS_3,
                   TRXFILE.TX_EARNINGS_4,
                   TRXFILE.TX_EARNINGS_5,
                   TRXFILE.TX_EARNINGS_6,
                   TRXFILE.TX_EARNINGS_7,
                   TRXFILE.TX_EARNINGS_8,
                   TRXFILE.TX_EARNINGS_9,
                   TRXFILE.TX_EARNINGS_10,
                   TRXFILE.TX_PRLNNO,
                   TRXFILE.TX_PRBAL,
                   TRXFILE.TX_TFRBAL,
                   TRXFILE.TX_SPRCLASS,
                   TRXFILE.TX_USERID,
                   CAST(TRXFILE.TX_POSTDATE
                                                  AS VARCHAR(10)),
                   TRXFILE.TX_POSTTIME
            FROM DBO.TRXFILE
            WHERE TRXFILE.TX_BRNO  = :QTRX1-WBEG-BRNO
              AND TRXFILE.TX_DATE >= :QTRX1-WBEG-DATE
              AND TRXFILE.TX_DATE <= :QTRX1-WEND-DATE
              AND TRXFILE.TX_CLASS >= :QTRX1-WBEG-CLASS
              AND TRXFILE.TX_CLASS <= :QTRX1-WEND-CLASS
              AND TRXFILE.TX_ACCTNO >= :QTRX1-WBEG-ACCTNO
              AND TRXFILE.TX_ACCTNO <= :QTRX1-WEND-ACCTNO
              AND TRXFILE.TX_LAWCODE >= :QTRX1-WBEG-LAWCODE
              AND TRXFILE.TX_LAWCODE <= :QTRX1-WEND-LAWCODE
              AND TRXFILE.TX_LINE >= :QTRX1-WBEG-LINE
              AND TRXFILE.TX_LINE <= :QTRX1-WEND-LINE
            ORDER BY TRXFILE.TX_BRNO,
                     TRXFILE.TX_DATE,
                     TRXFILE.TX_CLASS,
                     TRXFILE.TX_ACCTNO,
                     TRXFILE.TX_LAWCODE,
                     TRXFILE.TX_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRX1-WBEG-BRNO
                          QTRX1-WEND-BRNO
                          QTRX1-WBEG-CLASS
                          QTRX1-WEND-CLASS
                          QTRX1-WBEG-ACCTNO
                          QTRX1-WEND-ACCTNO
                          QTRX1-WBEG-LAWCODE
                          QTRX1-WEND-LAWCODE
                          QTRX1-WBEG-LINE
                          QTRX1-WEND-LINE.
           MOVE SPACES TO QTRX1-WBEG-DATE
                          QTRX1-WEND-DATE.


           MOVE STAT---GOOD TO TRX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRX1-FILE.
           PERFORM READ-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO. 
           MOVE TRX-PATH-SQL    TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.
           INITIALIZE QTRX-REC.

           MOVE TX-BRNO             TO QTX-BRNO.
           MOVE TX-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTX-DATE.
           MOVE TX-CLASS            TO QTX-CLASS.
           MOVE TX-ACCTNO           TO QTX-ACCTNO.
           MOVE TX-LAWCODE          TO QTX-LAWCODE.
           MOVE TX-LINE             TO QTX-LINE.


           EXEC SQL
            SELECT TRXFILE.TX_BRNO,
                   CAST(TRXFILE.TX_DATE AS VARCHAR(10)),
                   TRXFILE.TX_CLASS,
                   TRXFILE.TX_ACCTNO,
                   TRXFILE.TX_LAWCODE,
                   TRXFILE.TX_LINE,
                   CAST(TRXFILE.TX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRXFILE.TX_SSNO_1,
                   TRXFILE.TX_MAKERCD_1,
                   TRXFILE.TX_SSNO_2,
                   TRXFILE.TX_MAKERCD_2,
                   TRXFILE.TX_SSNO_3,
                   TRXFILE.TX_MAKERCD_3,
                   TRXFILE.TX_SSNO_4,
                   TRXFILE.TX_MAKERCD_4,
                   TRXFILE.TX_PURCD,
                   TRXFILE.TX_SRCD,
                   TRXFILE.TX_COLLCD,
                   TRXFILE.TX_SERVACCT,
                   TRXFILE.TX_LOANTYPE,
                   TRXFILE.TX_SUBCLASS,
                   CAST(TRXFILE.TX_LOANDATE
                                                  AS VARCHAR(10)),
                   TRXFILE.TX_PROCEEDS,
                   TRXFILE.TX_LNAMT,
                   TRXFILE.TX_FINCHG,
                   TRXFILE.TX_INTCHG,
                   TRXFILE.TX_SERCHG,
                   TRXFILE.TX_EXTCHG,
                   TRXFILE.TX_MAINTFEE,
                   TRXFILE.TX_ORGTERM,
                   TRXFILE.TX_INSURED,
                   TRXFILE.TX_INSANTICERN_1,
                   TRXFILE.TX_INSANTICERN_2,
                   TRXFILE.TX_INSANTICERN_3,
                   TRXFILE.TX_INSANTICERN_4,
                   TRXFILE.TX_INSANTICERN_5,
                   TRXFILE.TX_INSANTICERN_6,
                   TRXFILE.TX_INSANTICERN_7,
                   TRXFILE.TX_INSANTICERN_8,
                   TRXFILE.TX_INSANTICERN_9,
                   TRXFILE.TX_INSANTICERN_10,
                   TRXFILE.TX_INSEARNINGS_1,
                   TRXFILE.TX_INSEARNINGS_2,
                   TRXFILE.TX_INSEARNINGS_3,
                   TRXFILE.TX_INSEARNINGS_4,
                   TRXFILE.TX_INSEARNINGS_5,
                   TRXFILE.TX_INSEARNINGS_6,
                   TRXFILE.TX_INSEARNINGS_7,
                   TRXFILE.TX_INSEARNINGS_8,
                   TRXFILE.TX_INSEARNINGS_9,
                   TRXFILE.TX_INSEARNINGS_10,
                   TRXFILE.TX_DLNO,
                   TRXFILE.TX_DLRECOURSE,
                   TRXFILE.TX_DLPARTRCAMT,
                   TRXFILE.TX_PURBAL,
                   TRXFILE.TX_DLRVPRIN_1,
                   TRXFILE.TX_DLRVPRIN_2,
                   TRXFILE.TX_DLRVPRIN_3,
                   TRXFILE.TX_DLRVCHGS_1,
                   TRXFILE.TX_DLRVCHGS_2,
                   TRXFILE.TX_DLRVCHGS_3,
                   TRXFILE.TX_DLPARVHELD,
                   TRXFILE.TX_DLPARVPAID,
                   TRXFILE.TX_DLDISC,
                   TRXFILE.TX_ANTICTERM,
                   TRXFILE.TX_ANTICERN_1,
                   TRXFILE.TX_ANTICERN_2,
                   TRXFILE.TX_ANTICERN_3,
                   TRXFILE.TX_ANTICERN_4,
                   TRXFILE.TX_ANTICERN_5,
                   TRXFILE.TX_ANTICERN_6,
                   TRXFILE.TX_ANTICERN_7,
                   TRXFILE.TX_ANTICERN_8,
                   TRXFILE.TX_ANTICERN_9,
                   TRXFILE.TX_ANTICERN_10,
                   TRXFILE.TX_EARNINGS_1,
                   TRXFILE.TX_EARNINGS_2,
                   TRXFILE.TX_EARNINGS_3,
                   TRXFILE.TX_EARNINGS_4,
                   TRXFILE.TX_EARNINGS_5,
                   TRXFILE.TX_EARNINGS_6,
                   TRXFILE.TX_EARNINGS_7,
                   TRXFILE.TX_EARNINGS_8,
                   TRXFILE.TX_EARNINGS_9,
                   TRXFILE.TX_EARNINGS_10,
                   TRXFILE.TX_PRLNNO,
                   TRXFILE.TX_PRBAL,
                   TRXFILE.TX_TFRBAL,
                   TRXFILE.TX_SPRCLASS,
                   TRXFILE.TX_USERID,
                   CAST(TRXFILE.TX_POSTDATE
                                                  AS VARCHAR(10)),
                   TRXFILE.TX_POSTTIME
            INTO :QTRX-REC
            FROM DBO.TRXFILE
            WHERE TRXFILE.TX_BRNO = :QTX-BRNO
              AND TRXFILE.TX_DATE = :QTX-DATE
              AND TRXFILE.TX_CLASS = :QTX-CLASS
              AND TRXFILE.TX_ACCTNO = :QTX-ACCTNO
              AND TRXFILE.TX_LAWCODE = :QTX-LAWCODE
              AND TRXFILE.TX_LINE = :QTX-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRX-FIELDS.

      *-----------------------------------------------------------------
       READ-TRX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           MOVE TRX-PATH-SQL    TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.
           INITIALIZE QTRX-REC.

           IF ( TRX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRX1_CURSOR INTO :QTRX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRX-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRX1-FILE.

           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           ACCEPT TX-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRX-PATH-SQL    TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.
           PERFORM SET-TRX-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRXFILE
             (TRXFILE.TX_BRNO,
              TRXFILE.TX_DATE,
              TRXFILE.TX_CLASS,
              TRXFILE.TX_ACCTNO,
              TRXFILE.TX_LAWCODE,
              TRXFILE.TX_LINE,
              TRXFILE.TX_LTOUCH_DATE,
              TRXFILE.TX_SSNO_1,
              TRXFILE.TX_MAKERCD_1,
              TRXFILE.TX_SSNO_2,
              TRXFILE.TX_MAKERCD_2,
              TRXFILE.TX_SSNO_3,
              TRXFILE.TX_MAKERCD_3,
              TRXFILE.TX_SSNO_4,
              TRXFILE.TX_MAKERCD_4,
              TRXFILE.TX_PURCD,
              TRXFILE.TX_SRCD,
              TRXFILE.TX_COLLCD,
              TRXFILE.TX_SERVACCT,
              TRXFILE.TX_LOANTYPE,
              TRXFILE.TX_SUBCLASS,
              TRXFILE.TX_LOANDATE,
              TRXFILE.TX_PROCEEDS,
              TRXFILE.TX_LNAMT,
              TRXFILE.TX_FINCHG,
              TRXFILE.TX_INTCHG,
              TRXFILE.TX_SERCHG,
              TRXFILE.TX_EXTCHG,
              TRXFILE.TX_MAINTFEE,
              TRXFILE.TX_ORGTERM,
              TRXFILE.TX_INSURED,
              TRXFILE.TX_INSANTICERN_1,
              TRXFILE.TX_INSANTICERN_2,
              TRXFILE.TX_INSANTICERN_3,
              TRXFILE.TX_INSANTICERN_4,
              TRXFILE.TX_INSANTICERN_5,
              TRXFILE.TX_INSANTICERN_6,
              TRXFILE.TX_INSANTICERN_7,
              TRXFILE.TX_INSANTICERN_8,
              TRXFILE.TX_INSANTICERN_9,
              TRXFILE.TX_INSANTICERN_10,
              TRXFILE.TX_INSEARNINGS_1,
              TRXFILE.TX_INSEARNINGS_2,
              TRXFILE.TX_INSEARNINGS_3,
              TRXFILE.TX_INSEARNINGS_4,
              TRXFILE.TX_INSEARNINGS_5,
              TRXFILE.TX_INSEARNINGS_6,
              TRXFILE.TX_INSEARNINGS_7,
              TRXFILE.TX_INSEARNINGS_8,
              TRXFILE.TX_INSEARNINGS_9,
              TRXFILE.TX_INSEARNINGS_10,
              TRXFILE.TX_DLNO,
              TRXFILE.TX_DLRECOURSE,
              TRXFILE.TX_DLPARTRCAMT,
              TRXFILE.TX_PURBAL,
              TRXFILE.TX_DLRVPRIN_1,
              TRXFILE.TX_DLRVPRIN_2,
              TRXFILE.TX_DLRVPRIN_3,
              TRXFILE.TX_DLRVCHGS_1,
              TRXFILE.TX_DLRVCHGS_2,
              TRXFILE.TX_DLRVCHGS_3,
              TRXFILE.TX_DLPARVHELD,
              TRXFILE.TX_DLPARVPAID,
              TRXFILE.TX_DLDISC,
              TRXFILE.TX_ANTICTERM,
              TRXFILE.TX_ANTICERN_1,
              TRXFILE.TX_ANTICERN_2,
              TRXFILE.TX_ANTICERN_3,
              TRXFILE.TX_ANTICERN_4,
              TRXFILE.TX_ANTICERN_5,
              TRXFILE.TX_ANTICERN_6,
              TRXFILE.TX_ANTICERN_7,
              TRXFILE.TX_ANTICERN_8,
              TRXFILE.TX_ANTICERN_9,
              TRXFILE.TX_ANTICERN_10,
              TRXFILE.TX_EARNINGS_1,
              TRXFILE.TX_EARNINGS_2,
              TRXFILE.TX_EARNINGS_3,
              TRXFILE.TX_EARNINGS_4,
              TRXFILE.TX_EARNINGS_5,
              TRXFILE.TX_EARNINGS_6,
              TRXFILE.TX_EARNINGS_7,
              TRXFILE.TX_EARNINGS_8,
              TRXFILE.TX_EARNINGS_9,
              TRXFILE.TX_EARNINGS_10,
              TRXFILE.TX_PRLNNO,
              TRXFILE.TX_PRBAL,
              TRXFILE.TX_TFRBAL,
              TRXFILE.TX_SPRCLASS,
              TRXFILE.TX_USERID,
              TRXFILE.TX_POSTDATE,
              TRXFILE.TX_POSTTIME)
            VALUES
             (:QTX-BRNO,
              :QTX-DATE,
              :QTX-CLASS,
              :QTX-ACCTNO,
              :QTX-LAWCODE,
              :QTX-LINE,
              :QTX-LTOUCH-DATE,
              :QTX-SSNO-1,
              :QTX-MAKERCD-1,
              :QTX-SSNO-2,
              :QTX-MAKERCD-2,
              :QTX-SSNO-3,
              :QTX-MAKERCD-3,
              :QTX-SSNO-4,
              :QTX-MAKERCD-4,
              :QTX-PURCD,
              :QTX-SRCD,
              :QTX-COLLCD,
              :QTX-SERVACCT,
              :QTX-LOANTYPE,
              :QTX-SUBCLASS,
              :QTX-LOANDATE,
              :QTX-PROCEEDS,
              :QTX-LNAMT,
              :QTX-FINCHG,
              :QTX-INTCHG,
              :QTX-SERCHG,
              :QTX-EXTCHG,
              :QTX-MAINTFEE,
              :QTX-ORGTERM,
              :QTX-INSURED,
              :QTX-INSANTICERN-1,
              :QTX-INSANTICERN-2,
              :QTX-INSANTICERN-3,
              :QTX-INSANTICERN-4,
              :QTX-INSANTICERN-5,
              :QTX-INSANTICERN-6,
              :QTX-INSANTICERN-7,
              :QTX-INSANTICERN-8,
              :QTX-INSANTICERN-9,
              :QTX-INSANTICERN-10,
              :QTX-INSEARNINGS-1,
              :QTX-INSEARNINGS-2,
              :QTX-INSEARNINGS-3,
              :QTX-INSEARNINGS-4,
              :QTX-INSEARNINGS-5,
              :QTX-INSEARNINGS-6,
              :QTX-INSEARNINGS-7,
              :QTX-INSEARNINGS-8,
              :QTX-INSEARNINGS-9,
              :QTX-INSEARNINGS-10,
              :QTX-DLNO,
              :QTX-DLRECOURSE,
              :QTX-DLPARTRCAMT,
              :QTX-PURBAL,
              :QTX-DLRVPRIN-1,
              :QTX-DLRVPRIN-2,
              :QTX-DLRVPRIN-3,
              :QTX-DLRVCHGS-1,
              :QTX-DLRVCHGS-2,
              :QTX-DLRVCHGS-3,
              :QTX-DLPARVHELD,
              :QTX-DLPARVPAID,
              :QTX-DLDISC,
              :QTX-ANTICTERM,
              :QTX-ANTICERN-1,
              :QTX-ANTICERN-2,
              :QTX-ANTICERN-3,
              :QTX-ANTICERN-4,
              :QTX-ANTICERN-5,
              :QTX-ANTICERN-6,
              :QTX-ANTICERN-7,
              :QTX-ANTICERN-8,
              :QTX-ANTICERN-9,
              :QTX-ANTICERN-10,
              :QTX-EARNINGS-1,
              :QTX-EARNINGS-2,
              :QTX-EARNINGS-3,
              :QTX-EARNINGS-4,
              :QTX-EARNINGS-5,
              :QTX-EARNINGS-6,
              :QTX-EARNINGS-7,
              :QTX-EARNINGS-8,
              :QTX-EARNINGS-9,
              :QTX-EARNINGS-10,
              :QTX-PRLNNO,
              :QTX-PRBAL,
              :QTX-TFRBAL,
              :QTX-SPRCLASS,
              :QTX-USERID,
              :QTX-POSTDATE,
              :QTX-POSTTIME)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRX1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRX1-FILE.

           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           ACCEPT TX-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRX-PATH-SQL    TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.
           PERFORM SET-TRX-FIELDS.

           EXEC SQL
            UPDATE DBO.TRXFILE
             SET
              TRXFILE.TX_BRNO = :QTX-BRNO,
              TRXFILE.TX_DATE =
                 :QTX-DATE,
              TRXFILE.TX_CLASS = :QTX-CLASS,
              TRXFILE.TX_ACCTNO = :QTX-ACCTNO,
              TRXFILE.TX_LAWCODE = :QTX-LAWCODE,
              TRXFILE.TX_LINE = :QTX-LINE,
              TRXFILE.TX_LTOUCH_DATE =
                 :QTX-LTOUCH-DATE,
              TRXFILE.TX_SSNO_1 = :QTX-SSNO-1,
              TRXFILE.TX_MAKERCD_1 = :QTX-MAKERCD-1,
              TRXFILE.TX_SSNO_2 = :QTX-SSNO-2,
              TRXFILE.TX_MAKERCD_2 = :QTX-MAKERCD-2,
              TRXFILE.TX_SSNO_3 = :QTX-SSNO-3,
              TRXFILE.TX_MAKERCD_3 = :QTX-MAKERCD-3,
              TRXFILE.TX_SSNO_4 = :QTX-SSNO-4,
              TRXFILE.TX_MAKERCD_4 = :QTX-MAKERCD-4,
              TRXFILE.TX_PURCD = :QTX-PURCD,
              TRXFILE.TX_SRCD = :QTX-SRCD,
              TRXFILE.TX_COLLCD = :QTX-COLLCD,
              TRXFILE.TX_SERVACCT = :QTX-SERVACCT,
              TRXFILE.TX_LOANTYPE = :QTX-LOANTYPE,
              TRXFILE.TX_SUBCLASS = :QTX-SUBCLASS,
              TRXFILE.TX_LOANDATE =
                 :QTX-LOANDATE,
              TRXFILE.TX_PROCEEDS = :QTX-PROCEEDS,
              TRXFILE.TX_LNAMT = :QTX-LNAMT,
              TRXFILE.TX_FINCHG = :QTX-FINCHG,
              TRXFILE.TX_INTCHG = :QTX-INTCHG,
              TRXFILE.TX_SERCHG = :QTX-SERCHG,
              TRXFILE.TX_EXTCHG = :QTX-EXTCHG,
              TRXFILE.TX_MAINTFEE = :QTX-MAINTFEE,
              TRXFILE.TX_ORGTERM = :QTX-ORGTERM,
              TRXFILE.TX_INSURED = :QTX-INSURED,
              TRXFILE.TX_INSANTICERN_1 = :QTX-INSANTICERN-1,
              TRXFILE.TX_INSANTICERN_2 = :QTX-INSANTICERN-2,
              TRXFILE.TX_INSANTICERN_3 = :QTX-INSANTICERN-3,
              TRXFILE.TX_INSANTICERN_4 = :QTX-INSANTICERN-4,
              TRXFILE.TX_INSANTICERN_5 = :QTX-INSANTICERN-5,
              TRXFILE.TX_INSANTICERN_6 = :QTX-INSANTICERN-6,
              TRXFILE.TX_INSANTICERN_7 = :QTX-INSANTICERN-7,
              TRXFILE.TX_INSANTICERN_8 = :QTX-INSANTICERN-8,
              TRXFILE.TX_INSANTICERN_9 = :QTX-INSANTICERN-9,
              TRXFILE.TX_INSANTICERN_10 = :QTX-INSANTICERN-10,
              TRXFILE.TX_INSEARNINGS_1 = :QTX-INSEARNINGS-1,
              TRXFILE.TX_INSEARNINGS_2 = :QTX-INSEARNINGS-2,
              TRXFILE.TX_INSEARNINGS_3 = :QTX-INSEARNINGS-3,
              TRXFILE.TX_INSEARNINGS_4 = :QTX-INSEARNINGS-4,
              TRXFILE.TX_INSEARNINGS_5 = :QTX-INSEARNINGS-5,
              TRXFILE.TX_INSEARNINGS_6 = :QTX-INSEARNINGS-6,
              TRXFILE.TX_INSEARNINGS_7 = :QTX-INSEARNINGS-7,
              TRXFILE.TX_INSEARNINGS_8 = :QTX-INSEARNINGS-8,
              TRXFILE.TX_INSEARNINGS_9 = :QTX-INSEARNINGS-9,
              TRXFILE.TX_INSEARNINGS_10 = :QTX-INSEARNINGS-10,
              TRXFILE.TX_DLNO = :QTX-DLNO,
              TRXFILE.TX_DLRECOURSE = :QTX-DLRECOURSE,
              TRXFILE.TX_DLPARTRCAMT = :QTX-DLPARTRCAMT,
              TRXFILE.TX_PURBAL = :QTX-PURBAL,
              TRXFILE.TX_DLRVPRIN_1 = :QTX-DLRVPRIN-1,
              TRXFILE.TX_DLRVPRIN_2 = :QTX-DLRVPRIN-2,
              TRXFILE.TX_DLRVPRIN_3 = :QTX-DLRVPRIN-3,
              TRXFILE.TX_DLRVCHGS_1 = :QTX-DLRVCHGS-1,
              TRXFILE.TX_DLRVCHGS_2 = :QTX-DLRVCHGS-2,
              TRXFILE.TX_DLRVCHGS_3 = :QTX-DLRVCHGS-3,
              TRXFILE.TX_DLPARVHELD = :QTX-DLPARVHELD,
              TRXFILE.TX_DLPARVPAID = :QTX-DLPARVPAID,
              TRXFILE.TX_DLDISC = :QTX-DLDISC,
              TRXFILE.TX_ANTICTERM = :QTX-ANTICTERM,
              TRXFILE.TX_ANTICERN_1 = :QTX-ANTICERN-1,
              TRXFILE.TX_ANTICERN_2 = :QTX-ANTICERN-2,
              TRXFILE.TX_ANTICERN_3 = :QTX-ANTICERN-3,
              TRXFILE.TX_ANTICERN_4 = :QTX-ANTICERN-4,
              TRXFILE.TX_ANTICERN_5 = :QTX-ANTICERN-5,
              TRXFILE.TX_ANTICERN_6 = :QTX-ANTICERN-6,
              TRXFILE.TX_ANTICERN_7 = :QTX-ANTICERN-7,
              TRXFILE.TX_ANTICERN_8 = :QTX-ANTICERN-8,
              TRXFILE.TX_ANTICERN_9 = :QTX-ANTICERN-9,
              TRXFILE.TX_ANTICERN_10 = :QTX-ANTICERN-10,
              TRXFILE.TX_EARNINGS_1 = :QTX-EARNINGS-1,
              TRXFILE.TX_EARNINGS_2 = :QTX-EARNINGS-2,
              TRXFILE.TX_EARNINGS_3 = :QTX-EARNINGS-3,
              TRXFILE.TX_EARNINGS_4 = :QTX-EARNINGS-4,
              TRXFILE.TX_EARNINGS_5 = :QTX-EARNINGS-5,
              TRXFILE.TX_EARNINGS_6 = :QTX-EARNINGS-6,
              TRXFILE.TX_EARNINGS_7 = :QTX-EARNINGS-7,
              TRXFILE.TX_EARNINGS_8 = :QTX-EARNINGS-8,
              TRXFILE.TX_EARNINGS_9 = :QTX-EARNINGS-9,
              TRXFILE.TX_EARNINGS_10 = :QTX-EARNINGS-10,
              TRXFILE.TX_PRLNNO = :QTX-PRLNNO,
              TRXFILE.TX_PRBAL = :QTX-PRBAL,
              TRXFILE.TX_TFRBAL = :QTX-TFRBAL,
              TRXFILE.TX_SPRCLASS = :QTX-SPRCLASS,
              TRXFILE.TX_USERID = :QTX-USERID,
              TRXFILE.TX_POSTDATE = :QTX-POSTDATE,
              TRXFILE.TX_POSTTIME = :QTX-POSTTIME
            WHERE TRXFILE.TX_BRNO = :QTX-BRNO
              AND TRXFILE.TX_DATE = :QTX-DATE
              AND TRXFILE.TX_CLASS = :QTX-CLASS
              AND TRXFILE.TX_ACCTNO = :QTX-ACCTNO
              AND TRXFILE.TX_LAWCODE = :QTX-LAWCODE
              AND TRXFILE.TX_LINE = :QTX-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRX1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRX1-FILE.

           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           PERFORM DELETE-IT.
           MOVE TRX-PATH-SQL    TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.
           PERFORM SET-TRX-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRXFILE
            WHERE TRXFILE.TX_BRNO = :QTX-BRNO
              AND TRXFILE.TX_DATE = :QTX-DATE
              AND TRXFILE.TX_CLASS = :QTX-CLASS
              AND TRXFILE.TX_ACCTNO = :QTX-ACCTNO
              AND TRXFILE.TX_LAWCODE = :QTX-LAWCODE
              AND TRXFILE.TX_LINE = :QTX-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRX1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRX1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRX1-CURSOR-STAT.

