      * COPYMEMBER: LIBLP/LPTRX1RN
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
       START-TRX1-FILE-SCAN.
           PERFORM START-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           MOVE TRX-PATH-SQL    TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.

           IF ( TRX1-CURSOR-SC-STAT-GOOD )
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
                                           QTRX1-WEND-LINE
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }        

              MOVE TX-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRX1-WSBEG-DATE  

              IF ( TX-CLASS = 999 )
                 MOVE TX-CLASS          TO QTRX1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRX1-WSBEG-CLASS = TX-CLASS + 1
              END-IF

              IF ( TX-ACCTNO = 99999999 )
                 MOVE TX-ACCTNO         TO QTRX1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRX1-WSBEG-ACCTNO = TX-ACCTNO + 1 
              END-IF

              IF ( TX-LAWCODE = 99 )
                 MOVE TX-LAWCODE        TO QTRX1-WSBEG-LAWCODE
              ELSE
                 COMPUTE QTRX1-WSBEG-LAWCODE = TX-LAWCODE + 1
              END-IF.

           EXEC SQL
            DECLARE TRX1_CURSOR_SC CURSOR FOR
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
            WHERE (  TRXFILE.TX_BRNO     = :QTRX1-WBEG-BRNO      )
              AND (( TRXFILE.TX_DATE    >= :QTRX1-WBEG-DATE
              AND    TRXFILE.TX_CLASS   >= :QTRX1-WBEG-CLASS
              AND    TRXFILE.TX_ACCTNO  >= :QTRX1-WBEG-ACCTNO
              AND    TRXFILE.TX_LAWCODE >= :QTRX1-WBEG-LAWCODE
              AND    TRXFILE.TX_LINE    >= :QTRX1-WBEG-LINE     )
               OR  ( TRXFILE.TX_DATE    >= :QTRX1-WBEG-DATE
              AND    TRXFILE.TX_CLASS   >= :QTRX1-WBEG-CLASS
              AND    TRXFILE.TX_ACCTNO  >= :QTRX1-WBEG-ACCTNO
              AND    TRXFILE.TX_LAWCODE >= :QTRX1-WSBEG-LAWCODE
              AND    TRXFILE.TX_LINE    >= 0                    )
               OR  ( TRXFILE.TX_DATE    >= :QTRX1-WBEG-DATE
              AND    TRXFILE.TX_CLASS   >= :QTRX1-WBEG-CLASS
              AND    TRXFILE.TX_ACCTNO  >= :QTRX1-WSBEG-ACCTNO
              AND    TRXFILE.TX_LAWCODE >= 0
              AND    TRXFILE.TX_LINE    >= 0                    )
               OR  ( TRXFILE.TX_DATE    >= :QTRX1-WBEG-DATE
              AND    TRXFILE.TX_CLASS   >= :QTRX1-WSBEG-CLASS
              AND    TRXFILE.TX_ACCTNO  >= 0
              AND    TRXFILE.TX_LAWCODE >= 0 
              AND    TRXFILE.TX_LINE    >= 0                    )
               OR  ( TRXFILE.TX_DATE    >= :QTRX1-WSBEG-DATE
              AND    TRXFILE.TX_CLASS   >= 0
              AND    TRXFILE.TX_ACCTNO  >= 0
              AND    TRXFILE.TX_LAWCODE >= 0 
              AND    TRXFILE.TX_LINE    >= 0                    ))
              AND (  TRXFILE.TX_DATE    <= :QTRX1-WEND-DATE
              AND    TRXFILE.TX_CLASS   <= :QTRX1-WEND-CLASS
              AND    TRXFILE.TX_ACCTNO  <= :QTRX1-WEND-ACCTNO
              AND    TRXFILE.TX_LAWCODE <= :QTRX1-WEND-LAWCODE
              AND    TRXFILE.TX_LINE    <= :QTRX1-WEND-LINE      )
            ORDER BY TRXFILE.TX_BRNO,
                     TRXFILE.TX_DATE,
                     TRXFILE.TX_CLASS,
                     TRXFILE.TX_ACCTNO,
                     TRXFILE.TX_LAWCODE,
                     TRXFILE.TX_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRX1_CURSOR_SC
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

           MOVE STAT---GOOD TO TRX1-CURSOR-SC-STAT.


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
              IF ( TRX1-CURSOR-SC-STAT-GOOD )
                 EXEC SQL
                   FETCH TRX1_CURSOR_SC INTO :QTRX-REC
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
       CLOSE-TRX1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRX1-CURSOR-STAT.

           IF ( TRX1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE TRX1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO TRX1-CURSOR-SC-STAT.

