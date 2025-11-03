      * COPYMEMBER: LIBLP/LPTRJ1RN
       LOAD-TRJ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRJ-ALL-DATA
              MOVE TRJ-ALL-PATH             TO TRJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRJ-ALL-DATA
              MOVE TRJ-ALL-PATH             TO TRJ-PATH.

      *-----------------------------------------------------------------
       OPEN-TRJ1-FILE.
           PERFORM LOAD-TRJ1-FILE.
           PERFORM OPEN-IT.
           MOVE TRJ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRJ1-FILE.
           PERFORM START-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           MOVE TRJ-PATH-SQL    TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.

           IF ( TRJ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRJ1-FILE.

           IF ( QTRJ1-WEND-LINE = ZEROES    ) OR
              ( QTRJ1-WEND-LINE NOT NUMERIC )
              MOVE TRJ-PATH-OWNBR        TO QTRJ1-WBEG-BRNO
              MOVE TRJ-PATH-OWNBR        TO QTRJ1-WEND-BRNO
              MOVE TJ-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRJ1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRJ1-WEND-DATE
              MOVE TJ-CLASS             TO QTRJ1-WBEG-CLASS
              MOVE TJ-ACCTNO            TO QTRJ1-WBEG-ACCTNO
              MOVE TJ-LAWCODE           TO QTRJ1-WBEG-LAWCODE
              MOVE TJ-LINE              TO QTRJ1-WBEG-LINE
              MOVE ALL "9"              TO QTRJ1-WEND-CLASS
                                           QTRJ1-WEND-ACCTNO
                                           QTRJ1-WEND-LAWCODE
                                           QTRJ1-WEND-LINE.


           EXEC SQL
            DECLARE TRJ1_CURSOR CURSOR FOR
            SELECT TRJFILE.TJ_BRNO,
                   CAST(TRJFILE.TJ_DATE AS VARCHAR(10)),
                   TRJFILE.TJ_CLASS,
                   TRJFILE.TJ_ACCTNO,
                   TRJFILE.TJ_LAWCODE,
                   TRJFILE.TJ_LINE,
                   CAST(TRJFILE.TJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRJFILE.TJ_SSNO_1,
                   TRJFILE.TJ_MAKERCD_1,
                   TRJFILE.TJ_SSNO_2,
                   TRJFILE.TJ_MAKERCD_2,
                   TRJFILE.TJ_SSNO_3,
                   TRJFILE.TJ_MAKERCD_3,
                   TRJFILE.TJ_SSNO_4,
                   TRJFILE.TJ_MAKERCD_4,
                   TRJFILE.TJ_REF_FRLC,
                   TRJFILE.TJ_REF_X,
                   TRJFILE.TJ_REF_TOLC,
                   TRJFILE.TJ_FRLC,
                   TRJFILE.TJ_TOLC,
                   TRJFILE.TJ_PURCD,
                   TRJFILE.TJ_SRCD,
                   TRJFILE.TJ_COLLCD,
                   TRJFILE.TJ_SERVACCT,
                   TRJFILE.TJ_LOANTYPE,
                   TRJFILE.TJ_SUBCLASS,
                   CAST(TRJFILE.TJ_LOANDATE
                                                  AS VARCHAR(10)),
                   TRJFILE.TJ_PROCEEDS,
                   TRJFILE.TJ_LNAMT,
                   TRJFILE.TJ_FINCHG,
                   TRJFILE.TJ_INTCHG,
                   TRJFILE.TJ_SERCHG,
                   TRJFILE.TJ_EXTCHG,
                   TRJFILE.TJ_MAINTFEE,
                   TRJFILE.TJ_ORGTERM,
                   TRJFILE.TJ_INSURED,
                   TRJFILE.TJ_INSUNEARN_1,
                   TRJFILE.TJ_INSUNEARN_2,
                   TRJFILE.TJ_INSUNEARN_3,
                   TRJFILE.TJ_INSUNEARN_4,
                   TRJFILE.TJ_INSUNEARN_5,
                   TRJFILE.TJ_INSUNEARN_6,
                   TRJFILE.TJ_INSUNEARN_7,
                   TRJFILE.TJ_INSUNEARN_8,
                   TRJFILE.TJ_INSUNEARN_9,
                   TRJFILE.TJ_INSUNEARN_10,
                   TRJFILE.TJ_DLNO,
                   TRJFILE.TJ_DLRV,
                   TRJFILE.TJ_DLSPRV,
                   TRJFILE.TJ_DLDISC,
                   TRJFILE.TJ_DLPARV,
                   TRJFILE.TJ_DLRECOURSE,
                   TRJFILE.TJ_DLPARTRCAMT,
                   TRJFILE.TJ_PURBAL,
                   TRJFILE.TJ_UNEARN_1,
                   TRJFILE.TJ_UNEARN_2,
                   TRJFILE.TJ_UNEARN_3,
                   TRJFILE.TJ_UNEARN_4,
                   TRJFILE.TJ_UNEARN_5,
                   TRJFILE.TJ_UNEARN_6,
                   TRJFILE.TJ_UNEARN_7,
                   TRJFILE.TJ_UNEARN_8,
                   TRJFILE.TJ_UNEARN_9,
                   TRJFILE.TJ_UNEARN_10,
                   TRJFILE.TJ_PRLNNO,
                   TRJFILE.TJ_DLRVPRIN_1,
                   TRJFILE.TJ_DLRVPRIN_2,
                   TRJFILE.TJ_DLRVPRIN_3,
                   TRJFILE.TJ_DLRVCHGS_1,
                   TRJFILE.TJ_DLRVCHGS_2,
                   TRJFILE.TJ_DLRVCHGS_3,
                   TRJFILE.TJ_DLPARVPAID,
                   TRJFILE.TJ_DLPARVHELD,
                   TRJFILE.TJ_USERID,
                   CAST(TRJFILE.TJ_POSTDATE
                                                  AS VARCHAR(10)),
                   TRJFILE.TJ_POSTTIME,
                   TRJFILE.TJ_PLCD,
                   TRJFILE.TJ_PLTYPE,
                   TRJFILE.TJ_REPOCD
            FROM DBO.TRJFILE
            WHERE TRJFILE.TJ_BRNO  = :QTRJ1-WBEG-BRNO
              AND TRJFILE.TJ_DATE >= :QTRJ1-WBEG-DATE
              AND TRJFILE.TJ_DATE <= :QTRJ1-WEND-DATE
              AND TRJFILE.TJ_CLASS >= :QTRJ1-WBEG-CLASS
              AND TRJFILE.TJ_CLASS <= :QTRJ1-WEND-CLASS
              AND TRJFILE.TJ_ACCTNO >= :QTRJ1-WBEG-ACCTNO
              AND TRJFILE.TJ_ACCTNO <= :QTRJ1-WEND-ACCTNO
              AND TRJFILE.TJ_LAWCODE >= :QTRJ1-WBEG-LAWCODE
              AND TRJFILE.TJ_LAWCODE <= :QTRJ1-WEND-LAWCODE
              AND TRJFILE.TJ_LINE >= :QTRJ1-WBEG-LINE
              AND TRJFILE.TJ_LINE <= :QTRJ1-WEND-LINE
            ORDER BY TRJFILE.TJ_BRNO,
                     TRJFILE.TJ_DATE,
                     TRJFILE.TJ_CLASS,
                     TRJFILE.TJ_ACCTNO,
                     TRJFILE.TJ_LAWCODE,
                     TRJFILE.TJ_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRJ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRJ1-WBEG-BRNO
                          QTRJ1-WEND-BRNO
                          QTRJ1-WBEG-CLASS
                          QTRJ1-WEND-CLASS
                          QTRJ1-WBEG-ACCTNO
                          QTRJ1-WEND-ACCTNO
                          QTRJ1-WBEG-LAWCODE
                          QTRJ1-WEND-LAWCODE
                          QTRJ1-WBEG-LINE
                          QTRJ1-WEND-LINE.
           MOVE SPACES TO QTRJ1-WBEG-DATE
                          QTRJ1-WEND-DATE.

           MOVE STAT---GOOD TO TRJ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-TRJ1-FILE-SCAN.
           PERFORM START-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           MOVE TRJ-PATH-SQL    TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.

           IF ( TRJ1-CURSOR-SC-STAT-GOOD )
              PERFORM CLOSE-TRJ1-FILE.

           IF ( QTRJ1-WEND-LINE = ZEROES    ) OR
              ( QTRJ1-WEND-LINE NOT NUMERIC )
              MOVE TRJ-PATH-OWNBR        TO QTRJ1-WBEG-BRNO
              MOVE TRJ-PATH-OWNBR        TO QTRJ1-WEND-BRNO
              MOVE TJ-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRJ1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRJ1-WEND-DATE
              MOVE TJ-CLASS             TO QTRJ1-WBEG-CLASS
              MOVE TJ-ACCTNO            TO QTRJ1-WBEG-ACCTNO
              MOVE TJ-LAWCODE           TO QTRJ1-WBEG-LAWCODE
              MOVE TJ-LINE              TO QTRJ1-WBEG-LINE
              MOVE ALL "9"              TO QTRJ1-WEND-CLASS
                                           QTRJ1-WEND-ACCTNO
                                           QTRJ1-WEND-LAWCODE
                                           QTRJ1-WEND-LINE
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }        

              MOVE TJ-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRJ1-WSBEG-DATE  

              IF ( TJ-CLASS = 999 )
                 MOVE TJ-CLASS          TO QTRJ1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRJ1-WSBEG-CLASS = TJ-CLASS + 1
              END-IF

              IF ( TJ-ACCTNO = 99999999 )
                 MOVE TJ-ACCTNO         TO QTRJ1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRJ1-WSBEG-ACCTNO = TJ-ACCTNO + 1 
              END-IF

              IF ( TJ-LAWCODE = 99 )
                 MOVE TJ-LAWCODE        TO QTRJ1-WSBEG-LAWCODE
              ELSE
                 COMPUTE QTRJ1-WSBEG-LAWCODE = TJ-LAWCODE + 1.

           EXEC SQL
            DECLARE TRJ1_CURSOR_SC CURSOR FOR
            SELECT TRJFILE.TJ_BRNO,
                   CAST(TRJFILE.TJ_DATE AS VARCHAR(10)),
                   TRJFILE.TJ_CLASS,
                   TRJFILE.TJ_ACCTNO,
                   TRJFILE.TJ_LAWCODE,
                   TRJFILE.TJ_LINE,
                   CAST(TRJFILE.TJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRJFILE.TJ_SSNO_1,
                   TRJFILE.TJ_MAKERCD_1,
                   TRJFILE.TJ_SSNO_2,
                   TRJFILE.TJ_MAKERCD_2,
                   TRJFILE.TJ_SSNO_3,
                   TRJFILE.TJ_MAKERCD_3,
                   TRJFILE.TJ_SSNO_4,
                   TRJFILE.TJ_MAKERCD_4,
                   TRJFILE.TJ_REF_FRLC,
                   TRJFILE.TJ_REF_X,
                   TRJFILE.TJ_REF_TOLC,
                   TRJFILE.TJ_FRLC,
                   TRJFILE.TJ_TOLC,
                   TRJFILE.TJ_PURCD,
                   TRJFILE.TJ_SRCD,
                   TRJFILE.TJ_COLLCD,
                   TRJFILE.TJ_SERVACCT,
                   TRJFILE.TJ_LOANTYPE,
                   TRJFILE.TJ_SUBCLASS,
                   CAST(TRJFILE.TJ_LOANDATE
                                                  AS VARCHAR(10)),
                   TRJFILE.TJ_PROCEEDS,
                   TRJFILE.TJ_LNAMT,
                   TRJFILE.TJ_FINCHG,
                   TRJFILE.TJ_INTCHG,
                   TRJFILE.TJ_SERCHG,
                   TRJFILE.TJ_EXTCHG,
                   TRJFILE.TJ_MAINTFEE,
                   TRJFILE.TJ_ORGTERM,
                   TRJFILE.TJ_INSURED,
                   TRJFILE.TJ_INSUNEARN_1,
                   TRJFILE.TJ_INSUNEARN_2,
                   TRJFILE.TJ_INSUNEARN_3,
                   TRJFILE.TJ_INSUNEARN_4,
                   TRJFILE.TJ_INSUNEARN_5,
                   TRJFILE.TJ_INSUNEARN_6,
                   TRJFILE.TJ_INSUNEARN_7,
                   TRJFILE.TJ_INSUNEARN_8,
                   TRJFILE.TJ_INSUNEARN_9,
                   TRJFILE.TJ_INSUNEARN_10,
                   TRJFILE.TJ_DLNO,
                   TRJFILE.TJ_DLRV,
                   TRJFILE.TJ_DLSPRV,
                   TRJFILE.TJ_DLDISC,
                   TRJFILE.TJ_DLPARV,
                   TRJFILE.TJ_DLRECOURSE,
                   TRJFILE.TJ_DLPARTRCAMT,
                   TRJFILE.TJ_PURBAL,
                   TRJFILE.TJ_UNEARN_1,
                   TRJFILE.TJ_UNEARN_2,
                   TRJFILE.TJ_UNEARN_3,
                   TRJFILE.TJ_UNEARN_4,
                   TRJFILE.TJ_UNEARN_5,
                   TRJFILE.TJ_UNEARN_6,
                   TRJFILE.TJ_UNEARN_7,
                   TRJFILE.TJ_UNEARN_8,
                   TRJFILE.TJ_UNEARN_9,
                   TRJFILE.TJ_UNEARN_10,
                   TRJFILE.TJ_PRLNNO,
                   TRJFILE.TJ_DLRVPRIN_1,
                   TRJFILE.TJ_DLRVPRIN_2,
                   TRJFILE.TJ_DLRVPRIN_3,
                   TRJFILE.TJ_DLRVCHGS_1,
                   TRJFILE.TJ_DLRVCHGS_2,
                   TRJFILE.TJ_DLRVCHGS_3,
                   TRJFILE.TJ_DLPARVPAID,
                   TRJFILE.TJ_DLPARVHELD,
                   TRJFILE.TJ_USERID,
                   CAST(TRJFILE.TJ_POSTDATE
                                                  AS VARCHAR(10)),
                   TRJFILE.TJ_POSTTIME,
                   TRJFILE.TJ_PLCD,
                   TRJFILE.TJ_PLTYPE,
                   TRJFILE.TJ_REPOCD
            FROM DBO.TRJFILE
            WHERE (  TRJFILE.TJ_BRNO     = :QTRJ1-WBEG-BRNO      )
              AND (( TRJFILE.TJ_DATE    >= :QTRJ1-WBEG-DATE
              AND    TRJFILE.TJ_CLASS   >= :QTRJ1-WBEG-CLASS
              AND    TRJFILE.TJ_ACCTNO  >= :QTRJ1-WBEG-ACCTNO
              AND    TRJFILE.TJ_LAWCODE >= :QTRJ1-WBEG-LAWCODE
              AND    TRJFILE.TJ_LINE    >= :QTRJ1-WBEG-LINE     )
               OR  ( TRJFILE.TJ_DATE    >= :QTRJ1-WBEG-DATE
              AND    TRJFILE.TJ_CLASS   >= :QTRJ1-WBEG-CLASS
              AND    TRJFILE.TJ_ACCTNO  >= :QTRJ1-WBEG-ACCTNO
              AND    TRJFILE.TJ_LAWCODE >= :QTRJ1-WSBEG-LAWCODE
              AND    TRJFILE.TJ_LINE    >= 0                    )
               OR  ( TRJFILE.TJ_DATE    >= :QTRJ1-WBEG-DATE
              AND    TRJFILE.TJ_CLASS   >= :QTRJ1-WBEG-CLASS
              AND    TRJFILE.TJ_ACCTNO  >= :QTRJ1-WSBEG-ACCTNO
              AND    TRJFILE.TJ_LAWCODE >= 0
              AND    TRJFILE.TJ_LINE    >= 0                    )
               OR  ( TRJFILE.TJ_DATE    >= :QTRJ1-WBEG-DATE
              AND    TRJFILE.TJ_CLASS   >= :QTRJ1-WSBEG-CLASS
              AND    TRJFILE.TJ_ACCTNO  >= 0
              AND    TRJFILE.TJ_LAWCODE >= 0
              AND    TRJFILE.TJ_LINE    >= 0                    )
               OR  ( TRJFILE.TJ_DATE    >= :QTRJ1-WSBEG-DATE
              AND    TRJFILE.TJ_CLASS   >= 0
              AND    TRJFILE.TJ_ACCTNO  >= 0
              AND    TRJFILE.TJ_LAWCODE >= 0
              AND    TRJFILE.TJ_LINE    >= 0                    ))
              AND (  TRJFILE.TJ_DATE    <= :QTRJ1-WEND-DATE
              AND    TRJFILE.TJ_CLASS   <= :QTRJ1-WEND-CLASS
              AND    TRJFILE.TJ_ACCTNO  <= :QTRJ1-WEND-ACCTNO
              AND    TRJFILE.TJ_LAWCODE <= :QTRJ1-WEND-LAWCODE
              AND    TRJFILE.TJ_LINE    <= :QTRJ1-WEND-LINE      )
            ORDER BY TRJFILE.TJ_BRNO,
                     TRJFILE.TJ_DATE,
                     TRJFILE.TJ_CLASS,
                     TRJFILE.TJ_ACCTNO,
                     TRJFILE.TJ_LAWCODE,
                     TRJFILE.TJ_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRJ1_CURSOR_SC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRJ1-WBEG-BRNO 
                          QTRJ1-WEND-BRNO
                          QTRJ1-WBEG-CLASS
                          QTRJ1-WEND-CLASS
                          QTRJ1-WBEG-ACCTNO
                          QTRJ1-WEND-ACCTNO
                          QTRJ1-WBEG-LAWCODE
                          QTRJ1-WEND-LAWCODE
                          QTRJ1-WBEG-LINE
                          QTRJ1-WEND-LINE.
           MOVE SPACES TO QTRJ1-WBEG-DATE
                          QTRJ1-WEND-DATE.

           MOVE STAT---GOOD TO TRJ1-CURSOR-SC-STAT.

      *-----------------------------------------------------------------
       READ-TRJ1-FILE.
           PERFORM READ-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO. 
           MOVE TRJ-PATH-SQL    TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.
           INITIALIZE QTRJ-REC.

           MOVE TJ-BRNO             TO QTJ-BRNO.
           MOVE TJ-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTJ-DATE.
           MOVE TJ-CLASS            TO QTJ-CLASS.
           MOVE TJ-ACCTNO           TO QTJ-ACCTNO.
           MOVE TJ-LAWCODE          TO QTJ-LAWCODE.
           MOVE TJ-LINE             TO QTJ-LINE.

           EXEC SQL
            SELECT TRJFILE.TJ_BRNO,
                   CAST(TRJFILE.TJ_DATE AS VARCHAR(10)),
                   TRJFILE.TJ_CLASS,
                   TRJFILE.TJ_ACCTNO,
                   TRJFILE.TJ_LAWCODE,
                   TRJFILE.TJ_LINE,
                   CAST(TRJFILE.TJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRJFILE.TJ_SSNO_1,
                   TRJFILE.TJ_MAKERCD_1,
                   TRJFILE.TJ_SSNO_2,
                   TRJFILE.TJ_MAKERCD_2,
                   TRJFILE.TJ_SSNO_3,
                   TRJFILE.TJ_MAKERCD_3,
                   TRJFILE.TJ_SSNO_4,
                   TRJFILE.TJ_MAKERCD_4,
                   TRJFILE.TJ_REF_FRLC,
                   TRJFILE.TJ_REF_X,
                   TRJFILE.TJ_REF_TOLC,
                   TRJFILE.TJ_FRLC,
                   TRJFILE.TJ_TOLC,
                   TRJFILE.TJ_PURCD,
                   TRJFILE.TJ_SRCD,
                   TRJFILE.TJ_COLLCD,
                   TRJFILE.TJ_SERVACCT,
                   TRJFILE.TJ_LOANTYPE,
                   TRJFILE.TJ_SUBCLASS,
                   CAST(TRJFILE.TJ_LOANDATE
                                                  AS VARCHAR(10)),
                   TRJFILE.TJ_PROCEEDS,
                   TRJFILE.TJ_LNAMT,
                   TRJFILE.TJ_FINCHG,
                   TRJFILE.TJ_INTCHG,
                   TRJFILE.TJ_SERCHG,
                   TRJFILE.TJ_EXTCHG,
                   TRJFILE.TJ_MAINTFEE,
                   TRJFILE.TJ_ORGTERM,
                   TRJFILE.TJ_INSURED,
                   TRJFILE.TJ_INSUNEARN_1,
                   TRJFILE.TJ_INSUNEARN_2,
                   TRJFILE.TJ_INSUNEARN_3,
                   TRJFILE.TJ_INSUNEARN_4,
                   TRJFILE.TJ_INSUNEARN_5,
                   TRJFILE.TJ_INSUNEARN_6,
                   TRJFILE.TJ_INSUNEARN_7,
                   TRJFILE.TJ_INSUNEARN_8,
                   TRJFILE.TJ_INSUNEARN_9,
                   TRJFILE.TJ_INSUNEARN_10,
                   TRJFILE.TJ_DLNO,
                   TRJFILE.TJ_DLRV,
                   TRJFILE.TJ_DLSPRV,
                   TRJFILE.TJ_DLDISC,
                   TRJFILE.TJ_DLPARV,
                   TRJFILE.TJ_DLRECOURSE,
                   TRJFILE.TJ_DLPARTRCAMT,
                   TRJFILE.TJ_PURBAL,
                   TRJFILE.TJ_UNEARN_1,
                   TRJFILE.TJ_UNEARN_2,
                   TRJFILE.TJ_UNEARN_3,
                   TRJFILE.TJ_UNEARN_4,
                   TRJFILE.TJ_UNEARN_5,
                   TRJFILE.TJ_UNEARN_6,
                   TRJFILE.TJ_UNEARN_7,
                   TRJFILE.TJ_UNEARN_8,
                   TRJFILE.TJ_UNEARN_9,
                   TRJFILE.TJ_UNEARN_10,
                   TRJFILE.TJ_PRLNNO,
                   TRJFILE.TJ_DLRVPRIN_1,
                   TRJFILE.TJ_DLRVPRIN_2,
                   TRJFILE.TJ_DLRVPRIN_3,
                   TRJFILE.TJ_DLRVCHGS_1,
                   TRJFILE.TJ_DLRVCHGS_2,
                   TRJFILE.TJ_DLRVCHGS_3,
                   TRJFILE.TJ_DLPARVPAID,
                   TRJFILE.TJ_DLPARVHELD,
                   TRJFILE.TJ_USERID,
                   CAST(TRJFILE.TJ_POSTDATE
                                                  AS VARCHAR(10)),
                   TRJFILE.TJ_POSTTIME,
                   TRJFILE.TJ_PLCD,
                   TRJFILE.TJ_PLTYPE,
                   TRJFILE.TJ_REPOCD
            INTO :QTRJ-REC
            FROM DBO.TRJFILE
            WHERE TRJFILE.TJ_BRNO = :QTJ-BRNO
              AND TRJFILE.TJ_DATE = :QTJ-DATE
              AND TRJFILE.TJ_CLASS = :QTJ-CLASS
              AND TRJFILE.TJ_ACCTNO = :QTJ-ACCTNO
              AND TRJFILE.TJ_LAWCODE = :QTJ-LAWCODE
              AND TRJFILE.TJ_LINE = :QTJ-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRJ1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRJ-FIELDS.

      *-----------------------------------------------------------------
       READ-TRJ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           MOVE TRJ-PATH-SQL    TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.
           INITIALIZE QTRJ-REC.

           IF ( TRJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRJ1_CURSOR INTO :QTRJ-REC
              END-EXEC
           ELSE
              IF ( TRJ1-CURSOR-SC-STAT-GOOD )
                 EXEC SQL
                      FETCH TRJ1_CURSOR_SC INTO :QTRJ-REC
                 END-EXEC
              ELSE
                 MOVE 9        TO SQLCODE
                 MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRJ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRJ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRJ1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRJ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRJ1-CURSOR-STAT.

           IF ( TRJ1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE TRJ1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO TRJ1-CURSOR-SC-STAT.

