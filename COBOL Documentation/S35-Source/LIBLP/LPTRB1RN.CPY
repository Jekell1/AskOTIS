      * COPYMEMBER: LIBLP/LPTRB1RN
       LOAD-TRB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRB-ALL-DATA
              MOVE TRB-ALL-PATH             TO TRB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRB-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRB-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRB-ALL-DATA
              MOVE TRB-ALL-PATH             TO TRB-PATH.

      *-----------------------------------------------------------------
       OPEN-TRB1-FILE.
           PERFORM LOAD-TRB1-FILE.
           PERFORM OPEN-IT.
           MOVE TRB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRB1-FILE.
           PERFORM START-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.

           IF ( TRB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRB1-FILE.

           IF ( QTRB1-WEND-LINE = ZEROES    ) OR
              ( QTRB1-WEND-LINE NOT NUMERIC )
              MOVE TRB-PATH-OWNBR        TO QTRB1-WBEG-BRNO
              MOVE TRB-PATH-OWNBR        TO QTRB1-WEND-BRNO
              MOVE TB-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRB1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRB1-WEND-DATE
              MOVE TB-CLASS             TO QTRB1-WBEG-CLASS
              MOVE TB-ACCTNO            TO QTRB1-WBEG-ACCTNO
              MOVE TB-LAWCODE           TO QTRB1-WBEG-LAWCODE
              MOVE TB-LINE              TO QTRB1-WBEG-LINE
              MOVE ALL "9"              TO QTRB1-WEND-CLASS
                                           QTRB1-WEND-ACCTNO
                                           QTRB1-WEND-LAWCODE
                                           QTRB1-WEND-LINE.


           EXEC SQL
            DECLARE TRB1_CURSOR CURSOR FOR
            SELECT TRBFILE.TB_BRNO,
                   CAST(TRBFILE.TB_DATE AS VARCHAR(10)),
                   TRBFILE.TB_CLASS,
                   TRBFILE.TB_ACCTNO,
                   TRBFILE.TB_LAWCODE,
                   TRBFILE.TB_LINE,
                   CAST(TRBFILE.TB_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRBFILE.TB_SSNO_1,
                   TRBFILE.TB_MAKERCD_1,
                   TRBFILE.TB_SSNO_2,
                   TRBFILE.TB_MAKERCD_2,
                   TRBFILE.TB_SSNO_3,
                   TRBFILE.TB_MAKERCD_3,
                   TRBFILE.TB_SSNO_4,
                   TRBFILE.TB_MAKERCD_4,
                   TRBFILE.TB_REF_FRTO,
                   TRBFILE.TB_REFBR,
                   TRBFILE.TB_PURCD,
                   TRBFILE.TB_SRCD,
                   TRBFILE.TB_COLLCD,
                   TRBFILE.TB_SERVACCT,
                   TRBFILE.TB_LOANTYPE,
                   TRBFILE.TB_SUBCLASS,
                   CAST(TRBFILE.TB_LOANDATE
                                                  AS VARCHAR(10)),
                   TRBFILE.TB_PROCEEDS,
                   TRBFILE.TB_LNAMT,
                   TRBFILE.TB_FINCHG,
                   TRBFILE.TB_INTCHG,
                   TRBFILE.TB_SERCHG,
                   TRBFILE.TB_EXTCHG,
                   TRBFILE.TB_MAINTFEE,
                   TRBFILE.TB_ORGTERM,
                   TRBFILE.TB_INSURED,
                   TRBFILE.TB_INSUNEARN_1,
                   TRBFILE.TB_INSUNEARN_2,
                   TRBFILE.TB_INSUNEARN_3,
                   TRBFILE.TB_INSUNEARN_4,
                   TRBFILE.TB_INSUNEARN_5,
                   TRBFILE.TB_INSUNEARN_6,
                   TRBFILE.TB_INSUNEARN_7,
                   TRBFILE.TB_INSUNEARN_8,
                   TRBFILE.TB_INSUNEARN_9,
                   TRBFILE.TB_INSUNEARN_10,
                   TRBFILE.TB_DLNO,
                   TRBFILE.TB_DLRV,
                   TRBFILE.TB_DLSPRV,
                   TRBFILE.TB_DLDISC,
                   TRBFILE.TB_DLPARV,
                   TRBFILE.TB_DLRECOURSE,
                   TRBFILE.TB_DLPARTRCAMT,
                   TRBFILE.TB_PURBAL,
                   TRBFILE.TB_UNEARN_1,
                   TRBFILE.TB_UNEARN_2,
                   TRBFILE.TB_UNEARN_3,
                   TRBFILE.TB_UNEARN_4,
                   TRBFILE.TB_UNEARN_5,
                   TRBFILE.TB_UNEARN_6,
                   TRBFILE.TB_UNEARN_7,
                   TRBFILE.TB_UNEARN_8,
                   TRBFILE.TB_UNEARN_9,
                   TRBFILE.TB_UNEARN_10,
                   TRBFILE.TB_REF_ACCTNO,
                   TRBFILE.TB_DLRVPRIN_1,
                   TRBFILE.TB_DLRVPRIN_2,
                   TRBFILE.TB_DLRVPRIN_3,
                   TRBFILE.TB_DLRVCHGS_1,
                   TRBFILE.TB_DLRVCHGS_2,
                   TRBFILE.TB_DLRVCHGS_3,
                   TRBFILE.TB_DLPARVPAID,
                   TRBFILE.TB_DLPARVHELD,
                   TRBFILE.TB_USERID,
                   CAST(TRBFILE.TB_POSTDATE
                                                  AS VARCHAR(10)),
                   TRBFILE.TB_POSTTIME,
                   TRBFILE.TB_PLCD,
                   TRBFILE.TB_PLTYPE,
                   TRBFILE.TB_REPOCD,
                   TRBFILE.TB_ACTIONCD2
            FROM DBO.TRBFILE
            WHERE TRBFILE.TB_BRNO  = :QTRB1-WBEG-BRNO
              AND TRBFILE.TB_DATE >= :QTRB1-WBEG-DATE
              AND TRBFILE.TB_DATE <= :QTRB1-WEND-DATE
              AND TRBFILE.TB_CLASS >= :QTRB1-WBEG-CLASS
              AND TRBFILE.TB_CLASS <= :QTRB1-WEND-CLASS
              AND TRBFILE.TB_ACCTNO >= :QTRB1-WBEG-ACCTNO
              AND TRBFILE.TB_ACCTNO <= :QTRB1-WEND-ACCTNO
              AND TRBFILE.TB_LAWCODE >= :QTRB1-WBEG-LAWCODE
              AND TRBFILE.TB_LAWCODE <= :QTRB1-WEND-LAWCODE
              AND TRBFILE.TB_LINE >= :QTRB1-WBEG-LINE
              AND TRBFILE.TB_LINE <= :QTRB1-WEND-LINE
            ORDER BY TRBFILE.TB_BRNO,
                     TRBFILE.TB_DATE,
                     TRBFILE.TB_CLASS,
                     TRBFILE.TB_ACCTNO,
                     TRBFILE.TB_LAWCODE,
                     TRBFILE.TB_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRB1-WBEG-BRNO 
                          QTRB1-WEND-BRNO
                          QTRB1-WBEG-CLASS
                          QTRB1-WEND-CLASS
                          QTRB1-WBEG-ACCTNO
                          QTRB1-WEND-ACCTNO
                          QTRB1-WBEG-LAWCODE
                          QTRB1-WEND-LAWCODE
                          QTRB1-WBEG-LINE
                          QTRB1-WEND-LINE.
           MOVE SPACES TO QTRB1-WBEG-DATE
                          QTRB1-WEND-DATE.

           MOVE STAT---GOOD TO TRB1-CURSOR-STAT.
      
      *-----------------------------------------------------------------
       START-TRB1-FILE-SCAN.
           PERFORM START-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.

           IF ( TRB1-CURSOR-SC-STAT-GOOD )
              PERFORM CLOSE-TRB1-FILE.

           IF ( QTRB1-WEND-LINE = ZEROES    ) OR
              ( QTRB1-WEND-LINE NOT NUMERIC )
              MOVE TRB-PATH-OWNBR        TO QTRB1-WBEG-BRNO
              MOVE TRB-PATH-OWNBR        TO QTRB1-WEND-BRNO
              MOVE TB-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRB1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRB1-WEND-DATE
              MOVE TB-CLASS             TO QTRB1-WBEG-CLASS
              MOVE TB-ACCTNO            TO QTRB1-WBEG-ACCTNO
              MOVE TB-LAWCODE           TO QTRB1-WBEG-LAWCODE
              MOVE TB-LINE              TO QTRB1-WBEG-LINE
              MOVE ALL "9"              TO QTRB1-WEND-CLASS
                                           QTRB1-WEND-ACCTNO
                                           QTRB1-WEND-LAWCODE
                                           QTRB1-WEND-LINE.
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }        

              MOVE TB-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRB1-WSBEG-DATE  

              IF ( TB-CLASS = 999 )
                 MOVE TB-CLASS          TO QTRB1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRB1-WSBEG-CLASS = TB-CLASS + 1
              END-IF

              IF ( TB-ACCTNO = 99999999 )
                 MOVE TB-ACCTNO         TO QTRB1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRB1-WSBEG-ACCTNO = TB-ACCTNO + 1 
              END-IF

              IF ( TB-LAWCODE = 99 )
                 MOVE TB-LAWCODE        TO QTRB1-WSBEG-LAWCODE
              ELSE
                 COMPUTE QTRB1-WSBEG-LAWCODE = TB-LAWCODE + 1.

           EXEC SQL
            DECLARE TRB1_CURSOR_SC CURSOR FOR
            SELECT TRBFILE.TB_BRNO,
                   CAST(TRBFILE.TB_DATE AS VARCHAR(10)),
                   TRBFILE.TB_CLASS,
                   TRBFILE.TB_ACCTNO,
                   TRBFILE.TB_LAWCODE,
                   TRBFILE.TB_LINE,
                   CAST(TRBFILE.TB_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRBFILE.TB_SSNO_1,
                   TRBFILE.TB_MAKERCD_1,
                   TRBFILE.TB_SSNO_2,
                   TRBFILE.TB_MAKERCD_2,
                   TRBFILE.TB_SSNO_3,
                   TRBFILE.TB_MAKERCD_3,
                   TRBFILE.TB_SSNO_4,
                   TRBFILE.TB_MAKERCD_4,
                   TRBFILE.TB_REF_FRTO,
                   TRBFILE.TB_REFBR,
                   TRBFILE.TB_PURCD,
                   TRBFILE.TB_SRCD,
                   TRBFILE.TB_COLLCD,
                   TRBFILE.TB_SERVACCT,
                   TRBFILE.TB_LOANTYPE,
                   TRBFILE.TB_SUBCLASS,
                   CAST(TRBFILE.TB_LOANDATE
                                                  AS VARCHAR(10)),
                   TRBFILE.TB_PROCEEDS,
                   TRBFILE.TB_LNAMT,
                   TRBFILE.TB_FINCHG,
                   TRBFILE.TB_INTCHG,
                   TRBFILE.TB_SERCHG,
                   TRBFILE.TB_EXTCHG,
                   TRBFILE.TB_MAINTFEE,
                   TRBFILE.TB_ORGTERM,
                   TRBFILE.TB_INSURED,
                   TRBFILE.TB_INSUNEARN_1,
                   TRBFILE.TB_INSUNEARN_2,
                   TRBFILE.TB_INSUNEARN_3,
                   TRBFILE.TB_INSUNEARN_4,
                   TRBFILE.TB_INSUNEARN_5,
                   TRBFILE.TB_INSUNEARN_6,
                   TRBFILE.TB_INSUNEARN_7,
                   TRBFILE.TB_INSUNEARN_8,
                   TRBFILE.TB_INSUNEARN_9,
                   TRBFILE.TB_INSUNEARN_10,
                   TRBFILE.TB_DLNO,
                   TRBFILE.TB_DLRV,
                   TRBFILE.TB_DLSPRV,
                   TRBFILE.TB_DLDISC,
                   TRBFILE.TB_DLPARV,
                   TRBFILE.TB_DLRECOURSE,
                   TRBFILE.TB_DLPARTRCAMT,
                   TRBFILE.TB_PURBAL,
                   TRBFILE.TB_UNEARN_1,
                   TRBFILE.TB_UNEARN_2,
                   TRBFILE.TB_UNEARN_3,
                   TRBFILE.TB_UNEARN_4,
                   TRBFILE.TB_UNEARN_5,
                   TRBFILE.TB_UNEARN_6,
                   TRBFILE.TB_UNEARN_7,
                   TRBFILE.TB_UNEARN_8,
                   TRBFILE.TB_UNEARN_9,
                   TRBFILE.TB_UNEARN_10,
                   TRBFILE.TB_REF_ACCTNO,
                   TRBFILE.TB_DLRVPRIN_1,
                   TRBFILE.TB_DLRVPRIN_2,
                   TRBFILE.TB_DLRVPRIN_3,
                   TRBFILE.TB_DLRVCHGS_1,
                   TRBFILE.TB_DLRVCHGS_2,
                   TRBFILE.TB_DLRVCHGS_3,
                   TRBFILE.TB_DLPARVPAID,
                   TRBFILE.TB_DLPARVHELD,
                   TRBFILE.TB_USERID,
                   CAST(TRBFILE.TB_POSTDATE
                                                  AS VARCHAR(10)),
                   TRBFILE.TB_POSTTIME,
                   TRBFILE.TB_PLCD,
                   TRBFILE.TB_PLTYPE,
                   TRBFILE.TB_REPOCD,
                   TRBFILE.TB_ACTIONCD2
            FROM DBO.TRBFILE
            WHERE (  TRBFILE.TB_BRNO    = :QTRB1-WBEG-BRNO       )
              AND (( TRBFILE.TB_DATE    >= :QTRB1-WBEG-DATE
              AND    TRBFILE.TB_CLASS   >= :QTRB1-WBEG-CLASS
              AND    TRBFILE.TB_ACCTNO  >= :QTRB1-WBEG-ACCTNO
              AND    TRBFILE.TB_LAWCODE >= :QTRB1-WBEG-LAWCODE
              AND    TRBFILE.TB_LINE    >= :QTRB1-WBEG-LINE     )
               OR  ( TRBFILE.TB_DATE    >= :QTRB1-WBEG-DATE
              AND    TRBFILE.TB_CLASS   >= :QTRB1-WBEG-CLASS
              AND    TRBFILE.TB_ACCTNO  >= :QTRB1-WBEG-ACCTNO
              AND    TRBFILE.TB_LAWCODE >= :QTRB1-WSBEG-LAWCODE
              AND    TRBFILE.TB_LINE    >= 0                    )
               OR  ( TRBFILE.TB_DATE    >= :QTRB1-WBEG-DATE
              AND    TRBFILE.TB_CLASS   >= :QTRB1-WBEG-CLASS
              AND    TRBFILE.TB_ACCTNO  >= :QTRB1-WSBEG-ACCTNO
              AND    TRBFILE.TB_LAWCODE >= 0
              AND    TRBFILE.TB_LINE    >= 0                    )
               OR  ( TRBFILE.TB_DATE    >= :QTRB1-WBEG-DATE
              AND    TRBFILE.TB_CLASS   >= :QTRB1-WSBEG-CLASS
              AND    TRBFILE.TB_ACCTNO  >= 0
              AND    TRBFILE.TB_LAWCODE >= 0
              AND    TRBFILE.TB_LINE    >= 0                    )
               OR  ( TRBFILE.TB_DATE    >= :QTRB1-WSBEG-DATE
              AND    TRBFILE.TB_CLASS   >= 0
              AND    TRBFILE.TB_ACCTNO  >= 0
              AND    TRBFILE.TB_LAWCODE >= 0
              AND    TRBFILE.TB_LINE    >= 0                    ))
              AND (  TRBFILE.TB_DATE    <= :QTRB1-WEND-DATE
              AND    TRBFILE.TB_CLASS   <= :QTRB1-WEND-CLASS
              AND    TRBFILE.TB_ACCTNO  <= :QTRB1-WEND-ACCTNO
              AND    TRBFILE.TB_LAWCODE <= :QTRB1-WEND-LAWCODE
              AND    TRBFILE.TB_LINE    <= :QTRB1-WEND-LINE      )
            ORDER BY TRBFILE.TB_BRNO,
                     TRBFILE.TB_DATE,
                     TRBFILE.TB_CLASS,
                     TRBFILE.TB_ACCTNO,
                     TRBFILE.TB_LAWCODE,
                     TRBFILE.TB_LINE           
           END-EXEC.

           EXEC SQL
               OPEN TRB1_CURSOR_SC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRB1-WBEG-BRNO 
                          QTRB1-WEND-BRNO
                          QTRB1-WBEG-CLASS
                          QTRB1-WEND-CLASS
                          QTRB1-WBEG-ACCTNO
                          QTRB1-WEND-ACCTNO
                          QTRB1-WBEG-LAWCODE
                          QTRB1-WEND-LAWCODE
                          QTRB1-WBEG-LINE
                          QTRB1-WEND-LINE.
           MOVE SPACES TO QTRB1-WBEG-DATE
                          QTRB1-WEND-DATE.
 

           MOVE STAT---GOOD TO TRB1-CURSOR-SC-STAT.

      *-----------------------------------------------------------------
       READ-TRB1-FILE.
           PERFORM READ-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO. 
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.
           INITIALIZE QTRB-REC.

           MOVE TB-BRNO             TO QTB-BRNO.
           MOVE TB-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTB-DATE.
           MOVE TB-CLASS            TO QTB-CLASS.
           MOVE TB-ACCTNO           TO QTB-ACCTNO.
           MOVE TB-LAWCODE          TO QTB-LAWCODE.
           MOVE TB-LINE             TO QTB-LINE.

           EXEC SQL
            SELECT TRBFILE.TB_BRNO,
                   CAST(TRBFILE.TB_DATE AS VARCHAR(10)),
                   TRBFILE.TB_CLASS,
                   TRBFILE.TB_ACCTNO,
                   TRBFILE.TB_LAWCODE,
                   TRBFILE.TB_LINE,
                   CAST(TRBFILE.TB_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRBFILE.TB_SSNO_1,
                   TRBFILE.TB_MAKERCD_1,
                   TRBFILE.TB_SSNO_2,
                   TRBFILE.TB_MAKERCD_2,
                   TRBFILE.TB_SSNO_3,
                   TRBFILE.TB_MAKERCD_3,
                   TRBFILE.TB_SSNO_4,
                   TRBFILE.TB_MAKERCD_4,
                   TRBFILE.TB_REF_FRTO,
                   TRBFILE.TB_REFBR,
                   TRBFILE.TB_PURCD,
                   TRBFILE.TB_SRCD,
                   TRBFILE.TB_COLLCD,
                   TRBFILE.TB_SERVACCT,
                   TRBFILE.TB_LOANTYPE,
                   TRBFILE.TB_SUBCLASS,
                   CAST(TRBFILE.TB_LOANDATE
                                                  AS VARCHAR(10)),
                   TRBFILE.TB_PROCEEDS,
                   TRBFILE.TB_LNAMT,
                   TRBFILE.TB_FINCHG,
                   TRBFILE.TB_INTCHG,
                   TRBFILE.TB_SERCHG,
                   TRBFILE.TB_EXTCHG,
                   TRBFILE.TB_MAINTFEE,
                   TRBFILE.TB_ORGTERM,
                   TRBFILE.TB_INSURED,
                   TRBFILE.TB_INSUNEARN_1,
                   TRBFILE.TB_INSUNEARN_2,
                   TRBFILE.TB_INSUNEARN_3,
                   TRBFILE.TB_INSUNEARN_4,
                   TRBFILE.TB_INSUNEARN_5,
                   TRBFILE.TB_INSUNEARN_6,
                   TRBFILE.TB_INSUNEARN_7,
                   TRBFILE.TB_INSUNEARN_8,
                   TRBFILE.TB_INSUNEARN_9,
                   TRBFILE.TB_INSUNEARN_10,
                   TRBFILE.TB_DLNO,
                   TRBFILE.TB_DLRV,
                   TRBFILE.TB_DLSPRV,
                   TRBFILE.TB_DLDISC,
                   TRBFILE.TB_DLPARV,
                   TRBFILE.TB_DLRECOURSE,
                   TRBFILE.TB_DLPARTRCAMT,
                   TRBFILE.TB_PURBAL,
                   TRBFILE.TB_UNEARN_1,
                   TRBFILE.TB_UNEARN_2,
                   TRBFILE.TB_UNEARN_3,
                   TRBFILE.TB_UNEARN_4,
                   TRBFILE.TB_UNEARN_5,
                   TRBFILE.TB_UNEARN_6,
                   TRBFILE.TB_UNEARN_7,
                   TRBFILE.TB_UNEARN_8,
                   TRBFILE.TB_UNEARN_9,
                   TRBFILE.TB_UNEARN_10,
                   TRBFILE.TB_REF_ACCTNO,
                   TRBFILE.TB_DLRVPRIN_1,
                   TRBFILE.TB_DLRVPRIN_2,
                   TRBFILE.TB_DLRVPRIN_3,
                   TRBFILE.TB_DLRVCHGS_1,
                   TRBFILE.TB_DLRVCHGS_2,
                   TRBFILE.TB_DLRVCHGS_3,
                   TRBFILE.TB_DLPARVPAID,
                   TRBFILE.TB_DLPARVHELD,
                   TRBFILE.TB_USERID,
                   CAST(TRBFILE.TB_POSTDATE
                                                  AS VARCHAR(10)),
                   TRBFILE.TB_POSTTIME,
                   TRBFILE.TB_PLCD,
                   TRBFILE.TB_PLTYPE,
                   TRBFILE.TB_REPOCD,
                   TRBFILE.TB_ACTIONCD2
            INTO :QTRB-REC
            FROM DBO.TRBFILE
            WHERE TRBFILE.TB_BRNO = :QTB-BRNO
              AND TRBFILE.TB_DATE = :QTB-DATE
              AND TRBFILE.TB_CLASS = :QTB-CLASS
              AND TRBFILE.TB_ACCTNO = :QTB-ACCTNO
              AND TRBFILE.TB_LAWCODE = :QTB-LAWCODE
              AND TRBFILE.TB_LINE = :QTB-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRB-FIELDS.

      *-----------------------------------------------------------------
       READ-TRB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.
           INITIALIZE QTRB-REC.

           IF ( TRB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRB1_CURSOR INTO :QTRB-REC
              END-EXEC
           ELSE
              IF ( TRB1-CURSOR-SC-STAT-GOOD )
                 EXEC SQL
                      FETCH TRB1_CURSOR_SC INTO :QTRB-REC
                 END-EXEC
              ELSE
                 MOVE 9        TO SQLCODE
                 MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRB1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRB1-CURSOR-STAT.

           IF ( TRB1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE TRB1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO TRB1-CURSOR-SC-STAT.

