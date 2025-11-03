      * COPYMEMBER: LIBLP/LPTRK1RN
       LOAD-TRK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRK-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRK-ALL-DATA
              MOVE TRK-ALL-PATH             TO TRK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRK-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRK-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRK-ALL-DATA
              MOVE TRK-ALL-PATH             TO TRK-PATH.

      *-----------------------------------------------------------------
       OPEN-TRK1-FILE.
           PERFORM LOAD-TRK1-FILE.
           PERFORM OPEN-IT.
           MOVE TRK-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRK1-FILE.
           PERFORM START-IT.
           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           MOVE TRK-PATH-SQL    TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.

           IF ( TRK1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRK1-FILE.

           IF ( QTRK1-WEND-LINE = ZEROES    ) OR
              ( QTRK1-WEND-LINE NOT NUMERIC )
              MOVE TRK-PATH-OWNBR        TO QTRK1-WBEG-BRNO
              MOVE TRK-PATH-OWNBR        TO QTRK1-WEND-BRNO
              MOVE TK-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRK1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRK1-WEND-DATE
              MOVE TK-CLASS             TO QTRK1-WBEG-CLASS
              MOVE TK-ACCTNO            TO QTRK1-WBEG-ACCTNO
              MOVE TK-LAWCODE           TO QTRK1-WBEG-LAWCODE
              MOVE TK-LINE              TO QTRK1-WBEG-LINE
              MOVE ALL "9"              TO QTRK1-WEND-CLASS
                                           QTRK1-WEND-ACCTNO
                                           QTRK1-WEND-LAWCODE
                                           QTRK1-WEND-LINE.


           EXEC SQL
            DECLARE TRK1_CURSOR CURSOR FOR
            SELECT TRKFILE.TK_BRNO,
                   CAST(TRKFILE.TK_DATE AS VARCHAR(10)),
                   TRKFILE.TK_CLASS,
                   TRKFILE.TK_ACCTNO,
                   TRKFILE.TK_LAWCODE,
                   TRKFILE.TK_LINE,
                   CAST(TRKFILE.TK_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRKFILE.TK_SSNO_1,
                   TRKFILE.TK_MAKERCD_1,
                   TRKFILE.TK_SSNO_2,
                   TRKFILE.TK_MAKERCD_2,
                   TRKFILE.TK_SSNO_3,
                   TRKFILE.TK_MAKERCD_3,
                   TRKFILE.TK_SSNO_4,
                   TRKFILE.TK_MAKERCD_4,
                   TRKFILE.TK_REF_FRTO,
                   TRKFILE.TK_REFBR,
                   TRKFILE.TK_PURCD,
                   TRKFILE.TK_SRCD,
                   TRKFILE.TK_COLLCD,
                   TRKFILE.TK_SERVACCT,
                   TRKFILE.TK_LOANTYPE,
                   TRKFILE.TK_SUBCLASS,
                   TRKFILE.TK_REPOCD,
                   TRKFILE.TK_PLCD,
                   TRKFILE.TK_PLTYPE,
                   CAST(TRKFILE.TK_LOANDATE
                                                  AS VARCHAR(10)),
                   TRKFILE.TK_PROCEEDS,
                   TRKFILE.TK_LNAMT,
                   TRKFILE.TK_FINCHG,
                   TRKFILE.TK_INTCHG,
                   TRKFILE.TK_SERCHG,
                   TRKFILE.TK_EXTCHG,
                   TRKFILE.TK_MAINTFEE,
                   TRKFILE.TK_ORGTERM,
                   TRKFILE.TK_INSURED,
                   TRKFILE.TK_INSUNEARN_1,
                   TRKFILE.TK_INSUNEARN_2,
                   TRKFILE.TK_INSUNEARN_3,
                   TRKFILE.TK_INSUNEARN_4,
                   TRKFILE.TK_INSUNEARN_5,
                   TRKFILE.TK_INSUNEARN_6,
                   TRKFILE.TK_INSUNEARN_7,
                   TRKFILE.TK_INSUNEARN_8,
                   TRKFILE.TK_INSUNEARN_9,
                   TRKFILE.TK_INSUNEARN_10,
                   TRKFILE.TK_INSEARNED_1,
                   TRKFILE.TK_INSEARNED_2,
                   TRKFILE.TK_INSEARNED_3,
                   TRKFILE.TK_INSEARNED_4,
                   TRKFILE.TK_INSEARNED_5,
                   TRKFILE.TK_INSEARNED_6,
                   TRKFILE.TK_INSEARNED_7,
                   TRKFILE.TK_INSEARNED_8,
                   TRKFILE.TK_INSEARNED_9,
                   TRKFILE.TK_INSEARNED_10,
                   TRKFILE.TK_DLNO,
                   TRKFILE.TK_DLRV,
                   TRKFILE.TK_DLSPRV,
                   TRKFILE.TK_DLDISC,
                   TRKFILE.TK_DLPARV,
                   TRKFILE.TK_DLRECOURSE,
                   TRKFILE.TK_DLPARTRCAMT,
                   TRKFILE.TK_PURBAL,
                   TRKFILE.TK_UNEARN_1,
                   TRKFILE.TK_UNEARN_2,
                   TRKFILE.TK_UNEARN_3,
                   TRKFILE.TK_UNEARN_4,
                   TRKFILE.TK_UNEARN_5,
                   TRKFILE.TK_UNEARN_6,
                   TRKFILE.TK_UNEARN_7,
                   TRKFILE.TK_UNEARN_8,
                   TRKFILE.TK_UNEARN_9,
                   TRKFILE.TK_UNEARN_10,
                   TRKFILE.TK_PRLNNO,
                   TRKFILE.TK_DLRVPRIN_1,
                   TRKFILE.TK_DLRVPRIN_2,
                   TRKFILE.TK_DLRVPRIN_3,
                   TRKFILE.TK_DLRVCHGS_1,
                   TRKFILE.TK_DLRVCHGS_2,
                   TRKFILE.TK_DLRVCHGS_3,
                   TRKFILE.TK_DLPARVPAID,
                   TRKFILE.TK_DLPARVHELD,
                   TRKFILE.TK_USERID,
                   CAST(TRKFILE.TK_POSTDATE
                                                  AS VARCHAR(10)),
                   TRKFILE.TK_POSTTIME,
                   TRKFILE.TK_EARNED_1,
                   TRKFILE.TK_EARNED_2,
                   TRKFILE.TK_EARNED_3,
                   TRKFILE.TK_EARNED_4,
                   TRKFILE.TK_EARNED_5,
                   TRKFILE.TK_EARNED_6,
                   TRKFILE.TK_EARNED_7,
                   TRKFILE.TK_EARNED_8,
                   TRKFILE.TK_EARNED_9,
                   TRKFILE.TK_EARNED_10
            FROM DBO.TRKFILE
            WHERE TRKFILE.TK_BRNO  = :QTRK1-WBEG-BRNO
              AND TRKFILE.TK_DATE >= :QTRK1-WBEG-DATE 
              AND TRKFILE.TK_DATE <= :QTRK1-WEND-DATE
              AND TRKFILE.TK_CLASS >= :QTRK1-WBEG-CLASS
              AND TRKFILE.TK_CLASS <= :QTRK1-WEND-CLASS
              AND TRKFILE.TK_ACCTNO >= :QTRK1-WBEG-ACCTNO
              AND TRKFILE.TK_ACCTNO <= :QTRK1-WEND-ACCTNO
              AND TRKFILE.TK_LAWCODE >= :QTRK1-WBEG-LAWCODE
              AND TRKFILE.TK_LAWCODE <= :QTRK1-WEND-LAWCODE
              AND TRKFILE.TK_LINE >= :QTRK1-WBEG-LINE
              AND TRKFILE.TK_LINE <= :QTRK1-WEND-LINE
            ORDER BY TRKFILE.TK_BRNO,
                     TRKFILE.TK_DATE,
                     TRKFILE.TK_CLASS,
                     TRKFILE.TK_ACCTNO,
                     TRKFILE.TK_LAWCODE,
                     TRKFILE.TK_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRK1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRK1-WBEG-BRNO
                          QTRK1-WEND-BRNO
                          QTRK1-WBEG-CLASS
                          QTRK1-WEND-CLASS
                          QTRK1-WBEG-ACCTNO
                          QTRK1-WEND-ACCTNO
                          QTRK1-WBEG-LAWCODE
                          QTRK1-WEND-LAWCODE
                          QTRK1-WBEG-LINE
                          QTRK1-WEND-LINE.
           MOVE SPACES TO QTRK1-WBEG-DATE
                          QTRK1-WEND-DATE.

           MOVE STAT---GOOD TO TRK1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-TRK1-FILE-SCAN.
           PERFORM START-IT.
           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           MOVE TRK-PATH-SQL    TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.

           IF ( TRK1-CURSOR-SC-STAT-GOOD )
              PERFORM CLOSE-TRK1-FILE.

           IF ( QTRK1-WEND-LINE = ZEROES    ) OR
              ( QTRK1-WEND-LINE NOT NUMERIC )
              MOVE TRK-PATH-OWNBR        TO QTRK1-WBEG-BRNO
              MOVE TRK-PATH-OWNBR        TO QTRK1-WEND-BRNO
              MOVE TK-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRK1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRK1-WEND-DATE
              MOVE TK-CLASS             TO QTRK1-WBEG-CLASS
              MOVE TK-ACCTNO            TO QTRK1-WBEG-ACCTNO
              MOVE TK-LAWCODE           TO QTRK1-WBEG-LAWCODE
              MOVE TK-LINE              TO QTRK1-WBEG-LINE
              MOVE ALL "9"              TO QTRK1-WEND-CLASS
                                           QTRK1-WEND-ACCTNO
                                           QTRK1-WEND-LAWCODE
                                           QTRK1-WEND-LINE

      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }        

              MOVE TK-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRK1-WSBEG-DATE  

              IF ( TK-CLASS = 999 )
                 MOVE TK-CLASS          TO QTRK1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRK1-WSBEG-CLASS = TK-CLASS + 1
              END-IF

              IF ( TK-ACCTNO = 99999999 )
                 MOVE TK-ACCTNO         TO QTRK1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRK1-WSBEG-ACCTNO = TK-ACCTNO + 1 
              END-IF

              IF ( TK-LAWCODE = 99 )
                 MOVE TK-LAWCODE        TO QTRK1-WSBEG-LAWCODE
              ELSE
                 COMPUTE QTRK1-WSBEG-LAWCODE = TK-LAWCODE + 1.

           EXEC SQL
            DECLARE TRK1_CURSOR_SC CURSOR FOR
            SELECT TRKFILE.TK_BRNO,
                   CAST(TRKFILE.TK_DATE AS VARCHAR(10)),
                   TRKFILE.TK_CLASS,
                   TRKFILE.TK_ACCTNO,
                   TRKFILE.TK_LAWCODE,
                   TRKFILE.TK_LINE,
                   CAST(TRKFILE.TK_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRKFILE.TK_SSNO_1,
                   TRKFILE.TK_MAKERCD_1,
                   TRKFILE.TK_SSNO_2,
                   TRKFILE.TK_MAKERCD_2,
                   TRKFILE.TK_SSNO_3,
                   TRKFILE.TK_MAKERCD_3,
                   TRKFILE.TK_SSNO_4,
                   TRKFILE.TK_MAKERCD_4,
                   TRKFILE.TK_REF_FRTO, 
                   TRKFILE.TK_REFBR,
                   TRKFILE.TK_PURCD,
                   TRKFILE.TK_SRCD,
                   TRKFILE.TK_COLLCD,
                   TRKFILE.TK_SERVACCT,
                   TRKFILE.TK_LOANTYPE,
                   TRKFILE.TK_SUBCLASS,
                   TRKFILE.TK_REPOCD,
                   TRKFILE.TK_PLCD,
                   TRKFILE.TK_PLTYPE,
                   CAST(TRKFILE.TK_LOANDATE
                                                  AS VARCHAR(10)),
                   TRKFILE.TK_PROCEEDS,
                   TRKFILE.TK_LNAMT,
                   TRKFILE.TK_FINCHG,
                   TRKFILE.TK_INTCHG,
                   TRKFILE.TK_SERCHG,
                   TRKFILE.TK_EXTCHG,
                   TRKFILE.TK_MAINTFEE,
                   TRKFILE.TK_ORGTERM,
                   TRKFILE.TK_INSURED,
                   TRKFILE.TK_INSUNEARN_1,
                   TRKFILE.TK_INSUNEARN_2,
                   TRKFILE.TK_INSUNEARN_3,
                   TRKFILE.TK_INSUNEARN_4,
                   TRKFILE.TK_INSUNEARN_5,
                   TRKFILE.TK_INSUNEARN_6,
                   TRKFILE.TK_INSUNEARN_7,
                   TRKFILE.TK_INSUNEARN_8,
                   TRKFILE.TK_INSUNEARN_9,
                   TRKFILE.TK_INSUNEARN_10,
                   TRKFILE.TK_INSEARNED_1,
                   TRKFILE.TK_INSEARNED_2,
                   TRKFILE.TK_INSEARNED_3,
                   TRKFILE.TK_INSEARNED_4,
                   TRKFILE.TK_INSEARNED_5,
                   TRKFILE.TK_INSEARNED_6,
                   TRKFILE.TK_INSEARNED_7,
                   TRKFILE.TK_INSEARNED_8,
                   TRKFILE.TK_INSEARNED_9,
                   TRKFILE.TK_INSEARNED_10,
                   TRKFILE.TK_DLNO,
                   TRKFILE.TK_DLRV,
                   TRKFILE.TK_DLSPRV,
                   TRKFILE.TK_DLDISC,
                   TRKFILE.TK_DLPARV,
                   TRKFILE.TK_DLRECOURSE,
                   TRKFILE.TK_DLPARTRCAMT,
                   TRKFILE.TK_PURBAL,
                   TRKFILE.TK_UNEARN_1,
                   TRKFILE.TK_UNEARN_2,
                   TRKFILE.TK_UNEARN_3,
                   TRKFILE.TK_UNEARN_4,
                   TRKFILE.TK_UNEARN_5,
                   TRKFILE.TK_UNEARN_6,
                   TRKFILE.TK_UNEARN_7,
                   TRKFILE.TK_UNEARN_8,
                   TRKFILE.TK_UNEARN_9,
                   TRKFILE.TK_UNEARN_10,
                   TRKFILE.TK_PRLNNO,
                   TRKFILE.TK_DLRVPRIN_1,
                   TRKFILE.TK_DLRVPRIN_2,
                   TRKFILE.TK_DLRVPRIN_3,
                   TRKFILE.TK_DLRVCHGS_1,
                   TRKFILE.TK_DLRVCHGS_2,
                   TRKFILE.TK_DLRVCHGS_3,
                   TRKFILE.TK_DLPARVPAID,
                   TRKFILE.TK_DLPARVHELD,
                   TRKFILE.TK_USERID,
                   CAST(TRKFILE.TK_POSTDATE
                                                  AS VARCHAR(10)),
                   TRKFILE.TK_POSTTIME,
                   TRKFILE.TK_EARNED_1,
                   TRKFILE.TK_EARNED_2,
                   TRKFILE.TK_EARNED_3,
                   TRKFILE.TK_EARNED_4,
                   TRKFILE.TK_EARNED_5,
                   TRKFILE.TK_EARNED_6,
                   TRKFILE.TK_EARNED_7,
                   TRKFILE.TK_EARNED_8,
                   TRKFILE.TK_EARNED_9,
                   TRKFILE.TK_EARNED_10
            FROM DBO.TRKFILE
            WHERE (  TRKFILE.TK_BRNO     = :QTRK1-WBEG-BRNO      )
              AND (( TRKFILE.TK_DATE    >= :QTRK1-WBEG-DATE
              AND    TRKFILE.TK_CLASS   >= :QTRK1-WBEG-CLASS
              AND    TRKFILE.TK_ACCTNO  >= :QTRK1-WBEG-ACCTNO
              AND    TRKFILE.TK_LAWCODE >= :QTRK1-WBEG-LAWCODE
              AND    TRKFILE.TK_LINE    >= :QTRK1-WBEG-LINE     )
               OR  ( TRKFILE.TK_DATE    >= :QTRK1-WBEG-DATE
              AND    TRKFILE.TK_CLASS   >= :QTRK1-WBEG-CLASS
              AND    TRKFILE.TK_ACCTNO  >= :QTRK1-WBEG-ACCTNO
              AND    TRKFILE.TK_LAWCODE >= :QTRK1-WSBEG-LAWCODE
              AND    TRKFILE.TK_LINE    >= 0                    )
               OR  ( TRKFILE.TK_DATE    >= :QTRK1-WBEG-DATE
              AND    TRKFILE.TK_CLASS   >= :QTRK1-WBEG-CLASS
              AND    TRKFILE.TK_ACCTNO  >= :QTRK1-WSBEG-ACCTNO
              AND    TRKFILE.TK_LAWCODE >= 0
              AND    TRKFILE.TK_LINE    >= 0                    )
               OR  ( TRKFILE.TK_DATE    >= :QTRK1-WBEG-DATE
              AND    TRKFILE.TK_CLASS   >= :QTRK1-WSBEG-CLASS
              AND    TRKFILE.TK_ACCTNO  >= 0
              AND    TRKFILE.TK_LAWCODE >= 0
              AND    TRKFILE.TK_LINE    >= 0                    )
               OR  ( TRKFILE.TK_DATE    >= :QTRK1-WSBEG-DATE
              AND    TRKFILE.TK_CLASS   >= 0
              AND    TRKFILE.TK_ACCTNO  >= 0
              AND    TRKFILE.TK_LAWCODE >= 0
              AND    TRKFILE.TK_LINE    >= 0                    ))
              AND (  TRKFILE.TK_DATE    <= :QTRK1-WEND-DATE
              AND    TRKFILE.TK_CLASS   <= :QTRK1-WEND-CLASS
              AND    TRKFILE.TK_ACCTNO  <= :QTRK1-WEND-ACCTNO
              AND    TRKFILE.TK_LAWCODE <= :QTRK1-WEND-LAWCODE
              AND    TRKFILE.TK_LINE    <= :QTRK1-WEND-LINE      )
            ORDER BY TRKFILE.TK_BRNO,
                     TRKFILE.TK_DATE,
                     TRKFILE.TK_CLASS,
                     TRKFILE.TK_ACCTNO,
                     TRKFILE.TK_LAWCODE,
                     TRKFILE.TK_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRK1_CURSOR_SC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRK1-WBEG-BRNO 
                          QTRK1-WEND-BRNO
                          QTRK1-WBEG-CLASS
                          QTRK1-WEND-CLASS
                          QTRK1-WBEG-ACCTNO
                          QTRK1-WEND-ACCTNO
                          QTRK1-WBEG-LAWCODE
                          QTRK1-WEND-LAWCODE
                          QTRK1-WBEG-LINE
                          QTRK1-WEND-LINE.
           MOVE SPACES TO QTRK1-WBEG-DATE
                          QTRK1-WEND-DATE.

           MOVE STAT---GOOD TO TRK1-CURSOR-SC-STAT.

      *-----------------------------------------------------------------
       READ-TRK1-FILE.
           PERFORM READ-IT.
           MOVE TRK-PATH-OWNBR  TO TK-BRNO. 
           MOVE TRK-PATH-SQL    TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.
           INITIALIZE QTRK-REC.

           MOVE TK-BRNO             TO QTK-BRNO.
           MOVE TK-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTK-DATE.
           MOVE TK-CLASS            TO QTK-CLASS.
           MOVE TK-ACCTNO           TO QTK-ACCTNO.
           MOVE TK-LAWCODE          TO QTK-LAWCODE.
           MOVE TK-LINE             TO QTK-LINE.


           EXEC SQL
            SELECT TRKFILE.TK_BRNO,
                   CAST(TRKFILE.TK_DATE AS VARCHAR(10)),
                   TRKFILE.TK_CLASS,
                   TRKFILE.TK_ACCTNO,
                   TRKFILE.TK_LAWCODE,
                   TRKFILE.TK_LINE,
                   CAST(TRKFILE.TK_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRKFILE.TK_SSNO_1,
                   TRKFILE.TK_MAKERCD_1,
                   TRKFILE.TK_SSNO_2,
                   TRKFILE.TK_MAKERCD_2,
                   TRKFILE.TK_SSNO_3,
                   TRKFILE.TK_MAKERCD_3,
                   TRKFILE.TK_SSNO_4,
                   TRKFILE.TK_MAKERCD_4,
                   TRKFILE.TK_REF_FRTO,
                   TRKFILE.TK_REFBR,
                   TRKFILE.TK_PURCD,
                   TRKFILE.TK_SRCD,
                   TRKFILE.TK_COLLCD,
                   TRKFILE.TK_SERVACCT,
                   TRKFILE.TK_LOANTYPE,
                   TRKFILE.TK_SUBCLASS,
                   TRKFILE.TK_REPOCD,
                   TRKFILE.TK_PLCD,
                   TRKFILE.TK_PLTYPE,
                   CAST(TRKFILE.TK_LOANDATE
                                                  AS VARCHAR(10)),
                   TRKFILE.TK_PROCEEDS,
                   TRKFILE.TK_LNAMT,
                   TRKFILE.TK_FINCHG,
                   TRKFILE.TK_INTCHG,
                   TRKFILE.TK_SERCHG,
                   TRKFILE.TK_EXTCHG,
                   TRKFILE.TK_MAINTFEE,
                   TRKFILE.TK_ORGTERM,
                   TRKFILE.TK_INSURED,
                   TRKFILE.TK_INSUNEARN_1,
                   TRKFILE.TK_INSUNEARN_2,
                   TRKFILE.TK_INSUNEARN_3,
                   TRKFILE.TK_INSUNEARN_4,
                   TRKFILE.TK_INSUNEARN_5,
                   TRKFILE.TK_INSUNEARN_6,
                   TRKFILE.TK_INSUNEARN_7,
                   TRKFILE.TK_INSUNEARN_8,
                   TRKFILE.TK_INSUNEARN_9,
                   TRKFILE.TK_INSUNEARN_10,
                   TRKFILE.TK_INSEARNED_1,
                   TRKFILE.TK_INSEARNED_2,
                   TRKFILE.TK_INSEARNED_3,
                   TRKFILE.TK_INSEARNED_4,
                   TRKFILE.TK_INSEARNED_5,
                   TRKFILE.TK_INSEARNED_6,
                   TRKFILE.TK_INSEARNED_7,
                   TRKFILE.TK_INSEARNED_8,
                   TRKFILE.TK_INSEARNED_9,
                   TRKFILE.TK_INSEARNED_10,
                   TRKFILE.TK_DLNO,
                   TRKFILE.TK_DLRV,
                   TRKFILE.TK_DLSPRV,
                   TRKFILE.TK_DLDISC,
                   TRKFILE.TK_DLPARV,
                   TRKFILE.TK_DLRECOURSE,
                   TRKFILE.TK_DLPARTRCAMT,
                   TRKFILE.TK_PURBAL,
                   TRKFILE.TK_UNEARN_1,
                   TRKFILE.TK_UNEARN_2,
                   TRKFILE.TK_UNEARN_3,
                   TRKFILE.TK_UNEARN_4,
                   TRKFILE.TK_UNEARN_5,
                   TRKFILE.TK_UNEARN_6,
                   TRKFILE.TK_UNEARN_7,
                   TRKFILE.TK_UNEARN_8,
                   TRKFILE.TK_UNEARN_9,
                   TRKFILE.TK_UNEARN_10,
                   TRKFILE.TK_PRLNNO,
                   TRKFILE.TK_DLRVPRIN_1,
                   TRKFILE.TK_DLRVPRIN_2,
                   TRKFILE.TK_DLRVPRIN_3,
                   TRKFILE.TK_DLRVCHGS_1,
                   TRKFILE.TK_DLRVCHGS_2,
                   TRKFILE.TK_DLRVCHGS_3,
                   TRKFILE.TK_DLPARVPAID,
                   TRKFILE.TK_DLPARVHELD,
                   TRKFILE.TK_USERID,
                   CAST(TRKFILE.TK_POSTDATE
                                                  AS VARCHAR(10)),
                   TRKFILE.TK_POSTTIME,
                   TRKFILE.TK_EARNED_1,
                   TRKFILE.TK_EARNED_2,
                   TRKFILE.TK_EARNED_3,
                   TRKFILE.TK_EARNED_4,
                   TRKFILE.TK_EARNED_5,
                   TRKFILE.TK_EARNED_6,
                   TRKFILE.TK_EARNED_7,
                   TRKFILE.TK_EARNED_8,
                   TRKFILE.TK_EARNED_9,
                   TRKFILE.TK_EARNED_10
            INTO :QTRK-REC
            FROM DBO.TRKFILE
            WHERE TRKFILE.TK_BRNO = :QTK-BRNO
              AND TRKFILE.TK_DATE = :QTK-DATE
              AND TRKFILE.TK_CLASS = :QTK-CLASS
              AND TRKFILE.TK_ACCTNO = :QTK-ACCTNO
              AND TRKFILE.TK_LAWCODE = :QTK-LAWCODE
              AND TRKFILE.TK_LINE = :QTK-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRK1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRK-FIELDS.

      *-----------------------------------------------------------------
       READ-TRK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           MOVE TRK-PATH-SQL    TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.
           INITIALIZE QTRK-REC.

           IF ( TRK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRK1_CURSOR INTO :QTRK-REC
              END-EXEC
           ELSE
              IF ( TRK1-CURSOR-SC-STAT-GOOD )
                 EXEC SQL
                      FETCH TRK1_CURSOR_SC INTO :QTRK-REC
                 END-EXEC
              ELSE
                 MOVE 9        TO SQLCODE
                 MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRK1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRK-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRK1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRK1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRK1-CURSOR-STAT.

           IF ( TRK1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE TRK1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO TRK1-CURSOR-SC-STAT.

