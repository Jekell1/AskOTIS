      * COPYMEMBER: LIBLP/LPTRK1IN
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
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRK1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRK-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRK1-FILE.

           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           ACCEPT TK-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRK-PATH-SQL    TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.
           PERFORM SET-TRK-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRKFILE
             (TRKFILE.TK_BRNO,
              TRKFILE.TK_DATE,
              TRKFILE.TK_CLASS,
              TRKFILE.TK_ACCTNO,
              TRKFILE.TK_LAWCODE,
              TRKFILE.TK_LINE,
              TRKFILE.TK_LTOUCH_DATE,
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
              TRKFILE.TK_LOANDATE,
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
              TRKFILE.TK_POSTDATE,
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
              TRKFILE.TK_EARNED_10)
            VALUES
             (:QTK-BRNO,
              :QTK-DATE,
              :QTK-CLASS,
              :QTK-ACCTNO,
              :QTK-LAWCODE,
              :QTK-LINE,
              :QTK-LTOUCH-DATE,
              :QTK-SSNO-1,
              :QTK-MAKERCD-1,
              :QTK-SSNO-2,
              :QTK-MAKERCD-2,
              :QTK-SSNO-3,
              :QTK-MAKERCD-3,
              :QTK-SSNO-4,
              :QTK-MAKERCD-4,
              :QTK-REF-FRTO,
              :QTK-REFBR,
              :QTK-PURCD,
              :QTK-SRCD,
              :QTK-COLLCD,
              :QTK-SERVACCT,
              :QTK-LOANTYPE,
              :QTK-SUBCLASS,
              :QTK-REPOCD,
              :QTK-PLCD,
              :QTK-PLTYPE,
              :QTK-LOANDATE,
              :QTK-PROCEEDS,
              :QTK-LNAMT,
              :QTK-FINCHG,
              :QTK-INTCHG,
              :QTK-SERCHG,
              :QTK-EXTCHG,
              :QTK-MAINTFEE,
              :QTK-ORGTERM,
              :QTK-INSURED,
              :QTK-INSUNEARN-1,
              :QTK-INSUNEARN-2,
              :QTK-INSUNEARN-3,
              :QTK-INSUNEARN-4,
              :QTK-INSUNEARN-5,
              :QTK-INSUNEARN-6,
              :QTK-INSUNEARN-7,
              :QTK-INSUNEARN-8,
              :QTK-INSUNEARN-9,
              :QTK-INSUNEARN-10,
              :QTK-INSEARNED-1,
              :QTK-INSEARNED-2,
              :QTK-INSEARNED-3,
              :QTK-INSEARNED-4,
              :QTK-INSEARNED-5,
              :QTK-INSEARNED-6,
              :QTK-INSEARNED-7,
              :QTK-INSEARNED-8,
              :QTK-INSEARNED-9,
              :QTK-INSEARNED-10,
              :QTK-DLNO,
              :QTK-DLRV,
              :QTK-DLSPRV,
              :QTK-DLDISC,
              :QTK-DLPARV,
              :QTK-DLRECOURSE,
              :QTK-DLPARTRCAMT,
              :QTK-PURBAL,
              :QTK-UNEARN-1,
              :QTK-UNEARN-2,
              :QTK-UNEARN-3,
              :QTK-UNEARN-4,
              :QTK-UNEARN-5,
              :QTK-UNEARN-6,
              :QTK-UNEARN-7,
              :QTK-UNEARN-8,
              :QTK-UNEARN-9,
              :QTK-UNEARN-10,
              :QTK-PRLNNO,
              :QTK-DLRVPRIN-1,
              :QTK-DLRVPRIN-2,
              :QTK-DLRVPRIN-3,
              :QTK-DLRVCHGS-1,
              :QTK-DLRVCHGS-2,
              :QTK-DLRVCHGS-3,
              :QTK-DLPARVPAID,
              :QTK-DLPARVHELD,
              :QTK-USERID,
              :QTK-POSTDATE,
              :QTK-POSTTIME,
              :QTK-EARNED-1,
              :QTK-EARNED-2,
              :QTK-EARNED-3,
              :QTK-EARNED-4,
              :QTK-EARNED-5,
              :QTK-EARNED-6,
              :QTK-EARNED-7,
              :QTK-EARNED-8,
              :QTK-EARNED-9,
              :QTK-EARNED-10)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRK1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRK1-FILE.

           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           ACCEPT TK-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRK-PATH-SQL    TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.
           PERFORM SET-TRK-FIELDS.

           EXEC SQL
            UPDATE DBO.TRKFILE
             SET
              TRKFILE.TK_BRNO = :QTK-BRNO,
              TRKFILE.TK_DATE =
                 :QTK-DATE,
              TRKFILE.TK_CLASS = :QTK-CLASS,
              TRKFILE.TK_ACCTNO = :QTK-ACCTNO,
              TRKFILE.TK_LAWCODE = :QTK-LAWCODE,
              TRKFILE.TK_LINE = :QTK-LINE,
              TRKFILE.TK_LTOUCH_DATE =
                 :QTK-LTOUCH-DATE,
              TRKFILE.TK_SSNO_1 = :QTK-SSNO-1,
              TRKFILE.TK_MAKERCD_1 = :QTK-MAKERCD-1,
              TRKFILE.TK_SSNO_2 = :QTK-SSNO-2,
              TRKFILE.TK_MAKERCD_2 = :QTK-MAKERCD-2,
              TRKFILE.TK_SSNO_3 = :QTK-SSNO-3,
              TRKFILE.TK_MAKERCD_3 = :QTK-MAKERCD-3,
              TRKFILE.TK_SSNO_4 = :QTK-SSNO-4,
              TRKFILE.TK_MAKERCD_4 = :QTK-MAKERCD-4,
              TRKFILE.TK_REF_FRTO = :QTK-REF-FRTO,
              TRKFILE.TK_REFBR = :QTK-REFBR,
              TRKFILE.TK_PURCD = :QTK-PURCD,
              TRKFILE.TK_SRCD = :QTK-SRCD,
              TRKFILE.TK_COLLCD = :QTK-COLLCD,
              TRKFILE.TK_SERVACCT = :QTK-SERVACCT,
              TRKFILE.TK_LOANTYPE = :QTK-LOANTYPE,
              TRKFILE.TK_SUBCLASS = :QTK-SUBCLASS,
              TRKFILE.TK_REPOCD = :QTK-REPOCD,
              TRKFILE.TK_PLCD = :QTK-PLCD,
              TRKFILE.TK_PLTYPE = :QTK-PLTYPE,
              TRKFILE.TK_LOANDATE =
                 :QTK-LOANDATE,
              TRKFILE.TK_PROCEEDS = :QTK-PROCEEDS,
              TRKFILE.TK_LNAMT = :QTK-LNAMT,
              TRKFILE.TK_FINCHG = :QTK-FINCHG,
              TRKFILE.TK_INTCHG = :QTK-INTCHG,
              TRKFILE.TK_SERCHG = :QTK-SERCHG,
              TRKFILE.TK_EXTCHG = :QTK-EXTCHG,
              TRKFILE.TK_MAINTFEE = :QTK-MAINTFEE,
              TRKFILE.TK_ORGTERM = :QTK-ORGTERM,
              TRKFILE.TK_INSURED = :QTK-INSURED,
              TRKFILE.TK_INSUNEARN_1 = :QTK-INSUNEARN-1,
              TRKFILE.TK_INSUNEARN_2 = :QTK-INSUNEARN-2,
              TRKFILE.TK_INSUNEARN_3 = :QTK-INSUNEARN-3,
              TRKFILE.TK_INSUNEARN_4 = :QTK-INSUNEARN-4,
              TRKFILE.TK_INSUNEARN_5 = :QTK-INSUNEARN-5,
              TRKFILE.TK_INSUNEARN_6 = :QTK-INSUNEARN-6,
              TRKFILE.TK_INSUNEARN_7 = :QTK-INSUNEARN-7,
              TRKFILE.TK_INSUNEARN_8 = :QTK-INSUNEARN-8,
              TRKFILE.TK_INSUNEARN_9 = :QTK-INSUNEARN-9,
              TRKFILE.TK_INSUNEARN_10 = :QTK-INSUNEARN-10,
              TRKFILE.TK_INSEARNED_1 = :QTK-INSEARNED-1,
              TRKFILE.TK_INSEARNED_2 = :QTK-INSEARNED-2,
              TRKFILE.TK_INSEARNED_3 = :QTK-INSEARNED-3,
              TRKFILE.TK_INSEARNED_4 = :QTK-INSEARNED-4,
              TRKFILE.TK_INSEARNED_5 = :QTK-INSEARNED-5,
              TRKFILE.TK_INSEARNED_6 = :QTK-INSEARNED-6,
              TRKFILE.TK_INSEARNED_7 = :QTK-INSEARNED-7,
              TRKFILE.TK_INSEARNED_8 = :QTK-INSEARNED-8,
              TRKFILE.TK_INSEARNED_9 = :QTK-INSEARNED-9,
              TRKFILE.TK_INSEARNED_10 = :QTK-INSEARNED-10,
              TRKFILE.TK_DLNO = :QTK-DLNO,
              TRKFILE.TK_DLRV = :QTK-DLRV,
              TRKFILE.TK_DLSPRV = :QTK-DLSPRV,
              TRKFILE.TK_DLDISC = :QTK-DLDISC,
              TRKFILE.TK_DLPARV = :QTK-DLPARV,
              TRKFILE.TK_DLRECOURSE = :QTK-DLRECOURSE,
              TRKFILE.TK_DLPARTRCAMT = :QTK-DLPARTRCAMT,
              TRKFILE.TK_PURBAL = :QTK-PURBAL,
              TRKFILE.TK_UNEARN_1 = :QTK-UNEARN-1,
              TRKFILE.TK_UNEARN_2 = :QTK-UNEARN-2,
              TRKFILE.TK_UNEARN_3 = :QTK-UNEARN-3,
              TRKFILE.TK_UNEARN_4 = :QTK-UNEARN-4,
              TRKFILE.TK_UNEARN_5 = :QTK-UNEARN-5,
              TRKFILE.TK_UNEARN_6 = :QTK-UNEARN-6,
              TRKFILE.TK_UNEARN_7 = :QTK-UNEARN-7,
              TRKFILE.TK_UNEARN_8 = :QTK-UNEARN-8,
              TRKFILE.TK_UNEARN_9 = :QTK-UNEARN-9,
              TRKFILE.TK_UNEARN_10 = :QTK-UNEARN-10,
              TRKFILE.TK_PRLNNO = :QTK-PRLNNO,
              TRKFILE.TK_DLRVPRIN_1 = :QTK-DLRVPRIN-1,
              TRKFILE.TK_DLRVPRIN_2 = :QTK-DLRVPRIN-2,
              TRKFILE.TK_DLRVPRIN_3 = :QTK-DLRVPRIN-3,
              TRKFILE.TK_DLRVCHGS_1 = :QTK-DLRVCHGS-1,
              TRKFILE.TK_DLRVCHGS_2 = :QTK-DLRVCHGS-2,
              TRKFILE.TK_DLRVCHGS_3 = :QTK-DLRVCHGS-3,
              TRKFILE.TK_DLPARVPAID = :QTK-DLPARVPAID,
              TRKFILE.TK_DLPARVHELD = :QTK-DLPARVHELD,
              TRKFILE.TK_USERID = :QTK-USERID,
              TRKFILE.TK_POSTDATE = :QTK-POSTDATE,
              TRKFILE.TK_POSTTIME = :QTK-POSTTIME,
              TRKFILE.TK_EARNED_1 = :QTK-EARNED-1,
              TRKFILE.TK_EARNED_2 = :QTK-EARNED-2,
              TRKFILE.TK_EARNED_3 = :QTK-EARNED-3,
              TRKFILE.TK_EARNED_4 = :QTK-EARNED-4,
              TRKFILE.TK_EARNED_5 = :QTK-EARNED-5,
              TRKFILE.TK_EARNED_6 = :QTK-EARNED-6,
              TRKFILE.TK_EARNED_7 = :QTK-EARNED-7,
              TRKFILE.TK_EARNED_8 = :QTK-EARNED-8,
              TRKFILE.TK_EARNED_9 = :QTK-EARNED-9,
              TRKFILE.TK_EARNED_10 = :QTK-EARNED-10
            WHERE TRKFILE.TK_BRNO = :QTK-BRNO
              AND TRKFILE.TK_DATE = :QTK-DATE
              AND TRKFILE.TK_CLASS = :QTK-CLASS
              AND TRKFILE.TK_ACCTNO = :QTK-ACCTNO
              AND TRKFILE.TK_LAWCODE = :QTK-LAWCODE
              AND TRKFILE.TK_LINE = :QTK-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRK1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRK1-FILE.

           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           PERFORM DELETE-IT.
           MOVE TRK-PATH-SQL    TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.
           PERFORM SET-TRK-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRKFILE
            WHERE TRKFILE.TK_BRNO = :QTK-BRNO
              AND TRKFILE.TK_DATE = :QTK-DATE
              AND TRKFILE.TK_CLASS = :QTK-CLASS
              AND TRKFILE.TK_ACCTNO = :QTK-ACCTNO
              AND TRKFILE.TK_LAWCODE = :QTK-LAWCODE
              AND TRKFILE.TK_LINE = :QTK-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRK1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRK1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRK1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRK1-CURSOR-STAT.

