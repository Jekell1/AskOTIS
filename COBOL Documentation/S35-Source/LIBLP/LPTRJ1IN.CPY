      * COPYMEMBER: LIBLP/LPTRJ1IN
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
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRJ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRJ-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRJ1-FILE.

           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           ACCEPT TJ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRJ-PATH-SQL    TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.
           PERFORM SET-TRJ-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRJFILE
             (TRJFILE.TJ_BRNO,
              TRJFILE.TJ_DATE,
              TRJFILE.TJ_CLASS,
              TRJFILE.TJ_ACCTNO,
              TRJFILE.TJ_LAWCODE,
              TRJFILE.TJ_LINE,
              TRJFILE.TJ_LTOUCH_DATE,
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
              TRJFILE.TJ_LOANDATE,
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
              TRJFILE.TJ_POSTDATE,
              TRJFILE.TJ_POSTTIME,
              TRJFILE.TJ_PLCD,
              TRJFILE.TJ_PLTYPE,
              TRJFILE.TJ_REPOCD)
            VALUES
             (:QTJ-BRNO,
              :QTJ-DATE,
              :QTJ-CLASS,
              :QTJ-ACCTNO,
              :QTJ-LAWCODE,
              :QTJ-LINE,
              :QTJ-LTOUCH-DATE,
              :QTJ-SSNO-1,
              :QTJ-MAKERCD-1,
              :QTJ-SSNO-2,
              :QTJ-MAKERCD-2,
              :QTJ-SSNO-3,
              :QTJ-MAKERCD-3,
              :QTJ-SSNO-4,
              :QTJ-MAKERCD-4,
              :QTJ-REF-FRLC,
              :QTJ-REF-X,
              :QTJ-REF-TOLC,
              :QTJ-FRLC,
              :QTJ-TOLC,
              :QTJ-PURCD,
              :QTJ-SRCD,
              :QTJ-COLLCD,
              :QTJ-SERVACCT,
              :QTJ-LOANTYPE,
              :QTJ-SUBCLASS,
              :QTJ-LOANDATE,
              :QTJ-PROCEEDS,
              :QTJ-LNAMT,
              :QTJ-FINCHG,
              :QTJ-INTCHG,
              :QTJ-SERCHG,
              :QTJ-EXTCHG,
              :QTJ-MAINTFEE,
              :QTJ-ORGTERM,
              :QTJ-INSURED,
              :QTJ-INSUNEARN-1,
              :QTJ-INSUNEARN-2,
              :QTJ-INSUNEARN-3,
              :QTJ-INSUNEARN-4,
              :QTJ-INSUNEARN-5,
              :QTJ-INSUNEARN-6,
              :QTJ-INSUNEARN-7,
              :QTJ-INSUNEARN-8,
              :QTJ-INSUNEARN-9,
              :QTJ-INSUNEARN-10,
              :QTJ-DLNO,
              :QTJ-DLRV,
              :QTJ-DLSPRV,
              :QTJ-DLDISC,
              :QTJ-DLPARV,
              :QTJ-DLRECOURSE,
              :QTJ-DLPARTRCAMT,
              :QTJ-PURBAL,
              :QTJ-UNEARN-1,
              :QTJ-UNEARN-2,
              :QTJ-UNEARN-3,
              :QTJ-UNEARN-4,
              :QTJ-UNEARN-5,
              :QTJ-UNEARN-6,
              :QTJ-UNEARN-7,
              :QTJ-UNEARN-8,
              :QTJ-UNEARN-9,
              :QTJ-UNEARN-10,
              :QTJ-PRLNNO,
              :QTJ-DLRVPRIN-1,
              :QTJ-DLRVPRIN-2,
              :QTJ-DLRVPRIN-3,
              :QTJ-DLRVCHGS-1,
              :QTJ-DLRVCHGS-2,
              :QTJ-DLRVCHGS-3,
              :QTJ-DLPARVPAID,
              :QTJ-DLPARVHELD,
              :QTJ-USERID,
              :QTJ-POSTDATE,
              :QTJ-POSTTIME,
              :QTJ-PLCD,
              :QTJ-PLTYPE,
              :QTJ-REPOCD)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRJ1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRJ1-FILE.

           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           ACCEPT TJ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRJ-PATH-SQL    TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.
           PERFORM SET-TRJ-FIELDS.

           EXEC SQL
            UPDATE DBO.TRJFILE
             SET
              TRJFILE.TJ_BRNO = :QTJ-BRNO,
              TRJFILE.TJ_DATE =
                 :QTJ-DATE,
              TRJFILE.TJ_CLASS = :QTJ-CLASS,
              TRJFILE.TJ_ACCTNO = :QTJ-ACCTNO,
              TRJFILE.TJ_LAWCODE = :QTJ-LAWCODE,
              TRJFILE.TJ_LINE = :QTJ-LINE,
              TRJFILE.TJ_LTOUCH_DATE =
                 :QTJ-LTOUCH-DATE,
              TRJFILE.TJ_SSNO_1 = :QTJ-SSNO-1,
              TRJFILE.TJ_MAKERCD_1 = :QTJ-MAKERCD-1,
              TRJFILE.TJ_SSNO_2 = :QTJ-SSNO-2,
              TRJFILE.TJ_MAKERCD_2 = :QTJ-MAKERCD-2,
              TRJFILE.TJ_SSNO_3 = :QTJ-SSNO-3,
              TRJFILE.TJ_MAKERCD_3 = :QTJ-MAKERCD-3,
              TRJFILE.TJ_SSNO_4 = :QTJ-SSNO-4,
              TRJFILE.TJ_MAKERCD_4 = :QTJ-MAKERCD-4,
              TRJFILE.TJ_REF_FRLC = :QTJ-REF-FRLC,
              TRJFILE.TJ_REF_X = :QTJ-REF-X,
              TRJFILE.TJ_REF_TOLC = :QTJ-REF-TOLC,
              TRJFILE.TJ_FRLC = :QTJ-FRLC,
              TRJFILE.TJ_TOLC = :QTJ-TOLC,
              TRJFILE.TJ_PURCD = :QTJ-PURCD,
              TRJFILE.TJ_SRCD = :QTJ-SRCD,
              TRJFILE.TJ_COLLCD = :QTJ-COLLCD,
              TRJFILE.TJ_SERVACCT = :QTJ-SERVACCT,
              TRJFILE.TJ_LOANTYPE = :QTJ-LOANTYPE,
              TRJFILE.TJ_SUBCLASS = :QTJ-SUBCLASS,
              TRJFILE.TJ_LOANDATE =
                 :QTJ-LOANDATE,
              TRJFILE.TJ_PROCEEDS = :QTJ-PROCEEDS,
              TRJFILE.TJ_LNAMT = :QTJ-LNAMT,
              TRJFILE.TJ_FINCHG = :QTJ-FINCHG,
              TRJFILE.TJ_INTCHG = :QTJ-INTCHG,
              TRJFILE.TJ_SERCHG = :QTJ-SERCHG,
              TRJFILE.TJ_EXTCHG = :QTJ-EXTCHG,
              TRJFILE.TJ_MAINTFEE = :QTJ-MAINTFEE,
              TRJFILE.TJ_ORGTERM = :QTJ-ORGTERM,
              TRJFILE.TJ_INSURED = :QTJ-INSURED,
              TRJFILE.TJ_INSUNEARN_1 = :QTJ-INSUNEARN-1,
              TRJFILE.TJ_INSUNEARN_2 = :QTJ-INSUNEARN-2,
              TRJFILE.TJ_INSUNEARN_3 = :QTJ-INSUNEARN-3,
              TRJFILE.TJ_INSUNEARN_4 = :QTJ-INSUNEARN-4,
              TRJFILE.TJ_INSUNEARN_5 = :QTJ-INSUNEARN-5,
              TRJFILE.TJ_INSUNEARN_6 = :QTJ-INSUNEARN-6,
              TRJFILE.TJ_INSUNEARN_7 = :QTJ-INSUNEARN-7,
              TRJFILE.TJ_INSUNEARN_8 = :QTJ-INSUNEARN-8,
              TRJFILE.TJ_INSUNEARN_9 = :QTJ-INSUNEARN-9,
              TRJFILE.TJ_INSUNEARN_10 = :QTJ-INSUNEARN-10,
              TRJFILE.TJ_DLNO = :QTJ-DLNO,
              TRJFILE.TJ_DLRV = :QTJ-DLRV,
              TRJFILE.TJ_DLSPRV = :QTJ-DLSPRV,
              TRJFILE.TJ_DLDISC = :QTJ-DLDISC,
              TRJFILE.TJ_DLPARV = :QTJ-DLPARV,
              TRJFILE.TJ_DLRECOURSE = :QTJ-DLRECOURSE,
              TRJFILE.TJ_DLPARTRCAMT = :QTJ-DLPARTRCAMT,
              TRJFILE.TJ_PURBAL = :QTJ-PURBAL,
              TRJFILE.TJ_UNEARN_1 = :QTJ-UNEARN-1,
              TRJFILE.TJ_UNEARN_2 = :QTJ-UNEARN-2,
              TRJFILE.TJ_UNEARN_3 = :QTJ-UNEARN-3,
              TRJFILE.TJ_UNEARN_4 = :QTJ-UNEARN-4,
              TRJFILE.TJ_UNEARN_5 = :QTJ-UNEARN-5,
              TRJFILE.TJ_UNEARN_6 = :QTJ-UNEARN-6,
              TRJFILE.TJ_UNEARN_7 = :QTJ-UNEARN-7,
              TRJFILE.TJ_UNEARN_8 = :QTJ-UNEARN-8,
              TRJFILE.TJ_UNEARN_9 = :QTJ-UNEARN-9,
              TRJFILE.TJ_UNEARN_10 = :QTJ-UNEARN-10,
              TRJFILE.TJ_PRLNNO = :QTJ-PRLNNO,
              TRJFILE.TJ_DLRVPRIN_1 = :QTJ-DLRVPRIN-1,
              TRJFILE.TJ_DLRVPRIN_2 = :QTJ-DLRVPRIN-2,
              TRJFILE.TJ_DLRVPRIN_3 = :QTJ-DLRVPRIN-3,
              TRJFILE.TJ_DLRVCHGS_1 = :QTJ-DLRVCHGS-1,
              TRJFILE.TJ_DLRVCHGS_2 = :QTJ-DLRVCHGS-2,
              TRJFILE.TJ_DLRVCHGS_3 = :QTJ-DLRVCHGS-3,
              TRJFILE.TJ_DLPARVPAID = :QTJ-DLPARVPAID,
              TRJFILE.TJ_DLPARVHELD = :QTJ-DLPARVHELD,
              TRJFILE.TJ_USERID = :QTJ-USERID,
              TRJFILE.TJ_POSTDATE = :QTJ-POSTDATE,
              TRJFILE.TJ_POSTTIME = :QTJ-POSTTIME,
              TRJFILE.TJ_PLCD = :QTJ-PLCD,
              TRJFILE.TJ_PLTYPE = :QTJ-PLTYPE,
              TRJFILE.TJ_REPOCD = :QTJ-REPOCD
            WHERE TRJFILE.TJ_BRNO = :QTJ-BRNO
              AND TRJFILE.TJ_DATE = :QTJ-DATE
              AND TRJFILE.TJ_CLASS = :QTJ-CLASS
              AND TRJFILE.TJ_ACCTNO = :QTJ-ACCTNO
              AND TRJFILE.TJ_LAWCODE = :QTJ-LAWCODE
              AND TRJFILE.TJ_LINE = :QTJ-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRJ1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRJ1-FILE.

           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           PERFORM DELETE-IT.
           MOVE TRJ-PATH-SQL    TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.
           PERFORM SET-TRJ-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRJFILE
            WHERE TRJFILE.TJ_BRNO = :QTJ-BRNO
              AND TRJFILE.TJ_DATE = :QTJ-DATE
              AND TRJFILE.TJ_CLASS = :QTJ-CLASS
              AND TRJFILE.TJ_ACCTNO = :QTJ-ACCTNO
              AND TRJFILE.TJ_LAWCODE = :QTJ-LAWCODE
              AND TRJFILE.TJ_LINE = :QTJ-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRJ1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRJ1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRJ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRJ1-CURSOR-STAT.

