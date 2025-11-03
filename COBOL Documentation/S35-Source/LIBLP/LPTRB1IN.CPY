      * COPYMEMBER: LIBLP/LPTRB1IN
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
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRB1-FILE.

           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           ACCEPT TB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.
           PERFORM SET-TRB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRBFILE
             (TRBFILE.TB_BRNO,
              TRBFILE.TB_DATE,
              TRBFILE.TB_CLASS,
              TRBFILE.TB_ACCTNO,
              TRBFILE.TB_LAWCODE,
              TRBFILE.TB_LINE,
              TRBFILE.TB_LTOUCH_DATE,
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
              TRBFILE.TB_LOANDATE,
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
              TRBFILE.TB_POSTDATE,
              TRBFILE.TB_POSTTIME,
              TRBFILE.TB_PLCD,
              TRBFILE.TB_PLTYPE,
              TRBFILE.TB_REPOCD,
              TRBFILE.TB_ACTIONCD2)
            VALUES
             (:QTB-BRNO,
              :QTB-DATE,
              :QTB-CLASS,
              :QTB-ACCTNO,
              :QTB-LAWCODE,
              :QTB-LINE,
              :QTB-LTOUCH-DATE,
              :QTB-SSNO-1,
              :QTB-MAKERCD-1,
              :QTB-SSNO-2,
              :QTB-MAKERCD-2,
              :QTB-SSNO-3,
              :QTB-MAKERCD-3,
              :QTB-SSNO-4,
              :QTB-MAKERCD-4,
              :QTB-REF-FRTO,
              :QTB-REFBR,
              :QTB-PURCD,
              :QTB-SRCD,
              :QTB-COLLCD,
              :QTB-SERVACCT,
              :QTB-LOANTYPE,
              :QTB-SUBCLASS,
              :QTB-LOANDATE,
              :QTB-PROCEEDS,
              :QTB-LNAMT,
              :QTB-FINCHG,
              :QTB-INTCHG,
              :QTB-SERCHG,
              :QTB-EXTCHG,
              :QTB-MAINTFEE,
              :QTB-ORGTERM,
              :QTB-INSURED,
              :QTB-INSUNEARN-1,
              :QTB-INSUNEARN-2,
              :QTB-INSUNEARN-3,
              :QTB-INSUNEARN-4,
              :QTB-INSUNEARN-5,
              :QTB-INSUNEARN-6,
              :QTB-INSUNEARN-7,
              :QTB-INSUNEARN-8,
              :QTB-INSUNEARN-9,
              :QTB-INSUNEARN-10,
              :QTB-DLNO,
              :QTB-DLRV,
              :QTB-DLSPRV,
              :QTB-DLDISC,
              :QTB-DLPARV,
              :QTB-DLRECOURSE,
              :QTB-DLPARTRCAMT,
              :QTB-PURBAL,
              :QTB-UNEARN-1,
              :QTB-UNEARN-2,
              :QTB-UNEARN-3,
              :QTB-UNEARN-4,
              :QTB-UNEARN-5,
              :QTB-UNEARN-6,
              :QTB-UNEARN-7,
              :QTB-UNEARN-8,
              :QTB-UNEARN-9,
              :QTB-UNEARN-10,
              :QTB-REF-ACCTNO,
              :QTB-DLRVPRIN-1,
              :QTB-DLRVPRIN-2,
              :QTB-DLRVPRIN-3,
              :QTB-DLRVCHGS-1,
              :QTB-DLRVCHGS-2,
              :QTB-DLRVCHGS-3,
              :QTB-DLPARVPAID,
              :QTB-DLPARVHELD,
              :QTB-USERID,
              :QTB-POSTDATE,
              :QTB-POSTTIME,
              :QTB-PLCD,
              :QTB-PLTYPE,
              :QTB-REPOCD,
              :QTB-ACTIONCD2)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRB1-FILE.

           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           ACCEPT TB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.
           PERFORM SET-TRB-FIELDS.

           EXEC SQL
            UPDATE DBO.TRBFILE
             SET
              TRBFILE.TB_BRNO = :QTB-BRNO,
              TRBFILE.TB_DATE =
                 :QTB-DATE,
              TRBFILE.TB_CLASS = :QTB-CLASS,
              TRBFILE.TB_ACCTNO = :QTB-ACCTNO,
              TRBFILE.TB_LAWCODE = :QTB-LAWCODE,
              TRBFILE.TB_LINE = :QTB-LINE,
              TRBFILE.TB_LTOUCH_DATE =
                 :QTB-LTOUCH-DATE,
              TRBFILE.TB_SSNO_1 = :QTB-SSNO-1,
              TRBFILE.TB_MAKERCD_1 = :QTB-MAKERCD-1,
              TRBFILE.TB_SSNO_2 = :QTB-SSNO-2,
              TRBFILE.TB_MAKERCD_2 = :QTB-MAKERCD-2,
              TRBFILE.TB_SSNO_3 = :QTB-SSNO-3,
              TRBFILE.TB_MAKERCD_3 = :QTB-MAKERCD-3,
              TRBFILE.TB_SSNO_4 = :QTB-SSNO-4,
              TRBFILE.TB_MAKERCD_4 = :QTB-MAKERCD-4,
              TRBFILE.TB_REF_FRTO = :QTB-REF-FRTO,
              TRBFILE.TB_REFBR = :QTB-REFBR,
              TRBFILE.TB_PURCD = :QTB-PURCD,
              TRBFILE.TB_SRCD = :QTB-SRCD,
              TRBFILE.TB_COLLCD = :QTB-COLLCD,
              TRBFILE.TB_SERVACCT = :QTB-SERVACCT,
              TRBFILE.TB_LOANTYPE = :QTB-LOANTYPE,
              TRBFILE.TB_SUBCLASS = :QTB-SUBCLASS,
              TRBFILE.TB_LOANDATE =
                 :QTB-LOANDATE,
              TRBFILE.TB_PROCEEDS = :QTB-PROCEEDS,
              TRBFILE.TB_LNAMT = :QTB-LNAMT,
              TRBFILE.TB_FINCHG = :QTB-FINCHG,
              TRBFILE.TB_INTCHG = :QTB-INTCHG,
              TRBFILE.TB_SERCHG = :QTB-SERCHG,
              TRBFILE.TB_EXTCHG = :QTB-EXTCHG,
              TRBFILE.TB_MAINTFEE = :QTB-MAINTFEE,
              TRBFILE.TB_ORGTERM = :QTB-ORGTERM,
              TRBFILE.TB_INSURED = :QTB-INSURED,
              TRBFILE.TB_INSUNEARN_1 = :QTB-INSUNEARN-1,
              TRBFILE.TB_INSUNEARN_2 = :QTB-INSUNEARN-2,
              TRBFILE.TB_INSUNEARN_3 = :QTB-INSUNEARN-3,
              TRBFILE.TB_INSUNEARN_4 = :QTB-INSUNEARN-4,
              TRBFILE.TB_INSUNEARN_5 = :QTB-INSUNEARN-5,
              TRBFILE.TB_INSUNEARN_6 = :QTB-INSUNEARN-6,
              TRBFILE.TB_INSUNEARN_7 = :QTB-INSUNEARN-7,
              TRBFILE.TB_INSUNEARN_8 = :QTB-INSUNEARN-8,
              TRBFILE.TB_INSUNEARN_9 = :QTB-INSUNEARN-9,
              TRBFILE.TB_INSUNEARN_10 = :QTB-INSUNEARN-10,
              TRBFILE.TB_DLNO = :QTB-DLNO,
              TRBFILE.TB_DLRV = :QTB-DLRV,
              TRBFILE.TB_DLSPRV = :QTB-DLSPRV,
              TRBFILE.TB_DLDISC = :QTB-DLDISC,
              TRBFILE.TB_DLPARV = :QTB-DLPARV,
              TRBFILE.TB_DLRECOURSE = :QTB-DLRECOURSE,
              TRBFILE.TB_DLPARTRCAMT = :QTB-DLPARTRCAMT,
              TRBFILE.TB_PURBAL = :QTB-PURBAL,
              TRBFILE.TB_UNEARN_1 = :QTB-UNEARN-1,
              TRBFILE.TB_UNEARN_2 = :QTB-UNEARN-2,
              TRBFILE.TB_UNEARN_3 = :QTB-UNEARN-3,
              TRBFILE.TB_UNEARN_4 = :QTB-UNEARN-4,
              TRBFILE.TB_UNEARN_5 = :QTB-UNEARN-5,
              TRBFILE.TB_UNEARN_6 = :QTB-UNEARN-6,
              TRBFILE.TB_UNEARN_7 = :QTB-UNEARN-7,
              TRBFILE.TB_UNEARN_8 = :QTB-UNEARN-8,
              TRBFILE.TB_UNEARN_9 = :QTB-UNEARN-9,
              TRBFILE.TB_UNEARN_10 = :QTB-UNEARN-10,
              TRBFILE.TB_REF_ACCTNO = :QTB-REF-ACCTNO,
              TRBFILE.TB_DLRVPRIN_1 = :QTB-DLRVPRIN-1,
              TRBFILE.TB_DLRVPRIN_2 = :QTB-DLRVPRIN-2,
              TRBFILE.TB_DLRVPRIN_3 = :QTB-DLRVPRIN-3,
              TRBFILE.TB_DLRVCHGS_1 = :QTB-DLRVCHGS-1,
              TRBFILE.TB_DLRVCHGS_2 = :QTB-DLRVCHGS-2,
              TRBFILE.TB_DLRVCHGS_3 = :QTB-DLRVCHGS-3,
              TRBFILE.TB_DLPARVPAID = :QTB-DLPARVPAID,
              TRBFILE.TB_DLPARVHELD = :QTB-DLPARVHELD,
              TRBFILE.TB_USERID = :QTB-USERID,
              TRBFILE.TB_POSTDATE = :QTB-POSTDATE,
              TRBFILE.TB_POSTTIME = :QTB-POSTTIME,
              TRBFILE.TB_PLCD = :QTB-PLCD,
              TRBFILE.TB_PLTYPE = :QTB-PLTYPE,
              TRBFILE.TB_REPOCD = :QTB-REPOCD,
              TRBFILE.TB_ACTIONCD2 = :QTB-ACTIONCD2
            WHERE TRBFILE.TB_BRNO = :QTB-BRNO
              AND TRBFILE.TB_DATE = :QTB-DATE
              AND TRBFILE.TB_CLASS = :QTB-CLASS
              AND TRBFILE.TB_ACCTNO = :QTB-ACCTNO
              AND TRBFILE.TB_LAWCODE = :QTB-LAWCODE
              AND TRBFILE.TB_LINE = :QTB-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRB1-FILE.

           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           PERFORM DELETE-IT.
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.
           PERFORM SET-TRB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRBFILE
            WHERE TRBFILE.TB_BRNO = :QTB-BRNO
              AND TRBFILE.TB_DATE = :QTB-DATE
              AND TRBFILE.TB_CLASS = :QTB-CLASS
              AND TRBFILE.TB_ACCTNO = :QTB-ACCTNO
              AND TRBFILE.TB_LAWCODE = :QTB-LAWCODE
              AND TRBFILE.TB_LINE = :QTB-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRB1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRB1-CURSOR-STAT.

