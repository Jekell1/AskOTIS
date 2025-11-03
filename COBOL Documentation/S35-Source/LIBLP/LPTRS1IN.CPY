      * COPYMEMBER: LIBLP/LPTR1IN
       LOAD-TRS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRS-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRS-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH.

      *-----------------------------------------------------------------
       OPEN-TRS1-FILE.
           PERFORM LOAD-TRS1-FILE.
           PERFORM OPEN-IT.
           MOVE TRS-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRS1-FILE.
           PERFORM START-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           IF ( TRS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRS1-FILE.

           IF ( QTRS1-WEND-LINE = ZEROES    ) OR
              ( QTRS1-WEND-LINE NOT NUMERIC )
              MOVE TRS-PATH-OWNBR        TO QTRS1-WBEG-BRNO
              MOVE TRS-PATH-OWNBR        TO QTRS1-WEND-BRNO
              MOVE TS-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WEND-DATE
              MOVE TS-CLASS             TO QTRS1-WBEG-CLASS
              MOVE TS-ACCTNO            TO QTRS1-WBEG-ACCTNO
              MOVE TS-LAWCODE           TO QTRS1-WBEG-LAWCODE
              MOVE TS-LINE              TO QTRS1-WBEG-LINE
              MOVE ALL "9"              TO QTRS1-WEND-CLASS
                                           QTRS1-WEND-ACCTNO
                                           QTRS1-WEND-LAWCODE
                                           QTRS1-WEND-LINE.

           EXEC SQL
            DECLARE TRS1_CURSOR CURSOR FOR
            SELECT TRSFILE.TS_BRNO,
                   CAST(TRSFILE.TS_DATE AS VARCHAR(10)),
                   TRSFILE.TS_CLASS,
                   TRSFILE.TS_ACCTNO,
                   TRSFILE.TS_LAWCODE,
                   TRSFILE.TS_LINE,
                   CAST(TRSFILE.TS_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_SSNO_1,
                   TRSFILE.TS_MAKERCD_1,
                   TRSFILE.TS_SSNO_2,
                   TRSFILE.TS_MAKERCD_2,
                   TRSFILE.TS_SSNO_3,
                   TRSFILE.TS_MAKERCD_3,
                   TRSFILE.TS_SSNO_4,
                   TRSFILE.TS_MAKERCD_4,
                   TRSFILE.TS_PURCD,
                   TRSFILE.TS_SRCD,
                   TRSFILE.TS_COLLCD,
                   TRSFILE.TS_SERVACCT,
                   TRSFILE.TS_LOANTYPE,
                   TRSFILE.TS_SUBCLASS,
                   CAST(TRSFILE.TS_LOANDATE
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INTDATE AS VARCHAR(10)),
                   TRSFILE.TS_PROCEEDS,
                   TRSFILE.TS_LNAMT,
                   TRSFILE.TS_FINCHG,
                   TRSFILE.TS_INTCHG,
                   TRSFILE.TS_SERCHG,
                   TRSFILE.TS_EXTCHG,
                   TRSFILE.TS_MAINTFEE,
                   TRSFILE.TS_ORGTERM,
                   TRSFILE.TS_1STPYAMT,
                   TRSFILE.TS_NOREGPY,
                   TRSFILE.TS_REGPYAMT,
                   TRSFILE.TS_LASTPYAMT,
                   TRSFILE.TS_FEECODE_1,
                   TRSFILE.TS_FEEAMT_1,
                   TRSFILE.TS_FEEOURS_1,
                   TRSFILE.TS_FEECODE_2,
                   TRSFILE.TS_FEEAMT_2,
                   TRSFILE.TS_FEEOURS_2,
                   TRSFILE.TS_FEECODE_3,
                   TRSFILE.TS_FEEAMT_3,
                   TRSFILE.TS_FEEOURS_3,
                   TRSFILE.TS_FEECODE_4,
                   TRSFILE.TS_FEEAMT_4,
                   TRSFILE.TS_FEEOURS_4,
                   TRSFILE.TS_FEECODE_5,
                   TRSFILE.TS_FEEAMT_5,
                   TRSFILE.TS_FEEOURS_5,
                   TRSFILE.TS_FEECODE_6,
                   TRSFILE.TS_FEEAMT_6,
                   TRSFILE.TS_FEEOURS_6,
                   TRSFILE.TS_FEECODE_7,
                   TRSFILE.TS_FEEAMT_7,
                   TRSFILE.TS_FEEOURS_7,
                   TRSFILE.TS_FEECODE_8,
                   TRSFILE.TS_FEEAMT_8,
                   TRSFILE.TS_FEEOURS_8,
                   TRSFILE.TS_FEECODE_9,
                   TRSFILE.TS_FEEAMT_9,
                   TRSFILE.TS_FEEOURS_9,
                   TRSFILE.TS_FEECODE_10,
                   TRSFILE.TS_FEEAMT_10,
                   TRSFILE.TS_FEEOURS_10,
                   TRSFILE.TS_FEECODE_11,
                   TRSFILE.TS_FEEAMT_11,
                   TRSFILE.TS_FEEOURS_11,
                   TRSFILE.TS_FEECODE_12,
                   TRSFILE.TS_FEEAMT_12,
                   TRSFILE.TS_FEEOURS_12,
                   TRSFILE.TS_FEECODE_13,
                   TRSFILE.TS_FEEAMT_13,
                   TRSFILE.TS_FEEOURS_13,
                   TRSFILE.TS_FEECODE_14,
                   TRSFILE.TS_FEEAMT_14,
                   TRSFILE.TS_FEEOURS_14,
                   TRSFILE.TS_FEECODE_15,
                   TRSFILE.TS_FEEAMT_15,
                   TRSFILE.TS_FEEOURS_15,
                   TRSFILE.TS_FEECODE_16,
                   TRSFILE.TS_FEEAMT_16,
                   TRSFILE.TS_FEEOURS_16,
                   TRSFILE.TS_FEECODE_17,
                   TRSFILE.TS_FEEAMT_17,
                   TRSFILE.TS_FEEOURS_17,
                   TRSFILE.TS_FEECODE_18,
                   TRSFILE.TS_FEEAMT_18,
                   TRSFILE.TS_FEEOURS_18,
                   TRSFILE.TS_FEECODE_19,
                   TRSFILE.TS_FEEAMT_19,
                   TRSFILE.TS_FEEOURS_19,
                   TRSFILE.TS_FEECODE_20,
                   TRSFILE.TS_FEEAMT_20,
                   TRSFILE.TS_FEEOURS_20,
                   TRSFILE.TS_INSURED,
                   TRSFILE.TS_INSTYPES,
                   TRSFILE.TS_INSELIGIBLE,
                   TRSFILE.TS_INSCOMP_1,
                   TRSFILE.TS_INSOURS_1,
                   CAST(TRSFILE.TS_INSEFF_DATE_1
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_1
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_1,
                   TRSFILE.TS_INSCOMM_1,
                   TRSFILE.TS_INSANTICERN_1,
                   TRSFILE.TS_INSEARNINGS_1,
                   TRSFILE.TS_INSCOMP_2,
                   TRSFILE.TS_INSOURS_2,
                   CAST(TRSFILE.TS_INSEFF_DATE_2
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_2
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_2,
                   TRSFILE.TS_INSCOMM_2,
                   TRSFILE.TS_INSANTICERN_2,
                   TRSFILE.TS_INSEARNINGS_2,
                   TRSFILE.TS_INSCOMP_3,
                   TRSFILE.TS_INSOURS_3,
                   CAST(TRSFILE.TS_INSEFF_DATE_3
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_3
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_3,
                   TRSFILE.TS_INSCOMM_3,
                   TRSFILE.TS_INSANTICERN_3,
                   TRSFILE.TS_INSEARNINGS_3,
                   TRSFILE.TS_INSCOMP_4,
                   TRSFILE.TS_INSOURS_4,
                   CAST(TRSFILE.TS_INSEFF_DATE_4
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_4
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_4,
                   TRSFILE.TS_INSCOMM_4,
                   TRSFILE.TS_INSANTICERN_4,
                   TRSFILE.TS_INSEARNINGS_4,
                   TRSFILE.TS_INSCOMP_5,
                   TRSFILE.TS_INSOURS_5,
                   CAST(TRSFILE.TS_INSEFF_DATE_5
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_5
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_5,
                   TRSFILE.TS_INSCOMM_5,
                   TRSFILE.TS_INSANTICERN_5,
                   TRSFILE.TS_INSEARNINGS_5,
                   TRSFILE.TS_INSCOMP_6,
                   TRSFILE.TS_INSOURS_6,
                   CAST(TRSFILE.TS_INSEFF_DATE_6
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_6
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_6,
                   TRSFILE.TS_INSCOMM_6,
                   TRSFILE.TS_INSANTICERN_6,
                   TRSFILE.TS_INSEARNINGS_6,
                   TRSFILE.TS_INSCOMP_7,
                   TRSFILE.TS_INSOURS_7,
                   CAST(TRSFILE.TS_INSEFF_DATE_7
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_7
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_7,
                   TRSFILE.TS_INSCOMM_7,
                   TRSFILE.TS_INSANTICERN_7,
                   TRSFILE.TS_INSEARNINGS_7,
                   TRSFILE.TS_INSCOMP_8,
                   TRSFILE.TS_INSOURS_8,
                   CAST(TRSFILE.TS_INSEFF_DATE_8
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_8
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_8,
                   TRSFILE.TS_INSCOMM_8,
                   TRSFILE.TS_INSANTICERN_8,
                   TRSFILE.TS_INSEARNINGS_8,
                   TRSFILE.TS_INSCOMP_9,
                   TRSFILE.TS_INSOURS_9,
                   CAST(TRSFILE.TS_INSEFF_DATE_9
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_9
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_9,
                   TRSFILE.TS_INSCOMM_9,
                   TRSFILE.TS_INSANTICERN_9,
                   TRSFILE.TS_INSEARNINGS_9,
                   TRSFILE.TS_INSCOMP_10,
                   TRSFILE.TS_INSOURS_10,
                   CAST(TRSFILE.TS_INSEFF_DATE_10
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_10
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_10,
                   TRSFILE.TS_INSCOMM_10,
                   TRSFILE.TS_INSANTICERN_10,
                   TRSFILE.TS_INSEARNINGS_10,
                   TRSFILE.TS_DLCOMM_PP_CMSI,
                   TRSFILE.TS_DLCOMM_O3_CMSI,
                   TRSFILE.TS_INS_TAX_COUNTY_CD,
                   TRSFILE.TS_INS_TAX_CITY_CD,
                   TRSFILE.TS_TILLNO,
                   TRSFILE.TS_UNWIND_REASON_CODE,
                   TRSFILE.TS_DLNO,
                   TRSFILE.TS_DLRECOURSE,
                   TRSFILE.TS_DLPARTRCAMT,
                   TRSFILE.TS_PURBAL,
                   TRSFILE.TS_DLRVPRIN_1,
                   TRSFILE.TS_DLRVPRIN_2,
                   TRSFILE.TS_DLRVPRIN_3,
                   TRSFILE.TS_DLRVCHGS_1,
                   TRSFILE.TS_DLRVCHGS_2,
                   TRSFILE.TS_DLRVCHGS_3,
                   TRSFILE.TS_DL_RVCHGS_CODE_1,
                   TRSFILE.TS_DL_RVCHGS_CODE_2,
                   TRSFILE.TS_DL_RVCHGS_CODE_3,
                   TRSFILE.TS_DL_PARVHELD_CODE,
                   TRSFILE.TS_DL_PARVPAID_CODE,
                   TRSFILE.TS_DLPARVHELD,
                   TRSFILE.TS_DLPARVPAID,
                   TRSFILE.TS_DLDISC,
                   TRSFILE.TS_ANTICTERM,
                   TRSFILE.TS_ANTICERN_1,
                   TRSFILE.TS_ANTICERN_2,
                   TRSFILE.TS_ANTICERN_3,
                   TRSFILE.TS_ANTICERN_4,
                   TRSFILE.TS_ANTICERN_5,
                   TRSFILE.TS_ANTICERN_6,
                   TRSFILE.TS_ANTICERN_7,
                   TRSFILE.TS_ANTICERN_8,
                   TRSFILE.TS_ANTICERN_9,
                   TRSFILE.TS_ANTICERN_10,
                   TRSFILE.TS_EARNINGS_1,
                   TRSFILE.TS_EARNINGS_2,
                   TRSFILE.TS_EARNINGS_3,
                   TRSFILE.TS_EARNINGS_4,
                   TRSFILE.TS_EARNINGS_5,
                   TRSFILE.TS_EARNINGS_6,
                   TRSFILE.TS_EARNINGS_7,
                   TRSFILE.TS_EARNINGS_8,
                   TRSFILE.TS_EARNINGS_9,
                   TRSFILE.TS_EARNINGS_10,
                   TRSFILE.TS_PRLNNO,
                   TRSFILE.TS_INSPREM_DLADJ,
                   TRSFILE.TS_PRDLDISC_UNEARN,
                   TRSFILE.TS_SP_LNCOST_FRMLA,
                   TRSFILE.TS_DLPAID,
                   TRSFILE.TS_MIP_FG,
                   TRSFILE.TS_SPRCLASS,
                   TRSFILE.TS_O3INS_DLPROC,
                   TRSFILE.TS_USERID,
                   CAST(TRSFILE.TS_POSTDATE
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_POSTTIME,
                   TRSFILE.TS_TAX_AMT,
                   TRSFILE.TS_APPROVED_BY_USER,
                   TRSFILE.TS_APPROVED_BY_CODE,
                   CAST(TRSFILE.TS_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_APPROVED_BY_TIME,
                   TRSFILE.TS_ACTIONCD2
            FROM DBO.TRSFILE
            WHERE TRSFILE.TS_BRNO  = :QTRS1-WBEG-BRNO
              AND TRSFILE.TS_DATE >= :QTRS1-WBEG-DATE 
              AND TRSFILE.TS_DATE <= :QTRS1-WEND-DATE
              AND TRSFILE.TS_CLASS >= :QTRS1-WBEG-CLASS
              AND TRSFILE.TS_CLASS <= :QTRS1-WEND-CLASS
              AND TRSFILE.TS_ACCTNO >= :QTRS1-WBEG-ACCTNO
              AND TRSFILE.TS_ACCTNO <= :QTRS1-WEND-ACCTNO
              AND TRSFILE.TS_LAWCODE >= :QTRS1-WBEG-LAWCODE
              AND TRSFILE.TS_LAWCODE <= :QTRS1-WEND-LAWCODE
              AND TRSFILE.TS_LINE >= :QTRS1-WBEG-LINE
              AND TRSFILE.TS_LINE <= :QTRS1-WEND-LINE
            ORDER BY TRSFILE.TS_BRNO,
                     TRSFILE.TS_DATE,
                     TRSFILE.TS_CLASS,
                     TRSFILE.TS_ACCTNO,
                     TRSFILE.TS_LAWCODE,
                     TRSFILE.TS_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRS1-WBEG-BRNO
                          QTRS1-WEND-BRNO
                          QTRS1-WBEG-CLASS
                          QTRS1-WEND-CLASS
                          QTRS1-WBEG-ACCTNO
                          QTRS1-WEND-ACCTNO
                          QTRS1-WBEG-LAWCODE
                          QTRS1-WEND-LAWCODE
                          QTRS1-WBEG-LINE
                          QTRS1-WEND-LINE.
           MOVE SPACES TO QTRS1-WBEG-DATE
                          QTRS1-WEND-DATE.


           MOVE STAT---GOOD TO TRS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRS1-FILE.
           PERFORM READ-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO. 
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           INITIALIZE QTRS-REC.

           MOVE TS-BRNO             TO QTS-BRNO.
           MOVE TS-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTS-DATE.
           MOVE TS-CLASS            TO QTS-CLASS.
           MOVE TS-ACCTNO           TO QTS-ACCTNO.
           MOVE TS-LAWCODE          TO QTS-LAWCODE.
           MOVE TS-LINE             TO QTS-LINE.


           EXEC SQL
            SELECT TRSFILE.TS_BRNO,
                   CAST(TRSFILE.TS_DATE AS VARCHAR(10)),
                   TRSFILE.TS_CLASS,
                   TRSFILE.TS_ACCTNO,
                   TRSFILE.TS_LAWCODE,
                   TRSFILE.TS_LINE,
                   CAST(TRSFILE.TS_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_SSNO_1,
                   TRSFILE.TS_MAKERCD_1,
                   TRSFILE.TS_SSNO_2,
                   TRSFILE.TS_MAKERCD_2,
                   TRSFILE.TS_SSNO_3,
                   TRSFILE.TS_MAKERCD_3,
                   TRSFILE.TS_SSNO_4,
                   TRSFILE.TS_MAKERCD_4,
                   TRSFILE.TS_PURCD,
                   TRSFILE.TS_SRCD,
                   TRSFILE.TS_COLLCD,
                   TRSFILE.TS_SERVACCT,
                   TRSFILE.TS_LOANTYPE,
                   TRSFILE.TS_SUBCLASS,
                   CAST(TRSFILE.TS_LOANDATE
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INTDATE AS VARCHAR(10)),
                   TRSFILE.TS_PROCEEDS,
                   TRSFILE.TS_LNAMT,
                   TRSFILE.TS_FINCHG,
                   TRSFILE.TS_INTCHG,
                   TRSFILE.TS_SERCHG,
                   TRSFILE.TS_EXTCHG,
                   TRSFILE.TS_MAINTFEE,
                   TRSFILE.TS_ORGTERM,
                   TRSFILE.TS_1STPYAMT,
                   TRSFILE.TS_NOREGPY,
                   TRSFILE.TS_REGPYAMT,
                   TRSFILE.TS_LASTPYAMT,
                   TRSFILE.TS_FEECODE_1,
                   TRSFILE.TS_FEEAMT_1,
                   TRSFILE.TS_FEEOURS_1,
                   TRSFILE.TS_FEECODE_2,
                   TRSFILE.TS_FEEAMT_2,
                   TRSFILE.TS_FEEOURS_2,
                   TRSFILE.TS_FEECODE_3,
                   TRSFILE.TS_FEEAMT_3,
                   TRSFILE.TS_FEEOURS_3,
                   TRSFILE.TS_FEECODE_4,
                   TRSFILE.TS_FEEAMT_4,
                   TRSFILE.TS_FEEOURS_4,
                   TRSFILE.TS_FEECODE_5,
                   TRSFILE.TS_FEEAMT_5,
                   TRSFILE.TS_FEEOURS_5,
                   TRSFILE.TS_FEECODE_6,
                   TRSFILE.TS_FEEAMT_6,
                   TRSFILE.TS_FEEOURS_6,
                   TRSFILE.TS_FEECODE_7,
                   TRSFILE.TS_FEEAMT_7,
                   TRSFILE.TS_FEEOURS_7,
                   TRSFILE.TS_FEECODE_8,
                   TRSFILE.TS_FEEAMT_8,
                   TRSFILE.TS_FEEOURS_8,
                   TRSFILE.TS_FEECODE_9,
                   TRSFILE.TS_FEEAMT_9,
                   TRSFILE.TS_FEEOURS_9,
                   TRSFILE.TS_FEECODE_10,
                   TRSFILE.TS_FEEAMT_10,
                   TRSFILE.TS_FEEOURS_10,
                   TRSFILE.TS_FEECODE_11,
                   TRSFILE.TS_FEEAMT_11,
                   TRSFILE.TS_FEEOURS_11,
                   TRSFILE.TS_FEECODE_12,
                   TRSFILE.TS_FEEAMT_12,
                   TRSFILE.TS_FEEOURS_12,
                   TRSFILE.TS_FEECODE_13,
                   TRSFILE.TS_FEEAMT_13,
                   TRSFILE.TS_FEEOURS_13,
                   TRSFILE.TS_FEECODE_14,
                   TRSFILE.TS_FEEAMT_14,
                   TRSFILE.TS_FEEOURS_14,
                   TRSFILE.TS_FEECODE_15,
                   TRSFILE.TS_FEEAMT_15,
                   TRSFILE.TS_FEEOURS_15,
                   TRSFILE.TS_FEECODE_16,
                   TRSFILE.TS_FEEAMT_16,
                   TRSFILE.TS_FEEOURS_16,
                   TRSFILE.TS_FEECODE_17,
                   TRSFILE.TS_FEEAMT_17,
                   TRSFILE.TS_FEEOURS_17,
                   TRSFILE.TS_FEECODE_18,
                   TRSFILE.TS_FEEAMT_18,
                   TRSFILE.TS_FEEOURS_18,
                   TRSFILE.TS_FEECODE_19,
                   TRSFILE.TS_FEEAMT_19,
                   TRSFILE.TS_FEEOURS_19,
                   TRSFILE.TS_FEECODE_20,
                   TRSFILE.TS_FEEAMT_20,
                   TRSFILE.TS_FEEOURS_20,
                   TRSFILE.TS_INSURED,
                   TRSFILE.TS_INSTYPES,
                   TRSFILE.TS_INSELIGIBLE,
                   TRSFILE.TS_INSCOMP_1,
                   TRSFILE.TS_INSOURS_1,
                   CAST(TRSFILE.TS_INSEFF_DATE_1
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_1
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_1,
                   TRSFILE.TS_INSCOMM_1,
                   TRSFILE.TS_INSANTICERN_1,
                   TRSFILE.TS_INSEARNINGS_1,
                   TRSFILE.TS_INSCOMP_2,
                   TRSFILE.TS_INSOURS_2,
                   CAST(TRSFILE.TS_INSEFF_DATE_2
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_2
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_2,
                   TRSFILE.TS_INSCOMM_2,
                   TRSFILE.TS_INSANTICERN_2,
                   TRSFILE.TS_INSEARNINGS_2,
                   TRSFILE.TS_INSCOMP_3,
                   TRSFILE.TS_INSOURS_3,
                   CAST(TRSFILE.TS_INSEFF_DATE_3
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_3
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_3,
                   TRSFILE.TS_INSCOMM_3,
                   TRSFILE.TS_INSANTICERN_3,
                   TRSFILE.TS_INSEARNINGS_3,
                   TRSFILE.TS_INSCOMP_4,
                   TRSFILE.TS_INSOURS_4,
                   CAST(TRSFILE.TS_INSEFF_DATE_4
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_4
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_4,
                   TRSFILE.TS_INSCOMM_4,
                   TRSFILE.TS_INSANTICERN_4,
                   TRSFILE.TS_INSEARNINGS_4,
                   TRSFILE.TS_INSCOMP_5,
                   TRSFILE.TS_INSOURS_5,
                   CAST(TRSFILE.TS_INSEFF_DATE_5
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_5
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_5,
                   TRSFILE.TS_INSCOMM_5,
                   TRSFILE.TS_INSANTICERN_5,
                   TRSFILE.TS_INSEARNINGS_5,
                   TRSFILE.TS_INSCOMP_6,
                   TRSFILE.TS_INSOURS_6,
                   CAST(TRSFILE.TS_INSEFF_DATE_6
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_6
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_6,
                   TRSFILE.TS_INSCOMM_6,
                   TRSFILE.TS_INSANTICERN_6,
                   TRSFILE.TS_INSEARNINGS_6,
                   TRSFILE.TS_INSCOMP_7,
                   TRSFILE.TS_INSOURS_7,
                   CAST(TRSFILE.TS_INSEFF_DATE_7
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_7
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_7,
                   TRSFILE.TS_INSCOMM_7,
                   TRSFILE.TS_INSANTICERN_7,
                   TRSFILE.TS_INSEARNINGS_7,
                   TRSFILE.TS_INSCOMP_8,
                   TRSFILE.TS_INSOURS_8,
                   CAST(TRSFILE.TS_INSEFF_DATE_8
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_8
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_8,
                   TRSFILE.TS_INSCOMM_8,
                   TRSFILE.TS_INSANTICERN_8,
                   TRSFILE.TS_INSEARNINGS_8,
                   TRSFILE.TS_INSCOMP_9,
                   TRSFILE.TS_INSOURS_9,
                   CAST(TRSFILE.TS_INSEFF_DATE_9
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_9
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_9,
                   TRSFILE.TS_INSCOMM_9,
                   TRSFILE.TS_INSANTICERN_9,
                   TRSFILE.TS_INSEARNINGS_9,
                   TRSFILE.TS_INSCOMP_10,
                   TRSFILE.TS_INSOURS_10,
                   CAST(TRSFILE.TS_INSEFF_DATE_10
                                                  AS VARCHAR(10)),
                   CAST(TRSFILE.TS_INSEXP_DATE_10
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_INSPREM_10,
                   TRSFILE.TS_INSCOMM_10,
                   TRSFILE.TS_INSANTICERN_10,
                   TRSFILE.TS_INSEARNINGS_10,
                   TRSFILE.TS_DLCOMM_PP_CMSI,
                   TRSFILE.TS_DLCOMM_O3_CMSI,
                   TRSFILE.TS_INS_TAX_COUNTY_CD,
                   TRSFILE.TS_INS_TAX_CITY_CD,
                   TRSFILE.TS_TILLNO,
                   TRSFILE.TS_UNWIND_REASON_CODE,
                   TRSFILE.TS_DLNO,
                   TRSFILE.TS_DLRECOURSE,
                   TRSFILE.TS_DLPARTRCAMT,
                   TRSFILE.TS_PURBAL,
                   TRSFILE.TS_DLRVPRIN_1,
                   TRSFILE.TS_DLRVPRIN_2,
                   TRSFILE.TS_DLRVPRIN_3,
                   TRSFILE.TS_DLRVCHGS_1,
                   TRSFILE.TS_DLRVCHGS_2,
                   TRSFILE.TS_DLRVCHGS_3,
                   TRSFILE.TS_DL_RVCHGS_CODE_1,
                   TRSFILE.TS_DL_RVCHGS_CODE_2,
                   TRSFILE.TS_DL_RVCHGS_CODE_3,
                   TRSFILE.TS_DL_PARVHELD_CODE,
                   TRSFILE.TS_DL_PARVPAID_CODE,
                   TRSFILE.TS_DLPARVHELD,
                   TRSFILE.TS_DLPARVPAID,
                   TRSFILE.TS_DLDISC,
                   TRSFILE.TS_ANTICTERM,
                   TRSFILE.TS_ANTICERN_1,
                   TRSFILE.TS_ANTICERN_2,
                   TRSFILE.TS_ANTICERN_3,
                   TRSFILE.TS_ANTICERN_4,
                   TRSFILE.TS_ANTICERN_5,
                   TRSFILE.TS_ANTICERN_6,
                   TRSFILE.TS_ANTICERN_7,
                   TRSFILE.TS_ANTICERN_8,
                   TRSFILE.TS_ANTICERN_9,
                   TRSFILE.TS_ANTICERN_10,
                   TRSFILE.TS_EARNINGS_1,
                   TRSFILE.TS_EARNINGS_2,
                   TRSFILE.TS_EARNINGS_3,
                   TRSFILE.TS_EARNINGS_4,
                   TRSFILE.TS_EARNINGS_5,
                   TRSFILE.TS_EARNINGS_6,
                   TRSFILE.TS_EARNINGS_7,
                   TRSFILE.TS_EARNINGS_8,
                   TRSFILE.TS_EARNINGS_9,
                   TRSFILE.TS_EARNINGS_10,
                   TRSFILE.TS_PRLNNO,
                   TRSFILE.TS_INSPREM_DLADJ,
                   TRSFILE.TS_PRDLDISC_UNEARN,
                   TRSFILE.TS_SP_LNCOST_FRMLA,
                   TRSFILE.TS_DLPAID,
                   TRSFILE.TS_MIP_FG,
                   TRSFILE.TS_SPRCLASS,
                   TRSFILE.TS_O3INS_DLPROC,
                   TRSFILE.TS_USERID,
                   CAST(TRSFILE.TS_POSTDATE
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_POSTTIME,
                   TRSFILE.TS_TAX_AMT,
                   TRSFILE.TS_APPROVED_BY_USER,
                   TRSFILE.TS_APPROVED_BY_CODE,
                   CAST(TRSFILE.TS_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),
                   TRSFILE.TS_APPROVED_BY_TIME,
                   TRSFILE.TS_ACTIONCD2
            INTO :QTRS-REC
            FROM DBO.TRSFILE
            WHERE TRSFILE.TS_BRNO = :QTS-BRNO
              AND TRSFILE.TS_DATE = :QTS-DATE
              AND TRSFILE.TS_CLASS = :QTS-CLASS
              AND TRSFILE.TS_ACCTNO = :QTS-ACCTNO
              AND TRSFILE.TS_LAWCODE = :QTS-LAWCODE
              AND TRSFILE.TS_LINE = :QTS-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRS-FIELDS.

      *-----------------------------------------------------------------
       READ-TRS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           INITIALIZE QTRS-REC.

           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRS1_CURSOR INTO :QTRS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRS-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRS1-FILE.

           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           ACCEPT TS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           PERFORM SET-TRS-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRSFILE
             (TRSFILE.TS_BRNO,
              TRSFILE.TS_DATE,
              TRSFILE.TS_CLASS,
              TRSFILE.TS_ACCTNO,
              TRSFILE.TS_LAWCODE,
              TRSFILE.TS_LINE,
              TRSFILE.TS_LTOUCH_DATE,
              TRSFILE.TS_SSNO_1,
              TRSFILE.TS_MAKERCD_1,
              TRSFILE.TS_SSNO_2,
              TRSFILE.TS_MAKERCD_2,
              TRSFILE.TS_SSNO_3,
              TRSFILE.TS_MAKERCD_3,
              TRSFILE.TS_SSNO_4,
              TRSFILE.TS_MAKERCD_4,
              TRSFILE.TS_PURCD,
              TRSFILE.TS_SRCD,
              TRSFILE.TS_COLLCD,
              TRSFILE.TS_SERVACCT,
              TRSFILE.TS_LOANTYPE,
              TRSFILE.TS_SUBCLASS,
              TRSFILE.TS_LOANDATE,
              TRSFILE.TS_INTDATE,
              TRSFILE.TS_PROCEEDS,
              TRSFILE.TS_LNAMT,
              TRSFILE.TS_FINCHG,
              TRSFILE.TS_INTCHG,
              TRSFILE.TS_SERCHG,
              TRSFILE.TS_EXTCHG,
              TRSFILE.TS_MAINTFEE,
              TRSFILE.TS_ORGTERM,
              TRSFILE.TS_1STPYAMT,
              TRSFILE.TS_NOREGPY,
              TRSFILE.TS_REGPYAMT,
              TRSFILE.TS_LASTPYAMT,
              TRSFILE.TS_FEECODE_1,
              TRSFILE.TS_FEEAMT_1,
              TRSFILE.TS_FEEOURS_1,
              TRSFILE.TS_FEECODE_2,
              TRSFILE.TS_FEEAMT_2,
              TRSFILE.TS_FEEOURS_2,
              TRSFILE.TS_FEECODE_3,
              TRSFILE.TS_FEEAMT_3,
              TRSFILE.TS_FEEOURS_3,
              TRSFILE.TS_FEECODE_4,
              TRSFILE.TS_FEEAMT_4,
              TRSFILE.TS_FEEOURS_4,
              TRSFILE.TS_FEECODE_5,
              TRSFILE.TS_FEEAMT_5,
              TRSFILE.TS_FEEOURS_5,
              TRSFILE.TS_FEECODE_6,
              TRSFILE.TS_FEEAMT_6,
              TRSFILE.TS_FEEOURS_6,
              TRSFILE.TS_FEECODE_7,
              TRSFILE.TS_FEEAMT_7,
              TRSFILE.TS_FEEOURS_7,
              TRSFILE.TS_FEECODE_8,
              TRSFILE.TS_FEEAMT_8,
              TRSFILE.TS_FEEOURS_8,
              TRSFILE.TS_FEECODE_9,
              TRSFILE.TS_FEEAMT_9,
              TRSFILE.TS_FEEOURS_9,
              TRSFILE.TS_FEECODE_10,
              TRSFILE.TS_FEEAMT_10,
              TRSFILE.TS_FEEOURS_10,
              TRSFILE.TS_FEECODE_11,
              TRSFILE.TS_FEEAMT_11,
              TRSFILE.TS_FEEOURS_11,
              TRSFILE.TS_FEECODE_12,
              TRSFILE.TS_FEEAMT_12,
              TRSFILE.TS_FEEOURS_12,
              TRSFILE.TS_FEECODE_13,
              TRSFILE.TS_FEEAMT_13,
              TRSFILE.TS_FEEOURS_13,
              TRSFILE.TS_FEECODE_14,
              TRSFILE.TS_FEEAMT_14,
              TRSFILE.TS_FEEOURS_14,
              TRSFILE.TS_FEECODE_15,
              TRSFILE.TS_FEEAMT_15,
              TRSFILE.TS_FEEOURS_15,
              TRSFILE.TS_FEECODE_16,
              TRSFILE.TS_FEEAMT_16,
              TRSFILE.TS_FEEOURS_16,
              TRSFILE.TS_FEECODE_17,
              TRSFILE.TS_FEEAMT_17,
              TRSFILE.TS_FEEOURS_17,
              TRSFILE.TS_FEECODE_18,
              TRSFILE.TS_FEEAMT_18,
              TRSFILE.TS_FEEOURS_18,
              TRSFILE.TS_FEECODE_19,
              TRSFILE.TS_FEEAMT_19,
              TRSFILE.TS_FEEOURS_19,
              TRSFILE.TS_FEECODE_20,
              TRSFILE.TS_FEEAMT_20,
              TRSFILE.TS_FEEOURS_20,
              TRSFILE.TS_INSURED,
              TRSFILE.TS_INSTYPES,
              TRSFILE.TS_INSELIGIBLE,
              TRSFILE.TS_INSCOMP_1,
              TRSFILE.TS_INSOURS_1,
              TRSFILE.TS_INSEFF_DATE_1,
              TRSFILE.TS_INSEXP_DATE_1,
              TRSFILE.TS_INSPREM_1,
              TRSFILE.TS_INSCOMM_1,
              TRSFILE.TS_INSANTICERN_1,
              TRSFILE.TS_INSEARNINGS_1,
              TRSFILE.TS_INSCOMP_2,
              TRSFILE.TS_INSOURS_2,
              TRSFILE.TS_INSEFF_DATE_2,
              TRSFILE.TS_INSEXP_DATE_2,
              TRSFILE.TS_INSPREM_2,
              TRSFILE.TS_INSCOMM_2,
              TRSFILE.TS_INSANTICERN_2,
              TRSFILE.TS_INSEARNINGS_2,
              TRSFILE.TS_INSCOMP_3,
              TRSFILE.TS_INSOURS_3,
              TRSFILE.TS_INSEFF_DATE_3,
              TRSFILE.TS_INSEXP_DATE_3,
              TRSFILE.TS_INSPREM_3,
              TRSFILE.TS_INSCOMM_3,
              TRSFILE.TS_INSANTICERN_3,
              TRSFILE.TS_INSEARNINGS_3,
              TRSFILE.TS_INSCOMP_4,
              TRSFILE.TS_INSOURS_4,
              TRSFILE.TS_INSEFF_DATE_4,
              TRSFILE.TS_INSEXP_DATE_4,
              TRSFILE.TS_INSPREM_4,
              TRSFILE.TS_INSCOMM_4,
              TRSFILE.TS_INSANTICERN_4,
              TRSFILE.TS_INSEARNINGS_4,
              TRSFILE.TS_INSCOMP_5,
              TRSFILE.TS_INSOURS_5,
              TRSFILE.TS_INSEFF_DATE_5,
              TRSFILE.TS_INSEXP_DATE_5,
              TRSFILE.TS_INSPREM_5,
              TRSFILE.TS_INSCOMM_5,
              TRSFILE.TS_INSANTICERN_5,
              TRSFILE.TS_INSEARNINGS_5,
              TRSFILE.TS_INSCOMP_6,
              TRSFILE.TS_INSOURS_6,
              TRSFILE.TS_INSEFF_DATE_6,
              TRSFILE.TS_INSEXP_DATE_6,
              TRSFILE.TS_INSPREM_6,
              TRSFILE.TS_INSCOMM_6,
              TRSFILE.TS_INSANTICERN_6,
              TRSFILE.TS_INSEARNINGS_6,
              TRSFILE.TS_INSCOMP_7,
              TRSFILE.TS_INSOURS_7,
              TRSFILE.TS_INSEFF_DATE_7,
              TRSFILE.TS_INSEXP_DATE_7,
              TRSFILE.TS_INSPREM_7,
              TRSFILE.TS_INSCOMM_7,
              TRSFILE.TS_INSANTICERN_7,
              TRSFILE.TS_INSEARNINGS_7,
              TRSFILE.TS_INSCOMP_8,
              TRSFILE.TS_INSOURS_8,
              TRSFILE.TS_INSEFF_DATE_8,
              TRSFILE.TS_INSEXP_DATE_8,
              TRSFILE.TS_INSPREM_8,
              TRSFILE.TS_INSCOMM_8,
              TRSFILE.TS_INSANTICERN_8,
              TRSFILE.TS_INSEARNINGS_8,
              TRSFILE.TS_INSCOMP_9,
              TRSFILE.TS_INSOURS_9,
              TRSFILE.TS_INSEFF_DATE_9,
              TRSFILE.TS_INSEXP_DATE_9,
              TRSFILE.TS_INSPREM_9,
              TRSFILE.TS_INSCOMM_9,
              TRSFILE.TS_INSANTICERN_9,
              TRSFILE.TS_INSEARNINGS_9,
              TRSFILE.TS_INSCOMP_10,
              TRSFILE.TS_INSOURS_10,
              TRSFILE.TS_INSEFF_DATE_10,
              TRSFILE.TS_INSEXP_DATE_10,
              TRSFILE.TS_INSPREM_10,
              TRSFILE.TS_INSCOMM_10,
              TRSFILE.TS_INSANTICERN_10,
              TRSFILE.TS_INSEARNINGS_10,
              TRSFILE.TS_DLCOMM_PP_CMSI,
              TRSFILE.TS_DLCOMM_O3_CMSI,
              TRSFILE.TS_INS_TAX_COUNTY_CD,
              TRSFILE.TS_INS_TAX_CITY_CD,
              TRSFILE.TS_TILLNO,
              TRSFILE.TS_UNWIND_REASON_CODE,
              TRSFILE.TS_DLNO,
              TRSFILE.TS_DLRECOURSE,
              TRSFILE.TS_DLPARTRCAMT,
              TRSFILE.TS_PURBAL,
              TRSFILE.TS_DLRVPRIN_1,
              TRSFILE.TS_DLRVPRIN_2,
              TRSFILE.TS_DLRVPRIN_3,
              TRSFILE.TS_DLRVCHGS_1,
              TRSFILE.TS_DLRVCHGS_2,
              TRSFILE.TS_DLRVCHGS_3,
              TRSFILE.TS_DL_RVCHGS_CODE_1,
              TRSFILE.TS_DL_RVCHGS_CODE_2,
              TRSFILE.TS_DL_RVCHGS_CODE_3,
              TRSFILE.TS_DL_PARVHELD_CODE,
              TRSFILE.TS_DL_PARVPAID_CODE,
              TRSFILE.TS_DLPARVHELD,
              TRSFILE.TS_DLPARVPAID,
              TRSFILE.TS_DLDISC,
              TRSFILE.TS_ANTICTERM,
              TRSFILE.TS_ANTICERN_1,
              TRSFILE.TS_ANTICERN_2,
              TRSFILE.TS_ANTICERN_3,
              TRSFILE.TS_ANTICERN_4,
              TRSFILE.TS_ANTICERN_5,
              TRSFILE.TS_ANTICERN_6,
              TRSFILE.TS_ANTICERN_7,
              TRSFILE.TS_ANTICERN_8,
              TRSFILE.TS_ANTICERN_9,
              TRSFILE.TS_ANTICERN_10,
              TRSFILE.TS_EARNINGS_1,
              TRSFILE.TS_EARNINGS_2,
              TRSFILE.TS_EARNINGS_3,
              TRSFILE.TS_EARNINGS_4,
              TRSFILE.TS_EARNINGS_5,
              TRSFILE.TS_EARNINGS_6,
              TRSFILE.TS_EARNINGS_7,
              TRSFILE.TS_EARNINGS_8,
              TRSFILE.TS_EARNINGS_9,
              TRSFILE.TS_EARNINGS_10,
              TRSFILE.TS_PRLNNO,
              TRSFILE.TS_INSPREM_DLADJ,
              TRSFILE.TS_PRDLDISC_UNEARN,
              TRSFILE.TS_SP_LNCOST_FRMLA,
              TRSFILE.TS_DLPAID,
              TRSFILE.TS_MIP_FG,
              TRSFILE.TS_SPRCLASS,
              TRSFILE.TS_O3INS_DLPROC,
              TRSFILE.TS_USERID,
              TRSFILE.TS_POSTDATE,
              TRSFILE.TS_POSTTIME,
              TRSFILE.TS_TAX_AMT,
              TRSFILE.TS_APPROVED_BY_USER,
              TRSFILE.TS_APPROVED_BY_CODE,
              TRSFILE.TS_APPROVED_BY_DATE,
              TRSFILE.TS_APPROVED_BY_TIME,
              TRSFILE.TS_ACTIONCD2)
            VALUES
             (:QTS-BRNO,
              :QTS-DATE,
              :QTS-CLASS,
              :QTS-ACCTNO,
              :QTS-LAWCODE,
              :QTS-LINE,
              :QTS-LTOUCH-DATE,
              :QTS-SSNO-1,
              :QTS-MAKERCD-1,
              :QTS-SSNO-2,
              :QTS-MAKERCD-2,
              :QTS-SSNO-3,
              :QTS-MAKERCD-3,
              :QTS-SSNO-4,
              :QTS-MAKERCD-4,
              :QTS-PURCD,
              :QTS-SRCD,
              :QTS-COLLCD,
              :QTS-SERVACCT,
              :QTS-LOANTYPE,
              :QTS-SUBCLASS,
              :QTS-LOANDATE,
              :QTS-INTDATE,
              :QTS-PROCEEDS,
              :QTS-LNAMT,
              :QTS-FINCHG,
              :QTS-INTCHG,
              :QTS-SERCHG,
              :QTS-EXTCHG,
              :QTS-MAINTFEE,
              :QTS-ORGTERM,
              :QTS-1STPYAMT,
              :QTS-NOREGPY,
              :QTS-REGPYAMT,
              :QTS-LASTPYAMT,
              :QTS-FEECODE-1,
              :QTS-FEEAMT-1,
              :QTS-FEEOURS-1,
              :QTS-FEECODE-2,
              :QTS-FEEAMT-2,
              :QTS-FEEOURS-2,
              :QTS-FEECODE-3,
              :QTS-FEEAMT-3,
              :QTS-FEEOURS-3,
              :QTS-FEECODE-4,
              :QTS-FEEAMT-4,
              :QTS-FEEOURS-4,
              :QTS-FEECODE-5,
              :QTS-FEEAMT-5,
              :QTS-FEEOURS-5,
              :QTS-FEECODE-6,
              :QTS-FEEAMT-6,
              :QTS-FEEOURS-6,
              :QTS-FEECODE-7,
              :QTS-FEEAMT-7,
              :QTS-FEEOURS-7,
              :QTS-FEECODE-8,
              :QTS-FEEAMT-8,
              :QTS-FEEOURS-8,
              :QTS-FEECODE-9,
              :QTS-FEEAMT-9,
              :QTS-FEEOURS-9,
              :QTS-FEECODE-10,
              :QTS-FEEAMT-10,
              :QTS-FEEOURS-10,
              :QTS-FEECODE-11,
              :QTS-FEEAMT-11,
              :QTS-FEEOURS-11,
              :QTS-FEECODE-12,
              :QTS-FEEAMT-12,
              :QTS-FEEOURS-12,
              :QTS-FEECODE-13,
              :QTS-FEEAMT-13,
              :QTS-FEEOURS-13,
              :QTS-FEECODE-14,
              :QTS-FEEAMT-14,
              :QTS-FEEOURS-14,
              :QTS-FEECODE-15,
              :QTS-FEEAMT-15,
              :QTS-FEEOURS-15,
              :QTS-FEECODE-16,
              :QTS-FEEAMT-16,
              :QTS-FEEOURS-16,
              :QTS-FEECODE-17,
              :QTS-FEEAMT-17,
              :QTS-FEEOURS-17,
              :QTS-FEECODE-18,
              :QTS-FEEAMT-18,
              :QTS-FEEOURS-18,
              :QTS-FEECODE-19,
              :QTS-FEEAMT-19,
              :QTS-FEEOURS-19,
              :QTS-FEECODE-20,
              :QTS-FEEAMT-20,
              :QTS-FEEOURS-20,
              :QTS-INSURED,
              :QTS-INSTYPES,
              :QTS-INSELIGIBLE,
              :QTS-INSCOMP-1,
              :QTS-INSOURS-1,
              :QTS-INSEFF-DATE-1,
              :QTS-INSEXP-DATE-1,
              :QTS-INSPREM-1,
              :QTS-INSCOMM-1,
              :QTS-INSANTICERN-1,
              :QTS-INSEARNINGS-1,
              :QTS-INSCOMP-2,
              :QTS-INSOURS-2,
              :QTS-INSEFF-DATE-2,
              :QTS-INSEXP-DATE-2,
              :QTS-INSPREM-2,
              :QTS-INSCOMM-2,
              :QTS-INSANTICERN-2,
              :QTS-INSEARNINGS-2,
              :QTS-INSCOMP-3,
              :QTS-INSOURS-3,
              :QTS-INSEFF-DATE-3,
              :QTS-INSEXP-DATE-3,
              :QTS-INSPREM-3,
              :QTS-INSCOMM-3,
              :QTS-INSANTICERN-3,
              :QTS-INSEARNINGS-3,
              :QTS-INSCOMP-4,
              :QTS-INSOURS-4,
              :QTS-INSEFF-DATE-4,
              :QTS-INSEXP-DATE-4,
              :QTS-INSPREM-4,
              :QTS-INSCOMM-4,
              :QTS-INSANTICERN-4,
              :QTS-INSEARNINGS-4,
              :QTS-INSCOMP-5,
              :QTS-INSOURS-5,
              :QTS-INSEFF-DATE-5,
              :QTS-INSEXP-DATE-5,
              :QTS-INSPREM-5,
              :QTS-INSCOMM-5,
              :QTS-INSANTICERN-5,
              :QTS-INSEARNINGS-5,
              :QTS-INSCOMP-6,
              :QTS-INSOURS-6,
              :QTS-INSEFF-DATE-6,
              :QTS-INSEXP-DATE-6,
              :QTS-INSPREM-6,
              :QTS-INSCOMM-6,
              :QTS-INSANTICERN-6,
              :QTS-INSEARNINGS-6,
              :QTS-INSCOMP-7,
              :QTS-INSOURS-7,
              :QTS-INSEFF-DATE-7,
              :QTS-INSEXP-DATE-7,
              :QTS-INSPREM-7,
              :QTS-INSCOMM-7,
              :QTS-INSANTICERN-7,
              :QTS-INSEARNINGS-7,
              :QTS-INSCOMP-8,
              :QTS-INSOURS-8,
              :QTS-INSEFF-DATE-8,
              :QTS-INSEXP-DATE-8,
              :QTS-INSPREM-8,
              :QTS-INSCOMM-8,
              :QTS-INSANTICERN-8,
              :QTS-INSEARNINGS-8,
              :QTS-INSCOMP-9,
              :QTS-INSOURS-9,
              :QTS-INSEFF-DATE-9,
              :QTS-INSEXP-DATE-9,
              :QTS-INSPREM-9,
              :QTS-INSCOMM-9,
              :QTS-INSANTICERN-9,
              :QTS-INSEARNINGS-9,
              :QTS-INSCOMP-10,
              :QTS-INSOURS-10,
              :QTS-INSEFF-DATE-10,
              :QTS-INSEXP-DATE-10,
              :QTS-INSPREM-10,
              :QTS-INSCOMM-10,
              :QTS-INSANTICERN-10,
              :QTS-INSEARNINGS-10,
              :QTS-DLCOMM-PP-CMSI,
              :QTS-DLCOMM-O3-CMSI,
              :QTS-INS-TAX-COUNTY-CD,
              :QTS-INS-TAX-CITY-CD,
              :QTS-TILLNO,
              :QTS-UNWIND-REASON-CODE,
              :QTS-DLNO,
              :QTS-DLRECOURSE,
              :QTS-DLPARTRCAMT,
              :QTS-PURBAL,
              :QTS-DLRVPRIN-1,
              :QTS-DLRVPRIN-2,
              :QTS-DLRVPRIN-3,
              :QTS-DLRVCHGS-1,
              :QTS-DLRVCHGS-2,
              :QTS-DLRVCHGS-3,
              :QTS-DL-RVCHGS-CODE-1,
              :QTS-DL-RVCHGS-CODE-2,
              :QTS-DL-RVCHGS-CODE-3,
              :QTS-DL-PARVHELD-CODE,
              :QTS-DL-PARVPAID-CODE,
              :QTS-DLPARVHELD,
              :QTS-DLPARVPAID,
              :QTS-DLDISC,
              :QTS-ANTICTERM,
              :QTS-ANTICERN-1,
              :QTS-ANTICERN-2,
              :QTS-ANTICERN-3,
              :QTS-ANTICERN-4,
              :QTS-ANTICERN-5,
              :QTS-ANTICERN-6,
              :QTS-ANTICERN-7,
              :QTS-ANTICERN-8,
              :QTS-ANTICERN-9,
              :QTS-ANTICERN-10,
              :QTS-EARNINGS-1,
              :QTS-EARNINGS-2,
              :QTS-EARNINGS-3,
              :QTS-EARNINGS-4,
              :QTS-EARNINGS-5,
              :QTS-EARNINGS-6,
              :QTS-EARNINGS-7,
              :QTS-EARNINGS-8,
              :QTS-EARNINGS-9,
              :QTS-EARNINGS-10,
              :QTS-PRLNNO,
              :QTS-INSPREM-DLADJ,
              :QTS-PRDLDISC-UNEARN,
              :QTS-SP-LNCOST-FRMLA,
              :QTS-DLPAID,
              :QTS-MIP-FG,
              :QTS-SPRCLASS,
              :QTS-O3INS-DLPROC,
              :QTS-USERID,
              :QTS-POSTDATE,
              :QTS-POSTTIME,
              :QTS-TAX-AMT,
              :QTS-APPROVED-BY-USER,
              :QTS-APPROVED-BY-CODE,
              :QTS-APPROVED-BY-DATE,
              :QTS-APPROVED-BY-TIME,
              :QTS-ACTIONCD2)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRS1-FILE.

           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           ACCEPT TS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           PERFORM SET-TRS-FIELDS.

           EXEC SQL
            UPDATE DBO.TRSFILE
             SET
              TRSFILE.TS_BRNO = :QTS-BRNO,
              TRSFILE.TS_DATE =
                 :QTS-DATE,
              TRSFILE.TS_CLASS = :QTS-CLASS,
              TRSFILE.TS_ACCTNO = :QTS-ACCTNO,
              TRSFILE.TS_LAWCODE = :QTS-LAWCODE,
              TRSFILE.TS_LINE = :QTS-LINE,
              TRSFILE.TS_LTOUCH_DATE =
                 :QTS-LTOUCH-DATE,
              TRSFILE.TS_SSNO_1 = :QTS-SSNO-1,
              TRSFILE.TS_MAKERCD_1 = :QTS-MAKERCD-1,
              TRSFILE.TS_SSNO_2 = :QTS-SSNO-2,
              TRSFILE.TS_MAKERCD_2 = :QTS-MAKERCD-2,
              TRSFILE.TS_SSNO_3 = :QTS-SSNO-3,
              TRSFILE.TS_MAKERCD_3 = :QTS-MAKERCD-3,
              TRSFILE.TS_SSNO_4 = :QTS-SSNO-4,
              TRSFILE.TS_MAKERCD_4 = :QTS-MAKERCD-4,
              TRSFILE.TS_PURCD = :QTS-PURCD,
              TRSFILE.TS_SRCD = :QTS-SRCD,
              TRSFILE.TS_COLLCD = :QTS-COLLCD,
              TRSFILE.TS_SERVACCT = :QTS-SERVACCT,
              TRSFILE.TS_LOANTYPE = :QTS-LOANTYPE,
              TRSFILE.TS_SUBCLASS = :QTS-SUBCLASS,
              TRSFILE.TS_LOANDATE =
                 :QTS-LOANDATE,
              TRSFILE.TS_INTDATE =
                 :QTS-INTDATE,
              TRSFILE.TS_PROCEEDS = :QTS-PROCEEDS,
              TRSFILE.TS_LNAMT = :QTS-LNAMT,
              TRSFILE.TS_FINCHG = :QTS-FINCHG,
              TRSFILE.TS_INTCHG = :QTS-INTCHG,
              TRSFILE.TS_SERCHG = :QTS-SERCHG,
              TRSFILE.TS_EXTCHG = :QTS-EXTCHG,
              TRSFILE.TS_MAINTFEE = :QTS-MAINTFEE,
              TRSFILE.TS_ORGTERM = :QTS-ORGTERM,
              TRSFILE.TS_1STPYAMT = :QTS-1STPYAMT,
              TRSFILE.TS_NOREGPY = :QTS-NOREGPY,
              TRSFILE.TS_REGPYAMT = :QTS-REGPYAMT,
              TRSFILE.TS_LASTPYAMT = :QTS-LASTPYAMT,
              TRSFILE.TS_FEECODE_1 = :QTS-FEECODE-1,
              TRSFILE.TS_FEEAMT_1 = :QTS-FEEAMT-1,
              TRSFILE.TS_FEEOURS_1 = :QTS-FEEOURS-1,
              TRSFILE.TS_FEECODE_2 = :QTS-FEECODE-2,
              TRSFILE.TS_FEEAMT_2 = :QTS-FEEAMT-2,
              TRSFILE.TS_FEEOURS_2 = :QTS-FEEOURS-2,
              TRSFILE.TS_FEECODE_3 = :QTS-FEECODE-3,
              TRSFILE.TS_FEEAMT_3 = :QTS-FEEAMT-3,
              TRSFILE.TS_FEEOURS_3 = :QTS-FEEOURS-3,
              TRSFILE.TS_FEECODE_4 = :QTS-FEECODE-4,
              TRSFILE.TS_FEEAMT_4 = :QTS-FEEAMT-4,
              TRSFILE.TS_FEEOURS_4 = :QTS-FEEOURS-4,
              TRSFILE.TS_FEECODE_5 = :QTS-FEECODE-5,
              TRSFILE.TS_FEEAMT_5 = :QTS-FEEAMT-5,
              TRSFILE.TS_FEEOURS_5 = :QTS-FEEOURS-5,
              TRSFILE.TS_FEECODE_6 = :QTS-FEECODE-6,
              TRSFILE.TS_FEEAMT_6 = :QTS-FEEAMT-6,
              TRSFILE.TS_FEEOURS_6 = :QTS-FEEOURS-6,
              TRSFILE.TS_FEECODE_7 = :QTS-FEECODE-7,
              TRSFILE.TS_FEEAMT_7 = :QTS-FEEAMT-7,
              TRSFILE.TS_FEEOURS_7 = :QTS-FEEOURS-7,
              TRSFILE.TS_FEECODE_8 = :QTS-FEECODE-8,
              TRSFILE.TS_FEEAMT_8 = :QTS-FEEAMT-8,
              TRSFILE.TS_FEEOURS_8 = :QTS-FEEOURS-8,
              TRSFILE.TS_FEECODE_9 = :QTS-FEECODE-9,
              TRSFILE.TS_FEEAMT_9 = :QTS-FEEAMT-9,
              TRSFILE.TS_FEEOURS_9 = :QTS-FEEOURS-9,
              TRSFILE.TS_FEECODE_10 = :QTS-FEECODE-10,
              TRSFILE.TS_FEEAMT_10 = :QTS-FEEAMT-10,
              TRSFILE.TS_FEEOURS_10 = :QTS-FEEOURS-10,
              TRSFILE.TS_FEECODE_11 = :QTS-FEECODE-11,
              TRSFILE.TS_FEEAMT_11 = :QTS-FEEAMT-11,
              TRSFILE.TS_FEEOURS_11 = :QTS-FEEOURS-11,
              TRSFILE.TS_FEECODE_12 = :QTS-FEECODE-12,
              TRSFILE.TS_FEEAMT_12 = :QTS-FEEAMT-12,
              TRSFILE.TS_FEEOURS_12 = :QTS-FEEOURS-12,
              TRSFILE.TS_FEECODE_13 = :QTS-FEECODE-13,
              TRSFILE.TS_FEEAMT_13 = :QTS-FEEAMT-13,
              TRSFILE.TS_FEEOURS_13 = :QTS-FEEOURS-13,
              TRSFILE.TS_FEECODE_14 = :QTS-FEECODE-14,
              TRSFILE.TS_FEEAMT_14 = :QTS-FEEAMT-14,
              TRSFILE.TS_FEEOURS_14 = :QTS-FEEOURS-14,
              TRSFILE.TS_FEECODE_15 = :QTS-FEECODE-15,
              TRSFILE.TS_FEEAMT_15 = :QTS-FEEAMT-15,
              TRSFILE.TS_FEEOURS_15 = :QTS-FEEOURS-15,
              TRSFILE.TS_FEECODE_16 = :QTS-FEECODE-16,
              TRSFILE.TS_FEEAMT_16 = :QTS-FEEAMT-16,
              TRSFILE.TS_FEEOURS_16 = :QTS-FEEOURS-16,
              TRSFILE.TS_FEECODE_17 = :QTS-FEECODE-17,
              TRSFILE.TS_FEEAMT_17 = :QTS-FEEAMT-17,
              TRSFILE.TS_FEEOURS_17 = :QTS-FEEOURS-17,
              TRSFILE.TS_FEECODE_18 = :QTS-FEECODE-18,
              TRSFILE.TS_FEEAMT_18 = :QTS-FEEAMT-18,
              TRSFILE.TS_FEEOURS_18 = :QTS-FEEOURS-18,
              TRSFILE.TS_FEECODE_19 = :QTS-FEECODE-19,
              TRSFILE.TS_FEEAMT_19 = :QTS-FEEAMT-19,
              TRSFILE.TS_FEEOURS_19 = :QTS-FEEOURS-19,
              TRSFILE.TS_FEECODE_20 = :QTS-FEECODE-20,
              TRSFILE.TS_FEEAMT_20 = :QTS-FEEAMT-20,
              TRSFILE.TS_FEEOURS_20 = :QTS-FEEOURS-20,
              TRSFILE.TS_INSURED = :QTS-INSURED,
              TRSFILE.TS_INSTYPES = :QTS-INSTYPES,
              TRSFILE.TS_INSELIGIBLE = :QTS-INSELIGIBLE,
              TRSFILE.TS_INSCOMP_1 = :QTS-INSCOMP-1,
              TRSFILE.TS_INSOURS_1 = :QTS-INSOURS-1,
              TRSFILE.TS_INSEFF_DATE_1 =
                 :QTS-INSEFF-DATE-1,
              TRSFILE.TS_INSEXP_DATE_1 =
                 :QTS-INSEXP-DATE-1,
              TRSFILE.TS_INSPREM_1 = :QTS-INSPREM-1,
              TRSFILE.TS_INSCOMM_1 = :QTS-INSCOMM-1,
              TRSFILE.TS_INSANTICERN_1 = :QTS-INSANTICERN-1,
              TRSFILE.TS_INSEARNINGS_1 = :QTS-INSEARNINGS-1,
              TRSFILE.TS_INSCOMP_2 = :QTS-INSCOMP-2,
              TRSFILE.TS_INSOURS_2 = :QTS-INSOURS-2,
              TRSFILE.TS_INSEFF_DATE_2 =
                 :QTS-INSEFF-DATE-2,
              TRSFILE.TS_INSEXP_DATE_2 =
                 :QTS-INSEXP-DATE-2,
              TRSFILE.TS_INSPREM_2 = :QTS-INSPREM-2,
              TRSFILE.TS_INSCOMM_2 = :QTS-INSCOMM-2,
              TRSFILE.TS_INSANTICERN_2 = :QTS-INSANTICERN-2,
              TRSFILE.TS_INSEARNINGS_2 = :QTS-INSEARNINGS-2,
              TRSFILE.TS_INSCOMP_3 = :QTS-INSCOMP-3,
              TRSFILE.TS_INSOURS_3 = :QTS-INSOURS-3,
              TRSFILE.TS_INSEFF_DATE_3 =
                 :QTS-INSEFF-DATE-3,
              TRSFILE.TS_INSEXP_DATE_3 =
                 :QTS-INSEXP-DATE-3,
              TRSFILE.TS_INSPREM_3 = :QTS-INSPREM-3,
              TRSFILE.TS_INSCOMM_3 = :QTS-INSCOMM-3,
              TRSFILE.TS_INSANTICERN_3 = :QTS-INSANTICERN-3,
              TRSFILE.TS_INSEARNINGS_3 = :QTS-INSEARNINGS-3,
              TRSFILE.TS_INSCOMP_4 = :QTS-INSCOMP-4,
              TRSFILE.TS_INSOURS_4 = :QTS-INSOURS-4,
              TRSFILE.TS_INSEFF_DATE_4 =
                 :QTS-INSEFF-DATE-4,
              TRSFILE.TS_INSEXP_DATE_4 =
                 :QTS-INSEXP-DATE-4,
              TRSFILE.TS_INSPREM_4 = :QTS-INSPREM-4,
              TRSFILE.TS_INSCOMM_4 = :QTS-INSCOMM-4,
              TRSFILE.TS_INSANTICERN_4 = :QTS-INSANTICERN-4,
              TRSFILE.TS_INSEARNINGS_4 = :QTS-INSEARNINGS-4,
              TRSFILE.TS_INSCOMP_5 = :QTS-INSCOMP-5,
              TRSFILE.TS_INSOURS_5 = :QTS-INSOURS-5,
              TRSFILE.TS_INSEFF_DATE_5 =
                 :QTS-INSEFF-DATE-5,
              TRSFILE.TS_INSEXP_DATE_5 =
                 :QTS-INSEXP-DATE-5,
              TRSFILE.TS_INSPREM_5 = :QTS-INSPREM-5,
              TRSFILE.TS_INSCOMM_5 = :QTS-INSCOMM-5,
              TRSFILE.TS_INSANTICERN_5 = :QTS-INSANTICERN-5,
              TRSFILE.TS_INSEARNINGS_5 = :QTS-INSEARNINGS-5,
              TRSFILE.TS_INSCOMP_6 = :QTS-INSCOMP-6,
              TRSFILE.TS_INSOURS_6 = :QTS-INSOURS-6,
              TRSFILE.TS_INSEFF_DATE_6 =
                 :QTS-INSEFF-DATE-6,
              TRSFILE.TS_INSEXP_DATE_6 =
                 :QTS-INSEXP-DATE-6,
              TRSFILE.TS_INSPREM_6 = :QTS-INSPREM-6,
              TRSFILE.TS_INSCOMM_6 = :QTS-INSCOMM-6,
              TRSFILE.TS_INSANTICERN_6 = :QTS-INSANTICERN-6,
              TRSFILE.TS_INSEARNINGS_6 = :QTS-INSEARNINGS-6,
              TRSFILE.TS_INSCOMP_7 = :QTS-INSCOMP-7,
              TRSFILE.TS_INSOURS_7 = :QTS-INSOURS-7,
              TRSFILE.TS_INSEFF_DATE_7 =
                 :QTS-INSEFF-DATE-7,
              TRSFILE.TS_INSEXP_DATE_7 =
                 :QTS-INSEXP-DATE-7,
              TRSFILE.TS_INSPREM_7 = :QTS-INSPREM-7,
              TRSFILE.TS_INSCOMM_7 = :QTS-INSCOMM-7,
              TRSFILE.TS_INSANTICERN_7 = :QTS-INSANTICERN-7,
              TRSFILE.TS_INSEARNINGS_7 = :QTS-INSEARNINGS-7,
              TRSFILE.TS_INSCOMP_8 = :QTS-INSCOMP-8,
              TRSFILE.TS_INSOURS_8 = :QTS-INSOURS-8,
              TRSFILE.TS_INSEFF_DATE_8 =
                 :QTS-INSEFF-DATE-8,
              TRSFILE.TS_INSEXP_DATE_8 =
                 :QTS-INSEXP-DATE-8,
              TRSFILE.TS_INSPREM_8 = :QTS-INSPREM-8,
              TRSFILE.TS_INSCOMM_8 = :QTS-INSCOMM-8,
              TRSFILE.TS_INSANTICERN_8 = :QTS-INSANTICERN-8,
              TRSFILE.TS_INSEARNINGS_8 = :QTS-INSEARNINGS-8,
              TRSFILE.TS_INSCOMP_9 = :QTS-INSCOMP-9,
              TRSFILE.TS_INSOURS_9 = :QTS-INSOURS-9,
              TRSFILE.TS_INSEFF_DATE_9 =
                 :QTS-INSEFF-DATE-9,
              TRSFILE.TS_INSEXP_DATE_9 =
                 :QTS-INSEXP-DATE-9,
              TRSFILE.TS_INSPREM_9 = :QTS-INSPREM-9,
              TRSFILE.TS_INSCOMM_9 = :QTS-INSCOMM-9,
              TRSFILE.TS_INSANTICERN_9 = :QTS-INSANTICERN-9,
              TRSFILE.TS_INSEARNINGS_9 = :QTS-INSEARNINGS-9,
              TRSFILE.TS_INSCOMP_10 = :QTS-INSCOMP-10,
              TRSFILE.TS_INSOURS_10 = :QTS-INSOURS-10,
              TRSFILE.TS_INSEFF_DATE_10 =
                 :QTS-INSEFF-DATE-10,
              TRSFILE.TS_INSEXP_DATE_10 =
                 :QTS-INSEXP-DATE-10,
              TRSFILE.TS_INSPREM_10 = :QTS-INSPREM-10,
              TRSFILE.TS_INSCOMM_10 = :QTS-INSCOMM-10,
              TRSFILE.TS_INSANTICERN_10 = :QTS-INSANTICERN-10,
              TRSFILE.TS_INSEARNINGS_10 = :QTS-INSEARNINGS-10,
              TRSFILE.TS_DLCOMM_PP_CMSI = :QTS-DLCOMM-PP-CMSI,
              TRSFILE.TS_DLCOMM_O3_CMSI = :QTS-DLCOMM-O3-CMSI,
              TRSFILE.TS_INS_TAX_COUNTY_CD =
                   :QTS-INS-TAX-COUNTY-CD,
              TRSFILE.TS_INS_TAX_CITY_CD = :QTS-INS-TAX-CITY-CD,
              TRSFILE.TS_TILLNO = :QTS-TILLNO,
              TRSFILE.TS_UNWIND_REASON_CODE =
                   :QTS-UNWIND-REASON-CODE,
              TRSFILE.TS_DLNO = :QTS-DLNO,
              TRSFILE.TS_DLRECOURSE = :QTS-DLRECOURSE,
              TRSFILE.TS_DLPARTRCAMT = :QTS-DLPARTRCAMT,
              TRSFILE.TS_PURBAL = :QTS-PURBAL,
              TRSFILE.TS_DLRVPRIN_1 = :QTS-DLRVPRIN-1,
              TRSFILE.TS_DLRVPRIN_2 = :QTS-DLRVPRIN-2,
              TRSFILE.TS_DLRVPRIN_3 = :QTS-DLRVPRIN-3,
              TRSFILE.TS_DLRVCHGS_1 = :QTS-DLRVCHGS-1,
              TRSFILE.TS_DLRVCHGS_2 = :QTS-DLRVCHGS-2,
              TRSFILE.TS_DLRVCHGS_3 = :QTS-DLRVCHGS-3,
              TRSFILE.TS_DL_RVCHGS_CODE_1 = :QTS-DL-RVCHGS-CODE-1,
              TRSFILE.TS_DL_RVCHGS_CODE_2 = :QTS-DL-RVCHGS-CODE-2,
              TRSFILE.TS_DL_RVCHGS_CODE_3 = :QTS-DL-RVCHGS-CODE-3,
              TRSFILE.TS_DL_PARVHELD_CODE = :QTS-DL-PARVHELD-CODE,
              TRSFILE.TS_DL_PARVPAID_CODE = :QTS-DL-PARVPAID-CODE,
              TRSFILE.TS_DLPARVHELD = :QTS-DLPARVHELD,
              TRSFILE.TS_DLPARVPAID = :QTS-DLPARVPAID,
              TRSFILE.TS_DLDISC = :QTS-DLDISC,
              TRSFILE.TS_ANTICTERM = :QTS-ANTICTERM,
              TRSFILE.TS_ANTICERN_1 = :QTS-ANTICERN-1,
              TRSFILE.TS_ANTICERN_2 = :QTS-ANTICERN-2,
              TRSFILE.TS_ANTICERN_3 = :QTS-ANTICERN-3,
              TRSFILE.TS_ANTICERN_4 = :QTS-ANTICERN-4,
              TRSFILE.TS_ANTICERN_5 = :QTS-ANTICERN-5,
              TRSFILE.TS_ANTICERN_6 = :QTS-ANTICERN-6,
              TRSFILE.TS_ANTICERN_7 = :QTS-ANTICERN-7,
              TRSFILE.TS_ANTICERN_8 = :QTS-ANTICERN-8,
              TRSFILE.TS_ANTICERN_9 = :QTS-ANTICERN-9,
              TRSFILE.TS_ANTICERN_10 = :QTS-ANTICERN-10,
              TRSFILE.TS_EARNINGS_1 = :QTS-EARNINGS-1,
              TRSFILE.TS_EARNINGS_2 = :QTS-EARNINGS-2,
              TRSFILE.TS_EARNINGS_3 = :QTS-EARNINGS-3,
              TRSFILE.TS_EARNINGS_4 = :QTS-EARNINGS-4,
              TRSFILE.TS_EARNINGS_5 = :QTS-EARNINGS-5,
              TRSFILE.TS_EARNINGS_6 = :QTS-EARNINGS-6,
              TRSFILE.TS_EARNINGS_7 = :QTS-EARNINGS-7,
              TRSFILE.TS_EARNINGS_8 = :QTS-EARNINGS-8,
              TRSFILE.TS_EARNINGS_9 = :QTS-EARNINGS-9,
              TRSFILE.TS_EARNINGS_10 = :QTS-EARNINGS-10,
              TRSFILE.TS_PRLNNO = :QTS-PRLNNO,
              TRSFILE.TS_INSPREM_DLADJ = :QTS-INSPREM-DLADJ,
              TRSFILE.TS_PRDLDISC_UNEARN = :QTS-PRDLDISC-UNEARN,
              TRSFILE.TS_SP_LNCOST_FRMLA = :QTS-SP-LNCOST-FRMLA,
              TRSFILE.TS_DLPAID = :QTS-DLPAID,
              TRSFILE.TS_MIP_FG = :QTS-MIP-FG,
              TRSFILE.TS_SPRCLASS = :QTS-SPRCLASS,
              TRSFILE.TS_O3INS_DLPROC = :QTS-O3INS-DLPROC,
              TRSFILE.TS_USERID = :QTS-USERID,
              TRSFILE.TS_POSTDATE =
                 :QTS-POSTDATE,
              TRSFILE.TS_POSTTIME = :QTS-POSTTIME,
              TRSFILE.TS_TAX_AMT = :QTS-TAX-AMT,
              TRSFILE.TS_APPROVED_BY_USER = :QTS-APPROVED-BY-USER,
              TRSFILE.TS_APPROVED_BY_CODE = :QTS-APPROVED-BY-CODE,
              TRSFILE.TS_APPROVED_BY_DATE = :QTS-APPROVED-BY-DATE,
              TRSFILE.TS_APPROVED_BY_TIME = :QTS-APPROVED-BY-TIME,
              TRSFILE.TS_ACTIONCD2 = :QTS-ACTIONCD2
            WHERE TRSFILE.TS_BRNO = :QTS-BRNO
              AND TRSFILE.TS_DATE = :QTS-DATE
              AND TRSFILE.TS_CLASS = :QTS-CLASS
              AND TRSFILE.TS_ACCTNO = :QTS-ACCTNO
              AND TRSFILE.TS_LAWCODE = :QTS-LAWCODE
              AND TRSFILE.TS_LINE = :QTS-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRS1-FILE.

           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           PERFORM DELETE-IT.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           PERFORM SET-TRS-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRSFILE
            WHERE TRSFILE.TS_BRNO = :QTS-BRNO
              AND TRSFILE.TS_DATE = :QTS-DATE
              AND TRSFILE.TS_CLASS = :QTS-CLASS
              AND TRSFILE.TS_ACCTNO = :QTS-ACCTNO
              AND TRSFILE.TS_LAWCODE = :QTS-LAWCODE
              AND TRSFILE.TS_LINE = :QTS-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRS1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRS1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRS1-CURSOR-STAT.

