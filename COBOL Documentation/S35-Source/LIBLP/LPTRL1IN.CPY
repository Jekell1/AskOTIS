      * COPYMEMBER: LIBLP/LPTR1IN
       LOAD-TRL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRL-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRL-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH.

      *-----------------------------------------------------------------
       OPEN-TRL1-FILE.
           PERFORM LOAD-TRL1-FILE.
           PERFORM OPEN-IT.
           MOVE TRL-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRL1-FILE.
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

           IF ( QTRL1-WEND-LINE = ZEROES    ) OR
              ( QTRL1-WEND-LINE NOT NUMERIC )
              MOVE TRL-PATH-OWNBR        TO QTRL1-WBEG-BRNO
              MOVE TRL-PATH-OWNBR        TO QTRL1-WEND-BRNO
              MOVE TL-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WEND-DATE
              MOVE TL-CLASS             TO QTRL1-WBEG-CLASS
              MOVE TL-ACCTNO            TO QTRL1-WBEG-ACCTNO
              MOVE TL-LAWCODE           TO QTRL1-WBEG-LAWCODE
              MOVE TL-LINE              TO QTRL1-WBEG-LINE
              MOVE ALL "9"              TO QTRL1-WEND-CLASS
                                           QTRL1-WEND-ACCTNO
                                           QTRL1-WEND-LAWCODE
                                           QTRL1-WEND-LINE.


           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
            SELECT TRLFILE.TL_BRNO,
                   CAST(TRLFILE.TL_DATE AS VARCHAR(10)),
                   TRLFILE.TL_CLASS,
                   TRLFILE.TL_ACCTNO,
                   TRLFILE.TL_LAWCODE,
                   TRLFILE.TL_LINE,
                   CAST(TRLFILE.TL_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_SSNO_1,
                   TRLFILE.TL_MAKERCD_1,
                   TRLFILE.TL_SSNO_2,
                   TRLFILE.TL_MAKERCD_2,
                   TRLFILE.TL_SSNO_3,
                   TRLFILE.TL_MAKERCD_3,
                   TRLFILE.TL_SSNO_4,
                   TRLFILE.TL_MAKERCD_4,
                   TRLFILE.TL_PURCD,
                   TRLFILE.TL_SRCD,
                   TRLFILE.TL_COLLCD,
                   TRLFILE.TL_SERVACCT,
                   TRLFILE.TL_LOANTYPE,
                   TRLFILE.TL_SUBCLASS,
                   CAST(TRLFILE.TL_LOANDATE
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INTDATE AS VARCHAR(10)),
                   TRLFILE.TL_PROCEEDS,
                   TRLFILE.TL_LNAMT,
                   TRLFILE.TL_FINCHG,
                   TRLFILE.TL_INTCHG,
                   TRLFILE.TL_SERCHG,
                   TRLFILE.TL_EXTCHG,
                   TRLFILE.TL_MAINTFEE,
                   TRLFILE.TL_ORGTERM,
                   TRLFILE.TL_1STPYAMT,
                   TRLFILE.TL_NOREGPY,
                   TRLFILE.TL_REGPYAMT,
                   TRLFILE.TL_LASTPYAMT,
                   TRLFILE.TL_PROCASH,
                   TRLFILE.TL_TILLNO,
                   TRLFILE.TL_FEECODE_1,
                   TRLFILE.TL_FEEAMT_1,
                   TRLFILE.TL_FEECODE_2,
                   TRLFILE.TL_FEEAMT_2,
                   TRLFILE.TL_FEECODE_3,
                   TRLFILE.TL_FEEAMT_3,
                   TRLFILE.TL_FEECODE_4,
                   TRLFILE.TL_FEEAMT_4,
                   TRLFILE.TL_FEECODE_5,
                   TRLFILE.TL_FEEAMT_5,
                   TRLFILE.TL_FEECODE_6,
                   TRLFILE.TL_FEEAMT_6,
                   TRLFILE.TL_FEECODE_7,
                   TRLFILE.TL_FEEAMT_7,
                   TRLFILE.TL_FEECODE_8,
                   TRLFILE.TL_FEEAMT_8,
                   TRLFILE.TL_FEECODE_9,
                   TRLFILE.TL_FEEAMT_9,
                   TRLFILE.TL_FEECODE_10,
                   TRLFILE.TL_FEEAMT_10,
                   TRLFILE.TL_FEECODE_11,
                   TRLFILE.TL_FEEAMT_11,
                   TRLFILE.TL_FEECODE_12,
                   TRLFILE.TL_FEEAMT_12,
                   TRLFILE.TL_FEECODE_13,
                   TRLFILE.TL_FEEAMT_13,
                   TRLFILE.TL_FEECODE_14,
                   TRLFILE.TL_FEEAMT_14,
                   TRLFILE.TL_FEECODE_15,
                   TRLFILE.TL_FEEAMT_15,
                   TRLFILE.TL_FEECODE_16,
                   TRLFILE.TL_FEEAMT_16,
                   TRLFILE.TL_FEECODE_17,
                   TRLFILE.TL_FEEAMT_17,
                   TRLFILE.TL_FEECODE_18,
                   TRLFILE.TL_FEEAMT_18,
                   TRLFILE.TL_FEECODE_19,
                   TRLFILE.TL_FEEAMT_19,
                   TRLFILE.TL_FEECODE_20,
                   TRLFILE.TL_FEEAMT_20,
                   TRLFILE.TL_INSURED,
                   TRLFILE.TL_INSTYPES,
                   TRLFILE.TL_INSELIGIBLE,
                   TRLFILE.TL_INSCOMP_1,
                   TRLFILE.TL_INSOURS_1,
                   CAST(TRLFILE.TL_INSEFF_DATE_1
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_1
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_1,
                   TRLFILE.TL_INSCOMM_1,
                   TRLFILE.TL_INSEARNINGS_1,
                   TRLFILE.TL_INSANTICERN_1,
                   TRLFILE.TL_INSCOMP_2,
                   TRLFILE.TL_INSOURS_2,
                   CAST(TRLFILE.TL_INSEFF_DATE_2
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_2
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_2,
                   TRLFILE.TL_INSCOMM_2,
                   TRLFILE.TL_INSEARNINGS_2,
                   TRLFILE.TL_INSANTICERN_2,
                   TRLFILE.TL_INSCOMP_3,
                   TRLFILE.TL_INSOURS_3,
                   CAST(TRLFILE.TL_INSEFF_DATE_3
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_3
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_3,
                   TRLFILE.TL_INSCOMM_3,
                   TRLFILE.TL_INSEARNINGS_3,
                   TRLFILE.TL_INSANTICERN_3,
                   TRLFILE.TL_INSCOMP_4,
                   TRLFILE.TL_INSOURS_4,
                   CAST(TRLFILE.TL_INSEFF_DATE_4
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_4
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_4,
                   TRLFILE.TL_INSCOMM_4,
                   TRLFILE.TL_INSEARNINGS_4,
                   TRLFILE.TL_INSANTICERN_4,
                   TRLFILE.TL_INSCOMP_5,
                   TRLFILE.TL_INSOURS_5,
                   CAST(TRLFILE.TL_INSEFF_DATE_5
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_5
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_5,
                   TRLFILE.TL_INSCOMM_5,
                   TRLFILE.TL_INSEARNINGS_5,
                   TRLFILE.TL_INSANTICERN_5,
                   TRLFILE.TL_INSCOMP_6,
                   TRLFILE.TL_INSOURS_6,
                   CAST(TRLFILE.TL_INSEFF_DATE_6
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_6
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_6,
                   TRLFILE.TL_INSCOMM_6,
                   TRLFILE.TL_INSEARNINGS_6,
                   TRLFILE.TL_INSANTICERN_6,
                   TRLFILE.TL_INSCOMP_7,
                   TRLFILE.TL_INSOURS_7,
                   CAST(TRLFILE.TL_INSEFF_DATE_7
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_7
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_7,
                   TRLFILE.TL_INSCOMM_7,
                   TRLFILE.TL_INSEARNINGS_7,
                   TRLFILE.TL_INSANTICERN_7,
                   TRLFILE.TL_INSCOMP_8,
                   TRLFILE.TL_INSOURS_8,
                   CAST(TRLFILE.TL_INSEFF_DATE_8
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_8
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_8,
                   TRLFILE.TL_INSCOMM_8,
                   TRLFILE.TL_INSEARNINGS_8,
                   TRLFILE.TL_INSANTICERN_8,
                   TRLFILE.TL_INSCOMP_9,
                   TRLFILE.TL_INSOURS_9,
                   CAST(TRLFILE.TL_INSEFF_DATE_9
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_9
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_9,
                   TRLFILE.TL_INSCOMM_9,
                   TRLFILE.TL_INSEARNINGS_9,
                   TRLFILE.TL_INSANTICERN_9,
                   TRLFILE.TL_INSCOMP_10,
                   TRLFILE.TL_INSOURS_10,
                   CAST(TRLFILE.TL_INSEFF_DATE_10
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_10
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_10,
                   TRLFILE.TL_INSCOMM_10,
                   TRLFILE.TL_INSEARNINGS_10,
                   TRLFILE.TL_INSANTICERN_10,
                   TRLFILE.TL_DLNO,
                   TRLFILE.TL_EARNINGS_1,
                   TRLFILE.TL_EARNINGS_2,
                   TRLFILE.TL_EARNINGS_3,
                   TRLFILE.TL_EARNINGS_4,
                   TRLFILE.TL_EARNINGS_5,
                   TRLFILE.TL_EARNINGS_6,
                   TRLFILE.TL_EARNINGS_7,
                   TRLFILE.TL_EARNINGS_8,
                   TRLFILE.TL_EARNINGS_9,
                   TRLFILE.TL_EARNINGS_10,
                   TRLFILE.TL_ANTICERN_1,
                   TRLFILE.TL_ANTICERN_2,
                   TRLFILE.TL_ANTICERN_3,
                   TRLFILE.TL_ANTICERN_4,
                   TRLFILE.TL_ANTICERN_5,
                   TRLFILE.TL_ANTICERN_6,
                   TRLFILE.TL_ANTICERN_7,
                   TRLFILE.TL_ANTICERN_8,
                   TRLFILE.TL_ANTICERN_9,
                   TRLFILE.TL_ANTICERN_10,
                   TRLFILE.TL_PRLNNO,
                   TRLFILE.TL_PRBAL,
                   TRLFILE.TL_PRINTDUE,
                   TRLFILE.TL_PRFINCHGRB,
                   TRLFILE.TL_PRINSRB,
                   TRLFILE.TL_LPEOD5_FG,
                   TRLFILE.TL_STATE_WORKER,
                   TRLFILE.TL_RESTRICTD_ACCTNO,
                   TRLFILE.TL_SPRCLASS,
                   TRLFILE.TL_USERID,
                   CAST(TRLFILE.TL_POSTDATE
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_POSTTIME,
                   TRLFILE.TL_GIFTNET,
                   TRLFILE.TL_APR,
                   TRLFILE.TL_SP_LNCOST_FRMLA,
                   TRLFILE.TL_MIP_FG,
                   TRLFILE.TL_INS_TAX_COUNTY_CD,
                   TRLFILE.TL_INS_TAX_CITY_CD,
                   TRLFILE.TL_PRDLDISC_UNEARN,
                   TRLFILE.TL_POINTS,
                   TRLFILE.TL_TAX_AMT,
                   TRLFILE.TL_PBI_PBD_FG,
                   TRLFILE.TL_APPROVED_BY_USER,
                   TRLFILE.TL_APPROVED_BY_CODE,
                   CAST(TRLFILE.TL_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_APPROVED_BY_TIME,
                   TRLFILE.TL_DEBIT_CARD_AMT,
                   TRLFILE.TL_ACTIONCD2
            FROM DBO.TRLFILE
            WHERE TRLFILE.TL_BRNO  = :QTRL1-WBEG-BRNO
              AND TRLFILE.TL_DATE >= :QTRL1-WBEG-DATE
              AND TRLFILE.TL_DATE <= :QTRL1-WEND-DATE 
              AND TRLFILE.TL_CLASS >= :QTRL1-WBEG-CLASS
              AND TRLFILE.TL_CLASS <= :QTRL1-WEND-CLASS
              AND TRLFILE.TL_ACCTNO >= :QTRL1-WBEG-ACCTNO
              AND TRLFILE.TL_ACCTNO <= :QTRL1-WEND-ACCTNO
              AND TRLFILE.TL_LAWCODE >= :QTRL1-WBEG-LAWCODE
              AND TRLFILE.TL_LAWCODE <= :QTRL1-WEND-LAWCODE
              AND TRLFILE.TL_LINE >= :QTRL1-WBEG-LINE
              AND TRLFILE.TL_LINE <= :QTRL1-WEND-LINE
            ORDER BY TRLFILE.TL_BRNO,
                     TRLFILE.TL_DATE,
                     TRLFILE.TL_CLASS,
                     TRLFILE.TL_ACCTNO,
                     TRLFILE.TL_LAWCODE,
                     TRLFILE.TL_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-BRNO
                          QTRL1-WEND-BRNO
                          QTRL1-WBEG-CLASS
                          QTRL1-WEND-CLASS
                          QTRL1-WBEG-ACCTNO
                          QTRL1-WEND-ACCTNO
                          QTRL1-WBEG-LAWCODE
                          QTRL1-WEND-LAWCODE
                          QTRL1-WBEG-LINE
                          QTRL1-WEND-LINE.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.


           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.


      *-----------------------------------------------------------------
       READ-TRL1-FILE.
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO. 
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC.

           MOVE TL-BRNO             TO QTL-BRNO.
           MOVE TL-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTL-DATE.
           MOVE TL-CLASS            TO QTL-CLASS.
           MOVE TL-ACCTNO           TO QTL-ACCTNO.
           MOVE TL-LAWCODE          TO QTL-LAWCODE.
           MOVE TL-LINE             TO QTL-LINE.

           EXEC SQL
            SELECT TRLFILE.TL_BRNO,
                   CAST(TRLFILE.TL_DATE AS VARCHAR(10)),
                   TRLFILE.TL_CLASS,
                   TRLFILE.TL_ACCTNO,
                   TRLFILE.TL_LAWCODE,
                   TRLFILE.TL_LINE,
                   CAST(TRLFILE.TL_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_SSNO_1,
                   TRLFILE.TL_MAKERCD_1,
                   TRLFILE.TL_SSNO_2,
                   TRLFILE.TL_MAKERCD_2,
                   TRLFILE.TL_SSNO_3,
                   TRLFILE.TL_MAKERCD_3,
                   TRLFILE.TL_SSNO_4,
                   TRLFILE.TL_MAKERCD_4,
                   TRLFILE.TL_PURCD,
                   TRLFILE.TL_SRCD,
                   TRLFILE.TL_COLLCD,
                   TRLFILE.TL_SERVACCT,
                   TRLFILE.TL_LOANTYPE,
                   TRLFILE.TL_SUBCLASS,
                   CAST(TRLFILE.TL_LOANDATE
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INTDATE AS VARCHAR(10)),
                   TRLFILE.TL_PROCEEDS,
                   TRLFILE.TL_LNAMT,
                   TRLFILE.TL_FINCHG,
                   TRLFILE.TL_INTCHG,
                   TRLFILE.TL_SERCHG,
                   TRLFILE.TL_EXTCHG,
                   TRLFILE.TL_MAINTFEE,
                   TRLFILE.TL_ORGTERM,
                   TRLFILE.TL_1STPYAMT,
                   TRLFILE.TL_NOREGPY,
                   TRLFILE.TL_REGPYAMT,
                   TRLFILE.TL_LASTPYAMT,
                   TRLFILE.TL_PROCASH,
                   TRLFILE.TL_TILLNO,
                   TRLFILE.TL_FEECODE_1,
                   TRLFILE.TL_FEEAMT_1,
                   TRLFILE.TL_FEECODE_2,
                   TRLFILE.TL_FEEAMT_2,
                   TRLFILE.TL_FEECODE_3,
                   TRLFILE.TL_FEEAMT_3,
                   TRLFILE.TL_FEECODE_4,
                   TRLFILE.TL_FEEAMT_4,
                   TRLFILE.TL_FEECODE_5,
                   TRLFILE.TL_FEEAMT_5,
                   TRLFILE.TL_FEECODE_6,
                   TRLFILE.TL_FEEAMT_6,
                   TRLFILE.TL_FEECODE_7,
                   TRLFILE.TL_FEEAMT_7,
                   TRLFILE.TL_FEECODE_8,
                   TRLFILE.TL_FEEAMT_8,
                   TRLFILE.TL_FEECODE_9,
                   TRLFILE.TL_FEEAMT_9,
                   TRLFILE.TL_FEECODE_10,
                   TRLFILE.TL_FEEAMT_10,
                   TRLFILE.TL_FEECODE_11,
                   TRLFILE.TL_FEEAMT_11,
                   TRLFILE.TL_FEECODE_12,
                   TRLFILE.TL_FEEAMT_12,
                   TRLFILE.TL_FEECODE_13,
                   TRLFILE.TL_FEEAMT_13,
                   TRLFILE.TL_FEECODE_14,
                   TRLFILE.TL_FEEAMT_14,
                   TRLFILE.TL_FEECODE_15,
                   TRLFILE.TL_FEEAMT_15,
                   TRLFILE.TL_FEECODE_16,
                   TRLFILE.TL_FEEAMT_16,
                   TRLFILE.TL_FEECODE_17,
                   TRLFILE.TL_FEEAMT_17,
                   TRLFILE.TL_FEECODE_18,
                   TRLFILE.TL_FEEAMT_18,
                   TRLFILE.TL_FEECODE_19,
                   TRLFILE.TL_FEEAMT_19,
                   TRLFILE.TL_FEECODE_20,
                   TRLFILE.TL_FEEAMT_20,
                   TRLFILE.TL_INSURED,
                   TRLFILE.TL_INSTYPES,
                   TRLFILE.TL_INSELIGIBLE,
                   TRLFILE.TL_INSCOMP_1,
                   TRLFILE.TL_INSOURS_1,
                   CAST(TRLFILE.TL_INSEFF_DATE_1
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_1
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_1,
                   TRLFILE.TL_INSCOMM_1,
                   TRLFILE.TL_INSEARNINGS_1,
                   TRLFILE.TL_INSANTICERN_1,
                   TRLFILE.TL_INSCOMP_2,
                   TRLFILE.TL_INSOURS_2,
                   CAST(TRLFILE.TL_INSEFF_DATE_2
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_2
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_2,
                   TRLFILE.TL_INSCOMM_2,
                   TRLFILE.TL_INSEARNINGS_2,
                   TRLFILE.TL_INSANTICERN_2,
                   TRLFILE.TL_INSCOMP_3,
                   TRLFILE.TL_INSOURS_3,
                   CAST(TRLFILE.TL_INSEFF_DATE_3
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_3
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_3,
                   TRLFILE.TL_INSCOMM_3,
                   TRLFILE.TL_INSEARNINGS_3,
                   TRLFILE.TL_INSANTICERN_3,
                   TRLFILE.TL_INSCOMP_4,
                   TRLFILE.TL_INSOURS_4,
                   CAST(TRLFILE.TL_INSEFF_DATE_4
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_4
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_4,
                   TRLFILE.TL_INSCOMM_4,
                   TRLFILE.TL_INSEARNINGS_4,
                   TRLFILE.TL_INSANTICERN_4,
                   TRLFILE.TL_INSCOMP_5,
                   TRLFILE.TL_INSOURS_5,
                   CAST(TRLFILE.TL_INSEFF_DATE_5
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_5
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_5,
                   TRLFILE.TL_INSCOMM_5,
                   TRLFILE.TL_INSEARNINGS_5,
                   TRLFILE.TL_INSANTICERN_5,
                   TRLFILE.TL_INSCOMP_6,
                   TRLFILE.TL_INSOURS_6,
                   CAST(TRLFILE.TL_INSEFF_DATE_6
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_6
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_6,
                   TRLFILE.TL_INSCOMM_6,
                   TRLFILE.TL_INSEARNINGS_6,
                   TRLFILE.TL_INSANTICERN_6,
                   TRLFILE.TL_INSCOMP_7,
                   TRLFILE.TL_INSOURS_7,
                   CAST(TRLFILE.TL_INSEFF_DATE_7
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_7
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_7,
                   TRLFILE.TL_INSCOMM_7,
                   TRLFILE.TL_INSEARNINGS_7,
                   TRLFILE.TL_INSANTICERN_7,
                   TRLFILE.TL_INSCOMP_8,
                   TRLFILE.TL_INSOURS_8,
                   CAST(TRLFILE.TL_INSEFF_DATE_8
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_8
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_8,
                   TRLFILE.TL_INSCOMM_8,
                   TRLFILE.TL_INSEARNINGS_8,
                   TRLFILE.TL_INSANTICERN_8,
                   TRLFILE.TL_INSCOMP_9,
                   TRLFILE.TL_INSOURS_9,
                   CAST(TRLFILE.TL_INSEFF_DATE_9
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_9
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_9,
                   TRLFILE.TL_INSCOMM_9,
                   TRLFILE.TL_INSEARNINGS_9,
                   TRLFILE.TL_INSANTICERN_9,
                   TRLFILE.TL_INSCOMP_10,
                   TRLFILE.TL_INSOURS_10,
                   CAST(TRLFILE.TL_INSEFF_DATE_10
                                                  AS VARCHAR(10)),
                   CAST(TRLFILE.TL_INSEXP_DATE_10
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_INSPREM_10,
                   TRLFILE.TL_INSCOMM_10,
                   TRLFILE.TL_INSEARNINGS_10,
                   TRLFILE.TL_INSANTICERN_10,
                   TRLFILE.TL_DLNO,
                   TRLFILE.TL_EARNINGS_1,
                   TRLFILE.TL_EARNINGS_2,
                   TRLFILE.TL_EARNINGS_3,
                   TRLFILE.TL_EARNINGS_4,
                   TRLFILE.TL_EARNINGS_5,
                   TRLFILE.TL_EARNINGS_6,
                   TRLFILE.TL_EARNINGS_7,
                   TRLFILE.TL_EARNINGS_8,
                   TRLFILE.TL_EARNINGS_9,
                   TRLFILE.TL_EARNINGS_10,
                   TRLFILE.TL_ANTICERN_1,
                   TRLFILE.TL_ANTICERN_2,
                   TRLFILE.TL_ANTICERN_3,
                   TRLFILE.TL_ANTICERN_4,
                   TRLFILE.TL_ANTICERN_5,
                   TRLFILE.TL_ANTICERN_6,
                   TRLFILE.TL_ANTICERN_7,
                   TRLFILE.TL_ANTICERN_8,
                   TRLFILE.TL_ANTICERN_9,
                   TRLFILE.TL_ANTICERN_10,
                   TRLFILE.TL_PRLNNO,
                   TRLFILE.TL_PRBAL,
                   TRLFILE.TL_PRINTDUE,
                   TRLFILE.TL_PRFINCHGRB,
                   TRLFILE.TL_PRINSRB,
                   TRLFILE.TL_LPEOD5_FG,
                   TRLFILE.TL_STATE_WORKER,
                   TRLFILE.TL_RESTRICTD_ACCTNO,
                   TRLFILE.TL_SPRCLASS,
                   TRLFILE.TL_USERID,
                   CAST(TRLFILE.TL_POSTDATE
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_POSTTIME,
                   TRLFILE.TL_GIFTNET,
                   TRLFILE.TL_APR,
                   TRLFILE.TL_SP_LNCOST_FRMLA,
                   TRLFILE.TL_MIP_FG,
                   TRLFILE.TL_INS_TAX_COUNTY_CD,
                   TRLFILE.TL_INS_TAX_CITY_CD,
                   TRLFILE.TL_PRDLDISC_UNEARN,
                   TRLFILE.TL_POINTS,
                   TRLFILE.TL_TAX_AMT,
                   TRLFILE.TL_PBI_PBD_FG,
                   TRLFILE.TL_APPROVED_BY_USER,
                   TRLFILE.TL_APPROVED_BY_CODE,
                   CAST(TRLFILE.TL_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),
                   TRLFILE.TL_APPROVED_BY_TIME,
                   TRLFILE.TL_DEBIT_CARD_AMT,
                   TRLFILE.TL_ACTIONCD2
            INTO :QTRL-REC
            FROM DBO.TRLFILE
            WHERE TRLFILE.TL_BRNO = :QTL-BRNO
              AND TRLFILE.TL_DATE = :QTL-DATE
              AND TRLFILE.TL_CLASS = :QTL-CLASS
              AND TRLFILE.TL_ACCTNO = :QTL-ACCTNO
              AND TRLFILE.TL_LAWCODE = :QTL-LAWCODE
              AND TRLFILE.TL_LINE = :QTL-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRL1_CURSOR INTO :QTRL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRL1-FILE.

           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           ACCEPT TL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           PERFORM SET-TRL-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRLFILE
             (TRLFILE.TL_BRNO,
              TRLFILE.TL_DATE,
              TRLFILE.TL_CLASS,
              TRLFILE.TL_ACCTNO,
              TRLFILE.TL_LAWCODE,
              TRLFILE.TL_LINE,
              TRLFILE.TL_LTOUCH_DATE,
              TRLFILE.TL_SSNO_1,
              TRLFILE.TL_MAKERCD_1,
              TRLFILE.TL_SSNO_2,
              TRLFILE.TL_MAKERCD_2,
              TRLFILE.TL_SSNO_3,
              TRLFILE.TL_MAKERCD_3,
              TRLFILE.TL_SSNO_4,
              TRLFILE.TL_MAKERCD_4,
              TRLFILE.TL_PURCD,
              TRLFILE.TL_SRCD,
              TRLFILE.TL_COLLCD,
              TRLFILE.TL_SERVACCT,
              TRLFILE.TL_LOANTYPE,
              TRLFILE.TL_SUBCLASS,
              TRLFILE.TL_LOANDATE,
              TRLFILE.TL_INTDATE,
              TRLFILE.TL_PROCEEDS,
              TRLFILE.TL_LNAMT,
              TRLFILE.TL_FINCHG,
              TRLFILE.TL_INTCHG,
              TRLFILE.TL_SERCHG,
              TRLFILE.TL_EXTCHG,
              TRLFILE.TL_MAINTFEE,
              TRLFILE.TL_ORGTERM,
              TRLFILE.TL_1STPYAMT,
              TRLFILE.TL_NOREGPY,
              TRLFILE.TL_REGPYAMT,
              TRLFILE.TL_LASTPYAMT,
              TRLFILE.TL_PROCASH,
              TRLFILE.TL_TILLNO,
              TRLFILE.TL_FEECODE_1,
              TRLFILE.TL_FEEAMT_1,
              TRLFILE.TL_FEECODE_2,
              TRLFILE.TL_FEEAMT_2,
              TRLFILE.TL_FEECODE_3,
              TRLFILE.TL_FEEAMT_3,
              TRLFILE.TL_FEECODE_4,
              TRLFILE.TL_FEEAMT_4,
              TRLFILE.TL_FEECODE_5,
              TRLFILE.TL_FEEAMT_5,
              TRLFILE.TL_FEECODE_6,
              TRLFILE.TL_FEEAMT_6,
              TRLFILE.TL_FEECODE_7,
              TRLFILE.TL_FEEAMT_7,
              TRLFILE.TL_FEECODE_8,
              TRLFILE.TL_FEEAMT_8,
              TRLFILE.TL_FEECODE_9,
              TRLFILE.TL_FEEAMT_9,
              TRLFILE.TL_FEECODE_10,
              TRLFILE.TL_FEEAMT_10,
              TRLFILE.TL_FEECODE_11,
              TRLFILE.TL_FEEAMT_11,
              TRLFILE.TL_FEECODE_12,
              TRLFILE.TL_FEEAMT_12,
              TRLFILE.TL_FEECODE_13,
              TRLFILE.TL_FEEAMT_13,
              TRLFILE.TL_FEECODE_14,
              TRLFILE.TL_FEEAMT_14,
              TRLFILE.TL_FEECODE_15,
              TRLFILE.TL_FEEAMT_15,
              TRLFILE.TL_FEECODE_16,
              TRLFILE.TL_FEEAMT_16,
              TRLFILE.TL_FEECODE_17,
              TRLFILE.TL_FEEAMT_17,
              TRLFILE.TL_FEECODE_18,
              TRLFILE.TL_FEEAMT_18,
              TRLFILE.TL_FEECODE_19,
              TRLFILE.TL_FEEAMT_19,
              TRLFILE.TL_FEECODE_20,
              TRLFILE.TL_FEEAMT_20,
              TRLFILE.TL_INSURED,
              TRLFILE.TL_INSTYPES,
              TRLFILE.TL_INSELIGIBLE,
              TRLFILE.TL_INSCOMP_1,
              TRLFILE.TL_INSOURS_1,
              TRLFILE.TL_INSEFF_DATE_1,
              TRLFILE.TL_INSEXP_DATE_1,
              TRLFILE.TL_INSPREM_1,
              TRLFILE.TL_INSCOMM_1,
              TRLFILE.TL_INSEARNINGS_1,
              TRLFILE.TL_INSANTICERN_1,
              TRLFILE.TL_INSCOMP_2,
              TRLFILE.TL_INSOURS_2,
              TRLFILE.TL_INSEFF_DATE_2,
              TRLFILE.TL_INSEXP_DATE_2,
              TRLFILE.TL_INSPREM_2,
              TRLFILE.TL_INSCOMM_2,
              TRLFILE.TL_INSEARNINGS_2,
              TRLFILE.TL_INSANTICERN_2,
              TRLFILE.TL_INSCOMP_3,
              TRLFILE.TL_INSOURS_3,
              TRLFILE.TL_INSEFF_DATE_3,
              TRLFILE.TL_INSEXP_DATE_3,
              TRLFILE.TL_INSPREM_3,
              TRLFILE.TL_INSCOMM_3,
              TRLFILE.TL_INSEARNINGS_3,
              TRLFILE.TL_INSANTICERN_3,
              TRLFILE.TL_INSCOMP_4,
              TRLFILE.TL_INSOURS_4,
              TRLFILE.TL_INSEFF_DATE_4,
              TRLFILE.TL_INSEXP_DATE_4,
              TRLFILE.TL_INSPREM_4,
              TRLFILE.TL_INSCOMM_4,
              TRLFILE.TL_INSEARNINGS_4,
              TRLFILE.TL_INSANTICERN_4,
              TRLFILE.TL_INSCOMP_5,
              TRLFILE.TL_INSOURS_5,
              TRLFILE.TL_INSEFF_DATE_5,
              TRLFILE.TL_INSEXP_DATE_5,
              TRLFILE.TL_INSPREM_5,
              TRLFILE.TL_INSCOMM_5,
              TRLFILE.TL_INSEARNINGS_5,
              TRLFILE.TL_INSANTICERN_5,
              TRLFILE.TL_INSCOMP_6,
              TRLFILE.TL_INSOURS_6,
              TRLFILE.TL_INSEFF_DATE_6,
              TRLFILE.TL_INSEXP_DATE_6,
              TRLFILE.TL_INSPREM_6,
              TRLFILE.TL_INSCOMM_6,
              TRLFILE.TL_INSEARNINGS_6,
              TRLFILE.TL_INSANTICERN_6,
              TRLFILE.TL_INSCOMP_7,
              TRLFILE.TL_INSOURS_7,
              TRLFILE.TL_INSEFF_DATE_7,
              TRLFILE.TL_INSEXP_DATE_7,
              TRLFILE.TL_INSPREM_7,
              TRLFILE.TL_INSCOMM_7,
              TRLFILE.TL_INSEARNINGS_7,
              TRLFILE.TL_INSANTICERN_7,
              TRLFILE.TL_INSCOMP_8,
              TRLFILE.TL_INSOURS_8,
              TRLFILE.TL_INSEFF_DATE_8,
              TRLFILE.TL_INSEXP_DATE_8,
              TRLFILE.TL_INSPREM_8,
              TRLFILE.TL_INSCOMM_8,
              TRLFILE.TL_INSEARNINGS_8,
              TRLFILE.TL_INSANTICERN_8,
              TRLFILE.TL_INSCOMP_9,
              TRLFILE.TL_INSOURS_9,
              TRLFILE.TL_INSEFF_DATE_9,
              TRLFILE.TL_INSEXP_DATE_9,
              TRLFILE.TL_INSPREM_9,
              TRLFILE.TL_INSCOMM_9,
              TRLFILE.TL_INSEARNINGS_9,
              TRLFILE.TL_INSANTICERN_9,
              TRLFILE.TL_INSCOMP_10,
              TRLFILE.TL_INSOURS_10,
              TRLFILE.TL_INSEFF_DATE_10,
              TRLFILE.TL_INSEXP_DATE_10,
              TRLFILE.TL_INSPREM_10,
              TRLFILE.TL_INSCOMM_10,
              TRLFILE.TL_INSEARNINGS_10,
              TRLFILE.TL_INSANTICERN_10,
              TRLFILE.TL_DLNO,
              TRLFILE.TL_EARNINGS_1,
              TRLFILE.TL_EARNINGS_2,
              TRLFILE.TL_EARNINGS_3,
              TRLFILE.TL_EARNINGS_4,
              TRLFILE.TL_EARNINGS_5,
              TRLFILE.TL_EARNINGS_6,
              TRLFILE.TL_EARNINGS_7,
              TRLFILE.TL_EARNINGS_8,
              TRLFILE.TL_EARNINGS_9,
              TRLFILE.TL_EARNINGS_10,
              TRLFILE.TL_ANTICERN_1,
              TRLFILE.TL_ANTICERN_2,
              TRLFILE.TL_ANTICERN_3,
              TRLFILE.TL_ANTICERN_4,
              TRLFILE.TL_ANTICERN_5,
              TRLFILE.TL_ANTICERN_6,
              TRLFILE.TL_ANTICERN_7,
              TRLFILE.TL_ANTICERN_8,
              TRLFILE.TL_ANTICERN_9,
              TRLFILE.TL_ANTICERN_10,
              TRLFILE.TL_PRLNNO,
              TRLFILE.TL_PRBAL,
              TRLFILE.TL_PRINTDUE,
              TRLFILE.TL_PRFINCHGRB,
              TRLFILE.TL_PRINSRB,
              TRLFILE.TL_LPEOD5_FG,
              TRLFILE.TL_STATE_WORKER,
              TRLFILE.TL_RESTRICTD_ACCTNO,
              TRLFILE.TL_SPRCLASS,
              TRLFILE.TL_USERID,
              TRLFILE.TL_POSTDATE,
              TRLFILE.TL_POSTTIME,
              TRLFILE.TL_GIFTNET,
              TRLFILE.TL_APR,
              TRLFILE.TL_SP_LNCOST_FRMLA,
              TRLFILE.TL_MIP_FG,
              TRLFILE.TL_INS_TAX_COUNTY_CD,
              TRLFILE.TL_INS_TAX_CITY_CD,
              TRLFILE.TL_PRDLDISC_UNEARN,
              TRLFILE.TL_POINTS,
              TRLFILE.TL_TAX_AMT,
              TRLFILE.TL_PBI_PBD_FG,
              TRLFILE.TL_APPROVED_BY_USER,
              TRLFILE.TL_APPROVED_BY_CODE,
              TRLFILE.TL_APPROVED_BY_DATE,
              TRLFILE.TL_APPROVED_BY_TIME,
              TRLFILE.TL_DEBIT_CARD_AMT,
              TRLFILE.TL_ACTIONCD2)
            VALUES
             (:QTL-BRNO,
              :QTL-DATE,
              :QTL-CLASS,
              :QTL-ACCTNO,
              :QTL-LAWCODE,
              :QTL-LINE,
              :QTL-LTOUCH-DATE,
              :QTL-SSNO-1,
              :QTL-MAKERCD-1,
              :QTL-SSNO-2,
              :QTL-MAKERCD-2,
              :QTL-SSNO-3,
              :QTL-MAKERCD-3,
              :QTL-SSNO-4,
              :QTL-MAKERCD-4,
              :QTL-PURCD,
              :QTL-SRCD,
              :QTL-COLLCD,
              :QTL-SERVACCT,
              :QTL-LOANTYPE,
              :QTL-SUBCLASS,
              :QTL-LOANDATE,
              :QTL-INTDATE,
              :QTL-PROCEEDS,
              :QTL-LNAMT,
              :QTL-FINCHG,
              :QTL-INTCHG,
              :QTL-SERCHG,
              :QTL-EXTCHG,
              :QTL-MAINTFEE,
              :QTL-ORGTERM,
              :QTL-1STPYAMT,
              :QTL-NOREGPY,
              :QTL-REGPYAMT,
              :QTL-LASTPYAMT,
              :QTL-PROCASH,
              :QTL-TILLNO,
              :QTL-FEECODE-1,
              :QTL-FEEAMT-1,
              :QTL-FEECODE-2,
              :QTL-FEEAMT-2,
              :QTL-FEECODE-3,
              :QTL-FEEAMT-3,
              :QTL-FEECODE-4,
              :QTL-FEEAMT-4,
              :QTL-FEECODE-5,
              :QTL-FEEAMT-5,
              :QTL-FEECODE-6,
              :QTL-FEEAMT-6,
              :QTL-FEECODE-7,
              :QTL-FEEAMT-7,
              :QTL-FEECODE-8,
              :QTL-FEEAMT-8,
              :QTL-FEECODE-9,
              :QTL-FEEAMT-9,
              :QTL-FEECODE-10,
              :QTL-FEEAMT-10,
              :QTL-FEECODE-11,
              :QTL-FEEAMT-11,
              :QTL-FEECODE-12,
              :QTL-FEEAMT-12,
              :QTL-FEECODE-13,
              :QTL-FEEAMT-13,
              :QTL-FEECODE-14,
              :QTL-FEEAMT-14,
              :QTL-FEECODE-15,
              :QTL-FEEAMT-15,
              :QTL-FEECODE-16,
              :QTL-FEEAMT-16,
              :QTL-FEECODE-17,
              :QTL-FEEAMT-17,
              :QTL-FEECODE-18,
              :QTL-FEEAMT-18,
              :QTL-FEECODE-19,
              :QTL-FEEAMT-19,
              :QTL-FEECODE-20,
              :QTL-FEEAMT-20,
              :QTL-INSURED,
              :QTL-INSTYPES,
              :QTL-INSELIGIBLE,
              :QTL-INSCOMP-1,
              :QTL-INSOURS-1,
              :QTL-INSEFF-DATE-1,
              :QTL-INSEXP-DATE-1,
              :QTL-INSPREM-1,
              :QTL-INSCOMM-1,
              :QTL-INSEARNINGS-1,
              :QTL-INSANTICERN-1,
              :QTL-INSCOMP-2,
              :QTL-INSOURS-2,
              :QTL-INSEFF-DATE-2,
              :QTL-INSEXP-DATE-2,
              :QTL-INSPREM-2,
              :QTL-INSCOMM-2,
              :QTL-INSEARNINGS-2,
              :QTL-INSANTICERN-2,
              :QTL-INSCOMP-3,
              :QTL-INSOURS-3,
              :QTL-INSEFF-DATE-3,
              :QTL-INSEXP-DATE-3,
              :QTL-INSPREM-3,
              :QTL-INSCOMM-3,
              :QTL-INSEARNINGS-3,
              :QTL-INSANTICERN-3,
              :QTL-INSCOMP-4,
              :QTL-INSOURS-4,
              :QTL-INSEFF-DATE-4,
              :QTL-INSEXP-DATE-4,
              :QTL-INSPREM-4,
              :QTL-INSCOMM-4,
              :QTL-INSEARNINGS-4,
              :QTL-INSANTICERN-4,
              :QTL-INSCOMP-5,
              :QTL-INSOURS-5,
              :QTL-INSEFF-DATE-5,
              :QTL-INSEXP-DATE-5,
              :QTL-INSPREM-5,
              :QTL-INSCOMM-5,
              :QTL-INSEARNINGS-5,
              :QTL-INSANTICERN-5,
              :QTL-INSCOMP-6,
              :QTL-INSOURS-6,
              :QTL-INSEFF-DATE-6,
              :QTL-INSEXP-DATE-6,
              :QTL-INSPREM-6,
              :QTL-INSCOMM-6,
              :QTL-INSEARNINGS-6,
              :QTL-INSANTICERN-6,
              :QTL-INSCOMP-7,
              :QTL-INSOURS-7,
              :QTL-INSEFF-DATE-7,
              :QTL-INSEXP-DATE-7,
              :QTL-INSPREM-7,
              :QTL-INSCOMM-7,
              :QTL-INSEARNINGS-7,
              :QTL-INSANTICERN-7,
              :QTL-INSCOMP-8,
              :QTL-INSOURS-8,
              :QTL-INSEFF-DATE-8,
              :QTL-INSEXP-DATE-8,
              :QTL-INSPREM-8,
              :QTL-INSCOMM-8,
              :QTL-INSEARNINGS-8,
              :QTL-INSANTICERN-8,
              :QTL-INSCOMP-9,
              :QTL-INSOURS-9,
              :QTL-INSEFF-DATE-9,
              :QTL-INSEXP-DATE-9,
              :QTL-INSPREM-9,
              :QTL-INSCOMM-9,
              :QTL-INSEARNINGS-9,
              :QTL-INSANTICERN-9,
              :QTL-INSCOMP-10,
              :QTL-INSOURS-10,
              :QTL-INSEFF-DATE-10,
              :QTL-INSEXP-DATE-10,
              :QTL-INSPREM-10,
              :QTL-INSCOMM-10,
              :QTL-INSEARNINGS-10,
              :QTL-INSANTICERN-10,
              :QTL-DLNO,
              :QTL-EARNINGS-1,
              :QTL-EARNINGS-2,
              :QTL-EARNINGS-3,
              :QTL-EARNINGS-4,
              :QTL-EARNINGS-5,
              :QTL-EARNINGS-6,
              :QTL-EARNINGS-7,
              :QTL-EARNINGS-8,
              :QTL-EARNINGS-9,
              :QTL-EARNINGS-10,
              :QTL-ANTICERN-1,
              :QTL-ANTICERN-2,
              :QTL-ANTICERN-3,
              :QTL-ANTICERN-4,
              :QTL-ANTICERN-5,
              :QTL-ANTICERN-6,
              :QTL-ANTICERN-7,
              :QTL-ANTICERN-8,
              :QTL-ANTICERN-9,
              :QTL-ANTICERN-10,
              :QTL-PRLNNO,
              :QTL-PRBAL,
              :QTL-PRINTDUE,
              :QTL-PRFINCHGRB,
              :QTL-PRINSRB,
              :QTL-LPEOD5-FG,
              :QTL-STATE-WORKER,
              :QTL-RESTRICTD-ACCTNO,
              :QTL-SPRCLASS,
              :QTL-USERID,
              :QTL-POSTDATE,
              :QTL-POSTTIME,
              :QTL-GIFTNET,
              :QTL-APR,
              :QTL-SP-LNCOST-FRMLA,
              :QTL-MIP-FG,
              :QTL-INS-TAX-COUNTY-CD,
              :QTL-INS-TAX-CITY-CD,
              :QTL-PRDLDISC-UNEARN,
              :QTL-POINTS,
              :QTL-TAX-AMT,
              :QTL-PBI-PBD-FG,
              :QTL-APPROVED-BY-USER,
              :QTL-APPROVED-BY-CODE,
              :QTL-APPROVED-BY-DATE,
              :QTL-APPROVED-BY-TIME,
              :QTL-DEBIT-CARD-AMT,
              :QTL-ACTIONCD2)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRL1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRL1-FILE.

           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           ACCEPT TL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           PERFORM SET-TRL-FIELDS.

           EXEC SQL
            UPDATE DBO.TRLFILE
             SET
              TRLFILE.TL_BRNO = :QTL-BRNO,
              TRLFILE.TL_DATE =
                 :QTL-DATE,
              TRLFILE.TL_CLASS = :QTL-CLASS,
              TRLFILE.TL_ACCTNO = :QTL-ACCTNO,
              TRLFILE.TL_LAWCODE = :QTL-LAWCODE,
              TRLFILE.TL_LINE = :QTL-LINE,
              TRLFILE.TL_LTOUCH_DATE =
                 :QTL-LTOUCH-DATE,
              TRLFILE.TL_SSNO_1 = :QTL-SSNO-1,
              TRLFILE.TL_MAKERCD_1 = :QTL-MAKERCD-1,
              TRLFILE.TL_SSNO_2 = :QTL-SSNO-2,
              TRLFILE.TL_MAKERCD_2 = :QTL-MAKERCD-2,
              TRLFILE.TL_SSNO_3 = :QTL-SSNO-3,
              TRLFILE.TL_MAKERCD_3 = :QTL-MAKERCD-3,
              TRLFILE.TL_SSNO_4 = :QTL-SSNO-4,
              TRLFILE.TL_MAKERCD_4 = :QTL-MAKERCD-4,
              TRLFILE.TL_PURCD = :QTL-PURCD,
              TRLFILE.TL_SRCD = :QTL-SRCD,
              TRLFILE.TL_COLLCD = :QTL-COLLCD,
              TRLFILE.TL_SERVACCT = :QTL-SERVACCT,
              TRLFILE.TL_LOANTYPE = :QTL-LOANTYPE,
              TRLFILE.TL_SUBCLASS = :QTL-SUBCLASS,
              TRLFILE.TL_LOANDATE =
                 :QTL-LOANDATE,
              TRLFILE.TL_INTDATE =
                 :QTL-INTDATE,
              TRLFILE.TL_PROCEEDS = :QTL-PROCEEDS,
              TRLFILE.TL_LNAMT = :QTL-LNAMT,
              TRLFILE.TL_FINCHG = :QTL-FINCHG,
              TRLFILE.TL_INTCHG = :QTL-INTCHG,
              TRLFILE.TL_SERCHG = :QTL-SERCHG,
              TRLFILE.TL_EXTCHG = :QTL-EXTCHG,
              TRLFILE.TL_MAINTFEE = :QTL-MAINTFEE,
              TRLFILE.TL_ORGTERM = :QTL-ORGTERM,
              TRLFILE.TL_1STPYAMT = :QTL-1STPYAMT,
              TRLFILE.TL_NOREGPY = :QTL-NOREGPY,
              TRLFILE.TL_REGPYAMT = :QTL-REGPYAMT,
              TRLFILE.TL_LASTPYAMT = :QTL-LASTPYAMT,
              TRLFILE.TL_PROCASH = :QTL-PROCASH,
              TRLFILE.TL_TILLNO = :QTL-TILLNO,
              TRLFILE.TL_FEECODE_1 = :QTL-FEECODE-1,
              TRLFILE.TL_FEEAMT_1 = :QTL-FEEAMT-1,
              TRLFILE.TL_FEECODE_2 = :QTL-FEECODE-2,
              TRLFILE.TL_FEEAMT_2 = :QTL-FEEAMT-2,
              TRLFILE.TL_FEECODE_3 = :QTL-FEECODE-3,
              TRLFILE.TL_FEEAMT_3 = :QTL-FEEAMT-3,
              TRLFILE.TL_FEECODE_4 = :QTL-FEECODE-4,
              TRLFILE.TL_FEEAMT_4 = :QTL-FEEAMT-4,
              TRLFILE.TL_FEECODE_5 = :QTL-FEECODE-5,
              TRLFILE.TL_FEEAMT_5 = :QTL-FEEAMT-5,
              TRLFILE.TL_FEECODE_6 = :QTL-FEECODE-6,
              TRLFILE.TL_FEEAMT_6 = :QTL-FEEAMT-6,
              TRLFILE.TL_FEECODE_7 = :QTL-FEECODE-7,
              TRLFILE.TL_FEEAMT_7 = :QTL-FEEAMT-7,
              TRLFILE.TL_FEECODE_8 = :QTL-FEECODE-8,
              TRLFILE.TL_FEEAMT_8 = :QTL-FEEAMT-8,
              TRLFILE.TL_FEECODE_9 = :QTL-FEECODE-9,
              TRLFILE.TL_FEEAMT_9 = :QTL-FEEAMT-9,
              TRLFILE.TL_FEECODE_10 = :QTL-FEECODE-10,
              TRLFILE.TL_FEEAMT_10 = :QTL-FEEAMT-10,
              TRLFILE.TL_FEECODE_11 = :QTL-FEECODE-11,
              TRLFILE.TL_FEEAMT_11 = :QTL-FEEAMT-11,
              TRLFILE.TL_FEECODE_12 = :QTL-FEECODE-12,
              TRLFILE.TL_FEEAMT_12 = :QTL-FEEAMT-12,
              TRLFILE.TL_FEECODE_13 = :QTL-FEECODE-13,
              TRLFILE.TL_FEEAMT_13 = :QTL-FEEAMT-13,
              TRLFILE.TL_FEECODE_14 = :QTL-FEECODE-14,
              TRLFILE.TL_FEEAMT_14 = :QTL-FEEAMT-14,
              TRLFILE.TL_FEECODE_15 = :QTL-FEECODE-15,
              TRLFILE.TL_FEEAMT_15 = :QTL-FEEAMT-15,
              TRLFILE.TL_FEECODE_16 = :QTL-FEECODE-16,
              TRLFILE.TL_FEEAMT_16 = :QTL-FEEAMT-16,
              TRLFILE.TL_FEECODE_17 = :QTL-FEECODE-17,
              TRLFILE.TL_FEEAMT_17 = :QTL-FEEAMT-17,
              TRLFILE.TL_FEECODE_18 = :QTL-FEECODE-18,
              TRLFILE.TL_FEEAMT_18 = :QTL-FEEAMT-18,
              TRLFILE.TL_FEECODE_19 = :QTL-FEECODE-19,
              TRLFILE.TL_FEEAMT_19 = :QTL-FEEAMT-19,
              TRLFILE.TL_FEECODE_20 = :QTL-FEECODE-20,
              TRLFILE.TL_FEEAMT_20 = :QTL-FEEAMT-20,
              TRLFILE.TL_INSURED = :QTL-INSURED,
              TRLFILE.TL_INSTYPES = :QTL-INSTYPES,
              TRLFILE.TL_INSELIGIBLE = :QTL-INSELIGIBLE,
              TRLFILE.TL_INSCOMP_1 = :QTL-INSCOMP-1,
              TRLFILE.TL_INSOURS_1 = :QTL-INSOURS-1,
              TRLFILE.TL_INSEFF_DATE_1 =
                 :QTL-INSEFF-DATE-1,
              TRLFILE.TL_INSEXP_DATE_1 =
                 :QTL-INSEXP-DATE-1,
              TRLFILE.TL_INSPREM_1 = :QTL-INSPREM-1,
              TRLFILE.TL_INSCOMM_1 = :QTL-INSCOMM-1,
              TRLFILE.TL_INSEARNINGS_1 = :QTL-INSEARNINGS-1,
              TRLFILE.TL_INSANTICERN_1 = :QTL-INSANTICERN-1,
              TRLFILE.TL_INSCOMP_2 = :QTL-INSCOMP-2,
              TRLFILE.TL_INSOURS_2 = :QTL-INSOURS-2,
              TRLFILE.TL_INSEFF_DATE_2 =
                 :QTL-INSEFF-DATE-2,
              TRLFILE.TL_INSEXP_DATE_2 =
                 :QTL-INSEXP-DATE-2,
              TRLFILE.TL_INSPREM_2 = :QTL-INSPREM-2,
              TRLFILE.TL_INSCOMM_2 = :QTL-INSCOMM-2,
              TRLFILE.TL_INSEARNINGS_2 = :QTL-INSEARNINGS-2,
              TRLFILE.TL_INSANTICERN_2 = :QTL-INSANTICERN-2,
              TRLFILE.TL_INSCOMP_3 = :QTL-INSCOMP-3,
              TRLFILE.TL_INSOURS_3 = :QTL-INSOURS-3,
              TRLFILE.TL_INSEFF_DATE_3 =
                 :QTL-INSEFF-DATE-3,
              TRLFILE.TL_INSEXP_DATE_3 =
                 :QTL-INSEXP-DATE-3,
              TRLFILE.TL_INSPREM_3 = :QTL-INSPREM-3,
              TRLFILE.TL_INSCOMM_3 = :QTL-INSCOMM-3,
              TRLFILE.TL_INSEARNINGS_3 = :QTL-INSEARNINGS-3,
              TRLFILE.TL_INSANTICERN_3 = :QTL-INSANTICERN-3,
              TRLFILE.TL_INSCOMP_4 = :QTL-INSCOMP-4,
              TRLFILE.TL_INSOURS_4 = :QTL-INSOURS-4,
              TRLFILE.TL_INSEFF_DATE_4 =
                 :QTL-INSEFF-DATE-4,
              TRLFILE.TL_INSEXP_DATE_4 =
                 :QTL-INSEXP-DATE-4,
              TRLFILE.TL_INSPREM_4 = :QTL-INSPREM-4,
              TRLFILE.TL_INSCOMM_4 = :QTL-INSCOMM-4,
              TRLFILE.TL_INSEARNINGS_4 = :QTL-INSEARNINGS-4,
              TRLFILE.TL_INSANTICERN_4 = :QTL-INSANTICERN-4,
              TRLFILE.TL_INSCOMP_5 = :QTL-INSCOMP-5,
              TRLFILE.TL_INSOURS_5 = :QTL-INSOURS-5,
              TRLFILE.TL_INSEFF_DATE_5 =
                 :QTL-INSEFF-DATE-5,
              TRLFILE.TL_INSEXP_DATE_5 =
                 :QTL-INSEXP-DATE-5,
              TRLFILE.TL_INSPREM_5 = :QTL-INSPREM-5,
              TRLFILE.TL_INSCOMM_5 = :QTL-INSCOMM-5,
              TRLFILE.TL_INSEARNINGS_5 = :QTL-INSEARNINGS-5,
              TRLFILE.TL_INSANTICERN_5 = :QTL-INSANTICERN-5,
              TRLFILE.TL_INSCOMP_6 = :QTL-INSCOMP-6,
              TRLFILE.TL_INSOURS_6 = :QTL-INSOURS-6,
              TRLFILE.TL_INSEFF_DATE_6 =
                 :QTL-INSEFF-DATE-6,
              TRLFILE.TL_INSEXP_DATE_6 =
                 :QTL-INSEXP-DATE-6,
              TRLFILE.TL_INSPREM_6 = :QTL-INSPREM-6,
              TRLFILE.TL_INSCOMM_6 = :QTL-INSCOMM-6,
              TRLFILE.TL_INSEARNINGS_6 = :QTL-INSEARNINGS-6,
              TRLFILE.TL_INSANTICERN_6 = :QTL-INSANTICERN-6,
              TRLFILE.TL_INSCOMP_7 = :QTL-INSCOMP-7,
              TRLFILE.TL_INSOURS_7 = :QTL-INSOURS-7,
              TRLFILE.TL_INSEFF_DATE_7 =
                 :QTL-INSEFF-DATE-7,
              TRLFILE.TL_INSEXP_DATE_7 =
                 :QTL-INSEXP-DATE-7,
              TRLFILE.TL_INSPREM_7 = :QTL-INSPREM-7,
              TRLFILE.TL_INSCOMM_7 = :QTL-INSCOMM-7,
              TRLFILE.TL_INSEARNINGS_7 = :QTL-INSEARNINGS-7,
              TRLFILE.TL_INSANTICERN_7 = :QTL-INSANTICERN-7,
              TRLFILE.TL_INSCOMP_8 = :QTL-INSCOMP-8,
              TRLFILE.TL_INSOURS_8 = :QTL-INSOURS-8,
              TRLFILE.TL_INSEFF_DATE_8 =
                 :QTL-INSEFF-DATE-8,
              TRLFILE.TL_INSEXP_DATE_8 =
                 :QTL-INSEXP-DATE-8,
              TRLFILE.TL_INSPREM_8 = :QTL-INSPREM-8,
              TRLFILE.TL_INSCOMM_8 = :QTL-INSCOMM-8,
              TRLFILE.TL_INSEARNINGS_8 = :QTL-INSEARNINGS-8,
              TRLFILE.TL_INSANTICERN_8 = :QTL-INSANTICERN-8,
              TRLFILE.TL_INSCOMP_9 = :QTL-INSCOMP-9,
              TRLFILE.TL_INSOURS_9 = :QTL-INSOURS-9,
              TRLFILE.TL_INSEFF_DATE_9 =
                 :QTL-INSEFF-DATE-9,
              TRLFILE.TL_INSEXP_DATE_9 =
                 :QTL-INSEXP-DATE-9,
              TRLFILE.TL_INSPREM_9 = :QTL-INSPREM-9,
              TRLFILE.TL_INSCOMM_9 = :QTL-INSCOMM-9,
              TRLFILE.TL_INSEARNINGS_9 = :QTL-INSEARNINGS-9,
              TRLFILE.TL_INSANTICERN_9 = :QTL-INSANTICERN-9,
              TRLFILE.TL_INSCOMP_10 = :QTL-INSCOMP-10,
              TRLFILE.TL_INSOURS_10 = :QTL-INSOURS-10,
              TRLFILE.TL_INSEFF_DATE_10 =
                 :QTL-INSEFF-DATE-10,
              TRLFILE.TL_INSEXP_DATE_10 =
                 :QTL-INSEXP-DATE-10,
              TRLFILE.TL_INSPREM_10 = :QTL-INSPREM-10,
              TRLFILE.TL_INSCOMM_10 = :QTL-INSCOMM-10,
              TRLFILE.TL_INSEARNINGS_10 = :QTL-INSEARNINGS-10,
              TRLFILE.TL_INSANTICERN_10 = :QTL-INSANTICERN-10,
              TRLFILE.TL_DLNO = :QTL-DLNO,
              TRLFILE.TL_EARNINGS_1 = :QTL-EARNINGS-1,
              TRLFILE.TL_EARNINGS_2 = :QTL-EARNINGS-2,
              TRLFILE.TL_EARNINGS_3 = :QTL-EARNINGS-3,
              TRLFILE.TL_EARNINGS_4 = :QTL-EARNINGS-4,
              TRLFILE.TL_EARNINGS_5 = :QTL-EARNINGS-5,
              TRLFILE.TL_EARNINGS_6 = :QTL-EARNINGS-6,
              TRLFILE.TL_EARNINGS_7 = :QTL-EARNINGS-7,
              TRLFILE.TL_EARNINGS_8 = :QTL-EARNINGS-8,
              TRLFILE.TL_EARNINGS_9 = :QTL-EARNINGS-9,
              TRLFILE.TL_EARNINGS_10 = :QTL-EARNINGS-10,
              TRLFILE.TL_ANTICERN_1 = :QTL-ANTICERN-1,
              TRLFILE.TL_ANTICERN_2 = :QTL-ANTICERN-2,
              TRLFILE.TL_ANTICERN_3 = :QTL-ANTICERN-3,
              TRLFILE.TL_ANTICERN_4 = :QTL-ANTICERN-4,
              TRLFILE.TL_ANTICERN_5 = :QTL-ANTICERN-5,
              TRLFILE.TL_ANTICERN_6 = :QTL-ANTICERN-6,
              TRLFILE.TL_ANTICERN_7 = :QTL-ANTICERN-7,
              TRLFILE.TL_ANTICERN_8 = :QTL-ANTICERN-8,
              TRLFILE.TL_ANTICERN_9 = :QTL-ANTICERN-9,
              TRLFILE.TL_ANTICERN_10 = :QTL-ANTICERN-10,
              TRLFILE.TL_PRLNNO = :QTL-PRLNNO,
              TRLFILE.TL_PRBAL = :QTL-PRBAL,
              TRLFILE.TL_PRINTDUE = :QTL-PRINTDUE,
              TRLFILE.TL_PRFINCHGRB = :QTL-PRFINCHGRB,
              TRLFILE.TL_PRINSRB = :QTL-PRINSRB,
              TRLFILE.TL_LPEOD5_FG = :QTL-LPEOD5-FG,
              TRLFILE.TL_STATE_WORKER = :QTL-STATE-WORKER,
              TRLFILE.TL_RESTRICTD_ACCTNO = :QTL-RESTRICTD-ACCTNO,
              TRLFILE.TL_SPRCLASS = :QTL-SPRCLASS,
              TRLFILE.TL_USERID = :QTL-USERID,
              TRLFILE.TL_POSTDATE =
                 :QTL-POSTDATE,
              TRLFILE.TL_POSTTIME = :QTL-POSTTIME,
              TRLFILE.TL_GIFTNET = :QTL-GIFTNET,
              TRLFILE.TL_APR = :QTL-APR,
              TRLFILE.TL_SP_LNCOST_FRMLA = :QTL-SP-LNCOST-FRMLA,
              TRLFILE.TL_MIP_FG = :QTL-MIP-FG,
              TRLFILE.TL_INS_TAX_COUNTY_CD =
                   :QTL-INS-TAX-COUNTY-CD,
              TRLFILE.TL_INS_TAX_CITY_CD = :QTL-INS-TAX-CITY-CD,
              TRLFILE.TL_PRDLDISC_UNEARN = :QTL-PRDLDISC-UNEARN,
              TRLFILE.TL_POINTS = :QTL-POINTS,
              TRLFILE.TL_TAX_AMT = :QTL-TAX-AMT,
              TRLFILE.TL_PBI_PBD_FG = :QTL-PBI-PBD-FG,
              TRLFILE.TL_APPROVED_BY_USER = :QTL-APPROVED-BY-USER,
              TRLFILE.TL_APPROVED_BY_CODE = :QTL-APPROVED-BY-CODE,
              TRLFILE.TL_APPROVED_BY_DATE = :QTL-APPROVED-BY-DATE,
              TRLFILE.TL_APPROVED_BY_TIME = :QTL-APPROVED-BY-TIME,
              TRLFILE.TL_DEBIT_CARD_AMT = :QTL-DEBIT-CARD-AMT,
              TRLFILE.TL_ACTIONCD2 = :QTL-ACTIONCD2
            WHERE TRLFILE.TL_BRNO = :QTL-BRNO
              AND TRLFILE.TL_DATE = :QTL-DATE
              AND TRLFILE.TL_CLASS = :QTL-CLASS
              AND TRLFILE.TL_ACCTNO = :QTL-ACCTNO
              AND TRLFILE.TL_LAWCODE = :QTL-LAWCODE
              AND TRLFILE.TL_LINE = :QTL-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRL1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRL1-FILE.

           MOVE TRL1-KEY        TO E-KEYX.
           PERFORM DELETE-IT.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           PERFORM SET-TRL-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRLFILE
            WHERE TRLFILE.TL_BRNO = :QTL-BRNO
              AND TRLFILE.TL_DATE = :QTL-DATE
              AND TRLFILE.TL_CLASS = :QTL-CLASS
              AND TRLFILE.TL_ACCTNO = :QTL-ACCTNO
              AND TRLFILE.TL_LAWCODE = :QTL-LAWCODE
              AND TRLFILE.TL_LINE = :QTL-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRL1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRL1-CURSOR-STAT.

