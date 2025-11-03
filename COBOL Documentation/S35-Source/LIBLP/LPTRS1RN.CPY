      * COPYMEMBER: LIBLP/LPTR1RN
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
       START-TRS1-FILE-SCAN.
           PERFORM START-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           IF ( TRS1-CURSOR-SC-STAT-GOOD )
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
                                           QTRS1-WEND-LINE
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }        
              MOVE TS-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WSBEG-DATE  

              IF ( TS-CLASS = 999 )
                 MOVE TS-CLASS          TO QTRS1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRS1-WSBEG-CLASS = TS-CLASS + 1
              END-IF

              IF ( TS-ACCTNO = 99999999 )
                 MOVE TS-ACCTNO         TO QTRS1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRS1-WSBEG-ACCTNO = TS-ACCTNO + 1 
              END-IF

              IF ( TS-LAWCODE = 99 )
                 MOVE TS-LAWCODE        TO QTRS1-WSBEG-LAWCODE
              ELSE
                 COMPUTE QTRS1-WSBEG-LAWCODE = TS-LAWCODE + 1
              END-IF.

           EXEC SQL
            DECLARE TRS1_CURSOR_SC CURSOR FOR
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
            WHERE (  TRSFILE.TS_BRNO     = :QTRS1-WBEG-BRNO      )
              AND (( TRSFILE.TS_DATE    >= :QTRS1-WBEG-DATE
              AND    TRSFILE.TS_CLASS   >= :QTRS1-WBEG-CLASS
              AND    TRSFILE.TS_ACCTNO  >= :QTRS1-WBEG-ACCTNO
              AND    TRSFILE.TS_LAWCODE >= :QTRS1-WBEG-LAWCODE
              AND    TRSFILE.TS_LINE    >= :QTRS1-WBEG-LINE     )
               OR  ( TRSFILE.TS_DATE    >= :QTRS1-WBEG-DATE
              AND    TRSFILE.TS_CLASS   >= :QTRS1-WBEG-CLASS
              AND    TRSFILE.TS_ACCTNO  >= :QTRS1-WBEG-ACCTNO
              AND    TRSFILE.TS_LAWCODE >= :QTRS1-WSBEG-LAWCODE
              AND    TRSFILE.TS_LINE    >= 0                    )
               OR  ( TRSFILE.TS_DATE    >= :QTRS1-WBEG-DATE
              AND    TRSFILE.TS_CLASS   >= :QTRS1-WBEG-CLASS
              AND    TRSFILE.TS_ACCTNO  >= :QTRS1-WSBEG-ACCTNO
              AND    TRSFILE.TS_LAWCODE >= 0
              AND    TRSFILE.TS_LINE    >= 0                    )
               OR  ( TRSFILE.TS_DATE    >= :QTRS1-WBEG-DATE
              AND    TRSFILE.TS_CLASS   >= :QTRS1-WSBEG-CLASS
              AND    TRSFILE.TS_ACCTNO  >= 0
              AND    TRSFILE.TS_LAWCODE >= 0 
              AND    TRSFILE.TS_LINE    >= 0                    )
               OR  ( TRSFILE.TS_DATE    >= :QTRS1-WSBEG-DATE
              AND    TRSFILE.TS_CLASS   >= 0
              AND    TRSFILE.TS_ACCTNO  >= 0
              AND    TRSFILE.TS_LAWCODE >= 0 
              AND    TRSFILE.TS_LINE    >= 0                    ))
              AND (  TRSFILE.TS_DATE    <= :QTRS1-WEND-DATE
              AND    TRSFILE.TS_CLASS   <= :QTRS1-WEND-CLASS
              AND    TRSFILE.TS_ACCTNO  <= :QTRS1-WEND-ACCTNO
              AND    TRSFILE.TS_LAWCODE <= :QTRS1-WEND-LAWCODE
              AND    TRSFILE.TS_LINE    <= :QTRS1-WEND-LINE      )
            ORDER BY TRSFILE.TS_BRNO,
                     TRSFILE.TS_DATE,
                     TRSFILE.TS_CLASS,
                     TRSFILE.TS_ACCTNO,
                     TRSFILE.TS_LAWCODE,
                     TRSFILE.TS_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRS1_CURSOR_SC
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

           MOVE STAT---GOOD TO TRS1-CURSOR-SC-STAT.


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
              IF ( TRS1-CURSOR-SC-STAT-GOOD )
                 EXEC SQL
                      FETCH TRS1_CURSOR_SC INTO :QTRS-REC
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
       CLOSE-TRS1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRS1-CURSOR-STAT.

           IF ( TRS1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE TRS1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO TRS1-CURSOR-SC-STAT.

