      * COPYMEMBER: LIBLP/LPTR1RN
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
                   ISNULL(CAST(TRLFILE.TL_DATE AS VARCHAR(10)),'0'),
                   TRLFILE.TL_CLASS,
                   TRLFILE.TL_ACCTNO,
                   TRLFILE.TL_LAWCODE,
                   TRLFILE.TL_LINE,
                   ISNULL(CAST(TRLFILE.TL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_SSNO_1,
                   ISNULL(TRLFILE.TL_MAKERCD_1,''),
                   TRLFILE.TL_SSNO_2,
                   ISNULL(TRLFILE.TL_MAKERCD_2,''),
                   TRLFILE.TL_SSNO_3,
                   ISNULL(TRLFILE.TL_MAKERCD_3,''),
                   TRLFILE.TL_SSNO_4,
                   ISNULL(TRLFILE.TL_MAKERCD_4,''),
                   ISNULL(TRLFILE.TL_PURCD,''),
                   ISNULL(TRLFILE.TL_SRCD,''),
                   ISNULL(TRLFILE.TL_COLLCD,''),
                   ISNULL(TRLFILE.TL_SERVACCT,''),
                   ISNULL(TRLFILE.TL_LOANTYPE,''),
                   TRLFILE.TL_SUBCLASS,
                   ISNULL(CAST(TRLFILE.TL_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INTDATE AS VARCHAR(10)),'0'),
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
                   ISNULL(TRLFILE.TL_FEECODE_1,''),
                   TRLFILE.TL_FEEAMT_1,
                   ISNULL(TRLFILE.TL_FEECODE_2,''),
                   TRLFILE.TL_FEEAMT_2,
                   ISNULL(TRLFILE.TL_FEECODE_3,''),
                   TRLFILE.TL_FEEAMT_3,
                   ISNULL(TRLFILE.TL_FEECODE_4,''),
                   TRLFILE.TL_FEEAMT_4,
                   ISNULL(TRLFILE.TL_FEECODE_5,''),
                   TRLFILE.TL_FEEAMT_5,
                   ISNULL(TRLFILE.TL_FEECODE_6,''),
                   TRLFILE.TL_FEEAMT_6,
                   ISNULL(TRLFILE.TL_FEECODE_7,''),
                   TRLFILE.TL_FEEAMT_7,
                   ISNULL(TRLFILE.TL_FEECODE_8,''),
                   TRLFILE.TL_FEEAMT_8,
                   ISNULL(TRLFILE.TL_FEECODE_9,''),
                   TRLFILE.TL_FEEAMT_9,
                   ISNULL(TRLFILE.TL_FEECODE_10,''),
                   TRLFILE.TL_FEEAMT_10,
                   ISNULL(TRLFILE.TL_FEECODE_11,''),
                   TRLFILE.TL_FEEAMT_11,
                   ISNULL(TRLFILE.TL_FEECODE_12,''),
                   TRLFILE.TL_FEEAMT_12,
                   ISNULL(TRLFILE.TL_FEECODE_13,''),
                   TRLFILE.TL_FEEAMT_13,
                   ISNULL(TRLFILE.TL_FEECODE_14,''),
                   TRLFILE.TL_FEEAMT_14,
                   ISNULL(TRLFILE.TL_FEECODE_15,''),
                   TRLFILE.TL_FEEAMT_15,
                   ISNULL(TRLFILE.TL_FEECODE_16,''),
                   TRLFILE.TL_FEEAMT_16,
                   ISNULL(TRLFILE.TL_FEECODE_17,''),
                   TRLFILE.TL_FEEAMT_17,
                   ISNULL(TRLFILE.TL_FEECODE_18,''),
                   TRLFILE.TL_FEEAMT_18,
                   ISNULL(TRLFILE.TL_FEECODE_19,''),
                   TRLFILE.TL_FEEAMT_19,
                   ISNULL(TRLFILE.TL_FEECODE_20,''),
                   TRLFILE.TL_FEEAMT_20,
                   ISNULL(TRLFILE.TL_INSURED,''),
                   ISNULL(TRLFILE.TL_INSTYPES,''),
                   ISNULL(TRLFILE.TL_INSELIGIBLE,''),
                   TRLFILE.TL_INSCOMP_1,
                   ISNULL(TRLFILE.TL_INSOURS_1,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_1,
                   TRLFILE.TL_INSCOMM_1,
                   TRLFILE.TL_INSEARNINGS_1,
                   TRLFILE.TL_INSANTICERN_1,
                   TRLFILE.TL_INSCOMP_2,
                   ISNULL(TRLFILE.TL_INSOURS_2,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_2,
                   TRLFILE.TL_INSCOMM_2,
                   TRLFILE.TL_INSEARNINGS_2,
                   TRLFILE.TL_INSANTICERN_2,
                   TRLFILE.TL_INSCOMP_3,
                   ISNULL(TRLFILE.TL_INSOURS_3,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_3,
                   TRLFILE.TL_INSCOMM_3,
                   TRLFILE.TL_INSEARNINGS_3,
                   TRLFILE.TL_INSANTICERN_3,
                   TRLFILE.TL_INSCOMP_4,
                   ISNULL(TRLFILE.TL_INSOURS_4,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_4,
                   TRLFILE.TL_INSCOMM_4,
                   TRLFILE.TL_INSEARNINGS_4,
                   TRLFILE.TL_INSANTICERN_4,
                   TRLFILE.TL_INSCOMP_5,
                   ISNULL(TRLFILE.TL_INSOURS_5,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_5,
                   TRLFILE.TL_INSCOMM_5,
                   TRLFILE.TL_INSEARNINGS_5,
                   TRLFILE.TL_INSANTICERN_5,
                   TRLFILE.TL_INSCOMP_6,
                   ISNULL(TRLFILE.TL_INSOURS_6,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_6,
                   TRLFILE.TL_INSCOMM_6,
                   TRLFILE.TL_INSEARNINGS_6,
                   TRLFILE.TL_INSANTICERN_6,
                   TRLFILE.TL_INSCOMP_7,
                   ISNULL(TRLFILE.TL_INSOURS_7,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_7,
                   TRLFILE.TL_INSCOMM_7,
                   TRLFILE.TL_INSEARNINGS_7,
                   TRLFILE.TL_INSANTICERN_7,
                   TRLFILE.TL_INSCOMP_8,
                   ISNULL(TRLFILE.TL_INSOURS_8,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_8,
                   TRLFILE.TL_INSCOMM_8,
                   TRLFILE.TL_INSEARNINGS_8,
                   TRLFILE.TL_INSANTICERN_8,
                   TRLFILE.TL_INSCOMP_9,
                   ISNULL(TRLFILE.TL_INSOURS_9,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_9,
                   TRLFILE.TL_INSCOMM_9,
                   TRLFILE.TL_INSEARNINGS_9,
                   TRLFILE.TL_INSANTICERN_9,
                   TRLFILE.TL_INSCOMP_10,
                   ISNULL(TRLFILE.TL_INSOURS_10,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
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
                   ISNULL(TRLFILE.TL_LPEOD5_FG,''),
                   TRLFILE.TL_STATE_WORKER,
                   TRLFILE.TL_RESTRICTD_ACCTNO,
                   TRLFILE.TL_SPRCLASS,
                   ISNULL(TRLFILE.TL_USERID,''),
                   ISNULL(CAST(TRLFILE.TL_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_POSTTIME,
                   ISNULL(TRLFILE.TL_GIFTNET,''),
                   TRLFILE.TL_APR,
                   ISNULL(TRLFILE.TL_SP_LNCOST_FRMLA,''),
                   ISNULL(TRLFILE.TL_MIP_FG,''),
                   ISNULL(TRLFILE.TL_INS_TAX_COUNTY_CD,''),
                   ISNULL(TRLFILE.TL_INS_TAX_CITY_CD,''),
                   TRLFILE.TL_PRDLDISC_UNEARN,
                   TRLFILE.TL_POINTS,
                   TRLFILE.TL_TAX_AMT,
                   ISNULL(TRLFILE.TL_PBI_PBD_FG,''),
                   ISNULL(TRLFILE.TL_APPROVED_BY_USER,''),
                   ISNULL(TRLFILE.TL_APPROVED_BY_CODE,''),
                   ISNULL(CAST(TRLFILE.TL_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_APPROVED_BY_TIME,
                   TRLFILE.TL_DEBIT_CARD_AMT,
                   ISNULL(TRLFILE.TL_ACTIONCD2,'')
            FROM DBO.TRLFILE
            WHERE TRLFILE.TL_BRNO     = :QTRL1-WBEG-BRNO
              AND TRLFILE.TL_DATE    >= :QTRL1-WBEG-DATE
              AND TRLFILE.TL_DATE    <= :QTRL1-WEND-DATE
              AND TRLFILE.TL_CLASS   >= :QTRL1-WBEG-CLASS
              AND TRLFILE.TL_CLASS   <= :QTRL1-WEND-CLASS
              AND TRLFILE.TL_ACCTNO  >= :QTRL1-WBEG-ACCTNO
              AND TRLFILE.TL_ACCTNO  <= :QTRL1-WEND-ACCTNO
              AND TRLFILE.TL_LAWCODE >= :QTRL1-WBEG-LAWCODE
              AND TRLFILE.TL_LAWCODE <= :QTRL1-WEND-LAWCODE
              AND TRLFILE.TL_LINE    >= :QTRL1-WBEG-LINE
              AND TRLFILE.TL_LINE    <= :QTRL1-WEND-LINE
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
       START-TRL1-FILE-SCAN.
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-SC-STAT-GOOD )
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
                                           QTRL1-WEND-LINE
      
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }        

              MOVE TL-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WSBEG-DATE  
 
              IF ( TL-CLASS = 999 )
                 MOVE TL-CLASS          TO QTRL1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRL1-WSBEG-CLASS = TL-CLASS + 1
              END-IF

              IF ( TL-ACCTNO = 99999999 )
                 MOVE TL-ACCTNO         TO QTRL1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRL1-WSBEG-ACCTNO = TL-ACCTNO + 1 
              END-IF

              IF ( TL-LAWCODE = 99 )
                 MOVE TL-LAWCODE        TO QTRL1-WSBEG-LAWCODE
              ELSE
                 COMPUTE QTRL1-WSBEG-LAWCODE = TL-LAWCODE + 1.

           EXEC SQL
            DECLARE TRL1_CURSOR_SC CURSOR FOR
            SELECT TRLFILE.TL_BRNO,
                   ISNULL(CAST(TRLFILE.TL_DATE AS VARCHAR(10)),'0'),
                   TRLFILE.TL_CLASS,
                   TRLFILE.TL_ACCTNO,
                   TRLFILE.TL_LAWCODE,
                   TRLFILE.TL_LINE,
                   ISNULL(CAST(TRLFILE.TL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_SSNO_1,
                   ISNULL(TRLFILE.TL_MAKERCD_1,''),
                   TRLFILE.TL_SSNO_2,
                   ISNULL(TRLFILE.TL_MAKERCD_2,''),
                   TRLFILE.TL_SSNO_3,
                   ISNULL(TRLFILE.TL_MAKERCD_3,''),
                   TRLFILE.TL_SSNO_4,
                   ISNULL(TRLFILE.TL_MAKERCD_4,''),
                   ISNULL(TRLFILE.TL_PURCD,''),
                   ISNULL(TRLFILE.TL_SRCD,''),
                   ISNULL(TRLFILE.TL_COLLCD,''),
                   ISNULL(TRLFILE.TL_SERVACCT,''),
                   ISNULL(TRLFILE.TL_LOANTYPE,''),
                   TRLFILE.TL_SUBCLASS,
                   ISNULL(CAST(TRLFILE.TL_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INTDATE AS VARCHAR(10)),'0'),
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
                   ISNULL(TRLFILE.TL_FEECODE_1,''),
                   TRLFILE.TL_FEEAMT_1,
                   ISNULL(TRLFILE.TL_FEECODE_2,''),
                   TRLFILE.TL_FEEAMT_2,
                   ISNULL(TRLFILE.TL_FEECODE_3,''),
                   TRLFILE.TL_FEEAMT_3,
                   ISNULL(TRLFILE.TL_FEECODE_4,''),
                   TRLFILE.TL_FEEAMT_4,
                   ISNULL(TRLFILE.TL_FEECODE_5,''),
                   TRLFILE.TL_FEEAMT_5,
                   ISNULL(TRLFILE.TL_FEECODE_6,''),
                   TRLFILE.TL_FEEAMT_6,
                   ISNULL(TRLFILE.TL_FEECODE_7,''),
                   TRLFILE.TL_FEEAMT_7,
                   ISNULL(TRLFILE.TL_FEECODE_8,''),
                   TRLFILE.TL_FEEAMT_8,
                   ISNULL(TRLFILE.TL_FEECODE_9,''),
                   TRLFILE.TL_FEEAMT_9,
                   ISNULL(TRLFILE.TL_FEECODE_10,''),
                   TRLFILE.TL_FEEAMT_10,
                   ISNULL(TRLFILE.TL_FEECODE_11,''),
                   TRLFILE.TL_FEEAMT_11,
                   ISNULL(TRLFILE.TL_FEECODE_12,''),
                   TRLFILE.TL_FEEAMT_12,
                   ISNULL(TRLFILE.TL_FEECODE_13,''),
                   TRLFILE.TL_FEEAMT_13,
                   ISNULL(TRLFILE.TL_FEECODE_14,''),
                   TRLFILE.TL_FEEAMT_14,
                   ISNULL(TRLFILE.TL_FEECODE_15,''),
                   TRLFILE.TL_FEEAMT_15,
                   ISNULL(TRLFILE.TL_FEECODE_16,''),
                   TRLFILE.TL_FEEAMT_16,
                   ISNULL(TRLFILE.TL_FEECODE_17,''),
                   TRLFILE.TL_FEEAMT_17,
                   ISNULL(TRLFILE.TL_FEECODE_18,''),
                   TRLFILE.TL_FEEAMT_18,
                   ISNULL(TRLFILE.TL_FEECODE_19,''),
                   TRLFILE.TL_FEEAMT_19,
                   ISNULL(TRLFILE.TL_FEECODE_20,''),
                   TRLFILE.TL_FEEAMT_20,
                   ISNULL(TRLFILE.TL_INSURED,''),
                   ISNULL(TRLFILE.TL_INSTYPES,''),
                   ISNULL(TRLFILE.TL_INSELIGIBLE,''),
                   TRLFILE.TL_INSCOMP_1,
                   ISNULL(TRLFILE.TL_INSOURS_1,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_1,
                   TRLFILE.TL_INSCOMM_1,
                   TRLFILE.TL_INSEARNINGS_1,
                   TRLFILE.TL_INSANTICERN_1,
                   TRLFILE.TL_INSCOMP_2,
                   ISNULL(TRLFILE.TL_INSOURS_2,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_2,
                   TRLFILE.TL_INSCOMM_2,
                   TRLFILE.TL_INSEARNINGS_2,
                   TRLFILE.TL_INSANTICERN_2,
                   TRLFILE.TL_INSCOMP_3,
                   ISNULL(TRLFILE.TL_INSOURS_3,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_3,
                   TRLFILE.TL_INSCOMM_3,
                   TRLFILE.TL_INSEARNINGS_3,
                   TRLFILE.TL_INSANTICERN_3,
                   TRLFILE.TL_INSCOMP_4,
                   ISNULL(TRLFILE.TL_INSOURS_4,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_4,
                   TRLFILE.TL_INSCOMM_4,
                   TRLFILE.TL_INSEARNINGS_4,
                   TRLFILE.TL_INSANTICERN_4,
                   TRLFILE.TL_INSCOMP_5,
                   ISNULL(TRLFILE.TL_INSOURS_5,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_5,
                   TRLFILE.TL_INSCOMM_5,
                   TRLFILE.TL_INSEARNINGS_5,
                   TRLFILE.TL_INSANTICERN_5,
                   TRLFILE.TL_INSCOMP_6,
                   ISNULL(TRLFILE.TL_INSOURS_6,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_6,
                   TRLFILE.TL_INSCOMM_6,
                   TRLFILE.TL_INSEARNINGS_6,
                   TRLFILE.TL_INSANTICERN_6,
                   TRLFILE.TL_INSCOMP_7,
                   ISNULL(TRLFILE.TL_INSOURS_7,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_7,
                   TRLFILE.TL_INSCOMM_7,
                   TRLFILE.TL_INSEARNINGS_7,
                   TRLFILE.TL_INSANTICERN_7,
                   TRLFILE.TL_INSCOMP_8,
                   ISNULL(TRLFILE.TL_INSOURS_8,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_8,
                   TRLFILE.TL_INSCOMM_8,
                   TRLFILE.TL_INSEARNINGS_8,
                   TRLFILE.TL_INSANTICERN_8,
                   TRLFILE.TL_INSCOMP_9,
                   ISNULL(TRLFILE.TL_INSOURS_9,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_9,
                   TRLFILE.TL_INSCOMM_9,
                   TRLFILE.TL_INSEARNINGS_9,
                   TRLFILE.TL_INSANTICERN_9,
                   TRLFILE.TL_INSCOMP_10,
                   ISNULL(TRLFILE.TL_INSOURS_10,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
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
                   ISNULL(TRLFILE.TL_LPEOD5_FG,''),
                   TRLFILE.TL_STATE_WORKER,
                   TRLFILE.TL_RESTRICTD_ACCTNO,
                   TRLFILE.TL_SPRCLASS,
                   ISNULL(TRLFILE.TL_USERID,''),
                   ISNULL(CAST(TRLFILE.TL_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_POSTTIME,
                   ISNULL(TRLFILE.TL_GIFTNET,''),
                   TRLFILE.TL_APR,
                   ISNULL(TRLFILE.TL_SP_LNCOST_FRMLA,''),
                   ISNULL(TRLFILE.TL_MIP_FG,''),
                   ISNULL(TRLFILE.TL_INS_TAX_COUNTY_CD,''),
                   ISNULL(TRLFILE.TL_INS_TAX_CITY_CD,''),
                   TRLFILE.TL_PRDLDISC_UNEARN,
                   TRLFILE.TL_POINTS,
                   TRLFILE.TL_TAX_AMT,
                   ISNULL(TRLFILE.TL_PBI_PBD_FG,''),
                   ISNULL(TRLFILE.TL_APPROVED_BY_USER,''),
                   ISNULL(TRLFILE.TL_APPROVED_BY_CODE,''),
                   ISNULL(CAST(TRLFILE.TL_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_APPROVED_BY_TIME,
                   TRLFILE.TL_DEBIT_CARD_AMT,
                   ISNULL(TRLFILE.TL_ACTIONCD2,'')
            FROM DBO.TRLFILE
            WHERE (  TRLFILE.TL_BRNO     = :QTRL1-WBEG-BRNO     )
              AND (( TRLFILE.TL_DATE    >= :QTRL1-WBEG-DATE
              AND    TRLFILE.TL_CLASS   >= :QTRL1-WBEG-CLASS
              AND    TRLFILE.TL_ACCTNO  >= :QTRL1-WBEG-ACCTNO
              AND    TRLFILE.TL_LAWCODE >= :QTRL1-WBEG-LAWCODE
              AND    TRLFILE.TL_LINE    >= :QTRL1-WBEG-LINE    )
               OR  ( TRLFILE.TL_DATE    >= :QTRL1-WBEG-DATE
              AND    TRLFILE.TL_CLASS   >= :QTRL1-WBEG-CLASS
              AND    TRLFILE.TL_ACCTNO  >= :QTRL1-WBEG-ACCTNO
              AND    TRLFILE.TL_LAWCODE >= :QTRL1-WSBEG-LAWCODE
              AND    TRLFILE.TL_LINE    >= 0                   )
               OR  ( TRLFILE.TL_DATE    >= :QTRL1-WBEG-DATE
              AND    TRLFILE.TL_CLASS   >= :QTRL1-WBEG-CLASS
              AND    TRLFILE.TL_ACCTNO  >= :QTRL1-WSBEG-ACCTNO
              AND    TRLFILE.TL_LAWCODE >= 0
              AND    TRLFILE.TL_LINE    >= 0                   )
               OR  ( TRLFILE.TL_DATE    >= :QTRL1-WBEG-DATE
              AND    TRLFILE.TL_CLASS   >= :QTRL1-WSBEG-CLASS
              AND    TRLFILE.TL_ACCTNO  >= 0
              AND    TRLFILE.TL_LAWCODE >= 0
              AND    TRLFILE.TL_LINE    >= 0                   )
               OR  ( TRLFILE.TL_DATE    >= :QTRL1-WSBEG-DATE
              AND    TRLFILE.TL_CLASS   >= 0
              AND    TRLFILE.TL_ACCTNO  >= 0
              AND    TRLFILE.TL_LAWCODE >= 0
              AND    TRLFILE.TL_LINE    >= 0                   ))
              AND (  TRLFILE.TL_DATE    <= :QTRL1-WEND-DATE
              AND    TRLFILE.TL_CLASS   <= :QTRL1-WEND-CLASS
              AND    TRLFILE.TL_ACCTNO  <= :QTRL1-WEND-ACCTNO
              AND    TRLFILE.TL_LAWCODE <= :QTRL1-WEND-LAWCODE
              AND    TRLFILE.TL_LINE    <= :QTRL1-WEND-LINE     )
            ORDER BY TRLFILE.TL_BRNO,
                     TRLFILE.TL_DATE,
                     TRLFILE.TL_CLASS,
                     TRLFILE.TL_ACCTNO,
                     TRLFILE.TL_LAWCODE,
                     TRLFILE.TL_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR_SC
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


           MOVE STAT---GOOD TO TRL1-CURSOR-SC-STAT.

      *-----------------------------------------------------------------
       READ-TRL1-FILE.
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO. 
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

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
                   ISNULL(CAST(TRLFILE.TL_DATE AS VARCHAR(10)),'0'),
                   TRLFILE.TL_CLASS,
                   TRLFILE.TL_ACCTNO,
                   TRLFILE.TL_LAWCODE,
                   TRLFILE.TL_LINE,
                   ISNULL(CAST(TRLFILE.TL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_SSNO_1,
                   ISNULL(TRLFILE.TL_MAKERCD_1,''),
                   TRLFILE.TL_SSNO_2,
                   ISNULL(TRLFILE.TL_MAKERCD_2,''),
                   TRLFILE.TL_SSNO_3,
                   ISNULL(TRLFILE.TL_MAKERCD_3,''),
                   TRLFILE.TL_SSNO_4,
                   ISNULL(TRLFILE.TL_MAKERCD_4,''),
                   ISNULL(TRLFILE.TL_PURCD,''),
                   ISNULL(TRLFILE.TL_SRCD,''),
                   ISNULL(TRLFILE.TL_COLLCD,''),
                   ISNULL(TRLFILE.TL_SERVACCT,''),
                   ISNULL(TRLFILE.TL_LOANTYPE,''),
                   TRLFILE.TL_SUBCLASS,
                   ISNULL(CAST(TRLFILE.TL_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INTDATE AS VARCHAR(10)),'0'),
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
                   ISNULL(TRLFILE.TL_FEECODE_1,''),
                   TRLFILE.TL_FEEAMT_1,
                   ISNULL(TRLFILE.TL_FEECODE_2,''),
                   TRLFILE.TL_FEEAMT_2,
                   ISNULL(TRLFILE.TL_FEECODE_3,''),
                   TRLFILE.TL_FEEAMT_3,
                   ISNULL(TRLFILE.TL_FEECODE_4,''),
                   TRLFILE.TL_FEEAMT_4,
                   ISNULL(TRLFILE.TL_FEECODE_5,''),
                   TRLFILE.TL_FEEAMT_5,
                   ISNULL(TRLFILE.TL_FEECODE_6,''),
                   TRLFILE.TL_FEEAMT_6,
                   ISNULL(TRLFILE.TL_FEECODE_7,''),
                   TRLFILE.TL_FEEAMT_7,
                   ISNULL(TRLFILE.TL_FEECODE_8,''),
                   TRLFILE.TL_FEEAMT_8,
                   ISNULL(TRLFILE.TL_FEECODE_9,''),
                   TRLFILE.TL_FEEAMT_9,
                   ISNULL(TRLFILE.TL_FEECODE_10,''),
                   TRLFILE.TL_FEEAMT_10,
                   ISNULL(TRLFILE.TL_FEECODE_11,''),
                   TRLFILE.TL_FEEAMT_11,
                   ISNULL(TRLFILE.TL_FEECODE_12,''),
                   TRLFILE.TL_FEEAMT_12,
                   ISNULL(TRLFILE.TL_FEECODE_13,''),
                   TRLFILE.TL_FEEAMT_13,
                   ISNULL(TRLFILE.TL_FEECODE_14,''),
                   TRLFILE.TL_FEEAMT_14,
                   ISNULL(TRLFILE.TL_FEECODE_15,''),
                   TRLFILE.TL_FEEAMT_15,
                   ISNULL(TRLFILE.TL_FEECODE_16,''),
                   TRLFILE.TL_FEEAMT_16,
                   ISNULL(TRLFILE.TL_FEECODE_17,''),
                   TRLFILE.TL_FEEAMT_17,
                   ISNULL(TRLFILE.TL_FEECODE_18,''),
                   TRLFILE.TL_FEEAMT_18,
                   ISNULL(TRLFILE.TL_FEECODE_19,''),
                   TRLFILE.TL_FEEAMT_19,
                   ISNULL(TRLFILE.TL_FEECODE_20,''),
                   TRLFILE.TL_FEEAMT_20,
                   ISNULL(TRLFILE.TL_INSURED,''),
                   ISNULL(TRLFILE.TL_INSTYPES,''),
                   ISNULL(TRLFILE.TL_INSELIGIBLE,''),
                   TRLFILE.TL_INSCOMP_1,
                   ISNULL(TRLFILE.TL_INSOURS_1,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_1,
                   TRLFILE.TL_INSCOMM_1,
                   TRLFILE.TL_INSEARNINGS_1,
                   TRLFILE.TL_INSANTICERN_1,
                   TRLFILE.TL_INSCOMP_2,
                   ISNULL(TRLFILE.TL_INSOURS_2,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_2,
                   TRLFILE.TL_INSCOMM_2,
                   TRLFILE.TL_INSEARNINGS_2,
                   TRLFILE.TL_INSANTICERN_2,
                   TRLFILE.TL_INSCOMP_3,
                   ISNULL(TRLFILE.TL_INSOURS_3,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_3,
                   TRLFILE.TL_INSCOMM_3,
                   TRLFILE.TL_INSEARNINGS_3,
                   TRLFILE.TL_INSANTICERN_3,
                   TRLFILE.TL_INSCOMP_4,
                   ISNULL(TRLFILE.TL_INSOURS_4,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_4,
                   TRLFILE.TL_INSCOMM_4,
                   TRLFILE.TL_INSEARNINGS_4,
                   TRLFILE.TL_INSANTICERN_4,
                   TRLFILE.TL_INSCOMP_5,
                   ISNULL(TRLFILE.TL_INSOURS_5,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_5,
                   TRLFILE.TL_INSCOMM_5,
                   TRLFILE.TL_INSEARNINGS_5,
                   TRLFILE.TL_INSANTICERN_5,
                   TRLFILE.TL_INSCOMP_6,
                   ISNULL(TRLFILE.TL_INSOURS_6,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_6,
                   TRLFILE.TL_INSCOMM_6,
                   TRLFILE.TL_INSEARNINGS_6,
                   TRLFILE.TL_INSANTICERN_6,
                   TRLFILE.TL_INSCOMP_7,
                   ISNULL(TRLFILE.TL_INSOURS_7,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_7,
                   TRLFILE.TL_INSCOMM_7,
                   TRLFILE.TL_INSEARNINGS_7,
                   TRLFILE.TL_INSANTICERN_7,
                   TRLFILE.TL_INSCOMP_8,
                   ISNULL(TRLFILE.TL_INSOURS_8,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_8,
                   TRLFILE.TL_INSCOMM_8,
                   TRLFILE.TL_INSEARNINGS_8,
                   TRLFILE.TL_INSANTICERN_8,
                   TRLFILE.TL_INSCOMP_9,
                   ISNULL(TRLFILE.TL_INSOURS_9,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_9,
                   TRLFILE.TL_INSCOMM_9,
                   TRLFILE.TL_INSEARNINGS_9,
                   TRLFILE.TL_INSANTICERN_9,
                   TRLFILE.TL_INSCOMP_10,
                   ISNULL(TRLFILE.TL_INSOURS_10,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
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
                   ISNULL(TRLFILE.TL_LPEOD5_FG,''),
                   TRLFILE.TL_STATE_WORKER,
                   TRLFILE.TL_RESTRICTD_ACCTNO,
                   TRLFILE.TL_SPRCLASS,
                   ISNULL(TRLFILE.TL_USERID,''),
                   ISNULL(CAST(TRLFILE.TL_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_POSTTIME,
                   ISNULL(TRLFILE.TL_GIFTNET,''),
                   TRLFILE.TL_APR,
                   ISNULL(TRLFILE.TL_SP_LNCOST_FRMLA,''),
                   ISNULL(TRLFILE.TL_MIP_FG,''),
                   ISNULL(TRLFILE.TL_INS_TAX_COUNTY_CD,''),
                   ISNULL(TRLFILE.TL_INS_TAX_CITY_CD,''),
                   TRLFILE.TL_PRDLDISC_UNEARN,
                   TRLFILE.TL_POINTS,
                   TRLFILE.TL_TAX_AMT,
                   ISNULL(TRLFILE.TL_PBI_PBD_FG,''),
                   ISNULL(TRLFILE.TL_APPROVED_BY_USER,''),
                   ISNULL(TRLFILE.TL_APPROVED_BY_CODE,''),
                   ISNULL(CAST(TRLFILE.TL_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_APPROVED_BY_TIME,
                   TRLFILE.TL_DEBIT_CARD_AMT,
                   ISNULL(TRLFILE.TL_ACTIONCD2,'')
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
              IF ( TRL1-CURSOR-SC-STAT-GOOD )
                 EXEC SQL
                      FETCH TRL1_CURSOR_SC INTO :QTRL-REC
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
       CLOSE-TRL1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRL1-CURSOR-STAT.

            IF ( TRL1-CURSOR-SC-STAT-GOOD )
               EXEC SQL
                    CLOSE TRL1_CURSOR_SC
               END-EXEC
               MOVE STAT---BAD TO TRL1-CURSOR-SC-STAT.

