      * COPYMEMBER: LIBLP/LPSS1RN
       LOAD-SS1-FILE.

      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO SS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SS-ALL-MACHINE
              MOVE SS-ALL-PATH             TO SS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO SS-ALL-BASE
              MOVE FILPATH-MACHINE         TO SS-ALL-MACHINE
              MOVE SS-ALL-PATH             TO SS-PATH.

      *-----------------------------------------------------------------
       OPEN-SS1-FILE.
           PERFORM LOAD-SS1-FILE.
           PERFORM OPEN-IT.
           MOVE SS-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SS1-FILE.
           PERFORM START-IT.
           MOVE SS-PATH-SQL TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.

           IF ( SS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SS1-FILE.

           IF ( QSS1-WEND-CLASS = ZEROES    ) OR
              ( QSS1-WEND-CLASS NOT NUMERIC )
              MOVE SS-DATE-YYYYMM TO QSS1-WBEG-DATE
              MOVE ALL "9"        TO QSS1-WEND-DATE
              MOVE SS-BRNO        TO QSS1-WBEG-BRNO
              MOVE ALL "9"        TO QSS1-WEND-BRNO
              MOVE SS-CLASS       TO QSS1-WBEG-CLASS
              MOVE ALL "9"        TO QSS1-WEND-CLASS.

           EXEC SQL
            DECLARE SS1_CURSOR CURSOR FOR
            SELECT SSFILE.SS_DATE_YYYYMM,
                   SSFILE.SS_BRNO,
                   SSFILE.SS_CLASS,
                   CAST(SSFILE.SS_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   SSFILE.SS_UNITS_1_1_1,
                   SSFILE.SS_OUTBAL_1_1_1,
                   SSFILE.SS_UNITS_1_1_2,
                   SSFILE.SS_OUTBAL_1_1_2,
                   SSFILE.SS_UNITS_1_1_3,
                   SSFILE.SS_OUTBAL_1_1_3,
                   SSFILE.SS_UNITS_1_1_4,
                   SSFILE.SS_OUTBAL_1_1_4,
                   SSFILE.SS_UNITS_1_1_5,
                   SSFILE.SS_OUTBAL_1_1_5,
                   SSFILE.SS_NO_PL_1_1_1,
                   SSFILE.SS_AMT_PL_1_1_1,
                   SSFILE.SS_NO_PL_1_1_2,
                   SSFILE.SS_AMT_PL_1_1_2,
                   SSFILE.SS_UNITS_1_2_1,
                   SSFILE.SS_OUTBAL_1_2_1,
                   SSFILE.SS_UNITS_1_2_2,
                   SSFILE.SS_OUTBAL_1_2_2,
                   SSFILE.SS_UNITS_1_2_3,
                   SSFILE.SS_OUTBAL_1_2_3,
                   SSFILE.SS_UNITS_1_2_4,
                   SSFILE.SS_OUTBAL_1_2_4,
                   SSFILE.SS_UNITS_1_2_5,
                   SSFILE.SS_OUTBAL_1_2_5,
                   SSFILE.SS_NO_PL_1_2_1,
                   SSFILE.SS_AMT_PL_1_2_1,
                   SSFILE.SS_NO_PL_1_2_2,
                   SSFILE.SS_AMT_PL_1_2_2,
                   SSFILE.SS_UNITS_2_1_1,
                   SSFILE.SS_OUTBAL_2_1_1,
                   SSFILE.SS_UNITS_2_1_2,
                   SSFILE.SS_OUTBAL_2_1_2,
                   SSFILE.SS_UNITS_2_1_3,
                   SSFILE.SS_OUTBAL_2_1_3,
                   SSFILE.SS_UNITS_2_1_4,
                   SSFILE.SS_OUTBAL_2_1_4,
                   SSFILE.SS_UNITS_2_1_5,
                   SSFILE.SS_OUTBAL_2_1_5,
                   SSFILE.SS_NO_PL_2_1_1,
                   SSFILE.SS_AMT_PL_2_1_1,
                   SSFILE.SS_NO_PL_2_1_2,
                   SSFILE.SS_AMT_PL_2_1_2,
                   SSFILE.SS_UNITS_2_2_1,
                   SSFILE.SS_OUTBAL_2_2_1,
                   SSFILE.SS_UNITS_2_2_2,
                   SSFILE.SS_OUTBAL_2_2_2,
                   SSFILE.SS_UNITS_2_2_3,
                   SSFILE.SS_OUTBAL_2_2_3,
                   SSFILE.SS_UNITS_2_2_4,
                   SSFILE.SS_OUTBAL_2_2_4,
                   SSFILE.SS_UNITS_2_2_5,
                   SSFILE.SS_OUTBAL_2_2_5,
                   SSFILE.SS_NO_PL_2_2_1,
                   SSFILE.SS_AMT_PL_2_2_1,
                   SSFILE.SS_NO_PL_2_2_2,
                   SSFILE.SS_AMT_PL_2_2_2,
                   SSFILE.SS_UNITS_3_1_1,
                   SSFILE.SS_OUTBAL_3_1_1,
                   SSFILE.SS_UNITS_3_1_2,
                   SSFILE.SS_OUTBAL_3_1_2,
                   SSFILE.SS_UNITS_3_1_3,
                   SSFILE.SS_OUTBAL_3_1_3,
                   SSFILE.SS_UNITS_3_1_4,
                   SSFILE.SS_OUTBAL_3_1_4,
                   SSFILE.SS_UNITS_3_1_5,
                   SSFILE.SS_OUTBAL_3_1_5,
                   SSFILE.SS_NO_PL_3_1_1,
                   SSFILE.SS_AMT_PL_3_1_1,
                   SSFILE.SS_NO_PL_3_1_2,
                   SSFILE.SS_AMT_PL_3_1_2,
                   SSFILE.SS_UNITS_3_2_1,
                   SSFILE.SS_OUTBAL_3_2_1,
                   SSFILE.SS_UNITS_3_2_2,
                   SSFILE.SS_OUTBAL_3_2_2,
                   SSFILE.SS_UNITS_3_2_3,
                   SSFILE.SS_OUTBAL_3_2_3,
                   SSFILE.SS_UNITS_3_2_4,
                   SSFILE.SS_OUTBAL_3_2_4,
                   SSFILE.SS_UNITS_3_2_5,
                   SSFILE.SS_OUTBAL_3_2_5,
                   SSFILE.SS_NO_PL_3_2_1,
                   SSFILE.SS_AMT_PL_3_2_1,
                   SSFILE.SS_NO_PL_3_2_2,
                   SSFILE.SS_AMT_PL_3_2_2,
                   SSFILE.SS_UNITS_4_1_1,
                   SSFILE.SS_OUTBAL_4_1_1,
                   SSFILE.SS_UNITS_4_1_2,
                   SSFILE.SS_OUTBAL_4_1_2,
                   SSFILE.SS_UNITS_4_1_3,
                   SSFILE.SS_OUTBAL_4_1_3,
                   SSFILE.SS_UNITS_4_1_4,
                   SSFILE.SS_OUTBAL_4_1_4,
                   SSFILE.SS_UNITS_4_1_5,
                   SSFILE.SS_OUTBAL_4_1_5,
                   SSFILE.SS_NO_PL_4_1_1,
                   SSFILE.SS_AMT_PL_4_1_1,
                   SSFILE.SS_NO_PL_4_1_2,
                   SSFILE.SS_AMT_PL_4_1_2,
                   SSFILE.SS_UNITS_4_2_1,
                   SSFILE.SS_OUTBAL_4_2_1,
                   SSFILE.SS_UNITS_4_2_2,
                   SSFILE.SS_OUTBAL_4_2_2,
                   SSFILE.SS_UNITS_4_2_3,
                   SSFILE.SS_OUTBAL_4_2_3,
                   SSFILE.SS_UNITS_4_2_4,
                   SSFILE.SS_OUTBAL_4_2_4,
                   SSFILE.SS_UNITS_4_2_5,
                   SSFILE.SS_OUTBAL_4_2_5,
                   SSFILE.SS_NO_PL_4_2_1,
                   SSFILE.SS_AMT_PL_4_2_1,
                   SSFILE.SS_NO_PL_4_2_2,
                   SSFILE.SS_AMT_PL_4_2_2,
                   SSFILE.SS_UNITS_5_1_1,
                   SSFILE.SS_OUTBAL_5_1_1,
                   SSFILE.SS_UNITS_5_1_2,
                   SSFILE.SS_OUTBAL_5_1_2,
                   SSFILE.SS_UNITS_5_1_3,
                   SSFILE.SS_OUTBAL_5_1_3,
                   SSFILE.SS_UNITS_5_1_4,
                   SSFILE.SS_OUTBAL_5_1_4,
                   SSFILE.SS_UNITS_5_1_5,
                   SSFILE.SS_OUTBAL_5_1_5,
                   SSFILE.SS_NO_PL_5_1_1,
                   SSFILE.SS_AMT_PL_5_1_1,
                   SSFILE.SS_NO_PL_5_1_2,
                   SSFILE.SS_AMT_PL_5_1_2,
                   SSFILE.SS_UNITS_5_2_1,
                   SSFILE.SS_OUTBAL_5_2_1,
                   SSFILE.SS_UNITS_5_2_2,
                   SSFILE.SS_OUTBAL_5_2_2,
                   SSFILE.SS_UNITS_5_2_3,
                   SSFILE.SS_OUTBAL_5_2_3,
                   SSFILE.SS_UNITS_5_2_4,
                   SSFILE.SS_OUTBAL_5_2_4,
                   SSFILE.SS_UNITS_5_2_5,
                   SSFILE.SS_OUTBAL_5_2_5,
                   SSFILE.SS_NO_PL_5_2_1,
                   SSFILE.SS_AMT_PL_5_2_1,
                   SSFILE.SS_NO_PL_5_2_2,
                   SSFILE.SS_AMT_PL_5_2_2,
                   SSFILE.SS_UNITS_6_1_1,
                   SSFILE.SS_OUTBAL_6_1_1,
                   SSFILE.SS_UNITS_6_1_2,
                   SSFILE.SS_OUTBAL_6_1_2,
                   SSFILE.SS_UNITS_6_1_3,
                   SSFILE.SS_OUTBAL_6_1_3,
                   SSFILE.SS_UNITS_6_1_4,
                   SSFILE.SS_OUTBAL_6_1_4,
                   SSFILE.SS_UNITS_6_1_5,
                   SSFILE.SS_OUTBAL_6_1_5,
                   SSFILE.SS_NO_PL_6_1_1,
                   SSFILE.SS_AMT_PL_6_1_1,
                   SSFILE.SS_NO_PL_6_1_2,
                   SSFILE.SS_AMT_PL_6_1_2,
                   SSFILE.SS_UNITS_6_2_1,
                   SSFILE.SS_OUTBAL_6_2_1,
                   SSFILE.SS_UNITS_6_2_2,
                   SSFILE.SS_OUTBAL_6_2_2,
                   SSFILE.SS_UNITS_6_2_3,
                   SSFILE.SS_OUTBAL_6_2_3,
                   SSFILE.SS_UNITS_6_2_4,
                   SSFILE.SS_OUTBAL_6_2_4,
                   SSFILE.SS_UNITS_6_2_5,
                   SSFILE.SS_OUTBAL_6_2_5,
                   SSFILE.SS_NO_PL_6_2_1,
                   SSFILE.SS_AMT_PL_6_2_1,
                   SSFILE.SS_NO_PL_6_2_2,
                   SSFILE.SS_AMT_PL_6_2_2,
                   SSFILE.SS_UNITS_7_1_1,
                   SSFILE.SS_OUTBAL_7_1_1,
                   SSFILE.SS_UNITS_7_1_2,
                   SSFILE.SS_OUTBAL_7_1_2,
                   SSFILE.SS_UNITS_7_1_3,
                   SSFILE.SS_OUTBAL_7_1_3,
                   SSFILE.SS_UNITS_7_1_4,
                   SSFILE.SS_OUTBAL_7_1_4,
                   SSFILE.SS_UNITS_7_1_5,
                   SSFILE.SS_OUTBAL_7_1_5,
                   SSFILE.SS_NO_PL_7_1_1,
                   SSFILE.SS_AMT_PL_7_1_1,
                   SSFILE.SS_NO_PL_7_1_2,
                   SSFILE.SS_AMT_PL_7_1_2,
                   SSFILE.SS_UNITS_7_2_1,
                   SSFILE.SS_OUTBAL_7_2_1,
                   SSFILE.SS_UNITS_7_2_2,
                   SSFILE.SS_OUTBAL_7_2_2,
                   SSFILE.SS_UNITS_7_2_3,
                   SSFILE.SS_OUTBAL_7_2_3,
                   SSFILE.SS_UNITS_7_2_4,
                   SSFILE.SS_OUTBAL_7_2_4,
                   SSFILE.SS_UNITS_7_2_5,
                   SSFILE.SS_OUTBAL_7_2_5,
                   SSFILE.SS_NO_PL_7_2_1,
                   SSFILE.SS_AMT_PL_7_2_1,
                   SSFILE.SS_NO_PL_7_2_2,
                   SSFILE.SS_AMT_PL_7_2_2,
                   SSFILE.SS_UNITS_8_1_1,
                   SSFILE.SS_OUTBAL_8_1_1,
                   SSFILE.SS_UNITS_8_1_2,
                   SSFILE.SS_OUTBAL_8_1_2,
                   SSFILE.SS_UNITS_8_1_3,
                   SSFILE.SS_OUTBAL_8_1_3,
                   SSFILE.SS_UNITS_8_1_4,
                   SSFILE.SS_OUTBAL_8_1_4,
                   SSFILE.SS_UNITS_8_1_5,
                   SSFILE.SS_OUTBAL_8_1_5,
                   SSFILE.SS_NO_PL_8_1_1,
                   SSFILE.SS_AMT_PL_8_1_1,
                   SSFILE.SS_NO_PL_8_1_2,
                   SSFILE.SS_AMT_PL_8_1_2,
                   SSFILE.SS_UNITS_8_2_1,
                   SSFILE.SS_OUTBAL_8_2_1,
                   SSFILE.SS_UNITS_8_2_2,
                   SSFILE.SS_OUTBAL_8_2_2,
                   SSFILE.SS_UNITS_8_2_3,
                   SSFILE.SS_OUTBAL_8_2_3,
                   SSFILE.SS_UNITS_8_2_4,
                   SSFILE.SS_OUTBAL_8_2_4,
                   SSFILE.SS_UNITS_8_2_5,
                   SSFILE.SS_OUTBAL_8_2_5,
                   SSFILE.SS_NO_PL_8_2_1,
                   SSFILE.SS_AMT_PL_8_2_1,
                   SSFILE.SS_NO_PL_8_2_2,
                   SSFILE.SS_AMT_PL_8_2_2
            FROM DBO.SSFILE
            WHERE SSFILE.SS_DATE_YYYYMM >= :QSS1-WBEG-DATE
              AND SSFILE.SS_DATE_YYYYMM <= :QSS1-WEND-DATE
              AND SSFILE.SS_BRNO >= :QSS1-WBEG-BRNO
              AND SSFILE.SS_BRNO <= :QSS1-WEND-BRNO
              AND SSFILE.SS_CLASS >= :QSS1-WBEG-CLASS
              AND SSFILE.SS_CLASS <= :QSS1-WEND-CLASS
            ORDER BY SSFILE.SS_DATE_YYYYMM,
                     SSFILE.SS_BRNO,
                     SSFILE.SS_CLASS
           END-EXEC.

           EXEC SQL
               OPEN SS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QSS1-WBEG-DATE
                          QSS1-WEND-DATE
                          QSS1-WBEG-BRNO
                          QSS1-WEND-BRNO
                          QSS1-WBEG-CLASS
                          QSS1-WEND-CLASS.

           MOVE STAT---GOOD TO SS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SS1-FILE.
           PERFORM READ-IT.
           MOVE SS-PATH-SQL TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.
           INITIALIZE QSS-REC.

           MOVE SS-DATE-YYYYMM TO QSS-DATE-YYYYMM.
           MOVE SS-BRNO        TO QSS-BRNO.
           MOVE SS-CLASS       TO QSS-CLASS.

           EXEC SQL
            SELECT SSFILE.SS_DATE_YYYYMM,
                   SSFILE.SS_BRNO,
                   SSFILE.SS_CLASS,
                   CAST(SSFILE.SS_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   SSFILE.SS_UNITS_1_1_1,
                   SSFILE.SS_OUTBAL_1_1_1,
                   SSFILE.SS_UNITS_1_1_2,
                   SSFILE.SS_OUTBAL_1_1_2,
                   SSFILE.SS_UNITS_1_1_3,
                   SSFILE.SS_OUTBAL_1_1_3,
                   SSFILE.SS_UNITS_1_1_4,
                   SSFILE.SS_OUTBAL_1_1_4,
                   SSFILE.SS_UNITS_1_1_5,
                   SSFILE.SS_OUTBAL_1_1_5,
                   SSFILE.SS_NO_PL_1_1_1,
                   SSFILE.SS_AMT_PL_1_1_1,
                   SSFILE.SS_NO_PL_1_1_2,
                   SSFILE.SS_AMT_PL_1_1_2,
                   SSFILE.SS_UNITS_1_2_1,
                   SSFILE.SS_OUTBAL_1_2_1,
                   SSFILE.SS_UNITS_1_2_2,
                   SSFILE.SS_OUTBAL_1_2_2,
                   SSFILE.SS_UNITS_1_2_3,
                   SSFILE.SS_OUTBAL_1_2_3,
                   SSFILE.SS_UNITS_1_2_4,
                   SSFILE.SS_OUTBAL_1_2_4,
                   SSFILE.SS_UNITS_1_2_5,
                   SSFILE.SS_OUTBAL_1_2_5,
                   SSFILE.SS_NO_PL_1_2_1,
                   SSFILE.SS_AMT_PL_1_2_1,
                   SSFILE.SS_NO_PL_1_2_2,
                   SSFILE.SS_AMT_PL_1_2_2,
                   SSFILE.SS_UNITS_2_1_1,
                   SSFILE.SS_OUTBAL_2_1_1,
                   SSFILE.SS_UNITS_2_1_2,
                   SSFILE.SS_OUTBAL_2_1_2,
                   SSFILE.SS_UNITS_2_1_3,
                   SSFILE.SS_OUTBAL_2_1_3,
                   SSFILE.SS_UNITS_2_1_4,
                   SSFILE.SS_OUTBAL_2_1_4,
                   SSFILE.SS_UNITS_2_1_5,
                   SSFILE.SS_OUTBAL_2_1_5,
                   SSFILE.SS_NO_PL_2_1_1,
                   SSFILE.SS_AMT_PL_2_1_1,
                   SSFILE.SS_NO_PL_2_1_2,
                   SSFILE.SS_AMT_PL_2_1_2,
                   SSFILE.SS_UNITS_2_2_1,
                   SSFILE.SS_OUTBAL_2_2_1,
                   SSFILE.SS_UNITS_2_2_2,
                   SSFILE.SS_OUTBAL_2_2_2,
                   SSFILE.SS_UNITS_2_2_3,
                   SSFILE.SS_OUTBAL_2_2_3,
                   SSFILE.SS_UNITS_2_2_4,
                   SSFILE.SS_OUTBAL_2_2_4,
                   SSFILE.SS_UNITS_2_2_5,
                   SSFILE.SS_OUTBAL_2_2_5,
                   SSFILE.SS_NO_PL_2_2_1,
                   SSFILE.SS_AMT_PL_2_2_1,
                   SSFILE.SS_NO_PL_2_2_2,
                   SSFILE.SS_AMT_PL_2_2_2,
                   SSFILE.SS_UNITS_3_1_1,
                   SSFILE.SS_OUTBAL_3_1_1,
                   SSFILE.SS_UNITS_3_1_2,
                   SSFILE.SS_OUTBAL_3_1_2,
                   SSFILE.SS_UNITS_3_1_3,
                   SSFILE.SS_OUTBAL_3_1_3,
                   SSFILE.SS_UNITS_3_1_4,
                   SSFILE.SS_OUTBAL_3_1_4,
                   SSFILE.SS_UNITS_3_1_5,
                   SSFILE.SS_OUTBAL_3_1_5,
                   SSFILE.SS_NO_PL_3_1_1,
                   SSFILE.SS_AMT_PL_3_1_1,
                   SSFILE.SS_NO_PL_3_1_2,
                   SSFILE.SS_AMT_PL_3_1_2,
                   SSFILE.SS_UNITS_3_2_1,
                   SSFILE.SS_OUTBAL_3_2_1,
                   SSFILE.SS_UNITS_3_2_2,
                   SSFILE.SS_OUTBAL_3_2_2,
                   SSFILE.SS_UNITS_3_2_3,
                   SSFILE.SS_OUTBAL_3_2_3,
                   SSFILE.SS_UNITS_3_2_4,
                   SSFILE.SS_OUTBAL_3_2_4,
                   SSFILE.SS_UNITS_3_2_5,
                   SSFILE.SS_OUTBAL_3_2_5,
                   SSFILE.SS_NO_PL_3_2_1,
                   SSFILE.SS_AMT_PL_3_2_1,
                   SSFILE.SS_NO_PL_3_2_2,
                   SSFILE.SS_AMT_PL_3_2_2,
                   SSFILE.SS_UNITS_4_1_1,
                   SSFILE.SS_OUTBAL_4_1_1,
                   SSFILE.SS_UNITS_4_1_2,
                   SSFILE.SS_OUTBAL_4_1_2,
                   SSFILE.SS_UNITS_4_1_3,
                   SSFILE.SS_OUTBAL_4_1_3,
                   SSFILE.SS_UNITS_4_1_4,
                   SSFILE.SS_OUTBAL_4_1_4,
                   SSFILE.SS_UNITS_4_1_5,
                   SSFILE.SS_OUTBAL_4_1_5,
                   SSFILE.SS_NO_PL_4_1_1,
                   SSFILE.SS_AMT_PL_4_1_1,
                   SSFILE.SS_NO_PL_4_1_2,
                   SSFILE.SS_AMT_PL_4_1_2,
                   SSFILE.SS_UNITS_4_2_1,
                   SSFILE.SS_OUTBAL_4_2_1,
                   SSFILE.SS_UNITS_4_2_2,
                   SSFILE.SS_OUTBAL_4_2_2,
                   SSFILE.SS_UNITS_4_2_3,
                   SSFILE.SS_OUTBAL_4_2_3,
                   SSFILE.SS_UNITS_4_2_4,
                   SSFILE.SS_OUTBAL_4_2_4,
                   SSFILE.SS_UNITS_4_2_5,
                   SSFILE.SS_OUTBAL_4_2_5,
                   SSFILE.SS_NO_PL_4_2_1,
                   SSFILE.SS_AMT_PL_4_2_1,
                   SSFILE.SS_NO_PL_4_2_2,
                   SSFILE.SS_AMT_PL_4_2_2,
                   SSFILE.SS_UNITS_5_1_1,
                   SSFILE.SS_OUTBAL_5_1_1,
                   SSFILE.SS_UNITS_5_1_2,
                   SSFILE.SS_OUTBAL_5_1_2,
                   SSFILE.SS_UNITS_5_1_3,
                   SSFILE.SS_OUTBAL_5_1_3,
                   SSFILE.SS_UNITS_5_1_4,
                   SSFILE.SS_OUTBAL_5_1_4,
                   SSFILE.SS_UNITS_5_1_5,
                   SSFILE.SS_OUTBAL_5_1_5,
                   SSFILE.SS_NO_PL_5_1_1,
                   SSFILE.SS_AMT_PL_5_1_1,
                   SSFILE.SS_NO_PL_5_1_2,
                   SSFILE.SS_AMT_PL_5_1_2,
                   SSFILE.SS_UNITS_5_2_1,
                   SSFILE.SS_OUTBAL_5_2_1,
                   SSFILE.SS_UNITS_5_2_2,
                   SSFILE.SS_OUTBAL_5_2_2,
                   SSFILE.SS_UNITS_5_2_3,
                   SSFILE.SS_OUTBAL_5_2_3,
                   SSFILE.SS_UNITS_5_2_4,
                   SSFILE.SS_OUTBAL_5_2_4,
                   SSFILE.SS_UNITS_5_2_5,
                   SSFILE.SS_OUTBAL_5_2_5,
                   SSFILE.SS_NO_PL_5_2_1,
                   SSFILE.SS_AMT_PL_5_2_1,
                   SSFILE.SS_NO_PL_5_2_2,
                   SSFILE.SS_AMT_PL_5_2_2,
                   SSFILE.SS_UNITS_6_1_1,
                   SSFILE.SS_OUTBAL_6_1_1,
                   SSFILE.SS_UNITS_6_1_2,
                   SSFILE.SS_OUTBAL_6_1_2,
                   SSFILE.SS_UNITS_6_1_3,
                   SSFILE.SS_OUTBAL_6_1_3,
                   SSFILE.SS_UNITS_6_1_4,
                   SSFILE.SS_OUTBAL_6_1_4,
                   SSFILE.SS_UNITS_6_1_5,
                   SSFILE.SS_OUTBAL_6_1_5,
                   SSFILE.SS_NO_PL_6_1_1,
                   SSFILE.SS_AMT_PL_6_1_1,
                   SSFILE.SS_NO_PL_6_1_2,
                   SSFILE.SS_AMT_PL_6_1_2,
                   SSFILE.SS_UNITS_6_2_1,
                   SSFILE.SS_OUTBAL_6_2_1,
                   SSFILE.SS_UNITS_6_2_2,
                   SSFILE.SS_OUTBAL_6_2_2,
                   SSFILE.SS_UNITS_6_2_3,
                   SSFILE.SS_OUTBAL_6_2_3,
                   SSFILE.SS_UNITS_6_2_4,
                   SSFILE.SS_OUTBAL_6_2_4,
                   SSFILE.SS_UNITS_6_2_5,
                   SSFILE.SS_OUTBAL_6_2_5,
                   SSFILE.SS_NO_PL_6_2_1,
                   SSFILE.SS_AMT_PL_6_2_1,
                   SSFILE.SS_NO_PL_6_2_2,
                   SSFILE.SS_AMT_PL_6_2_2,
                   SSFILE.SS_UNITS_7_1_1,
                   SSFILE.SS_OUTBAL_7_1_1,
                   SSFILE.SS_UNITS_7_1_2,
                   SSFILE.SS_OUTBAL_7_1_2,
                   SSFILE.SS_UNITS_7_1_3,
                   SSFILE.SS_OUTBAL_7_1_3,
                   SSFILE.SS_UNITS_7_1_4,
                   SSFILE.SS_OUTBAL_7_1_4,
                   SSFILE.SS_UNITS_7_1_5,
                   SSFILE.SS_OUTBAL_7_1_5,
                   SSFILE.SS_NO_PL_7_1_1,
                   SSFILE.SS_AMT_PL_7_1_1,
                   SSFILE.SS_NO_PL_7_1_2,
                   SSFILE.SS_AMT_PL_7_1_2,
                   SSFILE.SS_UNITS_7_2_1,
                   SSFILE.SS_OUTBAL_7_2_1,
                   SSFILE.SS_UNITS_7_2_2,
                   SSFILE.SS_OUTBAL_7_2_2,
                   SSFILE.SS_UNITS_7_2_3,
                   SSFILE.SS_OUTBAL_7_2_3,
                   SSFILE.SS_UNITS_7_2_4,
                   SSFILE.SS_OUTBAL_7_2_4,
                   SSFILE.SS_UNITS_7_2_5,
                   SSFILE.SS_OUTBAL_7_2_5,
                   SSFILE.SS_NO_PL_7_2_1,
                   SSFILE.SS_AMT_PL_7_2_1,
                   SSFILE.SS_NO_PL_7_2_2,
                   SSFILE.SS_AMT_PL_7_2_2,
                   SSFILE.SS_UNITS_8_1_1,
                   SSFILE.SS_OUTBAL_8_1_1,
                   SSFILE.SS_UNITS_8_1_2,
                   SSFILE.SS_OUTBAL_8_1_2,
                   SSFILE.SS_UNITS_8_1_3,
                   SSFILE.SS_OUTBAL_8_1_3,
                   SSFILE.SS_UNITS_8_1_4,
                   SSFILE.SS_OUTBAL_8_1_4,
                   SSFILE.SS_UNITS_8_1_5,
                   SSFILE.SS_OUTBAL_8_1_5,
                   SSFILE.SS_NO_PL_8_1_1,
                   SSFILE.SS_AMT_PL_8_1_1,
                   SSFILE.SS_NO_PL_8_1_2,
                   SSFILE.SS_AMT_PL_8_1_2,
                   SSFILE.SS_UNITS_8_2_1,
                   SSFILE.SS_OUTBAL_8_2_1,
                   SSFILE.SS_UNITS_8_2_2,
                   SSFILE.SS_OUTBAL_8_2_2,
                   SSFILE.SS_UNITS_8_2_3,
                   SSFILE.SS_OUTBAL_8_2_3,
                   SSFILE.SS_UNITS_8_2_4,
                   SSFILE.SS_OUTBAL_8_2_4,
                   SSFILE.SS_UNITS_8_2_5,
                   SSFILE.SS_OUTBAL_8_2_5,
                   SSFILE.SS_NO_PL_8_2_1,
                   SSFILE.SS_AMT_PL_8_2_1,
                   SSFILE.SS_NO_PL_8_2_2,
                   SSFILE.SS_AMT_PL_8_2_2
            INTO :QSS-REC
            FROM DBO.SSFILE
            WHERE SSFILE.SS_DATE_YYYYMM = :QSS-DATE-YYYYMM
              AND SSFILE.SS_BRNO = :QSS-BRNO
              AND SSFILE.SS_CLASS = :QSS-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SS-FIELDS.

      *-----------------------------------------------------------------
       READ-SS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SS-PATH-SQL TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.
           INITIALIZE QSS-REC.

           IF ( SS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SS1_CURSOR INTO :QSS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SS-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SS1-FILE.
           PERFORM CLOSE-IT.
           IF ( SS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SS1-CURSOR-STAT.

