      * COPYMEMBER: LIBLP/LPLC1RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LC-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LC-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO LC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LC-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO LC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH.

      *-----------------------------------------------------------------
       OPEN-LC1-FILE.
           PERFORM LOAD-LC1-FILE.
           PERFORM OPEN-IT.
           MOVE LC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       COUNT-LC1-FILE.
           PERFORM COUNT-IT.
           MOVE LC-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.

           EXEC SQL
            SELECT COUNT(*)
            INTO :LC1-ROW-COUNT
            FROM DBO.LCFILE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO COUNT-LC1-FILE.

      *-----------------------------------------------------------------
       START-LC1-FILE.
           PERFORM START-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           IF ( LC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL
            DECLARE LC1_CURSOR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   CAST(LCFILE.LC_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LCFILE.LC_DESC,
                   LCFILE.LC_SMALLOAN,
                   LCFILE.LC_SALEFIN,
                   LCFILE.LC_SERVACCT,
                   LCFILE.LC_LOANTYPE,
                   LCFILE.LC_PROF1,
                   LCFILE.LC_PROF2,
                   LCFILE.LC_SKIP_TRW,
                   LCFILE.LC_DISPLAY_ERN,
                   LCFILE.LC_LCARDFG,
                   LCFILE.LC_CALC_HI_CREDIT,
                   LCFILE.LC_STOP_ON_DLNO,
                   LCFILE.LC_SALES_FORCED_ENTRY,
                   LCFILE.LC_AFP_CLASS,
                   LCFILE.LC_CON_PDUE_DAYS,
                   LCFILE.LC_REC_PDUE_DAYS,
                   LCFILE.LC_GIFTNET,
                   LCFILE.LC_APP_REQ_FOR_LOAN,
                   LCFILE.LC_POOLID,
                   LCFILE.LC_USE_ALT_GL_FEE,
                   LCFILE.LC_PROGRP_CLASS,
                   LCFILE.LC_ALLOW_CASH_PROCEEDS
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >= :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
            ORDER BY LCFILE.LC_CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-LC1-FILE-GREATER.
           PERFORM START-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           IF ( LC1-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL
            DECLARE LC1_CURSOR_GR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   CAST(LCFILE.LC_LTOUCH_DATE
                                                   AS VARCHAR(10)),
                   LCFILE.LC_DESC,
                   LCFILE.LC_SMALLOAN,
                   LCFILE.LC_SALEFIN,
                   LCFILE.LC_SERVACCT,
                   LCFILE.LC_LOANTYPE,
                   LCFILE.LC_PROF1,
                   LCFILE.LC_PROF2,
                   LCFILE.LC_SKIP_TRW,
                   LCFILE.LC_DISPLAY_ERN,
                   LCFILE.LC_LCARDFG,
                   LCFILE.LC_CALC_HI_CREDIT,
                   LCFILE.LC_STOP_ON_DLNO,
                   LCFILE.LC_SALES_FORCED_ENTRY,
                   LCFILE.LC_AFP_CLASS,
                   LCFILE.LC_CON_PDUE_DAYS,
                   LCFILE.LC_REC_PDUE_DAYS,
                   LCFILE.LC_GIFTNET,
                   LCFILE.LC_APP_REQ_FOR_LOAN,
                   LCFILE.LC_POOLID,
                   LCFILE.LC_USE_ALT_GL_FEE,
                   LCFILE.LC_PROGRP_CLASS,
                   LCFILE.LC_ALLOW_CASH_PROCEEDS
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >  :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
            ORDER BY LCFILE.LC_CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-LC1-FILE.
           PERFORM READ-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.
           INITIALIZE QLC-REC.

           MOVE LC-CODE TO QLC-CODE.

           EXEC SQL
            SELECT LCFILE.LC_CODE,
                   CAST(LCFILE.LC_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LCFILE.LC_DESC,
                   LCFILE.LC_SMALLOAN,
                   LCFILE.LC_SALEFIN,
                   LCFILE.LC_SERVACCT,
                   LCFILE.LC_LOANTYPE,
                   LCFILE.LC_PROF1,
                   LCFILE.LC_PROF2,
                   LCFILE.LC_SKIP_TRW,
                   LCFILE.LC_DISPLAY_ERN,
                   LCFILE.LC_LCARDFG,
                   LCFILE.LC_CALC_HI_CREDIT,
                   LCFILE.LC_STOP_ON_DLNO,
                   LCFILE.LC_SALES_FORCED_ENTRY,
                   LCFILE.LC_AFP_CLASS,
                   LCFILE.LC_CON_PDUE_DAYS,
                   LCFILE.LC_REC_PDUE_DAYS,
                   LCFILE.LC_GIFTNET,
                   LCFILE.LC_APP_REQ_FOR_LOAN,
                   LCFILE.LC_POOLID,
                   LCFILE.LC_USE_ALT_GL_FEE,
                   LCFILE.LC_PROGRP_CLASS,
                   LCFILE.LC_ALLOW_CASH_PROCEEDS
            INTO :QLC-REC
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE = :QLC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
      *    STILL LOADING FILE WORKERS LIKE E-WORKERS
      *-----------------------------------------------------------------
       READ-LC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.
           INITIALIZE QLC-REC.

           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_CURSOR INTO :QLC-REC
              END-EXEC
           ELSE
           IF ( LC1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_CURSOR_GR INTO :QLC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LC1-FILE.

           PERFORM CLOSE-IT.
           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LC1-CURSOR-STAT.

           IF ( LC1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO LC1-CURSOR-GR-STAT.


