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
                   ISNULL(CAST(LCFILE.LC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LCFILE.LC_DESC,''),
                   ISNULL(LCFILE.LC_SMALLOAN,''),
                   ISNULL(LCFILE.LC_SALEFIN,''),
                   ISNULL(LCFILE.LC_SERVACCT,''),
                   ISNULL(LCFILE.LC_LOANTYPE,''),
                   ISNULL(LCFILE.LC_PROF1,''),
                   ISNULL(LCFILE.LC_PROF2,''),
                   ISNULL(LCFILE.LC_SKIP_TRW,''),
                   ISNULL(LCFILE.LC_DISPLAY_ERN,''),
                   ISNULL(LCFILE.LC_LCARDFG,''),
                   ISNULL(LCFILE.LC_CALC_HI_CREDIT,''),
                   ISNULL(LCFILE.LC_STOP_ON_DLNO,''),
                   ISNULL(LCFILE.LC_SALES_FORCED_ENTRY,''),
                   ISNULL(LCFILE.LC_AFP_CLASS,''),
                   ISNULL(LCFILE.LC_CON_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_REC_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_GIFTNET,''),
                   ISNULL(LCFILE.LC_APP_REQ_FOR_LOAN,''),
                   LCFILE.LC_POOLID,
                   ISNULL(LCFILE.LC_USE_ALT_GL_FEE,''),
                   LCFILE.LC_PROGRP_CLASS,
                   ISNULL(LCFILE.LC_ALLOW_CASH_PROCEEDS,'')
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

           IF ( LC1-GR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL
            DECLARE LC1_GR_CURSOR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   ISNULL(CAST(LCFILE.LC_LTOUCH_DATE
                                                   AS VARCHAR(10)),'0'),
                   ISNULL(LCFILE.LC_DESC,''),
                   ISNULL(LCFILE.LC_SMALLOAN,''),
                   ISNULL(LCFILE.LC_SALEFIN,''),
                   ISNULL(LCFILE.LC_SERVACCT,''),
                   ISNULL(LCFILE.LC_LOANTYPE,''),
                   ISNULL(LCFILE.LC_PROF1,''),
                   ISNULL(LCFILE.LC_PROF2,''),
                   ISNULL(LCFILE.LC_SKIP_TRW,''),
                   ISNULL(LCFILE.LC_DISPLAY_ERN,''),
                   ISNULL(LCFILE.LC_LCARDFG,''),
                   ISNULL(LCFILE.LC_CALC_HI_CREDIT,''),
                   ISNULL(LCFILE.LC_STOP_ON_DLNO,''),
                   ISNULL(LCFILE.LC_SALES_FORCED_ENTRY,''),
                   ISNULL(LCFILE.LC_AFP_CLASS,''),
                   ISNULL(LCFILE.LC_CON_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_REC_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_GIFTNET,''),
                   ISNULL(LCFILE.LC_APP_REQ_FOR_LOAN,''),
                   LCFILE.LC_POOLID,
                   ISNULL(LCFILE.LC_USE_ALT_GL_FEE,''),
                   LCFILE.LC_PROGRP_CLASS,
                   ISNULL(LCFILE.LC_ALLOW_CASH_PROCEEDS,'')
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >  :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
            ORDER BY LCFILE.LC_CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_GR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-GR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LC1-FILE.
           PERFORM READ-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           MOVE LC-CODE TO QLC-CODE.

           EXEC SQL
            SELECT LCFILE.LC_CODE,
                   ISNULL(CAST(LCFILE.LC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LCFILE.LC_DESC,''),
                   ISNULL(LCFILE.LC_SMALLOAN,''),
                   ISNULL(LCFILE.LC_SALEFIN,''),
                   ISNULL(LCFILE.LC_SERVACCT,''),
                   ISNULL(LCFILE.LC_LOANTYPE,''),
                   ISNULL(LCFILE.LC_PROF1,''),
                   ISNULL(LCFILE.LC_PROF2,''),
                   ISNULL(LCFILE.LC_SKIP_TRW,''),
                   ISNULL(LCFILE.LC_DISPLAY_ERN,''),
                   ISNULL(LCFILE.LC_LCARDFG,''),
                   ISNULL(LCFILE.LC_CALC_HI_CREDIT,''),
                   ISNULL(LCFILE.LC_STOP_ON_DLNO,''),
                   ISNULL(LCFILE.LC_SALES_FORCED_ENTRY,''),
                   ISNULL(LCFILE.LC_AFP_CLASS,''),
                   ISNULL(LCFILE.LC_CON_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_REC_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_GIFTNET,''),
                   ISNULL(LCFILE.LC_APP_REQ_FOR_LOAN,''),
                   LCFILE.LC_POOLID,
                   ISNULL(LCFILE.LC_USE_ALT_GL_FEE,''),
                   LCFILE.LC_PROGRP_CLASS,
                   ISNULL(LCFILE.LC_ALLOW_CASH_PROCEEDS,'')
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
           IF ( LC1-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_GR_CURSOR INTO :QLC-REC
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

           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LC1-CURSOR-STAT.

           IF ( LC1-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_GR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LC1-GR-CURSOR-STAT.


