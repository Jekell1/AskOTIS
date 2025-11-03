      * COPYMEMBER: LIBLP/LPDM1RN
       LOAD-DM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( DM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( DM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO DM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO DM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO DM-ALL-DATA
              MOVE DM-ALL-PATH             TO DM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( DM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO DM-ALL-BASE
              MOVE FILPATH-MACHINE         TO DM-ALL-MACHINE
              MOVE FILPATH-DATA            TO DM-ALL-DATA
              MOVE DM-ALL-PATH             TO DM-PATH.

      *-----------------------------------------------------------------
       OPEN-DM1-FILE.
           PERFORM LOAD-DM1-FILE.
           PERFORM OPEN-IT.
           MOVE DM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DM1-FILE.
           PERFORM START-IT.
           MOVE DM-PATH-OWNBR  TO DM-BRNO.
           MOVE DM-PATH-SQL    TO E-FILE.
           MOVE DM1-KEY        TO E-KEYX.

           IF ( DM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DM1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QDM1-WEND-SEQ = ZEROES    ) OR
              ( QDM1-WEND-SEQ NOT NUMERIC )
              MOVE DM-PATH-OWNBR        TO QDM1-WBEG-BRNO
              MOVE DM-PATH-OWNBR        TO QDM1-WEND-BRNO
              MOVE DM-DL-DLNO           TO QDM1-WBEG-DLNO
              MOVE ALL "9"              TO QDM1-WEND-DLNO
              MOVE DM-DATE-REVERSED     TO QDM1-WBEG-DATE-REV
              MOVE ALL "9"              TO QDM1-WEND-DATE-REV
              MOVE DM-SEQ               TO QDM1-WBEG-SEQ
              MOVE ALL "9"              TO QDM1-WEND-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE DM1_CURSOR CURSOR FOR
            SELECT DMFILE.DM_BRNO,
                   DMFILE.DM_DL_DLNO,
                   DMFILE.DM_DATE_REVERSED,
                   DMFILE.DM_SEQ,
                   CAST(DMFILE.DM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   DMFILE.DM_LTOUCH_TIME_HHMM,
                   DMFILE.DM_LTOUCH_USERID,
                   DMFILE.DM_MEMO,
                   DMFILE.DM_MEMO_TYPE
            FROM DBO.DMFILE
            WHERE   DMFILE.DM_BRNO           = :QDM1-WBEG-BRNO
              AND ( DMFILE.DM_DL_DLNO       >= :QDM1-WBEG-DLNO
              AND   DMFILE.DM_DATE_REVERSED >= :QDM1-WBEG-DATE-REV
              AND   DMFILE.DM_SEQ           >= :QDM1-WBEG-SEQ      )
              AND ( DMFILE.DM_DL_DLNO       <= :QDM1-WEND-DLNO
              AND   DMFILE.DM_DATE_REVERSED <= :QDM1-WEND-DATE-REV
              AND   DMFILE.DM_SEQ           <= :QDM1-WEND-SEQ      )
            ORDER BY DMFILE.DM_BRNO,
                     DMFILE.DM_DL_DLNO,
                     DMFILE.DM_DATE_REVERSED,
                     DMFILE.DM_SEQ
           END-EXEC.

           EXEC SQL
               OPEN DM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDM1-WBEG-BRNO
                          QDM1-WEND-BRNO
                          QDM1-WBEG-DLNO
                          QDM1-WEND-DLNO
                          QDM1-WBEG-DATE-REV
                          QDM1-WEND-DATE-REV
                          QDM1-WBEG-SEQ
                          QDM1-WEND-SEQ.

           MOVE STAT---GOOD TO DM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DM1-FILE.
           PERFORM READ-IT.
           MOVE DM-PATH-OWNBR  TO DM-BRNO. 
           MOVE DM-PATH-SQL    TO E-FILE.
           MOVE DM1-KEY        TO E-KEYX.
           INITIALIZE QDM-REC.

           MOVE DM-BRNO          TO QDM-BRNO.
           MOVE DM-DL-DLNO       TO QDM-DL-DLNO.
           MOVE DM-DATE-REVERSED TO QDM-DATE-REVERSED.
           MOVE DM-SEQ           TO QDM-SEQ.

           EXEC SQL
            SELECT DMFILE.DM_BRNO,
                   DMFILE.DM_DL_DLNO,
                   DMFILE.DM_DATE_REVERSED,
                   DMFILE.DM_SEQ,
                   CAST(DMFILE.DM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   DMFILE.DM_LTOUCH_TIME_HHMM,
                   DMFILE.DM_LTOUCH_USERID,
                   DMFILE.DM_MEMO,
                   DMFILE.DM_MEMO_TYPE
            INTO :QDM-REC
            FROM DBO.DMFILE
            WHERE DMFILE.DM_BRNO = :QDM-BRNO
              AND DMFILE.DM_DL_DLNO = :QDM-DL-DLNO
              AND DMFILE.DM_DATE_REVERSED = :QDM-DATE-REVERSED
              AND DMFILE.DM_SEQ = :QDM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DM-FIELDS.

      *-----------------------------------------------------------------
       READ-DM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DM-PATH-OWNBR  TO DM-BRNO.
           MOVE DM-PATH-SQL    TO E-FILE.
           MOVE DM1-KEY        TO E-KEYX.
           INITIALIZE QDM-REC.

           IF ( DM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DM1_CURSOR INTO :QDM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DM-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-DM1-FILE.

           PERFORM CLOSE-IT.
           IF ( DM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DM1-CURSOR-STAT.

