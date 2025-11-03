      * COPYMEMBER: LIBLP/LPIC1RN
       LOAD-IC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( IC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( IC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO IC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO IC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO IC-ALL-DATA
              MOVE IC-ALL-PATH             TO IC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( IC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO IC-ALL-BASE
              MOVE FILPATH-MACHINE         TO IC-ALL-MACHINE
              MOVE FILPATH-DATA            TO IC-ALL-DATA
              MOVE IC-ALL-PATH             TO IC-PATH.

      *-----------------------------------------------------------------
       OPEN-IC1-FILE.
           PERFORM LOAD-IC1-FILE.
           PERFORM OPEN-IT.
           MOVE IC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-IC1-FILE.
           PERFORM START-IT.
           MOVE IC-PATH-OWNBR  TO IC-BRNO.
           MOVE IC-PATH-SQL    TO E-FILE.
           MOVE IC1-KEY        TO E-KEYX.

           IF ( IC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-IC1-FILE.

           IF ( QIC1-WEND-CODE = ZEROES    ) OR
              ( QIC1-WEND-CODE NOT NUMERIC )
              MOVE IC-PATH-OWNBR        TO QIC1-WBEG-BRNO
              MOVE IC-PATH-OWNBR        TO QIC1-WEND-BRNO
              MOVE IC-CODE              TO QIC1-WBEG-CODE
              MOVE ALL "9"              TO QIC1-WEND-CODE.

           EXEC SQL
            DECLARE IC1_CURSOR CURSOR FOR
            SELECT ICFILE.IC_BRNO,
                   ICFILE.IC_CODE,
                   CAST(ICFILE.IC_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   ICFILE.IC_NAME,
                   ICFILE.IC_ADDR1,
                   ICFILE.IC_ADDR2,
                   ICFILE.IC_CITY,
                   ICFILE.IC_STATE,
                   ICFILE.IC_ZIP,
                   ICFILE.IC_PHONE,
                   ICFILE.IC_COMMRATE_1,
                   ICFILE.IC_COMMRATE_2,
                   ICFILE.IC_COMMRATE_3,
                   ICFILE.IC_COMMRATE_4,
                   ICFILE.IC_COMMRATE_5,
                   ICFILE.IC_COMMRATE_6,
                   ICFILE.IC_COMMRATE_7,
                   ICFILE.IC_COMMRATE_8,
                   ICFILE.IC_COMMRATE_9,
                   ICFILE.IC_COMMRATE_10,
                   ICFILE.IC_COMMRATE_11,
                   ICFILE.IC_COMMRATE_12,
                   ICFILE.IC_COMMRATE_13,
                   ICFILE.IC_COMMRATE_14
            FROM DBO.ICFILE
            WHERE ICFILE.IC_BRNO  = :QIC1-WBEG-BRNO
              AND ICFILE.IC_CODE >= :QIC1-WBEG-CODE
              AND ICFILE.IC_CODE <= :QIC1-WEND-CODE
            ORDER BY ICFILE.IC_BRNO,
                     ICFILE.IC_CODE
           END-EXEC.

           EXEC SQL
               OPEN IC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QIC1-WBEG-BRNO 
                          QIC1-WEND-BRNO
                          QIC1-WBEG-CODE
                          QIC1-WEND-CODE.

           MOVE STAT---GOOD TO IC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-IC1-FILE.
           PERFORM READ-IT.
           MOVE IC-PATH-OWNBR  TO IC-BRNO. 
           MOVE IC-PATH-SQL    TO E-FILE.
           MOVE IC1-KEY        TO E-KEYX.
           INITIALIZE QIC-REC.

           MOVE IC-BRNO   TO QIC-BRNO.
           MOVE IC-CODE   TO QIC-CODE.

           EXEC SQL
            SELECT ICFILE.IC_BRNO,
                   ICFILE.IC_CODE,
                   CAST(ICFILE.IC_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   ICFILE.IC_NAME,
                   ICFILE.IC_ADDR1,
                   ICFILE.IC_ADDR2,
                   ICFILE.IC_CITY,
                   ICFILE.IC_STATE,
                   ICFILE.IC_ZIP,
                   ICFILE.IC_PHONE,
                   ICFILE.IC_COMMRATE_1,
                   ICFILE.IC_COMMRATE_2,
                   ICFILE.IC_COMMRATE_3,
                   ICFILE.IC_COMMRATE_4,
                   ICFILE.IC_COMMRATE_5,
                   ICFILE.IC_COMMRATE_6,
                   ICFILE.IC_COMMRATE_7,
                   ICFILE.IC_COMMRATE_8,
                   ICFILE.IC_COMMRATE_9,
                   ICFILE.IC_COMMRATE_10,
                   ICFILE.IC_COMMRATE_11,
                   ICFILE.IC_COMMRATE_12,
                   ICFILE.IC_COMMRATE_13,
                   ICFILE.IC_COMMRATE_14
            INTO :QIC-REC
            FROM DBO.ICFILE
            WHERE ICFILE.IC_BRNO = :QIC-BRNO
              AND ICFILE.IC_CODE = :QIC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-IC-FIELDS.

      *-----------------------------------------------------------------
       READ-IC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE IC-PATH-OWNBR  TO IC-BRNO.
           MOVE IC-PATH-SQL    TO E-FILE.
           MOVE IC1-KEY        TO E-KEYX.
           INITIALIZE QIC-REC.

           IF ( IC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH IC1_CURSOR INTO :QIC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-IC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-IC1-FILE.

           PERFORM CLOSE-IT.
           IF ( IC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE IC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO IC1-CURSOR-STAT.

