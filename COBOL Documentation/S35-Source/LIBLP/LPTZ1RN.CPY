      * COPYMEMBER: LIBLP/LPTZ1RN
       LOAD-TZ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TZ-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TZ-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO TZ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TZ-ALL-MACHINE
              MOVE TZ-ALL-PATH             TO TZ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TZ-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO TZ-ALL-BASE
              MOVE FILPATH-MACHINE         TO TZ-ALL-MACHINE
              MOVE TZ-ALL-PATH             TO TZ-PATH.

      *-----------------------------------------------------------------
       OPEN-TZ1-FILE.
           PERFORM LOAD-TZ1-FILE.
           PERFORM OPEN-IT.
           MOVE TZ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TZ1-FILE.
           PERFORM START-IT.
           MOVE TZ-PATH-SQL TO E-FILE.
           MOVE TZ1-KEY TO E-KEYX.

           IF ( TZ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TZ1-FILE.

           IF ( QTZ1-WEND-PREFIX = ZEROES    ) OR
              ( QTZ1-WEND-PREFIX NOT NUMERIC )
              MOVE TZ-CODE              TO QTZ1-WBEG-CODE
              MOVE ALL "9"              TO QTZ1-WEND-CODE
              MOVE TZ-PREFIX            TO QTZ1-WBEG-PREFIX
              MOVE ALL "9"              TO QTZ1-WEND-PREFIX
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              IF ( TZ-CODE = 999 )
                 MOVE TZ-CODE           TO QTZ1-WSBEG-CODE
              ELSE
                 COMPUTE QTZ1-WSBEG-CODE = TZ-CODE + 1.


           EXEC SQL
            DECLARE TZ1_CURSOR CURSOR FOR
            SELECT TZFILE.TZ_CODE,
                   TZFILE.TZ_PREFIX,
                   TZFILE.TZ_STATE,
                   TZFILE.TZ_DESC,
                   TZFILE.TZ_ZONE,
                   TZFILE.TZ_FACTOR,
                   TZFILE.TZ_DLS,
                   CAST(TZFILE.TZ_LTOUCH_DATE
                                                  AS VARCHAR(10))
            FROM DBO.TZFILE
            WHERE ( TZFILE.TZ_CODE   >= :QTZ1-WBEG-CODE
              AND   TZFILE.TZ_PREFIX >= :QTZ1-WBEG-PREFIX )
               OR ( TZFILE.TZ_CODE   >= :QTZ1-WSBEG-CODE
              AND   TZFILE.TZ_PREFIX >= 0 )
            ORDER BY TZFILE.TZ_CODE,
                     TZFILE.TZ_PREFIX
           END-EXEC.

           EXEC SQL
               OPEN TZ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTZ1-WBEG-CODE
                          QTZ1-WBEG-PREFIX
                          QTZ1-WEND-CODE
                          QTZ1-WEND-PREFIX
                          QTZ1-WSBEG-CODE.

           MOVE STAT---GOOD TO TZ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TZ1-FILE.
           PERFORM READ-IT.
           MOVE TZ-PATH-SQL TO E-FILE.
           MOVE TZ1-KEY TO E-KEYX.
           INITIALIZE QTZ-REC.

           MOVE TZ-CODE    TO QTZ-CODE.
           MOVE TZ-PREFIX  TO QTZ-PREFIX.

           EXEC SQL
            SELECT TZFILE.TZ_CODE,
                   TZFILE.TZ_PREFIX,
                   TZFILE.TZ_STATE,
                   TZFILE.TZ_DESC,
                   TZFILE.TZ_ZONE,
                   TZFILE.TZ_FACTOR,
                   TZFILE.TZ_DLS,
                   CAST(TZFILE.TZ_LTOUCH_DATE
                                                  AS VARCHAR(10))
            INTO :QTZ-REC
            FROM DBO.TZFILE
            WHERE TZFILE.TZ_CODE = :QTZ-CODE
              AND TZFILE.TZ_PREFIX = :QTZ-PREFIX
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TZ1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TZ-FIELDS.

      *-----------------------------------------------------------------
       READ-TZ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TZ-PATH-SQL TO E-FILE.
           MOVE TZ1-KEY TO E-KEYX.
           INITIALIZE QTZ-REC.

           IF ( TZ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TZ1_CURSOR INTO :QTZ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TZ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TZ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TZ1-FILE.
           PERFORM CLOSE-IT.
           IF ( TZ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TZ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TZ1-CURSOR-STAT.

