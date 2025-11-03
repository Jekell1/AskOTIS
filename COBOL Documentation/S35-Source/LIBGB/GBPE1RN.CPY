      * COPYMEMBER: LIBGB/GBPE1RN
       LOAD-PE1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( PE-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( PE-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO PE-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO PE-ALL-MACHINE
              MOVE PE-ALL-PATH             TO PE-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( PE-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO PE-ALL-BASE
              MOVE FILPATH-MACHINE         TO PE-ALL-MACHINE
              MOVE PE-ALL-PATH             TO PE-PATH.

      *-----------------------------------------------------------------
       OPEN-PE1-FILE.
           PERFORM LOAD-PE1-FILE.
           PERFORM OPEN-IT.
           MOVE PE-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-PE1-FILE.
           PERFORM START-IT.
           MOVE PE-PATH-SQL TO E-FILE.
           MOVE PE1-KEY TO E-KEYX.

           IF ( PE1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-PE1-FILE.

           IF ( QPE1-WEND-PGM = SPACES    ) OR
              ( QPE1-WEND-SEQ = 0 )
              MOVE PE-PGM              TO QPE1-WBEG-PGM
              MOVE ALL "Z"             TO QPE1-WEND-PGM
              MOVE PE-LOGIN            TO QPE1-WBEG-LOGIN
              MOVE ALL "Z"             TO QPE1-WEND-LOGIN
              CALL "C$TOLOWER" USING QPE1-WEND-LOGIN, VALUE 8
              MOVE PE-SEQ              TO QPE1-WBEG-SEQ
              MOVE ALL "9"             TO QPE1-WEND-SEQ.

           EXEC SQL
            DECLARE PE1_CURSOR CURSOR FOR
            SELECT PEFILE.PE_PGM,
                   PEFILE.PE_SEQ,
                   PEFILE.PE_LOGIN,
                   CAST(PEFILE.PE_LTOUCH_DATE AS VARCHAR(10))
            FROM DBO.PEFILE
            WHERE PEFILE.PE_PGM >= :QPE1-WBEG-PGM
              AND PEFILE.PE_PGM <= :QPE1-WEND-PGM
              AND PEFILE.PE_SEQ >= :QPE1-WBEG-SEQ
              AND PEFILE.PE_SEQ <= :QPE1-WEND-SEQ
              AND PEFILE.PE_LOGIN >= :QPE1-WBEG-LOGIN
              AND PEFILE.PE_LOGIN <= :QPE1-WEND-LOGIN
            ORDER BY PEFILE.PE_PGM,
                     PEFILE.PE_SEQ,
                     PEFILE.PE_LOGIN
           END-EXEC.

           EXEC SQL
               OPEN PE1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QPE1-WBEG-PGM
                          QPE1-WEND-PGM
                          QPE1-WBEG-LOGIN
                          QPE1-WEND-LOGIN.
           MOVE ZEROES TO QPE1-WBEG-SEQ
                          QPE1-WEND-SEQ.

           MOVE STAT---GOOD TO PE1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-PE1-FILE.
           PERFORM READ-IT.
           MOVE PE-PATH-SQL TO E-FILE.
           MOVE PE1-KEY TO E-KEYX.
           INITIALIZE QPE-REC.

           MOVE PE-PGM    TO QPE-PGM.
           MOVE PE-SEQ    TO QPE-SEQ.
           MOVE PE-LOGIN  TO QPE-LOGIN.

           EXEC SQL
            SELECT PEFILE.PE_PGM,
                   PEFILE.PE_SEQ,
                   PEFILE.PE_LOGIN,
                   CAST(PEFILE.PE_LTOUCH_DATE AS VARCHAR(10))
            INTO :QPE-REC
            FROM DBO.PEFILE
            WHERE PEFILE.PE_PGM = :QPE-PGM
              AND PEFILE.PE_SEQ = :QPE-SEQ
              AND PEFILE.PE_LOGIN = :QPE-LOGIN
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PE1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-PE-FIELDS.

      *-----------------------------------------------------------------
       READ-PE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE PE-PATH-SQL TO E-FILE.
           MOVE PE1-KEY TO E-KEYX.
           INITIALIZE QPE-REC.

           IF ( PE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH PE1_CURSOR INTO :QPE-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PE1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-PE-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-PE1-FILE.
           PERFORM CLOSE-IT.
           IF ( PE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE PE1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO PE1-CURSOR-STAT.
 
 
