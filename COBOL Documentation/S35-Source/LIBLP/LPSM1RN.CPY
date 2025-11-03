      * COPYMEMBER: LIBLP/LPSM1RN
       LOAD-SM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SM-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SM-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SM-ALL-MACHINE
              MOVE SM-ALL-PATH             TO SM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SM-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SM-ALL-BASE
              MOVE FILPATH-MACHINE         TO SM-ALL-MACHINE
              MOVE SM-ALL-PATH             TO SM-PATH.

      *-----------------------------------------------------------------
       OPEN-SM1-FILE.
           PERFORM LOAD-SM1-FILE.
           PERFORM OPEN-IT.
           MOVE SM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SM1-FILE.
           PERFORM START-IT.
           MOVE SM-PATH-SQL    TO E-FILE.
           MOVE SM1-KEY        TO E-KEYX.

           IF ( SM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SM1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QSM1-WEND-SEQ = ZEROES    ) OR
              ( QSM1-WEND-SEQ NOT NUMERIC )

              MOVE SM-SP-ORGST          TO QSM1-WBEG-ORGST
              MOVE SM-SP-ORGST          TO QSM1-WEND-ORGST
              MOVE SM-SP-SPRCLASS       TO QSM1-WBEG-SPRCLASS
              MOVE SM-SP-SPRCLASS       TO QSM1-WEND-SPRCLASS
              MOVE SM-SP-SUBCLASS       TO QSM1-WBEG-SUBCLASS
              MOVE SM-SP-SUBCLASS       TO QSM1-WEND-SUBCLASS
              MOVE SM-SP-LAWCODE        TO QSM1-WBEG-LAWCODE
              MOVE SM-SP-LAWCODE        TO QSM1-WEND-LAWCODE
              MOVE SM-DATE-REVERSED     TO QSM1-WBEG-DATE-REVERSED
              MOVE ALL "9"              TO QSM1-WEND-DATE-REVERSED
              MOVE SM-SEQ               TO QSM1-WBEG-SEQ
              MOVE ALL "9"              TO QSM1-WEND-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE SM1_CURSOR CURSOR FOR

            SELECT SMFILE.SM_SP_ORGST,
                   SMFILE.SM_SP_SPRCLASS,
                   SMFILE.SM_SP_SUBCLASS,
                   SMFILE.SM_SP_LAWCODE,
                   SMFILE.SM_DATE_REVERSED,
                   SMFILE.SM_SEQ,
                   CAST(SMFILE.SM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   SMFILE.SM_LTOUCH_TIME_HHMM,
                   SMFILE.SM_LTOUCH_USERID,
                   SMFILE.SM_MEMO1,
                   SMFILE.SM_MEMO2,
                   SMFILE.SM_MEMO_TYPE
            FROM DBO.SMFILE
            WHERE ( SMFILE.SM_SP_ORGST      >= :QSM1-WBEG-ORGST
              AND   SMFILE.SM_SP_SPRCLASS   >= :QSM1-WBEG-SPRCLASS
              AND   SMFILE.SM_SP_SUBCLASS   >= :QSM1-WBEG-SUBCLASS
              AND   SMFILE.SM_SP_LAWCODE    >= :QSM1-WBEG-LAWCODE
              AND   SMFILE.SM_DATE_REVERSED >= :QSM1-WBEG-DATE-REVERSED
              AND   SMFILE.SM_SEQ           >= :QSM1-WBEG-SEQ       )

              AND ( SMFILE.SM_SP_ORGST      <= :QSM1-WEND-ORGST
              AND   SMFILE.SM_SP_SPRCLASS   <= :QSM1-WEND-SPRCLASS
              AND   SMFILE.SM_SP_SUBCLASS   <= :QSM1-WEND-SUBCLASS
              AND   SMFILE.SM_SP_LAWCODE    <= :QSM1-WEND-LAWCODE
              AND   SMFILE.SM_DATE_REVERSED <= :QSM1-WEND-DATE-REVERSED
              AND   SMFILE.SM_SEQ           <= :QSM1-WEND-SEQ       )

            ORDER BY SMFILE.SM_SP_ORGST,
                     SMFILE.SM_SP_SPRCLASS,
                     SMFILE.SM_SP_SUBCLASS,
                     SMFILE.SM_SP_LAWCODE,
                     SMFILE.SM_DATE_REVERSED,
                     SMFILE.SM_SEQ
           END-EXEC.

           EXEC SQL
               OPEN SM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSM1-WBEG-ORGST
                          QSM1-WEND-ORGST.

           MOVE ZEROES TO QSM1-WBEG-SPRCLASS
                          QSM1-WEND-SPRCLASS
                          QSM1-WBEG-SUBCLASS
                          QSM1-WEND-SUBCLASS
                          QSM1-WBEG-LAWCODE
                          QSM1-WEND-LAWCODE
                          QSM1-WBEG-DATE-REVERSED
                          QSM1-WEND-DATE-REVERSED
                          QSM1-WBEG-SEQ
                          QSM1-WEND-SEQ.

           MOVE STAT---GOOD TO SM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SM1-FILE.
           PERFORM READ-IT.
           MOVE SM-PATH-SQL    TO E-FILE.
           MOVE SM1-KEY TO E-KEYX.
           INITIALIZE QSM-REC.

           MOVE SM-SP-ORGST            TO QSM-SP-ORGST.  
           MOVE SM-SP-SPRCLASS         TO QSM-SP-SPRCLASS.  
           MOVE SM-SP-SUBCLASS         TO QSM-SP-SUBCLASS.  
           MOVE SM-SP-LAWCODE          TO QSM-SP-LAWCODE.  
           MOVE SM-DATE-REVERSED       TO QSM-DATE-REVERSED.  
           MOVE SM-SEQ                 TO QSM-SEQ.  

           EXEC SQL
            SELECT SMFILE.SM_SP_ORGST,
                   SMFILE.SM_SP_SPRCLASS,
                   SMFILE.SM_SP_SUBCLASS,
                   SMFILE.SM_SP_LAWCODE,
                   SMFILE.SM_DATE_REVERSED,
                   SMFILE.SM_SEQ,
                   CAST(SMFILE.SM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   SMFILE.SM_LTOUCH_TIME_HHMM,
                   SMFILE.SM_LTOUCH_USERID,
                   SMFILE.SM_MEMO1,
                   SMFILE.SM_MEMO2,
                   SMFILE.SM_MEMO_TYPE
            INTO :QSM-REC
            FROM DBO.SMFILE
            WHERE SMFILE.SM_SP_ORGST = :QSM-SP-ORGST
              AND SMFILE.SM_SP_SPRCLASS = :QSM-SP-SPRCLASS
              AND SMFILE.SM_SP_SUBCLASS = :QSM-SP-SUBCLASS
              AND SMFILE.SM_SP_LAWCODE = :QSM-SP-LAWCODE
              AND SMFILE.SM_DATE_REVERSED = :QSM-DATE-REVERSED
              AND SMFILE.SM_SEQ = :QSM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SM-FIELDS.

      *-----------------------------------------------------------------
       READ-SM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SM-PATH-SQL    TO E-FILE.
           MOVE SM1-KEY TO E-KEYX.
           INITIALIZE QSM-REC.

           IF ( SM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SM1_CURSOR INTO :QSM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SM-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SM1-FILE.
           PERFORM CLOSE-IT.
           IF ( SM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SM1-CURSOR-STAT.

