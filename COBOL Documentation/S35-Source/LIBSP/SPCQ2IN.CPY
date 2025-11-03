      * COPYMEMBER: LIBSP/SPCQ2IN
      * 210506 REMOVE CQ-FUTURE-KEY1,FUTURE-KEY2 IN LOAD CQ2-NONCOMP
      *               AND CLEAR-CQ2-KEY, SAVE-CQ2-KEY
      *        REMOVE SPWS-CQ2-FUTURE-KEY1,FUTURE-KEY2 IN LOAD-CQ2
      *        SAVED VERSION SPCQ2IN.CPY.210506
      *-----------------------------------------------------------------
      *    USED FOR CREATING SPWSCQ-CQ2-KEY-NON-COMP GROUP FIELD;
      *    NEEDED FOR POPULATING E-KEYX; CANNOT STRING ALL CQ2-KEY
      *    FIELDS INTO E-KEYX BECAUSE CANNOT STRING CQ-START-TIME
      *    BECAUSE IT CONTAINS A COMP-3 (STRING CANNOT HANDLE COMP).
      *-----------------------------------------------------------------
       LOAD-CQ2-NON-COMP-KEY.
           MOVE CQ-BRNO                 TO SPWSCQ-CQ2-BRNO-NC.
           MOVE CQ-COLLID               TO SPWSCQ-CQ2-COLLID-NC.
           MOVE CQ-REV-BEHAVIOR-RISK TO SPWSCQ-CQ2-REV-BEHAVIOR-RISK-NC.
           MOVE CQ-SEQ-FG               TO SPWSCQ-CQ2-SEQ-FG-NC.
           MOVE CQ-DATE                 TO SPWSCQ-CQ2-DATE-NC.
           MOVE CQ-START-TIME           TO SPWSCQ-CQ2-START-TIME-NC.
           MOVE CQ-REV-DEL              TO SPWSCQ-CQ2-REV-DEL-NC.
           MOVE CQ-MISC-KEY-FLD         TO SPWSCQ-CQ2-MISC-KEY-FLD-NC.
           MOVE CQ-REV-PDUE-DAYS        TO SPWSCQ-CQ2-REV-PDUE-DAYS-NC.
           MOVE CQ-REV-BAL              TO SPWSCQ-CQ2-REV-BAL-NC.
           MOVE CQ-SEQ                  TO SPWSCQ-CQ2-SEQ-NC.

      *-----------------------------------------------------------------
      *    USED TO CLEAR (SPACES/ZEROES) ALL CQ2-KEY FIELDS.
      *-----------------------------------------------------------------
       CLEAR-CQ2-KEY.
           MOVE ZEROES                       TO CQ-BRNO.
           MOVE SPACES                       TO CQ-COLLID.
           MOVE ZEROES                       TO CQ-REV-BEHAVIOR-RISK.
           MOVE SPACES                       TO CQ-SEQ-FG.
           MOVE ZEROES                       TO CQ-DATE.
           MOVE ZEROES                       TO CQ-START-TIME.
           MOVE ZEROES                       TO CQ-REV-DEL.
           MOVE SPACES                       TO CQ-MISC-KEY-FLD.
           MOVE ZEROES                       TO CQ-REV-PDUE-DAYS.
           MOVE ZEROES                       TO CQ-REV-BAL.
           MOVE ZEROES                       TO CQ-SEQ.

      *-----------------------------------------------------------------
      *    USED TO LOAD THE CQ2-KEY FIELDS FROM THE HOLD-CQ2-KEY OR
      *    SAVE-CQ2-KEY FIELDS (SPWSCQ-CQ2-KEY).
      *    SPWSCQ-CQ2-KEY FIELDS ARE USED TO POPULATE CQ2-KEY FIELDS.
      *    PSEUDO CODE:  MOVE SPWSCQ-CQ2-KEY TO CQ2-KEY.
      *-----------------------------------------------------------------
       LOAD-CQ2-KEY.
           MOVE SPWSCQ-CQ2-BRNO              TO CQ-BRNO.
           MOVE SPWSCQ-CQ2-COLLID            TO CQ-COLLID.
           MOVE SPWSCQ-CQ2-REV-BEHAVIOR-RISK TO CQ-REV-BEHAVIOR-RISK.
           MOVE SPWSCQ-CQ2-SEQ-FG            TO CQ-SEQ-FG.
           MOVE SPWSCQ-CQ2-DATE              TO CQ-DATE.
           MOVE SPWSCQ-CQ2-START-TIME        TO CQ-START-TIME.
           MOVE SPWSCQ-CQ2-REV-DEL           TO CQ-REV-DEL.
           MOVE SPWSCQ-CQ2-MISC-KEY-FLD      TO CQ-MISC-KEY-FLD.
           MOVE SPWSCQ-CQ2-REV-PDUE-DAYS     TO CQ-REV-PDUE-DAYS.
           MOVE SPWSCQ-CQ2-REV-BAL           TO CQ-REV-BAL.
           MOVE SPWSCQ-CQ2-SEQ               TO CQ-SEQ.

      *-----------------------------------------------------------------
       LOAD-CQ2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CQ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CQ-ALL-DATA
              MOVE CQ-ALL-PATH             TO CQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO CQ-ALL-MACHINE
              MOVE FILPATH-DATA            TO CQ-ALL-DATA
              MOVE CQ-ALL-PATH             TO CQ-PATH.

      *-----------------------------------------------------------------
       OPEN-CQ2-FILE.
           PERFORM LOAD-CQ2-FILE.
           PERFORM OPEN-IT.
           MOVE CQ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CQ2-FILE.
           PERFORM START-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           MOVE CQ-PATH-SQL    TO E-FILE.
           PERFORM LOAD-CQ2-NON-COMP-KEY.
           MOVE SPWSCQ-CQ2-KEY-NON-COMP TO E-KEYX.

           IF ( CQ2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CQ2-FILE.

           IF ( QCQ2-WEND-SEQ = ZEROES    ) OR
              ( QCQ2-WEND-SEQ NOT NUMERIC )
              MOVE CQ-PATH-OWNBR     TO QCQ2-WBEG-BRNO
              MOVE CQ-PATH-OWNBR     TO QCQ2-WEND-BRNO

              MOVE CQ-COLLID         TO QCQ2-WBEG-COLLID
              MOVE CQ-REV-BEHAVIOR-RISK TO
                                     QCQ2-WBEG-REV-BEHAVIOR-RISK
              MOVE CQ-SEQ-FG           TO QCQ2-WBEG-SEQ-FG
              MOVE CQ-DATE             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ2-WBEG-DATE
              MOVE EXT-JULIAN-END      TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ2-WEND-DATE
              MOVE CQ-START-TIME       TO QCQ2-WBEG-START-TIME
              MOVE CQ-REV-DEL          TO QCQ2-WBEG-REV-DEL
              MOVE CQ-MISC-KEY-FLD     TO QCQ2-WBEG-MISC-KEY-FLD
              MOVE CQ-REV-PDUE-DAYS    TO QCQ2-WBEG-REV-PDUE-DAYS
              MOVE CQ-REV-BAL          TO QCQ2-WBEG-REV-BAL
              MOVE CQ-SEQ              TO QCQ2-WBEG-SEQ

              MOVE ALL "Z"         TO QCQ2-WEND-COLLID
                                      QCQ2-WEND-SEQ-FG
                                      QCQ2-WEND-MISC-KEY-FLD
              MOVE ALL "9"         TO QCQ2-WEND-REV-BEHAVIOR-RISK
                                      QCQ2-WEND-START-TIME
                                      QCQ2-WEND-REV-DEL
                                      QCQ2-WEND-REV-PDUE-DAYS
                                      QCQ2-WEND-REV-BAL
                                      QCQ2-WEND-SEQ
              CALL "C$TOLOWER" USING QCQ2-WEND-COLLID, VALUE 3
              CALL "C$TOLOWER" USING QCQ2-WEND-SEQ-FG, VALUE 1
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              MOVE 03                   TO SQL-TEXT-SIZE
              MOVE CQ-COLLID            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QCQ2-WSBEG-COLLID

              IF ( CQ-REV-BEHAVIOR-RISK = 9 )
                 MOVE CQ-REV-BEHAVIOR-RISK TO
                                     QCQ2-WSBEG-REV-BEHAVIOR-RISK
              ELSE
                 COMPUTE QCQ2-WSBEG-REV-BEHAVIOR-RISK =
                         CQ-REV-BEHAVIOR-RISK + 1
              END-IF

              MOVE 01                   TO SQL-TEXT-SIZE
              MOVE CQ-SEQ-FG            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QCQ2-WSBEG-SEQ-FG

              MOVE CQ-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCQ2-WSBEG-DATE

              COMPUTE QCQ2-WSBEG-START-TIME = CQ-START-TIME + 1

              IF ( CQ-REV-DEL = 99999 )
                 MOVE CQ-REV-DEL        TO QCQ2-WSBEG-REV-DEL
              ELSE
                 COMPUTE QCQ2-WSBEG-REV-DEL = CQ-REV-DEL + 1
              END-IF

              MOVE 03                   TO SQL-TEXT-SIZE
              MOVE CQ-MISC-KEY-FLD      TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QCQ2-WSBEG-MISC-KEY-FLD

              COMPUTE QCQ2-WSBEG-REV-PDUE-DAYS = CQ-REV-PDUE-DAYS + 1
              COMPUTE QCQ2-WSBEG-REV-BAL = CQ-REV-BAL + 1

              IF ( CQ-SEQ  = 9 )
                 MOVE CQ-SEQ            TO QCQ2-WSBEG-SEQ
              ELSE
                 COMPUTE QCQ2-WSBEG-SEQ = CQ-SEQ + 1.


           EXEC SQL
            DECLARE CQ2_CURSOR CURSOR FOR
            SELECT CQFILE.CQ_BRNO,
                   CQFILE.CQ_ACCTNO,
                   CQFILE.CQ_COLLID,
                   CQFILE.CQ_REV_BEHAVIOR_RISK,
                   CQFILE.CQ_SEQ_FG,
                   CAST(CQFILE.CQ_DATE AS VARCHAR(10)),
                   CQFILE.CQ_START_TIME,
                   CQFILE.CQ_REV_DEL,
                   CQFILE.CQ_MISC_KEY_FLD,
                   CQFILE.CQ_REV_PDUE_DAYS,
                   CQFILE.CQ_REV_BAL,
                   CQFILE.CQ_SEQ,
                   CAST(CQFILE.CQ_LTOUCH_DATE AS VARCHAR(10)),
                   CQFILE.CQ_WORK_COLLID,
                   CQFILE.CQ_WORKED_FG,
                   CQFILE.CQ_IN_PROCESS,
                   CAST(CQFILE.CQ_DATE_QUEUED AS VARCHAR(10)),
                   CQFILE.CQ_TIME_ZONE_ERROR
            FROM DBO.CQFILE
            WHERE  ( CQFILE.CQ_BRNO    = :QCQ2-WBEG-BRNO )
              AND (( CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WBEG-DATE 
              AND    CQFILE.CQ_START_TIME >= :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   >= :QCQ2-WBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD >= :QCQ2-WBEG-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS >= :QCQ2-WBEG-REV-PDUE-DAYS
              AND    CQFILE.CQ_REV_BAL   >= :QCQ2-WBEG-REV-BAL
              AND    CQFILE.CQ_SEQ       >= :QCQ2-WBEG-SEQ            )
               OR  ( CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WBEG-DATE
              AND    CQFILE.CQ_START_TIME >= :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   >= :QCQ2-WBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD >= :QCQ2-WBEG-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS >= :QCQ2-WBEG-REV-PDUE-DAYS
              AND    CQFILE.CQ_REV_BAL   >= :QCQ2-WSBEG-REV-BAL
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WBEG-DATE 
              AND    CQFILE.CQ_START_TIME >= :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   >= :QCQ2-WBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD >= :QCQ2-WBEG-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS
                                         >= :QCQ2-WSBEG-REV-PDUE-DAYS
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WBEG-DATE 
              AND    CQFILE.CQ_START_TIME >= :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   >= :QCQ2-WBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD >= :QCQ2-WSBEG-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WBEG-DATE 
              AND    CQFILE.CQ_START_TIME >= :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   >= :QCQ2-WSBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD >= ''
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WBEG-DATE 
              AND    CQFILE.CQ_START_TIME >= :QCQ2-WSBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   >= 0
              AND    CQFILE.CQ_MISC_KEY_FLD >= ''
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WSBEG-DATE 
              AND    CQFILE.CQ_START_TIME >= 0
              AND    CQFILE.CQ_REV_DEL   >= 0
              AND    CQFILE.CQ_MISC_KEY_FLD >= ''
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WSBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= '1900-01-01' 
              AND    CQFILE.CQ_START_TIME >= 0
              AND    CQFILE.CQ_REV_DEL   >= 0
              AND    CQFILE.CQ_MISC_KEY_FLD >= ''
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                        >= :QCQ2-WSBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= ''
              AND    CQFILE.CQ_DATE      >= '1900-01-01' 
              AND    CQFILE.CQ_START_TIME >= 0
              AND    CQFILE.CQ_REV_DEL   >= 0
              AND    CQFILE.CQ_MISC_KEY_FLD >= ''
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_COLLID >= :QCQ2-WSBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= 0
              AND    CQFILE.CQ_SEQ_FG    >= ''
              AND    CQFILE.CQ_DATE      >= '1900-01-01' 
              AND    CQFILE.CQ_START_TIME >= 0
              AND    CQFILE.CQ_REV_DEL   >= 0
              AND    CQFILE.CQ_MISC_KEY_FLD >= ''
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                        ))
              AND (  CQFILE.CQ_COLLID <= :QCQ2-WEND-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         <= :QCQ2-WEND-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    <= :QCQ2-WEND-SEQ-FG
              AND    CQFILE.CQ_DATE      <= :QCQ2-WEND-DATE 
              AND    CQFILE.CQ_START_TIME <= :QCQ2-WEND-START-TIME
              AND    CQFILE.CQ_REV_DEL   <= :QCQ2-WEND-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD <= :QCQ2-WEND-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS <= :QCQ2-WEND-REV-PDUE-DAYS
              AND    CQFILE.CQ_REV_BAL   <= :QCQ2-WEND-REV-BAL
              AND    CQFILE.CQ_SEQ       <= :QCQ2-WEND-SEQ )
            ORDER BY CQFILE.CQ_BRNO,
                     CQFILE.CQ_COLLID,
                     CQFILE.CQ_REV_BEHAVIOR_RISK,
                     CQFILE.CQ_SEQ_FG,
                     CQFILE.CQ_DATE,
                     CQFILE.CQ_START_TIME,
                     CQFILE.CQ_REV_DEL,
                     CQFILE.CQ_MISC_KEY_FLD,
                     CQFILE.CQ_REV_PDUE_DAYS,
                     CQFILE.CQ_REV_BAL,
                     CQFILE.CQ_SEQ,
                     CQFILE.CQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN CQ2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCQ2-WBEG-BRNO 
                          QCQ2-WEND-BRNO
                          QCQ2-WBEG-REV-BEHAVIOR-RISK
                          QCQ2-WEND-REV-BEHAVIOR-RISK
                          QCQ2-WBEG-START-TIME
                          QCQ2-WEND-START-TIME
                          QCQ2-WBEG-REV-DEL
                          QCQ2-WEND-REV-DEL
                          QCQ2-WBEG-REV-PDUE-DAYS
                          QCQ2-WEND-REV-PDUE-DAYS
                          QCQ2-WBEG-REV-BAL
                          QCQ2-WEND-REV-BAL
                          QCQ2-WBEG-SEQ
                          QCQ2-WEND-SEQ
                          QCQ2-WSBEG-REV-BEHAVIOR-RISK
                          QCQ2-WSBEG-START-TIME
                          QCQ2-WSBEG-REV-DEL
                          QCQ2-WSBEG-REV-PDUE-DAYS
                          QCQ2-WSBEG-REV-BAL
                          QCQ2-WSBEG-SEQ.
 
           MOVE SPACES TO QCQ2-WBEG-DATE
                          QCQ2-WEND-DATE
                          QCQ2-WBEG-COLLID
                          QCQ2-WEND-COLLID
                          QCQ2-WBEG-SEQ-FG
                          QCQ2-WEND-SEQ-FG
                          QCQ2-WBEG-MISC-KEY-FLD
                          QCQ2-WEND-MISC-KEY-FLD
                          QCQ2-WSBEG-COLLID
                          QCQ2-WSBEG-SEQ-FG
                          QCQ2-WSBEG-DATE
                          QCQ2-WSBEG-MISC-KEY-FLD.
 
           MOVE STAT---GOOD TO CQ2-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-CQ2-FILE-GREATER.
           PERFORM START-IT.
           MOVE CQ-PATH-OWNBR TO CQ-BRNO.
           MOVE CQ-PATH-SQL   TO E-FILE.
           PERFORM LOAD-CQ2-NON-COMP-KEY.
           MOVE SPWSCQ-CQ2-KEY-NON-COMP TO E-KEYX.

           IF ( CQ2-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-CQ2-FILE.

           IF ( QCQ2-WEND-SEQ = ZEROS     ) OR
              ( QCQ2-WEND-SEQ NOT NUMERIC )
              MOVE CQ-PATH-OWNBR     TO QCQ2-WBEG-BRNO
              MOVE CQ-PATH-OWNBR     TO QCQ2-WEND-BRNO

              MOVE CQ-COLLID         TO QCQ2-WBEG-COLLID
              MOVE CQ-REV-BEHAVIOR-RISK TO
                                     QCQ2-WBEG-REV-BEHAVIOR-RISK
              MOVE CQ-SEQ-FG           TO QCQ2-WBEG-SEQ-FG
              MOVE CQ-DATE             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ2-WBEG-DATE
              MOVE EXT-JULIAN-END      TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ2-WEND-DATE
              MOVE CQ-START-TIME       TO QCQ2-WBEG-START-TIME
              MOVE CQ-REV-DEL          TO QCQ2-WBEG-REV-DEL
              MOVE CQ-MISC-KEY-FLD     TO QCQ2-WBEG-MISC-KEY-FLD
              MOVE CQ-REV-PDUE-DAYS    TO QCQ2-WBEG-REV-PDUE-DAYS
              MOVE CQ-REV-BAL          TO QCQ2-WBEG-REV-BAL
              MOVE CQ-SEQ              TO QCQ2-WBEG-SEQ

              MOVE ALL "Z"         TO QCQ2-WEND-COLLID
                                      QCQ2-WEND-SEQ-FG
                                      QCQ2-WEND-MISC-KEY-FLD
              MOVE ALL "9"         TO QCQ2-WEND-REV-BEHAVIOR-RISK
                                      QCQ2-WEND-START-TIME
                                      QCQ2-WEND-REV-DEL
                                      QCQ2-WEND-REV-PDUE-DAYS
                                      QCQ2-WEND-REV-BAL
                                      QCQ2-WEND-SEQ
              CALL "C$TOLOWER" USING QCQ2-WEND-COLLID, VALUE 3
              CALL "C$TOLOWER" USING QCQ2-WEND-SEQ-FG, VALUE 1
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              MOVE 03                   TO SQL-TEXT-SIZE
              MOVE CQ-COLLID            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QCQ2-WSBEG-COLLID

              IF ( CQ-REV-BEHAVIOR-RISK = 9 )
                 MOVE CQ-REV-BEHAVIOR-RISK TO
                                     QCQ2-WSBEG-REV-BEHAVIOR-RISK
              ELSE
                 COMPUTE QCQ2-WSBEG-REV-BEHAVIOR-RISK =
                         CQ-REV-BEHAVIOR-RISK + 1
              END-IF

              MOVE 01                   TO SQL-TEXT-SIZE
              MOVE CQ-SEQ-FG            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QCQ2-WSBEG-SEQ-FG

              MOVE CQ-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCQ2-WSBEG-DATE

              COMPUTE QCQ2-WSBEG-START-TIME = CQ-START-TIME + 1

              IF ( CQ-REV-DEL = 99999 )
                 MOVE CQ-REV-DEL        TO QCQ2-WSBEG-REV-DEL
              ELSE
                 COMPUTE QCQ2-WSBEG-REV-DEL = CQ-REV-DEL + 1
              END-IF

              MOVE 03                   TO SQL-TEXT-SIZE
              MOVE CQ-MISC-KEY-FLD      TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QCQ2-WSBEG-MISC-KEY-FLD

              COMPUTE QCQ2-WSBEG-REV-PDUE-DAYS = CQ-REV-PDUE-DAYS + 1
              COMPUTE QCQ2-WSBEG-REV-BAL = CQ-REV-BAL + 1

              IF ( CQ-SEQ  = 9 )
                 MOVE CQ-SEQ            TO QCQ2-WSBEG-SEQ
              ELSE
                 COMPUTE QCQ2-WSBEG-SEQ = CQ-SEQ + 1.

           EXEC SQL
            DECLARE CQ2_CURSOR_GR CURSOR FOR
            SELECT CQFILE.CQ_BRNO,
                   CQFILE.CQ_ACCTNO,
                   CQFILE.CQ_COLLID,
                   CQFILE.CQ_REV_BEHAVIOR_RISK,
                   CQFILE.CQ_SEQ_FG,
                   CAST(CQFILE.CQ_DATE AS VARCHAR(10)),
                   CQFILE.CQ_START_TIME,
                   CQFILE.CQ_REV_DEL,
                   CQFILE.CQ_MISC_KEY_FLD,
                   CQFILE.CQ_REV_PDUE_DAYS,
                   CQFILE.CQ_REV_BAL,
                   CQFILE.CQ_SEQ,
                   CAST(CQFILE.CQ_LTOUCH_DATE AS VARCHAR(10)),
                   CQFILE.CQ_WORK_COLLID,
                   CQFILE.CQ_WORKED_FG,
                   CQFILE.CQ_IN_PROCESS,
                   CAST(CQFILE.CQ_DATE_QUEUED AS VARCHAR(10)),
                   CQFILE.CQ_TIME_ZONE_ERROR
            FROM DBO.CQFILE
            WHERE  ( CQFILE.CQ_BRNO   > :QCQ2-WBEG-BRNO )
              AND (( CQFILE.CQ_COLLID > :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         > :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    > :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      > :QCQ2-WBEG-DATE
              AND    CQFILE.CQ_START_TIME > :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   > :QCQ2-WBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD > :QCQ2-WBEG-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS > :QCQ2-WBEG-REV-PDUE-DAYS
              AND    CQFILE.CQ_REV_BAL   > :QCQ2-WBEG-REV-BAL
              AND    CQFILE.CQ_SEQ       > :QCQ2-WBEG-SEQ            )
               OR  ( CQFILE.CQ_COLLID > :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         > :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    > :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      > :QCQ2-WBEG-DATE 
              AND    CQFILE.CQ_START_TIME > :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   > :QCQ2-WBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD > :QCQ2-WBEG-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS > :QCQ2-WBEG-REV-PDUE-DAYS
              AND    CQFILE.CQ_REV_BAL   > :QCQ2-WSBEG-REV-BAL
              AND    CQFILE.CQ_SEQ       > 0                         )
               OR  ( CQFILE.CQ_COLLID > :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         > :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    > :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      > :QCQ2-WBEG-DATE 
              AND    CQFILE.CQ_START_TIME > :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   > :QCQ2-WBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD > :QCQ2-WBEG-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS
                                         > :QCQ2-WSBEG-REV-PDUE-DAYS
              AND    CQFILE.CQ_REV_BAL   > 0
              AND    CQFILE.CQ_SEQ       > 0                         )
               OR  ( CQFILE.CQ_COLLID > :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         > :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    > :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      > :QCQ2-WBEG-DATE 
              AND    CQFILE.CQ_START_TIME > :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   > :QCQ2-WBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD > :QCQ2-WSBEG-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS > 0
              AND    CQFILE.CQ_REV_BAL   > 0
              AND    CQFILE.CQ_SEQ       > 0                         )
               OR  ( CQFILE.CQ_COLLID > :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         > :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    > :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      > :QCQ2-WBEG-DATE 
              AND    CQFILE.CQ_START_TIME > :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   > :QCQ2-WSBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD > ''
              AND    CQFILE.CQ_REV_PDUE_DAYS > 0
              AND    CQFILE.CQ_REV_BAL   > 0
              AND    CQFILE.CQ_SEQ       > 0                         )
               OR  ( CQFILE.CQ_COLLID > :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         > :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    > :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      > :QCQ2-WBEG-DATE 
              AND    CQFILE.CQ_START_TIME > :QCQ2-WSBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   > 0
              AND    CQFILE.CQ_MISC_KEY_FLD > ''
              AND    CQFILE.CQ_REV_PDUE_DAYS > 0
              AND    CQFILE.CQ_REV_BAL   > 0
              AND    CQFILE.CQ_SEQ       > 0                         )
               OR  ( CQFILE.CQ_COLLID > :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         > :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    > :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      > :QCQ2-WSBEG-DATE 
              AND    CQFILE.CQ_START_TIME > 0
              AND    CQFILE.CQ_REV_DEL   > 0
              AND    CQFILE.CQ_MISC_KEY_FLD > ''
              AND    CQFILE.CQ_REV_PDUE_DAYS > 0
              AND    CQFILE.CQ_REV_BAL   > 0
              AND    CQFILE.CQ_SEQ       > 0                         )
               OR  ( CQFILE.CQ_COLLID > :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         > :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    > :QCQ2-WSBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      > '1900-01-01'
              AND    CQFILE.CQ_START_TIME > 0
              AND    CQFILE.CQ_REV_DEL   > 0
              AND    CQFILE.CQ_MISC_KEY_FLD > ''
              AND    CQFILE.CQ_REV_PDUE_DAYS > 0
              AND    CQFILE.CQ_REV_BAL   > 0
              AND    CQFILE.CQ_SEQ       > 0                         )
               OR  ( CQFILE.CQ_COLLID > :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                        > :QCQ2-WSBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    > ''
              AND    CQFILE.CQ_DATE      > '1900-01-01' 
              AND    CQFILE.CQ_START_TIME > 0
              AND    CQFILE.CQ_REV_DEL   > 0
              AND    CQFILE.CQ_MISC_KEY_FLD > ''
              AND    CQFILE.CQ_REV_PDUE_DAYS > 0
              AND    CQFILE.CQ_REV_BAL   > 0
              AND    CQFILE.CQ_SEQ       > 0                         )
               OR  ( CQFILE.CQ_COLLID > :QCQ2-WSBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         > 0
              AND    CQFILE.CQ_SEQ_FG    > ''
              AND    CQFILE.CQ_DATE      > '1900-01-01' 
              AND    CQFILE.CQ_START_TIME > 0
              AND    CQFILE.CQ_REV_DEL   > 0
              AND    CQFILE.CQ_MISC_KEY_FLD > ''
              AND    CQFILE.CQ_REV_PDUE_DAYS > 0
              AND    CQFILE.CQ_REV_BAL   > 0
              AND    CQFILE.CQ_SEQ       > 0                        ))
              AND (  CQFILE.CQ_COLLID <= :QCQ2-WEND-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         <= :QCQ2-WEND-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    <= :QCQ2-WEND-SEQ-FG
              AND    CQFILE.CQ_DATE      <= :QCQ2-WEND-DATE`
              AND    CQFILE.CQ_START_TIME <= :QCQ2-WEND-START-TIME
              AND    CQFILE.CQ_REV_DEL   <= :QCQ2-WEND-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD <= :QCQ2-WEND-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS <= :QCQ2-WEND-REV-PDUE-DAYS
              AND    CQFILE.CQ_REV_BAL   <= :QCQ2-WEND-REV-BAL
              AND    CQFILE.CQ_SEQ       <= :QCQ2-WEND-SEQ )
            ORDER BY CQFILE.CQ_BRNO,
                     CQFILE.CQ_COLLID,
                     CQFILE.CQ_REV_BEHAVIOR_RISK,
                     CQFILE.CQ_SEQ_FG,
                     CQFILE.CQ_DATE,
                     CQFILE.CQ_START_TIME,
                     CQFILE.CQ_REV_DEL,
                     CQFILE.CQ_MISC_KEY_FLD,
                     CQFILE.CQ_REV_PDUE_DAYS,
                     CQFILE.CQ_REV_BAL,
                     CQFILE.CQ_SEQ,
                     CQFILE.CQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN CQ2_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCQ2-WBEG-BRNO 
                          QCQ2-WEND-BRNO
                          QCQ2-WBEG-REV-BEHAVIOR-RISK
                          QCQ2-WEND-REV-BEHAVIOR-RISK
                          QCQ2-WBEG-START-TIME
                          QCQ2-WEND-START-TIME
                          QCQ2-WBEG-REV-DEL
                          QCQ2-WEND-REV-DEL
                          QCQ2-WBEG-REV-PDUE-DAYS
                          QCQ2-WEND-REV-PDUE-DAYS
                          QCQ2-WBEG-REV-BAL
                          QCQ2-WEND-REV-BAL
                          QCQ2-WBEG-SEQ
                          QCQ2-WEND-SEQ
                          QCQ2-WSBEG-REV-BEHAVIOR-RISK
                          QCQ2-WSBEG-START-TIME
                          QCQ2-WSBEG-REV-DEL
                          QCQ2-WSBEG-REV-PDUE-DAYS
                          QCQ2-WSBEG-REV-BAL
                          QCQ2-WSBEG-SEQ.
 
           MOVE SPACES TO QCQ2-WBEG-DATE
                          QCQ2-WEND-DATE
                          QCQ2-WBEG-COLLID
                          QCQ2-WEND-COLLID
                          QCQ2-WBEG-SEQ-FG
                          QCQ2-WEND-SEQ-FG
                          QCQ2-WBEG-MISC-KEY-FLD
                          QCQ2-WEND-MISC-KEY-FLD
                          QCQ2-WSBEG-COLLID
                          QCQ2-WSBEG-SEQ-FG
                          QCQ2-WSBEG-DATE
                          QCQ2-WSBEG-MISC-KEY-FLD.
 
           MOVE STAT---GOOD TO CQ2-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-CQ2-FILE.
           PERFORM READ-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO. 
           MOVE CQ-PATH-SQL    TO E-FILE.
           PERFORM LOAD-CQ2-NON-COMP-KEY.
           MOVE SPWSCQ-CQ2-KEY-NON-COMP TO E-KEYX.
           INITIALIZE QCQ-REC. 

           MOVE CQ-BRNO              TO QCQ-BRNO.
           MOVE CQ-COLLID            TO QCQ-COLLID.
           MOVE CQ-REV-BEHAVIOR-RISK TO QCQ-REV-BEHAVIOR-RISK.
           MOVE CQ-SEQ-FG            TO QCQ-SEQ-FG.
           MOVE CQ-DATE    TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCQ-DATE.
           MOVE CQ-START-TIME        TO QCQ-START-TIME.
           MOVE CQ-REV-DEL           TO QCQ-REV-DEL.
           MOVE CQ-MISC-KEY-FLD      TO QCQ-MISC-KEY-FLD.
           MOVE CQ-REV-PDUE-DAYS     TO QCQ-REV-PDUE-DAYS.
           MOVE CQ-REV-BAL           TO QCQ-REV-BAL.
           MOVE CQ-SEQ               TO QCQ-SEQ.

           EXEC SQL
            SELECT CQFILE.CQ_BRNO,
                   CQFILE.CQ_ACCTNO,
                   CQFILE.CQ_COLLID,
                   CQFILE.CQ_REV_BEHAVIOR_RISK,
                   CQFILE.CQ_SEQ_FG,
                   CAST(CQFILE.CQ_DATE AS VARCHAR(10)),
                   CQFILE.CQ_START_TIME,
                   CQFILE.CQ_REV_DEL,
                   CQFILE.CQ_MISC_KEY_FLD,
                   CQFILE.CQ_REV_PDUE_DAYS,
                   CQFILE.CQ_REV_BAL,
                   CQFILE.CQ_SEQ,
                   CAST(CQFILE.CQ_LTOUCH_DATE AS VARCHAR(10)),
                   CQFILE.CQ_WORK_COLLID,
                   CQFILE.CQ_WORKED_FG,
                   CQFILE.CQ_IN_PROCESS,
                   CAST(CQFILE.CQ_DATE_QUEUED AS VARCHAR(10)),
                   CQFILE.CQ_TIME_ZONE_ERROR
            INTO :QCQ-REC
            FROM DBO.CQFILE
            WHERE CQFILE.CQ_BRNO = :QCQ-BRNO
              AND CQFILE.CQ_COLLID = :QCQ-COLLID
              AND CQFILE.CQ_REV_BEHAVIOR_RISK = :QCQ-REV-BEHAVIOR-RISK
              AND CQFILE.CQ_SEQ_FG = :QCQ-SEQ-FG
              AND CQFILE.CQ_DATE = :QCQ-DATE
              AND CQFILE.CQ_START_TIME = :QCQ-START-TIME
              AND CQFILE.CQ_REV_DEL = :QCQ-REV-DEL
              AND CQFILE.CQ_MISC_KEY_FLD = :QCQ-MISC-KEY-FLD
              AND CQFILE.CQ_REV_PDUE_DAYS = :QCQ-REV-PDUE-DAYS
              AND CQFILE.CQ_REV_BAL = :QCQ-REV-BAL
              AND CQFILE.CQ_SEQ = :QCQ-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQ2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CQ-FIELDS.

      *-----------------------------------------------------------------
       READ-CQ2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           MOVE CQ-PATH-SQL    TO E-FILE.
           PERFORM LOAD-CQ2-NON-COMP-KEY.
           MOVE SPWSCQ-CQ2-KEY-NON-COMP TO E-KEYX.
           INITIALIZE QCQ-REC.

           IF ( CQ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CQ2_CURSOR INTO :QCQ-REC
              END-EXEC
           ELSE
           IF ( CQ2-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH CQ2_CURSOR_GR INTO :QCQ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQ2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CQ-FIELDS.

      *-----------------------------------------------------------------
       REWRITE-CQ2-FILE.

           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           ACCEPT CQ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE CQ-PATH-SQL TO E-FILE.
           PERFORM LOAD-CQ2-NON-COMP-KEY.
           MOVE SPWSCQ-CQ2-KEY-NON-COMP TO E-KEYX.
           PERFORM SET-CQ-FIELDS.

           EXEC SQL
            UPDATE DBO.CQFILE
             SET
              CQFILE.CQ_BRNO = :QCQ-BRNO,
              CQFILE.CQ_ACCTNO = :QCQ-ACCTNO,
              CQFILE.CQ_COLLID = :QCQ-COLLID,
              CQFILE.CQ_REV_BEHAVIOR_RISK =
                   :QCQ-REV-BEHAVIOR-RISK,
              CQFILE.CQ_SEQ_FG = :QCQ-SEQ-FG,
              CQFILE.CQ_DATE = :QCQ-DATE,
              CQFILE.CQ_START_TIME = :QCQ-START-TIME,
              CQFILE.CQ_REV_DEL = :QCQ-REV-DEL,
              CQFILE.CQ_MISC_KEY_FLD = :QCQ-MISC-KEY-FLD,
              CQFILE.CQ_REV_PDUE_DAYS = :QCQ-REV-PDUE-DAYS,
              CQFILE.CQ_REV_BAL = :QCQ-REV-BAL,
              CQFILE.CQ_SEQ = :QCQ-SEQ,
              CQFILE.CQ_LTOUCH_DATE = :QCQ-LTOUCH-DATE,
              CQFILE.CQ_WORK_COLLID = :QCQ-WORK-COLLID,
              CQFILE.CQ_WORKED_FG = :QCQ-WORKED-FG,
              CQFILE.CQ_IN_PROCESS = :QCQ-IN-PROCESS,
              CQFILE.CQ_DATE_QUEUED = :QCQ-DATE-QUEUED,
              CQFILE.CQ_TIME_ZONE_ERROR = :QCQ-TIME-ZONE-ERROR
             WHERE CQFILE.CQ_BRNO = :QCQ-BRNO
               AND CQFILE.CQ_ACCTNO = :QCQ-ACCTNO
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NOTE: THE WHERE CONSTRAINT IN THE ABOVE REWRITE NEEDS TO USE
      *       THE PRIMARY KEY WHERE CONSTRAINT; CANNOT DO A REWRITE ON
      *       THE SECONDARY KEY SINCE THE Q??#-KEY IS CHANGED AND WILL
      *       NOT FIND THE ORIGINAL KEY, AS WELL AS Q??#-KEY COULD
      *       MATCH ANOTHER RECORD/ROW THAT SHOULD NOT CHANGE!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CQ2-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-CQ2-FILE.

           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           PERFORM DELETE-IT.
           MOVE CQ-PATH-SQL TO E-FILE.
           PERFORM LOAD-CQ2-NON-COMP-KEY.
           MOVE SPWSCQ-CQ2-KEY-NON-COMP TO E-KEYX.
           PERFORM SET-CQ-FIELDS.

           EXEC SQL
            DELETE FROM DBO.CQFILE
            WHERE CQFILE.CQ_BRNO = :QCQ-BRNO
              AND CQFILE.CQ_COLLID = :QCQ-COLLID
              AND CQFILE.CQ_REV_BEHAVIOR_RISK = :QCQ-REV-BEHAVIOR-RISK
              AND CQFILE.CQ_SEQ_FG = :QCQ-SEQ-FG
              AND CQFILE.CQ_DATE = :QCQ-DATE
              AND CQFILE.CQ_START_TIME = :QCQ-START-TIME
              AND CQFILE.CQ_REV_DEL = :QCQ-REV-DEL
              AND CQFILE.CQ_MISC_KEY_FLD = :QCQ-MISC-KEY-FLD
              AND CQFILE.CQ_REV_PDUE_DAYS = :QCQ-REV-PDUE-DAYS
              AND CQFILE.CQ_REV_BAL = :QCQ-REV-BAL
              AND CQFILE.CQ_SEQ = :QCQ-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-CQ2-FILE.

      *-----------------------------------------------------------------
       CLOSE-CQ2-FILE.

           PERFORM CLOSE-IT.
           IF ( CQ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CQ2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CQ1-CURSOR-STAT.

           IF ( CQ2-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE CQ2_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO CQ2-CURSOR-GR-STAT.

