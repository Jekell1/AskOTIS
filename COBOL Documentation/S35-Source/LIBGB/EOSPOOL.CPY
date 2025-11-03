      * COPYMEMBER: LIBGB/EOSPOOL
      ******************************************************************
      * REV:
      *            CHANGE ACCESS-CALL TO DIR-ACCESS-CALL
      * BLM 140522 CHANGE MKDIR-CMD TO USE C$MAKEDIR
      * BLM 140808 MOVE EXT-EOBUF-PRPATH TO EO-ALL-SPOOL TOO, NEED TO
      *            CORRECT PATH PROBLEM IF LOCAL-SPOOL-FG = Y
      ******************************************************************
       CREATE-SPOOL-DIR SECTION.

           MOVE EXT-EOBUF-PRPATH           TO EO-SPOOL
                                              EO-ALL-SPOOL.
           MOVE BR-MACHINE                 TO EO-ALL-SPOOL-MACHINE.
           MOVE BR-DATA-PATH               TO EO-ALL-SPOOL-DATA.
           MOVE EO-SPOOL-PCODE             TO EO-ALL-SPOOL-PCODE.
           MOVE EO-SPOOL-DATE              TO EO-ALL-SPOOL-DATE.
           MOVE EO-SPOOL-FILE              TO EO-ALL-SPOOL-FILE.
           MOVE EO-ALL-SPOOL-DIR           TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF ( STAT NOT = "00" )
              CALL "C$MAKEDIR" USING EO-ALL-SPOOL-DIR GIVING STAT-99
              IF ( STAT NOT = "00" )
                 MOVE "CANNOT CREATE SPOOL DIRECTORY" TO MESS
                 PERFORM SEND-MESS
                 MOVE EO-ALL-SPOOL-DIR     TO MESS
                 INSPECT MESS REPLACING FIRST "  " BY STAT
                 PERFORM SEND-MESS
                 GO TO EXIT-CREATE-SPOOL.

           MOVE EO-ALL-SPOOL               TO PRU-FILE.
           PERFORM OPEN-PR-FILE.

       EXIT-CREATE-SPOOL.
           EXIT.
