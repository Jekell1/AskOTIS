      ****************************************************************
      * COPY MEMBER LIBGB/DACCESS: CHECK TO SEE IF A DIRECTORY EXISTS
      * BLM 140501 CHANGE C$SYSTEM TO SYSTEM, VERYANT
      * BLM 140519 CHANGE TO NOT CALL SHELL, JUST DO SYSTEM CALL WITH
      *            "LS -LD" & CHECK STATUS.
      * BLM 140522 CHANGE CALL "SYSTEM" TO PERFORM SYSTEM-CALL; NEW
      *             "LS -LD" WAS CAUSING OUTPUT TO SCREEN
      ****************************************************************
       DIR-ACCESS-CALL.
           MOVE DIR-ACCESS-BUF                 TO DIR-ACCESS-SOURCE.
           MOVE SPACES                         TO DIR-ACCESS-TARGET.

           CALL "C$TOLOWER" USING CK-DIR-LS, VALUE 7.
           MOVE SYSTEM-DEVNULL TO CK-DIR-DEVNULL.
           MOVE CK-DIR-EXISTS-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
