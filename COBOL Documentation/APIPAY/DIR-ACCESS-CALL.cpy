      *================================================================*
      * END COPYBOOK: LIBGB\ACCESS.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\DACCESS.CPY                           *
      *================================================================*
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
      *================================================================*
      * END COPYBOOK: LIBGB\DACCESS.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\MKTEMP.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/MKTEMP
      *****************************************
      *  PLACE PATH PREFIX IN MKTEMP-BUF.
      *  UNIQUE PATH APPEARS IN MKTEMP-BUF.
      *  NOTE THAT CALL DOES NOT CREATE FILE
      *  (RETURNS UNIQUE PATH FOR OPEN OUTPUT)
      *
      * REV:
      * MJD 130404 CHANGED TO CALL GB/MKTEMP.C THE VERYANT CONVERTED VERSION
      *            OF PFS C ROUTINE.
      *
      *            ADDED IF STAT = "05" TO BE ABLE TO RUN EITHER IN DEBUG
      *            FOR TESTING.
      * BLM 140417 SAVE & RESTORE FORM-PATH, SO CORRECT PROGRAM WILL
      *            GET CANCELED
      *****************************************
