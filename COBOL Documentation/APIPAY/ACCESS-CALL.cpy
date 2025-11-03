      ******************************************************************
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\ACCESS.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/ACCESS
      *****************************************************
      *         VERIFY ACCESS TO A FILE
      *
      *   NAME: ACCESS
      *   DESC: THIS ROUTINE WILL TEST FOR THE EXISTENCE
      *         OF SOME FILE.
      *   IN  : PLACE PATH NAME OF FILE INTO ACCESS-BUF.
      *   OUT : STAT = "00" CONFIRMS EXISTENCE
      *   COPY: ACCESSW
      *   REV :
      *  120103 CLS CHANGED TO USE ACU C$FILEINFO
      *  120614 CLS ABOVE CHANGE WAS NOT PUT IN A15, IT IS NOW & ALSO A
      *             LONGER ACCESSW BUF OF 127
      *  120621  CS PUT BACK THE ORIGINAL 'C' CALL. C$FILEINFO DID NOT WORK IF
      *             IF ACCESS-CALL WAS TESTING FOR A DIRECTORY  VERYANT
      *  121017 KEC ADDED CHECK OF ACCESS-SOURCE & ACCESS-TARGET.
      *  130319  CS PUT BACK TO USE ACU CALL C$FILEINFO
      *  121017 KEC FIXED THE USE OF FILPATH-DATA INSTEAD OF
      *             EXT-FILPATH-DATA WHEN SETTING ACCESS-T-L-DATA.
      *****************************************************
       ACCESS-CALL.
           CALL "C$FILEINFO" USING ACCESS-BUF ACCESS-FILE-INFO
                                   GIVING ACCESS-STAT.
           IF ACCESS-STAT = 0
              MOVE "00" TO STAT
           ELSE
              MOVE "01" TO STAT.
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
