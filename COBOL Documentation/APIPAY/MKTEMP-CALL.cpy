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
       MKTEMP-CALL.
           MOVE "05" TO STAT.
           MOVE FORM-PATH TO MKTEMP-SAVE-FORM-PATH.
           MOVE "GB/MKTEMP" TO FORM-NAM.
           CALL FORM-PROGX USING STAT MKTEMP-BUF.
           CANCEL FORM-PROGX.
           MOVE MKTEMP-SAVE-FORM-PATH TO FORM-PATH.
      *================================================================*
      * END COPYBOOK: LIBGB\MKTEMP.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\SYSTEM.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/SYSTEM
      ******************************************************************
      *         EXECUTE A SYSTEM COMMAND
      *
      *   NAME: SYSTEM
      *   DESC: THIS ROUTINE ALLOWS FOR ANY SYSTEM COMMAND
      *         TO BE EXECUTED.
      *   IN  : PLACE COMMAND INTO SYSTEM-BUF.
      *   OUT : ..COMMAND IS EXECUTED..
      *   COPY: SYSTEMW
      *
      *=================================================================
      ***
      *** LIBGB/CSYSTEM.CPY & LIBGB/CSYSTEMW.CPY WERE COPY MEMBERS THAT
      *** USED C$SYSTEM. WERE CREATED BY BARB & CINDY BACK IN 2013 WHEN
      *** CREATING A30 FROM A15. CHANGES WERE DONE FOR VERYANT BUT WERE
      *** REVERSED LATER.  THERE WERE SOME DISPLAY ISSUES DEPENDING ON
      *** ITS USAGE.
      *** LIBGB/CSYSTEM.CPY & LIBGB/CSYSTEMW.CPY WERE MOVED TO THE
      *** SV DIRECTORY AS A .NO SINCE THEY WERE NOT USED.
      *** THERE ARE HARDCODED C$SYSTEM IN GB/QUEPRU, GB/PREPRU AND
      *** GB/POSPRU DUE TO GARBAGE:
      ***   "THIS MUST STAY C$SYSTEM OR GARBAGE PRINTS, NO IDEA WHY"
      *** FOR MORE INFO, LOOK IN THE SHARED DRIVE FOR BARB'S DOC:
      ***   == PROJECT DOCUMENTS==/2).PROJECT DEVELOPMENT/2).SYSTEM CALL
      ***   SYSTEM CALL.DOCX 
      ***                                        UPDATE BY JAY 2017-0919
      ***
      *=================================================================
      *   REV :
      *   BLM 130325 CHANGED FROM C ROUTINE TO CALL C$SYSTEM
      *   BLM 140430 CHANGED FROM C$SYSTEM TO SYSTEM, ALSO CHANGED
      *              PROPERITES FILE TO HAVE ISCOCOL.SYSTEM.EXEC=C,
      *              THIS WILL ALLOW FUNCTION OF SYSTEM CALLS THAT
      *              USE <, >, |, IN VERYANT
      *   BLM 140503 REMOVE "2" FROM CALL, THIS ARGUMENT WAS FROM
      *              C$SYSTEM
      ******************************************************************
