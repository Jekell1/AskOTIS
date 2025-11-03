      * COPYMEMBER: LIBGB/GETENV
      *****************************************************
      *         GET THE VALUE OF A SYSTEM VARIABLE
      *
      *   NAME: GETENV
      *   DESC: THIS ROUTINE ALLOWS FOR RETRIEVING ANY SYSTEM
      *         ENVIROMENT VARIABLE.
      *   IN  : PLACE ENVIROMENT VARIABLE NAME INTO
      *         GETENV-BUF
      *   OUT : ..VALUE OF ENVIROMENT VARIABE
      *   COPY: GETENVW
      *   REV :
      *****************************************************
       GETENV-CALL.
      *    MOVE "02" TO STAT.
      *    CALL CMD USING STAT GETENV-BUF.
      *    MOVE GETENV-BUF TO GETENV-NAME.
           ACCEPT GETENV-BUF FROM ENVIRONMENT GETENV-BUF.
           IF GETENV-BUF = " "
              MOVE "XX" TO STAT
           ELSE
              MOVE "00" TO STAT.
