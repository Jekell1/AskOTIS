      * COPYMEMBER: LIBLP/POSTDATE

      *****************************************************************
      *                      (LIBLP/POSTDATE)
      *          SET POSTDATE ,POSTTIME, AND USERID FOR UPDATING TR
      *
      *   DESC:  MOVES USERID , SYSTEM TIME AND SYSTEM DATE INTO
      *          POST-USERID, POST-TIME, AND POST-DATE. USED
      *          WHEN UPDATEING TRANSACTION FILES (USERID, POSTTIME,
      *          POSTDATE.)
      *   IN  :
      *   OUT :  POST-USERID = USERID.
      *          POST-TIME   = CURREMT SYSTEM TIME.
      *          POST-DATE   = CURRENT SYSTEM DATE.
      *
      * REV:
      *   MJD 032098 CHANGED TO GET USERID FROM LOGNAME. THIS WAY WE WILL
      *              ACCURATE TRACK USERS THAT CAME VIA REMOTE LOGIN.
      *****************************************************************
       SET-POSTDATE SECTION.

           MOVE "LOGNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO POST-USERID.

           ACCEPT POST-TIMEX FROM TIME.
           ACCEPT POST-DATE FROM CENTURY-DATE.

