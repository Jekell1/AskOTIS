      ************************************************************************
      *                    (UNAME)
      *
      *  1. GETS ENVIRONMENT VARIABLE 'POSTNAME'
      *  2. POSTING NAME APPEARS IN UNAME-BUF
      *     --TAKEN FROM ENVIRONMENT VARIABLE 'POSTNAME'
      *
      *     NOTE:
      *           IF 'POSTNAME' DOSN'T EXIST, THAN
      *              THE USER'S ENVIRONMENT VARIABLE 'LOGNAME' WILL APPEAR
      *           IF NEITHER ARE FOUND STAT = "99"
      * REV:
      *  JTG 101796 CHANGED FROM UNAME
      *  BLM 110804 INITIALIZE STAT TO "00"
      ************************************************************************
       UNAME-CALL.
           MOVE "00" TO STAT.
           ACCEPT UNAME-BUF FROM ENVIRONMENT "POSTNAME".
           IF UNAME-BUF = " "
              ACCEPT UNAME-BUF FROM ENVIRONMENT "LOGNAME"
              IF UNAME-BUF = " "
                 MOVE "99" TO STAT.

      *    MOVE "01" TO STAT.
      *    CALL CMD USING STAT UNAME-BUF.
