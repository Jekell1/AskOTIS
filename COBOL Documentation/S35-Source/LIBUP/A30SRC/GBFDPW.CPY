      * COPYMEMBER: LIBGB/GBFDPW
      **************************************************
      *     PWFILE - USER SECURITY PASSWORD FILE
      *              USED OUTSIDE OF SECURITY MODULE
      * REV:
      *  JTG 060995 ADDED PW-USER-PROFILE
      **************************************************
       FD  OPW-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/GB01PW.CPY" REPLACING LEADING "PW" BY "OPW".
