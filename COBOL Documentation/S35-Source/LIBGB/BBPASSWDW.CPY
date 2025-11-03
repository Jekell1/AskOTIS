      * COPYMEMBER: LIBGB/BBPASSWDW   
      * USED: WI/BBINQ2.CBL
      ******************************************************************
      * MJD  991209 NEW COPY MEMBER
      * BAH 2024.0321 REMOVED BBPASSWD-VDUSTATUS, HOLD-BB1-KEY
      ******************************************************************
       01  BBPASSWD-STATUS     PIC X(01) VALUE " ".
       01  BBPASSWD-LOGIN-ONLY PIC X(01) VALUE " ".
       01  BBPASSWD-OKAY       PIC X(01) VALUE " ".
       01  BBPASSWD-UNIVERSAL  PIC X(04) VALUE "$$$".
       01  BBPASSWD-LIST       PIC X(09) VALUE "BBPASSWD.".
       01  BBPASSWD-BUF        PIC X(08).
