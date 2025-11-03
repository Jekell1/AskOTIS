      * COPYMEMBER: LIBGB/GB01PW
      *****************************************************************
      *     PWFILE - USER SECURITY PASSWORD FILE
      *              USED OUTSIDE OF SECURITY MODULE
      * REV: 
      *  JTG 060995 ADDED PW-USER-PROFILE 
      *  BLF 071019 ADDED PW-ASK-FG, PW-EXCEPTION-FG, REGACC PR# 188
      *****************************************************************
       01  PW-REC.
           03  PW1-KEY.
               05  PW-DIR          PIC X(2).
               05  PW-PGM          PIC X(10).
               05  PW-SEQ          PIC 99.
           03  PW-PASSYN           PIC X.
           03  PW-PASSWORD         PIC X(14).
           03  PW-DESC             PIC X(30).
           03  PW-USER-PROFILE     PIC X.
           03  PW-ASK-FG           PIC X.
           03  PW-EXCEPTION-FG     PIC X.
           03  FILLER              PIC X(12).
