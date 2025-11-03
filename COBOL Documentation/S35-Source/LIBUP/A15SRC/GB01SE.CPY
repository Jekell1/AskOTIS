      * COPYMEMBER: LIBGB/GB01SE
      **************************************************
      *       SEFILE - USER SECURITY FILE              *
      **************************************************
       78  SE-SIZE                     VALUE 68.
       01  SE-REC.
           03  SE1-KEY.
               05  SE-DIR      PIC X(2).
               05  SE-FORM     PIC X(6).
           03  SE-EXP-DATE     PIC 9(6) COMP-6.
           03  SE-PURCHASABLE  PIC X.
           03  SE-PROFILE      PIC X(26).
           03  SE-PROFILEX REDEFINES SE-PROFILE.
               07  SE-PROF     PIC X(1) OCCURS 26 INDEXED BY SESUB.
           03  FILLER          PIC X(30).
