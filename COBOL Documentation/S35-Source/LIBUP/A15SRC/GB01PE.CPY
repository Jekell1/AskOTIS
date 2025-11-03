      * COPYMEMBER: LIBGB/GB01PE
      *****************************************************************
      *     PEFILE - PASSWORD EXCEPTION FILE
      *              USERS WHO WOULD BE REQUIRED TO ENTER A PASSWORD,
      *              BUT ARE EXEMPT BECAUSE THEIR LOGINS ARE IN THE
      *              FILE FOR A CERTAIN OPTION
      * REV: 
      *****************************************************************
       78  PE-SIZE                 VALUE 42.
       01  PE-REC.
           03  PE1-KEY.
               05  PE-PW-KEY.
                   07  PE-DIR      PIC X(2).
                   07  PE-PGM      PIC X(10).
                   07  PE-SEQ      PIC 99.
               05  PE-LOGIN        PIC X(8).
           03  FILLER              PIC X(20).
