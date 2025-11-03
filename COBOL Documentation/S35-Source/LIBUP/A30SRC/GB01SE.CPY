      * COPYMEMBER: LIBGB/GB01SE
      ******************************************************************
      *
      *       SEFILE - USER SECURITY FILE
      *
      *=================================================================
      * REV:
      * KEC 2015.1113 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *          SE-SIZE = 100;
      *          REMOVED USE OF SE-DIR
      *          ADDED SE-LTOUCH-DATE 
      *          CHANGE DATES TO CCYYMMDD
      *
      ******************************************************************
       78  SE-SIZE                         VALUE 100.
       01  SE-REC.
           03  SE1-KEY.
      *--->    05  SE-DIR                  PIC X(02).   *> KEC 2015.1113
               05  SE-FORM                 PIC X(06).
      *>>XDD DATE=YYYYMMDD
           03  SE-LTOUCH-DATE              PIC 9(08)    COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  SE-EXP-DATE                 PIC 9(08)    COMP-3.
           03  SE-PURCHASABLE              PIC X(01).
           03  SE-PROFILE                  PIC X(26).
           03  SE-PROFILEX                 REDEFINES SE-PROFILE.
               07  SE-PROF                 PIC X(01)
                                           OCCURS 26 INDEXED BY SESUB.
           03  FILLER                      PIC X(57).
