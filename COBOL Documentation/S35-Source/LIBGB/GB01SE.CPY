      * COPYMEMBER: LIBGB/GB01SE
      ******************************************************************
      *
      *       SEFILE - USER SECURITY FILE
      *
      *  SE1-KEY = SE-FORM
      *
      *=================================================================
      * REV:
      * KEC 2015.1113 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *          SE-SIZE = 100;
      *          REMOVED USE OF SE-DIR
      *          ADDED SE-LTOUCH-DATE 
      *          CHANGE DATES TO CCYYMMDD
      * BAH 2019.0218 CHANGED SE-PROFILE TO BE A GROUP LEVEL WITH
      *               SE-PROF BELOW IT, NOT REDEFINED
      *  CS 190409 ADDED FUTURE-FIELDS, REMOVED FILLER
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      *  20230600 BAH ADDED FILLER
      ******************************************************************
       78  SE-SIZE                         VALUE 100.
       01  SE-REC.
           03  SE1-KEY.
               05  SE-FORM                 PIC X(06).
      $XFD DATE=YYYYMMDD
           03  SE-LTOUCH-DATE              PIC 9(08)    COMP-3.
      $XFD DATE=YYYYMMDD
           03  SE-EXP-DATE                 PIC 9(08)    COMP-3.
           03  SE-PURCHASABLE              PIC X(01).
           03  SE-PROFILE.
               05  SE-PROF                 PIC X OCCURS 26
                                                 INDEXED BY SESUB.
           03  FILLER                      PIC X(57).
