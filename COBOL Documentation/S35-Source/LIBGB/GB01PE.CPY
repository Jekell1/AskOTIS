      * COPYMEMBER: LIBGB/GB01PE
      ******************************************************************
      *
      *     PEFILE - PASSWORD EXCEPTION FILE
      *              USERS WHO WOULD BE REQUIRED TO ENTER A PASSWORD,
      *              BUT ARE EXEMPT BECAUSE THEIR LOGINS ARE IN THE
      *              FILE FOR A CERTAIN OPTION
      *
      *   PE1-KEY = PE-PGM, PE-SEQ, PE-LOGIN
      *
      *=================================================================
      * REV:
      *
      * KEC 2015.1130 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED TO NOT USE PE-DIR ANYMORE
      *         INCREASED PE-SEQ TO BE 3 BYTES
      *         INCREASE RECORD SIZE; PE-SIZE = 50
      *         ADDED PE-LTOUCH-DATE
      *  CS 190411 ADD FUTURE FIELDS
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  20230600 BAH ADDED FILLER
      ******************************************************************
       78  PE-SIZE                             VALUE 50.
       01  PE-REC.
           03  PE1-KEY.
               05  PE-PW-KEY.
                   07  PE-PGM                  PIC X(10).
                   07  PE-SEQ                  PIC 9(03).
               05  PE-LOGIN                    PIC X(8).
      $XFD DATE=YYYYMMDD
           03  PE-LTOUCH-DATE                  PIC 9(08)   COMP-3.

           03 FILLER                           PIC X(24).
