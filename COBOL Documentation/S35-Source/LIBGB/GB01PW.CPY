      * COPYMEMBER: LIBGB/GB01PW
      ******************************************************************
      *
      *     PWFILE - USER SECURITY PASSWORD FILE
      *              USED OUTSIDE OF SECURITY MODULE
      *
      *   PW1-KEY = PW-PGM, PW-SEQ
      *
      *=================================================================
      * REV:
      * JTG 060995 ADDED PW-USER-PROFILE
      * BLF 071019 ADDED PW-ASK-FG, PW-EXCEPTION-FG, REGACC PR# 188
      *
      * KEC 2015.1130 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         INCREASED PW-REC; ADDED PW-SIZE = 125
      *         INCREASED PW-SEQ TO BE 3 BYTES
      *         FIXED TO NOT USE PW-DIR ANYMORE.
      *         ADDED PW-LTOUCH-DATE
      *  CS 190412 ADD FUTURE FIELDS, REMOVE ENDING FILLER
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      *  20230600 BAH ADDED FILLER
      ******************************************************************
       78  PW-SIZE                             VALUE 120.
       01  PW-REC.
           03  PW1-KEY.
               05  PW-PGM                      PIC X(10).
               05  PW-SEQ                      PIC 9(03).
      $XFD DATE=YYYYMMDD
           03  PW-LTOUCH-DATE                  PIC 9(08)       COMP-3.
           03  PW-PASSYN                       PIC X.
           03  PW-PASSWORD                     PIC X(14).
           03  PW-DESC                         PIC X(30).
           03  PW-USER-PROFILE                 PIC X.
           03  PW-ASK-FG                       PIC X.
           03  PW-EXCEPTION-FG                 PIC X.

           03  FILLER                          PIC X(54).
