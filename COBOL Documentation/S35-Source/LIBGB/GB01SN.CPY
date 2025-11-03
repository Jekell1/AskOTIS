      * COPYMEMBER: LIBGB/GB01SN
      ******************************************************************
      *
      *       SNFILE - SECURITY PROGRAM NAME FILE      *
      *
      *  SN1-KEY = SN-FORM
      *  SN2-KEY = SN-SEQ, SN-FORM
      *
      *
      *=================================================================
      * REV:
      * KEC 2015.1111 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *          SN-SIZE = 100;
      *          REMOVED USE OF SN-DIR
      *          ADDED SN-LTOUCH-DATE 
      *  CS 190411 ADDED FUTURE FIELDS
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      *  20230600 BAH ADDED FILLER
      ******************************************************************
       78  SN-SIZE                     VALUE 100.   
       01  SN-REC.
           03  SN2-KEY.
               05  SN-SEQ              PIC 9(8).
               05  SN1-KEY.
                   07  SN-FORM         PIC X(6).
      $XFD DATE=YYYYMMDD
           03  SN-LTOUCH-DATE          PIC 9(08)   COMP-3.
           03  SN-DESC                 PIC X(30).
           03  SN-PURCHASABLE          PIC X.

           03  FILLER                  PIC X(50).
