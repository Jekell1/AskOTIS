      * COPYMEMBER: LIBGB/GB01SN
      ******************************************************************
      *
      *       SNFILE - SECURITY PROGRAM NAME FILE      *
      *
      *=================================================================
      * REV:
      * KEC 2015.1111 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *          SN-SIZE = 100;
      *          REMOVED USE OF SN-DIR
      *          ADDED SN-LTOUCH-DATE 
      *
      ******************************************************************
       78  SN-SIZE                     VALUE 100.
       01  SN-REC.
           03  SN2-KEY.
               05  SN-SEQ              PIC 9(8).
               05  SN1-KEY.
      *------->    07  SN-DIR          PIC X(2).  *> REMOVED 2015.1111
                   07  SN-FORM         PIC X(6).
      *>>XDD DATE=YYYYMMDD
           03  SN-LTOUCH-DATE          PIC 9(08)   COMP-3.
           03  SN-DESC                 PIC X(30).
           03  SN-PURCHASABLE          PIC X.
           03  FILLER                  PIC X(50).
