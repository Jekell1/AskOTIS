      * COPYMEMBER: LIBGB/GB01SN
      **************************************************
      *       SNFILE - SECURITY PROGRAM NAME FILE      *
      **************************************************
       78  SN-SIZE                     VALUE 77.
       01  SN-REC.
           03  SN2-KEY.
               05  SN-SEQ              PIC 9(8).
               05  SN1-KEY.
                   07  SN-DIR          PIC X(2).
                   07  SN-FORM         PIC X(6).
           03  SN-DESC                 PIC X(30).
           03  SN-PURCHASABLE          PIC X.
           03  FILLER                  PIC X(30).
