      * COPYMEMBER: LIBGB/GB01ER

      *****************************************************************
      *          ERROR LOG FILE         
      * REV:
      * BLF 051012 ADDED ERRLOGF-REC, REDEFINITION FOR DETAILED
      *            FIELD-BY-FIELD LOGGING OF MAINTENANCE CHANGES
      *****************************************************************
       01  ERRLOG-REC.
           03  ERRLOG-TIME       PIC X(8).
           03  FILLER            PIC X.
           03  ERRLOG-USERID PIC X(10).
           03  FILLER            PIC X.
           03  ERRLOG-PROG       PIC X(9).
           03  FILLER            PIC X.
           03  ERRLOG-ERR-FIELDS.
               05  ERRLOG-TYPE       PIC X(4).
               05  FILLER            PIC X.
               05  ERRLOG-CODE       PIC XX.
               05  ERRLOG-COMMA      PIC X.
               05  ERRLOG-SUBCODE    PIC XX.
               05  FILLER            PIC X.
               05  ERRLOG-FILE       PIC X(35).
               05  ERRLOG-EOCRON-ID  PIC X(4).
           03  ERRLOG-KEY REDEFINES ERRLOG-ERR-FIELDS PIC X(50).
           03  ERRLOG-EOM-FIELDS REDEFINES ERRLOG-ERR-FIELDS.
               05 ERRLOG-BRNO        PIC 9999.
               05  FILLER            PIC X.
               05 ERRLOG-EOM-MSG     PIC X(45).
       01  ERRLOGF-REC.
           03  ERRLOGF-TIME          PIC X(8).
           03  FILLER                PIC X.
           03  ERRLOGF-USERID        PIC X(8).
           03  FILLER                PIC X.
           03  ERRLOGF-PROG          PIC X(6).
           03  FILLER                PIC X.
           03  ERRLOGF-DETAIL        PIC X(55).
