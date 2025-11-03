      * COPYMEMBER: LIBCL/ERRLOGW ** COPIED INSIDE OF FILEWK.CPY **
      ******************************************************************
      *    ERRLOGW     CREATE LOG RECORD WORKERS
      *
      * COPIED INSIDE FILEWK.CPY
      *
      * ERRLOGW-CODE   'E' - CREATE ERROR LOG ENTRY (ERR)
      *                'C' - CREATE MAINTENANCE CHANGE LOG ENTRY (CHG)
      *                'P' - CREATE EMPLOYEE TIME CARD LOG ENTRY (EMP)
      *
      * REV:
      * BLF 051012 ADDED ERRLOGF-FIELDS TO LOG DETAILED FIELD CHANGES
      *            IN LOAN FILE MAINT, PMT FILE MAINT, ETC.
      ******************************************************************
       01  ERRLOGW-AREA.
           03  ERRLOGW-ACTION     PIC X.
           03  ERRLOGW-PROGX.
               05  FILLER         PIC X(12).
               05  ERRLOGW-PROG.
                   07  FILLER         PIC X(3).
                   07  ERRLOGW-PROGRAM PIC X(6).
           03  ERRLOGW-ERR-FIELDS.
               05  ERRLOGW-TYPE       PIC X(4).
               05  ERRLOGW-FILE       PIC X(35).
               05  ERRLOGW-CODE       PIC XX.
               05  ERRLOGW-SUBCODE    PIC XX.
               05  ERRLOGW-KEY        PIC X(30).
           03  ERRLOGW-EOM-FIELDS REDEFINES ERRLOGW-ERR-FIELDS.
               05  ERRLOGW-BRNO       PIC 9(4).
               05  ERRLOGW-EOM-MSG    PIC X(69).
           03  ERRLOGF-FIELDS REDEFINES ERRLOGW-ERR-FIELDS.
               05  ERRLOGW-DETAIL     PIC X(55).
               05  FILLER             PIC X(18).
      *
      *
