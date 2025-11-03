      * COPYMEMBER: LIBLP/LP01TZ
      **********************************************************
      *         TIME ZONE FILE
      *
      *         TZ1-KEY = TZ-CODE, TZ-PREFIX
      *
      * REV:
      *  AMM 151027 PD#00003 INCREASED FILLER FOR A30
      *   CS 190513 REMOVE ENDING FILLER ADD FUTURE FIELDS
      *   CS 190612 SET TZ-SIZE
      *  BAH 20200325 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      *  BAH 20230600 ADDED FILLER
      **********************************************************
       78  TZ-SIZE                    VALUE 200.
       01  TZ-REC.
           03  TZ1-KEY.
               05  TZ-CODE            PIC 9(03).
               05  TZ-PREFIX          PIC 9(03).
           03  TZ-STATE               PIC X(02).
           03  TZ-DESC                PIC X(40).
           03  TZ-ZONE                PIC X(01).
      * GMT -5
               88  TZ-IS-EASTERN                  VALUE "E".
      * GMT -6
               88  TZ-IS-CENTRAL                  VALUE "C".
      * GMT -7
               88  TZ-IS-MOUNTAIN                 VALUE "M".
      * GMT -8
               88  TZ-IS-PACIFIC                  VALUE "P".
      * GMT -9
               88  TZ-IS-ALASKA                   VALUE "A".
      * GMT -10
               88  TZ-IS-HAWAII                   VALUE "H".
               88  TZ-IS-VALID                    VALUE "E" "C" "M"
                                                        "P" "A" "H".
           03  TZ-FACTOR              PIC S9(02).
           03  TZ-DLS                 PIC X(01).
      $XFD DATE=YYYYMMDD
           03  TZ-LTOUCH-DATE         PIC 9(8)  COMP-3.
           03  FILLER                 PIC X(143).
