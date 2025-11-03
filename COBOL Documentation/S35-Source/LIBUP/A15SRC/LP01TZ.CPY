      * COPYMEMBER: LIBLP/LP01TZ
      **********************************************************
      *         TIME ZONE FILE
      * REV:
      *
      **********************************************************
       78  TZ-SIZE                    VALUE 53.
       01  TZ-REC.
           03  TZ1-KEY.
               05  TZ-CODE            PIC 9(03).
               05  TZ-PREFIX          PIC 9(03).
           03  TZ-STATE               PIC X(02).
           03  TZ-DESC                PIC X(40).
           03  TZ-ZONE                PIC X(01).
GMT -5         88  TZ-IS-EASTERN                  VALUE "E".
GMT -6         88  TZ-IS-CENTRAL                  VALUE "C".
GMT -7         88  TZ-IS-MOUNTAIN                 VALUE "M".
GMT -8         88  TZ-IS-PACIFIC                  VALUE "P".
GMT -9         88  TZ-IS-ALASKA                   VALUE "A".
GMT-10         88  TZ-IS-HAWAII                   VALUE "H".
               88  TZ-IS-VALID                    VALUE "E" "C" "M"
                                                        "P" "A" "H".
           03  TZ-FACTOR              PIC S9(02).
           03  TZ-DLS                 PIC X(01).
           03  FILLER                 PIC X(01).
