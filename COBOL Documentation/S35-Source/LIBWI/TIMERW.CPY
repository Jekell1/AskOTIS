      * COPYMEMBER: LIBWI/TIMERW
      ******************************************************************
      *
      * IDENTIFICATION:  LIBGB/TIMERW
      *
      * DESCRIPTION   :  WORKERS FOR TIMER FOR RUNNING CONV/FIXES/REPORT
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 2000.0223 ***NEW***
      * VLM 140107 A30, CHANGED TIMER-CURRENT-TIME TO TIMER-CUR-TIME,
      *            CONFLICTED WITH FIELD IN TIMER.WKS
      ******************************************************************
       01  TIMER-ACCEPT-BUF.
           03  FILLER PIC X(14) VALUE "ACCEPT (Y/N):_".

       01  TIMER-ACCEPT-LIST.
           03  FILLER PIC X(21) VALUE "ACCEPTX TIMER_ACCEPT.".

       01  TIMER-ACCEPT-YN                           PIC X(01).

      *-----------------------------------------------------------------
       01  TIMER-FORMAT-TIME                         PIC X(11).
       01  FILLER                          REDEFINES TIMER-FORMAT-TIME.
           03  TIMER-FORMAT-TIME-HH                  PIC 9(02).
           03  TIMER-FORMAT-TIME-COLUMN              PIC X(01).
           03  TIMER-FORMAT-TIME-MM                  PIC 9(02).
           03  TIMER-FORMAT-TIME-PERIOD              PIC X(01).
           03  TIMER-FORMAT-TIME-SS                  PIC 9(02).
           03  TIMER-FORMAT-TIME-SSX      REDEFINES TIMER-FORMAT-TIME-SS
                                                     PIC X(02).
           03  TIMER-FORMAT-TIME-SPACE               PIC X(01).
           03  TIMER-FORMAT-TIME-XX                  PIC X(02).

      *-----------------------------------------------------------------
       01  TIMER-DISPLAY-TIME-BUF.
           03  TIMER-DISPLAY-CURRENT-TIME            PIC X(11).
           03  TIMER-DISPLAY-START-TIME              PIC X(11).

       01  TIMER-DISPLAY-TIME-LIST.
           03  FILLER PIC X(13) VALUE "CURRENT-TIME ".
           03  FILLER PIC X(09) VALUE "START-HH ".
           03  FILLER PIC X(13) VALUE "START-COLUMN ".
           03  FILLER PIC X(09) VALUE "START-MM ".
           03  FILLER PIC X(13) VALUE "START-PERIOD ".
           03  FILLER PIC X(09) VALUE "START-SS ".
           03  FILLER PIC X(12) VALUE "START-SPACE ".
           03  FILLER PIC X(09) VALUE "START-XX.".

      *-----------------------------------------------------------------
       01  TIMER-CUR-TIME                            PIC X(08).
       01  FILLER                              REDEFINES TIMER-CUR-TIME.
           03  TIMER-CURRENT-TIME-HH                 PIC 9(02).
           03  TIMER-CURRENT-TIME-MM                 PIC 9(02).
           03  TIMER-CURRENT-TIME-SS                 PIC 9(02).
           03  TIMER-CURRENT-TIME-XX                 PIC X(02).

      *-----------------------------------------------------------------
       01  TIMER-START-TIME                          PIC X(08).
       01  FILLER                          REDEFINES TIMER-START-TIME.
           03  TIMER-START-TIME-HH                   PIC 9(02).
           03  TIMER-START-TIME-MM                   PIC 9(02).
           03  TIMER-START-TIME-SS                   PIC 9(02).
           03  TIMER-START-TIME-XX                   PIC X(02).

      *-----------------------------------------------------------------
       01  TIMER-VERIFY-TIME                         PIC X(08).
       01  FILLER                          REDEFINES TIMER-VERIFY-TIME.
           03  TIMER-VERIFY-TIME-HH                  PIC 9(02).
               88  TIMER-VERIFY-TIME-HH-VALID          VALUE 01 THRU 12.
           03  TIMER-VERIFY-TIME-MM                  PIC 9(02).
               88  TIMER-VERIFY-TIME-MM-VALID          VALUE 00 THRU 59.
           03  TIMER-VERIFY-TIME-SS                  PIC 9(02).
               88  TIMER-VERIFY-TIME-SS-VALID          VALUE 00 THRU 59.
           03  TIMER-VERIFY-TIME-XX                  PIC X(02).
               88  TIMER-VERIFY-TIME-XX-VALID          VALUE "AM" "PM".

