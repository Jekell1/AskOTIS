      * COPYMEMBER: LIBLA/LA01MX
      ******************************************************************
      *
      * IDENTIFICATION: LIBLA/LA01MX
      *
      * DESCRIPTION   : MASTER APPLICATION TRAILER FILE        RSZ=1000
      *
      *
      ******************************************************************
      * REV:
      * KEC 130123 {PR#04196} REGENCY FINANCE <A15> [LORI MCBEE]
      *            ***NEW***
      ******************************************************************
       78  MX-SIZE                                           VALUE 1000.
      ******************************************************************
       01  MX-REC.
           03  MX1-KEY.
               05  MX-APPNO                PIC 9(06).

      ******************************************************************
      *
      *    M A S T E R - A P P L I C A T I O N - S T A T U S
      *
      *=================================================================
      *    STATUS LEVEL 
      *    ------------ 
      *    (1) P - PENDING
      *    (2) C - CONDITIONAL
      *    (3) A - APPROVED
      *    (4) M - MADE LOAN
      *    (5) T - TURNED DOWN
      *    (6) U - TURNED US DOWN
      *
      *=================================================================
      *    STATUS TRACK 
      *    ------------ 
      *    " " = NOT USED
      *    "B" = PREVIOUS STATUS - BACKWARD [ NOT IN USE ]
      *    "C" = CURRENT  STATUS
      *    "F" = PREVIOUS STATUS - FORWARD  [ IN USE ]
      *    "X" = PREVIOUS STATUS - USED     [ PREVIOUS BACKWARD ]
      *    "U" = PREVIOUS STATUS - FORWARD  [ PREVIOUS FORWARD ]
      *
      ******************************************************************
           03  MX-STATUS                   PIC X(01).
               88  MX-STATUS-PENDING           VALUE "P".
               88  MX-STATUS-CONDITIONAL       VALUE "C".
               88  MX-STATUS-APPROVED          VALUE "A".
               88  MX-STATUS-MADE              VALUE "M".
               88  MX-STATUS-TURNDOWN          VALUE "T".
               88  MX-STATUS-TURNUSDOWN        VALUE "U".

      *-----------------------------------------------------------------
           03  MX-STATUS-HISTORY.
               05  MX-STATUS-HISTORYX      OCCURS 10 TIMES.
                   07  MX-STATUS-DATE      PIC 9(08)       COMP-3.
                   07  MX-STATUS-REASON    PIC X(03)       OCCURS 4.
                   07  MX-STATUS-TIME      PIC 9(06)       COMP-3.
                   07  MX-STATUS-TRACK     PIC X(01).
                       88  MX-STATUS-TRACK-CURRENT    VALUE "C".
                       88  MX-STATUS-TRACK-PREVIOUS   VALUE "B" "F".
                       88  MX-STATUS-TRACK-PREV-BACK  VALUE "B".
                       88  MX-STATUS-TRACK-PREV-FWD   VALUE "F".
                       88  MX-STATUS-TRACK-PREV-USED  VALUE "B" "X".
                       88  MX-STATUS-TRACK-UNUSED     VALUE " " "B" "X".
                       88  MX-STATUS-TRACK-USED       VALUE "U" "C" "F".
                   07  MX-STATUS-USERID    PIC X(10).
                   07  FILLER              PIC X(18).

      ******************************************************************
           03  FILLER                      PIC X(493).
