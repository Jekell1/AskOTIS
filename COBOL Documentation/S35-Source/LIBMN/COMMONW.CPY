      * COPYMEMBER: LIBMN/COMMONW
      ******************************************************************
      *
      * IDENTIFICATION:  LIBMN/COMMONW
      *
      * DESCRIPTION   :  COMMON WORKERS FOR MAINTENANCE PROGRAMS
      *
      *=================================================================
      * REV:
      * KEC 2003.0401 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ***NEW***
      *
      ******************************************************************
      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  CURRENT-VDUCOL              PIC 9(01)     VALUE ZEROES.
       01  IO-TYPE                     PIC X(3)      VALUE SPACES.
       01  FIRST-SCAN                  PIC X(01)     VALUE "Y".
       01  FIRST-TIME                  PIC X(01)     VALUE SPACES.
       01  LOCK-KEY                    PIC X(04)     VALUE SPACES.
       01  LOCK-KEY-LIST               PIC X(05)     VALUE "CHAN.".

      *-----------------------------------------------------------------
       78  RECORD-STATUS---EXIST-MODIFY                    VALUE 0.
       78  RECORD-STATUS---NEW-ENTRY                       VALUE 1.
       78  RECORD-STATUS---NEW-MODIFY                      VALUE 2.
       78  SET-RECORD-EXIST-MODIFY                         VALUE 0.
       78  SET-RECORD-NEW-ENTRY                            VALUE 1.
       78  SET-RECORD-NEW-MODIFY                           VALUE 2.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       77  RECORD-STATUS               PIC 9(01)           VALUE ZEROES.
           88  RECORD-EXIST-MODIFY        VALUE SET-RECORD-EXIST-MODIFY.
           88  RECORD-NEW-ENTRY           VALUE SET-RECORD-NEW-ENTRY.
           88  RECORD-NEW-MODIFY          VALUE SET-RECORD-NEW-MODIFY.

      *-----------------------------------------------------------------
       01  SEL-ENTRY                   PIC X(06)     VALUE SPACES.

       01  FILLER                      REDEFINES     SEL-ENTRY.
           03  SEL-LOCK-KEY            PIC X(04).
           03  SEL-ELE                 PIC 9(02).
           03  SEL-ELEX                REDEFINES SEL-ELE
                                       PIC X(02).

       01  FILLER                      REDEFINES     SEL-ENTRY.
           03  SEL-PAGE-KEY            PIC X(03).
           03  SEL-PAGE-P              PIC X(01).
           03  SEL-PAGE-NUMBER         PIC 9(02).

       01  FILLER                      REDEFINES     SEL-ENTRY.
           03  SEL-SINGLE-PAGE-KEY     PIC X(04).
           03  SEL-SINGLE-PAGE-P       PIC X(01).
           03  SEL-SINGLE-PAGE-NUMBER  PIC 9(01).

       01  FILLER                      REDEFINES     SEL-ENTRY.
           03  SEL-DOUBLE-PAGE-KEY     PIC X(03).
           03  SEL-DOUBLE-PAGE-P       PIC X(01).
           03  SEL-DOUBLE-PAGE-NUMBER  PIC 9(02).

      *-----------------------------------------------------------------
       78  SELECT---CONTINUE                         VALUE "   ".
       78  SELECT---CHOOSE                           VALUE "CHO".
       78  SELECT---DELETE                           VALUE "DEL".
       78  SELECT---EDIT                             VALUE "EDT".
       78  SELECT---ENTRY                            VALUE "ENT".
       78  SELECT---EXIT                             VALUE "END".
       78  SELECT---LAUNCH                           VALUE "LAU".
       78  SELECT---LOAD                             VALUE "LOA".
       78  SELECT---RESET                            VALUE "CLR".
       78  SELECT---SAVE                             VALUE "RWR".
       78  SELECT---TOGGLE                           VALUE "TOG".
       78  SET-SELECT-CONTINUE                       VALUE "   ".
       78  SET-SELECT-CHOOSE                         VALUE "CHO".
       78  SET-SELECT-DELETE                         VALUE "DEL".
       78  SET-SELECT-EDIT                           VALUE "EDT".
       78  SET-SELECT-ENTRY                          VALUE "ENT".
       78  SET-SELECT-EXIT                           VALUE "END".
       78  SET-SELECT-LAUNCH                         VALUE "LAU".
       78  SET-SELECT-LOAD                           VALUE "LOA".
       78  SET-SELECT-RESET                          VALUE "CLR".
       78  SET-SELECT-SAVE                           VALUE "RWR".
       78  SET-SELECT-TOGGLE                         VALUE "TOG".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       77  SELECT-OPTION               PIC X(03)     VALUE SPACES.
           88  SELECT-CONTINUE             VALUE SET-SELECT-CONTINUE.
           88  SELECT-CHOOSE               VALUE SET-SELECT-CHOOSE.
           88  SELECT-DELETE               VALUE SET-SELECT-DELETE.
           88  SELECT-EDIT                 VALUE SET-SELECT-EDIT.
           88  SELECT-ENTRY                VALUE SET-SELECT-ENTRY.
           88  SELECT-EXIT                 VALUE SET-SELECT-EXIT.
           88  SELECT-LAUNCH               VALUE SET-SELECT-LAUNCH.
           88  SELECT-LOAD                 VALUE SET-SELECT-LOAD.
           88  SELECT-RESET                VALUE SET-SELECT-RESET.
           88  SELECT-SAVE                 VALUE SET-SELECT-SAVE.
           88  SELECT-TOGGLE               VALUE SET-SELECT-TOGGLE.

      ******************************************************************
      *
      *    T E X T - W O R K E R S
      *
      ******************************************************************
       01  TEXT-ACTIVE-RECORD.
           03  FILLER PIC X(30) VALUE "ACTIVE CUSTOMER CANNOT BE DELE".
           03  FILLER PIC X(30) VALUE "TED                           ".

       01  TEXT-ALREADY-CREATED.
           03  FILLER PIC X(30) VALUE "RECORD HAS BEEN ALREADY CREATE".
           03  FILLER PIC X(30) VALUE "D!                            ".

       01  TEXT-CHANGE-AGAIN.
           03  FILLER PIC X(30) VALUE "PLEASE RE-INPUT YOUR CHANGES! ".
           03  FILLER PIC X(30) VALUE "                              ".

       01  TEXT-CREATE-ONLY-ON-PAGE-01.
           03  FILLER PIC X(30) VALUE "CAN ONLY CREATE NEW RECORD ON ".
           03  FILLER PIC X(30) VALUE "PAGE 1 ONLY!!!                ".

       01  TEXT-RECORD-CHANGED.
           03  FILLER PIC X(30) VALUE "CHANGES WERE MADE TO THIS RECO".
           03  FILLER PIC X(30) VALUE "RD!                           ".

       01  TEXT-RECORD-DELETED.
           03  FILLER PIC X(30) VALUE "RECORD WAS DELETED!           ".
           03  FILLER PIC X(30) VALUE "                              ".

       01  TEXT-RECORD-NOT-FOUND.
           03  FILLER PIC X(30) VALUE "CHANGING PAGE BUT RECORD NOT F".
           03  FILLER PIC X(30) VALUE "OUND!                         ".

       01  TEXT-UNDERGONE-CHANGE.
           03  FILLER PIC X(30) VALUE "THIS RECORD HAS UNDERGONE CHAN".
           03  FILLER PIC X(30) VALUE "GE BY ANOTHER USER,           ".
