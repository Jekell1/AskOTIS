      * COPYMEMBER: LIBGB/TIMEWK
      ******************************************************************
      *                (TIMEWK)
      *    TIME DISPLAY WORKERS
      *
      *=================================================================
      * REV:
      * JTG 020594 ADDED TIME-MILITARY-FG
      * BLV 000911 CHANGED TIME-ZONE-FACTOR FROM S9 TO S99
      *
      * JKC 2020-1026 ADDED TIMER_LNK.CPY; USED FOR GB/TIMER.CBL
      *
      * SMR 2024-0401 ADDED TIMER-BEG-DATE TO BE USED FOR CALCULATING
      *          THE ELAPSED TIME IF THE END TIME CROSS OVER TO A NEW
      *          DAY.
      *
      ******************************************************************
       01  TIME-NOW.
           03  CUR-HH          PIC 99.
           03  CUR-MM          PIC 99.
           03  CUR-SS          PIC 99.
           03  CUR-HD          PIC 99.
       01  TIME-NOW2           REDEFINES TIME-NOW.
           03  H-AND-M         PIC 9(4).
           03  FILLER          PIC 9(4).
       01  TIME-NOW3           REDEFINES TIME-NOW.
           03  HH-MM-SS        PIC 9(6).
           03  FILLER          PIC 9(2).
       01  TIME-EDIT.
           03  T-HH            PIC 99.
           03  T-COL           PIC X.
           03  T-MM            PIC 99.
           03  T-SPACE         PIC X.
           03  T-AMPM          PIC XX.
       01  TIME-MILITARY-FG    PIC X    VALUE " ".
       01  SIGNED-HH           PIC S99.
       01  TIME-ZONE-FACTOR    PIC S99.

      * ****NOTE**** ****NOTE**** ****NOTE**** ****NOTE**** ****NOTE****
      * TIMER-LINK-BUF IS AN EXACT COPY IN LIBGB/PRU_LNK.CPY
      * TIMER-LINK-BUF IS AN EXACT COPY IN LIBGB/TIMER_LNK.CPY
      * ANY CHANGE HERE MUST BE MADE IN THE OTHER COPY MEMBERS!!!!!
      * ****NOTE**** ****NOTE**** ****NOTE**** ****NOTE**** ****NOTE****
       01  TIMER-LINK-BUF.
           03  TIMER-STAT                  PIC X(02).
           03  TIMER-BEG-DATE              PIC 9(08).
           03  TIMER-DAYSEC-BEG            PIC 99V9(7).
           03  TIMER-DAYSEC-END            PIC 99V9(7).
           03  TIMER-DAYSEC-ELAPSED        PIC 99V9(7).
           03  TIMER-BEG-EDIT.
               05  TIMER-BEG-HHMMSS        PIC 99/99/99.
               05  FILLER                  PIC X(01).
               05  TIMER-BEG-AMPM          PIC X(02).
           03  TIMER-END-EDIT.
               05  TIMER-END-HHMMSS        PIC 99/99/99.
               05  FILLER                  PIC X(01).
               05  TIMER-END-AMPM          PIC X(02).
           03  TIMER-ELAPSED-EDIT          PIC 99/99/99/99.
           03  TIMER-TYPE                  PIC X(1).
               88  TIMER-TYPE-EXCEL        VALUE "E".
               88  TIMER-TYPE-REPORT       VALUE "R".
               88  TIMER-TYPE-UTILITY      VALUE "U".
               88  TIMER-TYPE-VALID        VALUE "E" "R" "U".

