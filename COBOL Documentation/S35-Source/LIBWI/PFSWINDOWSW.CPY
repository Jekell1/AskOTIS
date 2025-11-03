      ********************************************************************
      *                     LIBWI/WINDOWSW
      * REV:
      * BLM 120724 CHANGED WINDOWS-BOXED FROM "Y" TO "N" TO MAKE WINDOWS
      *            NOT TRANSPARENT
      *  CS 120726 JUST WANTED TO ELABORATE ON THIS CHANGE, I LOOKED ON ACU
      *            WEBSITE & RAN ACCROSS INFO ON POPUP.
      *            IF POP-UP PHRASE IS SPECIFIED, THE BOX OVERLAYS ANY OTHER
      *            BOXES ON THE SCREEN. IF PHRASE IS NOT SPECIFIED, THE BOX
      *            IS ATTACHED TO ANY OTHER BOXES IT INTERSECTS. I SET THE
      *            WINDOWS-BOXED TO 'N' WHICH DISPLAYS FLOATING GRAPHICAL
      *            WINDOW. THE ORIGINAL SCREEN DID NOT SHOW THRU THE WINDOW.
      *            BARB ALSO ADDED A FLAG TO /USR/A30/SH/XCBLCONFIG. SHE SET
      *            BOXED_FLOATING_WINDOWS 1. OUR SCAN WINDOWS LOOK MUCH BETTER
      *
      *            I ALSO FOUND THAT THE WINDOW-SAVE-AREA PIX X(10) WHICH IS
      *            STMAIN-POP-UP-WINDOW HAS A DIFFERENT VALUE DEPENDING ON
      *            IF THE BOXED FLAG IS 'Y' OR 'N'.
      * BLM 120906 CHANGED SCAN-BUF-SIZE FROM 142 TO 144 TO MATCH SCAN-BUF
      * MJD 130118 ADDED ADDED UNDERLAYED-FORM-PATH2  THIS IS FOR CHANGES
      *            MADE TO CALL-WINDOW-PROGRAM.
      ********************************************************************
       01  NEWWIN-BUF.
           03  WINDOW-LINES           PIC 99    VALUE 10.
           03  FILLER                 PIC X     VALUE " ".
           03  WINDOW-COLUMNS         PIC 99    VALUE 60.
           03  FILLER                 PIC X     VALUE " ".
           03  WINDOW-BEGIN-Y         PIC 99    VALUE 5.
           03  FILLER                 PIC X     VALUE " ".
           03  WINDOW-BEGIN-X         PIC 99    VALUE 10.
           03  FILLER                 PIC X     VALUE " ".

       01  WINDOW-FIT-TO              PIC X(15) VALUE " ".
       01  WINDOW-FORM-NAM            PIC X(9)  VALUE " ".
       78  MAX-WINDOWS                          VALUE 4.
       01  UNDERLAYED-FORM-PATHS-TABLE.
           03  UNDERLAYED-FORM-PATHS  PIC X(22) OCCURS MAX-WINDOWS.
       01  UNDERLAYED-FORM-PATH2     PIC X(22).
       01  WINDOWS-POP-UP-AREA-TABLE.
           03  WINDOWS-POP-UP-AREA    PIC X(10) OCCURS MAX-WINDOWS.
           03  WINDOWS-NEWWIN-BUF     PIC X(12) OCCURS MAX-WINDOWS.
       01  DIM-TITLE-LIST             PIC X(8)  VALUE "TITLE:D.".
       01  NORMAL-TITLE-LIST          PIC X(6)  VALUE "TITLE.".
       01  WINDOW-INTERRUPT-FG        PIC X     VALUE " ".
       01  WINDOW-OPTIONS.
           03  WINDOW-BOXED           PIC X(01) VALUE "N".

       78  SCAN-BUF-SIZE              VALUE 144.
       01  SCAN-BUF.
           03  SCAN-LINES             PIC 99.
           03  SCAN-COLUMNS           PIC 99.
           03  SCAN-BEGIN-Y           PIC 99.
           03  SCAN-BEGIN-X           PIC 99.
           03  SCAN-FORM-NAM          PIC X(9).
           03  SCAN-TITLE             PIC X(25).
           03  SCAN-WINDOW-BUF        PIC X(102).

       01  SCAN-STATUS                PIC XX.

      * ************************************************************
      *
      *   -- DO NOT PUT ANYTHING BEYOND THESE 2 LINES, AS THEY ARE
      *      MEANT TO KEEP THE 03 LEVELS IN ANY ??SCAN-BUF COPY
      *      MODULES HAPPY.
      *
       01  SCAN-DUMMY-01-LEVEL.
           03  FILLER                 PIC X.
