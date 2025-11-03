      * COPYMEMBER: LIBLP/LP01IR
      **************************************************************************
      *    INSURANCE RATE TABLE FILE  
      *
      * IR-KEY = PIC 999
      *
      * REV:
      *  JTG 020603 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *             ADDED IR-DESC AND IR-PER-FG
      *  BAH 151006 ADDED IR-KEY, MADE FILE INDEXED, PD0005
      *   CS 190426 REMOVE ENDING FILLER
      *  BAH 20190723 CHANGED IR-KEY FROM 99 TO 999. 71 TO 99 IS RESERVED
      *               FOR SPR INSURANCE RATE TABLES AND WORLD MAY NEED MORE!
      *                                                             PT0147
      * BAH 20230612 ADDED FILLER
      **************************************************************************
       78  IR-SIZE                   VALUE 750.
       01  IR-REC.
           03  IR-KEY                PIC 999.
      $XFD DATE=YYYYMMDD
           03  IR-LTOUCH-DATE        PIC 9(8)    COMP-3.

           03  IR-PER-FG             PIC X.
               88  IR-PER-FG-VALID   VALUE "H" "T".
               88  IR-PER-FG-PER100  VALUE "H".
               88  IR-PER-FG-PER1000 VALUE "H".

           03  IR-INSRATETBL      OCCURS 120 INDEXED BY IRIX.
               05  IR-INSRATE        PIC 99V999  COMP-3.

           03  IR-DESC               PIC X(40).
           03  FILLER                PIC X(341).
