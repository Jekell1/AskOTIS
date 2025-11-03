      * COPYMEMBER: LIBLP/LP01IR
      **************************************************************************
      *    INSURANCE RATE TABLE FILE    BYTES = 768
      *           REMEMBER: LP02IR
      * REV:
      *  JTG 020603 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *             ADDED IR-DESC AND IR-PER-FG
      *  BAH 151006 ADDED IR-KEY, MADE FILE INDEXED, PD0005
      **************************************************************************
       78  IR-SIZE                   VALUE 768.
       01  IR-REC.
           03  IR-KEY                PIC 99.
      *>>XDD DATE=YYYYMMDD
           03  IR-LTOUCH-DATE        PIC 9(8)    COMP-3.
           03  IR-PER-FG             PIC X.
               88  IR-PER-FG-VALID   VALUE "H" "T".
               88  IR-PER-FG-PER100  VALUE "H".
               88  IR-PER-FG-PER1000 VALUE "H".
           03  IR-INSRATETBL         OCCURS 120 TIMES
                                     INDEXED BY IRIX.
               05  IR-INSRATE        PIC 99V999  COMP-3.
           03  IR-DESC               PIC X(40).
           03  FILLER                PIC X(360).
