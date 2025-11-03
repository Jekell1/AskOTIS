      * COPYMEMBER: LIBFP/PST.FORMW
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/PST.FORMW
      *
      * DESCRIPTION   :  LASER (PST) FORM PROGRAM WORKERS
      *
      *=================================================================
      * REV:
      * KEC 2007.1101 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      * KEC 2009.0821 {PR#03423} REGENCY <A15> [LORI MCBEE]
      *               ADDED OPTION TO USE LN OR PD FILES FOR PRINTING.
      *               ADDED 'PST-PRIMARY-FILE'.
      * KEC 2011.0729 ADDED 'SAVE-LM-MEMO-WORKERS' DUE TO AN ISSUE WITH
      *               A FORM FOR MID-ATLANTIC.
      * KEC 2011.0930 ***NEW*** POSTSCRIPT
      * KEC 2013.0815 INCREASED WS-MONTH-TEXT FROM X(10) TO X(12) DUE TO
      *               A STRING DELIMITED BY "  " WHEN THE MONTH IS SEPTEMBER
      * KEC 2016.0122 FIXED FMNO FROM 3 TO 4 BYTES. EDIT-FM-KEY NOW
      *               IN LIBLP/LPWSFM.
      * AMM 2018.0920 PR#01212 ADDED 1STPYDA EXTENSION; ORIGINAL PR# DONE ON
      *               01-20-17 IN A15 BUT LATER ADDED TO A30
      * BAH 181127 GLOBAL LMFILE
      * BAH 190312 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-KEY 
      *
      ******************************************************************
      *
      *    P S T - F O R M - W O R K E R S
      *
      ******************************************************************
       01  CASE-BUF                    PIC X(80)     VALUE SPACES.

       01  CONT-BUF                                  VALUE SPACES.
           03  CONT-PRINT              PIC X(01).
           03  QUEUE-PRINT             PIC X(01).

       01  CONT-LIST.
           03  FILLER PIC X(11) VALUE "CONT QUEUE.".

       78  CROSS-DATA-PATHS---OFF                    VALUE SPACES.
       78  CROSS-DATA-PATHS---ON                     VALUE "Y".
       77  CROSS-DATA-PATHS            PIC X(01)     VALUE SPACES.
           88  CROSS-DATA-PATHS-OFF                  VALUE SPACES.
           88  CROSS-DATA-PATHS-ON                   VALUE "Y".

       01  CROSS-DATA-PATHS-BRNO        PIC 9(04)     VALUE ZEROES.

      *=================================================================
       01  CURRENT-FORM-NAM            PIC X(09)     VALUE SPACES.
       01  FILLER                      REDEFINES CURRENT-FORM-NAM.
           03  CURRENT-FORM-DIR        PIC X(02).
           03  FILLER                  PIC X(01).
           03  CURRENT-FORM-PROGX      PIC X(06).
           03  CURRENT-PROGRAM         REDEFINES CURRENT-FORM-PROGX
                                       PIC X(06).

      *=================================================================
       01  HOLD-FM1-KEY                PIC X(28).
       01  HOLD-FM2-KEY.
           03  HOLD-FM2-BRNO PIC 9(4).
           03  HOLD-FM2-FMNO PIC 9(4).
           03  HOLD-FM2-ZIP  PIC X(15).

       01  MSEL-LIST                   PIC X(05)     VALUE "MSEL.".

       01  PAGE-COPY-MAX               PIC 9(03)     VALUE ZEROES.
       01  PAGE-COPY-ELE               PIC 9(03)     VALUE ZEROES.
       01  COPY-COUNTER                REDEFINES PAGE-COPY-ELE
                                       PIC 9(03).
 
       01  PST-CALLED-FROM             PIC X(6)      VALUE SPACES.

       78  PST-PRIMARY-FILE---LN                     VALUE "LN".
       78  PST-PRIMARY-FILE---PD                     VALUE "PD".
       77  PST-PRIMARY-FILE            PIC X(06)     VALUE "LN".
           88  PST-PRIMARY-FILE-LN                   VALUE "LN".
           88  PST-PRIMARY-FILE-PD                   VALUE "PD".

       01  PST-PHONE-EDIT.
           03  FILLER                  PIC X(01)     VALUE "(".
           03  PST-PHONE-EDIT-AREA     PIC X(03).
           03  FILLER                  PIC X(02)     VALUE ") ".
           03  PST-PHONE-EDIT-PREFIX   PIC X(03).
           03  FILLER                  PIC X(01)     VALUE "-".
           03  PST-PHONE-EDIT-SUFFIX   PIC X(04).

       01  PST-PHONE-UNEDIT            PIC 9(10)     VALUE ZEROES.
       01  FILLER                      REDEFINES PST-PHONE-UNEDIT.
           03  PST-PHONE-UNEDIT-AREA   PIC 9(03).
           03  PST-PHONE-UNEDIT-PREFIX PIC 9(03).
           03  PST-PHONE-UNEDIT-SUFFIX PIC 9(04).

       01  PRU-PATH-LIST               PIC X(06)     VALUE "PPATH.".

       01  PRINT-LINE-WORKERS.
           03  PLW-COUNT               PIC 9(03)     VALUE 99.
           03  PLW-COUNT-ESTIMATE      PIC 9(03)     VALUE ZEROES.
           03  PLW-CRLF                PIC X(80)     VALUE SPACES.

           03  PLW-FEED                PIC 9(03)     VALUE ZEROES.
           03  LN-FEED                 REDEFINES PLW-FEED
                                       PIC 9(03).

           03  PLW-PAGE-NUMBER         PIC 9(04)     VALUE ZEROES.

       78  PST-LM-MEMO-SWITCH---ON                   VALUE "O".
       78  PST-LM-MEMO-SWITCH---OFF                  VALUE "X".
       77  PST-LM-MEMO-SWITCH          PIC X(01)     VALUE "X".
           88  PST-LM-MEMO-SWITCH-ON                 VALUE "O".
           88  PST-LM-MEMO-SWITCH-OFF                VALUE "X".

       01  SAVE-LM-MEMO-WORKERS.
           03  SAVE-LM-MEMO.
               05  FILLER              PIC X(07)     VALUE "FORM # ".
               05  SAVE-LM-MEMO-FMNO   PIC 9(04)     VALUE ZEROES.
               05  FILLER              PIC X(03)     VALUE ",  ".
               05  SAVE-LM-MEMO-DESC   PIC X(25)     VALUE SPACES.
           03  SAVE-LM1-KEY                          VALUE SPACES.
               05  SAVE-LM-BRNO        PIC 9(04).
               05  SAVE-LM-ACCTNO      PIC 9(08).
               05  SAVE-LM-DATE        PIC 9(08).
               05  SAVE-LM-SEQ         PIC 9(03).

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(06)     VALUE ZEROES.
           03  S1                      PIC 9(06)     VALUE ZEROES.
           03  S2                      PIC 9(06)     VALUE ZEROES.
           03  S3                      PIC 9(06)     VALUE ZEROES.

       01  WS-LN-ACCTNO                PIC 9(08)     VALUE ZEROES.
       01  FILLER                      REDEFINES WS-LN-ACCTNO.
           03  WS-LN-NUMBER            PIC 9(08).

       01  WS-MONTH-MM                 PIC 9(02)     VALUE ZEROES.
       01  WS-MONTH-TEXT               PIC X(12)     VALUE SPACES.

       01  WS-1STPY-DD                 PIC 9(02)     VALUE ZEROES.
       01  WS-1STPY-EXT                PIC X(02)     VALUE SPACES.

       01  WS-LN-SSNO                  PIC 9(09)     VALUE ZEROES.
       01  FILLER                      REDEFINES WS-LN-SSNO.
           03  WS-LN-SSNO-3            PIC 9(03).
           03  WS-LN-SSNO-2            PIC 9(02).
           03  WS-LN-SSNO-4            PIC 9(04).
