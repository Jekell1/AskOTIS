       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOESIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  04/07-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOESIN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : ESFILE 
      * TABLE DESC    : LOAN ESCROW FILE
      * OUTPUT FILE   : XPC/IOESIN.PRE
      * USED FOR      : LIBLP/LPWSES.CPY  --- QES-REC
      *                 LIBLP/LPES1R.CPY  --- READ
      *                 LIBLP/LPES1RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPES2RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0407 ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)IO/IOESIN.CBL S34 05/20/21-09:04:10 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".

      *=================================================================
       COPY "LIBLP/LP01ES.CPY".
       COPY "LIBLP/LPWSES.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QES1-WHERE-BEG-KEY.
           03  QES1-WBEG-BRNO              PIC 9(4).
           03  QES1-WBEG-ACCTNO            PIC 9(8).
 
       01  QES1-WHERE-END-KEY.
           03  QES1-WEND-BRNO              PIC 9(4).
           03  QES1-WEND-ACCTNO            PIC 9(8).
 
       01  QES-REC.
           03  QES-BRNO                      PIC 9(4).
           03  QES-ACCTNO                    PIC 9(8).
           03  QES-LTOUCH-DATE               PIC X(10).
           03  QES-SCHD-EFF-DATE-1           PIC X(10).
           03  QES-SCHD-1STPYDATE-1          PIC X(10).
           03  QES-SCHD-ESCROW-1             PIC S9(7)V99.
           03  QES-SCHD-TERM-1               PIC 9(3).
           03  QES-SCHD-COLLECTED-1          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-2           PIC X(10).
           03  QES-SCHD-1STPYDATE-2          PIC X(10).
           03  QES-SCHD-ESCROW-2             PIC S9(7)V99.
           03  QES-SCHD-TERM-2               PIC 9(3).
           03  QES-SCHD-COLLECTED-2          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-3           PIC X(10).
           03  QES-SCHD-1STPYDATE-3          PIC X(10).
           03  QES-SCHD-ESCROW-3             PIC S9(7)V99.
           03  QES-SCHD-TERM-3               PIC 9(3).
           03  QES-SCHD-COLLECTED-3          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-4           PIC X(10).
           03  QES-SCHD-1STPYDATE-4          PIC X(10).
           03  QES-SCHD-ESCROW-4             PIC S9(7)V99.
           03  QES-SCHD-TERM-4               PIC 9(3).
           03  QES-SCHD-COLLECTED-4          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-5           PIC X(10).
           03  QES-SCHD-1STPYDATE-5          PIC X(10).
           03  QES-SCHD-ESCROW-5             PIC S9(7)V99.
           03  QES-SCHD-TERM-5               PIC 9(3).
           03  QES-SCHD-COLLECTED-5          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-6           PIC X(10).
           03  QES-SCHD-1STPYDATE-6          PIC X(10).
           03  QES-SCHD-ESCROW-6             PIC S9(7)V99.
           03  QES-SCHD-TERM-6               PIC 9(3).
           03  QES-SCHD-COLLECTED-6          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-7           PIC X(10).
           03  QES-SCHD-1STPYDATE-7          PIC X(10).
           03  QES-SCHD-ESCROW-7             PIC S9(7)V99.
           03  QES-SCHD-TERM-7               PIC 9(3).
           03  QES-SCHD-COLLECTED-7          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-8           PIC X(10).
           03  QES-SCHD-1STPYDATE-8          PIC X(10).
           03  QES-SCHD-ESCROW-8             PIC S9(7)V99.
           03  QES-SCHD-TERM-8               PIC 9(3).
           03  QES-SCHD-COLLECTED-8          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-9           PIC X(10).
           03  QES-SCHD-1STPYDATE-9          PIC X(10).
           03  QES-SCHD-ESCROW-9             PIC S9(7)V99.
           03  QES-SCHD-TERM-9               PIC 9(3).
           03  QES-SCHD-COLLECTED-9          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-10          PIC X(10).
           03  QES-SCHD-1STPYDATE-10         PIC X(10).
           03  QES-SCHD-ESCROW-10            PIC S9(7)V99.
           03  QES-SCHD-TERM-10              PIC 9(3).
           03  QES-SCHD-COLLECTED-10         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-11          PIC X(10).
           03  QES-SCHD-1STPYDATE-11         PIC X(10).
           03  QES-SCHD-ESCROW-11            PIC S9(7)V99.
           03  QES-SCHD-TERM-11              PIC 9(3).
           03  QES-SCHD-COLLECTED-11         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-12          PIC X(10).
           03  QES-SCHD-1STPYDATE-12         PIC X(10).
           03  QES-SCHD-ESCROW-12            PIC S9(7)V99.
           03  QES-SCHD-TERM-12              PIC 9(3).
           03  QES-SCHD-COLLECTED-12         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-13          PIC X(10).
           03  QES-SCHD-1STPYDATE-13         PIC X(10).
           03  QES-SCHD-ESCROW-13            PIC S9(7)V99.
           03  QES-SCHD-TERM-13              PIC 9(3).
           03  QES-SCHD-COLLECTED-13         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-14          PIC X(10).
           03  QES-SCHD-1STPYDATE-14         PIC X(10).
           03  QES-SCHD-ESCROW-14            PIC S9(7)V99.
           03  QES-SCHD-TERM-14              PIC 9(3).
           03  QES-SCHD-COLLECTED-14         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-15          PIC X(10).
           03  QES-SCHD-1STPYDATE-15         PIC X(10).
           03  QES-SCHD-ESCROW-15            PIC S9(7)V99.
           03  QES-SCHD-TERM-15              PIC 9(3).
           03  QES-SCHD-COLLECTED-15         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-16          PIC X(10).
           03  QES-SCHD-1STPYDATE-16         PIC X(10).
           03  QES-SCHD-ESCROW-16            PIC S9(7)V99.
           03  QES-SCHD-TERM-16              PIC 9(3).
           03  QES-SCHD-COLLECTED-16         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-17          PIC X(10).
           03  QES-SCHD-1STPYDATE-17         PIC X(10).
           03  QES-SCHD-ESCROW-17            PIC S9(7)V99.
           03  QES-SCHD-TERM-17              PIC 9(3).
           03  QES-SCHD-COLLECTED-17         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-18          PIC X(10).
           03  QES-SCHD-1STPYDATE-18         PIC X(10).
           03  QES-SCHD-ESCROW-18            PIC S9(7)V99.
           03  QES-SCHD-TERM-18              PIC 9(3).
           03  QES-SCHD-COLLECTED-18         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-19          PIC X(10).
           03  QES-SCHD-1STPYDATE-19         PIC X(10).
           03  QES-SCHD-ESCROW-19            PIC S9(7)V99.
           03  QES-SCHD-TERM-19              PIC 9(3).
           03  QES-SCHD-COLLECTED-19         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-20          PIC X(10).
           03  QES-SCHD-1STPYDATE-20         PIC X(10).
           03  QES-SCHD-ESCROW-20            PIC S9(7)V99.
           03  QES-SCHD-TERM-20              PIC 9(3).
           03  QES-SCHD-COLLECTED-20         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-21          PIC X(10).
           03  QES-SCHD-1STPYDATE-21         PIC X(10).
           03  QES-SCHD-ESCROW-21            PIC S9(7)V99.
           03  QES-SCHD-TERM-21              PIC 9(3).
           03  QES-SCHD-COLLECTED-21         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-22          PIC X(10).
           03  QES-SCHD-1STPYDATE-22         PIC X(10).
           03  QES-SCHD-ESCROW-22            PIC S9(7)V99.
           03  QES-SCHD-TERM-22              PIC 9(3).
           03  QES-SCHD-COLLECTED-22         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-23          PIC X(10).
           03  QES-SCHD-1STPYDATE-23         PIC X(10).
           03  QES-SCHD-ESCROW-23            PIC S9(7)V99.
           03  QES-SCHD-TERM-23              PIC 9(3).
           03  QES-SCHD-COLLECTED-23         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-24          PIC X(10).
           03  QES-SCHD-1STPYDATE-24         PIC X(10).
           03  QES-SCHD-ESCROW-24            PIC S9(7)V99.
           03  QES-SCHD-TERM-24              PIC 9(3).
           03  QES-SCHD-COLLECTED-24         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-25          PIC X(10).
           03  QES-SCHD-1STPYDATE-25         PIC X(10).
           03  QES-SCHD-ESCROW-25            PIC S9(7)V99.
           03  QES-SCHD-TERM-25              PIC 9(3).
           03  QES-SCHD-COLLECTED-25         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-26          PIC X(10).
           03  QES-SCHD-1STPYDATE-26         PIC X(10).
           03  QES-SCHD-ESCROW-26            PIC S9(7)V99.
           03  QES-SCHD-TERM-26              PIC 9(3).
           03  QES-SCHD-COLLECTED-26         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-27          PIC X(10).
           03  QES-SCHD-1STPYDATE-27         PIC X(10).
           03  QES-SCHD-ESCROW-27            PIC S9(7)V99.
           03  QES-SCHD-TERM-27              PIC 9(3).
           03  QES-SCHD-COLLECTED-27         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-28          PIC X(10).
           03  QES-SCHD-1STPYDATE-28         PIC X(10).
           03  QES-SCHD-ESCROW-28            PIC S9(7)V99.
           03  QES-SCHD-TERM-28              PIC 9(3).
           03  QES-SCHD-COLLECTED-28         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-29          PIC X(10).
           03  QES-SCHD-1STPYDATE-29         PIC X(10).
           03  QES-SCHD-ESCROW-29            PIC S9(7)V99.
           03  QES-SCHD-TERM-29              PIC 9(3).
           03  QES-SCHD-COLLECTED-29         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-30          PIC X(10).
           03  QES-SCHD-1STPYDATE-30         PIC X(10).
           03  QES-SCHD-ESCROW-30            PIC S9(7)V99.
           03  QES-SCHD-TERM-30              PIC 9(3).
           03  QES-SCHD-COLLECTED-30         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-31          PIC X(10).
           03  QES-SCHD-1STPYDATE-31         PIC X(10).
           03  QES-SCHD-ESCROW-31            PIC S9(7)V99.
           03  QES-SCHD-TERM-31              PIC 9(3).
           03  QES-SCHD-COLLECTED-31         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-32          PIC X(10).
           03  QES-SCHD-1STPYDATE-32         PIC X(10).
           03  QES-SCHD-ESCROW-32            PIC S9(7)V99.
           03  QES-SCHD-TERM-32              PIC 9(3).
           03  QES-SCHD-COLLECTED-32         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-33          PIC X(10).
           03  QES-SCHD-1STPYDATE-33         PIC X(10).
           03  QES-SCHD-ESCROW-33            PIC S9(7)V99.
           03  QES-SCHD-TERM-33              PIC 9(3).
           03  QES-SCHD-COLLECTED-33         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-34          PIC X(10).
           03  QES-SCHD-1STPYDATE-34         PIC X(10).
           03  QES-SCHD-ESCROW-34            PIC S9(7)V99.
           03  QES-SCHD-TERM-34              PIC 9(3).
           03  QES-SCHD-COLLECTED-34         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-35          PIC X(10).
           03  QES-SCHD-1STPYDATE-35         PIC X(10).
           03  QES-SCHD-ESCROW-35            PIC S9(7)V99.
           03  QES-SCHD-TERM-35              PIC 9(3).
           03  QES-SCHD-COLLECTED-35         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-36          PIC X(10).
           03  QES-SCHD-1STPYDATE-36         PIC X(10).
           03  QES-SCHD-ESCROW-36            PIC S9(7)V99.
           03  QES-SCHD-TERM-36              PIC 9(3).
           03  QES-SCHD-COLLECTED-36         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-37          PIC X(10).
           03  QES-SCHD-1STPYDATE-37         PIC X(10).
           03  QES-SCHD-ESCROW-37            PIC S9(7)V99.
           03  QES-SCHD-TERM-37              PIC 9(3).
           03  QES-SCHD-COLLECTED-37         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-38          PIC X(10).
           03  QES-SCHD-1STPYDATE-38         PIC X(10).
           03  QES-SCHD-ESCROW-38            PIC S9(7)V99.
           03  QES-SCHD-TERM-38              PIC 9(3).
           03  QES-SCHD-COLLECTED-38         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-39          PIC X(10).
           03  QES-SCHD-1STPYDATE-39         PIC X(10).
           03  QES-SCHD-ESCROW-39            PIC S9(7)V99.
           03  QES-SCHD-TERM-39              PIC 9(3).
           03  QES-SCHD-COLLECTED-39         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-40          PIC X(10).
           03  QES-SCHD-1STPYDATE-40         PIC X(10).
           03  QES-SCHD-ESCROW-40            PIC S9(7)V99.
           03  QES-SCHD-TERM-40              PIC 9(3).
           03  QES-SCHD-COLLECTED-40         PIC S9(7)V99.
           03  QES-BALANCE                   PIC S9(7)V99.
           03  QES-PREPAID-1                 PIC S9(7)V99.
           03  QES-COLLECTED-1               PIC S9(7)V99.
           03  QES-DISBURSED-TAXES-1         PIC S9(7)V99.
           03  QES-DISBURSED-INS-1           PIC S9(7)V99.
           03  QES-DISBURSED-PMI-1           PIC S9(7)V99.
           03  QES-DISBURSED-OTHER-1         PIC S9(7)V99.
           03  QES-DISBURSED-REF-1           PIC S9(7)V99.
           03  QES-DISBURSED-CNT-TAXES-1     PIC S9(7)V99.
           03  QES-DISBURSED-MISC1-1         PIC S9(7)V99.
           03  QES-DISBURSED-MISC2-1         PIC S9(7)V99.
           03  QES-PREPAID-2                 PIC S9(7)V99.
           03  QES-COLLECTED-2               PIC S9(7)V99.
           03  QES-DISBURSED-TAXES-2         PIC S9(7)V99.
           03  QES-DISBURSED-INS-2           PIC S9(7)V99.
           03  QES-DISBURSED-PMI-2           PIC S9(7)V99.
           03  QES-DISBURSED-OTHER-2         PIC S9(7)V99.
           03  QES-DISBURSED-REF-2           PIC S9(7)V99.
           03  QES-DISBURSED-CNT-TAXES-2     PIC S9(7)V99.
           03  QES-DISBURSED-MISC1-2         PIC S9(7)V99.
           03  QES-DISBURSED-MISC2-2         PIC S9(7)V99.
           03  QES-PREPAID-3                 PIC S9(7)V99.
           03  QES-COLLECTED-3               PIC S9(7)V99.
           03  QES-DISBURSED-TAXES-3         PIC S9(7)V99.
           03  QES-DISBURSED-INS-3           PIC S9(7)V99.
           03  QES-DISBURSED-PMI-3           PIC S9(7)V99.
           03  QES-DISBURSED-OTHER-3         PIC S9(7)V99.
           03  QES-DISBURSED-REF-3           PIC S9(7)V99.
           03  QES-DISBURSED-CNT-TAXES-3     PIC S9(7)V99.
           03  QES-DISBURSED-MISC1-3         PIC S9(7)V99.
           03  QES-DISBURSED-MISC2-3         PIC S9(7)V99.
           03  QES-MISC-AMT-1                PIC S9(7)V99.
           03  QES-EXP-DATE-1                PIC X(10).
           03  QES-MISC-AMT-2                PIC S9(7)V99.
           03  QES-EXP-DATE-2                PIC X(10).
           03  QES-MISC-AMT-3                PIC S9(7)V99.
           03  QES-EXP-DATE-3                PIC X(10).
           03  QES-MISC-AMT-4                PIC S9(7)V99.
           03  QES-EXP-DATE-4                PIC X(10).
           03  QES-MISC-AMT-5                PIC S9(7)V99.
           03  QES-EXP-DATE-5                PIC X(10).
           03  QES-MISC-AMT-6                PIC S9(7)V99.
           03  QES-EXP-DATE-6                PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  RECORDS-READ                PIC 9(11)     VALUE ZEROES.

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(09)     VALUE ZEROES.
           03  S1                      PIC 9(09)     VALUE ZEROES.
           03  S2                      PIC 9(09)     VALUE ZEROES.
           03  S3                      PIC 9(09)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-ARROW, (".
           03  FILLER PIC X(30) VALUE "Y)ES OR (N)O TO ACCEPT        ".
           03  FILLER PIC X(17) VALUE "   ACCEPT (Y/N): ".

       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER PRINTER OPTIONS".

       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH NO. (F3-ALL) ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRANCH NO. ".

       01  LEGEND-BEG-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING ACCTNO     (F3-ALL) ".

       01  LEGEND-END-ACCTNO.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING ACCTNO     ".

       01  LEGEND-PROCESS-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER NU".
           03  FILLER PIC X(30) VALUE "MBER OF FILE KEY TO PROCESS   ".

       01  LEGEND-BEG-END-RANGE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER RA".
           03  FILLER PIC X(30) VALUE "ANGE VALUE                    ".

      ******************************************************************
      *
      *    P A C E - W O R K E R S
      *
      ******************************************************************
       78  PACE-COUNTER-MAX                          VALUE 1000.
       77  PACE-COUNTER                PIC 9(6)      VALUE ZEROES.
       77  PACE-DISPLAY-MAX            PIC ZZZ,ZZ9   VALUE ZEROES.
       78  PACE-MAX                                  VALUE 15.
       77  PACE-LEV                    PIC 9(2)      VALUE ZEROES.

       01  PACE-HEAD-LINE.
           03  FILLER PIC X(28) VALUE "TIME        KEY             ".
           03  PACE-HEAD-MAX    PIC ZZZ,ZZ9 VALUE ZEROES.
           03  FILLER PIC X(05) VALUE " RECS".

       01  PACE-TIME.
           03  PACE-TIME-HH            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-MM            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-SS            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ".".
           03  PACE-TIME-HD            PIC 99.

       01  PACE-DETAIL.
           03  PACE-DETAIL-TIME        PIC 99/99/99/99.
           03  FILLER                  PIC X(1).
           03  PACE-DETAIL-KEY         PIC X(28).

       01  PACE-BUF.
           03  PACE-ELE                PIC X(40)     OCCURS PACE-MAX.

       01  PACE-LIST.
           03  FILLER PIC X(24) VALUE "PACE-01 PACE-02 PACE-03 ".
           03  FILLER PIC X(24) VALUE "PACE-04 PACE-05 PACE-06 ".
           03  FILLER PIC X(24) VALUE "PACE-07 PACE-08 PACE-09 ".
           03  FILLER PIC X(24) VALUE "PACE-10 PACE-11 PACE-12 ".
           03  FILLER PIC X(24) VALUE "PACE-13 PACE-14 PACE-15.".

      ******************************************************************
      *
      *    P A R A M E T E R - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

           03  PROCESS-KEY             PIC 9(01).
           03  BEG-ACCTNO              PIC 9(08).
           03  END-ACCTNO              PIC 9(08).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.

      *=================================================================
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(37) VALUE SPACES.
           03  FILLER             PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE           PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  H02-TITLE          PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER PIC X(30) VALUE "BRNO   ACCTNO                 ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(06) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-ES-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-ES-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-ES-CLASS                   PIC 9(02).
           03  FILLER                         PIC X(01).

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(100).
           03  R01-DATE                       REDEFINES R01-FIELD
                                              PIC 99/99/99.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(100).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBIO/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOESIN_DEF.CPY".
       COPY "LIBIO/IOESIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOESIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.

       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *=================================================================
       MAIN-PROGRAM-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO MAIN-PROGRAM-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MAIN-PROGRAM-PROCESS-BRNO.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.

           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-PROCESS-BRNO.

           PERFORM PROCESS-BRANCH-MODULE.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM GET-TIME.
           MOVE SPACES TO SENDMESS-HEAD.

           STRING "PROGRAM COMPLETED AT ", TIME-EDIT
                  DELIMITED BY SIZE INTO SENDMESS-HEAD.

           MOVE SPACES TO SENDMESS-MSG1.
           MOVE SPACES TO SENDMESS-MSG2.
           STRING "RECORDS READ: ", RECORDS-READ
                  DELIMITED BY SIZE INTO SENDMESS-MSG2.
           DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
                               SENDMESS-MSG1, X"0A"
                               SENDMESS-MSG2.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
      D    STOP FORM-PATHNAME.

           MOVE "X"               TO RECORDS-FOUND.
           MOVE FORM-PATHNAME     TO FORM-PATH.
           MOVE "IOESIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOESIN-QUEUE-POS  TO Q-POS.
           MOVE IOESIN-COPIES-POS TO C-POS.
           MOVE IOESIN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              PERFORM GET-TIME
              MOVE SPACES TO LEGEND
              STRING "STARTING TIME: ", TIME-EDIT
                     DELIMITED BY SIZE INTO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
              MOVE BEG-BRANCH TO SV-BEG-BRANCH
              MOVE END-BRANCH TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                           END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH         TO SV-BEG-BRANCH
              MOVE END-BRANCH         TO SV-END-BRANCH
              MOVE GROUP-FLAG         TO SV-GROUP-FLAG
              MOVE ENT-GROUP          TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP        TO ENT-GROUP
              MOVE ZEROES             TO BEG-BRANCH
              MOVE 9999               TO END-BRANCH
              MOVE "Y"                TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

           PERFORM GET-GPENV.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           PERFORM ENABLE-INTERRUPT.
           PERFORM SEND-PACE-HEAD.

           MOVE ZEROES TO PACE-LEV.
           MOVE ZEROES TO RECORDS-READ.

           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE "R" TO TIMER-TYPE.
           PERFORM TIMER-START.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.

      *=================================================================
       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF ( UP-KEY )
              GO TO ENTER-END-ACCTNO.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE ZEROES TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.

           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N"                    TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BR-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" ) GO TO ENTER-BEG-BRANCH.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 = ZEROES ) OR
              ( VDUNUM0 > 1      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-BEG-ACCTNO.

           MOVE "ENNN080BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE "ENNN080END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCTNO.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."   TO VDU
              MOVE ENT-GROUP              TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."   TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."   TO VDU
              MOVE BEG-BRANCH             TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."   TO VDU
              MOVE END-BRANCH             TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN080BEG-ACCTNO."      TO VDU.
           MOVE BEG-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO."      TO VDU.
           MOVE END-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

      *    MOVE EXT-EOBUF-DATE1 TO ???.
      *    MOVE EXT-EOBUF-DATE2 TO ???.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S C A N - G R 1 - F I L E
      *
      ******************************************************************
       SCAN-GR1-FILE SECTION.

           MOVE GRSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF   TO GRSCAN-BUF.

           IF ( SCAN-STATUS NOT = "00"   ) OR
              ( GRSCAN-KEY      = SPACES )
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GR1-FILE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - -{ VALID SELECTION  }
           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY TO VDUBUF.
           MOVE "00"       TO VDUSTATUS.

      *=================================================================
       SCAN-GR1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - B R A N C H - M O D U L E
      *
      ******************************************************************
       PROCESS-BRANCH-MODULE SECTION.

           MOVE PACE-COUNTER-MAX TO PACE-COUNTER.

      *- - - - - - - - - - - - - - - - - -{ VERIFY 'LOCAL' FILE PATHS  }
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO ES-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QES1-WBEG-BRNO.
           MOVE BR-NO     TO QES1-WEND-BRNO.

           MOVE BEG-ACCTNO TO QES1-WBEG-ACCTNO
           MOVE END-ACCTNO TO QES1-WEND-ACCTNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-ES1-FILE
              PERFORM START-ES1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-ES1-FILE-NEXT.

           IF ( IO-BAD ) OR ( ES-BRNO NOT = ES-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( ES-ACCTNO < BEG-ACCTNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( ES-ACCTNO > END-ACCTNO )
                 GO TO PROCESS-BRANCH-MODULE-END.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE ES-BRNO                    TO D01-ES-BRNO.
           MOVE ES-ACCTNO                  TO D01-ES-ACCTNO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 1 )
              PERFORM CLOSE-ES1-FILE.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P A C E - R O U T I N E
      *
      *=================================================================
      * DESC: DISPLAY THE PACE OF THE EXECUTION OF THE PROGRAM.
      ******************************************************************
       PACE-ROUTINE SECTION.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO PACE-ROUTINE-EXIT.

           ADD 01 TO PACE-COUNTER.

           IF ( PACE-COUNTER <= PACE-COUNTER-MAX )
              GO TO PACE-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD 01 TO PACE-LEV.

           IF ( PACE-LEV <= PACE-MAX )
              GO TO PACE-ROUTINE-UPDATE.

           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > PACE-MAX
              MOVE PACE-ELE(SUB) TO PACE-ELE(SUB - 1)
           END-PERFORM.

           MOVE PACE-MAX TO PACE-LEV.
           MOVE SPACES   TO PACE-ELE(PACE-LEV).

      *-----------------------------------------------------------------
       PACE-ROUTINE-UPDATE.

           MOVE SPACES TO PACE-DETAIL.

           PERFORM GET-TIME.
           MOVE CUR-HH    TO PACE-TIME-HH.
           MOVE CUR-MM    TO PACE-TIME-MM.
           MOVE CUR-SS    TO PACE-TIME-SS.
           MOVE CUR-HD    TO PACE-TIME-HD.
           MOVE PACE-TIME TO PACE-DETAIL-TIME.

           IF ( PROCESS-KEY = 1 )
              MOVE ES1-KEY TO PACE-DETAIL-KEY.

           MOVE PACE-DETAIL TO PACE-ELE(PACE-LEV).
           MOVE PACE-BUF    TO WR-BUF.
           MOVE PACE-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES TO PACE-COUNTER.

      *=================================================================
       PACE-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - S U M M A R Y
      *
      ******************************************************************
       WRITE-SUMMARY SECTION.

           IF ( NO-RECORDS-FOUND )
              GO TO WRITE-SUMMARY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID NAME                 : " TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER       : " TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH NUMBER       : " TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 1                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       WRITE-SUMMARY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - P A G E - H E A D E R
      *
      ******************************************************************
       WRITE-PAGE-HEADER SECTION.

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{01}
           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "SQL TABLE IO UTILITY - ESFILE"  TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           MOVE HEADER-LINE-03                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01 TO LN-FEED.

      *=================================================================
       WRITE-PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-CONAME TO IOESIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOESIN-SYSDATE.
           DISPLAY IOESIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOESIN-FORM-FIELDS TO WR-BUF.
           MOVE IOESIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=================================================================
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE IOESIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOESIN-HELP--DIR  TO HELP-LINK-DIR.

      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBIO/IOESIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *=================================================================
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      *COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *COPY "LIBGB/RMFILE.CPY".
      *=================================================================
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-PACE-HEAD.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE PACE-COUNTER-MAX    TO PACE-HEAD-MAX
              MOVE "S  A400PACE-HEAD." TO VDU
              MOVE PACE-HEAD-LINE      TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBIO/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

 
      *-----------------------------------------------------------------
      *    LOAD ES-REC FROM QES-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-ES-FIELDS.
           MOVE QES-BRNO TO ES-BRNO.
           MOVE QES-ACCTNO TO ES-ACCTNO.

           IF ( QES-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO ES-LTOUCH-DATE
           ELSE
              MOVE QES-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-LTOUCH-DATE.


           IF ( QES-SCHD-EFF-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(1)
           ELSE
              MOVE QES-SCHD-EFF-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(1).


           IF ( QES-SCHD-1STPYDATE-1 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(1)
           ELSE
              MOVE QES-SCHD-1STPYDATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(1).

           MOVE QES-SCHD-ESCROW-1 TO ES-SCHD-ESCROW(1).
           MOVE QES-SCHD-TERM-1 TO ES-SCHD-TERM(1).
           MOVE QES-SCHD-COLLECTED-1 TO ES-SCHD-COLLECTED(1).

           IF ( QES-SCHD-EFF-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(2)
           ELSE
              MOVE QES-SCHD-EFF-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(2).


           IF ( QES-SCHD-1STPYDATE-2 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(2)
           ELSE
              MOVE QES-SCHD-1STPYDATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(2).

           MOVE QES-SCHD-ESCROW-2 TO ES-SCHD-ESCROW(2).
           MOVE QES-SCHD-TERM-2 TO ES-SCHD-TERM(2).
           MOVE QES-SCHD-COLLECTED-2 TO ES-SCHD-COLLECTED(2).

           IF ( QES-SCHD-EFF-DATE-3 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(3)
           ELSE
              MOVE QES-SCHD-EFF-DATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(3).


           IF ( QES-SCHD-1STPYDATE-3 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(3)
           ELSE
              MOVE QES-SCHD-1STPYDATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(3).

           MOVE QES-SCHD-ESCROW-3 TO ES-SCHD-ESCROW(3).
           MOVE QES-SCHD-TERM-3 TO ES-SCHD-TERM(3).
           MOVE QES-SCHD-COLLECTED-3 TO ES-SCHD-COLLECTED(3).

           IF ( QES-SCHD-EFF-DATE-4 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(4)
           ELSE
              MOVE QES-SCHD-EFF-DATE-4 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(4).


           IF ( QES-SCHD-1STPYDATE-4 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(4)
           ELSE
              MOVE QES-SCHD-1STPYDATE-4 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(4).

           MOVE QES-SCHD-ESCROW-4 TO ES-SCHD-ESCROW(4).
           MOVE QES-SCHD-TERM-4 TO ES-SCHD-TERM(4).
           MOVE QES-SCHD-COLLECTED-4 TO ES-SCHD-COLLECTED(4).

           IF ( QES-SCHD-EFF-DATE-5 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(5)
           ELSE
              MOVE QES-SCHD-EFF-DATE-5 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(5).


           IF ( QES-SCHD-1STPYDATE-5 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(5)
           ELSE
              MOVE QES-SCHD-1STPYDATE-5 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(5).

           MOVE QES-SCHD-ESCROW-5 TO ES-SCHD-ESCROW(5).
           MOVE QES-SCHD-TERM-5 TO ES-SCHD-TERM(5).
           MOVE QES-SCHD-COLLECTED-5 TO ES-SCHD-COLLECTED(5).

           IF ( QES-SCHD-EFF-DATE-6 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(6)
           ELSE
              MOVE QES-SCHD-EFF-DATE-6 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(6).


           IF ( QES-SCHD-1STPYDATE-6 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(6)
           ELSE
              MOVE QES-SCHD-1STPYDATE-6 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(6).

           MOVE QES-SCHD-ESCROW-6 TO ES-SCHD-ESCROW(6).
           MOVE QES-SCHD-TERM-6 TO ES-SCHD-TERM(6).
           MOVE QES-SCHD-COLLECTED-6 TO ES-SCHD-COLLECTED(6).

           IF ( QES-SCHD-EFF-DATE-7 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(7)
           ELSE
              MOVE QES-SCHD-EFF-DATE-7 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(7).


           IF ( QES-SCHD-1STPYDATE-7 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(7)
           ELSE
              MOVE QES-SCHD-1STPYDATE-7 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(7).

           MOVE QES-SCHD-ESCROW-7 TO ES-SCHD-ESCROW(7).
           MOVE QES-SCHD-TERM-7 TO ES-SCHD-TERM(7).
           MOVE QES-SCHD-COLLECTED-7 TO ES-SCHD-COLLECTED(7).

           IF ( QES-SCHD-EFF-DATE-8 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(8)
           ELSE
              MOVE QES-SCHD-EFF-DATE-8 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(8).


           IF ( QES-SCHD-1STPYDATE-8 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(8)
           ELSE
              MOVE QES-SCHD-1STPYDATE-8 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(8).

           MOVE QES-SCHD-ESCROW-8 TO ES-SCHD-ESCROW(8).
           MOVE QES-SCHD-TERM-8 TO ES-SCHD-TERM(8).
           MOVE QES-SCHD-COLLECTED-8 TO ES-SCHD-COLLECTED(8).

           IF ( QES-SCHD-EFF-DATE-9 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(9)
           ELSE
              MOVE QES-SCHD-EFF-DATE-9 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(9).


           IF ( QES-SCHD-1STPYDATE-9 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(9)
           ELSE
              MOVE QES-SCHD-1STPYDATE-9 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(9).

           MOVE QES-SCHD-ESCROW-9 TO ES-SCHD-ESCROW(9).
           MOVE QES-SCHD-TERM-9 TO ES-SCHD-TERM(9).
           MOVE QES-SCHD-COLLECTED-9 TO ES-SCHD-COLLECTED(9).

           IF ( QES-SCHD-EFF-DATE-10 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(10)
           ELSE
              MOVE QES-SCHD-EFF-DATE-10 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(10).


           IF ( QES-SCHD-1STPYDATE-10 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(10)
           ELSE
              MOVE QES-SCHD-1STPYDATE-10 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(10).

           MOVE QES-SCHD-ESCROW-10 TO ES-SCHD-ESCROW(10).
           MOVE QES-SCHD-TERM-10 TO ES-SCHD-TERM(10).
           MOVE QES-SCHD-COLLECTED-10 TO ES-SCHD-COLLECTED(10).

           IF ( QES-SCHD-EFF-DATE-11 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(11)
           ELSE
              MOVE QES-SCHD-EFF-DATE-11 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(11).


           IF ( QES-SCHD-1STPYDATE-11 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(11)
           ELSE
              MOVE QES-SCHD-1STPYDATE-11 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(11).

           MOVE QES-SCHD-ESCROW-11 TO ES-SCHD-ESCROW(11).
           MOVE QES-SCHD-TERM-11 TO ES-SCHD-TERM(11).
           MOVE QES-SCHD-COLLECTED-11 TO ES-SCHD-COLLECTED(11).

           IF ( QES-SCHD-EFF-DATE-12 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(12)
           ELSE
              MOVE QES-SCHD-EFF-DATE-12 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(12).


           IF ( QES-SCHD-1STPYDATE-12 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(12)
           ELSE
              MOVE QES-SCHD-1STPYDATE-12 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(12).

           MOVE QES-SCHD-ESCROW-12 TO ES-SCHD-ESCROW(12).
           MOVE QES-SCHD-TERM-12 TO ES-SCHD-TERM(12).
           MOVE QES-SCHD-COLLECTED-12 TO ES-SCHD-COLLECTED(12).

           IF ( QES-SCHD-EFF-DATE-13 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(13)
           ELSE
              MOVE QES-SCHD-EFF-DATE-13 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(13).


           IF ( QES-SCHD-1STPYDATE-13 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(13)
           ELSE
              MOVE QES-SCHD-1STPYDATE-13 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(13).

           MOVE QES-SCHD-ESCROW-13 TO ES-SCHD-ESCROW(13).
           MOVE QES-SCHD-TERM-13 TO ES-SCHD-TERM(13).
           MOVE QES-SCHD-COLLECTED-13 TO ES-SCHD-COLLECTED(13).

           IF ( QES-SCHD-EFF-DATE-14 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(14)
           ELSE
              MOVE QES-SCHD-EFF-DATE-14 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(14).


           IF ( QES-SCHD-1STPYDATE-14 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(14)
           ELSE
              MOVE QES-SCHD-1STPYDATE-14 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(14).

           MOVE QES-SCHD-ESCROW-14 TO ES-SCHD-ESCROW(14).
           MOVE QES-SCHD-TERM-14 TO ES-SCHD-TERM(14).
           MOVE QES-SCHD-COLLECTED-14 TO ES-SCHD-COLLECTED(14).

           IF ( QES-SCHD-EFF-DATE-15 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(15)
           ELSE
              MOVE QES-SCHD-EFF-DATE-15 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(15).


           IF ( QES-SCHD-1STPYDATE-15 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(15)
           ELSE
              MOVE QES-SCHD-1STPYDATE-15 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(15).

           MOVE QES-SCHD-ESCROW-15 TO ES-SCHD-ESCROW(15).
           MOVE QES-SCHD-TERM-15 TO ES-SCHD-TERM(15).
           MOVE QES-SCHD-COLLECTED-15 TO ES-SCHD-COLLECTED(15).

           IF ( QES-SCHD-EFF-DATE-16 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(16)
           ELSE
              MOVE QES-SCHD-EFF-DATE-16 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(16).


           IF ( QES-SCHD-1STPYDATE-16 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(16)
           ELSE
              MOVE QES-SCHD-1STPYDATE-16 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(16).

           MOVE QES-SCHD-ESCROW-16 TO ES-SCHD-ESCROW(16).
           MOVE QES-SCHD-TERM-16 TO ES-SCHD-TERM(16).
           MOVE QES-SCHD-COLLECTED-16 TO ES-SCHD-COLLECTED(16).

           IF ( QES-SCHD-EFF-DATE-17 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(17)
           ELSE
              MOVE QES-SCHD-EFF-DATE-17 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(17).


           IF ( QES-SCHD-1STPYDATE-17 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(17)
           ELSE
              MOVE QES-SCHD-1STPYDATE-17 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(17).

           MOVE QES-SCHD-ESCROW-17 TO ES-SCHD-ESCROW(17).
           MOVE QES-SCHD-TERM-17 TO ES-SCHD-TERM(17).
           MOVE QES-SCHD-COLLECTED-17 TO ES-SCHD-COLLECTED(17).

           IF ( QES-SCHD-EFF-DATE-18 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(18)
           ELSE
              MOVE QES-SCHD-EFF-DATE-18 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(18).


           IF ( QES-SCHD-1STPYDATE-18 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(18)
           ELSE
              MOVE QES-SCHD-1STPYDATE-18 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(18).

           MOVE QES-SCHD-ESCROW-18 TO ES-SCHD-ESCROW(18).
           MOVE QES-SCHD-TERM-18 TO ES-SCHD-TERM(18).
           MOVE QES-SCHD-COLLECTED-18 TO ES-SCHD-COLLECTED(18).

           IF ( QES-SCHD-EFF-DATE-19 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(19)
           ELSE
              MOVE QES-SCHD-EFF-DATE-19 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(19).


           IF ( QES-SCHD-1STPYDATE-19 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(19)
           ELSE
              MOVE QES-SCHD-1STPYDATE-19 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(19).

           MOVE QES-SCHD-ESCROW-19 TO ES-SCHD-ESCROW(19).
           MOVE QES-SCHD-TERM-19 TO ES-SCHD-TERM(19).
           MOVE QES-SCHD-COLLECTED-19 TO ES-SCHD-COLLECTED(19).

           IF ( QES-SCHD-EFF-DATE-20 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(20)
           ELSE
              MOVE QES-SCHD-EFF-DATE-20 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(20).


           IF ( QES-SCHD-1STPYDATE-20 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(20)
           ELSE
              MOVE QES-SCHD-1STPYDATE-20 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(20).

           MOVE QES-SCHD-ESCROW-20 TO ES-SCHD-ESCROW(20).
           MOVE QES-SCHD-TERM-20 TO ES-SCHD-TERM(20).
           MOVE QES-SCHD-COLLECTED-20 TO ES-SCHD-COLLECTED(20).

           IF ( QES-SCHD-EFF-DATE-21 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(21)
           ELSE
              MOVE QES-SCHD-EFF-DATE-21 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(21).


           IF ( QES-SCHD-1STPYDATE-21 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(21)
           ELSE
              MOVE QES-SCHD-1STPYDATE-21 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(21).

           MOVE QES-SCHD-ESCROW-21 TO ES-SCHD-ESCROW(21).
           MOVE QES-SCHD-TERM-21 TO ES-SCHD-TERM(21).
           MOVE QES-SCHD-COLLECTED-21 TO ES-SCHD-COLLECTED(21).

           IF ( QES-SCHD-EFF-DATE-22 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(22)
           ELSE
              MOVE QES-SCHD-EFF-DATE-22 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(22).


           IF ( QES-SCHD-1STPYDATE-22 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(22)
           ELSE
              MOVE QES-SCHD-1STPYDATE-22 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(22).

           MOVE QES-SCHD-ESCROW-22 TO ES-SCHD-ESCROW(22).
           MOVE QES-SCHD-TERM-22 TO ES-SCHD-TERM(22).
           MOVE QES-SCHD-COLLECTED-22 TO ES-SCHD-COLLECTED(22).

           IF ( QES-SCHD-EFF-DATE-23 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(23)
           ELSE
              MOVE QES-SCHD-EFF-DATE-23 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(23).


           IF ( QES-SCHD-1STPYDATE-23 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(23)
           ELSE
              MOVE QES-SCHD-1STPYDATE-23 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(23).

           MOVE QES-SCHD-ESCROW-23 TO ES-SCHD-ESCROW(23).
           MOVE QES-SCHD-TERM-23 TO ES-SCHD-TERM(23).
           MOVE QES-SCHD-COLLECTED-23 TO ES-SCHD-COLLECTED(23).

           IF ( QES-SCHD-EFF-DATE-24 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(24)
           ELSE
              MOVE QES-SCHD-EFF-DATE-24 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(24).


           IF ( QES-SCHD-1STPYDATE-24 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(24)
           ELSE
              MOVE QES-SCHD-1STPYDATE-24 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(24).

           MOVE QES-SCHD-ESCROW-24 TO ES-SCHD-ESCROW(24).
           MOVE QES-SCHD-TERM-24 TO ES-SCHD-TERM(24).
           MOVE QES-SCHD-COLLECTED-24 TO ES-SCHD-COLLECTED(24).

           IF ( QES-SCHD-EFF-DATE-25 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(25)
           ELSE
              MOVE QES-SCHD-EFF-DATE-25 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(25).


           IF ( QES-SCHD-1STPYDATE-25 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(25)
           ELSE
              MOVE QES-SCHD-1STPYDATE-25 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(25).

           MOVE QES-SCHD-ESCROW-25 TO ES-SCHD-ESCROW(25).
           MOVE QES-SCHD-TERM-25 TO ES-SCHD-TERM(25).
           MOVE QES-SCHD-COLLECTED-25 TO ES-SCHD-COLLECTED(25).

           IF ( QES-SCHD-EFF-DATE-26 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(26)
           ELSE
              MOVE QES-SCHD-EFF-DATE-26 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(26).


           IF ( QES-SCHD-1STPYDATE-26 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(26)
           ELSE
              MOVE QES-SCHD-1STPYDATE-26 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(26).

           MOVE QES-SCHD-ESCROW-26 TO ES-SCHD-ESCROW(26).
           MOVE QES-SCHD-TERM-26 TO ES-SCHD-TERM(26).
           MOVE QES-SCHD-COLLECTED-26 TO ES-SCHD-COLLECTED(26).

           IF ( QES-SCHD-EFF-DATE-27 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(27)
           ELSE
              MOVE QES-SCHD-EFF-DATE-27 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(27).


           IF ( QES-SCHD-1STPYDATE-27 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(27)
           ELSE
              MOVE QES-SCHD-1STPYDATE-27 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(27).

           MOVE QES-SCHD-ESCROW-27 TO ES-SCHD-ESCROW(27).
           MOVE QES-SCHD-TERM-27 TO ES-SCHD-TERM(27).
           MOVE QES-SCHD-COLLECTED-27 TO ES-SCHD-COLLECTED(27).

           IF ( QES-SCHD-EFF-DATE-28 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(28)
           ELSE
              MOVE QES-SCHD-EFF-DATE-28 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(28).


           IF ( QES-SCHD-1STPYDATE-28 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(28)
           ELSE
              MOVE QES-SCHD-1STPYDATE-28 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(28).

           MOVE QES-SCHD-ESCROW-28 TO ES-SCHD-ESCROW(28).
           MOVE QES-SCHD-TERM-28 TO ES-SCHD-TERM(28).
           MOVE QES-SCHD-COLLECTED-28 TO ES-SCHD-COLLECTED(28).

           IF ( QES-SCHD-EFF-DATE-29 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(29)
           ELSE
              MOVE QES-SCHD-EFF-DATE-29 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(29).


           IF ( QES-SCHD-1STPYDATE-29 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(29)
           ELSE
              MOVE QES-SCHD-1STPYDATE-29 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(29).

           MOVE QES-SCHD-ESCROW-29 TO ES-SCHD-ESCROW(29).
           MOVE QES-SCHD-TERM-29 TO ES-SCHD-TERM(29).
           MOVE QES-SCHD-COLLECTED-29 TO ES-SCHD-COLLECTED(29).

           IF ( QES-SCHD-EFF-DATE-30 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(30)
           ELSE
              MOVE QES-SCHD-EFF-DATE-30 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(30).


           IF ( QES-SCHD-1STPYDATE-30 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(30)
           ELSE
              MOVE QES-SCHD-1STPYDATE-30 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(30).

           MOVE QES-SCHD-ESCROW-30 TO ES-SCHD-ESCROW(30).
           MOVE QES-SCHD-TERM-30 TO ES-SCHD-TERM(30).
           MOVE QES-SCHD-COLLECTED-30 TO ES-SCHD-COLLECTED(30).

           IF ( QES-SCHD-EFF-DATE-31 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(31)
           ELSE
              MOVE QES-SCHD-EFF-DATE-31 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(31).


           IF ( QES-SCHD-1STPYDATE-31 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(31)
           ELSE
              MOVE QES-SCHD-1STPYDATE-31 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(31).

           MOVE QES-SCHD-ESCROW-31 TO ES-SCHD-ESCROW(31).
           MOVE QES-SCHD-TERM-31 TO ES-SCHD-TERM(31).
           MOVE QES-SCHD-COLLECTED-31 TO ES-SCHD-COLLECTED(31).

           IF ( QES-SCHD-EFF-DATE-32 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(32)
           ELSE
              MOVE QES-SCHD-EFF-DATE-32 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(32).


           IF ( QES-SCHD-1STPYDATE-32 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(32)
           ELSE
              MOVE QES-SCHD-1STPYDATE-32 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(32).

           MOVE QES-SCHD-ESCROW-32 TO ES-SCHD-ESCROW(32).
           MOVE QES-SCHD-TERM-32 TO ES-SCHD-TERM(32).
           MOVE QES-SCHD-COLLECTED-32 TO ES-SCHD-COLLECTED(32).

           IF ( QES-SCHD-EFF-DATE-33 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(33)
           ELSE
              MOVE QES-SCHD-EFF-DATE-33 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(33).


           IF ( QES-SCHD-1STPYDATE-33 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(33)
           ELSE
              MOVE QES-SCHD-1STPYDATE-33 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(33).

           MOVE QES-SCHD-ESCROW-33 TO ES-SCHD-ESCROW(33).
           MOVE QES-SCHD-TERM-33 TO ES-SCHD-TERM(33).
           MOVE QES-SCHD-COLLECTED-33 TO ES-SCHD-COLLECTED(33).

           IF ( QES-SCHD-EFF-DATE-34 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(34)
           ELSE
              MOVE QES-SCHD-EFF-DATE-34 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(34).


           IF ( QES-SCHD-1STPYDATE-34 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(34)
           ELSE
              MOVE QES-SCHD-1STPYDATE-34 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(34).

           MOVE QES-SCHD-ESCROW-34 TO ES-SCHD-ESCROW(34).
           MOVE QES-SCHD-TERM-34 TO ES-SCHD-TERM(34).
           MOVE QES-SCHD-COLLECTED-34 TO ES-SCHD-COLLECTED(34).

           IF ( QES-SCHD-EFF-DATE-35 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(35)
           ELSE
              MOVE QES-SCHD-EFF-DATE-35 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(35).


           IF ( QES-SCHD-1STPYDATE-35 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(35)
           ELSE
              MOVE QES-SCHD-1STPYDATE-35 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(35).

           MOVE QES-SCHD-ESCROW-35 TO ES-SCHD-ESCROW(35).
           MOVE QES-SCHD-TERM-35 TO ES-SCHD-TERM(35).
           MOVE QES-SCHD-COLLECTED-35 TO ES-SCHD-COLLECTED(35).

           IF ( QES-SCHD-EFF-DATE-36 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(36)
           ELSE
              MOVE QES-SCHD-EFF-DATE-36 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(36).


           IF ( QES-SCHD-1STPYDATE-36 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(36)
           ELSE
              MOVE QES-SCHD-1STPYDATE-36 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(36).

           MOVE QES-SCHD-ESCROW-36 TO ES-SCHD-ESCROW(36).
           MOVE QES-SCHD-TERM-36 TO ES-SCHD-TERM(36).
           MOVE QES-SCHD-COLLECTED-36 TO ES-SCHD-COLLECTED(36).

           IF ( QES-SCHD-EFF-DATE-37 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(37)
           ELSE
              MOVE QES-SCHD-EFF-DATE-37 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(37).


           IF ( QES-SCHD-1STPYDATE-37 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(37)
           ELSE
              MOVE QES-SCHD-1STPYDATE-37 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(37).

           MOVE QES-SCHD-ESCROW-37 TO ES-SCHD-ESCROW(37).
           MOVE QES-SCHD-TERM-37 TO ES-SCHD-TERM(37).
           MOVE QES-SCHD-COLLECTED-37 TO ES-SCHD-COLLECTED(37).

           IF ( QES-SCHD-EFF-DATE-38 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(38)
           ELSE
              MOVE QES-SCHD-EFF-DATE-38 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(38).


           IF ( QES-SCHD-1STPYDATE-38 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(38)
           ELSE
              MOVE QES-SCHD-1STPYDATE-38 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(38).

           MOVE QES-SCHD-ESCROW-38 TO ES-SCHD-ESCROW(38).
           MOVE QES-SCHD-TERM-38 TO ES-SCHD-TERM(38).
           MOVE QES-SCHD-COLLECTED-38 TO ES-SCHD-COLLECTED(38).

           IF ( QES-SCHD-EFF-DATE-39 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(39)
           ELSE
              MOVE QES-SCHD-EFF-DATE-39 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(39).


           IF ( QES-SCHD-1STPYDATE-39 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(39)
           ELSE
              MOVE QES-SCHD-1STPYDATE-39 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(39).

           MOVE QES-SCHD-ESCROW-39 TO ES-SCHD-ESCROW(39).
           MOVE QES-SCHD-TERM-39 TO ES-SCHD-TERM(39).
           MOVE QES-SCHD-COLLECTED-39 TO ES-SCHD-COLLECTED(39).

           IF ( QES-SCHD-EFF-DATE-40 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-EFF-DATE(40)
           ELSE
              MOVE QES-SCHD-EFF-DATE-40 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-EFF-DATE(40).


           IF ( QES-SCHD-1STPYDATE-40 = "1900-01-01" )
              MOVE ZEROES TO ES-SCHD-1STPYDATE(40)
           ELSE
              MOVE QES-SCHD-1STPYDATE-40 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-SCHD-1STPYDATE(40).

           MOVE QES-SCHD-ESCROW-40 TO ES-SCHD-ESCROW(40).
           MOVE QES-SCHD-TERM-40 TO ES-SCHD-TERM(40).
           MOVE QES-SCHD-COLLECTED-40 TO ES-SCHD-COLLECTED(40).
           MOVE QES-BALANCE TO ES-BALANCE.
           MOVE QES-PREPAID-1 TO ES-PREPAID(1).
           MOVE QES-COLLECTED-1 TO ES-COLLECTED(1).
           MOVE QES-DISBURSED-TAXES-1 TO ES-DISBURSED-TAXES(1).
           MOVE QES-DISBURSED-INS-1 TO ES-DISBURSED-INS(1).
           MOVE QES-DISBURSED-PMI-1 TO ES-DISBURSED-PMI(1).
           MOVE QES-DISBURSED-OTHER-1 TO ES-DISBURSED-OTHER(1).
           MOVE QES-DISBURSED-REF-1 TO ES-DISBURSED-REF(1).
           MOVE QES-DISBURSED-CNT-TAXES-1 TO ES-DISBURSED-CNT-TAXES(1).
           MOVE QES-DISBURSED-MISC1-1 TO ES-DISBURSED-MISC1(1).
           MOVE QES-DISBURSED-MISC2-1 TO ES-DISBURSED-MISC2(1).
           MOVE QES-PREPAID-2 TO ES-PREPAID(2).
           MOVE QES-COLLECTED-2 TO ES-COLLECTED(2).
           MOVE QES-DISBURSED-TAXES-2 TO ES-DISBURSED-TAXES(2).
           MOVE QES-DISBURSED-INS-2 TO ES-DISBURSED-INS(2).
           MOVE QES-DISBURSED-PMI-2 TO ES-DISBURSED-PMI(2).
           MOVE QES-DISBURSED-OTHER-2 TO ES-DISBURSED-OTHER(2).
           MOVE QES-DISBURSED-REF-2 TO ES-DISBURSED-REF(2).
           MOVE QES-DISBURSED-CNT-TAXES-2 TO ES-DISBURSED-CNT-TAXES(2).
           MOVE QES-DISBURSED-MISC1-2 TO ES-DISBURSED-MISC1(2).
           MOVE QES-DISBURSED-MISC2-2 TO ES-DISBURSED-MISC2(2).
           MOVE QES-PREPAID-3 TO ES-PREPAID(3).
           MOVE QES-COLLECTED-3 TO ES-COLLECTED(3).
           MOVE QES-DISBURSED-TAXES-3 TO ES-DISBURSED-TAXES(3).
           MOVE QES-DISBURSED-INS-3 TO ES-DISBURSED-INS(3).
           MOVE QES-DISBURSED-PMI-3 TO ES-DISBURSED-PMI(3).
           MOVE QES-DISBURSED-OTHER-3 TO ES-DISBURSED-OTHER(3).
           MOVE QES-DISBURSED-REF-3 TO ES-DISBURSED-REF(3).
           MOVE QES-DISBURSED-CNT-TAXES-3 TO ES-DISBURSED-CNT-TAXES(3).
           MOVE QES-DISBURSED-MISC1-3 TO ES-DISBURSED-MISC1(3).
           MOVE QES-DISBURSED-MISC2-3 TO ES-DISBURSED-MISC2(3).
           MOVE QES-MISC-AMT-1 TO ES-MISC-AMT(1).

           IF ( QES-EXP-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO ES-EXP-DATE(1)
           ELSE
              MOVE QES-EXP-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-EXP-DATE(1).

           MOVE QES-MISC-AMT-2 TO ES-MISC-AMT(2).

           IF ( QES-EXP-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO ES-EXP-DATE(2)
           ELSE
              MOVE QES-EXP-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-EXP-DATE(2).

           MOVE QES-MISC-AMT-3 TO ES-MISC-AMT(3).

           IF ( QES-EXP-DATE-3 = "1900-01-01" )
              MOVE ZEROES TO ES-EXP-DATE(3)
           ELSE
              MOVE QES-EXP-DATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-EXP-DATE(3).

           MOVE QES-MISC-AMT-4 TO ES-MISC-AMT(4).

           IF ( QES-EXP-DATE-4 = "1900-01-01" )
              MOVE ZEROES TO ES-EXP-DATE(4)
           ELSE
              MOVE QES-EXP-DATE-4 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-EXP-DATE(4).

           MOVE QES-MISC-AMT-5 TO ES-MISC-AMT(5).

           IF ( QES-EXP-DATE-5 = "1900-01-01" )
              MOVE ZEROES TO ES-EXP-DATE(5)
           ELSE
              MOVE QES-EXP-DATE-5 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-EXP-DATE(5).

           MOVE QES-MISC-AMT-6 TO ES-MISC-AMT(6).

           IF ( QES-EXP-DATE-6 = "1900-01-01" )
              MOVE ZEROES TO ES-EXP-DATE(6)
           ELSE
              MOVE QES-EXP-DATE-6 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ES-EXP-DATE(6).

 
      *-----------------------------------------------------------------
      *    LOAD QES-REC FROM ES-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-ES-FIELDS.
           MOVE ES-BRNO TO QES-BRNO.
           MOVE ES-ACCTNO TO QES-ACCTNO.

           IF ( ES-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QES-LTOUCH-DATE
           ELSE
              MOVE ES-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-LTOUCH-DATE.


           IF ( ES-SCHD-EFF-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-1
           ELSE
              MOVE ES-SCHD-EFF-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-1.


           IF ( ES-SCHD-1STPYDATE(1) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-1
           ELSE
              MOVE ES-SCHD-1STPYDATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-1.

           MOVE ES-SCHD-ESCROW(1) TO QES-SCHD-ESCROW-1.
           MOVE ES-SCHD-TERM(1) TO QES-SCHD-TERM-1.
           MOVE ES-SCHD-COLLECTED(1) TO QES-SCHD-COLLECTED-1.

           IF ( ES-SCHD-EFF-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-2
           ELSE
              MOVE ES-SCHD-EFF-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-2.


           IF ( ES-SCHD-1STPYDATE(2) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-2
           ELSE
              MOVE ES-SCHD-1STPYDATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-2.

           MOVE ES-SCHD-ESCROW(2) TO QES-SCHD-ESCROW-2.
           MOVE ES-SCHD-TERM(2) TO QES-SCHD-TERM-2.
           MOVE ES-SCHD-COLLECTED(2) TO QES-SCHD-COLLECTED-2.

           IF ( ES-SCHD-EFF-DATE(3) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-3
           ELSE
              MOVE ES-SCHD-EFF-DATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-3.


           IF ( ES-SCHD-1STPYDATE(3) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-3
           ELSE
              MOVE ES-SCHD-1STPYDATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-3.

           MOVE ES-SCHD-ESCROW(3) TO QES-SCHD-ESCROW-3.
           MOVE ES-SCHD-TERM(3) TO QES-SCHD-TERM-3.
           MOVE ES-SCHD-COLLECTED(3) TO QES-SCHD-COLLECTED-3.

           IF ( ES-SCHD-EFF-DATE(4) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-4
           ELSE
              MOVE ES-SCHD-EFF-DATE(4) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-4.


           IF ( ES-SCHD-1STPYDATE(4) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-4
           ELSE
              MOVE ES-SCHD-1STPYDATE(4) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-4.

           MOVE ES-SCHD-ESCROW(4) TO QES-SCHD-ESCROW-4.
           MOVE ES-SCHD-TERM(4) TO QES-SCHD-TERM-4.
           MOVE ES-SCHD-COLLECTED(4) TO QES-SCHD-COLLECTED-4.

           IF ( ES-SCHD-EFF-DATE(5) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-5
           ELSE
              MOVE ES-SCHD-EFF-DATE(5) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-5.


           IF ( ES-SCHD-1STPYDATE(5) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-5
           ELSE
              MOVE ES-SCHD-1STPYDATE(5) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-5.

           MOVE ES-SCHD-ESCROW(5) TO QES-SCHD-ESCROW-5.
           MOVE ES-SCHD-TERM(5) TO QES-SCHD-TERM-5.
           MOVE ES-SCHD-COLLECTED(5) TO QES-SCHD-COLLECTED-5.

           IF ( ES-SCHD-EFF-DATE(6) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-6
           ELSE
              MOVE ES-SCHD-EFF-DATE(6) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-6.


           IF ( ES-SCHD-1STPYDATE(6) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-6
           ELSE
              MOVE ES-SCHD-1STPYDATE(6) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-6.

           MOVE ES-SCHD-ESCROW(6) TO QES-SCHD-ESCROW-6.
           MOVE ES-SCHD-TERM(6) TO QES-SCHD-TERM-6.
           MOVE ES-SCHD-COLLECTED(6) TO QES-SCHD-COLLECTED-6.

           IF ( ES-SCHD-EFF-DATE(7) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-7
           ELSE
              MOVE ES-SCHD-EFF-DATE(7) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-7.


           IF ( ES-SCHD-1STPYDATE(7) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-7
           ELSE
              MOVE ES-SCHD-1STPYDATE(7) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-7.

           MOVE ES-SCHD-ESCROW(7) TO QES-SCHD-ESCROW-7.
           MOVE ES-SCHD-TERM(7) TO QES-SCHD-TERM-7.
           MOVE ES-SCHD-COLLECTED(7) TO QES-SCHD-COLLECTED-7.

           IF ( ES-SCHD-EFF-DATE(8) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-8
           ELSE
              MOVE ES-SCHD-EFF-DATE(8) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-8.


           IF ( ES-SCHD-1STPYDATE(8) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-8
           ELSE
              MOVE ES-SCHD-1STPYDATE(8) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-8.

           MOVE ES-SCHD-ESCROW(8) TO QES-SCHD-ESCROW-8.
           MOVE ES-SCHD-TERM(8) TO QES-SCHD-TERM-8.
           MOVE ES-SCHD-COLLECTED(8) TO QES-SCHD-COLLECTED-8.

           IF ( ES-SCHD-EFF-DATE(9) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-9
           ELSE
              MOVE ES-SCHD-EFF-DATE(9) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-9.


           IF ( ES-SCHD-1STPYDATE(9) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-9
           ELSE
              MOVE ES-SCHD-1STPYDATE(9) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-9.

           MOVE ES-SCHD-ESCROW(9) TO QES-SCHD-ESCROW-9.
           MOVE ES-SCHD-TERM(9) TO QES-SCHD-TERM-9.
           MOVE ES-SCHD-COLLECTED(9) TO QES-SCHD-COLLECTED-9.

           IF ( ES-SCHD-EFF-DATE(10) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-10
           ELSE
              MOVE ES-SCHD-EFF-DATE(10) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-10.


           IF ( ES-SCHD-1STPYDATE(10) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-10
           ELSE
              MOVE ES-SCHD-1STPYDATE(10) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-10.

           MOVE ES-SCHD-ESCROW(10) TO QES-SCHD-ESCROW-10.
           MOVE ES-SCHD-TERM(10) TO QES-SCHD-TERM-10.
           MOVE ES-SCHD-COLLECTED(10) TO QES-SCHD-COLLECTED-10.

           IF ( ES-SCHD-EFF-DATE(11) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-11
           ELSE
              MOVE ES-SCHD-EFF-DATE(11) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-11.


           IF ( ES-SCHD-1STPYDATE(11) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-11
           ELSE
              MOVE ES-SCHD-1STPYDATE(11) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-11.

           MOVE ES-SCHD-ESCROW(11) TO QES-SCHD-ESCROW-11.
           MOVE ES-SCHD-TERM(11) TO QES-SCHD-TERM-11.
           MOVE ES-SCHD-COLLECTED(11) TO QES-SCHD-COLLECTED-11.

           IF ( ES-SCHD-EFF-DATE(12) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-12
           ELSE
              MOVE ES-SCHD-EFF-DATE(12) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-12.


           IF ( ES-SCHD-1STPYDATE(12) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-12
           ELSE
              MOVE ES-SCHD-1STPYDATE(12) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-12.

           MOVE ES-SCHD-ESCROW(12) TO QES-SCHD-ESCROW-12.
           MOVE ES-SCHD-TERM(12) TO QES-SCHD-TERM-12.
           MOVE ES-SCHD-COLLECTED(12) TO QES-SCHD-COLLECTED-12.

           IF ( ES-SCHD-EFF-DATE(13) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-13
           ELSE
              MOVE ES-SCHD-EFF-DATE(13) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-13.


           IF ( ES-SCHD-1STPYDATE(13) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-13
           ELSE
              MOVE ES-SCHD-1STPYDATE(13) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-13.

           MOVE ES-SCHD-ESCROW(13) TO QES-SCHD-ESCROW-13.
           MOVE ES-SCHD-TERM(13) TO QES-SCHD-TERM-13.
           MOVE ES-SCHD-COLLECTED(13) TO QES-SCHD-COLLECTED-13.

           IF ( ES-SCHD-EFF-DATE(14) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-14
           ELSE
              MOVE ES-SCHD-EFF-DATE(14) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-14.


           IF ( ES-SCHD-1STPYDATE(14) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-14
           ELSE
              MOVE ES-SCHD-1STPYDATE(14) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-14.

           MOVE ES-SCHD-ESCROW(14) TO QES-SCHD-ESCROW-14.
           MOVE ES-SCHD-TERM(14) TO QES-SCHD-TERM-14.
           MOVE ES-SCHD-COLLECTED(14) TO QES-SCHD-COLLECTED-14.

           IF ( ES-SCHD-EFF-DATE(15) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-15
           ELSE
              MOVE ES-SCHD-EFF-DATE(15) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-15.


           IF ( ES-SCHD-1STPYDATE(15) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-15
           ELSE
              MOVE ES-SCHD-1STPYDATE(15) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-15.

           MOVE ES-SCHD-ESCROW(15) TO QES-SCHD-ESCROW-15.
           MOVE ES-SCHD-TERM(15) TO QES-SCHD-TERM-15.
           MOVE ES-SCHD-COLLECTED(15) TO QES-SCHD-COLLECTED-15.

           IF ( ES-SCHD-EFF-DATE(16) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-16
           ELSE
              MOVE ES-SCHD-EFF-DATE(16) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-16.


           IF ( ES-SCHD-1STPYDATE(16) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-16
           ELSE
              MOVE ES-SCHD-1STPYDATE(16) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-16.

           MOVE ES-SCHD-ESCROW(16) TO QES-SCHD-ESCROW-16.
           MOVE ES-SCHD-TERM(16) TO QES-SCHD-TERM-16.
           MOVE ES-SCHD-COLLECTED(16) TO QES-SCHD-COLLECTED-16.

           IF ( ES-SCHD-EFF-DATE(17) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-17
           ELSE
              MOVE ES-SCHD-EFF-DATE(17) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-17.


           IF ( ES-SCHD-1STPYDATE(17) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-17
           ELSE
              MOVE ES-SCHD-1STPYDATE(17) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-17.

           MOVE ES-SCHD-ESCROW(17) TO QES-SCHD-ESCROW-17.
           MOVE ES-SCHD-TERM(17) TO QES-SCHD-TERM-17.
           MOVE ES-SCHD-COLLECTED(17) TO QES-SCHD-COLLECTED-17.

           IF ( ES-SCHD-EFF-DATE(18) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-18
           ELSE
              MOVE ES-SCHD-EFF-DATE(18) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-18.


           IF ( ES-SCHD-1STPYDATE(18) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-18
           ELSE
              MOVE ES-SCHD-1STPYDATE(18) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-18.

           MOVE ES-SCHD-ESCROW(18) TO QES-SCHD-ESCROW-18.
           MOVE ES-SCHD-TERM(18) TO QES-SCHD-TERM-18.
           MOVE ES-SCHD-COLLECTED(18) TO QES-SCHD-COLLECTED-18.

           IF ( ES-SCHD-EFF-DATE(19) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-19
           ELSE
              MOVE ES-SCHD-EFF-DATE(19) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-19.


           IF ( ES-SCHD-1STPYDATE(19) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-19
           ELSE
              MOVE ES-SCHD-1STPYDATE(19) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-19.

           MOVE ES-SCHD-ESCROW(19) TO QES-SCHD-ESCROW-19.
           MOVE ES-SCHD-TERM(19) TO QES-SCHD-TERM-19.
           MOVE ES-SCHD-COLLECTED(19) TO QES-SCHD-COLLECTED-19.

           IF ( ES-SCHD-EFF-DATE(20) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-20
           ELSE
              MOVE ES-SCHD-EFF-DATE(20) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-20.


           IF ( ES-SCHD-1STPYDATE(20) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-20
           ELSE
              MOVE ES-SCHD-1STPYDATE(20) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-20.

           MOVE ES-SCHD-ESCROW(20) TO QES-SCHD-ESCROW-20.
           MOVE ES-SCHD-TERM(20) TO QES-SCHD-TERM-20.
           MOVE ES-SCHD-COLLECTED(20) TO QES-SCHD-COLLECTED-20.

           IF ( ES-SCHD-EFF-DATE(21) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-21
           ELSE
              MOVE ES-SCHD-EFF-DATE(21) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-21.


           IF ( ES-SCHD-1STPYDATE(21) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-21
           ELSE
              MOVE ES-SCHD-1STPYDATE(21) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-21.

           MOVE ES-SCHD-ESCROW(21) TO QES-SCHD-ESCROW-21.
           MOVE ES-SCHD-TERM(21) TO QES-SCHD-TERM-21.
           MOVE ES-SCHD-COLLECTED(21) TO QES-SCHD-COLLECTED-21.

           IF ( ES-SCHD-EFF-DATE(22) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-22
           ELSE
              MOVE ES-SCHD-EFF-DATE(22) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-22.


           IF ( ES-SCHD-1STPYDATE(22) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-22
           ELSE
              MOVE ES-SCHD-1STPYDATE(22) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-22.

           MOVE ES-SCHD-ESCROW(22) TO QES-SCHD-ESCROW-22.
           MOVE ES-SCHD-TERM(22) TO QES-SCHD-TERM-22.
           MOVE ES-SCHD-COLLECTED(22) TO QES-SCHD-COLLECTED-22.

           IF ( ES-SCHD-EFF-DATE(23) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-23
           ELSE
              MOVE ES-SCHD-EFF-DATE(23) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-23.


           IF ( ES-SCHD-1STPYDATE(23) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-23
           ELSE
              MOVE ES-SCHD-1STPYDATE(23) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-23.

           MOVE ES-SCHD-ESCROW(23) TO QES-SCHD-ESCROW-23.
           MOVE ES-SCHD-TERM(23) TO QES-SCHD-TERM-23.
           MOVE ES-SCHD-COLLECTED(23) TO QES-SCHD-COLLECTED-23.

           IF ( ES-SCHD-EFF-DATE(24) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-24
           ELSE
              MOVE ES-SCHD-EFF-DATE(24) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-24.


           IF ( ES-SCHD-1STPYDATE(24) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-24
           ELSE
              MOVE ES-SCHD-1STPYDATE(24) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-24.

           MOVE ES-SCHD-ESCROW(24) TO QES-SCHD-ESCROW-24.
           MOVE ES-SCHD-TERM(24) TO QES-SCHD-TERM-24.
           MOVE ES-SCHD-COLLECTED(24) TO QES-SCHD-COLLECTED-24.

           IF ( ES-SCHD-EFF-DATE(25) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-25
           ELSE
              MOVE ES-SCHD-EFF-DATE(25) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-25.


           IF ( ES-SCHD-1STPYDATE(25) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-25
           ELSE
              MOVE ES-SCHD-1STPYDATE(25) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-25.

           MOVE ES-SCHD-ESCROW(25) TO QES-SCHD-ESCROW-25.
           MOVE ES-SCHD-TERM(25) TO QES-SCHD-TERM-25.
           MOVE ES-SCHD-COLLECTED(25) TO QES-SCHD-COLLECTED-25.

           IF ( ES-SCHD-EFF-DATE(26) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-26
           ELSE
              MOVE ES-SCHD-EFF-DATE(26) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-26.


           IF ( ES-SCHD-1STPYDATE(26) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-26
           ELSE
              MOVE ES-SCHD-1STPYDATE(26) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-26.

           MOVE ES-SCHD-ESCROW(26) TO QES-SCHD-ESCROW-26.
           MOVE ES-SCHD-TERM(26) TO QES-SCHD-TERM-26.
           MOVE ES-SCHD-COLLECTED(26) TO QES-SCHD-COLLECTED-26.

           IF ( ES-SCHD-EFF-DATE(27) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-27
           ELSE
              MOVE ES-SCHD-EFF-DATE(27) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-27.


           IF ( ES-SCHD-1STPYDATE(27) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-27
           ELSE
              MOVE ES-SCHD-1STPYDATE(27) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-27.

           MOVE ES-SCHD-ESCROW(27) TO QES-SCHD-ESCROW-27.
           MOVE ES-SCHD-TERM(27) TO QES-SCHD-TERM-27.
           MOVE ES-SCHD-COLLECTED(27) TO QES-SCHD-COLLECTED-27.

           IF ( ES-SCHD-EFF-DATE(28) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-28
           ELSE
              MOVE ES-SCHD-EFF-DATE(28) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-28.


           IF ( ES-SCHD-1STPYDATE(28) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-28
           ELSE
              MOVE ES-SCHD-1STPYDATE(28) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-28.

           MOVE ES-SCHD-ESCROW(28) TO QES-SCHD-ESCROW-28.
           MOVE ES-SCHD-TERM(28) TO QES-SCHD-TERM-28.
           MOVE ES-SCHD-COLLECTED(28) TO QES-SCHD-COLLECTED-28.

           IF ( ES-SCHD-EFF-DATE(29) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-29
           ELSE
              MOVE ES-SCHD-EFF-DATE(29) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-29.


           IF ( ES-SCHD-1STPYDATE(29) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-29
           ELSE
              MOVE ES-SCHD-1STPYDATE(29) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-29.

           MOVE ES-SCHD-ESCROW(29) TO QES-SCHD-ESCROW-29.
           MOVE ES-SCHD-TERM(29) TO QES-SCHD-TERM-29.
           MOVE ES-SCHD-COLLECTED(29) TO QES-SCHD-COLLECTED-29.

           IF ( ES-SCHD-EFF-DATE(30) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-30
           ELSE
              MOVE ES-SCHD-EFF-DATE(30) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-30.


           IF ( ES-SCHD-1STPYDATE(30) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-30
           ELSE
              MOVE ES-SCHD-1STPYDATE(30) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-30.

           MOVE ES-SCHD-ESCROW(30) TO QES-SCHD-ESCROW-30.
           MOVE ES-SCHD-TERM(30) TO QES-SCHD-TERM-30.
           MOVE ES-SCHD-COLLECTED(30) TO QES-SCHD-COLLECTED-30.

           IF ( ES-SCHD-EFF-DATE(31) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-31
           ELSE
              MOVE ES-SCHD-EFF-DATE(31) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-31.


           IF ( ES-SCHD-1STPYDATE(31) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-31
           ELSE
              MOVE ES-SCHD-1STPYDATE(31) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-31.

           MOVE ES-SCHD-ESCROW(31) TO QES-SCHD-ESCROW-31.
           MOVE ES-SCHD-TERM(31) TO QES-SCHD-TERM-31.
           MOVE ES-SCHD-COLLECTED(31) TO QES-SCHD-COLLECTED-31.

           IF ( ES-SCHD-EFF-DATE(32) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-32
           ELSE
              MOVE ES-SCHD-EFF-DATE(32) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-32.


           IF ( ES-SCHD-1STPYDATE(32) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-32
           ELSE
              MOVE ES-SCHD-1STPYDATE(32) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-32.

           MOVE ES-SCHD-ESCROW(32) TO QES-SCHD-ESCROW-32.
           MOVE ES-SCHD-TERM(32) TO QES-SCHD-TERM-32.
           MOVE ES-SCHD-COLLECTED(32) TO QES-SCHD-COLLECTED-32.

           IF ( ES-SCHD-EFF-DATE(33) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-33
           ELSE
              MOVE ES-SCHD-EFF-DATE(33) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-33.


           IF ( ES-SCHD-1STPYDATE(33) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-33
           ELSE
              MOVE ES-SCHD-1STPYDATE(33) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-33.

           MOVE ES-SCHD-ESCROW(33) TO QES-SCHD-ESCROW-33.
           MOVE ES-SCHD-TERM(33) TO QES-SCHD-TERM-33.
           MOVE ES-SCHD-COLLECTED(33) TO QES-SCHD-COLLECTED-33.

           IF ( ES-SCHD-EFF-DATE(34) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-34
           ELSE
              MOVE ES-SCHD-EFF-DATE(34) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-34.


           IF ( ES-SCHD-1STPYDATE(34) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-34
           ELSE
              MOVE ES-SCHD-1STPYDATE(34) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-34.

           MOVE ES-SCHD-ESCROW(34) TO QES-SCHD-ESCROW-34.
           MOVE ES-SCHD-TERM(34) TO QES-SCHD-TERM-34.
           MOVE ES-SCHD-COLLECTED(34) TO QES-SCHD-COLLECTED-34.

           IF ( ES-SCHD-EFF-DATE(35) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-35
           ELSE
              MOVE ES-SCHD-EFF-DATE(35) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-35.


           IF ( ES-SCHD-1STPYDATE(35) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-35
           ELSE
              MOVE ES-SCHD-1STPYDATE(35) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-35.

           MOVE ES-SCHD-ESCROW(35) TO QES-SCHD-ESCROW-35.
           MOVE ES-SCHD-TERM(35) TO QES-SCHD-TERM-35.
           MOVE ES-SCHD-COLLECTED(35) TO QES-SCHD-COLLECTED-35.

           IF ( ES-SCHD-EFF-DATE(36) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-36
           ELSE
              MOVE ES-SCHD-EFF-DATE(36) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-36.


           IF ( ES-SCHD-1STPYDATE(36) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-36
           ELSE
              MOVE ES-SCHD-1STPYDATE(36) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-36.

           MOVE ES-SCHD-ESCROW(36) TO QES-SCHD-ESCROW-36.
           MOVE ES-SCHD-TERM(36) TO QES-SCHD-TERM-36.
           MOVE ES-SCHD-COLLECTED(36) TO QES-SCHD-COLLECTED-36.

           IF ( ES-SCHD-EFF-DATE(37) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-37
           ELSE
              MOVE ES-SCHD-EFF-DATE(37) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-37.


           IF ( ES-SCHD-1STPYDATE(37) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-37
           ELSE
              MOVE ES-SCHD-1STPYDATE(37) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-37.

           MOVE ES-SCHD-ESCROW(37) TO QES-SCHD-ESCROW-37.
           MOVE ES-SCHD-TERM(37) TO QES-SCHD-TERM-37.
           MOVE ES-SCHD-COLLECTED(37) TO QES-SCHD-COLLECTED-37.

           IF ( ES-SCHD-EFF-DATE(38) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-38
           ELSE
              MOVE ES-SCHD-EFF-DATE(38) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-38.


           IF ( ES-SCHD-1STPYDATE(38) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-38
           ELSE
              MOVE ES-SCHD-1STPYDATE(38) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-38.

           MOVE ES-SCHD-ESCROW(38) TO QES-SCHD-ESCROW-38.
           MOVE ES-SCHD-TERM(38) TO QES-SCHD-TERM-38.
           MOVE ES-SCHD-COLLECTED(38) TO QES-SCHD-COLLECTED-38.

           IF ( ES-SCHD-EFF-DATE(39) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-39
           ELSE
              MOVE ES-SCHD-EFF-DATE(39) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-39.


           IF ( ES-SCHD-1STPYDATE(39) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-39
           ELSE
              MOVE ES-SCHD-1STPYDATE(39) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-39.

           MOVE ES-SCHD-ESCROW(39) TO QES-SCHD-ESCROW-39.
           MOVE ES-SCHD-TERM(39) TO QES-SCHD-TERM-39.
           MOVE ES-SCHD-COLLECTED(39) TO QES-SCHD-COLLECTED-39.

           IF ( ES-SCHD-EFF-DATE(40) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-EFF-DATE-40
           ELSE
              MOVE ES-SCHD-EFF-DATE(40) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-EFF-DATE-40.


           IF ( ES-SCHD-1STPYDATE(40) = ZEROES )
              MOVE "1900-01-01" TO QES-SCHD-1STPYDATE-40
           ELSE
              MOVE ES-SCHD-1STPYDATE(40) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-SCHD-1STPYDATE-40.

           MOVE ES-SCHD-ESCROW(40) TO QES-SCHD-ESCROW-40.
           MOVE ES-SCHD-TERM(40) TO QES-SCHD-TERM-40.
           MOVE ES-SCHD-COLLECTED(40) TO QES-SCHD-COLLECTED-40.
           MOVE ES-BALANCE TO QES-BALANCE.
           MOVE ES-PREPAID(1) TO QES-PREPAID-1.
           MOVE ES-COLLECTED(1) TO QES-COLLECTED-1.
           MOVE ES-DISBURSED-TAXES(1) TO QES-DISBURSED-TAXES-1.
           MOVE ES-DISBURSED-INS(1) TO QES-DISBURSED-INS-1.
           MOVE ES-DISBURSED-PMI(1) TO QES-DISBURSED-PMI-1.
           MOVE ES-DISBURSED-OTHER(1) TO QES-DISBURSED-OTHER-1.
           MOVE ES-DISBURSED-REF(1) TO QES-DISBURSED-REF-1.
           MOVE ES-DISBURSED-CNT-TAXES(1) TO QES-DISBURSED-CNT-TAXES-1.
           MOVE ES-DISBURSED-MISC1(1) TO QES-DISBURSED-MISC1-1.
           MOVE ES-DISBURSED-MISC2(1) TO QES-DISBURSED-MISC2-1.
           MOVE ES-PREPAID(2) TO QES-PREPAID-2.
           MOVE ES-COLLECTED(2) TO QES-COLLECTED-2.
           MOVE ES-DISBURSED-TAXES(2) TO QES-DISBURSED-TAXES-2.
           MOVE ES-DISBURSED-INS(2) TO QES-DISBURSED-INS-2.
           MOVE ES-DISBURSED-PMI(2) TO QES-DISBURSED-PMI-2.
           MOVE ES-DISBURSED-OTHER(2) TO QES-DISBURSED-OTHER-2.
           MOVE ES-DISBURSED-REF(2) TO QES-DISBURSED-REF-2.
           MOVE ES-DISBURSED-CNT-TAXES(2) TO QES-DISBURSED-CNT-TAXES-2.
           MOVE ES-DISBURSED-MISC1(2) TO QES-DISBURSED-MISC1-2.
           MOVE ES-DISBURSED-MISC2(2) TO QES-DISBURSED-MISC2-2.
           MOVE ES-PREPAID(3) TO QES-PREPAID-3.
           MOVE ES-COLLECTED(3) TO QES-COLLECTED-3.
           MOVE ES-DISBURSED-TAXES(3) TO QES-DISBURSED-TAXES-3.
           MOVE ES-DISBURSED-INS(3) TO QES-DISBURSED-INS-3.
           MOVE ES-DISBURSED-PMI(3) TO QES-DISBURSED-PMI-3.
           MOVE ES-DISBURSED-OTHER(3) TO QES-DISBURSED-OTHER-3.
           MOVE ES-DISBURSED-REF(3) TO QES-DISBURSED-REF-3.
           MOVE ES-DISBURSED-CNT-TAXES(3) TO QES-DISBURSED-CNT-TAXES-3.
           MOVE ES-DISBURSED-MISC1(3) TO QES-DISBURSED-MISC1-3.
           MOVE ES-DISBURSED-MISC2(3) TO QES-DISBURSED-MISC2-3.
           MOVE ES-MISC-AMT(1) TO QES-MISC-AMT-1.

           IF ( ES-EXP-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QES-EXP-DATE-1
           ELSE
              MOVE ES-EXP-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-EXP-DATE-1.

           MOVE ES-MISC-AMT(2) TO QES-MISC-AMT-2.

           IF ( ES-EXP-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QES-EXP-DATE-2
           ELSE
              MOVE ES-EXP-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-EXP-DATE-2.

           MOVE ES-MISC-AMT(3) TO QES-MISC-AMT-3.

           IF ( ES-EXP-DATE(3) = ZEROES )
              MOVE "1900-01-01" TO QES-EXP-DATE-3
           ELSE
              MOVE ES-EXP-DATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-EXP-DATE-3.

           MOVE ES-MISC-AMT(4) TO QES-MISC-AMT-4.

           IF ( ES-EXP-DATE(4) = ZEROES )
              MOVE "1900-01-01" TO QES-EXP-DATE-4
           ELSE
              MOVE ES-EXP-DATE(4) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-EXP-DATE-4.

           MOVE ES-MISC-AMT(5) TO QES-MISC-AMT-5.

           IF ( ES-EXP-DATE(5) = ZEROES )
              MOVE "1900-01-01" TO QES-EXP-DATE-5
           ELSE
              MOVE ES-EXP-DATE(5) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-EXP-DATE-5.

           MOVE ES-MISC-AMT(6) TO QES-MISC-AMT-6.

           IF ( ES-EXP-DATE(6) = ZEROES )
              MOVE "1900-01-01" TO QES-EXP-DATE-6
           ELSE
              MOVE ES-EXP-DATE(6) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QES-EXP-DATE-6.

 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBLP/LPES1R
      * COPYMEMBER: LIBLP/LPES1RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-ES1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ES-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ES-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ES-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ES-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO ES-ALL-DATA
              MOVE ES-ALL-PATH             TO ES-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ES-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ES-ALL-BASE
              MOVE FILPATH-MACHINE         TO ES-ALL-MACHINE
              MOVE FILPATH-DATA            TO ES-ALL-DATA
              MOVE ES-ALL-PATH             TO ES-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-ESFILE NOT = "S" )
              MOVE ES-PATH-BEG             TO ES-SELECT-PATH-BEG
              MOVE ES-PATH-FILE            TO ES-SELECT-PATH-FILE
           ELSE
              MOVE ES-PATH-SQL             TO ES-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-ES1-FILE.
           PERFORM LOAD-ES1-FILE.
           PERFORM OPEN-IT.
           MOVE ES-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-ES1-FILE.
           PERFORM START-IT.
           MOVE ES-PATH-OWNBR  TO ES-BRNO.
           MOVE ES-SELECT-PATH TO E-FILE.
           MOVE ES1-KEY        TO E-KEYX.

           IF ( ES1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-ES1-FILE.

           IF ( QES1-WEND-ACCTNO = ZEROES    ) OR
              ( QES1-WEND-ACCTNO NOT NUMERIC )
              MOVE ES-PATH-OWNBR        TO QES1-WBEG-BRNO
              MOVE ES-PATH-OWNBR        TO QES1-WEND-BRNO
              MOVE ES-ACCTNO            TO QES1-WBEG-ACCTNO
              MOVE ALL "9"              TO QES1-WEND-ACCTNO.

           EXEC SQL
            DECLARE ES1_CURSOR CURSOR FOR
            SELECT ESFILE.ES_BRNO,
                   ESFILE.ES_ACCTNO,
                   ISNULL(CAST(ESFILE.ES_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_1
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_1,
                   ESFILE.ES_SCHD_TERM_1,
                   ESFILE.ES_SCHD_COLLECTED_1,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_2
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_2,
                   ESFILE.ES_SCHD_TERM_2,
                   ESFILE.ES_SCHD_COLLECTED_2,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_3
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_3,
                   ESFILE.ES_SCHD_TERM_3,
                   ESFILE.ES_SCHD_COLLECTED_3,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_4
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_4,
                   ESFILE.ES_SCHD_TERM_4,
                   ESFILE.ES_SCHD_COLLECTED_4,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_5
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_5,
                   ESFILE.ES_SCHD_TERM_5,
                   ESFILE.ES_SCHD_COLLECTED_5,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_6
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_6,
                   ESFILE.ES_SCHD_TERM_6,
                   ESFILE.ES_SCHD_COLLECTED_6,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_7
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_7,
                   ESFILE.ES_SCHD_TERM_7,
                   ESFILE.ES_SCHD_COLLECTED_7,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_8
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_8,
                   ESFILE.ES_SCHD_TERM_8,
                   ESFILE.ES_SCHD_COLLECTED_8,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_9
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_9,
                   ESFILE.ES_SCHD_TERM_9,
                   ESFILE.ES_SCHD_COLLECTED_9,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_10
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_10,
                   ESFILE.ES_SCHD_TERM_10,
                   ESFILE.ES_SCHD_COLLECTED_10,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_11
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_11
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_11,
                   ESFILE.ES_SCHD_TERM_11,
                   ESFILE.ES_SCHD_COLLECTED_11,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_12
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_12
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_12,
                   ESFILE.ES_SCHD_TERM_12,
                   ESFILE.ES_SCHD_COLLECTED_12,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_13
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_13
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_13,
                   ESFILE.ES_SCHD_TERM_13,
                   ESFILE.ES_SCHD_COLLECTED_13,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_14
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_14
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_14,
                   ESFILE.ES_SCHD_TERM_14,
                   ESFILE.ES_SCHD_COLLECTED_14,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_15
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_15
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_15,
                   ESFILE.ES_SCHD_TERM_15,
                   ESFILE.ES_SCHD_COLLECTED_15,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_16
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_16
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_16,
                   ESFILE.ES_SCHD_TERM_16,
                   ESFILE.ES_SCHD_COLLECTED_16,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_17
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_17
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_17,
                   ESFILE.ES_SCHD_TERM_17,
                   ESFILE.ES_SCHD_COLLECTED_17,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_18
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_18
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_18,
                   ESFILE.ES_SCHD_TERM_18,
                   ESFILE.ES_SCHD_COLLECTED_18,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_19
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_19
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_19,
                   ESFILE.ES_SCHD_TERM_19,
                   ESFILE.ES_SCHD_COLLECTED_19,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_20
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_20
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_20,
                   ESFILE.ES_SCHD_TERM_20,
                   ESFILE.ES_SCHD_COLLECTED_20,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_21
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_21
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_21,
                   ESFILE.ES_SCHD_TERM_21,
                   ESFILE.ES_SCHD_COLLECTED_21,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_22
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_22
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_22,
                   ESFILE.ES_SCHD_TERM_22,
                   ESFILE.ES_SCHD_COLLECTED_22,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_23
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_23
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_23,
                   ESFILE.ES_SCHD_TERM_23,
                   ESFILE.ES_SCHD_COLLECTED_23,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_24
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_24
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_24,
                   ESFILE.ES_SCHD_TERM_24,
                   ESFILE.ES_SCHD_COLLECTED_24,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_25
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_25
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_25,
                   ESFILE.ES_SCHD_TERM_25,
                   ESFILE.ES_SCHD_COLLECTED_25,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_26
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_26
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_26,
                   ESFILE.ES_SCHD_TERM_26,
                   ESFILE.ES_SCHD_COLLECTED_26,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_27
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_27
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_27,
                   ESFILE.ES_SCHD_TERM_27,
                   ESFILE.ES_SCHD_COLLECTED_27,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_28
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_28
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_28,
                   ESFILE.ES_SCHD_TERM_28,
                   ESFILE.ES_SCHD_COLLECTED_28,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_29
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_29
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_29,
                   ESFILE.ES_SCHD_TERM_29,
                   ESFILE.ES_SCHD_COLLECTED_29,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_30
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_30
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_30,
                   ESFILE.ES_SCHD_TERM_30,
                   ESFILE.ES_SCHD_COLLECTED_30,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_31
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_31
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_31,
                   ESFILE.ES_SCHD_TERM_31,
                   ESFILE.ES_SCHD_COLLECTED_31,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_32
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_32
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_32,
                   ESFILE.ES_SCHD_TERM_32,
                   ESFILE.ES_SCHD_COLLECTED_32,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_33
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_33
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_33,
                   ESFILE.ES_SCHD_TERM_33,
                   ESFILE.ES_SCHD_COLLECTED_33,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_34
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_34
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_34,
                   ESFILE.ES_SCHD_TERM_34,
                   ESFILE.ES_SCHD_COLLECTED_34,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_35
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_35
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_35,
                   ESFILE.ES_SCHD_TERM_35,
                   ESFILE.ES_SCHD_COLLECTED_35,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_36
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_36
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_36,
                   ESFILE.ES_SCHD_TERM_36,
                   ESFILE.ES_SCHD_COLLECTED_36,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_37
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_37
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_37,
                   ESFILE.ES_SCHD_TERM_37,
                   ESFILE.ES_SCHD_COLLECTED_37,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_38
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_38
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_38,
                   ESFILE.ES_SCHD_TERM_38,
                   ESFILE.ES_SCHD_COLLECTED_38,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_39
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_39
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_39,
                   ESFILE.ES_SCHD_TERM_39,
                   ESFILE.ES_SCHD_COLLECTED_39,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_40
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_40
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_40,
                   ESFILE.ES_SCHD_TERM_40,
                   ESFILE.ES_SCHD_COLLECTED_40,
                   ESFILE.ES_BALANCE,
                   ESFILE.ES_PREPAID_1,
                   ESFILE.ES_COLLECTED_1,
                   ESFILE.ES_DISBURSED_TAXES_1,
                   ESFILE.ES_DISBURSED_INS_1,
                   ESFILE.ES_DISBURSED_PMI_1,
                   ESFILE.ES_DISBURSED_OTHER_1,
                   ESFILE.ES_DISBURSED_REF_1,
                   ESFILE.ES_DISBURSED_CNT_TAXES_1,
                   ESFILE.ES_DISBURSED_MISC1_1,
                   ESFILE.ES_DISBURSED_MISC2_1,
                   ESFILE.ES_PREPAID_2,
                   ESFILE.ES_COLLECTED_2,
                   ESFILE.ES_DISBURSED_TAXES_2,
                   ESFILE.ES_DISBURSED_INS_2,
                   ESFILE.ES_DISBURSED_PMI_2,
                   ESFILE.ES_DISBURSED_OTHER_2,
                   ESFILE.ES_DISBURSED_REF_2,
                   ESFILE.ES_DISBURSED_CNT_TAXES_2,
                   ESFILE.ES_DISBURSED_MISC1_2,
                   ESFILE.ES_DISBURSED_MISC2_2,
                   ESFILE.ES_PREPAID_3,
                   ESFILE.ES_COLLECTED_3,
                   ESFILE.ES_DISBURSED_TAXES_3,
                   ESFILE.ES_DISBURSED_INS_3,
                   ESFILE.ES_DISBURSED_PMI_3,
                   ESFILE.ES_DISBURSED_OTHER_3,
                   ESFILE.ES_DISBURSED_REF_3,
                   ESFILE.ES_DISBURSED_CNT_TAXES_3,
                   ESFILE.ES_DISBURSED_MISC1_3,
                   ESFILE.ES_DISBURSED_MISC2_3,
                   ESFILE.ES_MISC_AMT_1,
                   ISNULL(CAST(ESFILE.ES_EXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_MISC_AMT_2,
                   ISNULL(CAST(ESFILE.ES_EXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_MISC_AMT_3,
                   ISNULL(CAST(ESFILE.ES_EXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_MISC_AMT_4,
                   ISNULL(CAST(ESFILE.ES_EXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_MISC_AMT_5,
                   ISNULL(CAST(ESFILE.ES_EXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_MISC_AMT_6,
                   ISNULL(CAST(ESFILE.ES_EXP_DATE_6
                                                  AS VARCHAR(10)),'0')
            FROM DBO.ESFILE
            WHERE ESFILE.ES_BRNO >= :QES1-WBEG-BRNO
              AND ESFILE.ES_BRNO <= :QES1-WEND-BRNO
              AND ESFILE.ES_ACCTNO >= :QES1-WBEG-ACCTNO
              AND ESFILE.ES_ACCTNO <= :QES1-WEND-ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN ES1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QES1-WBEG-BRNO 
                          QES1-WEND-BRNO
                          QES1-WBEG-ACCTNO
                          QES1-WEND-ACCTNO.

           MOVE STAT---GOOD TO ES1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-ES1-FILE.
           PERFORM READ-IT.
           MOVE ES-PATH-OWNBR  TO ES-BRNO. 
           MOVE ES-SELECT-PATH TO E-FILE.
           MOVE ES1-KEY        TO E-KEYX.

           MOVE ES-BRNO   TO QES-BRNO.
           MOVE ES-ACCTNO TO QES-ACCTNO.

           EXEC SQL
            SELECT ESFILE.ES_BRNO,
                   ESFILE.ES_ACCTNO,
                   ISNULL(CAST(ESFILE.ES_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_1
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_1,
                   ESFILE.ES_SCHD_TERM_1,
                   ESFILE.ES_SCHD_COLLECTED_1,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_2
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_2,
                   ESFILE.ES_SCHD_TERM_2,
                   ESFILE.ES_SCHD_COLLECTED_2,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_3
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_3,
                   ESFILE.ES_SCHD_TERM_3,
                   ESFILE.ES_SCHD_COLLECTED_3,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_4
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_4,
                   ESFILE.ES_SCHD_TERM_4,
                   ESFILE.ES_SCHD_COLLECTED_4,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_5
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_5,
                   ESFILE.ES_SCHD_TERM_5,
                   ESFILE.ES_SCHD_COLLECTED_5,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_6
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_6,
                   ESFILE.ES_SCHD_TERM_6,
                   ESFILE.ES_SCHD_COLLECTED_6,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_7
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_7,
                   ESFILE.ES_SCHD_TERM_7,
                   ESFILE.ES_SCHD_COLLECTED_7,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_8
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_8,
                   ESFILE.ES_SCHD_TERM_8,
                   ESFILE.ES_SCHD_COLLECTED_8,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_9
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_9,
                   ESFILE.ES_SCHD_TERM_9,
                   ESFILE.ES_SCHD_COLLECTED_9,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_10
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_10,
                   ESFILE.ES_SCHD_TERM_10,
                   ESFILE.ES_SCHD_COLLECTED_10,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_11
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_11
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_11,
                   ESFILE.ES_SCHD_TERM_11,
                   ESFILE.ES_SCHD_COLLECTED_11,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_12
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_12
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_12,
                   ESFILE.ES_SCHD_TERM_12,
                   ESFILE.ES_SCHD_COLLECTED_12,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_13
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_13
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_13,
                   ESFILE.ES_SCHD_TERM_13,
                   ESFILE.ES_SCHD_COLLECTED_13,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_14
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_14
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_14,
                   ESFILE.ES_SCHD_TERM_14,
                   ESFILE.ES_SCHD_COLLECTED_14,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_15
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_15
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_15,
                   ESFILE.ES_SCHD_TERM_15,
                   ESFILE.ES_SCHD_COLLECTED_15,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_16
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_16
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_16,
                   ESFILE.ES_SCHD_TERM_16,
                   ESFILE.ES_SCHD_COLLECTED_16,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_17
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_17
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_17,
                   ESFILE.ES_SCHD_TERM_17,
                   ESFILE.ES_SCHD_COLLECTED_17,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_18
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_18
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_18,
                   ESFILE.ES_SCHD_TERM_18,
                   ESFILE.ES_SCHD_COLLECTED_18,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_19
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_19
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_19,
                   ESFILE.ES_SCHD_TERM_19,
                   ESFILE.ES_SCHD_COLLECTED_19,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_20
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_20
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_20,
                   ESFILE.ES_SCHD_TERM_20,
                   ESFILE.ES_SCHD_COLLECTED_20,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_21
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_21
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_21,
                   ESFILE.ES_SCHD_TERM_21,
                   ESFILE.ES_SCHD_COLLECTED_21,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_22
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_22
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_22,
                   ESFILE.ES_SCHD_TERM_22,
                   ESFILE.ES_SCHD_COLLECTED_22,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_23
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_23
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_23,
                   ESFILE.ES_SCHD_TERM_23,
                   ESFILE.ES_SCHD_COLLECTED_23,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_24
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_24
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_24,
                   ESFILE.ES_SCHD_TERM_24,
                   ESFILE.ES_SCHD_COLLECTED_24,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_25
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_25
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_25,
                   ESFILE.ES_SCHD_TERM_25,
                   ESFILE.ES_SCHD_COLLECTED_25,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_26
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_26
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_26,
                   ESFILE.ES_SCHD_TERM_26,
                   ESFILE.ES_SCHD_COLLECTED_26,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_27
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_27
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_27,
                   ESFILE.ES_SCHD_TERM_27,
                   ESFILE.ES_SCHD_COLLECTED_27,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_28
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_28
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_28,
                   ESFILE.ES_SCHD_TERM_28,
                   ESFILE.ES_SCHD_COLLECTED_28,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_29
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_29
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_29,
                   ESFILE.ES_SCHD_TERM_29,
                   ESFILE.ES_SCHD_COLLECTED_29,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_30
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_30
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_30,
                   ESFILE.ES_SCHD_TERM_30,
                   ESFILE.ES_SCHD_COLLECTED_30,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_31
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_31
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_31,
                   ESFILE.ES_SCHD_TERM_31,
                   ESFILE.ES_SCHD_COLLECTED_31,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_32
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_32
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_32,
                   ESFILE.ES_SCHD_TERM_32,
                   ESFILE.ES_SCHD_COLLECTED_32,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_33
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_33
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_33,
                   ESFILE.ES_SCHD_TERM_33,
                   ESFILE.ES_SCHD_COLLECTED_33,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_34
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_34
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_34,
                   ESFILE.ES_SCHD_TERM_34,
                   ESFILE.ES_SCHD_COLLECTED_34,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_35
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_35
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_35,
                   ESFILE.ES_SCHD_TERM_35,
                   ESFILE.ES_SCHD_COLLECTED_35,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_36
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_36
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_36,
                   ESFILE.ES_SCHD_TERM_36,
                   ESFILE.ES_SCHD_COLLECTED_36,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_37
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_37
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_37,
                   ESFILE.ES_SCHD_TERM_37,
                   ESFILE.ES_SCHD_COLLECTED_37,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_38
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_38
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_38,
                   ESFILE.ES_SCHD_TERM_38,
                   ESFILE.ES_SCHD_COLLECTED_38,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_39
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_39
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_39,
                   ESFILE.ES_SCHD_TERM_39,
                   ESFILE.ES_SCHD_COLLECTED_39,
                   ISNULL(CAST(ESFILE.ES_SCHD_EFF_DATE_40
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(ESFILE.ES_SCHD_1STPYDATE_40
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_SCHD_ESCROW_40,
                   ESFILE.ES_SCHD_TERM_40,
                   ESFILE.ES_SCHD_COLLECTED_40,
                   ESFILE.ES_BALANCE,
                   ESFILE.ES_PREPAID_1,
                   ESFILE.ES_COLLECTED_1,
                   ESFILE.ES_DISBURSED_TAXES_1,
                   ESFILE.ES_DISBURSED_INS_1,
                   ESFILE.ES_DISBURSED_PMI_1,
                   ESFILE.ES_DISBURSED_OTHER_1,
                   ESFILE.ES_DISBURSED_REF_1,
                   ESFILE.ES_DISBURSED_CNT_TAXES_1,
                   ESFILE.ES_DISBURSED_MISC1_1,
                   ESFILE.ES_DISBURSED_MISC2_1,
                   ESFILE.ES_PREPAID_2,
                   ESFILE.ES_COLLECTED_2,
                   ESFILE.ES_DISBURSED_TAXES_2,
                   ESFILE.ES_DISBURSED_INS_2,
                   ESFILE.ES_DISBURSED_PMI_2,
                   ESFILE.ES_DISBURSED_OTHER_2,
                   ESFILE.ES_DISBURSED_REF_2,
                   ESFILE.ES_DISBURSED_CNT_TAXES_2,
                   ESFILE.ES_DISBURSED_MISC1_2,
                   ESFILE.ES_DISBURSED_MISC2_2,
                   ESFILE.ES_PREPAID_3,
                   ESFILE.ES_COLLECTED_3,
                   ESFILE.ES_DISBURSED_TAXES_3,
                   ESFILE.ES_DISBURSED_INS_3,
                   ESFILE.ES_DISBURSED_PMI_3,
                   ESFILE.ES_DISBURSED_OTHER_3,
                   ESFILE.ES_DISBURSED_REF_3,
                   ESFILE.ES_DISBURSED_CNT_TAXES_3,
                   ESFILE.ES_DISBURSED_MISC1_3,
                   ESFILE.ES_DISBURSED_MISC2_3,
                   ESFILE.ES_MISC_AMT_1,
                   ISNULL(CAST(ESFILE.ES_EXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_MISC_AMT_2,
                   ISNULL(CAST(ESFILE.ES_EXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_MISC_AMT_3,
                   ISNULL(CAST(ESFILE.ES_EXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_MISC_AMT_4,
                   ISNULL(CAST(ESFILE.ES_EXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_MISC_AMT_5,
                   ISNULL(CAST(ESFILE.ES_EXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ESFILE.ES_MISC_AMT_6,
                   ISNULL(CAST(ESFILE.ES_EXP_DATE_6
                                                  AS VARCHAR(10)),'0')
            INTO :QES-REC
            FROM DBO.ESFILE
            WHERE ESFILE.ES_BRNO = :QES-BRNO
              AND ESFILE.ES_ACCTNO = :QES-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ES1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-ES-FIELDS.

      *-----------------------------------------------------------------
       READ-ES1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ES-PATH-OWNBR  TO ES-BRNO.
           MOVE ES-SELECT-PATH TO E-FILE.
           MOVE ES1-KEY        TO E-KEYX.
           INITIALIZE QES-REC.

           IF ( ES1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH ES1_CURSOR INTO :QES-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ES1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-ES-FIELDS.

      *-----------------------------------------------------------------
       WRITE-ES1-FILE.

           MOVE ES-PATH-OWNBR  TO ES-BRNO.
           ACCEPT ES-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE ES-SELECT-PATH TO E-FILE.
           MOVE ES1-KEY        TO E-KEYX.
           PERFORM SET-ES-FIELDS.

           EXEC SQL
            INSERT INTO DBO.ESFILE
             (ESFILE.ES_BRNO,
              ESFILE.ES_ACCTNO,
              ESFILE.ES_LTOUCH_DATE,
              ESFILE.ES_SCHD_EFF_DATE_1,
              ESFILE.ES_SCHD_1STPYDATE_1,
              ESFILE.ES_SCHD_ESCROW_1,
              ESFILE.ES_SCHD_TERM_1,
              ESFILE.ES_SCHD_COLLECTED_1,
              ESFILE.ES_SCHD_EFF_DATE_2,
              ESFILE.ES_SCHD_1STPYDATE_2,
              ESFILE.ES_SCHD_ESCROW_2,
              ESFILE.ES_SCHD_TERM_2,
              ESFILE.ES_SCHD_COLLECTED_2,
              ESFILE.ES_SCHD_EFF_DATE_3,
              ESFILE.ES_SCHD_1STPYDATE_3,
              ESFILE.ES_SCHD_ESCROW_3,
              ESFILE.ES_SCHD_TERM_3,
              ESFILE.ES_SCHD_COLLECTED_3,
              ESFILE.ES_SCHD_EFF_DATE_4,
              ESFILE.ES_SCHD_1STPYDATE_4,
              ESFILE.ES_SCHD_ESCROW_4,
              ESFILE.ES_SCHD_TERM_4,
              ESFILE.ES_SCHD_COLLECTED_4,
              ESFILE.ES_SCHD_EFF_DATE_5,
              ESFILE.ES_SCHD_1STPYDATE_5,
              ESFILE.ES_SCHD_ESCROW_5,
              ESFILE.ES_SCHD_TERM_5,
              ESFILE.ES_SCHD_COLLECTED_5,
              ESFILE.ES_SCHD_EFF_DATE_6,
              ESFILE.ES_SCHD_1STPYDATE_6,
              ESFILE.ES_SCHD_ESCROW_6,
              ESFILE.ES_SCHD_TERM_6,
              ESFILE.ES_SCHD_COLLECTED_6,
              ESFILE.ES_SCHD_EFF_DATE_7,
              ESFILE.ES_SCHD_1STPYDATE_7,
              ESFILE.ES_SCHD_ESCROW_7,
              ESFILE.ES_SCHD_TERM_7,
              ESFILE.ES_SCHD_COLLECTED_7,
              ESFILE.ES_SCHD_EFF_DATE_8,
              ESFILE.ES_SCHD_1STPYDATE_8,
              ESFILE.ES_SCHD_ESCROW_8,
              ESFILE.ES_SCHD_TERM_8,
              ESFILE.ES_SCHD_COLLECTED_8,
              ESFILE.ES_SCHD_EFF_DATE_9,
              ESFILE.ES_SCHD_1STPYDATE_9,
              ESFILE.ES_SCHD_ESCROW_9,
              ESFILE.ES_SCHD_TERM_9,
              ESFILE.ES_SCHD_COLLECTED_9,
              ESFILE.ES_SCHD_EFF_DATE_10,
              ESFILE.ES_SCHD_1STPYDATE_10,
              ESFILE.ES_SCHD_ESCROW_10,
              ESFILE.ES_SCHD_TERM_10,
              ESFILE.ES_SCHD_COLLECTED_10,
              ESFILE.ES_SCHD_EFF_DATE_11,
              ESFILE.ES_SCHD_1STPYDATE_11,
              ESFILE.ES_SCHD_ESCROW_11,
              ESFILE.ES_SCHD_TERM_11,
              ESFILE.ES_SCHD_COLLECTED_11,
              ESFILE.ES_SCHD_EFF_DATE_12,
              ESFILE.ES_SCHD_1STPYDATE_12,
              ESFILE.ES_SCHD_ESCROW_12,
              ESFILE.ES_SCHD_TERM_12,
              ESFILE.ES_SCHD_COLLECTED_12,
              ESFILE.ES_SCHD_EFF_DATE_13,
              ESFILE.ES_SCHD_1STPYDATE_13,
              ESFILE.ES_SCHD_ESCROW_13,
              ESFILE.ES_SCHD_TERM_13,
              ESFILE.ES_SCHD_COLLECTED_13,
              ESFILE.ES_SCHD_EFF_DATE_14,
              ESFILE.ES_SCHD_1STPYDATE_14,
              ESFILE.ES_SCHD_ESCROW_14,
              ESFILE.ES_SCHD_TERM_14,
              ESFILE.ES_SCHD_COLLECTED_14,
              ESFILE.ES_SCHD_EFF_DATE_15,
              ESFILE.ES_SCHD_1STPYDATE_15,
              ESFILE.ES_SCHD_ESCROW_15,
              ESFILE.ES_SCHD_TERM_15,
              ESFILE.ES_SCHD_COLLECTED_15,
              ESFILE.ES_SCHD_EFF_DATE_16,
              ESFILE.ES_SCHD_1STPYDATE_16,
              ESFILE.ES_SCHD_ESCROW_16,
              ESFILE.ES_SCHD_TERM_16,
              ESFILE.ES_SCHD_COLLECTED_16,
              ESFILE.ES_SCHD_EFF_DATE_17,
              ESFILE.ES_SCHD_1STPYDATE_17,
              ESFILE.ES_SCHD_ESCROW_17,
              ESFILE.ES_SCHD_TERM_17,
              ESFILE.ES_SCHD_COLLECTED_17,
              ESFILE.ES_SCHD_EFF_DATE_18,
              ESFILE.ES_SCHD_1STPYDATE_18,
              ESFILE.ES_SCHD_ESCROW_18,
              ESFILE.ES_SCHD_TERM_18,
              ESFILE.ES_SCHD_COLLECTED_18,
              ESFILE.ES_SCHD_EFF_DATE_19,
              ESFILE.ES_SCHD_1STPYDATE_19,
              ESFILE.ES_SCHD_ESCROW_19,
              ESFILE.ES_SCHD_TERM_19,
              ESFILE.ES_SCHD_COLLECTED_19,
              ESFILE.ES_SCHD_EFF_DATE_20,
              ESFILE.ES_SCHD_1STPYDATE_20,
              ESFILE.ES_SCHD_ESCROW_20,
              ESFILE.ES_SCHD_TERM_20,
              ESFILE.ES_SCHD_COLLECTED_20,
              ESFILE.ES_SCHD_EFF_DATE_21,
              ESFILE.ES_SCHD_1STPYDATE_21,
              ESFILE.ES_SCHD_ESCROW_21,
              ESFILE.ES_SCHD_TERM_21,
              ESFILE.ES_SCHD_COLLECTED_21,
              ESFILE.ES_SCHD_EFF_DATE_22,
              ESFILE.ES_SCHD_1STPYDATE_22,
              ESFILE.ES_SCHD_ESCROW_22,
              ESFILE.ES_SCHD_TERM_22,
              ESFILE.ES_SCHD_COLLECTED_22,
              ESFILE.ES_SCHD_EFF_DATE_23,
              ESFILE.ES_SCHD_1STPYDATE_23,
              ESFILE.ES_SCHD_ESCROW_23,
              ESFILE.ES_SCHD_TERM_23,
              ESFILE.ES_SCHD_COLLECTED_23,
              ESFILE.ES_SCHD_EFF_DATE_24,
              ESFILE.ES_SCHD_1STPYDATE_24,
              ESFILE.ES_SCHD_ESCROW_24,
              ESFILE.ES_SCHD_TERM_24,
              ESFILE.ES_SCHD_COLLECTED_24,
              ESFILE.ES_SCHD_EFF_DATE_25,
              ESFILE.ES_SCHD_1STPYDATE_25,
              ESFILE.ES_SCHD_ESCROW_25,
              ESFILE.ES_SCHD_TERM_25,
              ESFILE.ES_SCHD_COLLECTED_25,
              ESFILE.ES_SCHD_EFF_DATE_26,
              ESFILE.ES_SCHD_1STPYDATE_26,
              ESFILE.ES_SCHD_ESCROW_26,
              ESFILE.ES_SCHD_TERM_26,
              ESFILE.ES_SCHD_COLLECTED_26,
              ESFILE.ES_SCHD_EFF_DATE_27,
              ESFILE.ES_SCHD_1STPYDATE_27,
              ESFILE.ES_SCHD_ESCROW_27,
              ESFILE.ES_SCHD_TERM_27,
              ESFILE.ES_SCHD_COLLECTED_27,
              ESFILE.ES_SCHD_EFF_DATE_28,
              ESFILE.ES_SCHD_1STPYDATE_28,
              ESFILE.ES_SCHD_ESCROW_28,
              ESFILE.ES_SCHD_TERM_28,
              ESFILE.ES_SCHD_COLLECTED_28,
              ESFILE.ES_SCHD_EFF_DATE_29,
              ESFILE.ES_SCHD_1STPYDATE_29,
              ESFILE.ES_SCHD_ESCROW_29,
              ESFILE.ES_SCHD_TERM_29,
              ESFILE.ES_SCHD_COLLECTED_29,
              ESFILE.ES_SCHD_EFF_DATE_30,
              ESFILE.ES_SCHD_1STPYDATE_30,
              ESFILE.ES_SCHD_ESCROW_30,
              ESFILE.ES_SCHD_TERM_30,
              ESFILE.ES_SCHD_COLLECTED_30,
              ESFILE.ES_SCHD_EFF_DATE_31,
              ESFILE.ES_SCHD_1STPYDATE_31,
              ESFILE.ES_SCHD_ESCROW_31,
              ESFILE.ES_SCHD_TERM_31,
              ESFILE.ES_SCHD_COLLECTED_31,
              ESFILE.ES_SCHD_EFF_DATE_32,
              ESFILE.ES_SCHD_1STPYDATE_32,
              ESFILE.ES_SCHD_ESCROW_32,
              ESFILE.ES_SCHD_TERM_32,
              ESFILE.ES_SCHD_COLLECTED_32,
              ESFILE.ES_SCHD_EFF_DATE_33,
              ESFILE.ES_SCHD_1STPYDATE_33,
              ESFILE.ES_SCHD_ESCROW_33,
              ESFILE.ES_SCHD_TERM_33,
              ESFILE.ES_SCHD_COLLECTED_33,
              ESFILE.ES_SCHD_EFF_DATE_34,
              ESFILE.ES_SCHD_1STPYDATE_34,
              ESFILE.ES_SCHD_ESCROW_34,
              ESFILE.ES_SCHD_TERM_34,
              ESFILE.ES_SCHD_COLLECTED_34,
              ESFILE.ES_SCHD_EFF_DATE_35,
              ESFILE.ES_SCHD_1STPYDATE_35,
              ESFILE.ES_SCHD_ESCROW_35,
              ESFILE.ES_SCHD_TERM_35,
              ESFILE.ES_SCHD_COLLECTED_35,
              ESFILE.ES_SCHD_EFF_DATE_36,
              ESFILE.ES_SCHD_1STPYDATE_36,
              ESFILE.ES_SCHD_ESCROW_36,
              ESFILE.ES_SCHD_TERM_36,
              ESFILE.ES_SCHD_COLLECTED_36,
              ESFILE.ES_SCHD_EFF_DATE_37,
              ESFILE.ES_SCHD_1STPYDATE_37,
              ESFILE.ES_SCHD_ESCROW_37,
              ESFILE.ES_SCHD_TERM_37,
              ESFILE.ES_SCHD_COLLECTED_37,
              ESFILE.ES_SCHD_EFF_DATE_38,
              ESFILE.ES_SCHD_1STPYDATE_38,
              ESFILE.ES_SCHD_ESCROW_38,
              ESFILE.ES_SCHD_TERM_38,
              ESFILE.ES_SCHD_COLLECTED_38,
              ESFILE.ES_SCHD_EFF_DATE_39,
              ESFILE.ES_SCHD_1STPYDATE_39,
              ESFILE.ES_SCHD_ESCROW_39,
              ESFILE.ES_SCHD_TERM_39,
              ESFILE.ES_SCHD_COLLECTED_39,
              ESFILE.ES_SCHD_EFF_DATE_40,
              ESFILE.ES_SCHD_1STPYDATE_40,
              ESFILE.ES_SCHD_ESCROW_40,
              ESFILE.ES_SCHD_TERM_40,
              ESFILE.ES_SCHD_COLLECTED_40,
              ESFILE.ES_BALANCE,
              ESFILE.ES_PREPAID_1,
              ESFILE.ES_COLLECTED_1,
              ESFILE.ES_DISBURSED_TAXES_1,
              ESFILE.ES_DISBURSED_INS_1,
              ESFILE.ES_DISBURSED_PMI_1,
              ESFILE.ES_DISBURSED_OTHER_1,
              ESFILE.ES_DISBURSED_REF_1,
              ESFILE.ES_DISBURSED_CNT_TAXES_1,
              ESFILE.ES_DISBURSED_MISC1_1,
              ESFILE.ES_DISBURSED_MISC2_1,
              ESFILE.ES_PREPAID_2,
              ESFILE.ES_COLLECTED_2,
              ESFILE.ES_DISBURSED_TAXES_2,
              ESFILE.ES_DISBURSED_INS_2,
              ESFILE.ES_DISBURSED_PMI_2,
              ESFILE.ES_DISBURSED_OTHER_2,
              ESFILE.ES_DISBURSED_REF_2,
              ESFILE.ES_DISBURSED_CNT_TAXES_2,
              ESFILE.ES_DISBURSED_MISC1_2,
              ESFILE.ES_DISBURSED_MISC2_2,
              ESFILE.ES_PREPAID_3,
              ESFILE.ES_COLLECTED_3,
              ESFILE.ES_DISBURSED_TAXES_3,
              ESFILE.ES_DISBURSED_INS_3,
              ESFILE.ES_DISBURSED_PMI_3,
              ESFILE.ES_DISBURSED_OTHER_3,
              ESFILE.ES_DISBURSED_REF_3,
              ESFILE.ES_DISBURSED_CNT_TAXES_3,
              ESFILE.ES_DISBURSED_MISC1_3,
              ESFILE.ES_DISBURSED_MISC2_3,
              ESFILE.ES_MISC_AMT_1,
              ESFILE.ES_EXP_DATE_1,
              ESFILE.ES_MISC_AMT_2,
              ESFILE.ES_EXP_DATE_2,
              ESFILE.ES_MISC_AMT_3,
              ESFILE.ES_EXP_DATE_3,
              ESFILE.ES_MISC_AMT_4,
              ESFILE.ES_EXP_DATE_4,
              ESFILE.ES_MISC_AMT_5,
              ESFILE.ES_EXP_DATE_5,
              ESFILE.ES_MISC_AMT_6,
              ESFILE.ES_EXP_DATE_6)
            VALUES
             (:QES-BRNO,
              :QES-ACCTNO,
              NULLIF(:QES-LTOUCH-DATE,'1900-01-01'),
              NULLIF(:QES-SCHD-EFF-DATE-1,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-1,'1900-01-01'),
              :QES-SCHD-ESCROW-1,
              :QES-SCHD-TERM-1,
              :QES-SCHD-COLLECTED-1,
              NULLIF(:QES-SCHD-EFF-DATE-2,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-2,'1900-01-01'),
              :QES-SCHD-ESCROW-2,
              :QES-SCHD-TERM-2,
              :QES-SCHD-COLLECTED-2,
              NULLIF(:QES-SCHD-EFF-DATE-3,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-3,'1900-01-01'),
              :QES-SCHD-ESCROW-3,
              :QES-SCHD-TERM-3,
              :QES-SCHD-COLLECTED-3,
              NULLIF(:QES-SCHD-EFF-DATE-4,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-4,'1900-01-01'),
              :QES-SCHD-ESCROW-4,
              :QES-SCHD-TERM-4,
              :QES-SCHD-COLLECTED-4,
              NULLIF(:QES-SCHD-EFF-DATE-5,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-5,'1900-01-01'),
              :QES-SCHD-ESCROW-5,
              :QES-SCHD-TERM-5,
              :QES-SCHD-COLLECTED-5,
              NULLIF(:QES-SCHD-EFF-DATE-6,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-6,'1900-01-01'),
              :QES-SCHD-ESCROW-6,
              :QES-SCHD-TERM-6,
              :QES-SCHD-COLLECTED-6,
              NULLIF(:QES-SCHD-EFF-DATE-7,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-7,'1900-01-01'),
              :QES-SCHD-ESCROW-7,
              :QES-SCHD-TERM-7,
              :QES-SCHD-COLLECTED-7,
              NULLIF(:QES-SCHD-EFF-DATE-8,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-8,'1900-01-01'),
              :QES-SCHD-ESCROW-8,
              :QES-SCHD-TERM-8,
              :QES-SCHD-COLLECTED-8,
              NULLIF(:QES-SCHD-EFF-DATE-9,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-9,'1900-01-01'),
              :QES-SCHD-ESCROW-9,
              :QES-SCHD-TERM-9,
              :QES-SCHD-COLLECTED-9,
              NULLIF(:QES-SCHD-EFF-DATE-10,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-10,'1900-01-01'),
              :QES-SCHD-ESCROW-10,
              :QES-SCHD-TERM-10,
              :QES-SCHD-COLLECTED-10,
              NULLIF(:QES-SCHD-EFF-DATE-11,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-11,'1900-01-01'),
              :QES-SCHD-ESCROW-11,
              :QES-SCHD-TERM-11,
              :QES-SCHD-COLLECTED-11,
              NULLIF(:QES-SCHD-EFF-DATE-12,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-12,'1900-01-01'),
              :QES-SCHD-ESCROW-12,
              :QES-SCHD-TERM-12,
              :QES-SCHD-COLLECTED-12,
              NULLIF(:QES-SCHD-EFF-DATE-13,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-13,'1900-01-01'),
              :QES-SCHD-ESCROW-13,
              :QES-SCHD-TERM-13,
              :QES-SCHD-COLLECTED-13,
              NULLIF(:QES-SCHD-EFF-DATE-14,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-14,'1900-01-01'),
              :QES-SCHD-ESCROW-14,
              :QES-SCHD-TERM-14,
              :QES-SCHD-COLLECTED-14,
              NULLIF(:QES-SCHD-EFF-DATE-15,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-15,'1900-01-01'),
              :QES-SCHD-ESCROW-15,
              :QES-SCHD-TERM-15,
              :QES-SCHD-COLLECTED-15,
              NULLIF(:QES-SCHD-EFF-DATE-16,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-16,'1900-01-01'),
              :QES-SCHD-ESCROW-16,
              :QES-SCHD-TERM-16,
              :QES-SCHD-COLLECTED-16,
              NULLIF(:QES-SCHD-EFF-DATE-17,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-17,'1900-01-01'),
              :QES-SCHD-ESCROW-17,
              :QES-SCHD-TERM-17,
              :QES-SCHD-COLLECTED-17,
              NULLIF(:QES-SCHD-EFF-DATE-18,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-18,'1900-01-01'),
              :QES-SCHD-ESCROW-18,
              :QES-SCHD-TERM-18,
              :QES-SCHD-COLLECTED-18,
              NULLIF(:QES-SCHD-EFF-DATE-19,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-19,'1900-01-01'),
              :QES-SCHD-ESCROW-19,
              :QES-SCHD-TERM-19,
              :QES-SCHD-COLLECTED-19,
              NULLIF(:QES-SCHD-EFF-DATE-20,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-20,'1900-01-01'),
              :QES-SCHD-ESCROW-20,
              :QES-SCHD-TERM-20,
              :QES-SCHD-COLLECTED-20,
              NULLIF(:QES-SCHD-EFF-DATE-21,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-21,'1900-01-01'),
              :QES-SCHD-ESCROW-21,
              :QES-SCHD-TERM-21,
              :QES-SCHD-COLLECTED-21,
              NULLIF(:QES-SCHD-EFF-DATE-22,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-22,'1900-01-01'),
              :QES-SCHD-ESCROW-22,
              :QES-SCHD-TERM-22,
              :QES-SCHD-COLLECTED-22,
              NULLIF(:QES-SCHD-EFF-DATE-23,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-23,'1900-01-01'),
              :QES-SCHD-ESCROW-23,
              :QES-SCHD-TERM-23,
              :QES-SCHD-COLLECTED-23,
              NULLIF(:QES-SCHD-EFF-DATE-24,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-24,'1900-01-01'),
              :QES-SCHD-ESCROW-24,
              :QES-SCHD-TERM-24,
              :QES-SCHD-COLLECTED-24,
              NULLIF(:QES-SCHD-EFF-DATE-25,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-25,'1900-01-01'),
              :QES-SCHD-ESCROW-25,
              :QES-SCHD-TERM-25,
              :QES-SCHD-COLLECTED-25,
              NULLIF(:QES-SCHD-EFF-DATE-26,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-26,'1900-01-01'),
              :QES-SCHD-ESCROW-26,
              :QES-SCHD-TERM-26,
              :QES-SCHD-COLLECTED-26,
              NULLIF(:QES-SCHD-EFF-DATE-27,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-27,'1900-01-01'),
              :QES-SCHD-ESCROW-27,
              :QES-SCHD-TERM-27,
              :QES-SCHD-COLLECTED-27,
              NULLIF(:QES-SCHD-EFF-DATE-28,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-28,'1900-01-01'),
              :QES-SCHD-ESCROW-28,
              :QES-SCHD-TERM-28,
              :QES-SCHD-COLLECTED-28,
              NULLIF(:QES-SCHD-EFF-DATE-29,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-29,'1900-01-01'),
              :QES-SCHD-ESCROW-29,
              :QES-SCHD-TERM-29,
              :QES-SCHD-COLLECTED-29,
              NULLIF(:QES-SCHD-EFF-DATE-30,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-30,'1900-01-01'),
              :QES-SCHD-ESCROW-30,
              :QES-SCHD-TERM-30,
              :QES-SCHD-COLLECTED-30,
              NULLIF(:QES-SCHD-EFF-DATE-31,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-31,'1900-01-01'),
              :QES-SCHD-ESCROW-31,
              :QES-SCHD-TERM-31,
              :QES-SCHD-COLLECTED-31,
              NULLIF(:QES-SCHD-EFF-DATE-32,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-32,'1900-01-01'),
              :QES-SCHD-ESCROW-32,
              :QES-SCHD-TERM-32,
              :QES-SCHD-COLLECTED-32,
              NULLIF(:QES-SCHD-EFF-DATE-33,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-33,'1900-01-01'),
              :QES-SCHD-ESCROW-33,
              :QES-SCHD-TERM-33,
              :QES-SCHD-COLLECTED-33,
              NULLIF(:QES-SCHD-EFF-DATE-34,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-34,'1900-01-01'),
              :QES-SCHD-ESCROW-34,
              :QES-SCHD-TERM-34,
              :QES-SCHD-COLLECTED-34,
              NULLIF(:QES-SCHD-EFF-DATE-35,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-35,'1900-01-01'),
              :QES-SCHD-ESCROW-35,
              :QES-SCHD-TERM-35,
              :QES-SCHD-COLLECTED-35,
              NULLIF(:QES-SCHD-EFF-DATE-36,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-36,'1900-01-01'),
              :QES-SCHD-ESCROW-36,
              :QES-SCHD-TERM-36,
              :QES-SCHD-COLLECTED-36,
              NULLIF(:QES-SCHD-EFF-DATE-37,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-37,'1900-01-01'),
              :QES-SCHD-ESCROW-37,
              :QES-SCHD-TERM-37,
              :QES-SCHD-COLLECTED-37,
              NULLIF(:QES-SCHD-EFF-DATE-38,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-38,'1900-01-01'),
              :QES-SCHD-ESCROW-38,
              :QES-SCHD-TERM-38,
              :QES-SCHD-COLLECTED-38,
              NULLIF(:QES-SCHD-EFF-DATE-39,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-39,'1900-01-01'),
              :QES-SCHD-ESCROW-39,
              :QES-SCHD-TERM-39,
              :QES-SCHD-COLLECTED-39,
              NULLIF(:QES-SCHD-EFF-DATE-40,'1900-01-01'),
              NULLIF(:QES-SCHD-1STPYDATE-40,'1900-01-01'),
              :QES-SCHD-ESCROW-40,
              :QES-SCHD-TERM-40,
              :QES-SCHD-COLLECTED-40,
              :QES-BALANCE,
              :QES-PREPAID-1,
              :QES-COLLECTED-1,
              :QES-DISBURSED-TAXES-1,
              :QES-DISBURSED-INS-1,
              :QES-DISBURSED-PMI-1,
              :QES-DISBURSED-OTHER-1,
              :QES-DISBURSED-REF-1,
              :QES-DISBURSED-CNT-TAXES-1,
              :QES-DISBURSED-MISC1-1,
              :QES-DISBURSED-MISC2-1,
              :QES-PREPAID-2,
              :QES-COLLECTED-2,
              :QES-DISBURSED-TAXES-2,
              :QES-DISBURSED-INS-2,
              :QES-DISBURSED-PMI-2,
              :QES-DISBURSED-OTHER-2,
              :QES-DISBURSED-REF-2,
              :QES-DISBURSED-CNT-TAXES-2,
              :QES-DISBURSED-MISC1-2,
              :QES-DISBURSED-MISC2-2,
              :QES-PREPAID-3,
              :QES-COLLECTED-3,
              :QES-DISBURSED-TAXES-3,
              :QES-DISBURSED-INS-3,
              :QES-DISBURSED-PMI-3,
              :QES-DISBURSED-OTHER-3,
              :QES-DISBURSED-REF-3,
              :QES-DISBURSED-CNT-TAXES-3,
              :QES-DISBURSED-MISC1-3,
              :QES-DISBURSED-MISC2-3,
              :QES-MISC-AMT-1,
              NULLIF(:QES-EXP-DATE-1,'1900-01-01'),
              :QES-MISC-AMT-2,
              NULLIF(:QES-EXP-DATE-2,'1900-01-01'),
              :QES-MISC-AMT-3,
              NULLIF(:QES-EXP-DATE-3,'1900-01-01'),
              :QES-MISC-AMT-4,
              NULLIF(:QES-EXP-DATE-4,'1900-01-01'),
              :QES-MISC-AMT-5,
              NULLIF(:QES-EXP-DATE-5,'1900-01-01'),
              :QES-MISC-AMT-6,
              NULLIF(:QES-EXP-DATE-6,'1900-01-01'))
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-ES1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-ES1-FILE.

           MOVE ES-PATH-OWNBR  TO ES-BRNO.
           ACCEPT ES-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE ES-SELECT-PATH TO E-FILE.
           MOVE ES1-KEY        TO E-KEYX.
           PERFORM SET-ES-FIELDS.

           EXEC SQL
            UPDATE DBO.ESFILE
             SET
              ESFILE.ES_BRNO = :QES-BRNO,
              ESFILE.ES_ACCTNO = :QES-ACCTNO,
              ESFILE.ES_LTOUCH_DATE =
                 NULLIF(:QES-LTOUCH-DATE,'1900-01-01'),
              ESFILE.ES_SCHD_EFF_DATE_1 =
                 NULLIF(:QES-SCHD-EFF-DATE-1,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_1 =
                 NULLIF(:QES-SCHD-1STPYDATE-1,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_1 = :QES-SCHD-ESCROW-1,
              ESFILE.ES_SCHD_TERM_1 = :QES-SCHD-TERM-1,
              ESFILE.ES_SCHD_COLLECTED_1 = :QES-SCHD-COLLECTED-1,
              ESFILE.ES_SCHD_EFF_DATE_2 =
                 NULLIF(:QES-SCHD-EFF-DATE-2,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_2 =
                 NULLIF(:QES-SCHD-1STPYDATE-2,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_2 = :QES-SCHD-ESCROW-2,
              ESFILE.ES_SCHD_TERM_2 = :QES-SCHD-TERM-2,
              ESFILE.ES_SCHD_COLLECTED_2 = :QES-SCHD-COLLECTED-2,
              ESFILE.ES_SCHD_EFF_DATE_3 =
                 NULLIF(:QES-SCHD-EFF-DATE-3,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_3 =
                 NULLIF(:QES-SCHD-1STPYDATE-3,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_3 = :QES-SCHD-ESCROW-3,
              ESFILE.ES_SCHD_TERM_3 = :QES-SCHD-TERM-3,
              ESFILE.ES_SCHD_COLLECTED_3 = :QES-SCHD-COLLECTED-3,
              ESFILE.ES_SCHD_EFF_DATE_4 =
                 NULLIF(:QES-SCHD-EFF-DATE-4,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_4 =
                 NULLIF(:QES-SCHD-1STPYDATE-4,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_4 = :QES-SCHD-ESCROW-4,
              ESFILE.ES_SCHD_TERM_4 = :QES-SCHD-TERM-4,
              ESFILE.ES_SCHD_COLLECTED_4 = :QES-SCHD-COLLECTED-4,
              ESFILE.ES_SCHD_EFF_DATE_5 =
                 NULLIF(:QES-SCHD-EFF-DATE-5,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_5 =
                 NULLIF(:QES-SCHD-1STPYDATE-5,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_5 = :QES-SCHD-ESCROW-5,
              ESFILE.ES_SCHD_TERM_5 = :QES-SCHD-TERM-5,
              ESFILE.ES_SCHD_COLLECTED_5 = :QES-SCHD-COLLECTED-5,
              ESFILE.ES_SCHD_EFF_DATE_6 =
                 NULLIF(:QES-SCHD-EFF-DATE-6,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_6 =
                 NULLIF(:QES-SCHD-1STPYDATE-6,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_6 = :QES-SCHD-ESCROW-6,
              ESFILE.ES_SCHD_TERM_6 = :QES-SCHD-TERM-6,
              ESFILE.ES_SCHD_COLLECTED_6 = :QES-SCHD-COLLECTED-6,
              ESFILE.ES_SCHD_EFF_DATE_7 =
                 NULLIF(:QES-SCHD-EFF-DATE-7,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_7 =
                 NULLIF(:QES-SCHD-1STPYDATE-7,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_7 = :QES-SCHD-ESCROW-7,
              ESFILE.ES_SCHD_TERM_7 = :QES-SCHD-TERM-7,
              ESFILE.ES_SCHD_COLLECTED_7 = :QES-SCHD-COLLECTED-7,
              ESFILE.ES_SCHD_EFF_DATE_8 =
                 NULLIF(:QES-SCHD-EFF-DATE-8,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_8 =
                 NULLIF(:QES-SCHD-1STPYDATE-8,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_8 = :QES-SCHD-ESCROW-8,
              ESFILE.ES_SCHD_TERM_8 = :QES-SCHD-TERM-8,
              ESFILE.ES_SCHD_COLLECTED_8 = :QES-SCHD-COLLECTED-8,
              ESFILE.ES_SCHD_EFF_DATE_9 =
                 NULLIF(:QES-SCHD-EFF-DATE-9,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_9 =
                 NULLIF(:QES-SCHD-1STPYDATE-9,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_9 = :QES-SCHD-ESCROW-9,
              ESFILE.ES_SCHD_TERM_9 = :QES-SCHD-TERM-9,
              ESFILE.ES_SCHD_COLLECTED_9 = :QES-SCHD-COLLECTED-9,
              ESFILE.ES_SCHD_EFF_DATE_10 =
                 NULLIF(:QES-SCHD-EFF-DATE-10,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_10 =
                 NULLIF(:QES-SCHD-1STPYDATE-10,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_10 = :QES-SCHD-ESCROW-10,
              ESFILE.ES_SCHD_TERM_10 = :QES-SCHD-TERM-10,
              ESFILE.ES_SCHD_COLLECTED_10 =
                   :QES-SCHD-COLLECTED-10,
              ESFILE.ES_SCHD_EFF_DATE_11 =
                 NULLIF(:QES-SCHD-EFF-DATE-11,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_11 =
                 NULLIF(:QES-SCHD-1STPYDATE-11,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_11 = :QES-SCHD-ESCROW-11,
              ESFILE.ES_SCHD_TERM_11 = :QES-SCHD-TERM-11,
              ESFILE.ES_SCHD_COLLECTED_11 =
                   :QES-SCHD-COLLECTED-11,
              ESFILE.ES_SCHD_EFF_DATE_12 =
                 NULLIF(:QES-SCHD-EFF-DATE-12,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_12 =
                 NULLIF(:QES-SCHD-1STPYDATE-12,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_12 = :QES-SCHD-ESCROW-12,
              ESFILE.ES_SCHD_TERM_12 = :QES-SCHD-TERM-12,
              ESFILE.ES_SCHD_COLLECTED_12 =
                   :QES-SCHD-COLLECTED-12,
              ESFILE.ES_SCHD_EFF_DATE_13 =
                 NULLIF(:QES-SCHD-EFF-DATE-13,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_13 =
                 NULLIF(:QES-SCHD-1STPYDATE-13,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_13 = :QES-SCHD-ESCROW-13,
              ESFILE.ES_SCHD_TERM_13 = :QES-SCHD-TERM-13,
              ESFILE.ES_SCHD_COLLECTED_13 =
                   :QES-SCHD-COLLECTED-13,
              ESFILE.ES_SCHD_EFF_DATE_14 =
                 NULLIF(:QES-SCHD-EFF-DATE-14,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_14 =
                 NULLIF(:QES-SCHD-1STPYDATE-14,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_14 = :QES-SCHD-ESCROW-14,
              ESFILE.ES_SCHD_TERM_14 = :QES-SCHD-TERM-14,
              ESFILE.ES_SCHD_COLLECTED_14 =
                   :QES-SCHD-COLLECTED-14,
              ESFILE.ES_SCHD_EFF_DATE_15 =
                 NULLIF(:QES-SCHD-EFF-DATE-15,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_15 =
                 NULLIF(:QES-SCHD-1STPYDATE-15,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_15 = :QES-SCHD-ESCROW-15,
              ESFILE.ES_SCHD_TERM_15 = :QES-SCHD-TERM-15,
              ESFILE.ES_SCHD_COLLECTED_15 =
                   :QES-SCHD-COLLECTED-15,
              ESFILE.ES_SCHD_EFF_DATE_16 =
                 NULLIF(:QES-SCHD-EFF-DATE-16,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_16 =
                 NULLIF(:QES-SCHD-1STPYDATE-16,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_16 = :QES-SCHD-ESCROW-16,
              ESFILE.ES_SCHD_TERM_16 = :QES-SCHD-TERM-16,
              ESFILE.ES_SCHD_COLLECTED_16 =
                   :QES-SCHD-COLLECTED-16,
              ESFILE.ES_SCHD_EFF_DATE_17 =
                 NULLIF(:QES-SCHD-EFF-DATE-17,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_17 =
                 NULLIF(:QES-SCHD-1STPYDATE-17,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_17 = :QES-SCHD-ESCROW-17,
              ESFILE.ES_SCHD_TERM_17 = :QES-SCHD-TERM-17,
              ESFILE.ES_SCHD_COLLECTED_17 =
                   :QES-SCHD-COLLECTED-17,
              ESFILE.ES_SCHD_EFF_DATE_18 =
                 NULLIF(:QES-SCHD-EFF-DATE-18,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_18 =
                 NULLIF(:QES-SCHD-1STPYDATE-18,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_18 = :QES-SCHD-ESCROW-18,
              ESFILE.ES_SCHD_TERM_18 = :QES-SCHD-TERM-18,
              ESFILE.ES_SCHD_COLLECTED_18 =
                   :QES-SCHD-COLLECTED-18,
              ESFILE.ES_SCHD_EFF_DATE_19 =
                 NULLIF(:QES-SCHD-EFF-DATE-19,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_19 =
                 NULLIF(:QES-SCHD-1STPYDATE-19,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_19 = :QES-SCHD-ESCROW-19,
              ESFILE.ES_SCHD_TERM_19 = :QES-SCHD-TERM-19,
              ESFILE.ES_SCHD_COLLECTED_19 =
                   :QES-SCHD-COLLECTED-19,
              ESFILE.ES_SCHD_EFF_DATE_20 =
                 NULLIF(:QES-SCHD-EFF-DATE-20,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_20 =
                 NULLIF(:QES-SCHD-1STPYDATE-20,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_20 = :QES-SCHD-ESCROW-20,
              ESFILE.ES_SCHD_TERM_20 = :QES-SCHD-TERM-20,
              ESFILE.ES_SCHD_COLLECTED_20 =
                   :QES-SCHD-COLLECTED-20,
              ESFILE.ES_SCHD_EFF_DATE_21 =
                 NULLIF(:QES-SCHD-EFF-DATE-21,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_21 =
                 NULLIF(:QES-SCHD-1STPYDATE-21,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_21 = :QES-SCHD-ESCROW-21,
              ESFILE.ES_SCHD_TERM_21 = :QES-SCHD-TERM-21,
              ESFILE.ES_SCHD_COLLECTED_21 =
                   :QES-SCHD-COLLECTED-21,
              ESFILE.ES_SCHD_EFF_DATE_22 =
                 NULLIF(:QES-SCHD-EFF-DATE-22,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_22 =
                 NULLIF(:QES-SCHD-1STPYDATE-22,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_22 = :QES-SCHD-ESCROW-22,
              ESFILE.ES_SCHD_TERM_22 = :QES-SCHD-TERM-22,
              ESFILE.ES_SCHD_COLLECTED_22 =
                   :QES-SCHD-COLLECTED-22,
              ESFILE.ES_SCHD_EFF_DATE_23 =
                 NULLIF(:QES-SCHD-EFF-DATE-23,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_23 =
                 NULLIF(:QES-SCHD-1STPYDATE-23,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_23 = :QES-SCHD-ESCROW-23,
              ESFILE.ES_SCHD_TERM_23 = :QES-SCHD-TERM-23,
              ESFILE.ES_SCHD_COLLECTED_23 =
                   :QES-SCHD-COLLECTED-23,
              ESFILE.ES_SCHD_EFF_DATE_24 =
                 NULLIF(:QES-SCHD-EFF-DATE-24,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_24 =
                 NULLIF(:QES-SCHD-1STPYDATE-24,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_24 = :QES-SCHD-ESCROW-24,
              ESFILE.ES_SCHD_TERM_24 = :QES-SCHD-TERM-24,
              ESFILE.ES_SCHD_COLLECTED_24 =
                   :QES-SCHD-COLLECTED-24,
              ESFILE.ES_SCHD_EFF_DATE_25 =
                 NULLIF(:QES-SCHD-EFF-DATE-25,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_25 =
                 NULLIF(:QES-SCHD-1STPYDATE-25,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_25 = :QES-SCHD-ESCROW-25,
              ESFILE.ES_SCHD_TERM_25 = :QES-SCHD-TERM-25,
              ESFILE.ES_SCHD_COLLECTED_25 =
                   :QES-SCHD-COLLECTED-25,
              ESFILE.ES_SCHD_EFF_DATE_26 =
                 NULLIF(:QES-SCHD-EFF-DATE-26,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_26 =
                 NULLIF(:QES-SCHD-1STPYDATE-26,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_26 = :QES-SCHD-ESCROW-26,
              ESFILE.ES_SCHD_TERM_26 = :QES-SCHD-TERM-26,
              ESFILE.ES_SCHD_COLLECTED_26 =
                   :QES-SCHD-COLLECTED-26,
              ESFILE.ES_SCHD_EFF_DATE_27 =
                 NULLIF(:QES-SCHD-EFF-DATE-27,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_27 =
                 NULLIF(:QES-SCHD-1STPYDATE-27,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_27 = :QES-SCHD-ESCROW-27,
              ESFILE.ES_SCHD_TERM_27 = :QES-SCHD-TERM-27,
              ESFILE.ES_SCHD_COLLECTED_27 =
                   :QES-SCHD-COLLECTED-27,
              ESFILE.ES_SCHD_EFF_DATE_28 =
                 NULLIF(:QES-SCHD-EFF-DATE-28,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_28 =
                 NULLIF(:QES-SCHD-1STPYDATE-28,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_28 = :QES-SCHD-ESCROW-28,
              ESFILE.ES_SCHD_TERM_28 = :QES-SCHD-TERM-28,
              ESFILE.ES_SCHD_COLLECTED_28 =
                   :QES-SCHD-COLLECTED-28,
              ESFILE.ES_SCHD_EFF_DATE_29 =
                 NULLIF(:QES-SCHD-EFF-DATE-29,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_29 =
                 NULLIF(:QES-SCHD-1STPYDATE-29,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_29 = :QES-SCHD-ESCROW-29,
              ESFILE.ES_SCHD_TERM_29 = :QES-SCHD-TERM-29,
              ESFILE.ES_SCHD_COLLECTED_29 =
                   :QES-SCHD-COLLECTED-29,
              ESFILE.ES_SCHD_EFF_DATE_30 =
                 NULLIF(:QES-SCHD-EFF-DATE-30,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_30 =
                 NULLIF(:QES-SCHD-1STPYDATE-30,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_30 = :QES-SCHD-ESCROW-30,
              ESFILE.ES_SCHD_TERM_30 = :QES-SCHD-TERM-30,
              ESFILE.ES_SCHD_COLLECTED_30 =
                   :QES-SCHD-COLLECTED-30,
              ESFILE.ES_SCHD_EFF_DATE_31 =
                 NULLIF(:QES-SCHD-EFF-DATE-31,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_31 =
                 NULLIF(:QES-SCHD-1STPYDATE-31,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_31 = :QES-SCHD-ESCROW-31,
              ESFILE.ES_SCHD_TERM_31 = :QES-SCHD-TERM-31,
              ESFILE.ES_SCHD_COLLECTED_31 =
                   :QES-SCHD-COLLECTED-31,
              ESFILE.ES_SCHD_EFF_DATE_32 =
                 NULLIF(:QES-SCHD-EFF-DATE-32,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_32 =
                 NULLIF(:QES-SCHD-1STPYDATE-32,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_32 = :QES-SCHD-ESCROW-32,
              ESFILE.ES_SCHD_TERM_32 = :QES-SCHD-TERM-32,
              ESFILE.ES_SCHD_COLLECTED_32 =
                   :QES-SCHD-COLLECTED-32,
              ESFILE.ES_SCHD_EFF_DATE_33 =
                 NULLIF(:QES-SCHD-EFF-DATE-33,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_33 =
                 NULLIF(:QES-SCHD-1STPYDATE-33,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_33 = :QES-SCHD-ESCROW-33,
              ESFILE.ES_SCHD_TERM_33 = :QES-SCHD-TERM-33,
              ESFILE.ES_SCHD_COLLECTED_33 =
                   :QES-SCHD-COLLECTED-33,
              ESFILE.ES_SCHD_EFF_DATE_34 =
                 NULLIF(:QES-SCHD-EFF-DATE-34,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_34 =
                 NULLIF(:QES-SCHD-1STPYDATE-34,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_34 = :QES-SCHD-ESCROW-34,
              ESFILE.ES_SCHD_TERM_34 = :QES-SCHD-TERM-34,
              ESFILE.ES_SCHD_COLLECTED_34 =
                   :QES-SCHD-COLLECTED-34,
              ESFILE.ES_SCHD_EFF_DATE_35 =
                 NULLIF(:QES-SCHD-EFF-DATE-35,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_35 =
                 NULLIF(:QES-SCHD-1STPYDATE-35,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_35 = :QES-SCHD-ESCROW-35,
              ESFILE.ES_SCHD_TERM_35 = :QES-SCHD-TERM-35,
              ESFILE.ES_SCHD_COLLECTED_35 =
                   :QES-SCHD-COLLECTED-35,
              ESFILE.ES_SCHD_EFF_DATE_36 =
                 NULLIF(:QES-SCHD-EFF-DATE-36,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_36 =
                 NULLIF(:QES-SCHD-1STPYDATE-36,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_36 = :QES-SCHD-ESCROW-36,
              ESFILE.ES_SCHD_TERM_36 = :QES-SCHD-TERM-36,
              ESFILE.ES_SCHD_COLLECTED_36 =
                   :QES-SCHD-COLLECTED-36,
              ESFILE.ES_SCHD_EFF_DATE_37 =
                 NULLIF(:QES-SCHD-EFF-DATE-37,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_37 =
                 NULLIF(:QES-SCHD-1STPYDATE-37,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_37 = :QES-SCHD-ESCROW-37,
              ESFILE.ES_SCHD_TERM_37 = :QES-SCHD-TERM-37,
              ESFILE.ES_SCHD_COLLECTED_37 =
                   :QES-SCHD-COLLECTED-37,
              ESFILE.ES_SCHD_EFF_DATE_38 =
                 NULLIF(:QES-SCHD-EFF-DATE-38,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_38 =
                 NULLIF(:QES-SCHD-1STPYDATE-38,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_38 = :QES-SCHD-ESCROW-38,
              ESFILE.ES_SCHD_TERM_38 = :QES-SCHD-TERM-38,
              ESFILE.ES_SCHD_COLLECTED_38 =
                   :QES-SCHD-COLLECTED-38,
              ESFILE.ES_SCHD_EFF_DATE_39 =
                 NULLIF(:QES-SCHD-EFF-DATE-39,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_39 =
                 NULLIF(:QES-SCHD-1STPYDATE-39,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_39 = :QES-SCHD-ESCROW-39,
              ESFILE.ES_SCHD_TERM_39 = :QES-SCHD-TERM-39,
              ESFILE.ES_SCHD_COLLECTED_39 =
                   :QES-SCHD-COLLECTED-39,
              ESFILE.ES_SCHD_EFF_DATE_40 =
                 NULLIF(:QES-SCHD-EFF-DATE-40,'1900-01-01'),
              ESFILE.ES_SCHD_1STPYDATE_40 =
                 NULLIF(:QES-SCHD-1STPYDATE-40,'1900-01-01'),
              ESFILE.ES_SCHD_ESCROW_40 = :QES-SCHD-ESCROW-40,
              ESFILE.ES_SCHD_TERM_40 = :QES-SCHD-TERM-40,
              ESFILE.ES_SCHD_COLLECTED_40 =
                   :QES-SCHD-COLLECTED-40,
              ESFILE.ES_BALANCE = :QES-BALANCE,
              ESFILE.ES_PREPAID_1 = :QES-PREPAID-1,
              ESFILE.ES_COLLECTED_1 = :QES-COLLECTED-1,
              ESFILE.ES_DISBURSED_TAXES_1 =
                   :QES-DISBURSED-TAXES-1,
              ESFILE.ES_DISBURSED_INS_1 = :QES-DISBURSED-INS-1,
              ESFILE.ES_DISBURSED_PMI_1 = :QES-DISBURSED-PMI-1,
              ESFILE.ES_DISBURSED_OTHER_1 =
                   :QES-DISBURSED-OTHER-1,
              ESFILE.ES_DISBURSED_REF_1 = :QES-DISBURSED-REF-1,
              ESFILE.ES_DISBURSED_CNT_TAXES_1 =
                   :QES-DISBURSED-CNT-TAXES-1,
              ESFILE.ES_DISBURSED_MISC1_1 =
                   :QES-DISBURSED-MISC1-1,
              ESFILE.ES_DISBURSED_MISC2_1 =
                   :QES-DISBURSED-MISC2-1,
              ESFILE.ES_PREPAID_2 = :QES-PREPAID-2,
              ESFILE.ES_COLLECTED_2 = :QES-COLLECTED-2,
              ESFILE.ES_DISBURSED_TAXES_2 =
                   :QES-DISBURSED-TAXES-2,
              ESFILE.ES_DISBURSED_INS_2 = :QES-DISBURSED-INS-2,
              ESFILE.ES_DISBURSED_PMI_2 = :QES-DISBURSED-PMI-2,
              ESFILE.ES_DISBURSED_OTHER_2 =
                   :QES-DISBURSED-OTHER-2,
              ESFILE.ES_DISBURSED_REF_2 = :QES-DISBURSED-REF-2,
              ESFILE.ES_DISBURSED_CNT_TAXES_2 =
                   :QES-DISBURSED-CNT-TAXES-2,
              ESFILE.ES_DISBURSED_MISC1_2 =
                   :QES-DISBURSED-MISC1-2,
              ESFILE.ES_DISBURSED_MISC2_2 =
                   :QES-DISBURSED-MISC2-2,
              ESFILE.ES_PREPAID_3 = :QES-PREPAID-3,
              ESFILE.ES_COLLECTED_3 = :QES-COLLECTED-3,
              ESFILE.ES_DISBURSED_TAXES_3 =
                   :QES-DISBURSED-TAXES-3,
              ESFILE.ES_DISBURSED_INS_3 = :QES-DISBURSED-INS-3,
              ESFILE.ES_DISBURSED_PMI_3 = :QES-DISBURSED-PMI-3,
              ESFILE.ES_DISBURSED_OTHER_3 =
                   :QES-DISBURSED-OTHER-3,
              ESFILE.ES_DISBURSED_REF_3 = :QES-DISBURSED-REF-3,
              ESFILE.ES_DISBURSED_CNT_TAXES_3 =
                   :QES-DISBURSED-CNT-TAXES-3,
              ESFILE.ES_DISBURSED_MISC1_3 =
                   :QES-DISBURSED-MISC1-3,
              ESFILE.ES_DISBURSED_MISC2_3 =
                   :QES-DISBURSED-MISC2-3,
              ESFILE.ES_MISC_AMT_1 = :QES-MISC-AMT-1,
              ESFILE.ES_EXP_DATE_1 =
                 NULLIF(:QES-EXP-DATE-1,'1900-01-01'),
              ESFILE.ES_MISC_AMT_2 = :QES-MISC-AMT-2,
              ESFILE.ES_EXP_DATE_2 =
                 NULLIF(:QES-EXP-DATE-2,'1900-01-01'),
              ESFILE.ES_MISC_AMT_3 = :QES-MISC-AMT-3,
              ESFILE.ES_EXP_DATE_3 =
                 NULLIF(:QES-EXP-DATE-3,'1900-01-01'),
              ESFILE.ES_MISC_AMT_4 = :QES-MISC-AMT-4,
              ESFILE.ES_EXP_DATE_4 =
                 NULLIF(:QES-EXP-DATE-4,'1900-01-01'),
              ESFILE.ES_MISC_AMT_5 = :QES-MISC-AMT-5,
              ESFILE.ES_EXP_DATE_5 =
                 NULLIF(:QES-EXP-DATE-5,'1900-01-01'),
              ESFILE.ES_MISC_AMT_6 = :QES-MISC-AMT-6,
              ESFILE.ES_EXP_DATE_6 =
                 NULLIF(:QES-EXP-DATE-6,'1900-01-01')
             WHERE ESFILE.ES_BRNO = :QES-BRNO
               AND ESFILE.ES_ACCTNO = :QES-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-ES1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-ES1-FILE.

           MOVE ES-PATH-OWNBR  TO ES-BRNO.
           PERFORM DELETE-IT.
           MOVE ES-SELECT-PATH TO E-FILE.
           MOVE ES1-KEY        TO E-KEYX.
           PERFORM SET-ES-FIELDS.

           EXEC SQL
            DELETE FROM DBO.ESFILE
             WHERE ESFILE.ES_BRNO = :QES-BRNO
               AND ESFILE.ES_ACCTNO = :QES-ACCTNO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-ES1-FILE.

      *-----------------------------------------------------------------
       CLOSE-ES1-FILE.

           IF ( ES1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE ES1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO ES1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
 
      *=================================================================
      *=================================================================
      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *-----------------------------------------------------------------
       END-RUN.

           PERFORM CLOSE-FILES.

      *    MOVE WK1-PATH TO ACCESS-BUF.
      *    PERFORM REMOVE-WORKFILE.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y"  )
                 MOVE SV-BEG-BRANCH     TO BEG-BRANCH
                 MOVE SV-END-BRANCH     TO END-BRANCH
                 MOVE SV-GROUP-FLAG     TO GROUP-FLAG
                 MOVE SV-ENT-GROUP      TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH TO BEG-BRANCH
                 MOVE SV-END-BRANCH TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOESIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
