      * COPYMEMBER: LIBUP/UP1534W.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBUP/UP1534.CPY
      *
      * DESCRIPTION   : WORKERS FOR UPGRADE A30 TO A32
      *
      *=================================================================
      * REV:
      * KEC 2018-0918 ***NEW*** LOCAL-TO-GLOBAL L2G
      * KEC 2019-1010 ADDED OPTION OF "B" FOR BASESH (/USR/DATA/SH)
      *               FOR UP1534-FILE-TYPE.
      *               ADDED UP1534-MV15-OLD-PATH AND
      *               UP1534-MV15-NEW-PATH WORKERS.
      *
      * KEC 2019-1113 ADDED UP1534-FILE-BEG-TIME
      *                     UP1534-F1-FILE-TIMES
      *                     UP1534-F1-BEG-PROMPT
      *                     UP1534-F1-BEG-TIME
      *                     UP1534-F1-END-PROMPT
      *                     UP1534-F1-END-TIME
      *                     UP1534-F1-TOT-PROMPT
      *                     UP1534-F1-TOT-TIME
      *                     UP1534-TIME-BEG
      *                     UP1534-TIME-END
      *                     UP1534-TIME-ELAPSED
      *
      * KEC 2019-1120 ADDED UP1534-FILE-BEG-TIMEX
      *                     UP1534-FILE-END-TIMEX
      *
      ******************************************************************
      *
      *    U P 1 5 3 4 - M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  UP1534-RISC2                        PIC X(2) VALUE "R2".
       01  UP1534-RECORD-COUNTERS                          VALUE ZEROES.
           03  UP1534-RECORD-COUNTER-READ      PIC 9(10).
           03  UP1534-RECORD-COUNTER-WRITE     PIC 9(10).
           03  UP1534-RECORD-COUNTER-SKIP      PIC 9(10).

       01  UP1534-REPORT-IFN                   PIC X(06) VALUE "REPORT".

      ******************************************************************
      *
      *    U P 1 5 3 4 - F I L E - W O R K E R S
      *
      ******************************************************************
       01  UP1534-CURRENT-FILE                 PIC X(06)   VALUE SPACES.

       01  UP1534-FILE-BEG-TIME                PIC 9(06)   VALUE ZEROES.
       01  FILLER REDEFINES UP1534-FILE-BEG-TIME.
           03  UP1534-FILE-BEG-TIME-HH         PIC 9(02).
           03  UP1534-FILE-BEG-TIME-MM         PIC 9(02).
           03  UP1534-FILE-BEG-TIME-SS         PIC 9(02).

       01  UP1534-FILE-INPUT                   PIC X(22)   VALUE SPACES.
       01  FILLER                    REDEFINES UP1534-FILE-INPUT.
           03  UP1534-FILE-INPUT-DIR           PIC X(02).
           03  UP1534-FILE-INPUT-SLASH         PIC X(01).
           03  UP1534-FILE-INPUT-NAME          PIC X(19).

      *-----------------------------------------------------------------
       78  UP1534-FILE-ORDER---SOURCE                      VALUE "S".
       78  UP1534-FILE-ORDER---TARGET                      VALUE "T".
       77  UP1534-FILE-ORDER                   PIC X(01)   VALUE SPACES.
           88  UP1534-FILE-ORDER-SOURCE                    VALUE "S".
           88  UP1534-FILE-ORDER-TARGET                    VALUE "T".

      *-----------------------------------------------------------------
       78  UP1534-FILE-TYPE---BASESH                       VALUE "B".
       78  UP1534-FILE-TYPE---GLOBAL                       VALUE "G".
       78  UP1534-FILE-TYPE---LOCAL                        VALUE "L".
       77  UP1534-FILE-TYPE                    PIC X(01)   VALUE SPACES.
           88  UP1534-FILE-TYPE-BASESH                     VALUE "B".
           88  UP1534-FILE-TYPE-GLOBAL                     VALUE "G".
           88  UP1534-FILE-TYPE-LOCAL                      VALUE "L".

      *=================================================================
       01  UP1534-TIME-BEG                     PIC 9(06)   VALUE ZEROES.
       01  UP1534-TIME-BEGX          REDEFINES UP1534-TIME-BEG
                                               PIC X(06).
       01  FILLER                    REDEFINES UP1534-TIME-BEG.
           03  UP1534-TIME-BEG-HH              PIC 9(02).
           03  UP1534-TIME-BEG-MM              PIC 9(02).
           03  UP1534-TIME-BEG-SS              PIC 9(02).

       01  UP1534-TIME-END                     PIC 9(06)   VALUE ZEROES.
       01  UP1534-TIME-ENDX          REDEFINES UP1534-TIME-END
                                               PIC X(06).
       01  FILLER                    REDEFINES UP1534-TIME-END.
           03  UP1534-TIME-END-HH              PIC 9(02).
           03  UP1534-TIME-END-MM              PIC 9(02).
           03  UP1534-TIME-END-SS              PIC 9(02).

       01  UP1534-TIME-ELAPSED                 PIC 9(06)   VALUE ZEROES.
       01  FILLER REDEFINES UP1534-TIME-ELAPSED.
           03  UP1534-TIME-ELAPSED-HH          PIC 9(02).
           03  UP1534-TIME-ELAPSED-MM          PIC 9(02).
           03  UP1534-TIME-ELAPSED-SS          PIC 9(02).

       01  UP1534-TIME-SECONDS                 PIC 9(06)   VALUE ZEROES.

      *=================================================================
       01  UP1534-UPDATE-KEY           PIC X(40)     VALUE SPACES.

       01  UP1534-UPDATE-STAT          PIC X(05)     VALUE SPACES.
       01  FILLER            REDEFINES UP1534-UPDATE-STAT.
           03  UP1534-UPDATE-STAT-D1   PIC X(02).
           03  UP1534-UPDATE-STAT-C    PIC X(01).
           03  UP1534-UPDATE-STAT-D2   PIC X(02).

       01  UP1534-UPDATE-TYPE          PIC X(01)     VALUE SPACES.
           88  UP1534-UPDATE-TYPE-ERROR              VALUE "E".
           88  UP1534-UPDATE-TYPE-SKIP               VALUE SPACES.

      *=================================================================
       01  UP1534-FILE-PATH                          PIC X(35).

       01  FILLER                          REDEFINES UP1534-FILE-PATH.
           03  FILLER                                PIC X(01).
           03  UP1534-FILE-PATH-FIL                  PIC X(34).

       01  FILLER                          REDEFINES UP1534-FILE-PATH.
           03  UP1534-FILE-PATH-SLASH-1              PIC X(01).
           03  UP1534-FILE-PATH-DIR-1                PIC X(03).
           03  UP1534-FILE-PATH-SLASH-2              PIC X(01).
           03  UP1534-FILE-PATH-DIR-2                PIC X(04).
           03  UP1534-FILE-PATH-SLASH-3              PIC X(01).
           03  UP1534-FILE-PATH-MACHINE              PIC X(02).
           03  UP1534-FILE-PATH-SLASH-4              PIC X(01).
           03  UP1534-FILE-PATH-GLOBAL-LOCAL         PIC X(22).

       01  FILLER                          REDEFINES UP1534-FILE-PATH.
           03  FILLER                                PIC X(13).
           03  UP1534-FILE-PATH-GLOBAL-PATH.
               05  UP1534-FILE-PATH-GLOBAL-DIR       PIC X(02).
               05  UP1534-FILE-PATH-GLOBAL-SLASH     PIC X(01).
               05  UP1534-FILE-PATH-GLOBAL-FILE      PIC X(19).

       01  FILLER                          REDEFINES UP1534-FILE-PATH.
           03  UP1534-FILLER                         PIC X(13).
           03  UP1534-FILE-PATH-LOCAL-DATA           PIC X(06).
           03  UP1534-FILE-PATH-LOCAL-DATA-SL        PIC X(01).
           03  UP1534-FILE-PATH-LOCAL-PATH.
               05  UP1534-FILE-PATH-LOCAL-DIR        PIC X(02).
               05  UP1534-FILE-PATH-LOCAL-SLASH      PIC X(01).
               05  UP1534-FILE-PATH-LOCAL-FILE       PIC X(12).

      ******************************************************************
      *
      *    U P 1 5 3 4 - D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  UP1534-LN-FEED                     PIC 9(03)    VALUE ZEROES.

      *=================================================================
       01  UP1534-DETAIL-LINE                 PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  UP1534-DETAIL-LINE-1     REDEFINES UP1534-DETAIL-LINE.
           03  UP1534-D1-BRANCH-PROMPT        PIC X(08).
           03  UP1534-D1-BRANCH               PIC ZZZ9.
           03  FILLER                         PIC X(02).
           03  UP1534-D1-BRANCH-NAME          PIC X(66).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-2            REDEFINES UP1534-DETAIL-LINE.
           03  UP1534-D2-MACHINE-PROMPT       PIC X(09).
           03  UP1534-D2-MACHINE              PIC X(02).
           03  FILLER                         PIC X(03).
           03  UP1534-D2-MACHINE-NAME         PIC X(66).

      *-----------------------------------------------------------------
       01  UP1534-FILE-LINE-1       REDEFINES UP1534-DETAIL-LINE.
           03  FILLER                         PIC X(08).
           03  UP1534-F1-FILE-PROMPT          PIC X(06).
           03  UP1534-F1-FILE                 PIC X(02).
           03  FILLER                         PIC X(02).
           03  UP1534-F1-FILE-MESSAGE         PIC X(62).
           03  FILLER               REDEFINES UP1534-F1-FILE-MESSAGE.
               05  UP1534-F1-RECORD-READ-PROMPT   PIC X(06).
               05  UP1534-F1-RECORD-READ          PIC Z,ZZZ,ZZZ,ZZ9.
               05  FILLER                         PIC X(02).
               05  UP1534-F1-RECORD-WRITE-PROMPT  PIC X(07).
               05  UP1534-F1-RECORD-WRITE         PIC Z,ZZZ,ZZZ,ZZ9.
               05  FILLER                         PIC X(02).
               05  UP1534-F1-RECORD-SKIP-PROMPT   PIC X(06).
               05  UP1534-F1-RECORD-SKIP          PIC Z,ZZZ,ZZZ,ZZ9.
           03  FILLER                         PIC X(02).
           03  UP1534-F1-TIMES                PIC X(50).
           03  FILLER               REDEFINES UP1534-F1-TIMES.
               05  FILLER                     PIC X(05).
               05  UP1534-F1-BEG-PROMPT       PIC X(05).
               05  UP1534-F1-BEG-TIME         PIC 99/99/99.
               05  FILLER                     PIC X(02).
               05  UP1534-F1-END-PROMPT       PIC X(05).
               05  UP1534-F1-END-TIME         PIC 99/99/99.
               05  FILLER                     PIC X(02).
               05  UP1534-F1-TOT-PROMPT       PIC X(07).
               05  UP1534-F1-TOT-TIME         PIC 99/99/99.

      ******************************************************************
      *
      *    U P 1 5 3 4 - M V 1 5 - F I L E - W O R K E R S
      *
      *=================================================================
      * DUE TO ACU4GL, THE A15 FILE NEEDS TO BE RENAMED (VIA MOVE; MV)
      * SO IT IDENTIFIES THE CORRECT FILE.
      * FOR EXAMPLE, USING THE STFILE, THE A15 FILE OF
      * /US1/DA15/R1/LP/STFILE IS SEEN AS THE A34 STFILE DATABASE TABLE.
      * WHEN TRYING TO OPEN THE A15 FILE IT WILL RESULT IN AN ACU ERROR
      * OF "9D,,2".  FOR THE UPGRADE PROGRAMS TO WORK, NEED TO:
      * 
      *  1. MOVE /US1/DA15/R1/LP/STFILE TO /US1/DA15/R1/LP/OSTFILE
      *  2. OPEN, READ-NEXT, CLOSE ON THE OSTFILE.
      *  3. MOVE /US1/DA15/R1/LP/OSTFILE TO /US1/DA15/R1/LP/STFILE
      * 
      * NEED TO MOVE THE FILE BACK TO ITS ORIGINAL NAME IN CASE THE
      * UPGRADE IS BEING EXECUTED MULTIPLE TIMES (QA TESTING) OR FOR
      * SIDE-BY-SIDE TESTING BETWEEN A15 & A34.
      *
      *=================================================================
      *  BASESH - /US1/DA15/SH/YYFILE
      *  GLOBAL - /US1/DA15/R1/XX/YYFILE
      *  LOCAL  - /US1/DA15/R1/SS####/XX/YYFILE
      *           ....|....1....|....2....|....3
      *
      ******************************************************************
       01  UP1534-MV15-OLD-PATH                  PIC X(35).

       01  FILLER                      REDEFINES UP1534-MV15-OLD-PATH.
           03  FILLER                            PIC X(13).
           03  UP1534-MV15-OLD-BASESH-FILE       PIC X(22).

       01  FILLER                      REDEFINES UP1534-MV15-OLD-PATH.
           03  FILLER                            PIC X(16).
           03  UP1534-MV15-OLD-GLOBAL-FILE       PIC X(19).

       01  FILLER                      REDEFINES UP1534-MV15-OLD-PATH.
           03  FILLER                            PIC X(23).
           03  UP1534-MV15-OLD-LOCAL-FILE        PIC X(12).

      *-----------------------------------------------------------------
       01  UP1534-MV15-NEW-PATH                  PIC X(35).

       01  FILLER                      REDEFINES UP1534-MV15-NEW-PATH.
           03  FILLER                            PIC X(13).
           03  UP1534-MV15-NEW-BASESH-O          PIC X(01).
           03  UP1534-MV15-NEW-BASESH-FILE       PIC X(21).

       01  FILLER                      REDEFINES UP1534-MV15-NEW-PATH.
           03  FILLER                            PIC X(16).
           03  UP1534-MV15-NEW-GLOBAL-O          PIC X(01).
           03  UP1534-MV15-NEW-GLOBAL-FILE       PIC X(18).

       01  FILLER                      REDEFINES UP1534-MV15-NEW-PATH.
           03  FILLER                            PIC X(23).
           03  UP1534-MV15-NEW-LOCAL-O           PIC X(01).
           03  UP1534-MV15-NEW-LOCAL-FILE        PIC X(11).

      ******************************************************************
      *
      *    U P 1 5 3 4 - V P I - W O R K E R S 
      *
      *=================================================================
      *    UP1534 VIDEO PROGRESS INDICATOR (VPI)
      ******************************************************************
       01  UP1534-VPI-BRNO                 PIC 9(04)   VALUE ZEROES.

       01  UP1534-VPI-PRU-LP               PIC X(03)   VALUE "HLD".
           88  UP1534-VPI-QUEUE-PIGGYBACK              VALUE "PRU"
                                                             "DEV".
           88  UP1534-VPI-QUEUE-PRINTER                VALUE "   ".
           88  UP1534-VPI-QUEUE-TERMINAL               VALUE "VDU".
           88  UP1534-VPI-QUEUE-VIEW                   VALUE "HLD".
           88  UP1534-VPI-QUEUE-PRINTER-OR-VIEW        VALUE "   "
                                                             "HLD".

       01  UP1534-VPI-ROUTE-BUF            PIC X(02)   VALUE "00".
           88  UP1534-VPI-ROUTE-MENU-DRIVEN            VALUE "00" "  ".

       01  UP1534-VPI-SLEEP-COUNTER        PIC 9(04)   VALUE ZEROES.
       01  UP1534-VPI-SLEEP-COUNTER-MAX    PIC 9(04)   VALUE 9900.
       01  UP1534-VPI-TICKER               PIC X(01)   VALUE "-".
       01  UP1534-VPI-TIME-EDIT            PIC X(08)   VALUE SPACES.

      *=================================================================
      *
      *    U P 1 5 3 4 - V P I - D I S P L A Y  - W O R K E R S
      *
      *=================================================================
       01  UP1534-VPI-DISPLAY-BUF               PIC X(78)  VALUE SPACES.
       01  FILLER                     REDEFINES UP1534-VPI-DISPLAY-BUF.
           03  UP1534-VPI-DISPLAY-LEFT          PIC X(01).
           03  UP1534-VPI-DISPLAY-TICKER        PIC X(01).
           03  UP1534-VPI-DISPLAY-RIGHT         PIC X(01).
           03  FILLER                           PIC X(01).
           03  UP1534-VPI-DISPLAY-TEXT          PIC X(74).
           03  FILLER                 REDEFINES UP1534-VPI-DISPLAY-TEXT.
               05  UP1534-VPI-DISPLAY-BRNO      PIC 9(04).
               05  FILLER                       PIC X(02).
               05  UP1534-VPI-DISPLAY-FILE      PIC X(58).
               05  FILLER                       PIC X(02).
               05  UP1534-VPI-DISPLAY-TIME      PIC X(08).

       01  UP1534-VPI-DISPLAY-LIST.
           03  FILLER PIC X(14) VALUE "LEGEND ACCEPT.".

      *=================================================================
      *
      *    U P 1 5 3 4 - V P I - F I L E - W O R K E R S
      *
      *=================================================================
       78  UP1534-VPI-FILE-TYPE---GLOBAL                 VALUE "G".
       78  UP1534-VPI-FILE-TYPE---LOCAL                  VALUE "L".
       77  UP1534-VPI-FILE-TYPE            PIC X(01)     VALUE SPACES.
           88  UP1534-VPI-FILE-TYPE-GLOBAL               VALUE "G".
           88  UP1534-VPI-FILE-TYPE-LOCAL                VALUE "L".

      *-----------------------------------------------------------------
       01  UP1534-VPI-FILE-PREFIX         PIC X(06)   VALUE SPACES.
           88  UP1534-VPI-FILE-AL                     VALUE "AL".
           88  UP1534-VPI-FILE-AQ                     VALUE "AQ".
           88  UP1534-VPI-FILE-BM                     VALUE "BM".
           88  UP1534-VPI-FILE-BW                     VALUE "BW".
           88  UP1534-VPI-FILE-BR                     VALUE "BR".
           88  UP1534-VPI-FILE-BY                     VALUE "BY".
           88  UP1534-VPI-FILE-CA                     VALUE "CA".
           88  UP1534-VPI-FILE-CD                     VALUE "CD".
           88  UP1534-VPI-FILE-CI                     VALUE "CI".
           88  UP1534-VPI-FILE-CJ                     VALUE "CJ".
           88  UP1534-VPI-FILE-CK                     VALUE "CK".
           88  UP1534-VPI-FILE-CL                     VALUE "CL".
           88  UP1534-VPI-FILE-CP                     VALUE "CP".
           88  UP1534-VPI-FILE-CQ                     VALUE "CQ".
           88  UP1534-VPI-FILE-CX                     VALUE "CX".
           88  UP1534-VPI-FILE-DL                     VALUE "DL".
           88  UP1534-VPI-FILE-DM                     VALUE "DM".
           88  UP1534-VPI-FILE-DS                     VALUE "DS".
           88  UP1534-VPI-FILE-DT                     VALUE "DT".
           88  UP1534-VPI-FILE-EA                     VALUE "EA".
           88  UP1534-VPI-FILE-EP                     VALUE "EP".
           88  UP1534-VPI-FILE-ES                     VALUE "ES".
           88  UP1534-VPI-FILE-EW                     VALUE "EW".
           88  UP1534-VPI-FILE-FD                     VALUE "FD".
           88  UP1534-VPI-FILE-FM                     VALUE "FM".
           88  UP1534-VPI-FILE-FS                     VALUE "FS".
           88  UP1534-VPI-FILE-GB                     VALUE "GB".
           88  UP1534-VPI-FILE-GI                     VALUE "GI".
           88  UP1534-VPI-FILE-GL                     VALUE "GL".
           88  UP1534-VPI-FILE-GT                     VALUE "GT".
           88  UP1534-VPI-FILE-GP                     VALUE "GP".
           88  UP1534-VPI-FILE-GQ                     VALUE "GQ".
           88  UP1534-VPI-FILE-IC                     VALUE "IC".
           88  UP1534-VPI-FILE-II                     VALUE "II".
           88  UP1534-VPI-FILE-IR                     VALUE "IR".
           88  UP1534-VPI-FILE-IX                     VALUE "IX".
           88  UP1534-VPI-FILE-JD                     VALUE "JD".
           88  UP1534-VPI-FILE-JH                     VALUE "JH".
           88  UP1534-VPI-FILE-LC                     VALUE "LC".
           88  UP1534-VPI-FILE-LE                     VALUE "LE".
           88  UP1534-VPI-FILE-LH                     VALUE "LH".
           88  UP1534-VPI-FILE-LM                     VALUE "LM".
           88  UP1534-VPI-FILE-LN                     VALUE "LN".
           88  UP1534-VPI-FILE-LP                     VALUE "LP".
           88  UP1534-VPI-FILE-LS                     VALUE "LS".
           88  UP1534-VPI-FILE-LSD                    VALUE "LSD".
           88  UP1534-VPI-FILE-LSF                    VALUE "LSF".
           88  UP1534-VPI-FILE-LSG                    VALUE "LSG".
           88  UP1534-VPI-FILE-LSJ                    VALUE "LSJ".
           88  UP1534-VPI-FILE-LSL                    VALUE "LSL".
           88  UP1534-VPI-FILE-LSM                    VALUE "LSM".
           88  UP1534-VPI-FILE-LSP                    VALUE "LSP".
           88  UP1534-VPI-FILE-LSR                    VALUE "LSR".
           88  UP1534-VPI-FILE-LSX                    VALUE "LSX".
           88  UP1534-VPI-FILE-LT                     VALUE "LT".
           88  UP1534-VPI-FILE-LTA                    VALUE "LTA".
           88  UP1534-VPI-FILE-LTB                    VALUE "LTB".
           88  UP1534-VPI-FILE-LTC                    VALUE "LTC".
           88  UP1534-VPI-FILE-LTD                    VALUE "LTD".
           88  UP1534-VPI-FILE-LTF                    VALUE "LTF".
           88  UP1534-VPI-FILE-LTI                    VALUE "LTI".
           88  UP1534-VPI-FILE-LTL                    VALUE "LTL".
           88  UP1534-VPI-FILE-LTP                    VALUE "LTP".
           88  UP1534-VPI-FILE-LTR                    VALUE "LTR".
           88  UP1534-VPI-FILE-LX                     VALUE "LX".
           88  UP1534-VPI-FILE-ML                     VALUE "ML".
           88  UP1534-VPI-FILE-MP                     VALUE "MP".
           88  UP1534-VPI-FILE-NO                     VALUE "NO".
           88  UP1534-VPI-FILE-PA                     VALUE "PA".
           88  UP1534-VPI-FILE-PC                     VALUE "PC".
           88  UP1534-VPI-FILE-PD                     VALUE "PD".
           88  UP1534-VPI-FILE-PE                     VALUE "PE".
           88  UP1534-VPI-FILE-RB                     VALUE "RB".
           88  UP1534-VPI-FILE-RC                     VALUE "RC".
           88  UP1534-VPI-FILE-SE                     VALUE "SE".
           88  UP1534-VPI-FILE-SN                     VALUE "SN".
           88  UP1534-VPI-FILE-SM                     VALUE "SM".
           88  UP1534-VPI-FILE-SP                     VALUE "SP".
           88  UP1534-VPI-FILE-SS                     VALUE "SS".
           88  UP1534-VPI-FILE-ST                     VALUE "ST".
           88  UP1534-VPI-FILE-TY                     VALUE "TY".
           88  UP1534-VPI-FILE-TRWINFO                VALUE "TRWINF".
           88  UP1534-VPI-FILE-TZ                     VALUE "TZ".
           88  UP1534-VPI-FILE-WR                     VALUE "WR".
      *- - - - - - - - - - -{ MISC. }- - - - - - - - - - -{ MISC. }- - -
           88  UP1534-VPI-FILE-WK1                    VALUE "WK1".
           88  UP1534-VPI-FILE-WK2                    VALUE "WK2".
           88  UP1534-VPI-FILE-REPORT                 VALUE "REPORT".

