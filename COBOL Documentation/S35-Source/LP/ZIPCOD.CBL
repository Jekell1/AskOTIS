       IDENTIFICATION DIVISION.
       PROGRAM-ID.   ZIPCOD.
       AUTHOR.       KEC.
       DATE-WRITTEN. 072198.
      ******************************************************************
      *
      *  PROGRAM ID :  LP/ZIPCOD.C
      *  DESCRIPTION:  ZIPCODE INQUIRY REPORT
      *
      *-----------------------------------------------------------------
      * REV:
      *  KEC 072198 ***NEW***  PR#: 00191
      *  BAH 020227 WAS NOT SKIPPING VOIDS AND RESCISSIONS  REGENCY PL# 350
      *   CS 170525 ACTIVATE 8 DIGIT DATES (D1-LOANDATE, WK1-LOANDATE)
      * BAH 181004 GLOBAL BWFILE
      * BAH 181211 GLOBAL LNFILE
      * BAH 20190919 REMOVED ACCESS-CALL ON LNFILE, BWFILE
      * BAH 20200930 ADDED WHERE
      * BAH 2024.0815 ELIMINATE PRINTING OF SSNO #1673 S35Q-118
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       
           SELECT WK1-FILE ASSIGN WK1-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  FILE STATUS FILE-STAT.
       
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
       

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *    W O R K - F I L E - 1
      *
      *-----------------------------------------------------------------
      *    FOR PRINTING DETAIL LINES.
      ******************************************************************
       FD  WK1-FILE
           LABEL RECORDS ARE STANDARD.

      *-----------------------------------------------------------------
       01  WK1-REC.
           03  WK1-KEY.
               05  WK1-BRANCH                   PIC 9(04)    COMP-3.
               05  WK1-ZIPCODE                  PIC X(05).
               05  WK1-ACCTNO                   PIC 9(08)    COMP-3.

           03  WK1-INFORMATION.
               05  WK1-CLASS                    PIC 9(03)    COMP-3.
               05  WK1-NAME                     PIC X(20).
               05  WK1-LOANDATE                 PIC 9(08)    COMP-3.
               05  WK1-LNAMT                    PIC 9(07)V99 COMP-3.
               05  WK1-CURBAL                   PIC 9(07)V99 COMP-3.
               05  WK1-ACCOUNT-STATUS.
                   07  WK1-OPEN                 PIC X(03).
                       88  WK1-OPEN-YES             VALUE "YES".
                       88  WK1-OPEN-NO              VALUE "NO ".

                   07  WK1-PAID                 PIC X(03).
                       88  WK1-PAID-YES             VALUE "YES".
                       88  WK1-PAID-NO              VALUE "NO ".

                   07  WK1-PL                   PIC X(03).
                       88  WK1-PL-YES               VALUE "YES".
                       88  WK1-PL-NO                VALUE "NO ".

                   07  WK1-PL-TYPE              PIC X(02).
                       88  WK1-PL-PL                VALUE "PL".
                       88  WK1-PL-P2                VALUE "P2".
                       88  WK1-PL-P3                VALUE "P3".
                       88  WK1-PL-P4                VALUE "P4".
                       88  WK1-PL-P5                VALUE "P5".
                       88  WK1-PL-NONE              VALUE SPACES.

                   07  WK1-PL-ACTIVITY          PIC X(08).
                       88  WK1-PL-ACTIVITY-ACTIVE   VALUE "ACTIVE  ".
                       88  WK1-PL-ACTIVITY-INACTIVE VALUE "INACTIVE".
                       88  WK1-PL-ACTIVITY-NONE     VALUE SPACES.

      ******************************************************************
      *
      *    W O R K - F I L E - 2
      *
      *-----------------------------------------------------------------
      *    FOR PRINTING TOTALS.
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.

      *-----------------------------------------------------------------
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH             PIC 9(04)    COMP-3.
               05  WK2-ZIPCODE            PIC X(05).

       
           03  WK2-INFORMATION.
               05  WK2-ACCOUNTS           PIC 9(10)    COMP-3.
               05  WK2-LNAMT              PIC 9(10)V99 COMP-3.
               05  WK2-CURBAL             PIC 9(10)V99 COMP-3.
               05  WK2-OPEN               PIC 9(10)    COMP-3.
               05  WK2-OPEN-LNAMT         PIC 9(10)V99 COMP-3.
               05  WK2-OPEN-CURBAL        PIC 9(10)V99 COMP-3.
               05  WK2-PAID               PIC 9(10)    COMP-3.
               05  WK2-PAID-LNAMT         PIC 9(10)V99 COMP-3.
               05  WK2-PAID-CURBAL        PIC 9(10)V99 COMP-3.
               05  WK2-PL                 PIC 9(10)    COMP-3.
               05  WK2-PL-LNAMT           PIC 9(10)V99 COMP-3.
               05  WK2-PL-CURBAL          PIC 9(10)V99 COMP-3.
               05  WK2-PL-TYPE            PIC 9(10)    COMP-3 OCCURS 5.
               05  WK2-PL-TYPE-LNAMT      PIC 9(10)V99 COMP-3 OCCURS 5.
               05  WK2-PL-TYPE-CURBAL     PIC 9(10)V99 COMP-3 OCCURS 5.
               05  WK2-PL-ACTIVITY        PIC 9(10)    COMP-3 OCCURS 2.
               05  WK2-PL-ACTIVITY-LNAMT  PIC 9(10)V99 COMP-3 OCCURS 2.
               05  WK2-PL-ACTIVITY-CURBAL PIC 9(10)V99 COMP-3 OCCURS 2.
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/ZIPCOD S35 03/08/24-15:59:50 >".

      *=================================================================
      *
      *    F I L E - P A T H - W O R K E R S
      *
      *=================================================================
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
      *-----------------------------------------------------------------
       01  WK1-IFN                     PIC X(3)            VALUE "WK1".
       01  WK2-IFN                     PIC X(3)            VALUE "WK1".

      *-----------------------------------------------------------------
       01  WK1-PATH                    PIC X(35)           VALUE SPACES.
       01  WK2-PATH                    PIC X(35)           VALUE SPACES.

      *=================================================================
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      *=================================================================
       01  ACCOUNT-STATUS.
           03  ACCOUNT-STATUS-OPEN         PIC X(03)  VALUE SPACES.
               88  ACCOUNT-STATUS-OPEN-YES            VALUE "YES".
               88  ACCOUNT-STATUS-OPEN-NO             VALUE "NO ".

           03  ACCOUNT-STATUS-PAID         PIC X(03)  VALUE SPACES.
               88  ACCOUNT-STATUS-PAID-YES            VALUE "YES".
               88  ACCOUNT-STATUS-PAID-NO             VALUE "NO ".

           03  ACCOUNT-STATUS-PL           PIC X(03)  VALUE SPACES.
               88  ACCOUNT-STATUS-PL-YES              VALUE "YES".
               88  ACCOUNT-STATUS-PL-NO               VALUE "NO ".

           03  ACCOUNT-STATUS-PL-TYPE      PIC X(02)  VALUE SPACES.
               88  ACCOUNT-STATUS-PL-PL               VALUE "PL".
               88  ACCOUNT-STATUS-PL-P2               VALUE "P2".
               88  ACCOUNT-STATUS-PL-P3               VALUE "P3".
               88  ACCOUNT-STATUS-PL-P4               VALUE "P4".
               88  ACCOUNT-STATUS-PL-P5               VALUE "P5".
               88  ACCOUNT-STATUS-PL-NONE             VALUE SPACES.

           03  ACCOUNT-STATUS-PL-ACTIVITY  PIC X(08)  VALUE SPACES.
               88  ACCOUNT-STATUS-PL-ACTIVE           VALUE "ACTIVE  ".
               88  ACCOUNT-STATUS-PL-INACTIVE         VALUE "INACTIVE".
               88  ACCOUNT-STATUS-PL-NONE             VALUE SPACES.

      *-----------------------------------------------------------------
       01  CURRENT-INFORMATION.
           03  CURRENT-BRANCH              PIC 9(04)  VALUE ZEROES.
           03  CURRENT-ZIPCODE             PIC X(05)  VALUE SPACES.

       01  FIRST-TIME                      PIC X(01)  VALUE "Y".

       01  HOLD-WK2-KEY.
           03  HOLD-WK2-BRANCH             PIC 9(04)  VALUE ZEROES.
           03  HOLD-WK2-ZIPCODE            PIC X(05)  VALUE SPACES.

       01  LEGEND                          PIC X(80).
       01  LEGEND-LIST                     PIC X(07)  VALUE "LEGEND.".

       01  PRINT-TYPE                      PIC X(01)  VALUE SPACES.
           88  PRINT-TYPE-DETAIL                      VALUE "D".
           88  PRINT-TYPE-ZIPCODE                     VALUE "Z".
           88  PRINT-TYPE-TOTALS                      VALUE "T".

       01  RANGE-ENTRY-BEG-END             PIC X(03).
           88  RANGE-ENTRY-BEGINNING                  VALUE "BEG".
           88  RANGE-ENTRY-ENDING                     VALUE "END".

       01  RECORDS-FOUND                   PIC X(01)  VALUE "X".
           88  NO-RECORDS-FOUND                       VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION                VALUE "X".

       01  SUB                             PIC 9(02)  COMP-3.
       01  SUPPRESS-ZIPCODE                PIC X(01)  VALUE "N".
           88  SUPPRESS-ZIPCODE-YES                   VALUE "Y".
           88  SUPPRESS-ZIPCODE-NO                    VALUE "N".

      *=================================================================
      *
      *    T O T A L S - W O R K E R S
      *
      *=================================================================
       01  TT-TOTALS.
           03  TT-ACCOUNTS                 PIC 9(10).
           03  TT-LNAMT                    PIC 9(10)V99.
           03  TT-CURBAL                   PIC 9(10)V99.
           03  TT-OPEN                     PIC 9(10).
           03  TT-OPEN-LNAMT               PIC 9(10)V99.
           03  TT-OPEN-CURBAL              PIC 9(10)V99.
           03  TT-PAID                     PIC 9(10).
           03  TT-PAID-LNAMT               PIC 9(10)V99.
           03  TT-PAID-CURBAL              PIC 9(10)V99.
           03  TT-PL                       PIC 9(10).
           03  TT-PL-LNAMT                 PIC 9(10)V99.
           03  TT-PL-CURBAL                PIC 9(10)V99.
           03  TT-PL-TYPE                  PIC 9(10)      OCCURS 5.
           03  TT-PL-TYPE-LNAMT            PIC 9(10)V99   OCCURS 5.
           03  TT-PL-TYPE-CURBAL           PIC 9(10)V99   OCCURS 5.
           03  TT-PL-ACTIVITY              PIC 9(10)      OCCURS 2.
           03  TT-PL-ACTIVITY-LNAMT        PIC 9(10)V99   OCCURS 2.
           03  TT-PL-ACTIVITY-CURBAL       PIC 9(10)V99   OCCURS 2.

      ******************************************************************
      *
      *    S C R E E N - R A N G E - F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH                    PIC 9(04).
           03  END-BRANCH                    PIC 9(04).

           03  BEG-CLASS                     PIC 9(02).
           03  END-CLASS                     PIC 9(02).

           03  OPEN-ISO                      PIC X(01).
               88  OPEN-ISO-INCLUDE                        VALUE "I".
               88  OPEN-ISO-SKIP                           VALUE "S".
               88  OPEN-ISO-ONLY                           VALUE "O".

           03  PAID-ISO                      PIC X(01).
               88  PAID-ISO-INCLUDE                        VALUE "I".
               88  PAID-ISO-SKIP                           VALUE "S".
               88  PAID-ISO-ONLY                           VALUE "O".

           03  PL-ISO                        PIC X(01).
               88  PL-ISO-INCLUDE                          VALUE "I".
               88  PL-ISO-SKIP                             VALUE "S".
               88  PL-ISO-ONLY                             VALUE "O".

           03  PL-TYPE                       PIC X(01).
               88  PL-TYPE-ALL                             VALUE "A".
               88  PL-TYPE-PL                              VALUE "P".
               88  PL-TYPE-P2                              VALUE "2".
               88  PL-TYPE-P3                              VALUE "3".
               88  PL-TYPE-P4                              VALUE "4".
               88  PL-TYPE-P5                              VALUE "5".

           03  PL-ACTIVITY                   PIC X(01).
               88  PL-ACTIVITY-INACTIVE                    VALUE "I".
               88  PL-ACTIVITY-ACTIVE                      VALUE "A".
               88  PL-ACTIVITY-BOTH                        VALUE "B".

           03  PRINT-OPTIONS.
               05  PRINT-DETAIL              PIC X(01).
                   88  PRINT-DETAIL-YES                    VALUE "Y".
                   88  PRINT-DETAIL-NO                     VALUE "N".

               05  PRINT-ZIPCOD              PIC X(01).
                   88  PRINT-ZIPCOD-YES                    VALUE "Y".
                   88  PRINT-ZIPCOD-NO                     VALUE "N".

               05  PRINT-BRANCH              PIC X(01).
                   88  PRINT-BRANCH-YES                    VALUE "Y".
                   88  PRINT-BRANCH-NO                     VALUE "N".

               05  PRINT-CORP                PIC X(01).
                   88  PRINT-CORP-YES                      VALUE "Y".
                   88  PRINT-CORP-NO                       VALUE "N".

           03  GROUP-OPTIONS.
               05  GROUP-FLAG                PIC X(01).
                   88  GROUP-FLAG-YES                      VALUE "Y".
                   88  GROUP-FLAG-NO                       VALUE "N".

               05  ENT-GROUP                 PIC X(04).

               05  LOCAL-PRINT-FG            PIC X(01).
                   88  LOCAL-PRINT-YES                     VALUE "Y".
                   88  LOCAL-PRINT-NO                      VALUE "N".

      *-----------------------------------------------------------------
       01  VERSION-CONTROL                   PIC X(01)     VALUE "B".

       01  SAVE-BEG-BRANCH               PIC 9(04).
       01  SAVE-END-BRANCH               PIC 9(04).
       01  SAVE-GROUP-FLAG           PIC X(01).
       01  SAVE-ENT-GROUP            PIC X(04).

      ******************************************************************
      *
      *    R E P O R T - P R I N T - W O R K E R S
      *
      ******************************************************************
      *
      *    L I N E - W O R K E R S
      *
      *=================================================================
       01  LN-WORKERS.
           03  LN-FEED                       PIC 9(03)     VALUE ZEROES.
           03  LN-COUNT                      PIC 9(03)     VALUE 99.
           03  LN-WK                         PIC 9(03)     VALUE ZEROES.

       01  ER-FEED                           PIC 9(01)     VALUE 1.
       01  PAGE-NUMBER                       PIC 9(04)     VALUE ZEROES.

      *=================================================================
      *
      *    H E A D E R - B U F F E R S
      *
      *=================================================================
       01  HEADER-LINE-1.
           03  H1-DATE              PIC X(08)   VALUE SPACES.
           03  FILLER               PIC X(02)   VALUE SPACES.
           03  H1-TIME              PIC X(08)   VALUE SPACES.
           03  FILLER               PIC X(28)   VALUE SPACES.
           03  H1-NAME              PIC X(40)   VALUE SPACES.
           03  FILLER               PIC X(37)   VALUE SPACES.
           03  FILLER               PIC X(05)   VALUE "PAGE".
           03  H1-PAGE-NO           PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEADER-LINE-2.
           03  FILLER               PIC X(11)   VALUE "USER ID  : ".
           03  H2-USERID            PIC X(10)   VALUE SPACES.
           03  FILLER               PIC X(28)   VALUE SPACES.
           03  FILLER               PIC X(34)   VALUE
               "----- ZIPCODE INQUIRY REPORT -----".
           03  FILLER               PIC X(49)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-3.
           03  FILLER               PIC X(11)   VALUE "BRANCH   : ".
           03  H3-BRANCH            PIC 9(04)   VALUE ZEROES.
           03  FILLER               PIC X(08)   VALUE SPACES.
           03  H3-BRANCH-DESC       PIC X(40)   VALUE SPACES.
           03  FILLER               PIC X(69)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-DETAIL-1.
           03  FILLER PIC X(30) VALUE "             ACCOUNT   CLASS  ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(30) VALUE "         LOAN           LOAN  ".
           03  FILLER PIC X(30) VALUE "    CURRENT  -----ACCOUNT INFO".
           03  FILLER PIC X(12) VALUE "RMATION-----".

      *-----------------------------------------------------------------
       01  HEADER-LINE-DETAIL-2.
           03  FILLER PIC X(30) VALUE "ZIPCODE      NUMBER    NUMBER ".
           03  FILLER PIC X(30) VALUE "              NAME (LAST, FIRS".
           03  FILLER PIC X(30) VALUE "T)       DATE         AMOUNT  ".
           03  FILLER PIC X(30) VALUE "    BALANCE  OPEN PAID  P&L TY".
           03  FILLER PIC X(12) VALUE "PE  ACTIVITY".

      *-----------------------------------------------------------------
       01  HEADER-LINE-TOTALS-1.
           03  FILLER PIC X(30) VALUE "ZIPCODE             OPEN      ".
           03  FILLER PIC X(30) VALUE "    PAID         P & L        ".
           03  FILLER PIC X(30) VALUE "ACTIVE      INACTIVE          ".
           03  FILLER PIC X(30) VALUE "TOTAL       LOAN AMOUNT   CURR".
           03  FILLER PIC X(12) VALUE "ENT BALANCE ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-ZIPCODE-1.
           03  FILLER PIC X(30) VALUE "--ZIPCODE--         ----------".
           03  FILLER PIC X(30) VALUE "------------------------------".
           03  FILLER PIC X(30) VALUE "ACCOUNT TYPES-----------------".
           03  FILLER PIC X(30) VALUE "----------------------        ".
           03  FILLER PIC X(12) VALUE "  TOTAL OF  ".

      *-----------------------------------------------------------------
       01  HEADER-TOTALS-TITLE.
           03  FILLER  PIC X(30) VALUE SPACES.
           03  FILLER  PIC X(30) VALUE "- - - - - - - - - - - - - - - ".
           03  FILLER  PIC X(11) VALUE "T O T A L S".
           03  FILLER  PIC X(30) VALUE " - - - - - - - - - - - - - - -".
           03  FILLER  PIC X(31) VALUE SPACES.

      *=================================================================
      *
      *    D E T A I L - L I N E - B U F F E R
      *
      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-1            REDEFINES DETAIL-LINE.
           03  D1-ZIPCODE                     PIC X(05).
           03  FILLER                         PIC X(08).
           03  D1-ACCOUNT                     PIC ZZZZZZZ9.  
           03  FILLER                         PIC X(03).
           03  D1-CLASS                       PIC Z9.
           03  FILLER                         PIC X(17).
           03  D1-NAME                        PIC X(20).
           03  FILLER                         PIC X(03).
           03  D1-LOANDATE                    PIC 99/99/99.
           03  FILLER                         PIC X(03).
           03  D1-LNAMT                       PIC ZZZ,ZZ9.99.
           03  FILLER                         PIC X(03).
           03  D1-CURBAL                      PIC ZZZ,ZZ9.99.
           03  FILLER                         PIC X(02).
           03  D1-OPEN                        PIC X(03).
           03  FILLER                         PIC X(02).
           03  D1-PAID                        PIC X(03).
           03  FILLER                         PIC X(02).
           03  D1-PL                          PIC X(03).
           03  FILLER                         PIC X(02).
           03  D1-PL-TYPE                     PIC X(03).
           03  FILLER                         PIC X(02).
           03  D1-PL-ACTIVITY                 PIC X(08).

      *-----------------------------------------------------------------
       01  TOTALS-LINE-1            REDEFINES DETAIL-LINE.
           03  T1-ZIPCODE                     PIC X(05).
           03  FILLER                         PIC X(08).
           03  T1-OPEN                        PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                         PIC X(03).
           03  T1-PAID                        PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                         PIC X(03).
           03  T1-PL                          PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                         PIC X(03).
           03  T1-PL-ACTIVE                   PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                         PIC X(03).
           03  T1-PL-INACTIVE                 PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                         PIC X(04).
           03  T1-ACCOUNTS                    PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                         PIC X(04).
           03  T1-LNAMT                       PIC ZZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(04).
           03  T1-CURBAL                      PIC ZZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).

      *-----------------------------------------------------------------
       01  RANGE-LINE               REDEFINES DETAIL-LINE.
           03  R1-DESC                        PIC X(34).
           03  R1-FIELD                       PIC X(10).
           03  R1-NUMERIC           REDEFINES R1-FIELD
                                              PIC Z(09)9.
           03  FILLER                         PIC X(88).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/ZIPCOD_CMD.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/ZIPCOD_DEF.CPY".
       COPY "LIBLP/ZIPCOD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/ZIPCOD_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK1-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK1-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES      ) OR
              ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-MAIN-PROGRAM.

           PERFORM PROCESS-INFORMATION.

       END-MAIN-PROGRAM.

           IF ( NO-RECORDS-FOUND )
              GO TO EXIT-MAIN-PROGRAM.

           PERFORM PRINT-CORP-TOTALS.
           PERFORM PRINT-RANGES.

       EXIT-MAIN-PROGRAM.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

      D    STOP MESS.
           MOVE "X" TO RECORDS-FOUND.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ZIPCOD"      TO FORM-PROG VDU-FORM-NAME.

           MOVE ZIPCOD-QUEUE-POS TO Q-POS.
           MOVE ZIPCOD-COPIES-POS TO C-POS.
           MOVE ZIPCOD-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
      *-----------------------------------------------------------------
       COPY "LIBGB/EOINIT.CPY".
      *-----------------------------------------------------------------
       MAKING-TEMP-FILES.

      D    STOP MESS.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO WK1-PATH.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.

      *-----------------------------------------------------------------
       SETUP-HEADER.

           MOVE EXT-CONAME-CONAME  TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE TO H1-DATE.
           MOVE EXT-CONAME-USER-ID TO H2-USERID.

       CONTINUE-INITIALIZATION.

           IF EXT-ROUTE-BUF = "00"
              MOVE LEGEND-BEGIN-PROCESSING TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH TO SAVE-BEG-BRANCH
              MOVE END-BRANCH TO SAVE-END-BRANCH
              MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                           END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH         TO SAVE-BEG-BRANCH
              MOVE END-BRANCH         TO SAVE-END-BRANCH
              MOVE GROUP-FLAG         TO SAVE-GROUP-FLAG
              MOVE ENT-GROUP          TO SAVE-ENT-GROUP
              MOVE EXT-EOBUF-GROUP        TO ENT-GROUP
              MOVE ZEROES             TO BEG-BRANCH
              MOVE 9999               TO END-BRANCH
              MOVE "Y"                TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

           MOVE ZEROES TO PAGE-NUMBER.
           MOVE "N"    TO RECORDS-FOUND.

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           GO TO EXIT-INITIALIZATION.

       END-INITIALIZATION.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZATION.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
      
      D    STOP MESS.     
           IF ( UP-KEY ) GO TO ENTER-PRINT-CORP.

       ENTER-BEG-BRANCH.
           MOVE "BEG" TO RANGE-ENTRY-BEG-END.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF ( F6-KEY )
              PERFORM SCAN-GR-FILE
              IF ( IO-BAD )
                 GO TO ENTER-BEG-BRANCH.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

       
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
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
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"                  TO GROUP-FLAG.
           MOVE WGR-SRESULT          TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE "END" TO RANGE-ENTRY-BEG-END.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-BRANCH.

           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES                 TO VDUBUF

           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE "USER BRANCH ONLY" TO VDUBUF.

           PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

       ENTER-ALL-BRANCH.

           MOVE "N"                  TO GROUP-FLAG.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" )
              GO TO ENTER-BEG-BRANCH.

       ENTER-BEG-CLASS.

           MOVE "BEG" TO RANGE-ENTRY-BEG-END.

           MOVE LEGEND-BEG-CLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-END-BRANCH.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF ( F6-KEY )
              PERFORM SCAN-LC-FILE
              IF ( IO-BAD )
                 GO TO ENTER-BEG-CLASS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-CLASS.

           MOVE VDUNUM0 TO BEG-CLASS.

       ENTER-END-CLASS.

           MOVE "END" TO RANGE-ENTRY-BEG-END.

           MOVE LEGEND-END-CLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF ( F6-KEY )
              PERFORM SCAN-LC-FILE
              IF ( IO-BAD )
                 GO TO ENTER-END-CLASS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-CLASS.

       
           MOVE VDUNUM0 TO END-CLASS.

           IF (( BEG-CLASS = ZEROES    )  AND
               ( END-CLASS = ZEROES    )) OR
               ( BEG-CLASS > END-CLASS )
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-OPEN-ISO.

       ENTER-ALL-CLASS.

           MOVE "SNNN020BEG-CLASS." TO VDU.
           MOVE ZEROES              TO VDUNUM0 BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS." TO VDU.
           MOVE 99                  TO VDUNUM0 END-CLASS.
           PERFORM SCREENIO.

       ENTER-OPEN-ISO.

           MOVE LEGEND-OPEN-ISO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPEN-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-END-CLASS.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-OPEN-ISO.

       
           MOVE VDUBUF TO OPEN-ISO.

           IF ( NOT OPEN-ISO-INCLUDE ) AND
              ( NOT OPEN-ISO-SKIP    ) AND
              ( NOT OPEN-ISO-ONLY    )
              MOVE MESSAGE-INVALID-OPEN-ISO TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-OPEN-ISO.

       ENTER-PAID-ISO.

           MOVE LEGEND-PAID-ISO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PAID-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-OPEN-ISO.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PAID-ISO.

       
           MOVE VDUBUF TO PAID-ISO.

           IF ( NOT PAID-ISO-INCLUDE ) AND
              ( NOT PAID-ISO-SKIP    ) AND
              ( NOT PAID-ISO-ONLY    )
              MOVE MESSAGE-INVALID-PAID-ISO TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PAID-ISO.

       ENTER-PL-ISO.

           MOVE LEGEND-PL-ISO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PL-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-PAID-ISO.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PL-ISO.
       
           MOVE VDUBUF TO PL-ISO.

           IF ( NOT PL-ISO-INCLUDE ) AND
              ( NOT PL-ISO-SKIP    ) AND
              ( NOT PL-ISO-ONLY    )
              MOVE MESSAGE-INVALID-PL-ISO TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PL-ISO.

           IF ( PL-ISO-SKIP )
              MOVE "S  A010PL-TYPE." TO VDU
              MOVE "A"               TO VDUBUF PL-TYPE
              PERFORM SCREENIO
              MOVE "S  A010PL-ACTIVE." TO VDU
              MOVE "B"                 TO VDUBUF PL-ACTIVITY
              PERFORM SCREENIO
              GO TO ENTER-PRINT-DETAIL.

       ENTER-PL-TYPE.

           MOVE LEGEND-PL-TYPE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PL-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-PL-ISO.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PL-TYPE.
       
           MOVE VDUBUF TO PL-TYPE.

           IF ( NOT PL-TYPE-ALL ) AND
              ( NOT PL-TYPE-PL  ) AND
              ( NOT PL-TYPE-P2  ) AND
              ( NOT PL-TYPE-P3  ) AND
              ( NOT PL-TYPE-P4  ) AND
              ( NOT PL-TYPE-P5  )
              MOVE MESSAGE-INVALID-PL-TYPE TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PL-TYPE.

       ENTER-PL-ACTIVE.

           MOVE LEGEND-PL-ACTIVE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PL-ACTIVE." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-PL-TYPE.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PL-ACTIVE.

       
           MOVE VDUBUF TO PL-ACTIVITY.

           IF ( NOT PL-ACTIVITY-INACTIVE ) AND
              ( NOT PL-ACTIVITY-ACTIVE   ) AND
              ( NOT PL-ACTIVITY-BOTH     )
              MOVE MESSAGE-INVALID-PL-ACTIVE TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PL-ACTIVE.

       ENTER-PRINT-DETAIL.

           MOVE LEGEND-PRINT-DETAIL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRINT-DETAIL." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY )
              IF ( PL-ISO-SKIP )
                 GO TO ENTER-PL-ISO
              ELSE
                 GO TO ENTER-PL-ACTIVE.

           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PRINT-DETAIL.

       
           MOVE VDUBUF TO PRINT-DETAIL.

           IF ( NOT PRINT-DETAIL-YES ) AND
              ( NOT PRINT-DETAIL-NO  )
              MOVE MESSAGE-INVALID-PRINT-DETAIL TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-DETAIL.

           IF ( PRINT-DETAIL-NO )
              MOVE "S  A010PRINT-ZIPCOD." TO VDU
              MOVE "N"                    TO VDUBUF PRINT-ZIPCOD
              PERFORM SCREENIO
              GO TO ENTER-PRINT-BRANCH.

       ENTER-PRINT-ZIPCOD.

           MOVE LEGEND-PRINT-ZIPCOD TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRINT-ZIPCOD." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-PRINT-DETAIL
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PRINT-ZIPCOD.

       
           MOVE VDUBUF TO PRINT-ZIPCOD.

           IF ( NOT PRINT-ZIPCOD-YES ) AND
              ( NOT PRINT-ZIPCOD-NO  )
              MOVE MESSAGE-INVALID-PRINT-ZIPCOD TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-ZIPCOD.

       ENTER-PRINT-BRANCH.

           MOVE LEGEND-PRINT-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRINT-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY )
              IF ( PRINT-DETAIL-YES )
                 GO TO ENTER-PRINT-ZIPCOD
              ELSE
                 GO TO ENTER-PRINT-DETAIL.

           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PRINT-BRANCH.

       
           MOVE VDUBUF TO PRINT-BRANCH.

           IF ( NOT PRINT-BRANCH-YES ) AND
              ( NOT PRINT-BRANCH-NO  )
              MOVE MESSAGE-INVALID-PRINT-BRANCH TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-BRANCH.

       ENTER-PRINT-CORP.

           MOVE LEGEND-PRINT-CORP TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRINT-CORP." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-PRINT-BRANCH.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PRINT-CORP.

       
           MOVE VDUBUF TO PRINT-CORP.

           IF ( NOT PRINT-CORP-YES ) AND
              ( NOT PRINT-CORP-NO  )
              MOVE MESSAGE-INVALID-PRINT-CORP TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-CORP.

       CHECK-FOR-LOCAL-PRINT.

           IF ( EXT-ROUTE-BUF NOT = "40" ) AND
              ( EXT-ROUTE-BUF NOT = "50" )
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-ENTER-PARAMETERS.

           IF ( BEG-BRANCH = END-BRANCH )
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-ENTER-PARAMETERS.

       ENTER-LOCAL-PRINT-FG.

           MOVE LEGEND-ENTER-LOCAL-PRINT-FG TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A250SPOOL-PROMPT."      TO VDU.
           MOVE "SPOOL TO LOCAL?  (Y/N)  :" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-PRINT-CORP.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-LOCAL-PRINT-FG.

       
           MOVE VDUBUF TO LOCAL-PRINT-FG.

           IF ( LOCAL-PRINT-FG NOT = "Y" ) AND
              ( LOCAL-PRINT-FG NOT = "N" )
              GO TO ENTER-LOCAL-PRINT-FG.

       END-ENTER-PARAMETERS.

           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

      * A CHECK TO SEE IF LOCAL-SPOOLING AND CORPORATE TOTALS ARE
      * BOTH ON.  CORPORATE TOTALS CANNOT PRINT IF THERE IS LOCAL-SP.

           IF ( LOCAL-PRINT-FG = "Y" ) AND ( PRINT-CORP-YES )
              MOVE "S  A010PRINT-CORP." TO VDU
              MOVE "N"                  TO VDUBUF PRINT-CORP
              PERFORM SCREENIO
              MOVE MESSAGE-INVALID-CORP-LOCAL TO MESS
              PERFORM SEND-MESS.

      * CHECKING TO SEE IF THERE IS ANY TYPE OF PRINTING.
      * IF NOT, THEY MUST RE-DO THE SETUP.

           IF ( PRINT-DETAIL-NO ) AND
              ( PRINT-ZIPCOD-NO ) AND
              ( PRINT-BRANCH-NO ) AND
              ( PRINT-CORP-NO   )
              MOVE MESSAGE-INVALID-NO-PRINTING TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-DETAIL.

       EXIT-ENTER-PARAMETERS.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF ( GROUP-FLAG-YES )
              MOVE "S  A000BEG-BRANCH." TO VDU
              MOVE ENT-GROUP            TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH." TO VDU
              MOVE BEG-BRANCH           TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH." TO VDU
              MOVE END-BRANCH           TO VDUNUM0
              PERFORM SCREENIO.

       
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY"     TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO.

       
           MOVE "SNNN020BEG-CLASS." TO VDU.
           MOVE BEG-CLASS           TO VDUNUM0.
           PERFORM SCREENIO.

       
           MOVE "SNNN020END-CLASS." TO VDU.
           MOVE END-CLASS           TO VDUNUM0.
           PERFORM SCREENIO.

       
           MOVE "S  A010OPEN-ISO." TO VDU.
           MOVE OPEN-ISO           TO VDUBUF.
           PERFORM SCREENIO.

       
           MOVE "S  A010PAID-ISO." TO VDU.
           MOVE PAID-ISO           TO VDUBUF.
           PERFORM SCREENIO.

       
           MOVE "S  A010PL-ISO." TO VDU.
           MOVE PL-ISO           TO VDUBUF.
           PERFORM SCREENIO.

       
           MOVE "S  A010PL-TYPE." TO VDU.
           MOVE PL-TYPE          TO VDUBUF.
           PERFORM SCREENIO.

       
           MOVE "S  A010PL-ACTIVE." TO VDU.
           MOVE PL-ACTIVITY         TO VDUBUF.
           PERFORM SCREENIO.

       
           MOVE "S  A010PRINT-DETAIL." TO VDU.
           MOVE PRINT-DETAIL           TO VDUBUF.
           PERFORM SCREENIO.

       
           MOVE "S  A010PRINT-ZIPCOD." TO VDU.
           MOVE PRINT-ZIPCOD           TO VDUBUF.
           PERFORM SCREENIO.

       
           MOVE "S  A010PRINT-BRANCH." TO VDU.
           MOVE PRINT-BRANCH           TO VDUBUF.
           PERFORM SCREENIO.

       
           MOVE "S  A010PRINT-CORP." TO VDU.
           MOVE PRINT-CORP           TO VDUBUF.
           PERFORM SCREENIO.

       EXIT-DISPLAY-PARAMETERS.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       PROCESS-INFORMATION SECTION.
      ******************************************************************
      D    STOP MESS.
           IF ( GROUP-FLAG-NO )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-RECORD.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-RECORD.

           IF ( GROUP-FLAG-NO )
              GO TO NEXT-BRANCH-RECORD.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO EXIT-PROCESS-INFORMATION.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-RECORD.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD         ) OR
              ( BR-NO > END-BRANCH )
              GO TO EXIT-PROCESS-INFORMATION.

       MOVE-BRANCH-PATH-INFO.

           IF ( LOCAL-PRINT-YES )
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF ( NOT STAT-VALID )
                 GO TO NEXT-GROUP-RECORD.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM COMPILE-INFORMATION.

           PERFORM PRINT-INFORMATION.

           IF ( LOCAL-PRINT-FG = "Y" )
              MOVE ZEROES TO PAGE-NUMBER.

           GO TO NEXT-GROUP-RECORD.

       EXIT-PROCESS-INFORMATION.
           EXIT.

      ******************************************************************
       COMPILE-INFORMATION SECTION.
      ******************************************************************

           MOVE BR-NO  TO CURRENT-BRANCH.
           PERFORM SET-HEADER-LINE-BRANCH.

           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.
           PERFORM OPEN-WK2-FILE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.


           PERFORM START-LN1-FILE.

       NEXT-LN-RECORD.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-COMPILE-INFORMATION.

           IF LN-POCD-NONLOAN
              GO TO NEXT-LN-RECORD.

           PERFORM SETUP-ACCOUNT-STATUS.

           IF ( LN-CLASS < BEG-CLASS ) OR ( LN-CLASS > END-CLASS )
              GO TO NEXT-LN-RECORD.
       
           IF ( OPEN-ISO-ONLY          ) AND
              ( ACCOUNT-STATUS-OPEN-NO )
              GO TO NEXT-LN-RECORD.

           IF ( OPEN-ISO-SKIP           ) AND
              ( ACCOUNT-STATUS-OPEN-YES )
              GO TO NEXT-LN-RECORD.

           IF ( PAID-ISO-ONLY          ) AND
              ( ACCOUNT-STATUS-PAID-NO )
              GO TO NEXT-LN-RECORD.

           IF ( PAID-ISO-SKIP           ) AND
              ( ACCOUNT-STATUS-PAID-YES )
              GO TO NEXT-LN-RECORD.

           IF ( PL-ISO-ONLY          ) AND
              ( ACCOUNT-STATUS-PL-NO )
              GO TO NEXT-LN-RECORD.

           IF ( PL-ISO-SKIP           ) AND
              ( ACCOUNT-STATUS-PL-YES )
              GO TO NEXT-LN-RECORD.

           IF ( PL-TYPE-PL               ) AND
              ( NOT ACCOUNT-STATUS-PL-PL )
              GO TO NEXT-LN-RECORD.

           IF ( PL-TYPE-P2               ) AND
              ( NOT ACCOUNT-STATUS-PL-P2 )
              GO TO NEXT-LN-RECORD.

           IF ( PL-TYPE-P3               ) AND
              ( NOT ACCOUNT-STATUS-PL-P3 )
              GO TO NEXT-LN-RECORD.

           IF ( PL-TYPE-P4               ) AND
              ( NOT ACCOUNT-STATUS-PL-P4 )
              GO TO NEXT-LN-RECORD.

           IF ( PL-TYPE-P5               ) AND
              ( NOT ACCOUNT-STATUS-PL-P5 )
              GO TO NEXT-LN-RECORD.

       
           IF ( PL-ACTIVITY-ACTIVE         ) AND
              ( ACCOUNT-STATUS-PL-INACTIVE )
              GO TO NEXT-LN-RECORD.

           IF ( PL-ACTIVITY-INACTIVE     ) AND
              ( ACCOUNT-STATUS-PL-ACTIVE )
              GO TO NEXT-LN-RECORD.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF ( IO-BAD )
              GO TO NEXT-LN-RECORD.

      * 'LN' RECORD IS VALID; PROCESS RECORD!!!

           MOVE SPACES                         TO WK1-REC.
           MOVE BR-NO                          TO WK1-BRANCH.

           IF ( BW-ZIP = SPACES )
              MOVE "*N/A*     "                TO BW-ZIP.

           MOVE BW-ZIP                         TO WK1-ZIPCODE.
           MOVE LN-ACCTNO                      TO WK1-ACCTNO.
           MOVE LN-CLASS                       TO WK1-CLASS.

           STRING BW-LNAME,", ",BW-FNAME
                  DELIMITED BY "   "          INTO WK1-NAME.

           MOVE LN-LOANDATE                    TO WK1-LOANDATE.
           MOVE LN-LNAMT                       TO WK1-LNAMT.
           MOVE LN-CURBAL                      TO WK1-CURBAL.
           MOVE ACCOUNT-STATUS-OPEN            TO WK1-OPEN.
           MOVE ACCOUNT-STATUS-PAID            TO WK1-PAID.
           MOVE ACCOUNT-STATUS-PL              TO WK1-PL.
           MOVE ACCOUNT-STATUS-PL-TYPE         TO WK1-PL-TYPE.
           MOVE ACCOUNT-STATUS-PL-ACTIVITY     TO WK1-PL-ACTIVITY.

           PERFORM WRITE-WK1-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

       END-OF-PROCESSING-LN-REC.

           GO TO NEXT-LN-RECORD.

       END-COMPILE-INFORMATION.

           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-WK1-FILE.
           PERFORM CLOSE-WK2-FILE.

       EXIT-COMPILE-INFORMATION.
           EXIT.

      ******************************************************************
       SETUP-ACCOUNT-STATUS SECTION.
      ******************************************************************

           MOVE SPACES TO ACCOUNT-STATUS.
       
           IF ( LN-CURBAL NOT = 0 OR LN-INTBAL NOT = 0 OR
                 LN-LCBAL NOT = 0 OR LN-OTHBAL NOT = 0 )
              MOVE "YES" TO ACCOUNT-STATUS-OPEN
           ELSE
              MOVE "NO " TO ACCOUNT-STATUS-OPEN.

       
           IF ( LN-CURBAL = 0 AND LN-INTBAL = 0 AND LN-LCBAL = 0
                               AND LN-OTHBAL = 0 )
              MOVE "YES" TO ACCOUNT-STATUS-PAID
           ELSE
              MOVE "NO " TO ACCOUNT-STATUS-PAID.

       
           IF ( LN-PLDATE NOT = ZEROES )
              MOVE "YES" TO ACCOUNT-STATUS-PL
           ELSE
              MOVE "NO " TO ACCOUNT-STATUS-PL.

           IF ( ACCOUNT-STATUS-PL-NO )
              MOVE SPACES TO ACCOUNT-STATUS-PL-TYPE
              MOVE SPACES TO ACCOUNT-STATUS-PL-ACTIVITY
              GO TO EXIT-SETUP-ACCOUNT-STATUS.

       
           IF ( LN-PLTYPE = "P" )
              MOVE "PL" TO ACCOUNT-STATUS-PL-TYPE
           ELSE
           IF ( LN-PLTYPE = "2" )
              MOVE "P2" TO ACCOUNT-STATUS-PL-TYPE
           ELSE
           IF ( LN-PLTYPE = "3" )
              MOVE "P3" TO ACCOUNT-STATUS-PL-TYPE
           ELSE
           IF ( LN-PLTYPE = "4" )
              MOVE "P4" TO ACCOUNT-STATUS-PL-TYPE
           ELSE
              MOVE "P5" TO ACCOUNT-STATUS-PL-TYPE.

       
           IF ( LN-PLCD = "P" )
              MOVE "ACTIVE  " TO ACCOUNT-STATUS-PL-ACTIVITY
           ELSE
              MOVE "INACTIVE" TO ACCOUNT-STATUS-PL-ACTIVITY.

       EXIT-SETUP-ACCOUNT-STATUS.
           EXIT.

      ******************************************************************
       PRINT-INFORMATION SECTION.
      ******************************************************************

           MOVE "Y"            TO FIRST-TIME.
           MOVE 99             TO LN-COUNT.
           MOVE SPACES         TO WK1-ZIPCODE.
           MOVE CURRENT-BRANCH TO WK1-BRANCH.
           MOVE ZEROES         TO WK1-ACCTNO.

           PERFORM OPEN-WK1-FILE.
           PERFORM OPEN-WK2-FILE.
           PERFORM START-WK1-FILE.

       NEXT-WK1-RECORD.

           PERFORM READ-WK1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO END-PRINT-INFORMATION.

      * CHECKING FOR FIRST TIME IN THIS SECTION.

           IF ( FIRST-TIME = "N" )
              GO TO PRT-INFO-CHECK-ZIPCODE.
       
           MOVE "Y"             TO RECORDS-FOUND.
           MOVE "N"             TO FIRST-TIME.
           MOVE "N"             TO SUPPRESS-ZIPCODE.
           MOVE ZEROES          TO TT-TOTALS.
           MOVE WK1-ZIPCODE     TO CURRENT-ZIPCODE.

       PRT-INFO-CHECK-ZIPCODE.

      * CHECK FOR ZIPCODE CHANGE.

           IF ( WK1-ZIPCODE = CURRENT-ZIPCODE )
              GO TO PRINTING-DETAIL-ANALYSIS.

           PERFORM PRINT-ZIPCODE-TOTALS.
           PERFORM UPDATE-WK2-TOTALS.
           MOVE ZEROES      TO TT-TOTALS.
           MOVE WK1-ZIPCODE TO CURRENT-ZIPCODE.
           MOVE "N"         TO SUPPRESS-ZIPCODE.

       PRINTING-DETAIL-ANALYSIS.

      * PRINTING DETAIL ANALYSIS LINES FROM 'WK1' RECORD.

           IF ( PRINT-DETAIL-NO )
              GO TO END-OF-PRINTING-WK1-REC.

           IF ( SUPPRESS-ZIPCODE-NO )
              MOVE "Y"          TO SUPPRESS-ZIPCODE
              MOVE WK1-ZIPCODE  TO D1-ZIPCODE.

           MOVE WK1-ACCTNO      TO D1-ACCOUNT.
           INSPECT D1-ACCOUNT REPLACING ALL "/" BY "-".

           MOVE WK1-CLASS       TO D1-CLASS.

           MOVE WK1-NAME        TO D1-NAME.
           MOVE WK1-LOANDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY. 
           MOVE DATE-MMDDYY     TO D1-LOANDATE.

           MOVE WK1-LNAMT       TO D1-LNAMT.
           MOVE WK1-CURBAL      TO D1-CURBAL.
           MOVE WK1-OPEN        TO D1-OPEN.
           MOVE WK1-PAID        TO D1-PAID.
           MOVE WK1-PL          TO D1-PL.
           MOVE WK1-PL-TYPE     TO D1-PL-TYPE.
           MOVE WK1-PL-ACTIVITY TO D1-PL-ACTIVITY.

           MOVE "D" TO PRINT-TYPE.
           MOVE 1   TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       END-OF-PRINTING-WK1-REC.

           PERFORM UPDATE-TT-TOTALS.
           GO TO NEXT-WK1-RECORD.

       END-PRINT-INFORMATION.

           IF ( FIRST-TIME NOT = "Y" )
              PERFORM PRINT-ZIPCODE-TOTALS
              PERFORM UPDATE-WK2-TOTALS
              MOVE ZEROES TO TT-TOTALS
              PERFORM PRINT-BRANCH-TOTALS
              PERFORM DELETE-WK2-TOTALS.

           PERFORM CLOSE-WK1-FILE.
           PERFORM CLOSE-WK2-FILE.

       EXIT-PRINT-INFORMATION.
           EXIT.

      ******************************************************************
       PRINT-BRANCH-TOTALS SECTION.
      ******************************************************************

           IF ( PRINT-BRANCH-NO )
              GO TO END-PRINT-BRANCH-TOTALS.

           MOVE 99             TO LN-COUNT.
           MOVE "T"            TO PRINT-TYPE.
           MOVE CURRENT-BRANCH TO WK2-BRANCH.
           MOVE SPACES         TO WK2-ZIPCODE.
           PERFORM START-WK2-FILE.

       PRINT-BRANCH-ALL-ZIPCODE.

           PERFORM READ-WK2-FILE-NEXT.
           IF ( IO-BAD                      ) OR
              ( WK2-BRANCH NOT = CURRENT-BRANCH )
              GO TO END-PRINT-BRANCH-TOTALS.

           IF ( WK2-ZIPCODE = "ZZZZZ" )
              MOVE ALL "-"      TO DETAIL-LINE
              MOVE 1            TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "TOTAL     " TO T1-ZIPCODE
           ELSE
              MOVE WK2-ZIPCODE  TO T1-ZIPCODE.

           MOVE WK2-OPEN           TO T1-OPEN.
           MOVE WK2-PAID           TO T1-PAID.
           MOVE WK2-PL             TO T1-PL.
           MOVE WK2-PL-ACTIVITY(1) TO T1-PL-ACTIVE.
           MOVE WK2-PL-ACTIVITY(2) TO T1-PL-INACTIVE.
           MOVE WK2-ACCOUNTS       TO T1-ACCOUNTS.
           MOVE WK2-LNAMT          TO T1-LNAMT.
           MOVE WK2-CURBAL         TO T1-CURBAL.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-BRANCH-ALL-ZIPCODE.

       END-PRINT-BRANCH-TOTALS.
           MOVE 99 TO LN-COUNT.

       EXIT-PRINT-BRANCH-TOTALS.
           EXIT.

      ******************************************************************
       PRINT-CORP-TOTALS SECTION.
      ******************************************************************

           IF ( PRINT-CORP-NO )
              GO TO EXIT-PRINT-CORP-TOTALS.
       
           MOVE 99             TO LN-COUNT.
           MOVE "T"            TO PRINT-TYPE.
           MOVE 9999           TO WK2-BRANCH CURRENT-BRANCH.
           MOVE SPACES         TO WK2-ZIPCODE.
           PERFORM SET-HEADER-LINE-BRANCH.
           PERFORM OPEN-WK2-FILE.
           PERFORM START-WK2-FILE.

       PRINT-CORP-ALL-ZIPCODE.

           PERFORM READ-WK2-FILE-NEXT.
           IF ( IO-BAD            ) OR
              ( WK2-BRANCH NOT = 9999 )
              GO TO END-PRINT-CORP-TOTALS.

       
           IF ( WK2-ZIPCODE = "ZZZZZ" )
              MOVE ALL "-"      TO DETAIL-LINE
              MOVE 1            TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "TOTAL     " TO T1-ZIPCODE
           ELSE
              MOVE WK2-ZIPCODE  TO T1-ZIPCODE.

       
           MOVE WK2-OPEN           TO T1-OPEN.
           MOVE WK2-PAID           TO T1-PAID.
           MOVE WK2-PL             TO T1-PL.
           MOVE WK2-PL-ACTIVITY(1) TO T1-PL-ACTIVE.
           MOVE WK2-PL-ACTIVITY(2) TO T1-PL-INACTIVE.
           MOVE WK2-ACCOUNTS       TO T1-ACCOUNTS.
           MOVE WK2-LNAMT          TO T1-LNAMT.
           MOVE WK2-CURBAL         TO T1-CURBAL.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-CORP-ALL-ZIPCODE.

       END-PRINT-CORP-TOTALS.
           PERFORM CLOSE-WK2-FILE.

       EXIT-PRINT-CORP-TOTALS.
           EXIT.

      ******************************************************************
       PRINT-ZIPCODE-TOTALS SECTION.
      ******************************************************************

           IF ( PRINT-ZIPCOD-NO )
              GO TO EXIT-PRINT-ZIPCODE-TOTALS.

           MOVE "Z" TO PRINT-TYPE.
           PERFORM HEADER-TOTALS.

       
           MOVE CURRENT-ZIPCODE   TO T1-ZIPCODE.
           MOVE TT-OPEN           TO T1-OPEN.
           MOVE TT-PAID           TO T1-PAID.
           MOVE TT-PL             TO T1-PL.
           MOVE TT-PL-ACTIVITY(1) TO T1-PL-ACTIVE.
           MOVE TT-PL-ACTIVITY(2) TO T1-PL-INACTIVE.
           MOVE TT-ACCOUNTS       TO T1-ACCOUNTS.
           MOVE TT-LNAMT          TO T1-LNAMT.
           MOVE TT-CURBAL         TO T1-CURBAL.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       END-PRINT-ZIPCODE-TOTALS.

           PERFORM HEADER-DETAIL.

       EXIT-PRINT-ZIPCODE-TOTALS.
           EXIT.

      ******************************************************************
       PAGE-HEADER SECTION.
      ******************************************************************

           ADD 1              TO PAGE-NUMBER.
           MOVE PAGE-NUMBER   TO H1-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT     TO H1-TIME.
           MOVE HEADER-LINE-1 TO PR-REC.
           MOVE 99            TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-2 TO PR-REC.
           MOVE 1             TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE HEADER-LINE-3 TO PR-REC.
           MOVE 1             TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           IF ( PRINT-TYPE-DETAIL )
              MOVE ALL "=" TO PR-REC
              MOVE 2       TO LN-FEED
              PERFORM WRITE-PR-REC

              MOVE HEADER-LINE-DETAIL-1 TO PR-REC
              MOVE 1                    TO LN-FEED
              PERFORM WRITE-PR-REC

              MOVE HEADER-LINE-DETAIL-2 TO PR-REC
              MOVE 1                    TO LN-FEED
              PERFORM WRITE-PR-REC

              MOVE ALL "=" TO PR-REC
              MOVE 1       TO LN-FEED
              PERFORM WRITE-PR-REC.

           IF ( PRINT-TYPE-TOTALS )
              MOVE HEADER-TOTALS-TITLE TO PR-REC
              MOVE 2                    TO LN-FEED
              PERFORM WRITE-PR-REC.

           IF ( PRINT-TYPE-TOTALS  ) OR
              ( PRINT-TYPE-ZIPCODE )
              MOVE HEADER-LINE-TOTALS-1 TO PR-REC
              MOVE 2                    TO LN-FEED
              PERFORM WRITE-PR-REC

              MOVE ALL "-" TO PR-REC
              MOVE 1       TO LN-FEED
              PERFORM WRITE-PR-REC.

       END-PAGE-HEADER.

           MOVE 10 TO LN-COUNT.
           MOVE 1  TO LN-FEED.

       EXIT-PAGE-HEADER.
           EXIT.

      ******************************************************************
       HEADER-DETAIL SECTION.
      ******************************************************************

           MOVE "D"                  TO PRINT-TYPE.
           MOVE ALL "="              TO DETAIL-LINE.
           MOVE 2                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEADER-LINE-DETAIL-1 TO DETAIL-LINE.
           MOVE 1                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEADER-LINE-DETAIL-2 TO DETAIL-LINE.
           MOVE 1                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "="              TO DETAIL-LINE.
           MOVE 1                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-HEADER-DETAIL.
           EXIT.

      ******************************************************************
       HEADER-TOTALS SECTION.
      ******************************************************************

           MOVE HEADER-LINE-TOTALS-1 TO DETAIL-LINE.
           MOVE 2                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO DETAIL-LINE.
           MOVE 1       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-HEADER-TOTALS.
           EXIT.

      ******************************************************************
       PRINT-RANGES SECTION.
      ******************************************************************

           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
       
           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID NAME                    :" TO R1-DESC
              MOVE ENT-GROUP                            TO R1-FIELD
              MOVE 2                                    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER          :" TO R1-DESC
              MOVE BEG-BRANCH                           TO R1-NUMERIC
              MOVE 2                                    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH NUMBER             :" TO R1-DESC
              MOVE END-BRANCH                           TO R1-NUMERIC
              MOVE 1                                    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

       
           MOVE "BEGINNING CLASS NUMBER           :" TO R1-DESC.
           MOVE BEG-CLASS                            TO R1-NUMERIC.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "ENDING    CLASS NUMBER           :" TO R1-DESC.
           MOVE END-CLASS                            TO R1-NUMERIC.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "OPEN ACCOUNT (INCLUSE/SKIP/ONLY) :" TO R1-DESC.
           MOVE OPEN-ISO                             TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "PAID ACCOUNT (INCLUSE/SKIP/ONLY) :" TO R1-DESC.
           MOVE PAID-ISO                             TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "P & L ACCOUNT (INCLUSE/SKIP/ONLY):" TO R1-DESC.
           MOVE PL-ISO                               TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "----- TYPE (ALL/PL/P2/P3/P4/P5)  :" TO R1-DESC.
           MOVE PL-TYPE                              TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "----- ACTIVITY ([IN]ACTIVE/BOTH) :" TO R1-DESC.
           MOVE PL-ACTIVITY                          TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "PRINT DETAIL ANALYSIS    (YES/NO):" TO R1-DESC.
           MOVE PRINT-DETAIL                         TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "PRINT BRANCH TOTALS      (YES/NO):" TO R1-DESC.
           MOVE PRINT-BRANCH                         TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "PRINT ZIPCODE TOTALS     (YES/NO):" TO R1-DESC.
           MOVE PRINT-ZIPCOD                         TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "PRINT CORPORATE TOTALS   (YES/NO):" TO R1-DESC.
           MOVE PRINT-CORP                           TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-RANGES.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
              PERFORM PAGE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      ******************************************************************
       UPDATE-TT-TOTALS SECTION.
      ******************************************************************

           ADD 1          TO TT-ACCOUNTS.
           ADD WK1-LNAMT  TO TT-LNAMT.
           ADD WK1-CURBAL TO TT-CURBAL.

           IF ( WK1-OPEN-YES )
              ADD 1          TO TT-OPEN
              ADD WK1-LNAMT  TO TT-OPEN-LNAMT
              ADD WK1-CURBAL TO TT-OPEN-CURBAL.

           IF ( WK1-PAID-YES )
              ADD 1          TO TT-PAID
              ADD WK1-LNAMT  TO TT-PAID-LNAMT
              ADD WK1-CURBAL TO TT-PAID-CURBAL.

           IF ( WK1-PL-YES )
              ADD 1          TO TT-PL
              ADD WK1-LNAMT  TO TT-PL-LNAMT
              ADD WK1-CURBAL TO TT-PL-CURBAL.

       
           IF ( WK1-PL-PL )
              ADD 1          TO TT-PL-TYPE       (1)
              ADD WK1-LNAMT  TO TT-PL-TYPE-LNAMT (1)
              ADD WK1-CURBAL TO TT-PL-TYPE-CURBAL(1)
           ELSE
           IF ( WK1-PL-P2 )
              ADD 1          TO TT-PL-TYPE       (2)
              ADD WK1-LNAMT  TO TT-PL-TYPE-LNAMT (2)
              ADD WK1-CURBAL TO TT-PL-TYPE-CURBAL(2)
           ELSE
           IF ( WK1-PL-P3 )
              ADD 1          TO TT-PL-TYPE       (3)
              ADD WK1-LNAMT  TO TT-PL-TYPE-LNAMT (3)
              ADD WK1-CURBAL TO TT-PL-TYPE-CURBAL(3)
           ELSE
           IF ( WK1-PL-P4 )
              ADD 1          TO TT-PL-TYPE       (4)
              ADD WK1-LNAMT  TO TT-PL-TYPE-LNAMT (4)
              ADD WK1-CURBAL TO TT-PL-TYPE-CURBAL(4)
           ELSE
           IF ( WK1-PL-P5 )
              ADD 1          TO TT-PL-TYPE       (5)
              ADD WK1-LNAMT  TO TT-PL-TYPE-LNAMT (5)
              ADD WK1-CURBAL TO TT-PL-TYPE-CURBAL(5).

       
           IF ( WK1-PL-ACTIVITY-ACTIVE )
              ADD 1          TO TT-PL-ACTIVITY       (1)
              ADD WK1-LNAMT  TO TT-PL-ACTIVITY-LNAMT (1)
              ADD WK1-CURBAL TO TT-PL-ACTIVITY-CURBAL(1)
           ELSE
           IF ( WK1-PL-ACTIVITY-INACTIVE )
              ADD 1          TO TT-PL-ACTIVITY       (2)
              ADD WK1-LNAMT  TO TT-PL-ACTIVITY-LNAMT (2)
              ADD WK1-CURBAL TO TT-PL-ACTIVITY-CURBAL(2).

       EXIT-UPDATE-TT-TOTALS.
           EXIT.

      ******************************************************************
       UPDATE-WK2-TOTALS SECTION.
      ******************************************************************
       
      * WK2: MAKING BRANCH TOTALS (ZIPCODE: SINGLE)
       
           MOVE CURRENT-BRANCH  TO HOLD-WK2-BRANCH.
           MOVE CURRENT-ZIPCODE TO HOLD-WK2-ZIPCODE.
           PERFORM UPDATING-TO-WK2-FILE.
       
      * WK2: MAKING BRANCH TOTALS (ZIPCODE: *ALL*)
       
           MOVE CURRENT-BRANCH TO HOLD-WK2-BRANCH.
           MOVE "ZZZZZ"   TO HOLD-WK2-ZIPCODE.
           PERFORM UPDATING-TO-WK2-FILE.

       
      * WK2: MAKING GRAND TOTALS (ZIPCODE: SINGLE)
       
           MOVE 9999             TO HOLD-WK2-BRANCH.
           MOVE CURRENT-ZIPCODE  TO HOLD-WK2-ZIPCODE.
           PERFORM UPDATING-TO-WK2-FILE.

       
      * WK2: MAKING GRAND TOTALS (ZIPCODE: *ALL*)
       
           MOVE 9999         TO HOLD-WK2-BRANCH.
           MOVE "ZZZZZ" TO HOLD-WK2-ZIPCODE.
           PERFORM UPDATING-TO-WK2-FILE.

       EXIT-UPDATE-WK2-TOTALS.
           EXIT.

      ******************************************************************
       UPDATING-TO-WK2-FILE SECTION.
      ******************************************************************
       BEGIN-UPDATING-TO-WK2-FILE.

           MOVE HOLD-WK2-BRANCH  TO WK2-BRANCH.
           MOVE HOLD-WK2-ZIPCODE TO WK2-ZIPCODE.
           PERFORM READ-WK2-FILE.
           IF ( IO-GOOD )
              GO TO UPDATING-EXISTING-WK2-FILE.

      * CREATE A NEW RECORD FOR THIS TYPE OF TOTALS

           PERFORM CLEAR-WK2-RECORD.
           MOVE HOLD-WK2-BRANCH  TO WK2-BRANCH.
           MOVE HOLD-WK2-ZIPCODE TO WK2-ZIPCODE.
           PERFORM WRITE-WK2-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR
           ELSE
              GO TO BEGIN-UPDATING-TO-WK2-FILE.


       UPDATING-EXISTING-WK2-FILE.

      * UPDATING TO EXISTING RECORD FOR THIS TYPE OF TOTALS

           ADD TT-ACCOUNTS    TO WK2-ACCOUNTS.
           ADD TT-LNAMT       TO WK2-LNAMT.
           ADD TT-CURBAL      TO WK2-CURBAL.

           ADD TT-OPEN        TO WK2-OPEN
           ADD TT-OPEN-LNAMT  TO WK2-OPEN-LNAMT
           ADD TT-OPEN-CURBAL TO WK2-OPEN-CURBAL.

           ADD TT-PAID        TO WK2-PAID
           ADD TT-PAID-LNAMT  TO WK2-PAID-LNAMT
           ADD TT-PAID-CURBAL TO WK2-PAID-CURBAL.

           ADD TT-PL          TO WK2-PL
           ADD TT-PL-LNAMT    TO WK2-PL-LNAMT
           ADD TT-PL-CURBAL   TO WK2-PL-CURBAL.

       
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
                   ADD TT-PL-TYPE       (SUB) TO WK2-PL-TYPE       (SUB)
                   ADD TT-PL-TYPE-LNAMT (SUB) TO WK2-PL-TYPE-LNAMT (SUB)
                   ADD TT-PL-TYPE-CURBAL(SUB) TO WK2-PL-TYPE-CURBAL(SUB)
           END-PERFORM.

       
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                   ADD  TT-PL-ACTIVITY       (SUB) TO
                       WK2-PL-ACTIVITY       (SUB)
                   ADD  TT-PL-ACTIVITY-LNAMT (SUB) TO
                       WK2-PL-ACTIVITY-LNAMT (SUB)
                   ADD  TT-PL-ACTIVITY-CURBAL(SUB) TO
                       WK2-PL-ACTIVITY-CURBAL(SUB)
           END-PERFORM.

       
           PERFORM REWRITE-WK2-FILE
           IF ( IO-BAD )
              GO TO IO-ERROR.

       EXIT-UPDATING-TO-WK2-FILE.
           EXIT.

      ******************************************************************
       DELETE-WK2-TOTALS SECTION.
      ******************************************************************

      * DELETING BRANCH TOTALS RECORDS FOR COMPRESSING SIZE

           MOVE CURRENT-BRANCH TO WK2-BRANCH.
           MOVE SPACES         TO WK2-ZIPCODE.
           PERFORM START-WK2-FILE.

       DELETING-WK2-TOTAL-RECORDS.

           PERFORM READ-WK2-FILE-NEXT.

           IF ( IO-BAD                      ) OR
              ( WK2-BRANCH NOT = CURRENT-BRANCH )
              GO TO EXIT-DELETE-WK2-TOTALS.

           PERFORM DELETE-WK2-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO DELETING-WK2-TOTAL-RECORDS.

       EXIT-DELETE-WK2-TOTALS.
           EXIT.

      ******************************************************************
       CLEAR-WK2-RECORD SECTION.
      ******************************************************************

           MOVE SPACES TO WK2-REC.

           MOVE ZEROES TO WK2-BRANCH
                          WK2-ACCOUNTS
                          WK2-LNAMT
                          WK2-CURBAL
                          WK2-OPEN    WK2-OPEN-LNAMT    WK2-OPEN-CURBAL
                          WK2-PAID    WK2-PAID-LNAMT    WK2-PAID-CURBAL
                          WK2-PL      WK2-PL-LNAMT      WK2-PL-CURBAL.

       
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
                   MOVE ZEROES TO WK2-PL-TYPE       (SUB)
                                  WK2-PL-TYPE-LNAMT (SUB)
                                  WK2-PL-TYPE-CURBAL(SUB)
           END-PERFORM.

       
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                   MOVE ZEROES TO WK2-PL-ACTIVITY       (SUB)
                                  WK2-PL-ACTIVITY-LNAMT (SUB)
                                  WK2-PL-ACTIVITY-CURBAL(SUB)
           END-PERFORM.

       EXIT-CLEAR-WK2-RECORD.
           EXIT.

      *****************************************************************
       SCAN-GR-FILE SECTION.
      *****************************************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF GRSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
              GO TO EXIT-SCAN-GR-FILE.

           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF GR1-KEY.
           PERFORM SCREENIO.

           MOVE "00"       TO VDUSTATUS.
           MOVE ZEROES     TO IO-FG.
           MOVE GRSCAN-KEY TO VDUBUF.

       EXIT-SCAN-GR-FILE.
           EXIT.

      *****************************************************************
       SCAN-LC-FILE SECTION.
      *****************************************************************

           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-SCAN-LC-FILE.

           IF ( RANGE-ENTRY-BEGINNING )
              MOVE "SNNN020BEG-CLASS." TO VDU
           ELSE
              MOVE "SNNN020END-CLASS." TO VDU.

           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "00"       TO VDUSTATUS.
           MOVE ZEROES     TO IO-FG.
           MOVE LCSCAN-KEY TO VDUBUF.

       EXIT-SCAN-LC-FILE.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO ZIPCOD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ZIPCOD-SYSDATE.
           DISPLAY ZIPCOD-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ZIPCOD-FORM-FIELDS TO WR-BUF.
           MOVE ZIPCOD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE ZIPCOD-HELP--FILE TO HELP-LINK-FILE.
           MOVE ZIPCOD-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/ZIPCOD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      *=================================================================
       SET-HEADER-LINE-BRANCH.
           MOVE CURRENT-BRANCH TO H3-BRANCH.

           IF ( CURRENT-BRANCH = 9999 )
              MOVE "***** ALL BRANCHES *****" TO H3-BRANCH-DESC
           ELSE
              MOVE BR-NAME                   TO H3-BRANCH-DESC.

      *=================================================================
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBWK1IN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".

       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
             MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           MOVE WK1-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.
           IF ( SAVE-BEG-BRANCH     > 0 )    AND
              ( SAVE-BEG-BRANCH NOT = 9999 )
              MOVE SAVE-BEG-BRANCH TO BEG-BRANCH
              MOVE SAVE-END-BRANCH TO END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE SAVE-BEG-BRANCH TO BEG-BRANCH
              MOVE SAVE-END-BRANCH TO END-BRANCH
              MOVE SAVE-GROUP-FLAG TO GROUP-FLAG
              MOVE SAVE-ENT-GROUP  TO ENT-GROUP.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ZIPCOD-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
