      * COPYMEMBER: LIBGB/GB01RC
      ******************************************************************
      *
      *            ( GLOBAL RECAP (DAILY) FILE )          
      *
      *  RC1-KEY - RC-BRNO, RC-TRANS-DATE
      *  RC2-KEY - RC-STATUS, RC-BRNO, RC-TRANS-DATE
      *
      * NOTE:
      *   RC-TRANS-DATE - IS THE TRANSACTION DATE POSTED FOR (CCYYMMDD)
      *
      *=================================================================
      * REV:
      *  BLL 062195 ADDED RC-STATUS OF M1 (CHECKS EXTRACTED)
      *  BLL 090595 ADDED RC-STATUS OF G1 (EOM EARNINGS DONE),
      *             RC-EARNINGS-DATE, EARNINGS-TIME, LAST-ACCRUE
      *  JTG 112197 EXPANDED RECORD BY 25 BYTES 135 --> 160
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * BLL 050920 ADDED RC-BANK-BAL, CLOSING BRANCH BANK BALANCE AS OF
      *            END OF DAY, WORLD PR# 433
      * BLF 051215 ADDED REDEF OF RC-TRANS-DATE TO GET MM, DD, YYY
      * KEC 2015.1119 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED RC-SIZE; RC-REC SIZE = 250
      *         ADDED RC-LTOUCH-DATE;
      *         CHANGED DATE FORMAT NOW CCYYMMDD;
      *         CHANGED RC-TRANS-DATE FORMAT FROM YYYMMDD TO CCYYMMDD.
      * BLM 160915 ADDED RC-CHECK-DEPOSIT FOR SCANNED CHECKS,
      *            WORLD PR# 1188
      * BAH 190218 REMOVED REDEFINE OF RC-TRANS-DATE.
      * BAH 190305 REMOVED FILLER IN MIDDLE OF REC
      *  CS 190410 ADD FUTURE FIELDS, RC-FUTURE-DATES, RC-FUTURE-ALPHAS 
      *            RC-FUTURE-AMOUNTS, RC-FUTURE-FLAGS, RC-FUTURE-NUMERICS
      * BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210427 REMOVED FUTURE FIELDS
      *  20230600 BAH ADDED FILLER
      ******************************************************************
       78  RC-SIZE                                VALUE 300.
       01  RC-REC.
           03  RC2-KEY.
               05  RC-STATUS                  PIC XX.
                   88 RC-STATUS-VALID             VALUE
                      "A1" "B1" "C1" "D1" "G1"
                      "H1" "I1" "J1" "K1" "K2"
                      "M1".
                   88 RC-OPEN                     VALUE "A1".
                   88 RC-INPROCESS                VALUE "B1".
                   88 RC-UPDATED                  VALUE "C1".
                   88 RC-EOD-BATCH-DONE           VALUE "D1".
                   88 RC-EOM-EARNINGS-DONE        VALUE "G1".
                   88 RC-EOM1-BATCH-DONE          VALUE "H1".
                   88 RC-EOM-UPDATE-DONE          VALUE "I1".
                   88 RC-EOM2-BATCH-DONE          VALUE "J1".
                   88 RC-BANK-DEPOSIT             VALUE "K1".
                   88 RC-DEPOSIT-TRANSMITTED      VALUE "K2".
                   88 RC-CHECKS-EXTRACTED         VALUE "M1".
               05  RC1-KEY.
                   07  RC-BRNO                PIC 9(4).
      $XFD DATE=YYYYMMDD
                   07  RC-TRANS-DATE          PIC 9(8).

      $XFD DATE=YYYYMMDD
           03  RC-LTOUCH-DATE              PIC 9(08)      COMP-3.

           03  RC-FLAGS.
               05  RC-RECAP-FG             PIC X.
                   88 RC-RECAP-SUCESSFUL       VALUE "Y".

               05  RC-GL-FG                PIC X.
                   88 RC-GL-IN-BALANCE         VALUE "Y".

               05  RC-EOD-BATCH-FG         PIC X.
                   88 RC-EOD-BATCH-SUCESSFUL   VALUE "Y".

               05  RC-NIGHTLY-FUNDS-FG     PIC X.
                   88 RC-WENT-TO-DEPOSIT-FILE  VALUE "D".
                   88 RC-TRANS-SUCCESSFUL      VALUE "Y".

               05  RC-EOM1-BATCH-FG        PIC X.
                   88 RC-EOM1-BATCH-SUCESSFUL  VALUE "Y".
               05  RC-EOM-UPDATE-FG        PIC X.
                   88 RC-EOM-UPDATE-SUCESSFUL  VALUE "Y".
               05  RC-EOM2-BATCH-FG        PIC X.
                   88 RC-EOM2-BATCH-SUCESSFUL  VALUE "Y".

               05  RC-BS-P2P3-INBAL-FG     PIC X.
                   88 RC-BS-P2P3-INBAL         VALUE "Y".

           03  RC-DEPOSIT                  PIC S9(7)V99   COMP-3.
           03  RC-OVER-SHORT               PIC S9(7)V99   COMP-3.
           03  RC-CHECKS                   PIC S9(7)V99   COMP-3.

           03  RC-OPENED-INFO.
      $XFD DATE=YYYYMMDD
               05  RC-OPENED-DATE          PIC 9(8)       COMP-3.
               05  RC-OPENED-TIME          PIC 9(6)       COMP-3.
               05  RC-OP-USERID            PIC X(10).

           03  RC-CLOSED-INFO.
      $XFD DATE=YYYYMMDD
               05  RC-CLOSED-DATE          PIC 9(8)       COMP-3.
               05  RC-CLOSED-TIME          PIC 9(6)       COMP-3.
               05  RC-CL-USERID            PIC X(10).

           03  RC-UPDATED-INFO.
      $XFD DATE=YYYYMMDD
               05  RC-UPDATED-DATE         PIC 9(8)       COMP-3.
               05  RC-UPDATED-TIME         PIC 9(6)       COMP-3.
               05  RC-UPDATED-USERID       PIC X(10).

           03  RC-EOD-BATCH-INFO.
      $XFD DATE=YYYYMMDD
               05  RC-EOD-BATCH-DATE       PIC 9(8)       COMP-3.
               05  RC-EOD-BATCH-TIME       PIC 9(6)       COMP-3.

           03  RC-NIGHTLY-FUNDS-INFO.
      $XFD DATE=YYYYMMDD
               05  RC-FUNDS-DATE           PIC 9(8)       COMP-3.
               05  RC-FUNDS-TIME           PIC 9(6)       COMP-3.

           03  RC-EOM1-BATCH-INFO.
      $XFD DATE=YYYYMMDD
               05  RC-EARNINGS-DATE        PIC 9(8)       COMP-3.
               05  RC-EARNINGS-TIME        PIC 9(6)       COMP-3.

           03  RC-EOM-UPDATED-INFO.
      $XFD DATE=YYYYMMDD
               05  RC-EOM-UPDATED-DATE     PIC 9(8)       COMP-3.
               05  RC-EOM-UPDATED-TIME     PIC 9(6)       COMP-3.

           03  RC-EOM2-BATCH-INFO.
      $XFD DATE=YYYYMMDD
               05  RC-EOM2-BATCH-DATE      PIC 9(8)       COMP-3.
               05  RC-EOM2-BATCH-TIME      PIC 9(6)       COMP-3.

           03  RC-CHECKS-EXTRACTED-INFO.
      $XFD DATE=YYYYMMDD
               05  RC-CHECK-DATE           PIC 9(8)       COMP-3.
               05  RC-CHECK-TIME           PIC 9(6)       COMP-3.

           03  RC-LAST-ACCRUE              PIC 9(4)       COMP-3.
           03  RC-ERROR-MSG                PIC X(15).
           03  RC-BANK-BAL                 PIC S9(9)V99   COMP-3.
           03  RC-CHECK-DEPOSIT            PIC S9(7)V99   COMP-3.

           03  FILLER                      PIC X(118).
