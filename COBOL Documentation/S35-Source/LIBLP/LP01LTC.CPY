      * COPYMEMBER: LIBLP/LP01LTC
      ******************************************************************
      *    COLLATERAL TRAILER FILE 
      *
      ***********************************************************
      * NOTE:
      *   IF YOU CHANGE THIS LAYOUT, YOU MUST CHANGE LP01PC, PC-LTC-RECORD
      ***********************************************************
      *
      * KEY 1 : BRNO, ACCTNO, SEQNO
      * KEY 2 : BRNO, AUTO 1 VIN-1
      * KEY 3 : BRNO, AUTO 2 VIN-2
      * KEY 4 : BRNO, AUTO 3 VIN-3
      *
      * BAH 06/05/2015 CREATED NEW FROM LP01LT
      * BAH 03/04/2019 REMOVED FILLER FROM THE LTC-AUTO OCCURS TABLE, CAN NOT
      *                HAVE FILLER IN MIDDLE OF RECORD FOR SQL. ALSO CHANGED
      *                FILLERS IN THE LTC-VIN-KEYS TABLE TO REAL NAMES.
      *  CS 190604 REMOVE ENDING FILLER, NO FUTURE FIELDS
      *  BAH 20230612 ADDED FILLER
      **************************************************************************
       78  LTC-SIZE                      VALUE 1000.
       01  LTC-REC.
           03  LTC1-KEY.
               05  LTC-BRNO              PIC 9(4).
               05  LTC-ACCTNO            PIC 9(8).
               05  LTC-SEQNO             PIC 999.

      $XFD DATE=YYYYMMDD
           03  LTC-LTOUCH-DATE           PIC 9(8)     COMP-3.

           03  LTC-COLLATERAL-INFO.
      * 480
      * NEEDED FOR VIN SCAN KEYS
               05  LTC-VIN-KEYS.
                   07  LTC-DESC-1             PIC X(15).
                   07  LTC-MAKE-1             PIC X(15).
                   07  LTC-MODEL-1            PIC X(15).
                   07  LTC-YEAR-1             PIC X(4).
                   07  LTC-STYLE-1            PIC X(8).
                   07  LTC-CLASS-1            PIC X(8).
                   07  LTC-VIN-1              PIC X(20).
                   07  LTC-TITLE-NO-1         PIC X(20).
                   07  LTC-MILEAGE-1          PIC S9(7)    COMP-3.
                   07  LTC-VALUE-1            PIC S9(7)V99 COMP-3.
                   07  LTC-SELLPR-1           PIC S9(7)V99 COMP-3.
                   07  LTC-MISC-AMT-1         PIC S9(7)V99 COMP-3.
                   07  LTC-SENT-FLAG-1        PIC X.
      $XFD DATE=YYYYMMDD
                   07  LTC-VIN-CHG-DATE-1     PIC 9(8)     COMP-3.
                   07  FILLER                 PIC X(30).

                   07  LTC-DESC-2             PIC X(15).
                   07  LTC-MAKE-2             PIC X(15).
                   07  LTC-MODEL-2            PIC X(15).
                   07  LTC-YEAR-2             PIC X(4).
                   07  LTC-STYLE-2            PIC X(8).
                   07  LTC-CLASS-2            PIC X(8).
                   07  LTC-VIN-2              PIC X(20).
                   07  LTC-TITLE-NO-2         PIC X(20).
                   07  LTC-MILEAGE-2          PIC S9(7)    COMP-3.
                   07  LTC-VALUE-2            PIC S9(7)V99 COMP-3.
                   07  LTC-SELLPR-2           PIC S9(7)V99 COMP-3.
                   07  LTC-MISC-AMT-2         PIC S9(7)V99 COMP-3.
                   07  LTC-SENT-FLAG-2        PIC X.
      $XFD DATE=YYYYMMDD
                   07  LTC-VIN-CHG-DATE-2     PIC 9(8)     COMP-3.
                   07  FILLER                 PIC X(30).

                   07  LTC-DESC-3             PIC X(15).
                   07  LTC-MAKE-3             PIC X(15).
                   07  LTC-MODEL-3            PIC X(15).
                   07  LTC-YEAR-3             PIC X(4).
                   07  LTC-STYLE-3            PIC X(8).
                   07  LTC-CLASS-3            PIC X(8).
                   07  LTC-VIN-3              PIC X(20).
                   07  LTC-TITLE-NO-3         PIC X(20).
                   07  LTC-MILEAGE-3          PIC S9(7)    COMP-3.
                   07  LTC-VALUE-3            PIC S9(7)V99 COMP-3.
                   07  LTC-SELLPR-3           PIC S9(7)V99 COMP-3.
                   07  LTC-MISC-AMT-3         PIC S9(7)V99 COMP-3.
                   07  LTC-SENT-FLAG-3        PIC X.
      $XFD DATE=YYYYMMDD
                   07  LTC-VIN-CHG-DATE-3     PIC 9(8)     COMP-3.
                   07  FILLER                 PIC X(30).

      * 160 X 3 = 480
               05  LTC-AUTO-GROUP REDEFINES LTC-VIN-KEYS.
                   07  LTC-AUTO             OCCURS 3.
                       09  LTC-DESC           PIC X(15).
                       09  LTC-MAKE           PIC X(15).
                       09  LTC-MODEL          PIC X(15).
                       09  LTC-YEAR           PIC X(4).
                       09  LTC-STYLE          PIC X(8).
                       09  LTC-CLASS          PIC X(8).
                       09  LTC-VIN            PIC X(20).
                       09  LTC-TITLE-NO       PIC X(20).
                       09  LTC-MILEAGE        PIC S9(7)    COMP-3.
                       09  LTC-VALUE          PIC S9(7)V99 COMP-3.
                       09  LTC-SELLPR         PIC S9(7)V99 COMP-3.
                       09  LTC-MISC-AMT       PIC S9(7)V99 COMP-3.
                       09  LTC-SENT-FLAG      PIC X.
      $XFD DATE=YYYYMMDD
                       09  LTC-VIN-CHG-DATE   PIC 9(8)     COMP-3.
                       09  FILLER             PIC X(30).

      * 80 X 3 = 240
               05  LTC-HHGS-GROUP.
                   07  LTC-HHGS               PIC X(80) OCCURS 3.
               05  LTC-HHGS-INFO REDEFINES LTC-HHGS-GROUP.
                   07  LTC-HHGS-TABLE         OCCURS 3.
                       09  LTC-COLL-INFO1    PIC X(20).
                       09  LTC-COLL-INFO2    PIC X(20).
                       09  LTC-COLL-INFO3    PIC X(20).
                       09  LTC-COLL-INFO4    PIC X(20).

           03  FILLER                        PIC X(260).
