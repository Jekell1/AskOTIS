      * COPYMEMBER: LIBLP/LP01LTC
      ******************************************************************
      *    COLLATERAL TRAILER FILE : (1200) BYTES
      *
      * COLLATERAL INFO:
      *
      * BAH 06/05/2015 CREATED NEW FROM LP01LT
      **************************************************************************
       78  LTC-SIZE                      VALUE 1200.
       01  LTC-REC.
           03  LTC1-KEY.
               05  LTC-BRNO              PIC 9(4).
               05  LTC-ACCTNO            PIC 9(8).
               05  LTC-SEQNO             PIC 999.

      *>>XDD DATE=YYYYMMDD
           03  LTC-LTOUCH-DATE           PIC 9(8)     COMP-3.

           03  LTC-COLLATERAL-INFO.
      * 160 X 3 = 480
               05  LTC-AUTO-GROUP.
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
      *>>XDD DATE=YYYYMMDD
                       09  LTC-VIN-CHG-DATE   PIC 9(8)     COMP-3.
                       09  FILLER             PIC X(30).

      * NEEDED FOR VIN SCAN KEYS
               05  LTC-AUTO-GROUPX REDEFINES LTC-AUTO-GROUP.
                   07  FILLER                 PIC X(65).
                   07  LTC2-KEY               PIC X(20).
                   07  FILLER                 PIC X(140).
                   07  LTC3-KEY               PIC X(20).
                   07  FILLER                 PIC X(140).
                   07  LTC4-KEY               PIC X(20).
                   07  FILLER                 PIC X(75).


      * 120 X 3 = 360
               05  LTC-HHGS-GROUP.
                   07  LTC-HHGS             OCCURS 3.
                       09  LTC-HHGS-GROUP-LEVEL.
                           11  LTC-COLL-INFO1   PIC X(20).
                           11  LTC-COLL-INFO2   PIC X(20).
                           11  LTC-COLL-INFO3   PIC X(20).
                           11  LTC-COLL-INFO4   PIC X(20).
                       09  FILLER REDEFINES LTC-HHGS-GROUP-LEVEL.
                           11  LTC-COLL-BUF     PIC X OCCURS 80.
                       09  FILLER            PIC X(40).

           03  FILLER            PIC X(340).
