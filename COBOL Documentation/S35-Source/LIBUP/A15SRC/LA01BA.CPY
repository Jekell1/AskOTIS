      * COPYMEMBER: LIBLA/LA01BA
      ******************************************************************
      *
      *            ( BORROWER APPS FILE )            
      *                     LA01BA                             RSZ=0776
      *
      * REV:
      * KEC 020397 REVISED KEY STRUCTURE OF BA-FILE.
      * KEC 052297 ADDED EXTRA FILLER (62) TO RECORD SIZE.
      * BLV 090497 ADDED NEW FIELDS - MISC: OTHPHONE, BEEPER, SEX,
      *                                     ORIGIN, MED-INS
      * KEC 022398 REVISED THE LAYOUT. REMOVED THE FOLLOWING:
      *
      *               ADR1, ADR2, BEEPER-NO, CITY, OTHPHONE, STATE, ZIP 
      *
      *            ADDED THE FOLLOWING:
      *
      *               BA-MISC-PHONES (OCCURS 6).
      *
      *            KEPT THE SIZE OF THE RECORD THE SAME: RSZ=777
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 2000.0605 {PR#01216} NEW SOUTH [KEVIN MULLEN]
      *                ADDED THE FOLLOWING:
      *                   03  BA-CO-ORIGIN                   PIC X(04).
      *                   03  BA-CO-SEX                      PIC X(01).
      *
      * KEC 2003.1130 {PR#00088} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *               ADDED 78 BA-SIZE   VALUE 776             RSZ=0776
      *
      * KEC 2005.0112 {PR#02453} LENDMARK <A15> [K. MULLEN]
      *               ADDED BA-COUNTY-TAX-CD AND BA-CITY-TAX-CD.
      *
      ******************************************************************
       78  BA-SIZE                                            VALUE 776.
      *=================================================================
       01  BA-REC.
           03  BA1-KEY.
               05  BA-APPNO                      PIC 9(06).
           03  BA2-KEY.
               05  BA-SSNO                       PIC 9(09).
               05  BA-APPDATE                    PIC 9(07)       COMP-3.
           03  BA4-KEY.
               05  BA-DLNO                       PIC 9(04)V99.
               05  BA3-KEY.
                   07  BA-LNAME                  PIC X(16).
                   07  BA-FNAME                  PIC X(16).

      *=================================================================
      *
      *    COLLATERAL INFORMATION
      *
      *=================================================================
           03  BA-COLLATERAL-INFORMATION.
               05  BA-CLASS                      PIC X(08).
               05  BA-DESCRIPTION                PIC X(10).
               05  BA-ID                         PIC X(20).
               05  BA-MAKE                       PIC X(15).
               05  BA-MILEAGE                    PIC S9(7)       COMP-3.
               05  BA-MISC-AMT                   PIC S9(5)V99    COMP-3.
               05  BA-MODEL                      PIC X(15).
               05  BA-SELLPR                     PIC S9(5)V99    COMP-3.
               05  BA-STYLE                      PIC X(08).
               05  BA-TITLE-NO                   PIC X(08).
               05  BA-VALUE                      PIC S9(5)V99    COMP-3.
               05  BA-YEAR                       PIC X(04).

      *- - - - - - - - - - - - - -  HOUSEHOLD GOODS AND APPLIANCES 1 & 2
           03  BA-HOUSEHOLD-GOODS-INFORMATION.
               05  BA-HHGS1                      PIC X(80).
               05  BA-HHGS2                      PIC X(80).

      *=================================================================
      *
      *    CO-MAKER INFORMATION
      *
      *=================================================================
           03  BA-CO-BDATE                       PIC 9(06)       COMP-3.
           03  BA-CO-EMPADDR1                    PIC X(24).
           03  BA-CO-EMPADDR2                    PIC X(24).
           03  BA-CO-EMPLOYER                    PIC X(24).
           03  BA-CO-EMPLPOS                     PIC X(24).
           03  BA-CO-EMPPAY                      PIC 9(06)V99    COMP-3.

           03  BA-CO-EMPPHONE-INFO.
               05  BA-CO-EMPPHONE                PIC 9(10)       COMP-3.
               05  BA-CO-EMPEXT                  PIC 9(04)       COMP-3.

           03  BA-CO-EMPYEARS                    PIC 9(02)       COMP-3.

           03  BA-CO-FIRST-LAST-NAME.
               05  BA-CO-FNAME                   PIC X(16).
               05  BA-CO-LNAME                   PIC X(16).

           03  BA-CO-SSNO                        PIC 9(09).
           03  BA-CO-WEEK-MONTH                  PIC X(01).

      *=================================================================
      *
      *    MAKER EMPLOYMENT INFORMATION
      *
      *=================================================================
           03  BA-EMPLOYER                       PIC X(24).
           03  BA-EMP-ADDR1                      PIC X(24).
           03  BA-EMP-ADDR2                      PIC X(24).

           03  BA-EMP-PHONE-INFO.
               05  BA-EMP-PHONE                  PIC 9(10)      COMP-3.
               05  BA-EMP-EXT                    PIC 9(04)      COMP-3.

           03  BA-EMP-POSITION                   PIC X(24).
           03  BA-EMP-PAY                        PIC 9(06)V99   COMP-3.
           03  BA-EMP-PAY-WEEK-MONTH             PIC X(01).
           03  BA-EMP-PAYOT                      PIC 9(06)V99   COMP-3.
           03  BA-EMP-PAYSR                      PIC X(08).
           03  BA-EMP-YEARS                      PIC 9(02)      COMP-3.

      *=================================================================
      *
      *    MAKER PHONE INFORMATION
      *
      *=================================================================
           03  BA-HOME-PHONE                     PIC 9(10)      COMP-3.
           03  BA-PHONE                REDEFINES BA-HOME-PHONE
                                                 PIC 9(10)      COMP-3.

           03  BA-MISC-PHONES                    OCCURS 6 TIMES.
               05  BA-MISC-PHONE                 PIC 9(10)     COMP-3.
               05  BA-MISC-EXT                   PIC 9(04)     COMP-3.
               05  BA-MISC-PHONE-CD              PIC X(02).

      *=================================================================
      *
      *    MAKER RESIDENCE INFORMATION
      *
      *=================================================================
           03  BA-RENTCD                         PIC X(01).
           03  BA-RES-SINCE                      PIC 9(04).
           03  BA-RES-SINCEX           REDEFINES BA-RES-SINCE.
               05  BA-RES-MM                     PIC 9(02).
               05  BA-RES-YY                     PIC 9(02).

      *=================================================================
      *
      *    MAKER MISCELLANEOUS INFORMATION
      *
      *=================================================================
           03  BA-BDATE                          PIC 9(06)       COMP-3.
           03  BA-MED-INS                        PIC X(01).
           03  BA-ORIGIN                         PIC X(04).
           03  BA-SEX                            PIC X(01).

      *=================================================================
      *
      *    MISCELLANEOUS INFORMATION
      *
      *=================================================================
           03  BA-BRNO                           PIC 9(04)       COMP-3.
           03  BA-REFDATE                        PIC 9(06)       COMP-3.
           03  BA-CO-ORIGIN                      PIC X(04).
           03  BA-CO-SEX                         PIC X(01).
           03  BA-CITY-TAX-CD                    PIC X(03).
           03  BA-COUNTY-TAX-CD                  PIC X(03).

      *-----------------------------------------------------------------
           03  FILLER                            PIC X(66).
