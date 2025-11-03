      * COPYMEMBER: LIBSP/SPFDREPORT
      ******************************************************************
      *
      * IDENTIFICATION:  LIBSP/SPFDREPORT
      *
      * DESCRIPTION   :  STANDARD REPORT FILE (FD)
      *
      *=================================================================
      * REV:
      * KEC 2003.1023 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ***NEW***
      *
      ******************************************************************
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD.

      *=================================================================
      *01  REPORT-REC.
      *
      *    03  REPORT-KEY.
      *
      *        05  REPORT-BRNO                       PIC 9(05).
      *            88  REPORT-GRAND-TOTAL                VALUE 99999.
      *
      *        05  REPORT-CLASS                      PIC 9(02).
      *            88  REPORT-BRNO-TOTAL                 VALUE 99.
      *
      *        05  REPORT-DLNO                       PIC 9(05)V99.
      *            88  REPORT-CLASS-TOTAL                VALUE 99999.99.
      *
      *        05  REPORT-ACCTNO                     PIC 9(08).
      *            88  REPORT-DLNO-TOTAL                 VALUE 99999999.
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    03  REPORT-REC-INFO                       PIC X(76).

