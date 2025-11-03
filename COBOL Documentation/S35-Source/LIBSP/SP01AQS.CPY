      * COPYMEMBER: LIBSP/SP01AQS
      ******************************************************************
      *
      * AUDITOR INQUIRY SUMMARY
      *
      *=================================================================
      *
      * HEADER RECORD:
      *    AQ-BRNO   = NON-ZERO (HAS BRANCH NUMBER IN IT)
      *
      *=================================================================
      *
      * AQS1-KEY = AQ-BRNO 
      *
      *=================================================================
      * REV:
      * 032619 AMM SPLIT FROM AQ FILE
      * 190515  CS ADD FUTURE FIELDS
      * 2020-0610 ADDED AQS-LTOUCH-DATE
      * AMM 20210423 REMOVED FUTURE FIELDS, RESIZED FILE
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  AQS-SIZE                     VALUE 70.
       01  AQS-REC.
           03  AQS1-KEY.
               05  AQS-BRNO             PIC 9(4).
           03  AQS-HEADER-AREA.
               05  AQS-NEXT-SEQNO       PIC 9(6).
               05  AQS-TOT-REC          PIC 9(6).
               05  AQS-WORKED-REC       PIC 9(6).
      
      $XFD DATE=YYYYMMDD
           03  AQS-LTOUCH-DATE          PIC 9(8).
           03  FILLER                   PIC X(40).
