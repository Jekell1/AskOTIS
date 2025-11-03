      * COPYMEMBER: LIBLP/LP01BI
      *****************************************************************
      *                           (BI-FILE)
      *    BORROWER INDEX FILE
      *
      * BI1-KEY = BI-SSNO, BI-ACCTNO, BI-OWNBR
      * BI2-KEY = BI-LNAME, BI-FNAME
      * BI3-KEY = BI-OWNBR, BI-SSNO, BI-ACCTNO IS NON-CONTIGEOUS
      *
      * GP-SKIP-BI-UPDATE CAN BE SET IN GPMAN2, FIELD 28, TO AVOID UPDATES !
      *
      *=================================================================
      * REV:
      * JTG 072597 CHANGED BI-SALEFIN TO BI-GIFTNET RE: A90 TO A60
      *            ALSO EXPANDED RECORD FROM 53 TO 64 BYTES
      * BLV 020698 ADDED BI-SALEFIN BACK TO RECORD
      * MJD 030398 ADDED BI-PLCD AND BI-CONVERCD
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * BAH 160422 ADDED BI-SMALLOAN FOR DUAL TEST, REGENCY PR#5030
      * BAH 160826 EXPANDED DATES, ADDED BI-LTOUCH-DATE
      * BAH 180316 ADDED BI-PLDATE, WORLD PR#1308
      *
      * KEC 2019-0107 ADDED BI3-KEY WHICH IS NON-CONTIGEOUS;
      *               OWNBR/SSNO/ACCTNO.
      *  CS 190412 ADD FUTURE FIELDS
      * BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * AMM 20210426 REMOVED FUTURE FIELDS, RESIZED FILE
      *  20230600 BAH ADDED FILLER
      *****************************************************************
       78  BI-SIZE                 VALUE 170.
       01  BI-REC.
           03  BI1-KEY.
               05  BI-SSNO         PIC 9(9).
               05  BI-ACCTNO       PIC 9(8).
               05  BI-OWNBR        PIC 9(4).
           03  BI2-KEY.
               05  BI-LNAME        PIC X(35).
               05  BI-FNAME        PIC X(35).
      $XFD DATE=YYYYMMDD
           03  BI-LTOUCH-DATE      PIC 9(8) COMP-3.
           03  BI-MAKERCD          PIC X.
           03  BI-ORGST            PIC XX.
           03  BI-GIFTNET          PIC X.
      $XFD DATE=YYYYMMDD
           03  BI-POFFDATE         PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
           03  BI-ENTDATE          PIC 9(8) COMP-3.
           03  BI-SALEFIN          PIC X.
           03  BI-CONVERCD         PIC X.
           03  BI-PLCD             PIC X.
           03  BI-SMALLOAN         PIC X.
      $XFD DATE=YYYYMMDD
           03  BI-PLDATE           PIC 9(8) COMP-3.
           03  FILLER              PIC X(51).
