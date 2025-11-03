      * COPYMEMBER: LIBLP/LP01BI
      *****************************************************************
      *                           (BI-FILE)
      *    BORROWER INDEX FILE
      *
      * GP-SKIP-BI-UPDATE CAN BE SET IN GPMAN2, FIELD 28, TO AVOID UPDATES !
      *
      * REV:
      *  JTG 072597 CHANGED BI-SALEFIN TO BI-GIFTNET RE: A90 TO A60
      *             ALSO EXPANDED RECORD FROM 53 TO 64 BYTES
      *  BLV 020698 ADDED BI-SALEFIN BACK TO RECORD
      *  MJD 030398 ADDED BI-PLCD AND BI-CONVERCD
      *  MJD 990125 ----- UPDATED IN FLEXGEN -----
      *  BAH 160422 ADDED BI-SMALLOAN FOR DUAL TEST, REGENCY PR#5030
      *  BAH 180501 ADDED BI-PLDATE, WORLD PR#1308
      *****************************************************************
       78  BI-SIZE                 VALUE 64.
       01  BI-REC.
           03  BI1-KEY.
               05  BI-SSNO         PIC 9(9) COMP.
               05  BI-ACCTNO       PIC 9(8) COMP.
               05  BI-OWNBR        PIC 9(4) COMP.
           03  BI2-KEY.
               05  BI-LNAME        PIC X(16).
               05  BI-FNAME        PIC X(16).
           03  BI-MAKERCD          PIC X.
           03  BI-ORGST            PIC XX.
           03  BI-GIFTNET          PIC X.
           03  BI-POFFDATE         PIC 9(6) COMP.
           03  BI-ENTDATE          PIC 9(6) COMP.
           03  BI-ENTDATEX        REDEFINES BI-ENTDATE
                                   PIC X(3).
           03  BI-SALEFIN          PIC X.
           03  BI-CONVERCD         PIC X.
           03  BI-PLCD             PIC X.
           03  BI-SMALLOAN         PIC X.
           03  BI-PLDATE           PIC 9(6) COMP.
           03  FILLER              PIC X(5).
