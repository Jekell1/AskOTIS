      * COPYMEMBER: LIBLP/LP01LSM
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *         MISC FIELDS - UNDEFINED
      *
      * LSM1-KEY - BRNO, ACCTNO, SEQNO
      *
      * REV:
      *  BAH 100615 ADDED "M" MISC FIELDS RECORDS FOR UNITED PR# 3615
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *   CS 190506 REMOVE ENDING FILLER,NO FUTURE FIELDS
      *  BAH 2022.0321 ADDED LS-M-TRANS-FROM-BRNO, LS-M-TRANS-FROM-ACCTNO,
      *                LS-M-TRANS-FROM-DATE FOR METRO2 CREDIT BUREAU
      *                "L1" SEGMENT, #1547
      *  BAH 2022.0921 ADDED LS-M-MISPAY-90-DATE FOR VERITEC ILUEXT #1570
      *  BAH 20230600 ADDED FILLER
      *  BAH 20240109 ADDED SECOND BENEFICIARY INFO #1637
      ******************************************************************
       78  LSM-SIZE                       VALUE 900.
       01  LSM-REC.
           03  LSM1-KEY.
               05  LSM-BRNO               PIC 9(4).
               05  LSM-ACCTNO             PIC 9(8).
               05  LSM-SEQNO              PIC 99.

      $XFD DATE=YYYYMMDD
           03  LSM-LTOUCH-DATE               PIC 9(8)     COMP-3.

      * LS-M-MISC-FIELD(15) IS VERITEC ID
           03  LSM-MISC-AREA.
               05  LSM-MISC-FIELD          PIC X(20) OCCURS 32.

           03  LSM-TRANS-FROM-BRNO         PIC 9(4).
           03  LSM-TRANS-FROM-ACCTNO       PIC 9(8).
      $XFD DATE=YYYYMMDD
           03  LSM-TRANS-FROM-DATE         PIC 9(8).
      $XFD DATE=YYYYMMDD
           03  LSM-MISPAY-90-DATE          PIC 9(8).

           03  LSM-SECOND-BEN-NAME         PIC X(32).
           03  LSM-SECOND-BEN-ADR          PIC X(32).
           03  LSM-SECOND-BEN-CITY         PIC X(16).
           03  LSM-SECOND-BEN-STATE        PIC X(2).
           03  LSM-SECOND-BEN-ZIP          PIC X(10).
           03  LSM-SECOND-BEN-PHONE        PIC 9(10) COMP-3.
           03  LSM-SECOND-BEN-RELATION     PIC X(15).

           03  FILLER                      PIC X(100).
