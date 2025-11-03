      * COPYMEMBER: LIBLP/LP01LSF
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *     FOLLOW UP EXPANDED INFO
      *
      *   LSF1-KEY = LSF-BRNO, LSF-ACCTNO, LSF-SEQNO
      *
      * REV:
      *  BAH 070308 ADDED "F" FOLLOW UP INFO FOR EXPANDED FIELDS TRITON PR#2937
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *   CS 190503 ADD FUTURE FIELDS
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      *  BAH 20230600 ADDED FILLER
      ******************************************************************
       78  LSF-SIZE                       VALUE 380.
       01  LSF-REC.
           03  LSF1-KEY.
               05  LSF-BRNO               PIC 9(4).
               05  LSF-ACCTNO             PIC 9(8).
               05  LSF-SEQNO              PIC 99.

      $XFD DATE=YYYYMMDD
           03  LSF-LTOUCH-DATE            PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSF-SKIP-START-DATE        PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSF-SKIP-END-DATE          PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSF-OFR-START-DATE         PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSF-OFR-END-DATE           PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSF-IMPOUND-START-DATE     PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSF-IMPOUND-END-DATE       PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSF-BNKRPT-START-DATE      PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSF-BNKRPT-END-DATE        PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSF-INSUR-START-DATE       PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSF-INSUR-END-DATE         PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSF-COLLECT-START-DATE     PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSF-COLLECT-END-DATE       PIC 9(8)  COMP-3.

           03  FILLER                 PIC X(301).
