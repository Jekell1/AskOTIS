      * COPYMEMBER: LIBLP/LP01LSF
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *     FOLLOW UP EXPANDED INFO
      *
      * REV:
      *  BAH 070308 ADDED "F" FOLLOW UP INFO FOR EXPANDED FIELDS TRITON PR#2937
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      ******************************************************************
       78  LSF-SIZE                       VALUE 400.
       01  LSF-REC.
           03  LSF1-KEY.
               05  LSF-BRNO               PIC 9(4).
               05  LSF-ACCTNO             PIC 9(8).
               05  LSF-SEQNO              PIC 99.

      *>>XDD DATE=YYYYMMDD
           03  LSF-LTOUCH-DATE               PIC 9(8)     COMP-3.

           03  LSF-FOLLOW-UP-AREA.
      *>>XDD DATE=YYYYMMDD
               05  LSF-SKIP-START-DATE       PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSF-SKIP-END-DATE         PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSF-OFR-START-DATE        PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSF-OFR-END-DATE          PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSF-IMPOUND-START-DATE    PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSF-IMPOUND-END-DATE      PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSF-BNKRPT-START-DATE     PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSF-BNKRPT-END-DATE       PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSF-INSUR-START-DATE      PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSF-INSUR-END-DATE        PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSF-COLLECT-START-DATE    PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSF-COLLECT-END-DATE      PIC 9(8)      COMP-3.

               05  FILLER                    PIC X(321).

