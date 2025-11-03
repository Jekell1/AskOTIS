      * COPYMEMBER: LIBLP/LP01LSM
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *         MISC FIELDS - UNDEFINED
      *
      * REV:
      *  BAH 100615 ADDED "M" MISC FIELDS RECORDS FOR UNITED PR# 3615
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      ******************************************************************
       78  LSM-SIZE                       VALUE 900.
       01  LSM-REC.
           03  LSM1-KEY.
               05  LSM-BRNO               PIC 9(4).
               05  LSM-ACCTNO             PIC 9(8).
               05  LSM-SEQNO              PIC 99.

      *>>XDD DATE=YYYYMMDD
           03  LSM-LTOUCH-DATE               PIC 9(8)     COMP-3.


           03  LSM-MISC-AREA.
               05  LSM-MISC-FIELD          PIC X(20) OCCURS 32.

               05  FILLER                  PIC X(241).

