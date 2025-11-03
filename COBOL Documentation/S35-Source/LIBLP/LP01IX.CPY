      * COPYMEMBER: LIBLP/LP01IX
      ******************************************************************
      *
      * DESCRIPTION:   ALTERNATE INDEX FILE 
      *
      * C - LOAN TRAILER COLLATERAL ID:
      *
      * IX1-KEY =  IX-BRNO, IX-CODE, IX-ACCTNO, IX-SEQNO
      * IX2-KEY =  IX-BRNO, IX-VIN                      WITH DUPS
      * IX3-KEY =  IX-VIN 
      *
      *=================================================================
      * REV:
      * BAH 160114 CHANGED LTC-KEY, CHANGED IX1-KEY, PD0003
      *
      * KEC 2017.0221 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED IX-BRNO WITHIN IX1-KEY.  WAS NOT IN IX-REC.
      *
      * KEC 2018.1212 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED IX-REDEFINE-KEY & IX-REDEFINE-KEY2 SINCE THEY
      *          ARE NOT USED ANYWHERE IN A30/A32.
      *  CS 181219 REMOVED IX2-KEY
      *  CS 190520 REMOVE ENDING FILLER
      *  CS 190520 ADD DATE DESIGNATION TO IX-LTOUCH-DATE
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  IX-SIZE                       VALUE  100.
       01  IX-REC.
           03  IX1-KEY.
               05  IX-BRNO               PIC 9(04).
               05  IX-CODE               PIC X(01).
               05  IX-ACCTNO             PIC 9(08).
               05  IX-SEQNO              PIC 9(03).
           03  IX3-KEY.
               05  IX-VIN                PIC X(30).
      $XFD DATE=YYYYMMDD
           03  IX-LTOUCH-DATE            PIC 9(8)  COMP-3.
           03  FILLER                    PIC X(49).
