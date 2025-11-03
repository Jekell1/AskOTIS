      * COPYMEMBER: LIBLP/LP01IX
      ******************************************************************
      *
      * DESCRIPTION:   ALTERNATE INDEX FILE (100) BYTES
      *
      * C - LOAN TRAILER COLLATERAL ID:
      *
      *=================================================================
      * REV:
      * BAH 160114 CHANGED LTC-KEY, CHANGED IX1-KEY, PD0003
      *
      * KEC 2017.0221 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED IX-BRNO WITHIN IX1-KEY.  WAS NOT IN IX-REC.
      *
      ******************************************************************
       78  IX-SIZE                       VALUE 100.
       01  IX-REC.
           03  IX1-KEY.
               05  IX-BRNO               PIC 9(04).
               05  IX-CODE               PIC X(01).
               05  IX-REDEFINE-KEY       PIC X(11).
      *  C - LOAN TRAILER COLLATERAL ID:
               05  FILLER                REDEFINES IX-REDEFINE-KEY.
                   07  IX-ACCTNO         PIC 9(08).
      * THIS IX-SEQNO IS NOT THE LTC-SEQNO, THIS WILL HAVE MULTIPLE
      * SEQUENCES FOR EACH VIN, LTC HAS 3 VINS IN IT, THE IX-SEQNO WILL
      * BE THE SUBSCRIPT OF THE VIN, SO IF LTC-VIN(2), THIS IS 2
                   07  IX-SEQNO          PIC 9(03).

           03  IX2-KEY.
               05  IX-REDEFINE-KEY2      PIC X(30).
               05  FILLER                REDEFINES IX-REDEFINE-KEY2.
                   07  IX-VIN            PIC X(30).

           03  IX-LTOUCH-DATE            PIC 9(8)  COMP-3.

           03  FILLER                    PIC X(49).
