      * COPYMEMBER: LIBLP/LP01IX
      ******************************************************************
      *    ALTERNATE INDEX FILE : (39) BYTES
      *
      * C - LOAN TRAILER COLLATERAL ID:
      *
      * REV:
      ******************************************************************
       01  IX-REC.
      *-------------------   PRIMARY KEY   -----------------------
           03  IX1-KEY.
               05  IX-CODE               PIC X.
               05  IX-REDEFINE-KEY       PIC X(8).

      *  C - LOAN TRAILER COLLATERAL ID:
               05  FILLER                REDEFINES IX-REDEFINE-KEY.
                   07  IX-LT-ID-ACCTNO   PIC 9(8) COMP.
                   07  IX-LT-ID-SEQNO    PIC 9(3) COMP.
                   07  IX-LT-ID-TYPE     PIC 99   COMP.
                   07  IX-LT-ID-FILLER   PIC X(1).

      *------------------   SECONDARY KEY   ----------------------
           03  IX2-KEY.
               05  IX-REDEFINE-KEY2      PIC X(30).

      *  C - LOAN TRAILER COLLATERAL ID:
               05  FILLER                REDEFINES IX-REDEFINE-KEY2.
                   07  IX-LT-ID          PIC X(30).
