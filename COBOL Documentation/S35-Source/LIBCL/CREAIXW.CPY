      * COPYMEMBER: LIBCL/CREAIXW
      ******************************************************************
      *    CREAIXW   ALTERNATE INDEX FILE CREATE WORKER
      *
      * CREAIX-CODE    'C' - LOAN TRAILER COLLATERAL ID:
      *
      *    NOTE:
      *         1. THIS COPY MEMBER MUST FOLLOW 'LIBGB/FILEWK'
      *         2. CREAIX-ACTION MUST BE THE 1ST FIELD
      *
      *=================================================================
      * REV:
      * BAH 160114 CHANGED IX1-KEY, CHANGED CREAIX1-KEY PD0003
      * KEC 2017.0221 ADDED CREAIX-BRNO DUE TO ADDING IX-BRNO.
      *               ADDED CREAIX-SIZE (VALUE 143); USED FOR CL/CREAIX.
      *
      ******************************************************************
       78  CREAIX-SIZE                       VALUE 143.
       01  CREAIX-AREA.
           03  CREAIX-ACTION                 PIC X.
           03  CREAIX-ERRCD                  PIC X.
           03  CREAIX-IX-PATH                PIC X(35) VALUE SPACES.

           03  CREAIX-BRNO                   PIC 9(4).
           03  CREAIX-CODE                   PIC X(1).
           03  CREAIX-LTC-ACCTNO             PIC 9(8).
           03  CREAIX-LTC-SEQNO              PIC 9(3).

           03  CREAIX-LTC-VIN                PIC X(30) OCCURS 3.
