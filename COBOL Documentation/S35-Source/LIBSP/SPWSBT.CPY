      * COPYMEMBER: LIBSP/SPWSBT
      ******************************************************************
      *    LIBSP/SPWSBT:       BATCH ALLOTMENT TAPE
      * REV:
      *  JTG 050112 CORRECTED DIRECTORY FORM 'SP' TO 'ED'
      *  KEC 180202 ADDED BT-ALL-FILENAME SO THAT WE CAN ALTER
      *             THE FILENAME FOR FIX PROGRAMS (FX/SCREF8; PR#1299)
      ******************************************************************
      *    LIBSP/SPWSBT: %%FILE PATH WORKERS
      *    /USR/DATA/R#/ED/BTFILE
      ******************************************************************
       01  BT-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  BT-PATH                   PIC X(35)   VALUE SPACES.
       01  BT-ALL-PATH.
           03  BT-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES BT-ALL-BASE.
               05  BT-ALL-BASE-USR   PIC X(05).
               05  BT-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  BT-ALL-MACHINE        PIC X(02).
           03  BT-ALL-FILENAME       PIC X(23)   VALUE "/ED/BTFILE".
