      * COPYMEMBER: LIBAP/AP01VM
      **************************************************
      *          ( VENDOR MEMO INDEX # 1 )
      *        ( KEY: VENDOR/DATE/SEQ  )
      * REV:
      * BLV 990512 ADDED REDEFINE OF VM-DATE FOR VM-DATE-2000
      * GLB 001113 NEW MEMO RECORD FOR VENDOR
      **************************************************
       01  VM-REC.
           03  VM1-KEY.
               05  VM-NUMBER           PIC 9(6).
               05  VM-DATE             PIC 9(6).
               05  FILLER REDEFINES VM-DATE.
                   07  VM-DATE-2000 PIC 9(12) COMP-6.
               05  VM-SEQ              PIC 9(2).
           03  VM-MEMO.
               05  VM-MEMO1            PIC X(30).
               05  VM-MEMO2            PIC X(30).
           03  VM-MEMO-TYPE            PIC X.
           03  FILLER                  PIC X(5).
