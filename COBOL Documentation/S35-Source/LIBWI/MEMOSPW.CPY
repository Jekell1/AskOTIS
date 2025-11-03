      * COPYMEMBER: LIBWI/MEMOSPW
      ******************************************************************
      *
      * IDENTIFICATION: LIBWI/MEMOSPW.CPY
      *
      * DESCRIPTION   : LINK WORKERS FOR SPR MEMO WINDOW
      *
      * REV:
      * KEC 2015.1204 CHANGED MEMOSP-WORKER TO BE MEMOSP-SP1-KEY THEN
      *               ADD A FILLER LINE FOR THE REMAINING BYTES.
      *               THIS WAS DONE FOR INCREASING SM-SP-SPRCLASS TO 3 BYTES.
      * BAH 2024.0418 REMOVED MEMOSP-ALLOW-MOD, MEMOSP-ONLY, MEMOSP-MEMO-TYPE,
      *               MEMOSP-WIMENU-SELECTION, MEMOSP-CLMENU-SELECTION,
      *               MEMOSP-ACCOUNT-NO, MEMOSP-NOTE-NO
      ******************************************************************
       01  MEMOSP-BUF.
           03  MEMOSP-SP1-KEY          PIC X(09).
           03  FILLER        REDEFINES MEMOSP-SP1-KEY.
               05  MEMOSP-SP-ORGST     PIC X(02).
               05  MEMOSP-SP-SPRCLASS  PIC 9(03).
               05  MEMOSP-SP-SUBCLASS  PIC 9(02).
               05  MEMOSP-SP-LAWCODE   PIC 9(02).
