      * COPYMEMBER: LIBEX/EX01XBM
      ******************************************************************
      * 
      * IDENTIFICATION: LIBEX/EX01XBM
      * DESCRIPTION   : BORROWER MEMO EXTRACTION FILE
      * RECORD SIZE   : 88 BYTES
      * CUSTOMERS     : REGIONAL MANAGEMENT
      * 
      *=================================================================
      * REV:
      * KEC 2017.0417 {PR#05276} REGIONAL MANAGEMENT <A15> [D SINCLAIR]
      *         ***NEW***
      * 
      ******************************************************************
       78  XBM-SIZE                                  VALUE 88.
       01  XBM-REC.
           03  XBM-BRNO                PIC 9(4).
           03  XBM-SSNO                PIC 9(9).
           03  XBM-DATE                PIC 9(8).
           03  XBM-SEQ                 PIC 9(2).
           03  XBM-MEMO                PIC X(60).
           03  XBM-MEMO-TYPE           PIC X(1).
           03  XBM-REGION-NUMBER       PIC 9(2).
           03  XBM-CR                  PIC X(1).
           03  XBM-LF                  PIC X(1).
