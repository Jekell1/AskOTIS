      * COPYMEMBER: LIBAP/AP01VM
      ******************************************************************
      *
      * IDENTIFICATION: LIBAP/AP01VM.CPY
      *
      * DESCRIPTION   : A/P VENDOR MEMO
      *
      * PRIMARY KEY   : VENDOR/DATE/SEQ
      *
      *=================================================================
      * REV:
      * BLV 990512 ADDED REDEFINE OF VM-DATE FOR VM-DATE-2000
      * GLB 001113 NEW MEMO RECORD FOR VENDOR
      *
      * KEC 2015.1023 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         VM-SEQ CHANGED FROM 2 TO 3
      *         VM-DATE CHANGED FROM 6 TO 8 (YYYYMMDD - 99999999)
      *         REMOVED LONG NAME RECORDS; MERGED INTO VN RECORDS
      *         ADDED LTOUCH FIELDS
      *
      ******************************************************************
       78  VM-SIZE                                   VALUE 125.
       01  VM-REC.
           03  VM1-KEY.
               05  VM-NUMBER           PIC 9(06).
               05  VM-DATE             PIC 9(08).
               05  VM-SEQ              PIC 9(03).

      *>>XDD DATE=YYYYMMDD
           03  VM-LTOUCH-DATE          PIC 9(8) COMP-3.
           03  VM-LTOUCH-TIME-HHMM     PIC 9(4) COMP-3.
           03  VM-LTOUCH-USERID        PIC X(10).
           03  VM-MEMO.
               05  VM-MEMO1            PIC X(30).
               05  VM-MEMO2            PIC X(30).
           03  VM-MEMO-TYPE            PIC X(2).
           03  FILLER                  PIC X(28).
