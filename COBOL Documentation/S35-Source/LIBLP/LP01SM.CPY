      * COPYMEMBER: LIBLP/LP01SM
      ******************************************************************
      *        STATE PARAMETER MEMO FILE    
      *
      * SM1-KEY = SM-SP-ORGST, SM-SP-SPRCLASS, SM-SP-SUBCLASS, SM-SP-LAWCODE
      *           SM-DATE-REVERSED, SM-SEQ
      *
      *
      * REV:
      * KEC 2015.1111 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         SM-SEQ CHANGED FROM 2 TO 3
      *         SM-REAL-DATE ADDED (YYYYMMDD)
      *
      * KEC 2015.1204 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *          INCREASE SM-SP-SPRCLASS TO BE 3 BYTES;
      *          CHANGED SM-SP-KEY TO BE SM-SP1-KEY.
      * CS 190509 ADD FUTURE FIELDS
      * BAH 20210427 REMOVED FUTURE FIELDS
      *  BAH 20230600 ADDED FILLER
      ******************************************************************
       78  SM-SIZE                       VALUE  125.
       01  SM-REC.
           03  SM1-KEY.
               05  SM-SP1-KEY.
                   07 SM-SP-ORGST        PIC X(02).
                   07 SM-SP-SPRCLASS     PIC 9(03).
                   07 SM-SP-SUBCLASS     PIC 9(02).
                   07 SM-SP-LAWCODE      PIC 9(02).
      * CCYYMMDD SUBTRACTED FROM 9'S
               05  SM-DATE-REVERSED      PIC 9(08).
               05  SM-DATE REDEFINES SM-DATE-REVERSED
                                         PIC 9(08).
               05  SM-SEQ                PIC 9(03).
      $XFD DATE=YYYYMMDD
           03  SM-LTOUCH-DATE            PIC 9(8) COMP-3.
           03  SM-LTOUCH-TIME-HHMM       PIC 9(4) COMP-3.
           03  SM-LTOUCH-USERID          PIC X(10).
           03  SM-MEMO.
               05  SM-MEMO1              PIC X(30).
               05  SM-MEMO2              PIC X(30).
           03  SM-MEMO-TYPE              PIC XX.
           03  FILLER                 PIC X(25).
