      ******************************************************************
      *
      * IDENTIFICATION: LIBLP/LPIPBUF
      * DESCRIPTION   : FORMS BUFFER
      *
      *=================================================================
      * REV:
      * 130419 BAH NEW, NEEDED TO PASS THIS BUFFER SO I PULLED IT OUT OF
      *                 CHECKTRANSW
      *
      * KEC 2016.0121 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *         REMOVED FT-FILE; MERGED INTO FD-FILE.
      *         CHANGED FMNO FROM 3 TO 4 BYTES.
      *
      * KEC 2017.0118 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED IP-SMARTPORT TO IP-FD-OPTION [88 LEVELS].
      *
      ******************************************************************
       01  IP-BUF                       VALUE SPACES.
           03  IP-PRU                   PIC X(2).
           03  IP-PROG                  PIC X(2).
           03  IP-FMNO                  PIC 9(04).
           03  IP-AOK                   PIC X.
           03  CONT-PRINT               PIC X.
           03  QUEUE-PRINT              PIC X.
           03  IP-PRFG                  PIC X.
               88 INITIAL-PRINT           VALUE "P".
               88 REPRINT                 VALUE "R".
           03  PRINT-REPRINT REDEFINES IP-PRFG PIC X.
           03  BATCH-PR                 PIC X.
               88 BATCH-PRINT             VALUE "Y".
               88 DIRECT-PRINT            VALUE "N" " ".
           03  CXFILE-SCAN-FLAG         PIC X.
           03  PRINT-RANGE-BUF.
               05  CK1                  PIC 9(6).
               05  CK2                  PIC 9(6).

           03  IP-FMCODE                PIC X.
           03  IP-PROG-PREFIX           PIC X.
           03  IP-ACCTNO                PIC 9(8).

           03  IP-BEG-FMNO              PIC 9(4).
           03  IP-BEG-FMNOX REDEFINES IP-BEG-FMNO PIC XXXX.
           03  IP-END-FMNO              PIC 9(4).
           03  IP-END-FMNOX REDEFINES IP-END-FMNO PIC XXXX.
           03  IP-FMTYPE                PIC X.
           03  IP-FD-OPTION             PIC X.
