      * COPYMEMBER: LIBWI/CDBSCANW
      ******************************************************************
      * REV:
      *
      * BAH 2019.0122 SPLIT OUT CDFILE AND CDBFILE, NEED SEPARATE SCANS
      *               CDBFILE HAS "GC" AND "CN"
      * BAH 2024.0415 REMOVED CDBSCN-GC-FEE-GIFTNET-ISO 
      * 2024.0503 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  CDBSCN-BUF.
           03  CDBSCN-BEGIN-Y           PIC 9(02)  VALUE 6.
           03  CDBSCN-BEGIN-X           PIC 9(02)  VALUE 37.
           03  CDBSCN-WINDOW-BUF.
      * CDBFILE ("CN" AND "GC" WHICH NEED BRANCH IN KEY)
              05  CDBSCN-CDB1-KEY      PIC X(14).
              05  FILLER            REDEFINES CDBSCN-CDB1-KEY.
                  07  CDBSCN-TYPE       PIC X(2).
                  07  CDBSCN-BRANCH     PIC 9(4).
                  07  CDBSCN-CODE       PIC X(8).
              05  CDBSCN-STATUS        PIC XX.


