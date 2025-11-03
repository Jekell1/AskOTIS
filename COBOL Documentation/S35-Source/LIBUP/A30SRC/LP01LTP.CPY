      * COPYMEMBER: LIBLP/LP01LT
      ******************************************************************
      *    LOAN PAYMENT SCHEDULE TRAILER FILE : (250) BYTES
      *
      *  BAH 06/19/2015 CREATED NEW FROM LP01LT
      *   CS 160614 CS CHANGED LTP-PAY-SCHLD-TBL FROM OCCURS 17 TIMES
      *                TO OCCURS 41 TO HANDLE 40 YEARS OF ESCROW, NEED THE
      *                41 TO COVER THE 1ST YEAR WHICH COULD OCCUPY 2 LEVELS
      *                IF THERE IS AN ODD PAY, RECORD SIZE FROM 250 TO 450
      **************************************************************************
       78  LTP-SIZE                      VALUE 450.
       01  LTP-REC.
           03  LTP1-KEY.
               05  LTP-BRNO              PIC 9(4).
               05  LTP-ACCTNO            PIC 9(8).
               05  LTP-SEQNO             PIC 999.

      *>>XDD DATE=YYYYMMDD
           03  LTP-LTOUCH-DATE           PIC 9(8)     COMP-3.

      * 8 X 17 = 136
           03  LTP-PAY-SCHLD-AREA.
               05  LTP-PAY-SCHLD-TBL      OCCURS 41 TIMES.
                   07  LTP-SCHLD-TERM     PIC 9(4)    COMP-3.
                   07  LTP-SCHLD-PAYMNT   PIC 9(6)V99 COMP-3.

           03  FILLER                PIC X(102).
