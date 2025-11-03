      * COPYMEMBER: LIBLP/LP01RX_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - RESCISSION INDEX FILE
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  RX1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  RX1-CURSOR-STAT-GOOD              VALUE "00".
           88  RX1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  RX2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  RX2-CURSOR-STAT-GOOD              VALUE "00".
           88  RX2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  RX3-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  RX3-CURSOR-STAT-GOOD              VALUE "00".
           88  RX3-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  RX1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  RX2-RETURN-CODE                PIC S9(4)   COMP-5.
       01  RX3-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QRX1-WHERE-BEG-KEY.
           03  QRX1-WBEG-BRNO             PIC 9(04).
           03  QRX1-WBEG-ACCTNO           PIC 9(08).
 
       01  QRX1-WHERE-END-KEY.
           03  QRX1-WEND-BRNO             PIC 9(04).
           03  QRX1-WEND-ACCTNO           PIC 9(08).

       01  QRX2-WHERE-BEG-KEY.
           03  QRX2-WBEG-BRNO             PIC 9(04).
           03  QRX2-WBEG-LNAME            PIC X(35).
           03  QRX2-WBEG-FNAME            PIC X(35).

       01  QRX2-WHERE-END-KEY.
           03  QRX2-WEND-BRNO             PIC 9(04).
           03  QRX2-WEND-LNAME            PIC X(35).
           03  QRX2-WEND-FNAME            PIC X(35).


       01  QRX3-WHERE-BEG-KEY.
           03  QRX3-WBEG-BRNO             PIC 9(04).
           03  QRX3-WBEG-DLNO             PIC 9(04)V99.
           03  QRX3-WBEG-INTDATE          PIC X(10).
           03  QRX3-WBEG-LNAME            PIC X(35).
           03  QRX3-WBEG-FNAME            PIC X(35).

       01  QRX3-WHERE-END-KEY.
           03  QRX3-WEND-BRNO             PIC 9(04).
           03  QRX3-WEND-DLNO             PIC 9(04)V99.
           03  QRX3-WEND-INTDATE          PIC X(10).
           03  QRX3-WEND-LNAME            PIC X(35).
           03  QRX3-WEND-FNAME            PIC X(35).
 
       01  QRX-REC.
           03  QRX-BRNO                    PIC 9(4).
           03  QRX-ACCTNO                  PIC 9(8).
           03  QRX-DLNO                    PIC 9(4)V99.
           03  QRX-INTDATE                 PIC X(10).
           03  QRX-LNAME                   PIC X(35).
           03  QRX-FNAME                   PIC X(35).
           03  QRX-LTOUCH-DATE             PIC X(10).
           03  QRX-LOANDATE                    PIC X(10).
           03  QRX-LOANAMT                     PIC 9(7)V99.
           03  QRX-PROCEEDS                    PIC 9(7)V99.
           03  QRX-STATUS                      PIC X(1).

       EXEC SQL END DECLARE SECTION END-EXEC.
