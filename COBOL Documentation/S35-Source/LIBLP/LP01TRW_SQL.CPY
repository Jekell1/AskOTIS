      * COPYMEMBER: LIBLP/LP01TRW_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTION FILE - CASH BALANCING
      *
      *=================================================================
      * REV:
      * BLM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRW1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRW1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRW1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRW2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRW2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRW2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRW1-CURSOR-SC-STAT          PIC X(2) VALUE SPACES.
           88  TRW1-CURSOR-SC-STAT-GOOD          VALUE "00".
           88  TRW1-CURSOR-SC-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TRW1-RETURN-CODE                 PIC S9(4)   COMP-5.
       01  TRW2-RETURN-CODE                 PIC S9(4)   COMP-5.

       01  QTRW1-WHERE-BEG-KEY.
           03  QTRW1-WBEG-BRNO             PIC 9(4).
           03  QTRW1-WBEG-DATE             PIC X(10).
           03  QTRW1-WBEG-TILLNO           PIC 9(02).

       01  QTRW1-WHERE-END-KEY.
           03  QTRW1-WEND-BRNO             PIC 9(4).
           03  QTRW1-WEND-DATE             PIC X(10).
           03  QTRW1-WEND-TILLNO           PIC 9(02).

       01  QTRW1-WHERE-SCAN-BEG-KEY.
           03  QTRW1-WSBEG-DATE            PIC X(10).

       01  QTRW2-WHERE-BEG-KEY.
           03  QTRW2-WBEG-DATE             PIC X(10).
           03  QTRW2-WBEG-BRNO             PIC 9(4).
           03  QTRW2-WBEG-TILLNO           PIC 9(02).

       01  QTRW2-WHERE-END-KEY.
           03  QTRW2-WEND-DATE             PIC X(10).
           03  QTRW2-WEND-BRNO             PIC 9(4).
           03  QTRW2-WEND-TILLNO           PIC 9(02).
 
       01  QTRW-REC.
           03  QTW-BRNO             PIC 9(4).
           03  QTW-DATE             PIC X(10).
           03  QTW-TILLNO           PIC 9(2).
           03  QTW-LTOUCH-DATE          PIC X(10).
           03  QTW-POSTNAME-1         PIC X(08).
           03  QTW-TIME-1             PIC 9(04).
           03  QTW-AMOUNT-1           PIC S9(07)V99.
           03  QTW-OVERSHORT-1        PIC S9(07)V99.
           03  QTW-POSTNAME-2         PIC X(08).
           03  QTW-TIME-2             PIC 9(04).
           03  QTW-AMOUNT-2           PIC S9(07)V99.
           03  QTW-OVERSHORT-2        PIC S9(07)V99.
           03  QTW-POSTNAME-3         PIC X(08).
           03  QTW-TIME-3             PIC 9(04).
           03  QTW-AMOUNT-3           PIC S9(07)V99.
           03  QTW-OVERSHORT-3        PIC S9(07)V99.
           03  QTW-POSTNAME-4         PIC X(08).
           03  QTW-TIME-4             PIC 9(04).
           03  QTW-AMOUNT-4           PIC S9(07)V99.
           03  QTW-OVERSHORT-4        PIC S9(07)V99.
           03  QTW-POSTNAME-5         PIC X(08).
           03  QTW-TIME-5             PIC 9(04).
           03  QTW-AMOUNT-5           PIC S9(07)V99.
           03  QTW-OVERSHORT-5        PIC S9(07)V99.
           03  QTW-POSTNAME-6         PIC X(08).
           03  QTW-TIME-6             PIC 9(04).
           03  QTW-AMOUNT-6           PIC S9(07)V99.
           03  QTW-OVERSHORT-6        PIC S9(07)V99.
           03  QTW-POSTNAME-7         PIC X(08).
           03  QTW-TIME-7             PIC 9(04).
           03  QTW-AMOUNT-7           PIC S9(07)V99.
           03  QTW-OVERSHORT-7        PIC S9(07)V99.
           03  QTW-POSTNAME-8         PIC X(08).
           03  QTW-TIME-8             PIC 9(04).
           03  QTW-AMOUNT-8           PIC S9(07)V99.
           03  QTW-OVERSHORT-8        PIC S9(07)V99.
           03  QTW-POSTNAME-9         PIC X(08).
           03  QTW-TIME-9             PIC 9(04).
           03  QTW-AMOUNT-9           PIC S9(07)V99.
           03  QTW-OVERSHORT-9        PIC S9(07)V99.
           03  QTW-POSTNAME-10        PIC X(08).
           03  QTW-TIME-10            PIC 9(04).
           03  QTW-AMOUNT-10          PIC S9(07)V99.
           03  QTW-OVERSHORT-10       PIC S9(07)V99.
           03  QTW-POSTNAME-11         PIC X(08).
           03  QTW-TIME-11             PIC 9(04).
           03  QTW-AMOUNT-11           PIC S9(07)V99.
           03  QTW-OVERSHORT-11        PIC S9(07)V99.
           03  QTW-POSTNAME-12         PIC X(08).
           03  QTW-TIME-12             PIC 9(04).
           03  QTW-AMOUNT-12           PIC S9(07)V99.
           03  QTW-OVERSHORT-12        PIC S9(07)V99.
           03  QTW-POSTNAME-13         PIC X(08).
           03  QTW-TIME-13             PIC 9(04).
           03  QTW-AMOUNT-13           PIC S9(07)V99.
           03  QTW-OVERSHORT-13        PIC S9(07)V99.
           03  QTW-POSTNAME-14         PIC X(08).
           03  QTW-TIME-14             PIC 9(04).
           03  QTW-AMOUNT-14           PIC S9(07)V99.
           03  QTW-OVERSHORT-14        PIC S9(07)V99.
           03  QTW-POSTNAME-15         PIC X(08).
           03  QTW-TIME-15             PIC 9(04).
           03  QTW-AMOUNT-15           PIC S9(07)V99.
           03  QTW-OVERSHORT-15        PIC S9(07)V99.
           03  QTW-POSTNAME-16         PIC X(08).
           03  QTW-TIME-16             PIC 9(04).
           03  QTW-AMOUNT-16           PIC S9(07)V99.
           03  QTW-OVERSHORT-16        PIC S9(07)V99.
           03  QTW-POSTNAME-17         PIC X(08).
           03  QTW-TIME-17             PIC 9(04).
           03  QTW-AMOUNT-17           PIC S9(07)V99.
           03  QTW-OVERSHORT-17        PIC S9(07)V99.
           03  QTW-POSTNAME-18         PIC X(08).
           03  QTW-TIME-18             PIC 9(04).
           03  QTW-AMOUNT-18           PIC S9(07)V99.
           03  QTW-OVERSHORT-18        PIC S9(07)V99.
           03  QTW-POSTNAME-19         PIC X(08).
           03  QTW-TIME-19             PIC 9(04).
           03  QTW-AMOUNT-19           PIC S9(07)V99.
           03  QTW-OVERSHORT-19        PIC S9(07)V99.
           03  QTW-POSTNAME-20          PIC X(08).
           03  QTW-TIME-20              PIC 9(04).
           03  QTW-AMOUNT-20            PIC S9(07)V99.
           03  QTW-OVERSHORT-20         PIC S9(07)V99.
       
       EXEC SQL END DECLARE SECTION END-EXEC.
