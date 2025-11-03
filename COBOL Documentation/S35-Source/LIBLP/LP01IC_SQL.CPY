      * COPYMEMBER: LIBLP/LP01IC_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - INSURANCE COMPANY FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  IC1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  IC1-CURSOR-STAT-GOOD              VALUE "00".
           88  IC1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  IC1-RETURN-CODE              PIC S9(4)   COMP-5.
    
       01  QIC1-WHERE-BEG-KEY.
           03  QIC1-WBEG-BRNO             PIC 9(04).
           03  QIC1-WBEG-CODE             PIC 99.
 
       01  QIC1-WHERE-END-KEY.
           03  QIC1-WEND-BRNO             PIC 9(04).
           03  QIC1-WEND-CODE             PIC 99.
 
       01  QIC-REC.
           03  QIC-BRNO                 PIC 9(4).
           03  QIC-CODE                 PIC 99.
           03  QIC-LTOUCH-DATE          PIC X(10).
           03  QIC-NAME                 PIC X(20).
           03  QIC-ADDR1                PIC X(25).
           03  QIC-ADDR2                PIC X(25).
           03  QIC-CITY                 PIC X(15).
           03  QIC-STATE                PIC XX.
           03  QIC-ZIP                  PIC X(10).
           03  QIC-PHONE                PIC 9(10).
           03  QIC-COMMRATE-1           PIC S9(3)V99.
           03  QIC-COMMRATE-2           PIC S9(3)V99.
           03  QIC-COMMRATE-3           PIC S9(3)V99.
           03  QIC-COMMRATE-4           PIC S9(3)V99.
           03  QIC-COMMRATE-5           PIC S9(3)V99.
           03  QIC-COMMRATE-6           PIC S9(3)V99.
           03  QIC-COMMRATE-7           PIC S9(3)V99.
           03  QIC-COMMRATE-8           PIC S9(3)V99.
           03  QIC-COMMRATE-9           PIC S9(3)V99.
           03  QIC-COMMRATE-10          PIC S9(3)V99.
           03  QIC-COMMRATE-11          PIC S9(3)V99.
           03  QIC-COMMRATE-12          PIC S9(3)V99.
           03  QIC-COMMRATE-13          PIC S9(3)V99.
           03  QIC-COMMRATE-14          PIC S9(3)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
