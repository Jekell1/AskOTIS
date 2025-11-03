      * REV:
      *  2025.0303 ADDED QTRW-INFO-INNOVIS-ID #1702 S35Q-169

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRW-INFO-CURSOR-STAT         PIC X(2) VALUE SPACES.
           88  TRW-INFO-CURSOR-STAT-GOOD         VALUE "00".
           88  TRW-INFO-CURSOR-STAT-BAD          VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TRW-INFO-RETURN-CODE         PIC S9(4)   COMP-5.

       01  QTRWINFO-WHERE-BEG-KEY.
           03  QTRW-INFO-WBEG-INFO-ID           PIC 9(04).
 
       01  QTRWINFO-WHERE-END-KEY.
           03  QTRW-INFO-WEND-INFO-ID           PIC 9(04).
 
       01  QTRWINFO-REC.
           03  QTRW-INFO-ID             PIC 9(4).
           03  QTRW-INFO-EQUIFAX-ID         PIC X(10).
           03  QTRW-INFO-EXPERIAN-ID        PIC X(5).
           03  QTRW-INFO-TUC-ID             PIC X(10).
           03  QTRW-INFO-REPORTER-NAME      PIC X(40).
           03  QTRW-INFO-REPORTER-ADR1  PIC X(32).
           03  QTRW-INFO-REPORTER-ADR2  PIC X(32).
           03  QTRW-INFO-REPORTER-CSZ   PIC X(32).
           03  QTRW-INFO-REPORTER-PHONE     PIC 9(10).
           03  QTRW-INFO-GRANTOR-CODE       PIC X(20).
           03  QTRW-INFO-CR-LF              PIC X.
           03  QTRW-INFO-BRNO-ACCTNO        PIC X.
           03  QTRW-INFO-IDENT-BRNO         PIC X.
           03  QTRW-YEARS-STOP-REPORTING    PIC 99.
           03  QTRW-INVALID-SSNO            PIC X.
           03  QTRW-INFO-SCHED-PAY          PIC X.
           03  QTRW-INFO-LTOUCH-DATE        PIC X(10).
           03  QTRW-INFO-INNOVIS-ID         PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
