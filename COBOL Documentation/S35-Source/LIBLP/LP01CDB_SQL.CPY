      * COPYMEMBER: LIBLP/LP01CDB_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - CODE DESCRIPTION BRANCH FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      *
      ******************************************************************

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  CDB1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  CDB1-CURSOR-STAT-GOOD             VALUE "00".
           88  CDB1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  CDB1-CURSOR-GR-STAT          PIC X(2) VALUE SPACES.
           88  CDB1-CURSOR-GR-STAT-GOOD          VALUE "00".
           88  CDB1-CURSOR-GR-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  CDB1-RETURN-CODE               PIC S9(4)   COMP-5.

       01  QCDB1-WHERE-BEG-KEY.
           03  QCDB1-WBEG-TYPE            PIC X(2).
           03  QCDB1-WBEG-BRANCH          PIC 9(4).
           03  QCDB1-WBEG-CODE            PIC X(8).
 
       01  QCDB1-WHERE-END-KEY.
           03  QCDB1-WEND-TYPE            PIC X(2).
           03  QCDB1-WEND-BRANCH          PIC 9(4).
           03  QCDB1-WEND-CODE            PIC X(8).
 
       01  QCDB-REC.
           03  QCDB-TYPE                              PIC X(02).
           03  QCDB-BRANCH                            PIC 9(04).
           03  QCDB-CODE                              PIC X(08).
           03  QCDB-LTOUCH-DATE                       PIC X(10).
           03  QCDB-DESC                              PIC X(35).
           03  QCDB-CN-GROUP-ID                       PIC X(6).
           03  QCDB-CN-PHONE                          PIC 9(10).
           03  QCDB-CN-PHONE-EXT                      PIC 9(5).
           03  QCDB-GC-GLFEENO                        PIC 9(7).
           03  QCDB-GC-GLFEEFINCHG                    PIC X(1).
           03  QCDB-GC-GLFEE-INT-CHARGABLE            PIC X(1).
           03  QCDB-GC-FEE-GIFTNET                    PIC X(1).
           03  QCDB-GC-FEE-PRODNO                     PIC 9(5).
           03  QCDB-GC-FEE-PRICE                      PIC 9(4)V99.
           03  QCDB-GC-NEW-PRODNO                     PIC 9(5).
           03  QCDB-GC-FEE-MAXAMT                     PIC 9(5)V99.
           03  QCDB-GC-ALT-GLFEENO                    PIC 9(7).
           03  QCDB-GC-COMM-GLFEENO                   PIC 9(11).
           03  QCDB-GC-COMM-PERCENT                   PIC 999V99.
           03  QCDB-GC-FEE-EARNINGS-FG                PIC X.
           03  QCDB-GC-BRANCH-COMM-AMOUNT             PIC 9(6).

       EXEC SQL END DECLARE SECTION END-EXEC.
