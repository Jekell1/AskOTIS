      * COPYMEMBER: LIBLP/LP01LTF_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN TRAILER FILE - FEE CODES
      *
      *=================================================================
      * REV:
      * BLM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LTF1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LTF1-CURSOR-STAT-GOOD             VALUE "00".
           88  LTF1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LTF1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLTF1-WHERE-BEG-KEY.
           03  QLTF1-WBEG-BRNO             PIC 9(04).
           03  QLTF1-WBEG-ACCTNO           PIC 9(08).
           03  QLTF1-WBEG-SEQNO            PIC 9(03).

       01  QLTF1-WHERE-END-KEY.
           03  QLTF1-WEND-BRNO             PIC 9(04).
           03  QLTF1-WEND-ACCTNO           PIC 9(08).
           03  QLTF1-WEND-SEQNO            PIC 9(03).
 
       01  QLTF-REC.
           03  QLTF-BRNO                      PIC 9(4).
           03  QLTF-ACCTNO                    PIC 9(8).
           03  QLTF-SEQNO                     PIC 999.
           03  QLTF-LTOUCH-DATE               PIC X(10).
           03  QLTF-FEECODE-1                   PIC XX.
           03  QLTF-FEECODE-PAYABLE-1           PIC S9(7)V99.
           03  QLTF-FEECODE-2                   PIC XX.
           03  QLTF-FEECODE-PAYABLE-2           PIC S9(7)V99.
           03  QLTF-FEECODE-3                   PIC XX.
           03  QLTF-FEECODE-PAYABLE-3           PIC S9(7)V99.
           03  QLTF-FEECODE-4                   PIC XX.
           03  QLTF-FEECODE-PAYABLE-4           PIC S9(7)V99.
           03  QLTF-FEECODE-5                   PIC XX.
           03  QLTF-FEECODE-PAYABLE-5           PIC S9(7)V99.
           03  QLTF-FEECODE-6                   PIC XX.
           03  QLTF-FEECODE-PAYABLE-6           PIC S9(7)V99.
           03  QLTF-FEECODE-7                   PIC XX.
           03  QLTF-FEECODE-PAYABLE-7           PIC S9(7)V99.
           03  QLTF-FEECODE-8                   PIC XX.
           03  QLTF-FEECODE-PAYABLE-8           PIC S9(7)V99.
           03  QLTF-FEECODE-9                   PIC XX.
           03  QLTF-FEECODE-PAYABLE-9           PIC S9(7)V99.
           03  QLTF-FEECODE-10                  PIC XX.
           03  QLTF-FEECODE-PAYABLE-10          PIC S9(7)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
